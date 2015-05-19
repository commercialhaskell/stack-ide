{-# LANGUAGE OverloadedStrings #-}
-- | This module defines a function 'annotateMessage', which attempts to add some
--   helpful structure to GHC errors and warnings.
module IdeSession.Client.AnnotateMessage (annotateMessage) where

import           Control.Exception
import           Control.Monad
import           Control.Spoon (teaspoonWithHandles, Handles)
import           Data.List (intercalate)
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.ICU as ICU
import qualified Data.Text.ICU.Error as ICU
-- import           Language.Haskell.Extension (KnownExtension)
import           Prelude
import           Safe (headMay, lastMay)
import           IdeSession.Client.JsonAPI (AnnSourceError(..), Ann(..), MsgAnn(..))
import           IdeSession.Types.Public (SourceError(..), SourceErrorKind(..), SourceSpan(..), EitherSpan(..))

annotateMessage :: SourceError -> AnnSourceError
annotateMessage err = AnnSourceError
  { annErrorKind = errorKind err
  , annErrorSpan = errorSpan err
  , annErrorMsg = annotateMessage' err
  }

annotateMessage' :: SourceError -> Ann MsgAnn
annotateMessage' err = case (errorSpan err, errorMsg err) of
    ( ProperSpan sp,
      match "^(?:Warning: )?The import of ([^\\s]+) is redundant(?:.*)" -> Just [_, x])
      | fromGhc -> AnnGroup
        [ AnnLeaf "Redundant import: "
        , Ann MsgAnnModule (AnnLeaf (stripIdent x))
        , Ann (MsgAnnRefactor "remove the import statement" [(sp, "")])
              (AnnLeaf "Remove import")
        ]
    --TODO: remove the top-level forall from the type.
    ( ProperSpan (SourceSpan fp fl _ _ _),
      match "^((?:Warning: )?Top-level binding with no type signature:\n?)(.*)" -> Just [_, prefix, x])
      | fromGhc -> AnnGroup
        [ AnnLeaf prefix
        , Ann MsgAnnCode (AnnLeaf x)
        , Ann (MsgAnnRefactor
                "insert the type signature above the function declaration"
                [(SourceSpan fp fl 1 fl 1, T.strip x <> "\n")])
              (AnnLeaf "Insert")
        ]
    ( ProperSpan sp,
      match "^(Not in scope:\\s*(?:data constructor|variable|type variable|type constructor or class)?\\s*)([^\\s]+)(.*)$" ->
      Just [_, prefix, var, rest]) -> simplifyAnn $ AnnGroup
        [ AnnLeaf prefix
        , Ann MsgAnnCode (AnnLeaf (stripIdent var))
        , ghcMessage $ AnnGroup $ suggestions rest
        ]
      where
        suggestions
          (match "^(\\s*Perhaps you meant one of these:\\s*)(.*)$" -> Just [_, prefix', rest']) =
            AnnLeaf prefix' :
            intercalate [AnnLeaf "\n"] (map process (T.lines rest'))
        suggestions
          (match "^(\\s*Perhaps you meant\\s*)(.*)$" -> Just [_, prefix', rest']) =
            AnnLeaf prefix' : process rest'
        suggestions rest' = [AnnLeaf  rest']
        process :: Text -> [Ann MsgAnn]
        process (match "(\\s*)([^\\s]+)(.*)" -> Just [_, prefix', var', rest']) =
            [ AnnLeaf prefix'
            , Ann (MsgAnnRefactor "replace the identifier with this option" [(sp, stripIdent var')])
                  (AnnLeaf (stripIdent var'))
            , AnnLeaf rest'
            ]
        process l = [AnnLeaf l]
{-
    --TODO: Do something about GHC's bizarre notation when literals don't match:
    --   #x with #x `notElem` [0#]
    ( ProperSpan sp,
      match "^(Warning: Pattern match\\(es\\) are non-exhaustive\\s*In (?:a|an) ([^:$]+):\\s+Patterns not matched:)(.*)" ->
      Just [_, body, context, rest]) ->
        case context of
            "case alternative" -> success
            "multi-way if alternative" -> success
            (match "equation for ([^:$]+)" -> Just [_, x]) -> success
            _ -> AnnLeaf original
      where
        --TODO: rather tricky to get the patterns to insert, because if "rest" has
        -- more than one line, then the patterns need to be dedented first.
        -- Also need to strip out the "with" business.
        success f = AnnGroup $ AnnLeaf txt ++ map f (matchAll "\\s*")
-}
    _ -> simplifyAnn
        . (if fromGhc then ghcMessage else id)
        $ AnnLeaf (errorMsg err)
  where
    fromGhc = errorKind err `elem` [KindError, KindWarning]
    ghcMessage
        = onAnnLeafs addCodeBoxes
        -- . onAnnLeafs (addExtensionInserts esp)
        . onAnnLeafs collapseExtras

onAnnLeafs :: (Text -> Ann a) -> Ann a -> Ann a
onAnnLeafs f (Ann x inner) = Ann x (onAnnLeafs f inner)
onAnnLeafs f (AnnGroup xs) = AnnGroup (map (onAnnLeafs f) xs)
onAnnLeafs f (AnnLeaf l) = f l

{- FIXME: enable this (issue is it depends on cabal for the list of extensions)

addExtensionInserts :: EitherSpan -> Text -> Ann MsgAnn
addExtensionInserts (ProperSpan sp) txt =
    substAll' extensionRegex txt $ \ext ->
        let beginning = SourceSpan (spanFilePath sp) 1 1 1 1
         in Ann (MsgAnnRefactor "insert language extension" [(beginning, "{-# LANGUAGE " <> ext <> " #-}\n")])
                (AnnLeaf ext)
addExtensionInserts _ x = AnnLeaf x

extensionRegex :: Text
extensionRegex =
    "(?:-X)?(" <>
    T.intercalate "|" (map tshow ([minBound..maxBound] :: [KnownExtension])) <>
    ")"
-}

-- Finds a suffix of the message that should be collapsed as it's of limited
-- utility.  This includes poor suggestions ("add an instance declaration") and
-- information giving the context in the AST.
collapseExtras :: Text -> Ann MsgAnn
collapseExtras txt =
    case match ("(.*?)(\n\\s*(?:" <> T.intercalate "|" pats <> ").*)") txt of
        Just [_, prefix, collapsed] ->
          AnnGroup
            [ AnnLeaf prefix
            , Ann MsgAnnCollapse (AnnLeaf collapsed)
            ]
        _ -> AnnLeaf txt
  where
    pats =
        [ "Possible fix: add an instance declaration for"
        , "Or add an instance declaration"
        -- Match contexts - do an exact search for "\"In " in the code to see
        -- that these match all the usages.  I'm not 100% sure these are always
        -- only used at the end of errors, but it seems like it.
        , "In [the|a|an|some]"
        ]

addCodeBoxes :: Text -> Ann MsgAnn
addCodeBoxes txt =
    substAll' "((?:`|‘)[^\\s]+(?:'|’))" txt $ \code ->
        Ann MsgAnnCode (AnnLeaf (stripIdent code))

-- | Merge adjacent leafs and groups, remove degenerate groups
--
-- TODO: instead do this with smart constructors?
simplifyAnn :: Ann a -> Ann a
simplifyAnn = id -- FIXME

stripIdent :: Text -> Text
-- GHC < 7.8
stripIdent (T.stripPrefix "`" -> Just (T.stripSuffix "'" -> Just x)) = x
-- GHC >= 7.8
stripIdent (T.stripPrefix "‘" -> Just (T.stripSuffix "’" -> Just x)) = x
stripIdent x = x

-- These are inlined so that the regex gets lifted out to top level,
-- and only gets compiled once.

substAll' :: Text -> Text -> (Text -> Ann a) -> Ann a
substAll' r txt f = substAll r txt $ \m ->
    case (ICU.group 0 m, ICU.group 1 m) of
        -- Shouldn't happen
        (Nothing, Nothing) -> []
        (_, Just x) -> [f x]
        (Just m', _) -> [AnnLeaf m']
{-# INLINE substAll' #-}

substAll :: Text -> Text -> (ICU.Match -> [Ann a]) -> Ann a
substAll r txt f = case (headMay matches, lastMay matches) of
    (Just start, Just end)
       -> AnnGroup
        . concat
        . maybeToList
        . teaspoonWithHandles icuHandles
        $ map (AnnLeaf) (maybeToList (ICU.prefix 1 start)) ++
          f start ++
          concatMap (\m -> (AnnLeaf (ICU.span m)) : f m) (tail matches) ++
          map (AnnLeaf) (maybeToList (ICU.suffix (ICU.groupCount compiled) end))
    _ -> AnnLeaf txt
  where
    matches = ICU.findAll compiled txt
    compiled = ICU.regex matchOpts r
{-# INLINE substAll #-}

match :: Text -> Text -> Maybe [Text]
match r
    = join
    . teaspoonWithHandles icuHandles
    . fmap (ICU.unfold ICU.group)
    . ICU.find (ICU.regex matchOpts r)
{-# INLINE match #-}

matchOpts :: [ICU.MatchOption]
matchOpts = [ICU.HaskellLines, ICU.DotAll, ICU.ErrorOnUnknownEscapes, ICU.WorkLimit 20]

icuHandles :: Handles a
icuHandles =
    [ Handler $ \(err :: ICU.ICUError) -> putStrLn ("ICU Error:" ++ show err) >> return Nothing
    , Handler $ \(err :: SomeException) -> throwIO err
    ]
