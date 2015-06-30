{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Stack.Ide.AnnotateHaskell
  ( Autocomplete
  , annotateType
  , annotateExp
  ) where

import           Control.Applicative ((<$>))
import           Data.Data (Data, gmapQ)
import           Data.List ((\\), sortBy, find)
import           Data.Maybe (mapMaybe)
import           Data.Ord (comparing)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable, cast)
import           Debug.Trace (trace)
import           Stack.Ide.JsonAPI (Ann(..),sliceSpans)
import           IdeSession.Types.Public (IdInfo(..), IdProp(..), IdNameSpace(..), idInfoQN)
import           Language.Haskell.Exts.Annotated hiding (Ann, VarName)

-- FIXME: Lift the restriction that these only work on little chunks
-- of newlineless code.

type Autocomplete = String -> [IdInfo]

annotateType :: Autocomplete -> Text -> Ann IdInfo
annotateType = annotateHaskell IsType (parseTypeWithMode parseMode)

annotateExp :: Autocomplete -> Text -> Ann IdInfo
annotateExp = annotateHaskell IsValue (parseExpWithMode parseMode)

annotateHaskell
  :: (Data a, Show a)
  => ValueOrType
  -> (String -> ParseResult a)
  -> Autocomplete
  -> Text
  -> Ann IdInfo
annotateHaskell initialVoT parser autoComplete (toOneLineNoTabs -> input) =
  case parser (T.unpack input) of
    res@ParseFailed{} -> warning (show res) (AnnLeaf input)
    ParseOk parsed ->
      AnnGroup $
      map toAnn $
      sliceSpans 0 input $
      map (\(ss', info) ->
            (srcSpanStartColumn ss' - 1, srcSpanEndColumn ss' - 1, info)) $
      sortBy (comparing fst) $
      mapMaybe (\(x, vot) -> (srcInfoSpan (ann x), ) <$> exactLookup vot autoComplete (prettyPrint x)) $
      getQNames initialVoT parsed
  where
    toAnn (chunk, Nothing) = AnnLeaf chunk
    toAnn (chunk, Just x) = Ann x $ AnnLeaf chunk

data ValueOrType =
    IsValue
  | IsType
  deriving (Eq, Show)

getQNames :: Data a => ValueOrType -> a -> [(QName SrcSpanInfo, ValueOrType)]
getQNames vot =
    -- Default: recurse.
    recurseWith vot `extQ`
    -- Set to IsValue when recursing down Exp.
    (\(x :: Exp SrcSpanInfo) -> recurseWith IsValue x) `extQ`
    -- Set to IsType when recursing down Type.
    (\(x :: Type SrcSpanInfo) -> recurseWith IsType x) `extQ`
    -- Avoid recursing down strings.
    (\(_ :: String) -> []) `extQ`
    -- Yield all encountered QNames.
    (\qn -> [(qn, vot)])
  where
    -- recurseWith:: Data a => ValueOrType -> a -> [(QName SrcSpanInfo, ValueOrType)]
    recurseWith vot' = concat . gmapQ (getQNames vot')

exactLookup :: ValueOrType -> Autocomplete -> String -> Maybe IdInfo
exactLookup vot autoComplete ident =
    case filter predicate (autoComplete ident) of
      [] -> Nothing
      [unique] -> Just unique
      (find preferred -> Just result) ->
        warning ("Info ambiguous for " ++ ident ++ ", but there is a preferred result") (Just result)
      xs -> warning ("Info ambiguous for " ++ ident ++ ": " ++ show xs) Nothing
  where
    -- Why the asymmetry here, where there are only conditions when in
    -- an expression?  It's because:
    --
    --   * DataKinds allows data constructors in types.
    --
    --   * Type variables use 'Name' not 'QName'.
    --
    -- This means that all 'QName' matches are valid in types,
    -- regardless of space.
    predicate x = nameMatches ident x &&
      (if vot == IsValue then getSpace x `elem` valSpace else True)
    -- However, when ambiguous, we prefer results that come from the
    -- appropriate namespace.
    preferred x = case vot of
      IsValue -> getSpace x `elem` valSpace
      IsType -> getSpace x `elem` typeSpace
    getSpace = idSpace . idProp
    valSpace = [VarName, DataName]
    typeSpace = [TvName, TcClsName]

nameMatches :: String -> IdInfo -> Bool
nameMatches ident info =
  (T.pack ident == idName (idProp info)) ||
  (ident == idInfoQN info)

extQ :: (Typeable a, Typeable b) => (a -> q) -> (b -> q) -> a -> q
extQ f g a = maybe (f a) g (cast a)

-- NOTE: This sanitization is probably unnecessary, I don't think
-- ide-backend output will include tabs or newlines.  However, it
-- seems prudent.
toOneLineNoTabs :: Text -> Text
toOneLineNoTabs = T.map tabToSpace . T.unwords . T.lines
  where
    tabToSpace '\t' = ' '
    tabToSpace x = x

--TODO: better way to throw warnings than using trace?
warning :: String -> a -> a
warning msg = trace ("AnnotateTypeInfo warning: " ++ msg)

-- Copied from
-- https://github.com/chrisdone/structured-haskell-mode/blob/0c92facc8c0e8b603edbb78b5848ec4fa97e0a84/src/Main.hs#L131
--
-- Modification: allow UnboxedTuples

-- | Parse mode, includes all extensions, doesn't assume any fixities.
parseMode :: ParseMode
parseMode =
  defaultParseMode {extensions = defaultExtensions
                   ,fixities = Nothing}

-- | Default extensions.
defaultExtensions :: [Extension]
defaultExtensions =
  [e | e@EnableExtension{} <- knownExtensions] \\ map EnableExtension badExtensions

-- | Extensions which steal too much syntax.
badExtensions :: [KnownExtension]
badExtensions =
  [Arrows -- steals proc
  ,TransformListComp -- steals the group keyword
  ,XmlSyntax, RegularPatterns -- steals a-b
  -- ,UnboxedTuples -- breaks (#) lens operator
  -- ,QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
  ]
