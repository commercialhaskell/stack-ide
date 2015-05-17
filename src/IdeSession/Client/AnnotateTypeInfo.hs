{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module IdeSession.Client.AnnotateTypeInfo (annotateTypeInfo) where

import           Control.Applicative ((<$>))
import           Data.Data (Data, gmapQ)
import           Data.List ((\\), sortBy)
import           Data.Maybe (mapMaybe)
import           Data.Ord (comparing)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable, cast)
import           Debug.Trace (trace)
import           IdeSession.Client.JsonAPI (ResponseAnnExpType(..), AnnSpan(..), TypeAnn(..))
import           IdeSession.Types.Public (SourceSpan(..), IdInfo(..), IdProp(..), idInfoQN)
import           Language.Haskell.Exts.Annotated hiding (Ann)

--TODO: better way to throw warnings than using trace?
warning :: String -> a -> a
warning msg = trace ("AnnotateTypeInfo warning: " ++ msg)

annotateTypeInfo
 :: (String -> [IdInfo])
 -> (SourceSpan, Text)
 -> ResponseAnnExpType
annotateTypeInfo autoComplete (ss, typ) =
    ResponseAnnExpType anns typ ss
  where
    anns = case parseTypeWithMode parseMode (T.unpack typ) of
      res@ParseFailed{} -> warning (show res) []
      ParseOk parsed ->
        -- Note: assuming info is on one line (seems to be true).
        map (\(ss', info) -> AnnSpan (srcSpanStartColumn ss' - 1)
                                     (srcSpanEndColumn ss' - 1)
                                     (TypeIdInfo info)) $
        sortBy (comparing fst) $
        mapMaybe (\x -> (srcInfoSpan (ann x), ) <$> exactLookup autoComplete (prettyPrint x)) $
        getQNames parsed

exactLookup :: (String -> [IdInfo]) -> String -> Maybe IdInfo
exactLookup autoComplete ident =
  case filter (nameMatches ident) (autoComplete ident) of
    [] -> Nothing
    [unique] -> Just unique
    xs -> warning ("Info ambiguous for " ++ ident ++ ": " ++ show xs) Nothing

nameMatches :: String -> IdInfo -> Bool
nameMatches ident info =
  (ident == T.unpack (idName (idProp info))) ||
  (ident == idInfoQN info)

getQNames :: Data a => a -> [QName SrcSpanInfo]
getQNames =
  (concat . gmapQ getQNames) `extQ`
  -- Avoid recursing down strings
  (\(_ :: String) -> []) `extQ`
  (\qn -> [qn])

extQ :: (Typeable a, Typeable b) => (a -> q) -> (b -> q) -> a -> q
extQ f g a = maybe (f a) g (cast a)

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
  [e | e@EnableExtension{} <- knownExtensions] \\
  map EnableExtension badExtensions

-- | Extensions which steal too much syntax.
badExtensions :: [KnownExtension]
badExtensions =
  [Arrows -- steals proc
  ,TransformListComp -- steals the group keyword
  ,XmlSyntax, RegularPatterns -- steals a-b
  -- ,UnboxedTuples -- breaks (#) lens operator
  -- ,QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
  ]
