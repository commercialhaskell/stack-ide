{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

-- The JSON api that we expose
--
-- Some general design principles:
--
-- * We have a single Request type for requests sent by the editor to the client
--   and a single Response type for responses sent back from the client to the
--   editor.
--
-- * We try to roughly have a one-to-one mapping between the functions exported
--   by IdeSession; for instance, ide-backend's `updateSourceFileFromFile`
--   corresponds to the `RequestUpdateSourceFileFromFile` request and the
--   `ResponseUpdateSourceFileFromFile` response.
--
-- Hopefully with these principles in place the API should be predictable,
-- stable, and well documented.

module Stack.Ide.JsonAPI (
    -- * Requests
    Request(..)
  , RequestSessionUpdate(..)
  , Response(..)
  , ResponseSpanInfo(..)
  , ResponseExpType(..)
  , ResponseAnnExpType(..)
  , Ann(..)
  , CodeAnn(..)
  , AnnSourceError(..)
  , MsgAnn(..)
  , CodeVariety(..)
  , AutocompletionSpan(..)
  , AutocompletionInfo(..)
  , VersionInfo(..)
  , Identifier
  , Targets(..)
  , Sequenced(..), unsequenced, withSameSeqAs
  , sliceSpans
  ) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy      as Lazy
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.Encoding as T
import IdeSession.Types.Progress
import IdeSession.Types.Public hiding (idProp, Value)
import Stack.Ide.JsonAPI.TH

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Messages sent from the editor to the client
data Request =
  -- Update
    RequestUpdateSession [RequestSessionUpdate]
  -- Query
  | RequestGetSourceErrors
  | RequestGetAnnSourceErrors
  | RequestGetLoadedModules
  | RequestGetSpanInfo SourceSpan
  | RequestGetExpTypes SourceSpan
  | RequestGetAnnExpTypes SourceSpan
  | RequestGetAutocompletion AutocompletionSpan
  -- Run
  | RequestRun Bool ModuleName Identifier
  | RequestProcessInput String
  | RequestProcessKill
  -- Misc
  | RequestShutdownSession
  deriving (Show, Eq)

-- | Session updates
data RequestSessionUpdate
  = RequestUpdateTargets Targets
  | RequestSessionUpdate -- Simply calls updateSession with no changes to trigger a recompile.
  deriving (Show, Eq)

-- TODO:
-- RequestUpdateGhcOpts [String]
-- RequestUpdateRtsOpts [String]
-- RequestUpdateRelativeIncludes [h]
-- RequestUpdateStdoutBufferMode
-- RequestUpdateStderrBufferMode

-- | Messages sent back from the client to the editor
data Response =
    -- | Sent on session initialization
    ResponseWelcome VersionInfo
    -- | Nothing indicates the update completed
  | ResponseUpdateSession UpdateStatus
  | ResponseGetSourceErrors [SourceError]
  | ResponseGetAnnSourceErrors [AnnSourceError]
  | ResponseGetLoadedModules [ModuleName]
  | ResponseGetSpanInfo [ResponseSpanInfo]
  | ResponseGetExpTypes [ResponseExpType]
  | ResponseGetAnnExpTypes [ResponseAnnExpType]
  | ResponseGetAutocompletion [AutocompletionInfo]
  -- Run
  | ResponseProcessOutput String
  | ResponseProcessDone RunResult
  | ResponseNoProcessError
  -- Misc
  | ResponseLog Text
  | ResponseInvalidRequest String
  | ResponseFatalError String
  | ResponseShutdownSession
  deriving (Show, Eq)

data ResponseSpanInfo =
    ResponseSpanInfo SpanInfo SourceSpan
  deriving (Show, Eq)

data ResponseExpType =
    ResponseExpType Text SourceSpan
  deriving (Show, Eq)

data ResponseAnnExpType =
    ResponseAnnExpType (Ann CodeAnn) SourceSpan
  deriving (Show, Eq)

data MsgAnn =
    MsgAnnModule
  | MsgAnnCode CodeVariety
  | MsgAnnCodeAnn CodeAnn
  | MsgAnnRefactor Text [(SourceSpan, Text)]
  | MsgAnnCollapse
  deriving (Show, Eq)

data Ann a =
    Ann a (Ann a)
  | AnnGroup [Ann a]
  | AnnLeaf Text
  deriving (Show, Eq, Functor)

data CodeAnn =
    CodeIdInfo IdInfo
  deriving (Show, Eq)

data AnnSourceError = AnnSourceError
  { annErrorKind :: !SourceErrorKind
  , annErrorSpan :: !EitherSpan
  , annErrorMsg :: !(Ann MsgAnn)
  }
  deriving (Show, Eq)

data AutocompletionSpan = AutocompletionSpan
   { autocompletionFilePath :: FilePath
   , autocompletionPrefix :: String
   }
  deriving (Show, Eq)

data AutocompletionInfo = AutocompletionInfo
   { autocompletionInfoDefinedIn :: Text
   , autocompletionInfoName :: Text
   , autocompletionQualifier :: Maybe Text
   , autocompletionType :: Maybe Text
   }
  deriving (Show, Eq)

data CodeVariety =
    ExpCode
  | TypeCode
  | UnknownCode
  -- ^ When there isn't any id info, we don't know if it's an
  -- expression or type.
  | AmbiguousCode (Ann MsgAnn)
  -- ^ When we can't tell whether the code is an expression or type,
  -- default to yielding type id info.  The expression annotated code
  -- is yielded in this annotation.
  deriving (Show, Eq)

-- | Client version
--
-- Standard versioning applies (major, minor, patch)
data VersionInfo =
    VersionInfo Int Int Int
  deriving (Show, Eq)

type Identifier = Text

-- | No idea. Exported for client-side and server-side code.
sliceSpans :: Int -> Text -> [(Int, Int, a)] -> [(Text, Maybe a)]
sliceSpans _ txt _ | Text.null txt = []
sliceSpans _ txt [] = [(txt, Nothing)]
sliceSpans ix txt ((fr, to, x) : xs) =
    appendNonNull before Nothing $
    appendNonNull chunk (Just x) $
    sliceSpans to rest' xs
  where
    appendNonNull t mx = if Text.null t then id else ((t, mx) :)
    (chunk, rest') = Text.splitAt ((to - ix) - fr') rest
    (before, rest) = Text.splitAt fr' txt
    fr' = max 0 (fr - ix)


-- | An extension of messages with an optional sequence code,
--   an uninterpreted JSON value. See (#39) for motivation.
data Sequenced a =
    NoSeq  a
  | HasSeq Value a
  deriving (Show, Eq, Functor)

unsequenced :: Sequenced a -> a
unsequenced (NoSeq    a) = a
unsequenced (HasSeq _ a) = a

withSameSeqAs :: Sequenced a -> b -> Sequenced b
withSameSeqAs sa b = b <$ sa

instance ToJSON a => ToJSON (Sequenced a) where
  toJSON (NoSeq    a) = toJSON a
  toJSON (HasSeq s a) =
    case toJSON a of
      Object o -> Object (H.insert "seq" s o)
      json_a   -> json_a

instance FromJSON a => FromJSON (Sequenced a) where
  parseJSON x
    = case x of
        Object o | Just s <- H.lookup "seq" o ->
          HasSeq s <$> parseJSON (Object $ H.delete "seq" o)
        _ ->
          NoSeq <$> parseJSON x

--------------------------------------------------------------------------------
-- For the moment we use Aeson's built-in instance deriver

$(concat <$> mapM (deriveJSON defaultOptions)
  [ ''Ann
  , ''AnnSourceError
  , ''AutocompletionInfo
  , ''AutocompletionSpan
  , ''CodeAnn
  , ''CodeVariety
  , ''MsgAnn
  , ''Request
  , ''RequestSessionUpdate
  , ''Response
  , ''ResponseAnnExpType
  , ''ResponseExpType
  , ''ResponseSpanInfo
  , ''Targets
  , ''VersionInfo
  ])
