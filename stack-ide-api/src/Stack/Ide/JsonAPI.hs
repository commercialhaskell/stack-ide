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
  , sliceSpans
  ) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy      as Lazy
import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.Encoding as T
import IdeSession.Types.Progress
import IdeSession.Types.Public hiding (idProp, Value)
import Stack.Ide.JsonAPI.Aux

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
  | ResponseUpdateSession (Maybe Progress)
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
  | ResponseInvalidRequest String
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


--------------------------------------------------------------------------------
-- For the moment we use Aeson's built-in instance deriver

$(deriveJSON defaultOptions ''AnnSourceError)
$(deriveJSON defaultOptions ''MsgAnn)
$(deriveJSON defaultOptions ''CodeVariety)
$(deriveJSON defaultOptions ''AutocompletionInfo)
$(deriveJSON defaultOptions ''AutocompletionSpan)
$(deriveJSON defaultOptions ''RequestSessionUpdate)
$(deriveJSON defaultOptions ''Progress)
$(deriveJSON defaultOptions ''Targets)
$(deriveJSON defaultOptions ''VersionInfo)
$(deriveJSON defaultOptions ''Request)
$(deriveJSON defaultOptions ''Response)
$(deriveJSON defaultOptions ''ResponseSpanInfo)
$(deriveJSON defaultOptions ''ResponseExpType)
$(deriveJSON defaultOptions ''ResponseAnnExpType)
$(deriveJSON defaultOptions ''Ann)
$(deriveJSON defaultOptions ''CodeAnn)
