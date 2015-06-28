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

module IdeSession.Client.JsonAPI (
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
  , RequestTargets(..)
  , Orphan(..)
  , sliceSpans
  ) where

import qualified Data.ByteString.Char8 as S8
import Control.Applicative
import Data.Aeson hiding ((.:))
import qualified Data.Aeson ((.:?))
import Data.Aeson.Types hiding ((.:))
import qualified Data.Aeson.Types          as Aeson
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as Lazy
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import IdeSession.Client.JsonAPI.Aux
import IdeSession.Types.Progress
import IdeSession.Types.Public hiding (idProp, Value)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as L

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
  | RequestProcessInput BS.ByteString
  | RequestProcessKill
  -- Misc
  | RequestShutdownSession

instance FromJSON Request where
    parseJSON j = do
        o <- parseJSON j
        cmd <- o .: "tag" :: Parser Text
        case cmd of
            "UpdateSession" -> do
                updates <- o .: "payload"
                pure (RequestUpdateSession updates)
            "GetSourceErrors" -> pure RequestGetSourceErrors
            "GetAnnSourceErrors" -> pure RequestGetAnnSourceErrors
            "GetLoadedModules" -> pure RequestGetLoadedModules
            "GetSpanInfo" -> do
                span <- o .: "payload"
                pure (RequestGetSpanInfo span)
            "GetExpTypes" -> do
                span <- o .: "payload"
                pure (RequestGetExpTypes span)
            "GetAnnExpTypes" -> do
                span <- o .: "payload"
                pure (RequestGetAnnExpTypes span)
            "GetAutocompletion" -> do
                span <- o .: "payload"
                pure (RequestGetAutocompletion span)
            "Run" -> do
                payload <- o .: "payload"
                pty <- payload .: "usePty"
                mn <- payload .: "moduleName"
                ident <- payload .: "identifier"
                pure (RequestRun pty mn ident)
            "ProcessInput" -> do
                i <- o .: "payload"
                pure (RequestProcessInput (S8.pack i))
            "ProcessKill" -> pure RequestProcessKill
            "ShutdownSession" -> pure RequestShutdownSession
            _ -> fail (T.unpack ("Invalid command type: " <> cmd))

-- | Session updates
data RequestSessionUpdate
  = RequestUpdateTargets RequestTargets

-- | Wrapper to avoid orphan instances.
newtype RequestTargets = RequestTargets
    { unTargets :: Targets
    }

instance FromJSON RequestTargets where
    parseJSON j = do
        o <- parseJSON j
        tag <- o .: "tag" :: Parser Text
        case tag of
            "include" -> do
                fps <- o .: "payload"
                pure (RequestTargets (TargetsInclude fps))
            "exclude" -> do
                fps <- o .: "payload"
                pure (RequestTargets (TargetsExclude fps))

-- TODO:
-- RequestUpdateGhcOpts [String]
-- RequestUpdateRtsOpts [String]
-- RequestUpdateRelativeIncludes [FilePath]
-- RequestUpdateCodeGeneration Bool
-- RequestUpdateEnv [(String, Maybe String)]
-- RequestUpdateArgs [String]
-- RequestUpdateStdoutBufferMode
-- RequestUpdateStderrBufferMode

instance FromJSON RequestSessionUpdate where
    parseJSON j = do
        o <- parseJSON j
        cmd <- o .: "tag" :: Parser Text
        case cmd of
            "UpdateTargets" -> do
                targets <- o .: "payload"
                pure (RequestUpdateTargets targets)
            _ -> fail (T.unpack ("Invalid session update type: " <> cmd))

-- | Messages sent back from the client to the editor
data Response =
    -- | Sent on session initialization
    ResponseWelcome VersionInfo
    -- | Nothing indicates the update completed
  | ResponseUpdateSession (Maybe (Orphan Progress))
  | ResponseGetSourceErrors [SourceError]
  | ResponseGetAnnSourceErrors [AnnSourceError]
  | ResponseGetLoadedModules [ModuleName]
  | ResponseGetSpanInfo [ResponseSpanInfo]
  | ResponseGetExpTypes [ResponseExpType]
  | ResponseGetAnnExpTypes [ResponseAnnExpType]
  | ResponseGetAutocompletion [AutocompletionInfo]
  -- Run
  | ResponseProcessOutput BS.ByteString
  | ResponseProcessDone RunResult
  | ResponseNoProcessError
  -- Misc
  | ResponseInvalidRequest String
  | ResponseShutdownSession

instance ToJSON Response where
    toJSON o =
        case o of
            ResponseWelcome versionInfo ->
                object ["tag" .= "Welcome", "payload" .= versionInfo]
            ResponseUpdateSession mprogress ->
                object
                    (concat
                         [ ["tag" .= "UpdateSession"]
                         , ["payload" .= p | Just p <- [mprogress]]])
            ResponseGetSourceErrors errs ->
                object ["tag" .= "GetSourceErrors", "payload" .= errs]
            ResponseGetAnnSourceErrors anerrs ->
                object ["tag" .= "GetAnnSourceErrors", "payload" .= anerrs]
            ResponseGetLoadedModules ms ->
                object ["tag" .= "GetLoadedModules", "payload" .= ms]
            ResponseGetSpanInfo infos ->
                object ["tag" .= "GetSpanInfo", "payload" .= infos]
            ResponseGetExpTypes types ->
                object ["tag" .= "GetExpTypes", "payload" .= types]
            ResponseGetAnnExpTypes anntypes ->
                object ["tag" .= "GetAnnExpTypes", "payload" .= anntypes]
            ResponseGetAutocompletion autoinfos ->
                object ["tag" .= "GetAutocompletion", "payload" .= autoinfos]
            ResponseProcessOutput bs ->
                object ["tag" .= "ProcessOutput", "payload" .= S8.unpack bs]
            ResponseProcessDone result ->
                object ["tag" .= "ProcessDone", "payload" .= result]
            ResponseNoProcessError -> object ["tag" .= "NoProcessError"]
            ResponseInvalidRequest err ->
                object ["tag" .= "InvalidRequest", "payload" .= err]
            ResponseShutdownSession -> object ["tag" .= "ShutdownSession"]

instance ToJSON (Orphan Progress) where
    toJSON (Orphan (Progress step num parsed orig)) =
        object
            [ "tag" .= "progress"
            , "payload" .=
              object
                  (concat
                       [ ["step" .= step, "num" .= num]
                       , ["parsed" .= p | Just p <- [parsed]]
                       , ["orig" .= o | Just o <- [orig]]])]

data ResponseSpanInfo =
    ResponseSpanInfo SpanInfo SourceSpan

instance ToJSON ResponseSpanInfo where
    toJSON (ResponseSpanInfo info span) =
        object
            [ "tag" .= "SpanInfo"
            , "payload" .= object ["info" .= info, "span" .= span]]

data ResponseExpType =
    ResponseExpType Text SourceSpan

instance ToJSON ResponseExpType where
    toJSON (ResponseExpType text span) =
        object
            [ "tag" .= "ExpType"
            , "payload" .= object ["text" .= text, "span" .= span]]

data ResponseAnnExpType =
    ResponseAnnExpType (Ann CodeAnn) SourceSpan

instance ToJSON ResponseAnnExpType where
    toJSON (ResponseAnnExpType ann span) =
        object
            [ "tag" .= "ExpType"
            , "payload" .= object ["ann" .= ann, "span" .= span]]

data Ann a =
    Ann a (Ann a)
  | AnnGroup [Ann a]
  | AnnLeaf Text
  deriving (Functor)

instance ToJSON a => ToJSON (Ann a) where
    toJSON (Ann a x) = object ["tag" .= "Ann", "payload" .= x]
    toJSON (AnnGroup xs) = object ["tag" .= "AnnGroup", "payload" .= xs]
    toJSON (AnnLeaf t) = object ["tag" .= "AnnLeaf", "payload" .= t]

data CodeAnn =
    CodeIdInfo IdInfo

instance ToJSON CodeAnn where
    toJSON (CodeIdInfo info) =
        object ["tag" .= "CodeAnn", "payload" .= Orphan info]

newtype Orphan a = Orphan
    { orphan :: a
    }

instance ToJSON (Orphan IdInfo) where
    toJSON (Orphan (IdInfo prop scope)) =
        object ["tag" .= "IdInfo", "payload" .= object ["prop" .= prop]]

data AnnSourceError = AnnSourceError
  { annErrorKind :: !SourceErrorKind
  , annErrorSpan :: !EitherSpan
  , annErrorMsg :: !(Ann MsgAnn)
  }

instance ToJSON AnnSourceError where
    toJSON (AnnSourceError kind span msg) =
        object
            [ "tag" .= "AnnSourceError"
            , "payload" .=
              object ["kind" .= kind, "span" .= span, "msg" .= msg]]

data MsgAnn =
    MsgAnnModule
  | MsgAnnCode CodeVariety
  | MsgAnnCodeAnn CodeAnn
  | MsgAnnRefactor Text [(SourceSpan, Text)]
  | MsgAnnCollapse

instance ToJSON MsgAnn where
    toJSON (MsgAnnModule) = object ["tag" .= "MsgAnnModule"]
    toJSON (MsgAnnCode var) =
        object ["tag" .= "MsgAnnCode", "payload" .= var]
    toJSON (MsgAnnCodeAnn ann) =
        object ["tag" .= "MsgAnnCodeAnn", "payload" .= ann]
    toJSON (MsgAnnRefactor t spans) =
        object
            [ "tag" .= "MsgAnnRefactor"
            , "payload" .= object ["text" .= t, "spans" .= map to spans]]
      where
        to (x,y) =
            object
                [ "tag" .= "Refactor"
                , "payload" .= object ["span" .= x, "text" .= y]]
    toJSON MsgAnnCollapse =
        object ["tag" .= "MsgAnnCollapse"]

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

instance ToJSON CodeVariety where
    toJSON x =
        case x of
          ExpCode -> object ["tag" .= "ExpCode"]
          TypeCode -> object ["tag" .= "TypeCode"]
          UnknownCode -> object ["tag" .= "UnknownCode"]
          AmbiguousCode ann -> object ["tag" .= "AmbiguousCode","payload" .= ann]

data AutocompletionSpan = AutocompletionSpan
   { autocompletionFilePath :: FilePath
   , autocompletionPrefix :: String
   }

instance FromJSON AutocompletionSpan where
    parseJSON j = do
        o <- parseJSON j
        AutocompletionSpan <$> (o .: "filePath") <*> (o .: "prefix")

data AutocompletionInfo = AutocompletionInfo
   { autocompletionInfoDefinedIn :: Text
   , autocompletionInfoName :: Text
   , autocompletionQualifier :: Maybe Text
   , autocompletionType :: Maybe Text
   }

instance ToJSON AutocompletionInfo where
    toJSON (AutocompletionInfo defined name qual typ) =
        object
            [ "tag" .= "AutocompletionInfo"
            , "payload" .=
              object
                  (concat
                       [ [ "definedIn" .= defined
                         , "name" .= name
                         , "qualifier" .= qual
                         , "type" .= typ]
                       , ["qualifier" .= q | Just q <- [qual]]
                       , ["type" .= t | Just t <- [typ]]])]

-- | Client version
--
-- Standard versioning applies (major, minor, patch)
data VersionInfo =
    VersionInfo Int Int Int

instance ToJSON VersionInfo where
    toJSON (VersionInfo major minor patch) =
        object
            [ "tag" .= "VersionInfo"
            , "payload" .=
              object ["major" .= major, "minor" .= minor, "patch" .= patch]]

type Identifier = Text

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

o .: key = do
    mv <- get o key
    case mv of
        Nothing ->
            fail
                (T.unpack
                     ("Expected key '" <> key <> "' in object " <>
                      T.decodeUtf8 (L.toStrict (encode o))))
        Just v -> return v
  where
    get = (Data.Aeson..:?)
