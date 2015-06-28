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
import Stack.Ide.JsonAPI.Aux
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
  | RequestGetSpanInfo (Orphan SourceSpan)
  | RequestGetExpTypes (Orphan SourceSpan)
  | RequestGetAnnExpTypes (Orphan SourceSpan)
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
                updates <- o .: "contents"
                pure (RequestUpdateSession updates)
            "GetSourceErrors" -> pure RequestGetSourceErrors
            "GetAnnSourceErrors" -> pure RequestGetAnnSourceErrors
            "GetLoadedModules" -> pure RequestGetLoadedModules
            "GetSpanInfo" -> do
                span <- o .: "contents"
                pure (RequestGetSpanInfo span)
            "GetExpTypes" -> do
                span <- o .: "contents"
                pure (RequestGetExpTypes span)
            "GetAnnExpTypes" -> do
                span <- o .: "contents"
                pure (RequestGetAnnExpTypes span)
            "GetAutocompletion" -> do
                span <- o .: "contents"
                pure (RequestGetAutocompletion span)
            "Run" -> do
                payload <- o .: "contents"
                pty <- payload .: "usePty"
                mn <- payload .: "moduleName"
                ident <- payload .: "identifier"
                pure (RequestRun pty mn ident)
            "ProcessInput" -> do
                i <- o .: "contents"
                pure (RequestProcessInput (S8.pack i))
            "ProcessKill" -> pure RequestProcessKill
            "ShutdownSession" -> pure RequestShutdownSession
            _ -> fail (T.unpack ("Invalid command type: " <> cmd))

instance FromJSON (Orphan SourceSpan) where
    parseJSON j = do
        o <- parseJSON j
        filePath <- o .: "filePath"
        fromLine <- o .: "fromLine"
        fromColumn <- o .: "fromColumn"
        toLine <- o .: "toLine"
        toColumn <- o .: "toColumn"
        return (Orphan (SourceSpan filePath fromLine fromColumn toLine toColumn))

instance ToJSON (Orphan SourceSpan) where
    toJSON (Orphan (SourceSpan filePath fromLine fromColumn toLine toColumn)) =
        object
            [ "filePath" .= filePath
            , "fromLine" .= fromLine
            , "fromColumn" .= fromColumn
            , "toLine" .= toLine
            , "toColumn" .= toColumn]

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
                fps <- o .: "contents"
                pure (RequestTargets (TargetsInclude fps))
            "exclude" -> do
                fps <- o .: "contents"
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
                targets <- o .: "contents"
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
                object ["tag" .= "Welcome", "contents" .= versionInfo]
            ResponseUpdateSession mprogress ->
                object
                    (concat
                         [ ["tag" .= "UpdateSession"]
                         , ["contents" .= p | Just p <- [mprogress]]])
            ResponseGetSourceErrors errs ->
                object ["tag" .= "GetSourceErrors", "contents" .= errs]
            ResponseGetAnnSourceErrors anerrs ->
                object ["tag" .= "GetAnnSourceErrors", "contents" .= anerrs]
            ResponseGetLoadedModules ms ->
                object ["tag" .= "GetLoadedModules", "contents" .= ms]
            ResponseGetSpanInfo infos ->
                object ["tag" .= "GetSpanInfo", "contents" .= infos]
            ResponseGetExpTypes types ->
                object ["tag" .= "GetExpTypes", "contents" .= types]
            ResponseGetAnnExpTypes anntypes ->
                object ["tag" .= "GetAnnExpTypes", "contents" .= anntypes]
            ResponseGetAutocompletion autoinfos ->
                object ["tag" .= "GetAutocompletion", "contents" .= autoinfos]
            ResponseProcessOutput bs ->
                object ["tag" .= "ProcessOutput", "contents" .= S8.unpack bs]
            ResponseProcessDone result ->
                object ["tag" .= "ProcessDone", "contents" .= result]
            ResponseNoProcessError -> object ["tag" .= "NoProcessError"]
            ResponseInvalidRequest err ->
                object ["tag" .= "InvalidRequest", "contents" .= err]
            ResponseShutdownSession -> object ["tag" .= "ShutdownSession"]

instance ToJSON (Orphan Progress) where
    toJSON (Orphan (Progress step num parsed orig)) =
        object
            [ "tag" .= "progress"
            , "contents" .=
              object
                  (concat
                       [ ["step" .= step, "num" .= num]
                       , ["parsed" .= p | Just p <- [parsed]]
                       , ["orig" .= o | Just o <- [orig]]])]

data ResponseSpanInfo =
    ResponseSpanInfo (Orphan SpanInfo) (Orphan SourceSpan)

instance ToJSON ResponseSpanInfo where
    toJSON (ResponseSpanInfo info span) =
        object
            [ "tag" .= "SpanInfo"
            , "contents" .= object ["info" .= info, "span" .= span]]

instance ToJSON (Orphan SpanInfo) where
    toJSON (Orphan sum) =
        case sum of
            SpanId id ->
                object ["tag" .= "SpanId", "contents" .= toJSON (Orphan id)]
            SpanQQ id ->
                object ["tag" .= "SpanQQ", "contents" .= toJSON (Orphan id)]

instance ToJSON (Orphan IdInfo) where
    toJSON (Orphan (IdInfo prop scope)) =
        object
            [ "tag" .= "IdInfo"
            , "contents" .=
              object ["prop" .= Orphan prop, "scope" .= Orphan scope]]

instance ToJSON (Orphan IdProp) where
    toJSON (Orphan (IdProp name space typ defined defspan home)) =
        object
            [ "tag" .= "IdProp"
            , "contents" .=
              object
                  (concat
                       [ [ "name" .= (name :: Text)
                         , "space" .= Orphan space
                         , "definedIn" .= Orphan defined
                         , "defSpan" .= Orphan defspan]
                       , ["type" .= (t :: Text) | Just t <- [typ]]
                       , ["homeModule" .= Orphan t | Just t <- [home]]])]

instance ToJSON (Orphan IdNameSpace) where
    toJSON (Orphan sum) =
        object ["tag" .= case sum of
                           VarName -> "VarName"
                           DataName -> "DataName"
                           TvName -> "TvName"
                           TcClsName -> "TcClsName"]

instance ToJSON (Orphan IdScope) where
    toJSON (Orphan sum) =
        case sum of
            Binder -> object ["tag" .= "Binder"]
            Local -> object ["tag" .= "Local"]
            Imported from span qual ->
                object
                    [ "tag" .= "Imported"
                    , "contents" .=
                      object
                          [ "from" .= Orphan from
                          , "span" .= Orphan span
                          , "qual" .= qual]]

instance ToJSON (Orphan EitherSpan) where
    toJSON (Orphan sum) =
        case sum of
            ProperSpan srcspan ->
                object ["tag" .= "ProperSpan", "contents" .= Orphan srcspan]
            TextSpan text -> object ["tag" .= "TextSpan", "contents" .= text]

instance ToJSON (Orphan ModuleId) where
    toJSON (Orphan (ModuleId mo pkg)) =
        object
            [ "tag" .= "ModuleId"
            , "contents" .= object ["name" .= mo, "package" .= Orphan pkg]]

instance ToJSON (Orphan PackageId) where
    toJSON (Orphan (PackageId name ver key)) =
        object
            [ "tag" .= "PackageId"
            , "contents" .=
              object
                  (concat
                       [ ["name" .= name, "key" .= key]
                       , ["version" .= v | Just v <- [ver]]])]

data ResponseExpType =
    ResponseExpType Text (Orphan SourceSpan)

instance ToJSON ResponseExpType where
    toJSON (ResponseExpType text span) =
        object
            [ "tag" .= "ExpType"
            , "contents" .= object ["text" .= text, "span" .= span]]

data ResponseAnnExpType =
    ResponseAnnExpType (Ann CodeAnn) (Orphan SourceSpan)

instance ToJSON ResponseAnnExpType where
    toJSON (ResponseAnnExpType ann span) =
        object
            [ "tag" .= "ExpType"
            , "contents" .= object ["ann" .= ann, "span" .= span]]

data Ann a =
    Ann a (Ann a)
  | AnnGroup [Ann a]
  | AnnLeaf Text
  deriving (Functor)

instance ToJSON a => ToJSON (Ann a) where
    toJSON (Ann a x) = object ["tag" .= "Ann", "contents" .= x]
    toJSON (AnnGroup xs) = object ["tag" .= "AnnGroup", "contents" .= xs]
    toJSON (AnnLeaf t) = object ["tag" .= "AnnLeaf", "contents" .= t]

data CodeAnn =
    CodeIdInfo IdInfo

instance ToJSON CodeAnn where
    toJSON (CodeIdInfo info) =
        object ["tag" .= "CodeAnn", "contents" .= Orphan info]

newtype Orphan a = Orphan
    { orphan :: a
    }

data AnnSourceError = AnnSourceError
  { annErrorKind :: !SourceErrorKind
  , annErrorSpan :: !EitherSpan
  , annErrorMsg :: !(Ann MsgAnn)
  }

instance ToJSON AnnSourceError where
    toJSON (AnnSourceError kind span msg) =
        object
            [ "tag" .= "AnnSourceError"
            , "contents" .=
              object ["kind" .= kind, "span" .= span, "msg" .= msg]]

data MsgAnn =
    MsgAnnModule
  | MsgAnnCode CodeVariety
  | MsgAnnCodeAnn CodeAnn
  | MsgAnnRefactor Text [((Orphan SourceSpan), Text)]
  | MsgAnnCollapse

instance ToJSON MsgAnn where
    toJSON (MsgAnnModule) = object ["tag" .= "MsgAnnModule"]
    toJSON (MsgAnnCode var) =
        object ["tag" .= "MsgAnnCode", "contents" .= var]
    toJSON (MsgAnnCodeAnn ann) =
        object ["tag" .= "MsgAnnCodeAnn", "contents" .= ann]
    toJSON (MsgAnnRefactor t spans) =
        object
            [ "tag" .= "MsgAnnRefactor"
            , "contents" .= object ["text" .= t, "spans" .= map to spans]]
      where
        to (x,y) =
            object
                [ "tag" .= "Refactor"
                , "contents" .= object ["span" .= x, "text" .= y]]
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
          AmbiguousCode ann -> object ["tag" .= "AmbiguousCode","contents" .= ann]

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
            , "contents" .=
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
            , "contents" .=
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
