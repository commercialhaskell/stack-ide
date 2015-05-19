-- The JSON api that we expose
--
-- Some general design principles:
--
-- * We have a single Request type for requests sent by the editor to the client
--   and a single Response type for responses sent back from the client to the
--   editor.
-- * We use JsonGrammar to implement to the translation to and from JSON values.
--   The main advance of this is that this will also enable us to generate
--   documentation of the JSON types.
--   (NOTE: Currently the generated documentation is not great, so we probably
--   need to do some work on JsonGrammar itself; but nevertheless this is the
--   right approach: by having a deep embedding of the the grammar we can, at
--   at least in principle, derive documentation, even if we need to do some
--   work on making that better.)
-- * We try to roughly have a one-to-one mapping between the functions exported
--   by IdeSession; for instance, ide-backend's `updateSourceFileFromFile`
--   corresponds to the `RequestUpdateSourceFileFromFile` request and the
--   `ResponseUpdateSourceFileFromFile` response.
--
-- Hopefully with these principles in place the API should be predictable,
-- stable, and well documented.
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
module IdeSession.Client.JsonAPI (
    -- * Requests
    Request(..)
  , RequestSessionUpdate(..)
  , Response(..)
  , ResponseSpanInfo(..)
  , ResponseExpType(..)
  , ResponseAnnExpType(..)
  , Ann(..)
  , TypeAnn(..)
  , AnnSourceError(..)
  , MsgAnn(..)
  , AutocompletionSpan(..)
  , AutocompletionInfo(..)
  , VersionInfo(..)
  , Identifier
    -- * JSON API
  , apiDocs
  , toJSON
  , fromJSON
  ) where

import Prelude hiding ((.), id)
import Control.Category
import Data.Aeson (Value)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.StackPrism
import Data.StackPrism.TH
import Data.Text (Text)
import Language.JsonGrammar
import Language.TypeScript.Pretty (renderDeclarationSourceFile)
import qualified Data.Aeson.Types          as Aeson
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as Lazy

import IdeSession.Client.JsonAPI.Aux
import IdeSession.Client.JsonAPI.Common
import IdeSession.Client.JsonAPI.Public
import IdeSession.Types.Progress
import IdeSession.Types.Public hiding (idProp, Value)

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
  deriving Show

-- | Session updates
data RequestSessionUpdate =
    RequestUpdateSourceFile FilePath Lazy.ByteString
  | RequestUpdateSourceFileFromFile FilePath
  | RequestUpdateGhcOpts [String]
  deriving (Eq, Show)

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
  | ResponseProcessOutput BS.ByteString
  | ResponseProcessDone RunResult
  -- Misc
  | ResponseInvalidRequest String
  | ResponseShutdownSession
  deriving Show

data ResponseSpanInfo =
    ResponseSpanInfo SpanInfo SourceSpan
  deriving (Eq, Show)

data ResponseExpType =
    ResponseExpType Text SourceSpan
  deriving (Eq, Show)

data ResponseAnnExpType =
    ResponseAnnExpType (Ann TypeAnn) SourceSpan
  deriving (Eq, Show)

data Ann a =
    Ann a (Ann a)
  | AnnGroup [Ann a]
  | AnnLeaf Text
  deriving (Eq, Show)

data TypeAnn =
    TypeIdInfo IdInfo
  deriving (Eq, Show)

data AnnSourceError = AnnSourceError
  { annErrorKind :: !SourceErrorKind
  , annErrorSpan :: !EitherSpan
  , annErrorMsg :: !(Ann MsgAnn)
  }
  deriving (Eq, Show)

data MsgAnn =
    MsgAnnModule
  | MsgAnnCode -- ^ Note: Ideally we'd distinguish identifiers, types, exprs, etc
  | MsgAnnRefactor Text [(SourceSpan, Text)]
  | MsgAnnCollapse
  deriving (Eq, Show)

data AutocompletionSpan = AutocompletionSpan
   { autocompletionFilePath :: FilePath
   , autocompletionPrefix :: String
   }
   deriving (Eq, Show)

data AutocompletionInfo = AutocompletionInfo
   { autocompletionInfoDefinedIn :: Text
   , autocompletionInfoName :: Text
   , autocompletionQualifier :: Maybe Text
   , autocompletionType :: Maybe Text
   }
   deriving (Eq, Show)

-- | Client version
--
-- Standard versioning applies (major, minor, patch)
data VersionInfo =
    VersionInfo Int Int Int
  deriving (Eq, Show)

type Identifier = Text

{-------------------------------------------------------------------------------
  Stack prisms

  Stack prisms are just prisms with a special kind of type. See Data.StackPrism.
-------------------------------------------------------------------------------}

$(fmap concat $ mapM (deriveStackPrismsWith prismNameForConstructor)
  [ ''Request
  , ''RequestSessionUpdate
  , ''Response
  , ''ResponseSpanInfo
  , ''ResponseExpType
  , ''ResponseAnnExpType
  , ''Ann
  , ''TypeAnn
  , ''AnnSourceError
  , ''MsgAnn
  , ''AutocompletionSpan
  , ''AutocompletionInfo
  , ''VersionInfo
  ])

{-------------------------------------------------------------------------------
  Translation to and from JSON
-------------------------------------------------------------------------------}

instance Json Request where
  grammar = label "Request" $
    object $ mconcat [
          property "request" "updateSession"
        . fromPrism requestUpdateSession
        . prop "update"
      ,   property "request" "getSourceErrors"
        . fromPrism requestGetSourceErrors
      ,   property "request" "getAnnSourceErrors"
        . fromPrism requestGetAnnSourceErrors
      ,   property "request" "getLoadedModules"
        . fromPrism requestGetLoadedModules
      ,   property "request" "getSpanInfo"
        . fromPrism requestGetSpanInfo
        . prop "span"
      ,   property "request" "getExpTypes"
        . fromPrism requestGetExpTypes
        . prop "span"
      ,   property "request" "getAnnExpTypes"
        . fromPrism requestGetAnnExpTypes
        . prop "span"
      ,   property "request" "getAutocompletion"
        . fromPrism requestGetAutocompletion
        . prop "autocomplete"
      ,   property "request" "run"
        . fromPrism requestRun
        . prop "usePty"
        . prop "module"
        . prop "identifier"
      ,   property "request" "processInput"
        . fromPrism requestProcessInput
        . prop "value"
      ,   property "request" "processKill"
        . fromPrism requestProcessKill
      ,   property "request" "shutdownSession"
        . fromPrism requestShutdownSession
      ]

instance Json RequestSessionUpdate where
  grammar = label "SessionUpdate" $
    object $ mconcat [
          property "update" "updateSourceFile"
        . fromPrism requestUpdateSourceFile
        . prop "filePath"
        . prop "contents"
      ,   property "update" "updateSourceFileFromFile"
        . fromPrism requestUpdateSourceFileFromFile
        . prop "filePath"
      ,   property "update" "updateGhcOpts"
        . fromPrism requestUpdateGhcOpts
        . prop "options"
      ]

instance Json Response where
  grammar = label "Response" $
    object $ mconcat [
          property "response" "welcome"
        . fromPrism responseWelcome
        . prop "version"
      ,   property "response" "sessionUpdate"
        . fromPrism responseUpdateSession
        . optProp "progress"
      ,   property "response" "getSourceErrors"
        . fromPrism responseGetSourceErrors
        . prop "errors"
      ,   property "response" "getAnnSourceErrors"
        . fromPrism responseGetAnnSourceErrors
        . prop "errors"
      ,   property "response" "getLoadedModules"
        . fromPrism responseGetLoadedModules
        . prop "modules"
      ,   property "response" "getSpanInfo"
        . fromPrism responseGetSpanInfo
        . prop "info"
      ,   property "response" "getExpTypes"
        . fromPrism responseGetExpTypes
        . prop "info"
      ,   property "response" "getAnnExpTypes"
        . fromPrism responseGetAnnExpTypes
        . prop "info"
      ,   property "response" "getAutocompletion"
        . fromPrism responseGetAutocompletion
        . prop "completions"
      ,   property "response" "processOutput"
        . fromPrism responseProcessOutput
        . prop "value"
      ,   property "response" "processDone"
        . fromPrism responseProcessDone
        . prop "result"
      ,   property "response" "invalidRequest"
        . fromPrism responseInvalidRequest
        . prop "errorMessage"
      ,   property "response" "shutdownSession"
        . fromPrism responseShutdownSession
      ]

instance Json AutocompletionSpan where
  grammar = label "AutocompletionSpan" $
    object $
        fromPrism autocompletionSpan
      . prop "filePath"
      . prop "prefix"

instance Json AutocompletionInfo where
  grammar = label "AutocompletionInfo" $
    object $
        fromPrism autocompletionInfo
      . prop "definedIn"
      . prop "name"
      . optProp "qualifier"
      . optProp "type"

instance Json ResponseSpanInfo where
  grammar = label "ResponseSpanInfo" $
    object $ mconcat [
          property "isQuasiQuote" (literal (Aeson.Bool False))
        . fromPrism (responseSpanInfo . spanId)
        . prop "idInfo"
        . prop "span"
      ,   property "isQuasiQuote" (literal (Aeson.Bool True))
        . fromPrism (responseSpanInfo . spanQQ)
        . prop "idInfo"
        . prop "span"
      ]

instance Json ResponseExpType where
  grammar = label "ResponseExpType" $
    object $
        fromPrism responseExpType
      . prop "type"
      . prop "span"

instance Json ResponseAnnExpType where
  grammar = label "ResponseAnnExpType" $
    object $
        fromPrism responseAnnExpType
      . prop "type"
      . prop "span"

instance Json a => Json (Ann a) where
  grammar = label "Ann" $ mconcat
    [ object $
        fromPrism ann
      . prop "ann"
      . prop "value"
    ,   fromPrism annGroup . array (element grammar)
    ,   fromPrism annLeaf . grammar
    ]

instance Json TypeAnn where
  grammar = label "TypeAnn" $ mconcat
    [ object $
        fromPrism typeIdInfo
      . prop "idInfo"
    ]

instance Json AnnSourceError where
  grammar = label "AnnSourceError" $
    object $
        fromPrism annSourceError
      . prop "kind"
      . prop "span"
      . prop "msg"

instance Json MsgAnn where
  grammar = label "MsgAnn" $ mconcat
    [ object $
        property "label" "Module"
      . fromPrism msgAnnModule
    , object $
        property "label" "Code"
      . fromPrism msgAnnCode
    , object $
        property "label" "Refactor"
      . fromPrism msgAnnRefactor
      . prop "msg"
      . prop "replacements"
    , object $
        property "label" "Collapse"
      . fromPrism msgAnnCollapse
    ]

instance Json VersionInfo where
  grammar = label "VersionInfo" $
    object $
        fromPrism versionInfo
      . prop "major"
      . prop "minor"
      . prop "patch"

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

apiDocs :: String
apiDocs = renderDeclarationSourceFile $ interfaces [
    -- ide-backend-client specific types
    SomeGrammar (grammar :: Grammar Val (Value :- ()) (Request              :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (RequestSessionUpdate :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (Response             :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (ResponseSpanInfo     :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (ResponseExpType      :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (VersionInfo          :- ()))
    -- ide-backend types
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (EitherSpan           :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (IdInfo               :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (IdNameSpace          :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (IdProp               :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (IdScope              :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (ModuleId             :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (PackageId            :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (Progress             :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (SourceError          :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (SourceErrorKind      :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (SourceSpan           :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (AutocompletionSpan   :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (AutocompletionInfo   :- ()))
  ]

toJSON :: Json a => a -> Value
toJSON = fromMaybe (error "toJSON: Could not serialize") . serialize grammar

fromJSON :: Json a => Value -> Either String a
fromJSON = Aeson.parseEither (parse grammar)
