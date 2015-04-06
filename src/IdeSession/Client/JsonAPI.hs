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
  , VersionInfo(..)
    -- * JSON API
  , apiDocs
  , toJSON
  , fromJSON
    -- * Outputting JSON values
  , putEnc
  ) where

import Prelude hiding ((.), id)
import Control.Category
import Data.Aeson (Value, encode)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.StackPrism
import Data.StackPrism.TH
import Data.Text (Text)
import Language.JsonGrammar
import Language.TypeScript.Pretty (renderDeclarationSourceFile)
import System.IO
import qualified Data.Aeson.Types           as Aeson
import qualified Data.ByteString.Lazy       as Lazy
import qualified Data.ByteString.Lazy.UTF8  as Lazy (toString, fromString)
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8 (hPutStrLn)
import qualified Data.Text                  as Text

import IdeSession.Client.JsonAPI.Aux
import IdeSession hiding (idProp)

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Messages sent from the editor to the client
data Request =
    RequestUpdateSession [RequestSessionUpdate]
  | RequestGetSourceErrors
  | RequestGetLoadedModules
  | RequestGetSpanInfo SourceSpan
  | RequestGetExpTypes SourceSpan
  | RequestShutdownSession
  deriving Show

-- | Session updates
data RequestSessionUpdate =
    RequestUpdateSourceFile FilePath Lazy.ByteString
  | RequestUpdateSourceFileFromFile FilePath
  | RequestUpdateGhcOpts [String]
  deriving Show

-- | Messages sent back from the client to the editor
data Response =
    -- | Sent on session initialization
    ResponseWelcome VersionInfo
    -- | Nothing indicates the update completed
  | ResponseUpdateSession (Maybe Progress)
  | ResponseGetSourceErrors [SourceError]
  | ResponseGetLoadedModules [ModuleName]
  | ResponseGetSpanInfo [ResponseSpanInfo]
  | ResponseGetExpTypes [ResponseExpType]
  | ResponseInvalidRequest String
  | ResponseShutdownSession
  deriving Show

data ResponseSpanInfo =
    ResponseSpanInfo SpanInfo SourceSpan
  deriving Show

data ResponseExpType =
    ResponseExpType Text SourceSpan
  deriving Show

-- | Client version
--
-- Standard versioning applies (major, minor, patch)
data VersionInfo =
    VersionInfo Int Int Int
  deriving Show

{-------------------------------------------------------------------------------
  Stack prisms

  Stack prisms are just prisms with a special kind of type. See Data.StackPrism.
-------------------------------------------------------------------------------}

$(deriveStackPrismsWith prismNameForConstructor ''Request)
$(deriveStackPrismsWith prismNameForConstructor ''RequestSessionUpdate)
$(deriveStackPrismsWith prismNameForConstructor ''Response)
$(deriveStackPrismsWith prismNameForConstructor ''ResponseSpanInfo)
$(deriveStackPrismsWith prismNameForConstructor ''ResponseExpType)
$(deriveStackPrismsWith prismNameForConstructor ''VersionInfo)

$(deriveStackPrismsWith prismNameForConstructor ''EitherSpan)
$(deriveStackPrismsWith prismNameForConstructor ''IdInfo)
$(deriveStackPrismsWith prismNameForConstructor ''IdProp)
$(deriveStackPrismsWith prismNameForConstructor ''IdScope)
$(deriveStackPrismsWith prismNameForConstructor ''ModuleId)
$(deriveStackPrismsWith prismNameForConstructor ''PackageId)
$(deriveStackPrismsWith prismNameForConstructor ''Progress)
$(deriveStackPrismsWith prismNameForConstructor ''SourceError)
$(deriveStackPrismsWith prismNameForConstructor ''SourceSpan)
$(deriveStackPrismsWith prismNameForConstructor ''SpanInfo)

$(deriveStackPrismsWith prismNameForConstructor ''Maybe)

-- | Apply a prism to the top of the stack
top :: StackPrism a b -> StackPrism (a :- t) (b :- t)
top prism = stackPrism (\(a :- t) -> (forward prism a :- t))
                       (\(b :- t) -> (:- t) `fmap` backward prism b)

-- | Construct a stack prism from an isomorphism
iso :: (a -> b) -> (b -> a) -> StackPrism (a :- t) (b :- t)
iso f g = top (stackPrism f (Just . g))

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
      ,   property "request" "getLoadedModules"
        . fromPrism requestGetLoadedModules
      ,   property "request" "getSpanInfo"
        . fromPrism requestGetSpanInfo
        . prop "span"
      ,   property "request" "getExpTypes"
        . fromPrism requestGetExpTypes
        . prop "span"
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
      ,   property "response" "getLoadedModules"
        . fromPrism responseGetLoadedModules
        . prop "modules"
      ,   property "response" "getSpanInfo"
        . fromPrism responseGetSpanInfo
        . prop "info"
      ,   property "response" "getExpTypes"
        . fromPrism responseGetExpTypes
        . prop "info"
      ,   property "response" "invalidRequest"
        . fromPrism responseInvalidRequest
        . prop "errorMessage"
      ,   property "response" "shutdownSession"
        . fromPrism responseShutdownSession
      ]

instance Json VersionInfo where
  grammar = label "VersionInfo" $
    object $
        fromPrism versionInfo
      . prop "major"
      . prop "minor"
      . prop "patch"

instance Json Progress where
  grammar = label "Progress" $
    object $
        fromPrism progress
      . prop    "step"
      . prop    "numSteps"
      . optProp "parsedMsg"
      . optProp "origMsg"

instance Json SourceErrorKind where
  grammar = label "SourceErrorKind" $
    enumeration [
        ( KindError      , "error"      )
      , ( KindWarning    , "warning"    )
      , ( KindServerDied , "serverDied" )
      ]

instance Json EitherSpan where
  grammar = label "EitherSpan" $
    mconcat [
        fromPrism textSpan   . grammar
      , fromPrism properSpan . grammar
      ]

instance Json SourceSpan where
  grammar = label "SourceSpan" $
    object $
        fromPrism sourceSpan
      . prop "filePath"
      . prop "fromLine"
      . prop "fromColumn"
      . prop "toLine"
      . prop "toColumn"

instance Json SourceError where
  grammar = label "SourceError" $
    object $
        fromPrism sourceError
      . prop "kind"
      . prop "span"
      . prop "msg"

instance Json IdInfo where
  grammar = label "IdInfo" $
    object $
        fromPrism idInfo
      . prop "prop"
      . prop "scope"

instance Json IdProp where
  grammar = label "IdProp" $
    object $
        fromPrism idProp
      . prop    "name"
      . prop    "nameSpace"
      . optProp "type"
      . prop    "definedIn"
      . prop    "defSpan"
      . optProp "homeModule"

instance Json IdScope where
  grammar = label "IdScope" $
    object $ mconcat [
          property "scope" "binder"
        . fromPrism binder
      ,   property "scope" "local"
        . fromPrism local
      ,   property "scope" "imported"
        . fromPrism imported
        . prop "importedFrom"
        . prop "importSpan"
        . prop "importQual"
      ,   property "scope" "wiredIn"
        . fromPrism wiredIn
      ]

instance Json IdNameSpace where
  grammar = label "IdNameSpace" $
    enumeration [
        ( VarName   , "varName"   )
      , ( DataName  , "dataName"  )
      , ( TvName    , "tvName"    )
      , ( TcClsName , "tcClsName" )
      ]

instance Json ModuleId where
  grammar = label "ModuleId" $
    object $
        fromPrism moduleId
      . prop "name"
      . prop "package"

instance Json PackageId where
  grammar = label "PackageId" $
    object $
        fromPrism packageId
      . prop    "name"
      . optProp "version"
      . prop    "packageKey"

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
  ]

toJSON :: Json a => a -> Value
toJSON = fromMaybe (error "toJSON: Could not serialize") . serialize grammar

fromJSON :: Json a => Value -> Either String a
fromJSON = Aeson.parseEither (parse grammar)

-- | Output a JSON value
--
-- We separate JSON values in the output by newlines, so that editors have a
-- means to split the input into separate values. (The parser on the Haskell
-- side is a lot more sophisticated and deals with whitespace properly.)
putEnc :: Json a => a -> IO ()
putEnc = Lazy.Char8.hPutStrLn stdout . encode . toJSON

{-------------------------------------------------------------------------------
  Auxiliary grammar definitions
-------------------------------------------------------------------------------}

instance Json String          where grammar = string
instance Json Lazy.ByteString where grammar = lazyByteString

-- | Define a grammar by enumerating the possible values
--
-- For example:
--
-- > bool :: Grammar Val (Value :- t) (Bool :- t)
-- > bool = enumeration [(True, "true"), (False, "false")]
enumeration :: Eq a => [(a, Text)] -> Grammar Val (Value :- t) (a :- t)
enumeration = mconcat . map aux
  where
    aux (a, txt) = defaultValue a . literal (Aeson.String txt)

-- | String literal
string :: Grammar Val (Value :- t) (String :- t)
string = fromPrism (iso Text.unpack Text.pack) . grammar

-- | Lazy bytestring literal
lazyByteString :: Grammar Val (Value :- t) (Lazy.ByteString :- t)
lazyByteString = fromPrism (iso Lazy.fromString Lazy.toString) . grammar

-- | Optional property
optProp :: Json a => Text -> Grammar Obj t (Maybe a :- t)
optProp propName = fromPrism just . prop propName <> fromPrism nothing
