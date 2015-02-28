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
module IdeSession.Client.JsonAPI (
    -- * Requests
    Request(..)
  , RequestSessionUpdate(..)
  , Response(..)
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
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text        as Text

import IdeSession.Client.JsonAPI.Aux
import IdeSession

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data Request =
    RequestUpdateSession [RequestSessionUpdate]
  | RequestGetSourceErrors
  deriving Show

data RequestSessionUpdate =
    RequestUpdateSourceFileFromFile FilePath
  | RequestUpdateGhcOpts [String]
  deriving Show

data Response =
    ResponseSessionUpdate Progress
  | ResponseGetSourceErrors [SourceError]
  deriving Show

{-------------------------------------------------------------------------------
  Stack prisms

  Stack prisms are just prisms with a special kind of type. See Data.StackPrism.
-------------------------------------------------------------------------------}

$(deriveStackPrismsWith prismNameForConstructor ''Request)
$(deriveStackPrismsWith prismNameForConstructor ''RequestSessionUpdate)
$(deriveStackPrismsWith prismNameForConstructor ''Response)

$(deriveStackPrismsWith prismNameForConstructor ''Progress)
$(deriveStackPrismsWith prismNameForConstructor ''SourceError)
$(deriveStackPrismsWith prismNameForConstructor ''EitherSpan)
$(deriveStackPrismsWith prismNameForConstructor ''SourceSpan)

$(deriveStackPrismsWith prismNameForConstructor ''Maybe)

-- | Apply a prism to the top of the stack
top :: StackPrism a b -> StackPrism (a :- t) (b :- t)
top prism = stackPrism (\(a :- t) -> (forward prism a :- t))
                       (\(b :- t) -> (:- t) `fmap` backward prism b)

{-------------------------------------------------------------------------------
  Translation to and from JSON
-------------------------------------------------------------------------------}

instance Json Request where
  grammar = label "Request" $
    object $ mconcat [
          property "request" "updateSession"
        . fromPrism requestUpdateSession . prop "update"
      ,   property "request" "getSourceErrors"
        . fromPrism requestGetSourceErrors
      ]

instance Json RequestSessionUpdate where
  grammar = label "SessionUpdate" $
    object $ mconcat [
          property "sessionUpdate" "updateSourceFileFromFile"
        . fromPrism requestUpdateSourceFileFromFile . prop "filePath"
      ,   property "sessionUpdate" "updateGhcOpts"
        . fromPrism requestUpdateGhcOpts . prop "options"
      ]

instance Json Response where
  grammar = label "Response" $
    object $ mconcat [
          property "response" "sessionUpdate"
        . fromPrism responseSessionUpdate . prop "progress"
      ,   property "response" "getSourceErrors"
        . fromPrism responseGetSourceErrors . prop "errors"
      ]

instance Json Progress where
  grammar = label "Progress" $
    object $
        fromPrism progress
      . prop "step"
      . prop "numSteps"
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
  grammar = label "ProperSpan" $
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

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

apiDocs :: String
apiDocs = renderDeclarationSourceFile $ interfaces [
    SomeGrammar (grammar :: Grammar Val (Value :- ()) (Request              :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (RequestSessionUpdate :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (Response             :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (Progress             :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (SourceErrorKind      :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (EitherSpan           :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (SourceSpan           :- ()))
  , SomeGrammar (grammar :: Grammar Val (Value :- ()) (SourceError          :- ()))
  ]

toJSON :: Json a => a -> Value
toJSON = fromMaybe (error "toJSON: Could not serialize") . serialize grammar

fromJSON :: Json a => Value -> Either String a
fromJSON = Aeson.parseEither (parse grammar)

{-------------------------------------------------------------------------------
  Auxiliary grammar definitions
-------------------------------------------------------------------------------}

instance Json String where
  grammar = string

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
string = fromPrism (top (stackPrism Text.unpack (Just . Text.pack))) . grammar

-- | Optional property
optProp :: Json a => Text -> Grammar Obj t (Maybe a :- t)
optProp propName = fromPrism just . prop propName <> fromPrism nothing
