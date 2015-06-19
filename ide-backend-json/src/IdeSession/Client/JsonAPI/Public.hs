{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
-- | Defines 'Json' instances for the types in
-- "IdeSession.Types.Public".
module IdeSession.Client.JsonAPI.Public where

import Prelude hiding ((.), id)
import Control.Category
import Data.Monoid
import Data.StackPrism
import Data.StackPrism.TH
import Language.JsonGrammar

import IdeSession.Client.JsonAPI.Aux
import IdeSession.Client.JsonAPI.Common
import IdeSession.Types.Public hiding (idProp, Value)
import IdeSession.Types.Progress

{-------------------------------------------------------------------------------
  Stack prisms

  Stack prisms are just prisms with a special kind of type. See Data.StackPrism.
-------------------------------------------------------------------------------}

$(fmap concat $ mapM (deriveStackPrismsWith prismNameForConstructor)
  [ ''EitherSpan
  , ''IdInfo
  , ''IdProp
  , ''IdScope
  , ''ModuleId
  , ''PackageId
  , ''Progress
  , ''SourceError
  , ''SourceSpan
  , ''SpanInfo
  , ''RunResult
  ])

{-------------------------------------------------------------------------------
  Translation to and from JSON
-------------------------------------------------------------------------------}

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

instance Json RunResult where
  grammar = label "RunResult" $
    object $ mconcat [
        property "status" "ok"
      . fromPrism runOk
    ,   property "status" "progException"
      . fromPrism runProgException
      . prop "message"
    ,   property "status" "ghcException"
      . fromPrism runGhcException
      . prop "message"
    ,   property "status" "forceCancelled"
      . fromPrism runForceCancelled
    ,   property "status" "break"
      . fromPrism runBreak
    ]
