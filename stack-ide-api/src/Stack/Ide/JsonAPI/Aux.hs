-- | Auxiliary definitions for use in the JsonAPI module
--
-- Defined as a separate module only because of TH stage restrictions.
module Stack.Ide.JsonAPI.Aux where

import Data.Char

-- | Given FooBarBaz, return fooBarBaz
prismNameForConstructor :: String -> String
prismNameForConstructor constructorName =
    let x:xs = constructorName
    in toLower x : xs
