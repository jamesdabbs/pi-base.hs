module Main where

import Prelude (IO)

import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Spec

main :: IO ()
main = hspecWith defaultConfig {configColorMode = ColorAlways} Spec.spec
