
module Main where

import           Test.Tasty (defaultMain, testGroup)
--
import qualified State.Tests
-- import qualified Keymap.CustomDvorak.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ State.Tests.tests
    ]
