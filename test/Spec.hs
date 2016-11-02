{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Protolude
import           Test.Tasty                       (defaultMain,
                                                   testGroup)

import qualified State.Tests
import qualified Akb.Tests

-- import           FileSystem.Tests
-- import qualified Keymap.CustomDvorak.Tests
main :: IO ()
main = defaultMain (testGroup "Tests" [ State.Tests.tests
                                      , Akb.Tests.tests
                                      ])

