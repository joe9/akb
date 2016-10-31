{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Protolude
import System.Directory
import System.Posix.ByteString.FilePath
import System.Posix.FilePath
import Test.Tasty                       (defaultMain, testGroup)

import qualified State.Tests

import           Network.NineP.Context hiding (File)
import qualified Network.NineP.Context as Context

-- import           FileSystem.Tests
-- import qualified Keymap.CustomDvorak.Tests
main :: IO ()
main = do
  whenM
    (fmap not (doesFileExist "/home/j/dev/apps/durden-arcan/kbdfs/in"))
    (panic "kbdfs not mounted, /in not found")
  whenM
    (fmap not (doesFileExist "/home/j/dev/apps/durden-arcan/kbdfs/out"))
    (panic "kbdfs not mounted, /out not found")
  defaultMain $ testGroup "Tests" [State.Tests.tests]
