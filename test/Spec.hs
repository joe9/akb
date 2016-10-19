
module Main where

import           System.Posix.ByteString.FilePath
import           System.Posix.FilePath
import           Test.Tasty (defaultMain, testGroup)
import           Network.NineP.Context hiding (File)
import qualified Network.NineP.Context as Context
--
import qualified State.Tests
-- import qualified Keymap.CustomDvorak.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ State.Tests.tests
    ]

-- TODO  add the below
fsList :: V.Vector (FSItem Context)
fsList =
  V.fromList
    [ dir "/"
    , file "/test"
    , file "/test1"
    , file "/test2"
    , file "/test3"
    , dir "/test4/"
    , dir "/test5"
    , file "/test4/test5"
    ]

dir, file :: RawFilePath -> FSItem Context
dir name = FSItem Directory (dirDetails name ) []
file name = FSItem Context.File (fileDetails name ) []
