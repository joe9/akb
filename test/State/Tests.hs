
{-# LANGUAGE BinaryLiterals #-}

module State.Tests
    ( tests
    ) where

import           Test.Tasty
import           Data.Default
import           Test.Tasty.HUnit
--
import State
import KeySymbolDefinitions
import Skb

-- got this idea from
--  https://jaspervdj.be/posts/2015-03-13-practical-testing-in-haskell.html

tests :: TestTree
tests = testGroup "State"
    [ testCase "testIdentifyStateChanges01" testIdentifyStateChanges01
    , testCase "testIdentifyStateChanges02" testIdentifyStateChanges02
    , testCase "testIdentifyStateChanges03" testIdentifyStateChanges03
    , testCase "testIdentifyStateChanges04" testIdentifyStateChanges04
    , testCase "testIdentifyStateChanges05" testIdentifyStateChanges05
    , testCase "testIdentifyStateChanges06" testIdentifyStateChanges06
    , testCase "testIdentifyStateChanges07" testIdentifyStateChanges07
    , testCase "testIdentifyStateChanges08" testIdentifyStateChanges08
    , testCase "testIdentifyStateChanges09" testIdentifyStateChanges09

    , testCase "testOnKeyCodePress01" testOnKeyCodePress01
    , testCase "testOnKeyCodePress02" testOnKeyCodePress02
    ]

testIdentifyStateChanges01 :: Assertion
testIdentifyStateChanges01 =
  (identifyStateChanges def def) @?= 0

testIdentifyStateChanges02 :: Assertion
testIdentifyStateChanges02 =
  (identifyStateChanges (def {sDepressedModifiers = 1}) def) @?= 0b1

testIdentifyStateChanges03 :: Assertion
testIdentifyStateChanges03 =
  (identifyStateChanges (def {sLatchedModifiers = 1}) def) @?= 0b10

testIdentifyStateChanges04 :: Assertion
testIdentifyStateChanges04 =
  (identifyStateChanges (def {sLockedModifiers = 1}) def) @?= 0b100

testIdentifyStateChanges05 :: Assertion
testIdentifyStateChanges05 =
  (identifyStateChanges (def {sEffectiveModifiers = 1}) def) @?= 0b1000

testIdentifyStateChanges06 :: Assertion
testIdentifyStateChanges06 =
  (identifyStateChanges (def {sDepressedGroup = 1}) def) @?= 0b10000

testIdentifyStateChanges07 :: Assertion
testIdentifyStateChanges07 =
  (identifyStateChanges (def {sLatchedGroup = 1}) def) @?= 0b100000

testIdentifyStateChanges08 :: Assertion
testIdentifyStateChanges08 =
  (identifyStateChanges (def {sLockedGroup = 1}) def) @?= 0b1000000

testIdentifyStateChanges09 :: Assertion
testIdentifyStateChanges09 =
  (identifyStateChanges (def {sEffectiveGroup = 1}) def) @?= 0b10000000

-- testIdentifyStateChanges10 :: Assertion
-- testIdentifyStateChanges10 =
--   (identifyStateChanges (def {sEffectiveGroup = 1}) def) @?= 0b10000000

-- using the default void keymap
testOnKeyCodePress01 :: Assertion
testOnKeyCodePress01 = (onKeyCodePress 38 def) @?= (MkKeySymbol 0 , 0, def)

testOnKeyCodePress02 :: Assertion
testOnKeyCodePress02 =
  (onKeyCodePress 38 (pickInitialState 1)) @?= (MkKeySymbol 0x61 , 0, (pickInitialState 1))

testOnKeyCodePress03 :: Assertion
testOnKeyCodePress03 =
  (onKeyCodePress 38 (pickInitialState 1)) @?= (MkKeySymbol 0x61 , 0, (pickInitialState 1))
