{-# LANGUAGE BinaryLiterals #-}

module State.Tests
  ( tests
  ) where

import Data.Default
import Data.Bits
import Test.Tasty
import Test.Tasty.HUnit

--
import KeySymbolDefinitions
import Modifiers
import Skb
import State
import BitMask

-- got this idea from
--  https://jaspervdj.be/posts/2015-03-13-practical-testing-in-haskell.html
tests :: TestTree
tests =
  testGroup
    "State"
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
    , testCase "testOnKeyCodePress03" testOnKeyCodePress03
    , testCase "testOnKeyCodePress04" testOnKeyCodePress04
    , testCase "testOnKeyCodePress05" testOnKeyCodePress05
    , testCase "testOnKeyCodePress06" testOnKeyCodePress06
    , testCase "testOnKeyCodePress07" testOnKeyCodePress07
    , testCase "testOnKeyCodePress08" testOnKeyCodePress08
    , testCase "testOnKeyCodePress09" testOnKeyCodePress09
    , testCase "testOnKeyCodePress10" testOnKeyCodePress10
    , testCase "testOnKeyCodePress11" testOnKeyCodePress11

    , testCase "testOnKeyCodeRelease01" testOnKeyCodeRelease01
    , testCase "testOnKeyCodeRelease02" testOnKeyCodeRelease02
    ]

testIdentifyStateChanges01 :: Assertion
testIdentifyStateChanges01 = (identifyStateChanges def def) @?= UpdatedStateComponents 0

testIdentifyStateChanges02 :: Assertion
testIdentifyStateChanges02 =
  (identifyStateChanges (def {sDepressedModifiers = 1}) def) @?= UpdatedStateComponents 0b1

testIdentifyStateChanges03 :: Assertion
testIdentifyStateChanges03 =
  (identifyStateChanges (def {sLatchedModifiers = 1}) def) @?= UpdatedStateComponents 0b10

testIdentifyStateChanges04 :: Assertion
testIdentifyStateChanges04 =
  (identifyStateChanges (def {sLockedModifiers = 1}) def) @?= UpdatedStateComponents 0b100

testIdentifyStateChanges05 :: Assertion
testIdentifyStateChanges05 =
  (identifyStateChanges (def {sEffectiveModifiers = 1}) def) @?= UpdatedStateComponents 0b1000

testIdentifyStateChanges06 :: Assertion
testIdentifyStateChanges06 =
  (identifyStateChanges (def {sDepressedGroup = 1}) def) @?= UpdatedStateComponents 0b10000

testIdentifyStateChanges07 :: Assertion
testIdentifyStateChanges07 =
  (identifyStateChanges (def {sLatchedGroup = 1}) def) @?= UpdatedStateComponents 0b100000

testIdentifyStateChanges08 :: Assertion
testIdentifyStateChanges08 =
  (identifyStateChanges (def {sLockedGroup = 1}) def) @?= UpdatedStateComponents 0b1000000

testIdentifyStateChanges09 :: Assertion
testIdentifyStateChanges09 =
  (identifyStateChanges (def {sEffectiveGroup = 1}) def) @?= UpdatedStateComponents 0b10000000

testIdentifyStateChanges10 :: Assertion
testIdentifyStateChanges10 =
  (identifyStateChanges (def {sEffectiveGroup = 1}) def) @?= UpdatedStateComponents 0b10000000

-- a keycode = 38, keysymbol = 0x61
-- using the default void keymap
testOnKeyCodePress01 :: Assertion
testOnKeyCodePress01 = (onKeyCodePress 38 def) @?= (MkKeySymbol 0, UpdatedStateComponents 0, def)

testOnKeyCodePress02 :: Assertion
testOnKeyCodePress02 =
  (onKeyCodePress 38 (pickInitialState 1)) @?=
  (MkKeySymbol 0x61, UpdatedStateComponents 0, pickInitialState 1)

testOnKeyCodePress03 :: Assertion
testOnKeyCodePress03 =
  (onKeyCodePress 38 (pickInitialState 1)) @?=
  (MkKeySymbol 0x61, UpdatedStateComponents 0, pickInitialState 1)

-- o keycode 39, keysymbol 0x6f
testOnKeyCodePress04 :: Assertion
testOnKeyCodePress04 =
  (onKeyCodePress 39 (pickInitialState 1)) @?=
  (MkKeySymbol 0x6f, UpdatedStateComponents 0, pickInitialState 1)

-- q keycode 53, keysymbol 0x71
testOnKeyCodePress05 :: Assertion
testOnKeyCodePress05 =
  (onKeyCodePress 53 (pickInitialState 1)) @?=
  (MkKeySymbol 0x71, UpdatedStateComponents 0, pickInitialState 1)

-- Escape keycode 9, keysymbol 0xff1b
testOnKeyCodePress06 :: Assertion
testOnKeyCodePress06 =
  (onKeyCodePress 9 (pickInitialState 1)) @?=
  (MkKeySymbol 0xff1b, UpdatedStateComponents 0, pickInitialState 1)

-- k keycode = 38, keysymbol = 0x41
testOnKeyCodePress07 :: Assertion
testOnKeyCodePress07 =
  (onKeyCodePress
     38
     (((\s ->
          s {sDepressedModifiers = setModifier (sDepressedModifiers s) Shift}) .
       pickInitialState)
        1)) @?=
  (MkKeySymbol 0x41, UpdatedStateComponents 1, pickInitialState 1)

-- Meta_L keycode = 64, keysymbol = Meta_L = 0xffe7 = 65,511 ==> Mod3
testOnKeyCodePress08 :: Assertion
testOnKeyCodePress08 =
  let original = pickInitialState 1
      s = original
      expected = original
                    { sDepressedModifiers = setModifier (sDepressedModifiers s) Mod3
                    , sEffectiveModifiers = setModifier (sEffectiveModifiers s) Mod3
                    , sLatchedModifiers   = setModifier (sLatchedModifiers s) Mod3
                    }
  in (onKeyCodePress 64 original) @?=
     (XKB_KEY_Meta_L, identifyStateChanges original expected, expected)

-- Alt_L keycode = 108, keysymbol = Alt_L ==> Mod1
testOnKeyCodePress09 :: Assertion
testOnKeyCodePress09 =
  let original = pickInitialState 1
      s = original
      expected = original
                    { sDepressedModifiers = setModifier (sDepressedModifiers s) Mod1
                    , sEffectiveModifiers = setModifier (sEffectiveModifiers s) Mod1
                    , sLatchedModifiers   = setModifier (sLatchedModifiers s) Mod1
                    }
  in (onKeyCodePress 108 original) @?=
     (XKB_KEY_Alt_L, identifyStateChanges original expected, expected)

-- Control_L keycode = 66, keysymbol = Control_L ==> Control
testOnKeyCodePress10 :: Assertion
testOnKeyCodePress10 =
  let original = pickInitialState 1
      s = original
      expected = original
                    { sDepressedModifiers = setModifier (sDepressedModifiers s) Control
                    , sEffectiveModifiers = setModifier (sEffectiveModifiers s) Control
                    , sLatchedModifiers   = setModifier (sLatchedModifiers s) Control
                    }
  in (onKeyCodePress 66 original) @?=
     (XKB_KEY_Control_L, identifyStateChanges original expected, expected)

-- Shift_L keycode = 50, keysymbol = Shift_L ==> Shift
testOnKeyCodePress11 :: Assertion
testOnKeyCodePress11 =
  let original = pickInitialState 1
      s = original
      expected = original
                    { sDepressedModifiers = setModifier (sDepressedModifiers s) Shift
                    , sEffectiveModifiers = setModifier (sEffectiveModifiers s) Shift
                    , sLatchedModifiers   = setModifier (sLatchedModifiers s) Shift
                    }
  in (onKeyCodePress 50 original) @?=
     (XKB_KEY_Shift_L, identifyStateChanges original expected, expected)

-- the Shift should not be released
-- Escape keycode 9, keysymbol 0xff1b
testOnKeyCodeRelease01 :: Assertion
testOnKeyCodeRelease01 =
  let original = (pickInitialState 1)
                    { sDepressedModifiers = setModifier 0 Shift
                    }
      expected = (pickInitialState 1)
                    { sDepressedModifiers = setModifier 0 Shift
                    , sEffectiveModifiers = setModifier 0 Shift
                    }
  in (onKeyCodeRelease 9 original) @?= (UpdatedStateComponents 0, expected)

-- the Shift should not be released
-- A keycode 38, keysymbol 0x41
testOnKeyCodeRelease02 :: Assertion
testOnKeyCodeRelease02 =
  let original = (pickInitialState 1)
                   {sDepressedModifiers = setModifier 0 Shift}
      expected = (pickInitialState 1)
                    { sDepressedModifiers = setModifier 0 Shift
                    , sEffectiveModifiers = setModifier 0 Shift
                    }
  in (onKeyCodeRelease 9 original) @?= (UpdatedStateComponents 0, expected)
