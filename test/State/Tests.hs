{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module State.Tests
  ( tests
  ) where

import Data.Bits
import qualified Data.Vector as V
import Data.Foldable
import Test.Tasty
import Test.Tasty.HUnit
import Protolude

import BitMask

import KeySymbolDefinitions
import KeySymbolToUTF
import Keymap.CustomDvorak
import Modifiers
import Akb
import State

-- got this idea from
--  https://jaspervdj.be/posts/2015-03-13-practical-testing-in-haskell.html
tests :: TestTree
tests =
      testGroup
            "State"
            [ testCaseSteps "testKeyCodeToKeySymTranslations" testKeyCodeToKeySymTranslations
            , testCaseSteps "testShiftLevelAlphabet01" testShiftLevelAlphabet01
            , testCaseSteps "testShiftLevelAlphabet02" testShiftLevelAlphabet02
        --    , testCase "testIdentifyStateChanges01" testIdentifyStateChanges01
        --     , testCase "testIdentifyStateChanges02" testIdentifyStateChanges02
        --     , testCase "testIdentifyStateChanges03" testIdentifyStateChanges03
        --     , testCase "testIdentifyStateChanges04" testIdentifyStateChanges04
        --     , testCase "testIdentifyStateChanges05" testIdentifyStateChanges05
        --     , testCase "testIdentifyStateChanges06" testIdentifyStateChanges06
        --     , testCase "testIdentifyStateChanges07" testIdentifyStateChanges07
        --     , testCase "testIdentifyStateChanges08" testIdentifyStateChanges08
        --     , testCase "testIdentifyStateChanges09" testIdentifyStateChanges09
        --     , testCase "testOnKeyPress01" testOnKeyPress01
        --     , testCase "testOnKeyPress02" testOnKeyPress02
        --     , testCase "testOnKeyPress03" testOnKeyPress03
        --     , testCase "testOnKeyPress04" testOnKeyPress04
        --     , testCase "testOnKeyPress05" testOnKeyPress05
        --     , testCase "testOnKeyPress06" testOnKeyPress06
        --     , testCase "testOnKeyPress07" testOnKeyPress07
        --     , testCase "testOnKeyPress08" testOnKeyPress08
        --     , testCase "testOnKeyPress09" testOnKeyPress09
        --     , testCase "testOnKeyPress10" testOnKeyPress10
        --     , testCase "testOnKeyPress11" testOnKeyPress11
        --     , testCase "testOnKeyRelease01" testOnKeyRelease01
        --     , testCase "testOnKeyRelease02" testOnKeyRelease02
        --     , testCase "testOnKeyRelease03" testOnKeyRelease03
            , testCaseSteps "testStickyShift" testStickyShift
            , testCaseSteps "testStickyControlCycle" (testStickyModifierCycle 66 XKB_KEY_Control_L Control)
            , testCaseSteps "testStickyShiftLCycle" (testStickyModifierCycle 50 XKB_KEY_Shift_L Shift)
            , testCaseSteps "testStickyAltCycle" (testStickyModifierCycle 108 XKB_KEY_Alt_L Mod1)
            , testCaseSteps "testStickyMetaCycle" (testStickyModifierCycle 64 XKB_KEY_Meta_L Mod3)
            , testCaseSteps "testStickyControlOnce" (testStickyModifierOnce 66 XKB_KEY_Control_L Control)
            , testCaseSteps "testStickyShiftLOnce" (testStickyModifierOnce 50 XKB_KEY_Shift_L Shift)
            , testCaseSteps "testStickyAltOnce" (testStickyModifierOnce 108 XKB_KEY_Alt_L Mod1)
            , testCaseSteps "testStickyMetaOnce" (testStickyModifierOnce 64 XKB_KEY_Meta_L Mod3)
            , testCaseSteps "testStickyShiftAlphabet" testStickyShiftAlphabet
            , testCaseSteps "testStickyShiftLockAlphabet" testStickyShiftLockAlphabet
--             , testCase "testStickyLocking02" testStickyLocking02
--             , testCase "testStickyLocking03" testStickyLocking03
--             , testCase "testStickyLocking04" testStickyLocking04
--             , testCase "testNonStickyLatching01" testNonStickyLatching01
--             , testCase "testNonStickyLatching02" testNonStickyLatching02
            ]

testKeyCodeToKeySymTranslations :: IsString a => (a -> IO()) -> Assertion
testKeyCodeToKeySymTranslations step =
  mapM_ (\(kc,g) -> testKeyCodeToKeySymTranslation kc (firstKeySymbolOfGroup g) step)
    customDvorakKeymap

firstKeySymbolOfGroup :: Group -> Maybe KeySymbol
firstKeySymbolOfGroup (Group kss) = kss V.!? 0
firstKeySymbolOfGroup (Groups _ kss) = kss V.!? 0 >>= firstKeySymbolOfGroup

testKeyCodeToKeySymTranslation :: IsString a => KeyCode -> Maybe KeySymbol -> (a -> IO()) -> Assertion
testKeyCodeToKeySymTranslation _ Nothing _ = return ()
testKeyCodeToKeySymTranslation kc (Just ks) step = do
  let (keysym,_,st) = onPress kc (pickInitialState "customDvorak")
--   step "checking Keysymbol"
  keysym @?= ks
  let st1 = onRelease kc st
--   step "checking effective modifiers"
  sEffectiveModifiers st1 @?= 0
--   step "checking latched modifiers"
  sLatchedModifiers st1 @?= 0
--   step "checking locked modifiers"
  sLockedModifiers st1 @?= 0

-- keycode for left shift = 50
-- keycode for a = 38
testShiftLevelAlphabet01 :: IsString a => (a -> IO()) -> Assertion
testShiftLevelAlphabet01 step = do
  let (keysym,_,st) = onPress 50 (pickInitialState "customDvorak")
      (keysym1,_,st1) = onPress 38 st
--   step "checking Shift Keysymbol"
  keysym @?= XKB_KEY_Shift_L
--   step "checking Keysymbol"
  keysym1 @?= XKB_KEY_A
--   step "checking effective modifiers"
  sEffectiveModifiers st1 @?= setModifier 0 Shift
--   step "checking latched modifiers"
  sLatchedModifiers st1 @?= 0
--   step "checking locked modifiers"
  sLockedModifiers st1 @?= 0

-- keycode for left shift = 50
-- keycode for q = 53
testShiftLevelAlphabet02 :: IsString a => (a -> IO()) -> Assertion
testShiftLevelAlphabet02 step = do
  let (keysym,_,st) = onPress 50 (pickInitialState "customDvorak")
      (keysym1,_,st1) = onPress 53 st
  keysym @?= XKB_KEY_Shift_L
  keysym1 @?= XKB_KEY_Q
  sEffectiveModifiers st1 @?= setModifier 0 Shift
  sLatchedModifiers st1 @?= 0
  sLockedModifiers st1 @?= 0

-- testIdentifyStateChanges01 :: Assertion
-- testIdentifyStateChanges01 =
--   (identifyStateChanges def def) @?= UpdatedStateComponents 0

-- testIdentifyStateChanges02 :: Assertion
-- testIdentifyStateChanges02 =
--   (identifyStateChanges (def {sDepressedModifiers = 1}) def) @?=
--   UpdatedStateComponents 0b1

-- testIdentifyStateChanges03 :: Assertion
-- testIdentifyStateChanges03 =
--   (identifyStateChanges (def {sLatchedModifiers = 1}) def) @?=
--   UpdatedStateComponents 0b10

-- testIdentifyStateChanges04 :: Assertion
-- testIdentifyStateChanges04 =
--   (identifyStateChanges (def {sLockedModifiers = 1}) def) @?=
--   UpdatedStateComponents 0b100

-- testIdentifyStateChanges05 :: Assertion
-- testIdentifyStateChanges05 =
--   (identifyStateChanges (def {sEffectiveModifiers = 1}) def) @?=
--   UpdatedStateComponents 0b1000

-- testIdentifyStateChanges06 :: Assertion
-- testIdentifyStateChanges06 =
--   (identifyStateChanges (def {sDepressedGroup = 1}) def) @?=
--   UpdatedStateComponents 0b10000

-- testIdentifyStateChanges07 :: Assertion
-- testIdentifyStateChanges07 =
--   (identifyStateChanges (def {sLatchedGroup = 1}) def) @?=
--   UpdatedStateComponents 0b100000

-- testIdentifyStateChanges08 :: Assertion
-- testIdentifyStateChanges08 =
--   (identifyStateChanges (def {sLockedGroup = 1}) def) @?=
--   UpdatedStateComponents 0b1000000

-- testIdentifyStateChanges09 :: Assertion
-- testIdentifyStateChanges09 =
--   (identifyStateChanges (def {sEffectiveGroup = 1}) def) @?=
--   UpdatedStateComponents 0b10000000

-- testIdentifyStateChanges10 :: Assertion
-- testIdentifyStateChanges10 =
--   (identifyStateChanges (def {sEffectiveGroup = 1}) def) @?=
--   UpdatedStateComponents 0b10000000

-- -- a keycode = 38, keysymbol = 0x61
-- -- using the default void keymap, should return 0
-- testOnKeyPress01 :: Assertion
-- testOnKeyPress01 = (onKeyPress 38 def) @?= (UpdatedStateComponents 0, def)

-- -- using the CustomDvorak keymap
-- testOnKeyPress02 :: Assertion
-- testOnKeyPress02 =
--   (onKeyPress 38 (pickInitialState 1)) @?=
--   (UpdatedStateComponents 0, pickInitialState 1)

-- testOnKeyPress03 :: Assertion
-- testOnKeyPress03 =
--   (onKeyPress 38 (pickInitialState 1)) @?=
--   (UpdatedStateComponents 0, pickInitialState 1)

-- -- o keycode 39, keysymbol 0x6f
-- testOnKeyPress04 :: Assertion
-- testOnKeyPress04 =
--   (onKeyPress 39 (pickInitialState 1)) @?=
--   (UpdatedStateComponents 0, pickInitialState 1)

-- -- q keycode 53, keysymbol 0x71
-- testOnKeyPress05 :: Assertion
-- testOnKeyPress05 =
--   (onKeyPress 53 (pickInitialState 1)) @?=
--   (UpdatedStateComponents 0, pickInitialState 1)

-- -- Escape keycode 9, keysymbol 0xff1b
-- testOnKeyPress06 :: Assertion
-- testOnKeyPress06 =
--   (onKeyPress 9 (pickInitialState 1)) @?=
--   (UpdatedStateComponents 0, pickInitialState 1)

-- -- A keycode = 38, keysymbol = 0x41
-- testOnKeyPress07 :: Assertion
-- testOnKeyPress07 =
--   let original =
--         (pickInitialState 1) {sDepressedModifiers = setModifier 0 Shift}
--       s = original
--       expected =
--         original
--         { sDepressedModifiers = setModifier 0 Shift
--         , sEffectiveModifiers = setModifier 0 Shift
--         }
--   in (onKeyPress 38 original) @?=
--      (identifyStateChanges original expected, expected)

-- -- Meta_L keycode = 64, keysymbol = Meta_L = 0xffe7 = 65,511 ==> Mod3
-- testOnKeyPress08 :: Assertion
-- testOnKeyPress08 =
--   let original = pickInitialState 1
--       s = original
--       expected =
--         original
--         { sDepressedModifiers = setModifier (sDepressedModifiers s) Mod3
--         , sEffectiveModifiers = setModifier (sEffectiveModifiers s) Mod3
--         , sLatchedModifiers = setModifier (sLatchedModifiers s) Mod3
--         }
--   in (onKeyPress 64 original) @?=
--      (identifyStateChanges original expected, expected)

-- -- Alt_L keycode = 108, keysymbol = Alt_L ==> Mod1
-- testOnKeyPress09 :: Assertion
-- testOnKeyPress09 =
--   let original = pickInitialState 1
--       s = original
--       expected =
--         original
--         { sDepressedModifiers = setModifier (sDepressedModifiers s) Mod1
--         , sEffectiveModifiers = setModifier (sEffectiveModifiers s) Mod1
--         , sLatchedModifiers = setModifier (sLatchedModifiers s) Mod1
--         }
--   in (onKeyPress 108 original) @?=
--      (identifyStateChanges original expected, expected)

-- -- Control_L keycode = 66, keysymbol = Control_L ==> Control
-- testOnKeyPress10 :: Assertion
-- testOnKeyPress10 =
--   let original = pickInitialState 1
--       s = original
--       expected =
--         original
--         { sDepressedModifiers = setModifier (sDepressedModifiers s) Control
--         , sEffectiveModifiers = setModifier (sEffectiveModifiers s) Control
--         , sLatchedModifiers = setModifier (sLatchedModifiers s) Control
--         }
--   in (onKeyPress 66 original) @?=
--      (identifyStateChanges original expected, expected)

-- -- Shift_L keycode = 50, keysymbol = Shift_L ==> Shift
-- testOnKeyPress11 :: Assertion
-- testOnKeyPress11 =
--   let original = pickInitialState 1
--       s = original
--       expected =
--         original
--         { sDepressedModifiers = setModifier (sDepressedModifiers s) Shift
--         , sEffectiveModifiers = setModifier (sEffectiveModifiers s) Shift
--         , sLatchedModifiers = setModifier (sLatchedModifiers s) Shift
--         }
--   in (onKeyPress 50 original) @?=
--      (identifyStateChanges original expected, expected)

-- -- the depressed Shift should not be released as per the workflow in notes
-- -- if there is just a latched Shift, it should be released
-- -- Escape keycode 9, keysymbol 0xff1b
-- testOnKeyRelease01 :: Assertion
-- testOnKeyRelease01 =
--   let original =
--         (pickInitialState 1) {sDepressedModifiers = setModifier 0 Shift}
--       expected = original {sEffectiveModifiers = setModifier 0 Shift}
--   in (onKeyRelease 9 original) @?= (UpdatedStateComponents 0, expected)

-- -- the Shift should not be released
-- -- A keycode 38, keysymbol 0x41
-- testOnKeyRelease02 :: Assertion
-- testOnKeyRelease02 =
--   let original =
--         (pickInitialState 1) {sDepressedModifiers = setModifier 0 Shift}
--       expected =
--         (pickInitialState 1)
--         { sDepressedModifiers = setModifier 0 Shift
--         , sEffectiveModifiers = setModifier 0 Shift
--         }
--   in (onKeyRelease 38 original) @?= (UpdatedStateComponents 0, expected)

-- -- the depressed Shift should not be released as per the workflow in notes
-- -- if there is just a latched Shift, it should be released
-- -- Escape keycode 9, keysymbol 0xff1b
-- testOnKeyRelease03 :: Assertion
-- testOnKeyRelease03 =
--   let original = (pickInitialState 1) {sLatchedModifiers = setModifier 0 Shift}
--       expected = pickInitialState 1
--       -- if there is just a latched Shift then it implies an Effective Shift
--       -- too, so both should be released
--   in (onKeyRelease 9 original) @?= (UpdatedStateComponents 0b1010, expected)

-- shift twice should become locked with sticky on
-- Shift_L keycode = 50, keysymbol = Shift_L ==> Shift
testStickyShift :: IsString a => (a -> IO()) -> Assertion
testStickyShift step = do
  let (keysym,r,st) = onPress 50 (pickInitialState "customDvorakSticky")
      (keysym1,r1,st1) = onPress 50 st
      (keysym2,r2,st2) = onPress 50 st1
      st3 = onRelease 50 st2
  keysym @?= XKB_KEY_Shift_L
  keysym1 @?= XKB_KEY_Shift_L
  keysym2 @?= XKB_KEY_Shift_L
  r @?= False
  r1 @?= False
  r2 @?= False
  -- 1st press, latch
  --step "1. checking effective modifiers"
  sEffectiveModifiers st @?= setModifier 0 Shift
  --step "1. checking depressed modifiers"
  sDepressedModifiers st @?= setModifier 0 Shift
  --step "1. checking latched modifiers"
  sLatchedModifiers st @?= setModifier 0 Shift
  --step "1. checking locked modifiers"
  sLockedModifiers st @?= 0

  -- 2nd press, lock
  --step "2. checking effective modifiers"
  sEffectiveModifiers st1 @?= setModifier 0 Shift
  sDepressedModifiers st1 @?= setModifier 0 Shift
  sLatchedModifiers st1 @?= 0
  sLockedModifiers st1 @?= setModifier 0 Shift

  -- 3rd press, unlock
  --step "3. checking effective modifiers"
  sEffectiveModifiers st2 @?= setModifier 0 Shift
  sDepressedModifiers st2 @?= setModifier 0 Shift
  sLatchedModifiers st2 @?= 0
  sLockedModifiers st2 @?= 0

  -- on release, depressed and effective should be cleared
  --step "4. checking effective modifiers"
  sEffectiveModifiers st3 @?= 0
  sDepressedModifiers st3 @?= 0
  sLatchedModifiers st3 @?= 0
  sLockedModifiers st3 @?= 0

-- shift twice should become locked with sticky on
-- Shift_L keycode = 50, keysymbol = Shift_L ==> Shift
testStickyModifierCycle :: IsString a => KeyCode -> KeySymbol -> Modifier -> (a -> IO()) -> Assertion
testStickyModifierCycle keycode keySymbol modifier step = do
  let (keysym,r,st) = onPress keycode (pickInitialState "customDvorakSticky")
      (keysym1,r1,st1) = onPress keycode st
      (keysym2,r2,st2) = onPress keycode st1
      st3 = onRelease keycode st2
  keysym @?= keySymbol
  keysym1 @?= keySymbol
  keysym2 @?= keySymbol
  r @?= False
  r1 @?= False
  r2 @?= False
  -- 1st press, latch
  --step "1. checking effective modifiers"
  sEffectiveModifiers st @?= setModifier 0 modifier
  --step "1. checking depressed modifiers"
  sDepressedModifiers st @?= setModifier 0 modifier
  --step "1. checking latched modifiers"
  sLatchedModifiers st @?= setModifier 0 modifier
  --step "1. checking locked modifiers"
  sLockedModifiers st @?= 0

  -- 2nd press, lock
  --step "2. checking effective modifiers"
  sEffectiveModifiers st1 @?= setModifier 0 modifier
  sDepressedModifiers st @?= setModifier 0 modifier
  sLatchedModifiers st1 @?= 0
  sLockedModifiers st1 @?= setModifier 0 modifier

  -- 3rd press, unlock
  --step "3. checking effective modifiers"
  sEffectiveModifiers st2 @?= setModifier 0 modifier
  sDepressedModifiers st2 @?= setModifier 0 modifier
  sLatchedModifiers st2 @?= 0
  sLockedModifiers st2 @?= 0

  -- on release, depressed and effective should be cleared
  --step "4. checking effective modifiers"
  sEffectiveModifiers st3 @?= 0
  sDepressedModifiers st3 @?= 0
  sLatchedModifiers st3 @?= 0
  sLockedModifiers st3 @?= 0

-- shift twice should become locked with sticky on
-- Shift_L keycode = 50, keysymbol = Shift_L ==> Shift
-- keycode for a = 38
testStickyModifierOnce :: IsString a => KeyCode -> KeySymbol -> Modifier -> (a -> IO()) -> Assertion
testStickyModifierOnce keycode keySymbol modifier step = do
  let (keysym,r,st) = onPress keycode (pickInitialState "customDvorakSticky")
      st1 = onRelease keycode st
  keysym @?= keySymbol
  r @?= False
  -- 1st press, latch
  --step "1. checking effective modifiers"
  sEffectiveModifiers st @?= setModifier 0 modifier
  --step "1. checking depressed modifiers"
  sDepressedModifiers st @?= setModifier 0 modifier
  --step "1. checking latched modifiers"
  sLatchedModifiers st @?= setModifier 0 modifier
  --step "1. checking locked modifiers"
  sLockedModifiers st @?= 0

  -- on release, depressed and effective should be cleared
  --step "2. checking effective modifiers"
  sEffectiveModifiers st1 @?= setModifier 0 modifier
  sDepressedModifiers st1 @?= 0
  sLatchedModifiers st1 @?= setModifier 0 modifier
  sLockedModifiers st1 @?= 0

-- Shift_L keycode = 50, keysymbol = Shift_L ==> Shift
-- keycode for a = 38
testStickyShiftAlphabet :: IsString a => (a -> IO()) -> Assertion
testStickyShiftAlphabet step = do
  let (keysym,r,st) = onPress 50 (pickInitialState "customDvorakSticky")
      st1 = onRelease 50 st
      (keysym2,r2,st2) = onPress 38 st1
  keysym @?= XKB_KEY_Shift_L
  keysym2 @?= XKB_KEY_A
  r @?= False
  r2 @?= True
  -- 1st press, latch
  -- step "1. checking effective modifiers"
  sEffectiveModifiers st @?= setModifier 0 Shift
  -- step "1. checking depressed modifiers"
  sDepressedModifiers st @?= setModifier 0 Shift
  -- step "1. checking latched modifiers"
  sLatchedModifiers st @?= setModifier 0 Shift
  -- step "1. checking locked modifiers"
  sLockedModifiers st @?= 0

  -- release
  -- step "2. checking effective modifiers"
  sEffectiveModifiers st1 @?= setModifier 0 Shift
  -- step "2. checking depressed modifiers"
  sDepressedModifiers st1 @?= 0
  -- step "2. checking latched modifiers"
  sLatchedModifiers st1 @?= setModifier 0 Shift
  -- step "2. checking locked modifiers"
  sLockedModifiers st1 @?= 0

  -- press alphabet
  -- step "3. checking effective modifiers"
  sEffectiveModifiers st2 @?= 0
  -- step "3. checking depressed modifiers"
  sDepressedModifiers st2 @?= 0
  -- step "3. checking latched modifiers"
  sLatchedModifiers st2 @?= 0
  -- step "3. checking locked modifiers"
  sLockedModifiers st2 @?= 0

-- Shift_L keycode = 50, keysymbol = Shift_L ==> Shift
-- keycode for a = 38
testStickyShiftLockAlphabet :: IsString a => (a -> IO()) -> Assertion
testStickyShiftLockAlphabet step = do
  let (keysym,r,st) = onPress 50 (pickInitialState "customDvorakSticky")
      st1 = onRelease 50 st
      (keysym2,r2,st2) = onPress 50 st1
      st3 = onRelease 50 st2
      (keysym4,r4,st4) = onPress 38 st3
      st5 = onRelease 38 st4
      (keysym6,r6,st6) = onPress 38 st5
      st7 = onRelease 38 st6

  keysym @?= XKB_KEY_Shift_L
  keysym2 @?= XKB_KEY_Shift_L
  keysym4 @?= XKB_KEY_A
  keysym6 @?= XKB_KEY_A
  r @?= False
  r2 @?= False
  r4 @?= True
  r6 @?= True
  -- 1st press, latch
  -- step "1. checking effective modifiers"
  sEffectiveModifiers st @?= setModifier 0 Shift
  -- step "1. checking depressed modifiers"
  sDepressedModifiers st @?= setModifier 0 Shift
  -- step "1. checking latched modifiers"
  sLatchedModifiers st @?= setModifier 0 Shift
  -- step "1. checking locked modifiers"
  sLockedModifiers st @?= 0

  -- release
  -- step "2. checking effective modifiers"
  sEffectiveModifiers st1 @?= setModifier 0 Shift
  -- step "2. checking depressed modifiers"
  sDepressedModifiers st1 @?= 0
  -- step "2. checking latched modifiers"
  sLatchedModifiers st1 @?= setModifier 0 Shift
  -- step "2. checking locked modifiers"
  sLockedModifiers st1 @?= 0

  -- 2nd press, lock
  -- step "3. checking effective modifiers"
  sEffectiveModifiers st2 @?= setModifier 0 Shift
  -- step "3. checking depressed modifiers"
  sDepressedModifiers st2 @?= setModifier 0 Shift
  -- step "3. checking latched modifiers"
  sLatchedModifiers st2 @?= 0
  -- step "3. checking locked modifiers"
  sLockedModifiers st2 @?= setModifier 0 Shift

  -- release
  -- step "4. checking effective modifiers"
  sEffectiveModifiers st3 @?= setModifier 0 Shift
  -- step "4. checking depressed modifiers"
  sDepressedModifiers st3 @?= 0
  -- step "4. checking latched modifiers"
  sLatchedModifiers st3 @?= 0
  -- step "4. checking locked modifiers"
  sLockedModifiers st3 @?= setModifier 0 Shift

  -- press alphabet
  -- step "5. checking effective modifiers"
  sEffectiveModifiers st6 @?= setModifier 0 Shift
  -- step "5. checking depressed modifiers"
  sDepressedModifiers st6 @?= 0
  -- step "5. checking latched modifiers"
  sLatchedModifiers st6 @?= 0
  -- step "5. checking locked modifiers"
  sLockedModifiers st6 @?= setModifier 0 Shift

  -- press alphabet
  -- step "6. checking effective modifiers"
  sEffectiveModifiers st7 @?= setModifier 0 Shift
  -- step "6. checking depressed modifiers"
  sDepressedModifiers st7 @?= 0
  -- step "6. checking latched modifiers"
  sLatchedModifiers st7 @?= 0
  -- step "6. checking locked modifiers"
  sLockedModifiers st7 @?= setModifier 0 Shift

-- -- locked shift loses lock with sticky on
-- -- Shift_L keycode = 50, keysymbol = Shift_L ==> Shift
-- testStickyLocking02 :: Assertion
-- testStickyLocking02 =
--   let original =
--         (pickInitialState 1)
--         { sEffectiveModifiers = setModifier 0 Shift
--         , sLockedModifiers = setModifier 0 Shift
--         }
--       s = original
--       expected =
--         original
--         { sDepressedModifiers = setModifier 0 Shift
--         , sEffectiveModifiers = setModifier 0 Shift
--         , sLockedModifiers = clearModifier 0 Shift
--         }
--   in (onKeyPress 50 original) @?=
--      (identifyStateChanges original expected, expected)

-- -- locked shift stays there with sticky on even when key released
-- -- Shift_L keycode = 50, keysymbol = Shift_L ==> Shift
-- testStickyLocking03 :: Assertion
-- testStickyLocking03 =
--   let original =
--         (pickInitialState 1)
--         { sEffectiveModifiers = setModifier 0 Shift
--         , sLockedModifiers = setModifier 0 Shift
--         }
--       s = original
--       expected =
--         original
--         { sEffectiveModifiers = setModifier 0 Shift
--         , sLockedModifiers = setModifier 0 Shift
--         }
--   in (onKeyRelease 50 original) @?=
--      (identifyStateChanges original expected, expected)

-- -- latched shift stays there with sticky on even when key released
-- -- Shift_L keycode = 50, keysymbol = Shift_L ==> Shift
-- testStickyLocking04 :: Assertion
-- testStickyLocking04 =
--   let original =
--         (pickInitialState 1)
--         { sDepressedModifiers = setModifier 0 Shift
--         , sEffectiveModifiers = setModifier 0 Shift
--         , sLatchedModifiers = setModifier 0 Shift
--         }
--       s = original
--       expected =
--         (pickInitialState 1)
--         { sEffectiveModifiers = setModifier 0 Shift
--         , sLatchedModifiers = setModifier 0 Shift
--         }
--   in (onKeyRelease 50 original) @?=
--      (identifyStateChanges original expected, expected)

-- -- shift twice should just be depressed with sticky off
-- testNonStickyLatching01 :: Assertion
-- testNonStickyLatching01 =
--   let original =
--         (pickInitialState 0)
--         { sDepressedModifiers = setModifier 0 Shift
--         , sEffectiveModifiers = setModifier 0 Shift
--         }
--       s = original
--       expected =
--         original
--         { sDepressedModifiers = setModifier 0 Shift
--         , sEffectiveModifiers = setModifier 0 Shift
--         }
--   in (onKeyPress 50 original) @?=
--      (identifyStateChanges original expected, expected)

-- -- shift release should release the depressed and effective with sticky off
-- testNonStickyLatching02 :: Assertion
-- testNonStickyLatching02 =
--   let original =
--         (pickInitialState 0)
--         { sDepressedModifiers = setModifier 0 Shift
--         , sEffectiveModifiers = setModifier 0 Shift
--         }
--       s = original
--       expected =
--         original
--         { sDepressedModifiers = 0
--         , sEffectiveModifiers = 0
--         }
--   in (onKeyRelease 50 original) @?=
--      (identifyStateChanges original expected, expected)
