
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE OverloadedStrings  #-}

module NamesPatterns where

import Foreign

-- https://www.schoolofhaskell.com/user/icelandj/Pattern%20synonyms

newtype Name = MkName { unName :: String } deriving (Eq, Show)

-- generated using the below commands:
--- sh generate_modifier_names.sh >|NamesPatterns.hsc


#include "libxkbcommon/xkbcommon/xkbcommon-names.h"

-- #define XKB_MOD_NAME_SHIFT      "Shift"
pattern XKB_MOD_NAME_SHIFT :: Name
pattern XKB_MOD_NAME_SHIFT = MkName #{const_str XKB_MOD_NAME_SHIFT} 
-- #define XKB_MOD_NAME_CAPS       "Lock"
pattern XKB_MOD_NAME_CAPS :: Name
pattern XKB_MOD_NAME_CAPS = MkName #{const_str XKB_MOD_NAME_CAPS} 
-- #define XKB_MOD_NAME_CTRL       "Control"
pattern XKB_MOD_NAME_CTRL :: Name
pattern XKB_MOD_NAME_CTRL = MkName #{const_str XKB_MOD_NAME_CTRL} 
-- #define XKB_MOD_NAME_ALT        "Mod1"
pattern XKB_MOD_NAME_ALT :: Name
pattern XKB_MOD_NAME_ALT = MkName #{const_str XKB_MOD_NAME_ALT} 
-- #define XKB_MOD_NAME_NUM        "Mod2"
pattern XKB_MOD_NAME_NUM :: Name
pattern XKB_MOD_NAME_NUM = MkName #{const_str XKB_MOD_NAME_NUM} 
-- #define XKB_MOD_NAME_LOGO       "Mod4"
pattern XKB_MOD_NAME_LOGO :: Name
pattern XKB_MOD_NAME_LOGO = MkName #{const_str XKB_MOD_NAME_LOGO} 
-- #define XKB_LED_NAME_CAPS       "Caps Lock"
pattern XKB_LED_NAME_CAPS :: Name
pattern XKB_LED_NAME_CAPS = MkName #{const_str XKB_LED_NAME_CAPS} 
-- #define XKB_LED_NAME_NUM        "Num Lock"
pattern XKB_LED_NAME_NUM :: Name
pattern XKB_LED_NAME_NUM = MkName #{const_str XKB_LED_NAME_NUM} 
-- #define XKB_LED_NAME_SCROLL     "Scroll Lock"
pattern XKB_LED_NAME_SCROLL :: Name
pattern XKB_LED_NAME_SCROLL = MkName #{const_str XKB_LED_NAME_SCROLL} 
