
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms  #-}

module KeySymbolDefinitions where

import Foreign
import Foreign.C.Types

newtype KeySymbol = MkKeySymbol { unKeySymbol :: CInt } deriving (Eq, Show)

-- https://www.schoolofhaskell.com/user/icelandj/Pattern%20synonyms

-- generated using the below commands:
-- grep "^#ifdef" xproto/keysymdef.h | sed --expression="s/\(^#ifdef \(\S*\).*\)/-- \1\n#define \2 1/" >|/tmp/keysymifdefs.txt
-- grep "^#define" xproto/keysymdef.h | sed --expression="s/\(^#define \(\S*\) .*\)/-- \1\npattern \2 = MkKeySymbol #const \2/" >|/tmp/keysymbols.txt


-- #ifdef XK_MISCELLANY
#define XK_MISCELLANY 1
-- #ifdef XK_XKB_KEYS
#define XK_XKB_KEYS 1
-- #ifdef XK_3270
#define XK_3270 1
-- #ifdef XK_LATIN1
#define XK_LATIN1 1
-- #ifdef XK_LATIN2
#define XK_LATIN2 1
-- #ifdef XK_LATIN3
#define XK_LATIN3 1
-- #ifdef XK_LATIN4
#define XK_LATIN4 1
-- #ifdef XK_LATIN8
#define XK_LATIN8 1
-- #ifdef XK_LATIN9
#define XK_LATIN9 1
-- #ifdef XK_KATAKANA
#define XK_KATAKANA 1
-- #ifdef XK_ARABIC
#define XK_ARABIC 1
-- #ifdef XK_CYRILLIC
#define XK_CYRILLIC 1
-- #ifdef XK_GREEK
#define XK_GREEK 1
-- #ifdef XK_TECHNICAL
#define XK_TECHNICAL 1
-- #ifdef XK_SPECIAL
#define XK_SPECIAL 1
-- #ifdef XK_PUBLISHING
#define XK_PUBLISHING 1
-- #ifdef XK_APL
#define XK_APL 1
-- #ifdef XK_HEBREW
#define XK_HEBREW 1
-- #ifdef XK_THAI
#define XK_THAI 1
-- #ifdef XK_KOREAN
#define XK_KOREAN 1
-- #ifdef XK_ARMENIAN
#define XK_ARMENIAN 1
-- #ifdef XK_GEORGIAN
#define XK_GEORGIAN 1
-- #ifdef XK_CAUCASUS
#define XK_CAUCASUS 1
-- #ifdef XK_VIETNAMESE
#define XK_VIETNAMESE 1
-- #ifdef XK_CURRENCY
#define XK_CURRENCY 1
-- #ifdef XK_MATHEMATICAL
#define XK_MATHEMATICAL 1
-- #ifdef XK_BRAILLE
#define XK_BRAILLE 1
-- #ifdef XK_SINHALA
#define XK_SINHALA 1

#include "xproto/keysymdef.h"

pattern XK_NoSymbol = MkKeySymbol 0

-- #define XK_VoidSymbol                  0xffffff  /* Void symbol */
pattern XK_VoidSymbol = MkKeySymbol #const XK_VoidSymbol
-- #define XK_BackSpace                     0xff08  /* Back space, back char */
pattern XK_BackSpace = MkKeySymbol #const XK_BackSpace
-- #define XK_Tab                           0xff09
pattern XK_Tab = MkKeySymbol #const XK_Tab
-- #define XK_Linefeed                      0xff0a  /* Linefeed, LF */
pattern XK_Linefeed = MkKeySymbol #const XK_Linefeed
-- #define XK_Clear                         0xff0b
pattern XK_Clear = MkKeySymbol #const XK_Clear
-- #define XK_Return                        0xff0d  /* Return, enter */
pattern XK_Return = MkKeySymbol #const XK_Return
-- #define XK_Pause                         0xff13  /* Pause, hold */
pattern XK_Pause = MkKeySymbol #const XK_Pause
-- #define XK_Scroll_Lock                   0xff14
pattern XK_Scroll_Lock = MkKeySymbol #const XK_Scroll_Lock
-- #define XK_Sys_Req                       0xff15
pattern XK_Sys_Req = MkKeySymbol #const XK_Sys_Req
-- #define XK_Escape                        0xff1b
pattern XK_Escape = MkKeySymbol #const XK_Escape
-- #define XK_Delete                        0xffff  /* Delete, rubout */
pattern XK_Delete = MkKeySymbol #const XK_Delete
-- #define XK_Multi_key                     0xff20  /* Multi-key character compose */
pattern XK_Multi_key = MkKeySymbol #const XK_Multi_key
-- #define XK_Codeinput                     0xff37
pattern XK_Codeinput = MkKeySymbol #const XK_Codeinput
-- #define XK_SingleCandidate               0xff3c
pattern XK_SingleCandidate = MkKeySymbol #const XK_SingleCandidate
-- #define XK_MultipleCandidate             0xff3d
pattern XK_MultipleCandidate = MkKeySymbol #const XK_MultipleCandidate
-- #define XK_PreviousCandidate             0xff3e
pattern XK_PreviousCandidate = MkKeySymbol #const XK_PreviousCandidate
-- #define XK_Kanji                         0xff21  /* Kanji, Kanji convert */
pattern XK_Kanji = MkKeySymbol #const XK_Kanji
-- #define XK_Muhenkan                      0xff22  /* Cancel Conversion */
pattern XK_Muhenkan = MkKeySymbol #const XK_Muhenkan
-- #define XK_Henkan_Mode                   0xff23  /* Start/Stop Conversion */
pattern XK_Henkan_Mode = MkKeySymbol #const XK_Henkan_Mode
-- #define XK_Henkan                        0xff23  /* Alias for Henkan_Mode */
pattern XK_Henkan = MkKeySymbol #const XK_Henkan
-- #define XK_Romaji                        0xff24  /* to Romaji */
pattern XK_Romaji = MkKeySymbol #const XK_Romaji
-- #define XK_Hiragana                      0xff25  /* to Hiragana */
pattern XK_Hiragana = MkKeySymbol #const XK_Hiragana
-- #define XK_Katakana                      0xff26  /* to Katakana */
pattern XK_Katakana = MkKeySymbol #const XK_Katakana
-- #define XK_Hiragana_Katakana             0xff27  /* Hiragana/Katakana toggle */
pattern XK_Hiragana_Katakana = MkKeySymbol #const XK_Hiragana_Katakana
-- #define XK_Zenkaku                       0xff28  /* to Zenkaku */
pattern XK_Zenkaku = MkKeySymbol #const XK_Zenkaku
-- #define XK_Hankaku                       0xff29  /* to Hankaku */
pattern XK_Hankaku = MkKeySymbol #const XK_Hankaku
-- #define XK_Zenkaku_Hankaku               0xff2a  /* Zenkaku/Hankaku toggle */
pattern XK_Zenkaku_Hankaku = MkKeySymbol #const XK_Zenkaku_Hankaku
-- #define XK_Touroku                       0xff2b  /* Add to Dictionary */
pattern XK_Touroku = MkKeySymbol #const XK_Touroku
-- #define XK_Massyo                        0xff2c  /* Delete from Dictionary */
pattern XK_Massyo = MkKeySymbol #const XK_Massyo
-- #define XK_Kana_Lock                     0xff2d  /* Kana Lock */
pattern XK_Kana_Lock = MkKeySymbol #const XK_Kana_Lock
-- #define XK_Kana_Shift                    0xff2e  /* Kana Shift */
pattern XK_Kana_Shift = MkKeySymbol #const XK_Kana_Shift
-- #define XK_Eisu_Shift                    0xff2f  /* Alphanumeric Shift */
pattern XK_Eisu_Shift = MkKeySymbol #const XK_Eisu_Shift
-- #define XK_Eisu_toggle                   0xff30  /* Alphanumeric toggle */
pattern XK_Eisu_toggle = MkKeySymbol #const XK_Eisu_toggle
-- #define XK_Kanji_Bangou                  0xff37  /* Codeinput */
pattern XK_Kanji_Bangou = MkKeySymbol #const XK_Kanji_Bangou
-- #define XK_Zen_Koho                      0xff3d  /* Multiple/All Candidate(s) */
pattern XK_Zen_Koho = MkKeySymbol #const XK_Zen_Koho
-- #define XK_Mae_Koho                      0xff3e  /* Previous Candidate */
pattern XK_Mae_Koho = MkKeySymbol #const XK_Mae_Koho
-- #define XK_Home                          0xff50
pattern XK_Home = MkKeySymbol #const XK_Home
-- #define XK_Left                          0xff51  /* Move left, left arrow */
pattern XK_Left = MkKeySymbol #const XK_Left
-- #define XK_Up                            0xff52  /* Move up, up arrow */
pattern XK_Up = MkKeySymbol #const XK_Up
-- #define XK_Right                         0xff53  /* Move right, right arrow */
pattern XK_Right = MkKeySymbol #const XK_Right
-- #define XK_Down                          0xff54  /* Move down, down arrow */
pattern XK_Down = MkKeySymbol #const XK_Down
-- #define XK_Prior                         0xff55  /* Prior, previous */
pattern XK_Prior = MkKeySymbol #const XK_Prior
-- #define XK_Page_Up                       0xff55
pattern XK_Page_Up = MkKeySymbol #const XK_Page_Up
-- #define XK_Next                          0xff56  /* Next */
pattern XK_Next = MkKeySymbol #const XK_Next
-- #define XK_Page_Down                     0xff56
pattern XK_Page_Down = MkKeySymbol #const XK_Page_Down
-- #define XK_End                           0xff57  /* EOL */
pattern XK_End = MkKeySymbol #const XK_End
-- #define XK_Begin                         0xff58  /* BOL */
pattern XK_Begin = MkKeySymbol #const XK_Begin
-- #define XK_Select                        0xff60  /* Select, mark */
pattern XK_Select = MkKeySymbol #const XK_Select
-- #define XK_Print                         0xff61
pattern XK_Print = MkKeySymbol #const XK_Print
-- #define XK_Execute                       0xff62  /* Execute, run, do */
pattern XK_Execute = MkKeySymbol #const XK_Execute
-- #define XK_Insert                        0xff63  /* Insert, insert here */
pattern XK_Insert = MkKeySymbol #const XK_Insert
-- #define XK_Undo                          0xff65
pattern XK_Undo = MkKeySymbol #const XK_Undo
-- #define XK_Redo                          0xff66  /* Redo, again */
pattern XK_Redo = MkKeySymbol #const XK_Redo
-- #define XK_Menu                          0xff67
pattern XK_Menu = MkKeySymbol #const XK_Menu
-- #define XK_Find                          0xff68  /* Find, search */
pattern XK_Find = MkKeySymbol #const XK_Find
-- #define XK_Cancel                        0xff69  /* Cancel, stop, abort, exit */
pattern XK_Cancel = MkKeySymbol #const XK_Cancel
-- #define XK_Help                          0xff6a  /* Help */
pattern XK_Help = MkKeySymbol #const XK_Help
-- #define XK_Break                         0xff6b
pattern XK_Break = MkKeySymbol #const XK_Break
-- #define XK_Mode_switch                   0xff7e  /* Character set switch */
pattern XK_Mode_switch = MkKeySymbol #const XK_Mode_switch
-- #define XK_script_switch                 0xff7e  /* Alias for mode_switch */
pattern XK_script_switch = MkKeySymbol #const XK_script_switch
-- #define XK_Num_Lock                      0xff7f
pattern XK_Num_Lock = MkKeySymbol #const XK_Num_Lock
-- #define XK_KP_Space                      0xff80  /* Space */
pattern XK_KP_Space = MkKeySymbol #const XK_KP_Space
-- #define XK_KP_Tab                        0xff89
pattern XK_KP_Tab = MkKeySymbol #const XK_KP_Tab
-- #define XK_KP_Enter                      0xff8d  /* Enter */
pattern XK_KP_Enter = MkKeySymbol #const XK_KP_Enter
-- #define XK_KP_F1                         0xff91  /* PF1, KP_A, ... */
pattern XK_KP_F1 = MkKeySymbol #const XK_KP_F1
-- #define XK_KP_F2                         0xff92
pattern XK_KP_F2 = MkKeySymbol #const XK_KP_F2
-- #define XK_KP_F3                         0xff93
pattern XK_KP_F3 = MkKeySymbol #const XK_KP_F3
-- #define XK_KP_F4                         0xff94
pattern XK_KP_F4 = MkKeySymbol #const XK_KP_F4
-- #define XK_KP_Home                       0xff95
pattern XK_KP_Home = MkKeySymbol #const XK_KP_Home
-- #define XK_KP_Left                       0xff96
pattern XK_KP_Left = MkKeySymbol #const XK_KP_Left
-- #define XK_KP_Up                         0xff97
pattern XK_KP_Up = MkKeySymbol #const XK_KP_Up
-- #define XK_KP_Right                      0xff98
pattern XK_KP_Right = MkKeySymbol #const XK_KP_Right
-- #define XK_KP_Down                       0xff99
pattern XK_KP_Down = MkKeySymbol #const XK_KP_Down
-- #define XK_KP_Prior                      0xff9a
pattern XK_KP_Prior = MkKeySymbol #const XK_KP_Prior
-- #define XK_KP_Page_Up                    0xff9a
pattern XK_KP_Page_Up = MkKeySymbol #const XK_KP_Page_Up
-- #define XK_KP_Next                       0xff9b
pattern XK_KP_Next = MkKeySymbol #const XK_KP_Next
-- #define XK_KP_Page_Down                  0xff9b
pattern XK_KP_Page_Down = MkKeySymbol #const XK_KP_Page_Down
-- #define XK_KP_End                        0xff9c
pattern XK_KP_End = MkKeySymbol #const XK_KP_End
-- #define XK_KP_Begin                      0xff9d
pattern XK_KP_Begin = MkKeySymbol #const XK_KP_Begin
-- #define XK_KP_Insert                     0xff9e
pattern XK_KP_Insert = MkKeySymbol #const XK_KP_Insert
-- #define XK_KP_Delete                     0xff9f
pattern XK_KP_Delete = MkKeySymbol #const XK_KP_Delete
-- #define XK_KP_Equal                      0xffbd  /* Equals */
pattern XK_KP_Equal = MkKeySymbol #const XK_KP_Equal
-- #define XK_KP_Multiply                   0xffaa
pattern XK_KP_Multiply = MkKeySymbol #const XK_KP_Multiply
-- #define XK_KP_Add                        0xffab
pattern XK_KP_Add = MkKeySymbol #const XK_KP_Add
-- #define XK_KP_Separator                  0xffac  /* Separator, often comma */
pattern XK_KP_Separator = MkKeySymbol #const XK_KP_Separator
-- #define XK_KP_Subtract                   0xffad
pattern XK_KP_Subtract = MkKeySymbol #const XK_KP_Subtract
-- #define XK_KP_Decimal                    0xffae
pattern XK_KP_Decimal = MkKeySymbol #const XK_KP_Decimal
-- #define XK_KP_Divide                     0xffaf
pattern XK_KP_Divide = MkKeySymbol #const XK_KP_Divide
-- #define XK_KP_0                          0xffb0
pattern XK_KP_0 = MkKeySymbol #const XK_KP_0
-- #define XK_KP_1                          0xffb1
pattern XK_KP_1 = MkKeySymbol #const XK_KP_1
-- #define XK_KP_2                          0xffb2
pattern XK_KP_2 = MkKeySymbol #const XK_KP_2
-- #define XK_KP_3                          0xffb3
pattern XK_KP_3 = MkKeySymbol #const XK_KP_3
-- #define XK_KP_4                          0xffb4
pattern XK_KP_4 = MkKeySymbol #const XK_KP_4
-- #define XK_KP_5                          0xffb5
pattern XK_KP_5 = MkKeySymbol #const XK_KP_5
-- #define XK_KP_6                          0xffb6
pattern XK_KP_6 = MkKeySymbol #const XK_KP_6
-- #define XK_KP_7                          0xffb7
pattern XK_KP_7 = MkKeySymbol #const XK_KP_7
-- #define XK_KP_8                          0xffb8
pattern XK_KP_8 = MkKeySymbol #const XK_KP_8
-- #define XK_KP_9                          0xffb9
pattern XK_KP_9 = MkKeySymbol #const XK_KP_9
-- #define XK_F1                            0xffbe
pattern XK_F1 = MkKeySymbol #const XK_F1
-- #define XK_F2                            0xffbf
pattern XK_F2 = MkKeySymbol #const XK_F2
-- #define XK_F3                            0xffc0
pattern XK_F3 = MkKeySymbol #const XK_F3
-- #define XK_F4                            0xffc1
pattern XK_F4 = MkKeySymbol #const XK_F4
-- #define XK_F5                            0xffc2
pattern XK_F5 = MkKeySymbol #const XK_F5
-- #define XK_F6                            0xffc3
pattern XK_F6 = MkKeySymbol #const XK_F6
-- #define XK_F7                            0xffc4
pattern XK_F7 = MkKeySymbol #const XK_F7
-- #define XK_F8                            0xffc5
pattern XK_F8 = MkKeySymbol #const XK_F8
-- #define XK_F9                            0xffc6
pattern XK_F9 = MkKeySymbol #const XK_F9
-- #define XK_F10                           0xffc7
pattern XK_F10 = MkKeySymbol #const XK_F10
-- #define XK_F11                           0xffc8
pattern XK_F11 = MkKeySymbol #const XK_F11
-- #define XK_L1                            0xffc8
pattern XK_L1 = MkKeySymbol #const XK_L1
-- #define XK_F12                           0xffc9
pattern XK_F12 = MkKeySymbol #const XK_F12
-- #define XK_L2                            0xffc9
pattern XK_L2 = MkKeySymbol #const XK_L2
-- #define XK_F13                           0xffca
pattern XK_F13 = MkKeySymbol #const XK_F13
-- #define XK_L3                            0xffca
pattern XK_L3 = MkKeySymbol #const XK_L3
-- #define XK_F14                           0xffcb
pattern XK_F14 = MkKeySymbol #const XK_F14
-- #define XK_L4                            0xffcb
pattern XK_L4 = MkKeySymbol #const XK_L4
-- #define XK_F15                           0xffcc
pattern XK_F15 = MkKeySymbol #const XK_F15
-- #define XK_L5                            0xffcc
pattern XK_L5 = MkKeySymbol #const XK_L5
-- #define XK_F16                           0xffcd
pattern XK_F16 = MkKeySymbol #const XK_F16
-- #define XK_L6                            0xffcd
pattern XK_L6 = MkKeySymbol #const XK_L6
-- #define XK_F17                           0xffce
pattern XK_F17 = MkKeySymbol #const XK_F17
-- #define XK_L7                            0xffce
pattern XK_L7 = MkKeySymbol #const XK_L7
-- #define XK_F18                           0xffcf
pattern XK_F18 = MkKeySymbol #const XK_F18
-- #define XK_L8                            0xffcf
pattern XK_L8 = MkKeySymbol #const XK_L8
-- #define XK_F19                           0xffd0
pattern XK_F19 = MkKeySymbol #const XK_F19
-- #define XK_L9                            0xffd0
pattern XK_L9 = MkKeySymbol #const XK_L9
-- #define XK_F20                           0xffd1
pattern XK_F20 = MkKeySymbol #const XK_F20
-- #define XK_L10                           0xffd1
pattern XK_L10 = MkKeySymbol #const XK_L10
-- #define XK_F21                           0xffd2
pattern XK_F21 = MkKeySymbol #const XK_F21
-- #define XK_R1                            0xffd2
pattern XK_R1 = MkKeySymbol #const XK_R1
-- #define XK_F22                           0xffd3
pattern XK_F22 = MkKeySymbol #const XK_F22
-- #define XK_R2                            0xffd3
pattern XK_R2 = MkKeySymbol #const XK_R2
-- #define XK_F23                           0xffd4
pattern XK_F23 = MkKeySymbol #const XK_F23
-- #define XK_R3                            0xffd4
pattern XK_R3 = MkKeySymbol #const XK_R3
-- #define XK_F24                           0xffd5
pattern XK_F24 = MkKeySymbol #const XK_F24
-- #define XK_R4                            0xffd5
pattern XK_R4 = MkKeySymbol #const XK_R4
-- #define XK_F25                           0xffd6
pattern XK_F25 = MkKeySymbol #const XK_F25
-- #define XK_R5                            0xffd6
pattern XK_R5 = MkKeySymbol #const XK_R5
-- #define XK_F26                           0xffd7
pattern XK_F26 = MkKeySymbol #const XK_F26
-- #define XK_R6                            0xffd7
pattern XK_R6 = MkKeySymbol #const XK_R6
-- #define XK_F27                           0xffd8
pattern XK_F27 = MkKeySymbol #const XK_F27
-- #define XK_R7                            0xffd8
pattern XK_R7 = MkKeySymbol #const XK_R7
-- #define XK_F28                           0xffd9
pattern XK_F28 = MkKeySymbol #const XK_F28
-- #define XK_R8                            0xffd9
pattern XK_R8 = MkKeySymbol #const XK_R8
-- #define XK_F29                           0xffda
pattern XK_F29 = MkKeySymbol #const XK_F29
-- #define XK_R9                            0xffda
pattern XK_R9 = MkKeySymbol #const XK_R9
-- #define XK_F30                           0xffdb
pattern XK_F30 = MkKeySymbol #const XK_F30
-- #define XK_R10                           0xffdb
pattern XK_R10 = MkKeySymbol #const XK_R10
-- #define XK_F31                           0xffdc
pattern XK_F31 = MkKeySymbol #const XK_F31
-- #define XK_R11                           0xffdc
pattern XK_R11 = MkKeySymbol #const XK_R11
-- #define XK_F32                           0xffdd
pattern XK_F32 = MkKeySymbol #const XK_F32
-- #define XK_R12                           0xffdd
pattern XK_R12 = MkKeySymbol #const XK_R12
-- #define XK_F33                           0xffde
pattern XK_F33 = MkKeySymbol #const XK_F33
-- #define XK_R13                           0xffde
pattern XK_R13 = MkKeySymbol #const XK_R13
-- #define XK_F34                           0xffdf
pattern XK_F34 = MkKeySymbol #const XK_F34
-- #define XK_R14                           0xffdf
pattern XK_R14 = MkKeySymbol #const XK_R14
-- #define XK_F35                           0xffe0
pattern XK_F35 = MkKeySymbol #const XK_F35
-- #define XK_R15                           0xffe0
pattern XK_R15 = MkKeySymbol #const XK_R15
-- #define XK_Shift_L                       0xffe1  /* Left shift */
pattern XK_Shift_L = MkKeySymbol #const XK_Shift_L
-- #define XK_Shift_R                       0xffe2  /* Right shift */
pattern XK_Shift_R = MkKeySymbol #const XK_Shift_R
-- #define XK_Control_L                     0xffe3  /* Left control */
pattern XK_Control_L = MkKeySymbol #const XK_Control_L
-- #define XK_Control_R                     0xffe4  /* Right control */
pattern XK_Control_R = MkKeySymbol #const XK_Control_R
-- #define XK_Caps_Lock                     0xffe5  /* Caps lock */
pattern XK_Caps_Lock = MkKeySymbol #const XK_Caps_Lock
-- #define XK_Shift_Lock                    0xffe6  /* Shift lock */
pattern XK_Shift_Lock = MkKeySymbol #const XK_Shift_Lock
-- #define XK_Meta_L                        0xffe7  /* Left meta */
pattern XK_Meta_L = MkKeySymbol #const XK_Meta_L
-- #define XK_Meta_R                        0xffe8  /* Right meta */
pattern XK_Meta_R = MkKeySymbol #const XK_Meta_R
-- #define XK_Alt_L                         0xffe9  /* Left alt */
pattern XK_Alt_L = MkKeySymbol #const XK_Alt_L
-- #define XK_Alt_R                         0xffea  /* Right alt */
pattern XK_Alt_R = MkKeySymbol #const XK_Alt_R
-- #define XK_Super_L                       0xffeb  /* Left super */
pattern XK_Super_L = MkKeySymbol #const XK_Super_L
-- #define XK_Super_R                       0xffec  /* Right super */
pattern XK_Super_R = MkKeySymbol #const XK_Super_R
-- #define XK_Hyper_L                       0xffed  /* Left hyper */
pattern XK_Hyper_L = MkKeySymbol #const XK_Hyper_L
-- #define XK_Hyper_R                       0xffee  /* Right hyper */
pattern XK_Hyper_R = MkKeySymbol #const XK_Hyper_R
-- #define XK_ISO_Lock                      0xfe01
pattern XK_ISO_Lock = MkKeySymbol #const XK_ISO_Lock
-- #define XK_ISO_Level2_Latch              0xfe02
pattern XK_ISO_Level2_Latch = MkKeySymbol #const XK_ISO_Level2_Latch
-- #define XK_ISO_Level3_Shift              0xfe03
pattern XK_ISO_Level3_Shift = MkKeySymbol #const XK_ISO_Level3_Shift
-- #define XK_ISO_Level3_Latch              0xfe04
pattern XK_ISO_Level3_Latch = MkKeySymbol #const XK_ISO_Level3_Latch
-- #define XK_ISO_Level3_Lock               0xfe05
pattern XK_ISO_Level3_Lock = MkKeySymbol #const XK_ISO_Level3_Lock
-- #define XK_ISO_Level5_Shift              0xfe11
pattern XK_ISO_Level5_Shift = MkKeySymbol #const XK_ISO_Level5_Shift
-- #define XK_ISO_Level5_Latch              0xfe12
pattern XK_ISO_Level5_Latch = MkKeySymbol #const XK_ISO_Level5_Latch
-- #define XK_ISO_Level5_Lock               0xfe13
pattern XK_ISO_Level5_Lock = MkKeySymbol #const XK_ISO_Level5_Lock
-- #define XK_ISO_Group_Shift               0xff7e  /* Alias for mode_switch */
pattern XK_ISO_Group_Shift = MkKeySymbol #const XK_ISO_Group_Shift
-- #define XK_ISO_Group_Latch               0xfe06
pattern XK_ISO_Group_Latch = MkKeySymbol #const XK_ISO_Group_Latch
-- #define XK_ISO_Group_Lock                0xfe07
pattern XK_ISO_Group_Lock = MkKeySymbol #const XK_ISO_Group_Lock
-- #define XK_ISO_Next_Group                0xfe08
pattern XK_ISO_Next_Group = MkKeySymbol #const XK_ISO_Next_Group
-- #define XK_ISO_Next_Group_Lock           0xfe09
pattern XK_ISO_Next_Group_Lock = MkKeySymbol #const XK_ISO_Next_Group_Lock
-- #define XK_ISO_Prev_Group                0xfe0a
pattern XK_ISO_Prev_Group = MkKeySymbol #const XK_ISO_Prev_Group
-- #define XK_ISO_Prev_Group_Lock           0xfe0b
pattern XK_ISO_Prev_Group_Lock = MkKeySymbol #const XK_ISO_Prev_Group_Lock
-- #define XK_ISO_First_Group               0xfe0c
pattern XK_ISO_First_Group = MkKeySymbol #const XK_ISO_First_Group
-- #define XK_ISO_First_Group_Lock          0xfe0d
pattern XK_ISO_First_Group_Lock = MkKeySymbol #const XK_ISO_First_Group_Lock
-- #define XK_ISO_Last_Group                0xfe0e
pattern XK_ISO_Last_Group = MkKeySymbol #const XK_ISO_Last_Group
-- #define XK_ISO_Last_Group_Lock           0xfe0f
pattern XK_ISO_Last_Group_Lock = MkKeySymbol #const XK_ISO_Last_Group_Lock
-- #define XK_ISO_Left_Tab                  0xfe20
pattern XK_ISO_Left_Tab = MkKeySymbol #const XK_ISO_Left_Tab
-- #define XK_ISO_Move_Line_Up              0xfe21
pattern XK_ISO_Move_Line_Up = MkKeySymbol #const XK_ISO_Move_Line_Up
-- #define XK_ISO_Move_Line_Down            0xfe22
pattern XK_ISO_Move_Line_Down = MkKeySymbol #const XK_ISO_Move_Line_Down
-- #define XK_ISO_Partial_Line_Up           0xfe23
pattern XK_ISO_Partial_Line_Up = MkKeySymbol #const XK_ISO_Partial_Line_Up
-- #define XK_ISO_Partial_Line_Down         0xfe24
pattern XK_ISO_Partial_Line_Down = MkKeySymbol #const XK_ISO_Partial_Line_Down
-- #define XK_ISO_Partial_Space_Left        0xfe25
pattern XK_ISO_Partial_Space_Left = MkKeySymbol #const XK_ISO_Partial_Space_Left
-- #define XK_ISO_Partial_Space_Right       0xfe26
pattern XK_ISO_Partial_Space_Right = MkKeySymbol #const XK_ISO_Partial_Space_Right
-- #define XK_ISO_Set_Margin_Left           0xfe27
pattern XK_ISO_Set_Margin_Left = MkKeySymbol #const XK_ISO_Set_Margin_Left
-- #define XK_ISO_Set_Margin_Right          0xfe28
pattern XK_ISO_Set_Margin_Right = MkKeySymbol #const XK_ISO_Set_Margin_Right
-- #define XK_ISO_Release_Margin_Left       0xfe29
pattern XK_ISO_Release_Margin_Left = MkKeySymbol #const XK_ISO_Release_Margin_Left
-- #define XK_ISO_Release_Margin_Right      0xfe2a
pattern XK_ISO_Release_Margin_Right = MkKeySymbol #const XK_ISO_Release_Margin_Right
-- #define XK_ISO_Release_Both_Margins      0xfe2b
pattern XK_ISO_Release_Both_Margins = MkKeySymbol #const XK_ISO_Release_Both_Margins
-- #define XK_ISO_Fast_Cursor_Left          0xfe2c
pattern XK_ISO_Fast_Cursor_Left = MkKeySymbol #const XK_ISO_Fast_Cursor_Left
-- #define XK_ISO_Fast_Cursor_Right         0xfe2d
pattern XK_ISO_Fast_Cursor_Right = MkKeySymbol #const XK_ISO_Fast_Cursor_Right
-- #define XK_ISO_Fast_Cursor_Up            0xfe2e
pattern XK_ISO_Fast_Cursor_Up = MkKeySymbol #const XK_ISO_Fast_Cursor_Up
-- #define XK_ISO_Fast_Cursor_Down          0xfe2f
pattern XK_ISO_Fast_Cursor_Down = MkKeySymbol #const XK_ISO_Fast_Cursor_Down
-- #define XK_ISO_Continuous_Underline      0xfe30
pattern XK_ISO_Continuous_Underline = MkKeySymbol #const XK_ISO_Continuous_Underline
-- #define XK_ISO_Discontinuous_Underline   0xfe31
pattern XK_ISO_Discontinuous_Underline = MkKeySymbol #const XK_ISO_Discontinuous_Underline
-- #define XK_ISO_Emphasize                 0xfe32
pattern XK_ISO_Emphasize = MkKeySymbol #const XK_ISO_Emphasize
-- #define XK_ISO_Center_Object             0xfe33
pattern XK_ISO_Center_Object = MkKeySymbol #const XK_ISO_Center_Object
-- #define XK_ISO_Enter                     0xfe34
pattern XK_ISO_Enter = MkKeySymbol #const XK_ISO_Enter
-- #define XK_dead_grave                    0xfe50
pattern XK_dead_grave = MkKeySymbol #const XK_dead_grave
-- #define XK_dead_acute                    0xfe51
pattern XK_dead_acute = MkKeySymbol #const XK_dead_acute
-- #define XK_dead_circumflex               0xfe52
pattern XK_dead_circumflex = MkKeySymbol #const XK_dead_circumflex
-- #define XK_dead_tilde                    0xfe53
pattern XK_dead_tilde = MkKeySymbol #const XK_dead_tilde
-- #define XK_dead_perispomeni              0xfe53  /* alias for dead_tilde */
pattern XK_dead_perispomeni = MkKeySymbol #const XK_dead_perispomeni
-- #define XK_dead_macron                   0xfe54
pattern XK_dead_macron = MkKeySymbol #const XK_dead_macron
-- #define XK_dead_breve                    0xfe55
pattern XK_dead_breve = MkKeySymbol #const XK_dead_breve
-- #define XK_dead_abovedot                 0xfe56
pattern XK_dead_abovedot = MkKeySymbol #const XK_dead_abovedot
-- #define XK_dead_diaeresis                0xfe57
pattern XK_dead_diaeresis = MkKeySymbol #const XK_dead_diaeresis
-- #define XK_dead_abovering                0xfe58
pattern XK_dead_abovering = MkKeySymbol #const XK_dead_abovering
-- #define XK_dead_doubleacute              0xfe59
pattern XK_dead_doubleacute = MkKeySymbol #const XK_dead_doubleacute
-- #define XK_dead_caron                    0xfe5a
pattern XK_dead_caron = MkKeySymbol #const XK_dead_caron
-- #define XK_dead_cedilla                  0xfe5b
pattern XK_dead_cedilla = MkKeySymbol #const XK_dead_cedilla
-- #define XK_dead_ogonek                   0xfe5c
pattern XK_dead_ogonek = MkKeySymbol #const XK_dead_ogonek
-- #define XK_dead_iota                     0xfe5d
pattern XK_dead_iota = MkKeySymbol #const XK_dead_iota
-- #define XK_dead_voiced_sound             0xfe5e
pattern XK_dead_voiced_sound = MkKeySymbol #const XK_dead_voiced_sound
-- #define XK_dead_semivoiced_sound         0xfe5f
pattern XK_dead_semivoiced_sound = MkKeySymbol #const XK_dead_semivoiced_sound
-- #define XK_dead_belowdot                 0xfe60
pattern XK_dead_belowdot = MkKeySymbol #const XK_dead_belowdot
-- #define XK_dead_hook                     0xfe61
pattern XK_dead_hook = MkKeySymbol #const XK_dead_hook
-- #define XK_dead_horn                     0xfe62
pattern XK_dead_horn = MkKeySymbol #const XK_dead_horn
-- #define XK_dead_stroke                   0xfe63
pattern XK_dead_stroke = MkKeySymbol #const XK_dead_stroke
-- #define XK_dead_abovecomma               0xfe64
pattern XK_dead_abovecomma = MkKeySymbol #const XK_dead_abovecomma
-- #define XK_dead_psili                    0xfe64  /* alias for dead_abovecomma */
pattern XK_dead_psili = MkKeySymbol #const XK_dead_psili
-- #define XK_dead_abovereversedcomma       0xfe65
pattern XK_dead_abovereversedcomma = MkKeySymbol #const XK_dead_abovereversedcomma
-- #define XK_dead_dasia                    0xfe65  /* alias for dead_abovereversedcomma */
pattern XK_dead_dasia = MkKeySymbol #const XK_dead_dasia
-- #define XK_dead_doublegrave              0xfe66
pattern XK_dead_doublegrave = MkKeySymbol #const XK_dead_doublegrave
-- #define XK_dead_belowring                0xfe67
pattern XK_dead_belowring = MkKeySymbol #const XK_dead_belowring
-- #define XK_dead_belowmacron              0xfe68
pattern XK_dead_belowmacron = MkKeySymbol #const XK_dead_belowmacron
-- #define XK_dead_belowcircumflex          0xfe69
pattern XK_dead_belowcircumflex = MkKeySymbol #const XK_dead_belowcircumflex
-- #define XK_dead_belowtilde               0xfe6a
pattern XK_dead_belowtilde = MkKeySymbol #const XK_dead_belowtilde
-- #define XK_dead_belowbreve               0xfe6b
pattern XK_dead_belowbreve = MkKeySymbol #const XK_dead_belowbreve
-- #define XK_dead_belowdiaeresis           0xfe6c
pattern XK_dead_belowdiaeresis = MkKeySymbol #const XK_dead_belowdiaeresis
-- #define XK_dead_invertedbreve            0xfe6d
pattern XK_dead_invertedbreve = MkKeySymbol #const XK_dead_invertedbreve
-- #define XK_dead_belowcomma               0xfe6e
pattern XK_dead_belowcomma = MkKeySymbol #const XK_dead_belowcomma
-- #define XK_dead_currency                 0xfe6f
pattern XK_dead_currency = MkKeySymbol #const XK_dead_currency
-- #define XK_dead_lowline                  0xfe90
pattern XK_dead_lowline = MkKeySymbol #const XK_dead_lowline
-- #define XK_dead_aboveverticalline        0xfe91
pattern XK_dead_aboveverticalline = MkKeySymbol #const XK_dead_aboveverticalline
-- #define XK_dead_belowverticalline        0xfe92
pattern XK_dead_belowverticalline = MkKeySymbol #const XK_dead_belowverticalline
-- #define XK_dead_longsolidusoverlay       0xfe93
pattern XK_dead_longsolidusoverlay = MkKeySymbol #const XK_dead_longsolidusoverlay
-- #define XK_dead_a                        0xfe80
pattern XK_dead_a = MkKeySymbol #const XK_dead_a
-- #define XK_dead_A                        0xfe81
pattern XK_dead_A = MkKeySymbol #const XK_dead_A
-- #define XK_dead_e                        0xfe82
pattern XK_dead_e = MkKeySymbol #const XK_dead_e
-- #define XK_dead_E                        0xfe83
pattern XK_dead_E = MkKeySymbol #const XK_dead_E
-- #define XK_dead_i                        0xfe84
pattern XK_dead_i = MkKeySymbol #const XK_dead_i
-- #define XK_dead_I                        0xfe85
pattern XK_dead_I = MkKeySymbol #const XK_dead_I
-- #define XK_dead_o                        0xfe86
pattern XK_dead_o = MkKeySymbol #const XK_dead_o
-- #define XK_dead_O                        0xfe87
pattern XK_dead_O = MkKeySymbol #const XK_dead_O
-- #define XK_dead_u                        0xfe88
pattern XK_dead_u = MkKeySymbol #const XK_dead_u
-- #define XK_dead_U                        0xfe89
pattern XK_dead_U = MkKeySymbol #const XK_dead_U
-- #define XK_dead_small_schwa              0xfe8a
pattern XK_dead_small_schwa = MkKeySymbol #const XK_dead_small_schwa
-- #define XK_dead_capital_schwa            0xfe8b
pattern XK_dead_capital_schwa = MkKeySymbol #const XK_dead_capital_schwa
-- #define XK_dead_greek                    0xfe8c
pattern XK_dead_greek = MkKeySymbol #const XK_dead_greek
-- #define XK_First_Virtual_Screen          0xfed0
pattern XK_First_Virtual_Screen = MkKeySymbol #const XK_First_Virtual_Screen
-- #define XK_Prev_Virtual_Screen           0xfed1
pattern XK_Prev_Virtual_Screen = MkKeySymbol #const XK_Prev_Virtual_Screen
-- #define XK_Next_Virtual_Screen           0xfed2
pattern XK_Next_Virtual_Screen = MkKeySymbol #const XK_Next_Virtual_Screen
-- #define XK_Last_Virtual_Screen           0xfed4
pattern XK_Last_Virtual_Screen = MkKeySymbol #const XK_Last_Virtual_Screen
-- #define XK_Terminate_Server              0xfed5
pattern XK_Terminate_Server = MkKeySymbol #const XK_Terminate_Server
-- #define XK_AccessX_Enable                0xfe70
pattern XK_AccessX_Enable = MkKeySymbol #const XK_AccessX_Enable
-- #define XK_AccessX_Feedback_Enable       0xfe71
pattern XK_AccessX_Feedback_Enable = MkKeySymbol #const XK_AccessX_Feedback_Enable
-- #define XK_RepeatKeys_Enable             0xfe72
pattern XK_RepeatKeys_Enable = MkKeySymbol #const XK_RepeatKeys_Enable
-- #define XK_SlowKeys_Enable               0xfe73
pattern XK_SlowKeys_Enable = MkKeySymbol #const XK_SlowKeys_Enable
-- #define XK_BounceKeys_Enable             0xfe74
pattern XK_BounceKeys_Enable = MkKeySymbol #const XK_BounceKeys_Enable
-- #define XK_StickyKeys_Enable             0xfe75
pattern XK_StickyKeys_Enable = MkKeySymbol #const XK_StickyKeys_Enable
-- #define XK_MouseKeys_Enable              0xfe76
pattern XK_MouseKeys_Enable = MkKeySymbol #const XK_MouseKeys_Enable
-- #define XK_MouseKeys_Accel_Enable        0xfe77
pattern XK_MouseKeys_Accel_Enable = MkKeySymbol #const XK_MouseKeys_Accel_Enable
-- #define XK_Overlay1_Enable               0xfe78
pattern XK_Overlay1_Enable = MkKeySymbol #const XK_Overlay1_Enable
-- #define XK_Overlay2_Enable               0xfe79
pattern XK_Overlay2_Enable = MkKeySymbol #const XK_Overlay2_Enable
-- #define XK_AudibleBell_Enable            0xfe7a
pattern XK_AudibleBell_Enable = MkKeySymbol #const XK_AudibleBell_Enable
-- #define XK_Pointer_Left                  0xfee0
pattern XK_Pointer_Left = MkKeySymbol #const XK_Pointer_Left
-- #define XK_Pointer_Right                 0xfee1
pattern XK_Pointer_Right = MkKeySymbol #const XK_Pointer_Right
-- #define XK_Pointer_Up                    0xfee2
pattern XK_Pointer_Up = MkKeySymbol #const XK_Pointer_Up
-- #define XK_Pointer_Down                  0xfee3
pattern XK_Pointer_Down = MkKeySymbol #const XK_Pointer_Down
-- #define XK_Pointer_UpLeft                0xfee4
pattern XK_Pointer_UpLeft = MkKeySymbol #const XK_Pointer_UpLeft
-- #define XK_Pointer_UpRight               0xfee5
pattern XK_Pointer_UpRight = MkKeySymbol #const XK_Pointer_UpRight
-- #define XK_Pointer_DownLeft              0xfee6
pattern XK_Pointer_DownLeft = MkKeySymbol #const XK_Pointer_DownLeft
-- #define XK_Pointer_DownRight             0xfee7
pattern XK_Pointer_DownRight = MkKeySymbol #const XK_Pointer_DownRight
-- #define XK_Pointer_Button_Dflt           0xfee8
pattern XK_Pointer_Button_Dflt = MkKeySymbol #const XK_Pointer_Button_Dflt
-- #define XK_Pointer_Button1               0xfee9
pattern XK_Pointer_Button1 = MkKeySymbol #const XK_Pointer_Button1
-- #define XK_Pointer_Button2               0xfeea
pattern XK_Pointer_Button2 = MkKeySymbol #const XK_Pointer_Button2
-- #define XK_Pointer_Button3               0xfeeb
pattern XK_Pointer_Button3 = MkKeySymbol #const XK_Pointer_Button3
-- #define XK_Pointer_Button4               0xfeec
pattern XK_Pointer_Button4 = MkKeySymbol #const XK_Pointer_Button4
-- #define XK_Pointer_Button5               0xfeed
pattern XK_Pointer_Button5 = MkKeySymbol #const XK_Pointer_Button5
-- #define XK_Pointer_DblClick_Dflt         0xfeee
pattern XK_Pointer_DblClick_Dflt = MkKeySymbol #const XK_Pointer_DblClick_Dflt
-- #define XK_Pointer_DblClick1             0xfeef
pattern XK_Pointer_DblClick1 = MkKeySymbol #const XK_Pointer_DblClick1
-- #define XK_Pointer_DblClick2             0xfef0
pattern XK_Pointer_DblClick2 = MkKeySymbol #const XK_Pointer_DblClick2
-- #define XK_Pointer_DblClick3             0xfef1
pattern XK_Pointer_DblClick3 = MkKeySymbol #const XK_Pointer_DblClick3
-- #define XK_Pointer_DblClick4             0xfef2
pattern XK_Pointer_DblClick4 = MkKeySymbol #const XK_Pointer_DblClick4
-- #define XK_Pointer_DblClick5             0xfef3
pattern XK_Pointer_DblClick5 = MkKeySymbol #const XK_Pointer_DblClick5
-- #define XK_Pointer_Drag_Dflt             0xfef4
pattern XK_Pointer_Drag_Dflt = MkKeySymbol #const XK_Pointer_Drag_Dflt
-- #define XK_Pointer_Drag1                 0xfef5
pattern XK_Pointer_Drag1 = MkKeySymbol #const XK_Pointer_Drag1
-- #define XK_Pointer_Drag2                 0xfef6
pattern XK_Pointer_Drag2 = MkKeySymbol #const XK_Pointer_Drag2
-- #define XK_Pointer_Drag3                 0xfef7
pattern XK_Pointer_Drag3 = MkKeySymbol #const XK_Pointer_Drag3
-- #define XK_Pointer_Drag4                 0xfef8
pattern XK_Pointer_Drag4 = MkKeySymbol #const XK_Pointer_Drag4
-- #define XK_Pointer_Drag5                 0xfefd
pattern XK_Pointer_Drag5 = MkKeySymbol #const XK_Pointer_Drag5
-- #define XK_Pointer_EnableKeys            0xfef9
pattern XK_Pointer_EnableKeys = MkKeySymbol #const XK_Pointer_EnableKeys
-- #define XK_Pointer_Accelerate            0xfefa
pattern XK_Pointer_Accelerate = MkKeySymbol #const XK_Pointer_Accelerate
-- #define XK_Pointer_DfltBtnNext           0xfefb
pattern XK_Pointer_DfltBtnNext = MkKeySymbol #const XK_Pointer_DfltBtnNext
-- #define XK_Pointer_DfltBtnPrev           0xfefc
pattern XK_Pointer_DfltBtnPrev = MkKeySymbol #const XK_Pointer_DfltBtnPrev
-- #define XK_ch                            0xfea0
pattern XK_ch = MkKeySymbol #const XK_ch
-- #define XK_Ch                            0xfea1
pattern XK_Ch = MkKeySymbol #const XK_Ch
-- #define XK_CH                            0xfea2
pattern XK_CH = MkKeySymbol #const XK_CH
-- #define XK_c_h                           0xfea3
pattern XK_c_h = MkKeySymbol #const XK_c_h
-- #define XK_C_h                           0xfea4
pattern XK_C_h = MkKeySymbol #const XK_C_h
-- #define XK_C_H                           0xfea5
pattern XK_C_H = MkKeySymbol #const XK_C_H
-- #define XK_3270_Duplicate                0xfd01
pattern XK_3270_Duplicate = MkKeySymbol #const XK_3270_Duplicate
-- #define XK_3270_FieldMark                0xfd02
pattern XK_3270_FieldMark = MkKeySymbol #const XK_3270_FieldMark
-- #define XK_3270_Right2                   0xfd03
pattern XK_3270_Right2 = MkKeySymbol #const XK_3270_Right2
-- #define XK_3270_Left2                    0xfd04
pattern XK_3270_Left2 = MkKeySymbol #const XK_3270_Left2
-- #define XK_3270_BackTab                  0xfd05
pattern XK_3270_BackTab = MkKeySymbol #const XK_3270_BackTab
-- #define XK_3270_EraseEOF                 0xfd06
pattern XK_3270_EraseEOF = MkKeySymbol #const XK_3270_EraseEOF
-- #define XK_3270_EraseInput               0xfd07
pattern XK_3270_EraseInput = MkKeySymbol #const XK_3270_EraseInput
-- #define XK_3270_Reset                    0xfd08
pattern XK_3270_Reset = MkKeySymbol #const XK_3270_Reset
-- #define XK_3270_Quit                     0xfd09
pattern XK_3270_Quit = MkKeySymbol #const XK_3270_Quit
-- #define XK_3270_PA1                      0xfd0a
pattern XK_3270_PA1 = MkKeySymbol #const XK_3270_PA1
-- #define XK_3270_PA2                      0xfd0b
pattern XK_3270_PA2 = MkKeySymbol #const XK_3270_PA2
-- #define XK_3270_PA3                      0xfd0c
pattern XK_3270_PA3 = MkKeySymbol #const XK_3270_PA3
-- #define XK_3270_Test                     0xfd0d
pattern XK_3270_Test = MkKeySymbol #const XK_3270_Test
-- #define XK_3270_Attn                     0xfd0e
pattern XK_3270_Attn = MkKeySymbol #const XK_3270_Attn
-- #define XK_3270_CursorBlink              0xfd0f
pattern XK_3270_CursorBlink = MkKeySymbol #const XK_3270_CursorBlink
-- #define XK_3270_AltCursor                0xfd10
pattern XK_3270_AltCursor = MkKeySymbol #const XK_3270_AltCursor
-- #define XK_3270_KeyClick                 0xfd11
pattern XK_3270_KeyClick = MkKeySymbol #const XK_3270_KeyClick
-- #define XK_3270_Jump                     0xfd12
pattern XK_3270_Jump = MkKeySymbol #const XK_3270_Jump
-- #define XK_3270_Ident                    0xfd13
pattern XK_3270_Ident = MkKeySymbol #const XK_3270_Ident
-- #define XK_3270_Rule                     0xfd14
pattern XK_3270_Rule = MkKeySymbol #const XK_3270_Rule
-- #define XK_3270_Copy                     0xfd15
pattern XK_3270_Copy = MkKeySymbol #const XK_3270_Copy
-- #define XK_3270_Play                     0xfd16
pattern XK_3270_Play = MkKeySymbol #const XK_3270_Play
-- #define XK_3270_Setup                    0xfd17
pattern XK_3270_Setup = MkKeySymbol #const XK_3270_Setup
-- #define XK_3270_Record                   0xfd18
pattern XK_3270_Record = MkKeySymbol #const XK_3270_Record
-- #define XK_3270_ChangeScreen             0xfd19
pattern XK_3270_ChangeScreen = MkKeySymbol #const XK_3270_ChangeScreen
-- #define XK_3270_DeleteWord               0xfd1a
pattern XK_3270_DeleteWord = MkKeySymbol #const XK_3270_DeleteWord
-- #define XK_3270_ExSelect                 0xfd1b
pattern XK_3270_ExSelect = MkKeySymbol #const XK_3270_ExSelect
-- #define XK_3270_CursorSelect             0xfd1c
pattern XK_3270_CursorSelect = MkKeySymbol #const XK_3270_CursorSelect
-- #define XK_3270_PrintScreen              0xfd1d
pattern XK_3270_PrintScreen = MkKeySymbol #const XK_3270_PrintScreen
-- #define XK_3270_Enter                    0xfd1e
pattern XK_3270_Enter = MkKeySymbol #const XK_3270_Enter
-- #define XK_space                         0x0020  /* U+0020 SPACE */
pattern XK_space = MkKeySymbol #const XK_space
-- #define XK_exclam                        0x0021  /* U+0021 EXCLAMATION MARK */
pattern XK_exclam = MkKeySymbol #const XK_exclam
-- #define XK_quotedbl                      0x0022  /* U+0022 QUOTATION MARK */
pattern XK_quotedbl = MkKeySymbol #const XK_quotedbl
-- #define XK_numbersign                    0x0023  /* U+0023 NUMBER SIGN */
pattern XK_numbersign = MkKeySymbol #const XK_numbersign
-- #define XK_dollar                        0x0024  /* U+0024 DOLLAR SIGN */
pattern XK_dollar = MkKeySymbol #const XK_dollar
-- #define XK_percent                       0x0025  /* U+0025 PERCENT SIGN */
pattern XK_percent = MkKeySymbol #const XK_percent
-- #define XK_ampersand                     0x0026  /* U+0026 AMPERSAND */
pattern XK_ampersand = MkKeySymbol #const XK_ampersand
-- #define XK_apostrophe                    0x0027  /* U+0027 APOSTROPHE */
pattern XK_apostrophe = MkKeySymbol #const XK_apostrophe
-- #define XK_quoteright                    0x0027  /* deprecated */
pattern XK_quoteright = MkKeySymbol #const XK_quoteright
-- #define XK_parenleft                     0x0028  /* U+0028 LEFT PARENTHESIS */
pattern XK_parenleft = MkKeySymbol #const XK_parenleft
-- #define XK_parenright                    0x0029  /* U+0029 RIGHT PARENTHESIS */
pattern XK_parenright = MkKeySymbol #const XK_parenright
-- #define XK_asterisk                      0x002a  /* U+002A ASTERISK */
pattern XK_asterisk = MkKeySymbol #const XK_asterisk
-- #define XK_plus                          0x002b  /* U+002B PLUS SIGN */
pattern XK_plus = MkKeySymbol #const XK_plus
-- #define XK_comma                         0x002c  /* U+002C COMMA */
pattern XK_comma = MkKeySymbol #const XK_comma
-- #define XK_minus                         0x002d  /* U+002D HYPHEN-MINUS */
pattern XK_minus = MkKeySymbol #const XK_minus
-- #define XK_period                        0x002e  /* U+002E FULL STOP */
pattern XK_period = MkKeySymbol #const XK_period
-- #define XK_slash                         0x002f  /* U+002F SOLIDUS */
pattern XK_slash = MkKeySymbol #const XK_slash
-- #define XK_0                             0x0030  /* U+0030 DIGIT ZERO */
pattern XK_0 = MkKeySymbol #const XK_0
-- #define XK_1                             0x0031  /* U+0031 DIGIT ONE */
pattern XK_1 = MkKeySymbol #const XK_1
-- #define XK_2                             0x0032  /* U+0032 DIGIT TWO */
pattern XK_2 = MkKeySymbol #const XK_2
-- #define XK_3                             0x0033  /* U+0033 DIGIT THREE */
pattern XK_3 = MkKeySymbol #const XK_3
-- #define XK_4                             0x0034  /* U+0034 DIGIT FOUR */
pattern XK_4 = MkKeySymbol #const XK_4
-- #define XK_5                             0x0035  /* U+0035 DIGIT FIVE */
pattern XK_5 = MkKeySymbol #const XK_5
-- #define XK_6                             0x0036  /* U+0036 DIGIT SIX */
pattern XK_6 = MkKeySymbol #const XK_6
-- #define XK_7                             0x0037  /* U+0037 DIGIT SEVEN */
pattern XK_7 = MkKeySymbol #const XK_7
-- #define XK_8                             0x0038  /* U+0038 DIGIT EIGHT */
pattern XK_8 = MkKeySymbol #const XK_8
-- #define XK_9                             0x0039  /* U+0039 DIGIT NINE */
pattern XK_9 = MkKeySymbol #const XK_9
-- #define XK_colon                         0x003a  /* U+003A COLON */
pattern XK_colon = MkKeySymbol #const XK_colon
-- #define XK_semicolon                     0x003b  /* U+003B SEMICOLON */
pattern XK_semicolon = MkKeySymbol #const XK_semicolon
-- #define XK_less                          0x003c  /* U+003C LESS-THAN SIGN */
pattern XK_less = MkKeySymbol #const XK_less
-- #define XK_equal                         0x003d  /* U+003D EQUALS SIGN */
pattern XK_equal = MkKeySymbol #const XK_equal
-- #define XK_greater                       0x003e  /* U+003E GREATER-THAN SIGN */
pattern XK_greater = MkKeySymbol #const XK_greater
-- #define XK_question                      0x003f  /* U+003F QUESTION MARK */
pattern XK_question = MkKeySymbol #const XK_question
-- #define XK_at                            0x0040  /* U+0040 COMMERCIAL AT */
pattern XK_at = MkKeySymbol #const XK_at
-- #define XK_A                             0x0041  /* U+0041 LATIN CAPITAL LETTER A */
pattern XK_A = MkKeySymbol #const XK_A
-- #define XK_B                             0x0042  /* U+0042 LATIN CAPITAL LETTER B */
pattern XK_B = MkKeySymbol #const XK_B
-- #define XK_C                             0x0043  /* U+0043 LATIN CAPITAL LETTER C */
pattern XK_C = MkKeySymbol #const XK_C
-- #define XK_D                             0x0044  /* U+0044 LATIN CAPITAL LETTER D */
pattern XK_D = MkKeySymbol #const XK_D
-- #define XK_E                             0x0045  /* U+0045 LATIN CAPITAL LETTER E */
pattern XK_E = MkKeySymbol #const XK_E
-- #define XK_F                             0x0046  /* U+0046 LATIN CAPITAL LETTER F */
pattern XK_F = MkKeySymbol #const XK_F
-- #define XK_G                             0x0047  /* U+0047 LATIN CAPITAL LETTER G */
pattern XK_G = MkKeySymbol #const XK_G
-- #define XK_H                             0x0048  /* U+0048 LATIN CAPITAL LETTER H */
pattern XK_H = MkKeySymbol #const XK_H
-- #define XK_I                             0x0049  /* U+0049 LATIN CAPITAL LETTER I */
pattern XK_I = MkKeySymbol #const XK_I
-- #define XK_J                             0x004a  /* U+004A LATIN CAPITAL LETTER J */
pattern XK_J = MkKeySymbol #const XK_J
-- #define XK_K                             0x004b  /* U+004B LATIN CAPITAL LETTER K */
pattern XK_K = MkKeySymbol #const XK_K
-- #define XK_L                             0x004c  /* U+004C LATIN CAPITAL LETTER L */
pattern XK_L = MkKeySymbol #const XK_L
-- #define XK_M                             0x004d  /* U+004D LATIN CAPITAL LETTER M */
pattern XK_M = MkKeySymbol #const XK_M
-- #define XK_N                             0x004e  /* U+004E LATIN CAPITAL LETTER N */
pattern XK_N = MkKeySymbol #const XK_N
-- #define XK_O                             0x004f  /* U+004F LATIN CAPITAL LETTER O */
pattern XK_O = MkKeySymbol #const XK_O
-- #define XK_P                             0x0050  /* U+0050 LATIN CAPITAL LETTER P */
pattern XK_P = MkKeySymbol #const XK_P
-- #define XK_Q                             0x0051  /* U+0051 LATIN CAPITAL LETTER Q */
pattern XK_Q = MkKeySymbol #const XK_Q
-- #define XK_R                             0x0052  /* U+0052 LATIN CAPITAL LETTER R */
pattern XK_R = MkKeySymbol #const XK_R
-- #define XK_S                             0x0053  /* U+0053 LATIN CAPITAL LETTER S */
pattern XK_S = MkKeySymbol #const XK_S
-- #define XK_T                             0x0054  /* U+0054 LATIN CAPITAL LETTER T */
pattern XK_T = MkKeySymbol #const XK_T
-- #define XK_U                             0x0055  /* U+0055 LATIN CAPITAL LETTER U */
pattern XK_U = MkKeySymbol #const XK_U
-- #define XK_V                             0x0056  /* U+0056 LATIN CAPITAL LETTER V */
pattern XK_V = MkKeySymbol #const XK_V
-- #define XK_W                             0x0057  /* U+0057 LATIN CAPITAL LETTER W */
pattern XK_W = MkKeySymbol #const XK_W
-- #define XK_X                             0x0058  /* U+0058 LATIN CAPITAL LETTER X */
pattern XK_X = MkKeySymbol #const XK_X
-- #define XK_Y                             0x0059  /* U+0059 LATIN CAPITAL LETTER Y */
pattern XK_Y = MkKeySymbol #const XK_Y
-- #define XK_Z                             0x005a  /* U+005A LATIN CAPITAL LETTER Z */
pattern XK_Z = MkKeySymbol #const XK_Z
-- #define XK_bracketleft                   0x005b  /* U+005B LEFT SQUARE BRACKET */
pattern XK_bracketleft = MkKeySymbol #const XK_bracketleft
-- #define XK_backslash                     0x005c  /* U+005C REVERSE SOLIDUS */
pattern XK_backslash = MkKeySymbol #const XK_backslash
-- #define XK_bracketright                  0x005d  /* U+005D RIGHT SQUARE BRACKET */
pattern XK_bracketright = MkKeySymbol #const XK_bracketright
-- #define XK_asciicircum                   0x005e  /* U+005E CIRCUMFLEX ACCENT */
pattern XK_asciicircum = MkKeySymbol #const XK_asciicircum
-- #define XK_underscore                    0x005f  /* U+005F LOW LINE */
pattern XK_underscore = MkKeySymbol #const XK_underscore
-- #define XK_grave                         0x0060  /* U+0060 GRAVE ACCENT */
pattern XK_grave = MkKeySymbol #const XK_grave
-- #define XK_quoteleft                     0x0060  /* deprecated */
pattern XK_quoteleft = MkKeySymbol #const XK_quoteleft
-- #define XK_a                             0x0061  /* U+0061 LATIN SMALL LETTER A */
pattern XK_a = MkKeySymbol #const XK_a
-- #define XK_b                             0x0062  /* U+0062 LATIN SMALL LETTER B */
pattern XK_b = MkKeySymbol #const XK_b
-- #define XK_c                             0x0063  /* U+0063 LATIN SMALL LETTER C */
pattern XK_c = MkKeySymbol #const XK_c
-- #define XK_d                             0x0064  /* U+0064 LATIN SMALL LETTER D */
pattern XK_d = MkKeySymbol #const XK_d
-- #define XK_e                             0x0065  /* U+0065 LATIN SMALL LETTER E */
pattern XK_e = MkKeySymbol #const XK_e
-- #define XK_f                             0x0066  /* U+0066 LATIN SMALL LETTER F */
pattern XK_f = MkKeySymbol #const XK_f
-- #define XK_g                             0x0067  /* U+0067 LATIN SMALL LETTER G */
pattern XK_g = MkKeySymbol #const XK_g
-- #define XK_h                             0x0068  /* U+0068 LATIN SMALL LETTER H */
pattern XK_h = MkKeySymbol #const XK_h
-- #define XK_i                             0x0069  /* U+0069 LATIN SMALL LETTER I */
pattern XK_i = MkKeySymbol #const XK_i
-- #define XK_j                             0x006a  /* U+006A LATIN SMALL LETTER J */
pattern XK_j = MkKeySymbol #const XK_j
-- #define XK_k                             0x006b  /* U+006B LATIN SMALL LETTER K */
pattern XK_k = MkKeySymbol #const XK_k
-- #define XK_l                             0x006c  /* U+006C LATIN SMALL LETTER L */
pattern XK_l = MkKeySymbol #const XK_l
-- #define XK_m                             0x006d  /* U+006D LATIN SMALL LETTER M */
pattern XK_m = MkKeySymbol #const XK_m
-- #define XK_n                             0x006e  /* U+006E LATIN SMALL LETTER N */
pattern XK_n = MkKeySymbol #const XK_n
-- #define XK_o                             0x006f  /* U+006F LATIN SMALL LETTER O */
pattern XK_o = MkKeySymbol #const XK_o
-- #define XK_p                             0x0070  /* U+0070 LATIN SMALL LETTER P */
pattern XK_p = MkKeySymbol #const XK_p
-- #define XK_q                             0x0071  /* U+0071 LATIN SMALL LETTER Q */
pattern XK_q = MkKeySymbol #const XK_q
-- #define XK_r                             0x0072  /* U+0072 LATIN SMALL LETTER R */
pattern XK_r = MkKeySymbol #const XK_r
-- #define XK_s                             0x0073  /* U+0073 LATIN SMALL LETTER S */
pattern XK_s = MkKeySymbol #const XK_s
-- #define XK_t                             0x0074  /* U+0074 LATIN SMALL LETTER T */
pattern XK_t = MkKeySymbol #const XK_t
-- #define XK_u                             0x0075  /* U+0075 LATIN SMALL LETTER U */
pattern XK_u = MkKeySymbol #const XK_u
-- #define XK_v                             0x0076  /* U+0076 LATIN SMALL LETTER V */
pattern XK_v = MkKeySymbol #const XK_v
-- #define XK_w                             0x0077  /* U+0077 LATIN SMALL LETTER W */
pattern XK_w = MkKeySymbol #const XK_w
-- #define XK_x                             0x0078  /* U+0078 LATIN SMALL LETTER X */
pattern XK_x = MkKeySymbol #const XK_x
-- #define XK_y                             0x0079  /* U+0079 LATIN SMALL LETTER Y */
pattern XK_y = MkKeySymbol #const XK_y
-- #define XK_z                             0x007a  /* U+007A LATIN SMALL LETTER Z */
pattern XK_z = MkKeySymbol #const XK_z
-- #define XK_braceleft                     0x007b  /* U+007B LEFT CURLY BRACKET */
pattern XK_braceleft = MkKeySymbol #const XK_braceleft
-- #define XK_bar                           0x007c  /* U+007C VERTICAL LINE */
pattern XK_bar = MkKeySymbol #const XK_bar
-- #define XK_braceright                    0x007d  /* U+007D RIGHT CURLY BRACKET */
pattern XK_braceright = MkKeySymbol #const XK_braceright
-- #define XK_asciitilde                    0x007e  /* U+007E TILDE */
pattern XK_asciitilde = MkKeySymbol #const XK_asciitilde
-- #define XK_nobreakspace                  0x00a0  /* U+00A0 NO-BREAK SPACE */
pattern XK_nobreakspace = MkKeySymbol #const XK_nobreakspace
-- #define XK_exclamdown                    0x00a1  /* U+00A1 INVERTED EXCLAMATION MARK */
pattern XK_exclamdown = MkKeySymbol #const XK_exclamdown
-- #define XK_cent                          0x00a2  /* U+00A2 CENT SIGN */
pattern XK_cent = MkKeySymbol #const XK_cent
-- #define XK_sterling                      0x00a3  /* U+00A3 POUND SIGN */
pattern XK_sterling = MkKeySymbol #const XK_sterling
-- #define XK_currency                      0x00a4  /* U+00A4 CURRENCY SIGN */
pattern XK_currency = MkKeySymbol #const XK_currency
-- #define XK_yen                           0x00a5  /* U+00A5 YEN SIGN */
pattern XK_yen = MkKeySymbol #const XK_yen
-- #define XK_brokenbar                     0x00a6  /* U+00A6 BROKEN BAR */
pattern XK_brokenbar = MkKeySymbol #const XK_brokenbar
-- #define XK_section                       0x00a7  /* U+00A7 SECTION SIGN */
pattern XK_section = MkKeySymbol #const XK_section
-- #define XK_diaeresis                     0x00a8  /* U+00A8 DIAERESIS */
pattern XK_diaeresis = MkKeySymbol #const XK_diaeresis
-- #define XK_copyright                     0x00a9  /* U+00A9 COPYRIGHT SIGN */
pattern XK_copyright = MkKeySymbol #const XK_copyright
-- #define XK_ordfeminine                   0x00aa  /* U+00AA FEMININE ORDINAL INDICATOR */
pattern XK_ordfeminine = MkKeySymbol #const XK_ordfeminine
-- #define XK_guillemotleft                 0x00ab  /* U+00AB LEFT-POINTING DOUBLE ANGLE QUOTATION MARK */
pattern XK_guillemotleft = MkKeySymbol #const XK_guillemotleft
-- #define XK_notsign                       0x00ac  /* U+00AC NOT SIGN */
pattern XK_notsign = MkKeySymbol #const XK_notsign
-- #define XK_hyphen                        0x00ad  /* U+00AD SOFT HYPHEN */
pattern XK_hyphen = MkKeySymbol #const XK_hyphen
-- #define XK_registered                    0x00ae  /* U+00AE REGISTERED SIGN */
pattern XK_registered = MkKeySymbol #const XK_registered
-- #define XK_macron                        0x00af  /* U+00AF MACRON */
pattern XK_macron = MkKeySymbol #const XK_macron
-- #define XK_degree                        0x00b0  /* U+00B0 DEGREE SIGN */
pattern XK_degree = MkKeySymbol #const XK_degree
-- #define XK_plusminus                     0x00b1  /* U+00B1 PLUS-MINUS SIGN */
pattern XK_plusminus = MkKeySymbol #const XK_plusminus
-- #define XK_twosuperior                   0x00b2  /* U+00B2 SUPERSCRIPT TWO */
pattern XK_twosuperior = MkKeySymbol #const XK_twosuperior
-- #define XK_threesuperior                 0x00b3  /* U+00B3 SUPERSCRIPT THREE */
pattern XK_threesuperior = MkKeySymbol #const XK_threesuperior
-- #define XK_acute                         0x00b4  /* U+00B4 ACUTE ACCENT */
pattern XK_acute = MkKeySymbol #const XK_acute
-- #define XK_mu                            0x00b5  /* U+00B5 MICRO SIGN */
pattern XK_mu = MkKeySymbol #const XK_mu
-- #define XK_paragraph                     0x00b6  /* U+00B6 PILCROW SIGN */
pattern XK_paragraph = MkKeySymbol #const XK_paragraph
-- #define XK_periodcentered                0x00b7  /* U+00B7 MIDDLE DOT */
pattern XK_periodcentered = MkKeySymbol #const XK_periodcentered
-- #define XK_cedilla                       0x00b8  /* U+00B8 CEDILLA */
pattern XK_cedilla = MkKeySymbol #const XK_cedilla
-- #define XK_onesuperior                   0x00b9  /* U+00B9 SUPERSCRIPT ONE */
pattern XK_onesuperior = MkKeySymbol #const XK_onesuperior
-- #define XK_masculine                     0x00ba  /* U+00BA MASCULINE ORDINAL INDICATOR */
pattern XK_masculine = MkKeySymbol #const XK_masculine
-- #define XK_guillemotright                0x00bb  /* U+00BB RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK */
pattern XK_guillemotright = MkKeySymbol #const XK_guillemotright
-- #define XK_onequarter                    0x00bc  /* U+00BC VULGAR FRACTION ONE QUARTER */
pattern XK_onequarter = MkKeySymbol #const XK_onequarter
-- #define XK_onehalf                       0x00bd  /* U+00BD VULGAR FRACTION ONE HALF */
pattern XK_onehalf = MkKeySymbol #const XK_onehalf
-- #define XK_threequarters                 0x00be  /* U+00BE VULGAR FRACTION THREE QUARTERS */
pattern XK_threequarters = MkKeySymbol #const XK_threequarters
-- #define XK_questiondown                  0x00bf  /* U+00BF INVERTED QUESTION MARK */
pattern XK_questiondown = MkKeySymbol #const XK_questiondown
-- #define XK_Agrave                        0x00c0  /* U+00C0 LATIN CAPITAL LETTER A WITH GRAVE */
pattern XK_Agrave = MkKeySymbol #const XK_Agrave
-- #define XK_Aacute                        0x00c1  /* U+00C1 LATIN CAPITAL LETTER A WITH ACUTE */
pattern XK_Aacute = MkKeySymbol #const XK_Aacute
-- #define XK_Acircumflex                   0x00c2  /* U+00C2 LATIN CAPITAL LETTER A WITH CIRCUMFLEX */
pattern XK_Acircumflex = MkKeySymbol #const XK_Acircumflex
-- #define XK_Atilde                        0x00c3  /* U+00C3 LATIN CAPITAL LETTER A WITH TILDE */
pattern XK_Atilde = MkKeySymbol #const XK_Atilde
-- #define XK_Adiaeresis                    0x00c4  /* U+00C4 LATIN CAPITAL LETTER A WITH DIAERESIS */
pattern XK_Adiaeresis = MkKeySymbol #const XK_Adiaeresis
-- #define XK_Aring                         0x00c5  /* U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE */
pattern XK_Aring = MkKeySymbol #const XK_Aring
-- #define XK_AE                            0x00c6  /* U+00C6 LATIN CAPITAL LETTER AE */
pattern XK_AE = MkKeySymbol #const XK_AE
-- #define XK_Ccedilla                      0x00c7  /* U+00C7 LATIN CAPITAL LETTER C WITH CEDILLA */
pattern XK_Ccedilla = MkKeySymbol #const XK_Ccedilla
-- #define XK_Egrave                        0x00c8  /* U+00C8 LATIN CAPITAL LETTER E WITH GRAVE */
pattern XK_Egrave = MkKeySymbol #const XK_Egrave
-- #define XK_Eacute                        0x00c9  /* U+00C9 LATIN CAPITAL LETTER E WITH ACUTE */
pattern XK_Eacute = MkKeySymbol #const XK_Eacute
-- #define XK_Ecircumflex                   0x00ca  /* U+00CA LATIN CAPITAL LETTER E WITH CIRCUMFLEX */
pattern XK_Ecircumflex = MkKeySymbol #const XK_Ecircumflex
-- #define XK_Ediaeresis                    0x00cb  /* U+00CB LATIN CAPITAL LETTER E WITH DIAERESIS */
pattern XK_Ediaeresis = MkKeySymbol #const XK_Ediaeresis
-- #define XK_Igrave                        0x00cc  /* U+00CC LATIN CAPITAL LETTER I WITH GRAVE */
pattern XK_Igrave = MkKeySymbol #const XK_Igrave
-- #define XK_Iacute                        0x00cd  /* U+00CD LATIN CAPITAL LETTER I WITH ACUTE */
pattern XK_Iacute = MkKeySymbol #const XK_Iacute
-- #define XK_Icircumflex                   0x00ce  /* U+00CE LATIN CAPITAL LETTER I WITH CIRCUMFLEX */
pattern XK_Icircumflex = MkKeySymbol #const XK_Icircumflex
-- #define XK_Idiaeresis                    0x00cf  /* U+00CF LATIN CAPITAL LETTER I WITH DIAERESIS */
pattern XK_Idiaeresis = MkKeySymbol #const XK_Idiaeresis
-- #define XK_ETH                           0x00d0  /* U+00D0 LATIN CAPITAL LETTER ETH */
pattern XK_ETH = MkKeySymbol #const XK_ETH
-- #define XK_Eth                           0x00d0  /* deprecated */
pattern XK_Eth = MkKeySymbol #const XK_Eth
-- #define XK_Ntilde                        0x00d1  /* U+00D1 LATIN CAPITAL LETTER N WITH TILDE */
pattern XK_Ntilde = MkKeySymbol #const XK_Ntilde
-- #define XK_Ograve                        0x00d2  /* U+00D2 LATIN CAPITAL LETTER O WITH GRAVE */
pattern XK_Ograve = MkKeySymbol #const XK_Ograve
-- #define XK_Oacute                        0x00d3  /* U+00D3 LATIN CAPITAL LETTER O WITH ACUTE */
pattern XK_Oacute = MkKeySymbol #const XK_Oacute
-- #define XK_Ocircumflex                   0x00d4  /* U+00D4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX */
pattern XK_Ocircumflex = MkKeySymbol #const XK_Ocircumflex
-- #define XK_Otilde                        0x00d5  /* U+00D5 LATIN CAPITAL LETTER O WITH TILDE */
pattern XK_Otilde = MkKeySymbol #const XK_Otilde
-- #define XK_Odiaeresis                    0x00d6  /* U+00D6 LATIN CAPITAL LETTER O WITH DIAERESIS */
pattern XK_Odiaeresis = MkKeySymbol #const XK_Odiaeresis
-- #define XK_multiply                      0x00d7  /* U+00D7 MULTIPLICATION SIGN */
pattern XK_multiply = MkKeySymbol #const XK_multiply
-- #define XK_Oslash                        0x00d8  /* U+00D8 LATIN CAPITAL LETTER O WITH STROKE */
pattern XK_Oslash = MkKeySymbol #const XK_Oslash
-- #define XK_Ooblique                      0x00d8  /* U+00D8 LATIN CAPITAL LETTER O WITH STROKE */
pattern XK_Ooblique = MkKeySymbol #const XK_Ooblique
-- #define XK_Ugrave                        0x00d9  /* U+00D9 LATIN CAPITAL LETTER U WITH GRAVE */
pattern XK_Ugrave = MkKeySymbol #const XK_Ugrave
-- #define XK_Uacute                        0x00da  /* U+00DA LATIN CAPITAL LETTER U WITH ACUTE */
pattern XK_Uacute = MkKeySymbol #const XK_Uacute
-- #define XK_Ucircumflex                   0x00db  /* U+00DB LATIN CAPITAL LETTER U WITH CIRCUMFLEX */
pattern XK_Ucircumflex = MkKeySymbol #const XK_Ucircumflex
-- #define XK_Udiaeresis                    0x00dc  /* U+00DC LATIN CAPITAL LETTER U WITH DIAERESIS */
pattern XK_Udiaeresis = MkKeySymbol #const XK_Udiaeresis
-- #define XK_Yacute                        0x00dd  /* U+00DD LATIN CAPITAL LETTER Y WITH ACUTE */
pattern XK_Yacute = MkKeySymbol #const XK_Yacute
-- #define XK_THORN                         0x00de  /* U+00DE LATIN CAPITAL LETTER THORN */
pattern XK_THORN = MkKeySymbol #const XK_THORN
-- #define XK_Thorn                         0x00de  /* deprecated */
pattern XK_Thorn = MkKeySymbol #const XK_Thorn
-- #define XK_ssharp                        0x00df  /* U+00DF LATIN SMALL LETTER SHARP S */
pattern XK_ssharp = MkKeySymbol #const XK_ssharp
-- #define XK_agrave                        0x00e0  /* U+00E0 LATIN SMALL LETTER A WITH GRAVE */
pattern XK_agrave = MkKeySymbol #const XK_agrave
-- #define XK_aacute                        0x00e1  /* U+00E1 LATIN SMALL LETTER A WITH ACUTE */
pattern XK_aacute = MkKeySymbol #const XK_aacute
-- #define XK_acircumflex                   0x00e2  /* U+00E2 LATIN SMALL LETTER A WITH CIRCUMFLEX */
pattern XK_acircumflex = MkKeySymbol #const XK_acircumflex
-- #define XK_atilde                        0x00e3  /* U+00E3 LATIN SMALL LETTER A WITH TILDE */
pattern XK_atilde = MkKeySymbol #const XK_atilde
-- #define XK_adiaeresis                    0x00e4  /* U+00E4 LATIN SMALL LETTER A WITH DIAERESIS */
pattern XK_adiaeresis = MkKeySymbol #const XK_adiaeresis
-- #define XK_aring                         0x00e5  /* U+00E5 LATIN SMALL LETTER A WITH RING ABOVE */
pattern XK_aring = MkKeySymbol #const XK_aring
-- #define XK_ae                            0x00e6  /* U+00E6 LATIN SMALL LETTER AE */
pattern XK_ae = MkKeySymbol #const XK_ae
-- #define XK_ccedilla                      0x00e7  /* U+00E7 LATIN SMALL LETTER C WITH CEDILLA */
pattern XK_ccedilla = MkKeySymbol #const XK_ccedilla
-- #define XK_egrave                        0x00e8  /* U+00E8 LATIN SMALL LETTER E WITH GRAVE */
pattern XK_egrave = MkKeySymbol #const XK_egrave
-- #define XK_eacute                        0x00e9  /* U+00E9 LATIN SMALL LETTER E WITH ACUTE */
pattern XK_eacute = MkKeySymbol #const XK_eacute
-- #define XK_ecircumflex                   0x00ea  /* U+00EA LATIN SMALL LETTER E WITH CIRCUMFLEX */
pattern XK_ecircumflex = MkKeySymbol #const XK_ecircumflex
-- #define XK_ediaeresis                    0x00eb  /* U+00EB LATIN SMALL LETTER E WITH DIAERESIS */
pattern XK_ediaeresis = MkKeySymbol #const XK_ediaeresis
-- #define XK_igrave                        0x00ec  /* U+00EC LATIN SMALL LETTER I WITH GRAVE */
pattern XK_igrave = MkKeySymbol #const XK_igrave
-- #define XK_iacute                        0x00ed  /* U+00ED LATIN SMALL LETTER I WITH ACUTE */
pattern XK_iacute = MkKeySymbol #const XK_iacute
-- #define XK_icircumflex                   0x00ee  /* U+00EE LATIN SMALL LETTER I WITH CIRCUMFLEX */
pattern XK_icircumflex = MkKeySymbol #const XK_icircumflex
-- #define XK_idiaeresis                    0x00ef  /* U+00EF LATIN SMALL LETTER I WITH DIAERESIS */
pattern XK_idiaeresis = MkKeySymbol #const XK_idiaeresis
-- #define XK_eth                           0x00f0  /* U+00F0 LATIN SMALL LETTER ETH */
pattern XK_eth = MkKeySymbol #const XK_eth
-- #define XK_ntilde                        0x00f1  /* U+00F1 LATIN SMALL LETTER N WITH TILDE */
pattern XK_ntilde = MkKeySymbol #const XK_ntilde
-- #define XK_ograve                        0x00f2  /* U+00F2 LATIN SMALL LETTER O WITH GRAVE */
pattern XK_ograve = MkKeySymbol #const XK_ograve
-- #define XK_oacute                        0x00f3  /* U+00F3 LATIN SMALL LETTER O WITH ACUTE */
pattern XK_oacute = MkKeySymbol #const XK_oacute
-- #define XK_ocircumflex                   0x00f4  /* U+00F4 LATIN SMALL LETTER O WITH CIRCUMFLEX */
pattern XK_ocircumflex = MkKeySymbol #const XK_ocircumflex
-- #define XK_otilde                        0x00f5  /* U+00F5 LATIN SMALL LETTER O WITH TILDE */
pattern XK_otilde = MkKeySymbol #const XK_otilde
-- #define XK_odiaeresis                    0x00f6  /* U+00F6 LATIN SMALL LETTER O WITH DIAERESIS */
pattern XK_odiaeresis = MkKeySymbol #const XK_odiaeresis
-- #define XK_division                      0x00f7  /* U+00F7 DIVISION SIGN */
pattern XK_division = MkKeySymbol #const XK_division
-- #define XK_oslash                        0x00f8  /* U+00F8 LATIN SMALL LETTER O WITH STROKE */
pattern XK_oslash = MkKeySymbol #const XK_oslash
-- #define XK_ooblique                      0x00f8  /* U+00F8 LATIN SMALL LETTER O WITH STROKE */
pattern XK_ooblique = MkKeySymbol #const XK_ooblique
-- #define XK_ugrave                        0x00f9  /* U+00F9 LATIN SMALL LETTER U WITH GRAVE */
pattern XK_ugrave = MkKeySymbol #const XK_ugrave
-- #define XK_uacute                        0x00fa  /* U+00FA LATIN SMALL LETTER U WITH ACUTE */
pattern XK_uacute = MkKeySymbol #const XK_uacute
-- #define XK_ucircumflex                   0x00fb  /* U+00FB LATIN SMALL LETTER U WITH CIRCUMFLEX */
pattern XK_ucircumflex = MkKeySymbol #const XK_ucircumflex
-- #define XK_udiaeresis                    0x00fc  /* U+00FC LATIN SMALL LETTER U WITH DIAERESIS */
pattern XK_udiaeresis = MkKeySymbol #const XK_udiaeresis
-- #define XK_yacute                        0x00fd  /* U+00FD LATIN SMALL LETTER Y WITH ACUTE */
pattern XK_yacute = MkKeySymbol #const XK_yacute
-- #define XK_thorn                         0x00fe  /* U+00FE LATIN SMALL LETTER THORN */
pattern XK_thorn = MkKeySymbol #const XK_thorn
-- #define XK_ydiaeresis                    0x00ff  /* U+00FF LATIN SMALL LETTER Y WITH DIAERESIS */
pattern XK_ydiaeresis = MkKeySymbol #const XK_ydiaeresis
-- #define XK_Aogonek                       0x01a1  /* U+0104 LATIN CAPITAL LETTER A WITH OGONEK */
pattern XK_Aogonek = MkKeySymbol #const XK_Aogonek
-- #define XK_breve                         0x01a2  /* U+02D8 BREVE */
pattern XK_breve = MkKeySymbol #const XK_breve
-- #define XK_Lstroke                       0x01a3  /* U+0141 LATIN CAPITAL LETTER L WITH STROKE */
pattern XK_Lstroke = MkKeySymbol #const XK_Lstroke
-- #define XK_Lcaron                        0x01a5  /* U+013D LATIN CAPITAL LETTER L WITH CARON */
pattern XK_Lcaron = MkKeySymbol #const XK_Lcaron
-- #define XK_Sacute                        0x01a6  /* U+015A LATIN CAPITAL LETTER S WITH ACUTE */
pattern XK_Sacute = MkKeySymbol #const XK_Sacute
-- #define XK_Scaron                        0x01a9  /* U+0160 LATIN CAPITAL LETTER S WITH CARON */
pattern XK_Scaron = MkKeySymbol #const XK_Scaron
-- #define XK_Scedilla                      0x01aa  /* U+015E LATIN CAPITAL LETTER S WITH CEDILLA */
pattern XK_Scedilla = MkKeySymbol #const XK_Scedilla
-- #define XK_Tcaron                        0x01ab  /* U+0164 LATIN CAPITAL LETTER T WITH CARON */
pattern XK_Tcaron = MkKeySymbol #const XK_Tcaron
-- #define XK_Zacute                        0x01ac  /* U+0179 LATIN CAPITAL LETTER Z WITH ACUTE */
pattern XK_Zacute = MkKeySymbol #const XK_Zacute
-- #define XK_Zcaron                        0x01ae  /* U+017D LATIN CAPITAL LETTER Z WITH CARON */
pattern XK_Zcaron = MkKeySymbol #const XK_Zcaron
-- #define XK_Zabovedot                     0x01af  /* U+017B LATIN CAPITAL LETTER Z WITH DOT ABOVE */
pattern XK_Zabovedot = MkKeySymbol #const XK_Zabovedot
-- #define XK_aogonek                       0x01b1  /* U+0105 LATIN SMALL LETTER A WITH OGONEK */
pattern XK_aogonek = MkKeySymbol #const XK_aogonek
-- #define XK_ogonek                        0x01b2  /* U+02DB OGONEK */
pattern XK_ogonek = MkKeySymbol #const XK_ogonek
-- #define XK_lstroke                       0x01b3  /* U+0142 LATIN SMALL LETTER L WITH STROKE */
pattern XK_lstroke = MkKeySymbol #const XK_lstroke
-- #define XK_lcaron                        0x01b5  /* U+013E LATIN SMALL LETTER L WITH CARON */
pattern XK_lcaron = MkKeySymbol #const XK_lcaron
-- #define XK_sacute                        0x01b6  /* U+015B LATIN SMALL LETTER S WITH ACUTE */
pattern XK_sacute = MkKeySymbol #const XK_sacute
-- #define XK_caron                         0x01b7  /* U+02C7 CARON */
pattern XK_caron = MkKeySymbol #const XK_caron
-- #define XK_scaron                        0x01b9  /* U+0161 LATIN SMALL LETTER S WITH CARON */
pattern XK_scaron = MkKeySymbol #const XK_scaron
-- #define XK_scedilla                      0x01ba  /* U+015F LATIN SMALL LETTER S WITH CEDILLA */
pattern XK_scedilla = MkKeySymbol #const XK_scedilla
-- #define XK_tcaron                        0x01bb  /* U+0165 LATIN SMALL LETTER T WITH CARON */
pattern XK_tcaron = MkKeySymbol #const XK_tcaron
-- #define XK_zacute                        0x01bc  /* U+017A LATIN SMALL LETTER Z WITH ACUTE */
pattern XK_zacute = MkKeySymbol #const XK_zacute
-- #define XK_doubleacute                   0x01bd  /* U+02DD DOUBLE ACUTE ACCENT */
pattern XK_doubleacute = MkKeySymbol #const XK_doubleacute
-- #define XK_zcaron                        0x01be  /* U+017E LATIN SMALL LETTER Z WITH CARON */
pattern XK_zcaron = MkKeySymbol #const XK_zcaron
-- #define XK_zabovedot                     0x01bf  /* U+017C LATIN SMALL LETTER Z WITH DOT ABOVE */
pattern XK_zabovedot = MkKeySymbol #const XK_zabovedot
-- #define XK_Racute                        0x01c0  /* U+0154 LATIN CAPITAL LETTER R WITH ACUTE */
pattern XK_Racute = MkKeySymbol #const XK_Racute
-- #define XK_Abreve                        0x01c3  /* U+0102 LATIN CAPITAL LETTER A WITH BREVE */
pattern XK_Abreve = MkKeySymbol #const XK_Abreve
-- #define XK_Lacute                        0x01c5  /* U+0139 LATIN CAPITAL LETTER L WITH ACUTE */
pattern XK_Lacute = MkKeySymbol #const XK_Lacute
-- #define XK_Cacute                        0x01c6  /* U+0106 LATIN CAPITAL LETTER C WITH ACUTE */
pattern XK_Cacute = MkKeySymbol #const XK_Cacute
-- #define XK_Ccaron                        0x01c8  /* U+010C LATIN CAPITAL LETTER C WITH CARON */
pattern XK_Ccaron = MkKeySymbol #const XK_Ccaron
-- #define XK_Eogonek                       0x01ca  /* U+0118 LATIN CAPITAL LETTER E WITH OGONEK */
pattern XK_Eogonek = MkKeySymbol #const XK_Eogonek
-- #define XK_Ecaron                        0x01cc  /* U+011A LATIN CAPITAL LETTER E WITH CARON */
pattern XK_Ecaron = MkKeySymbol #const XK_Ecaron
-- #define XK_Dcaron                        0x01cf  /* U+010E LATIN CAPITAL LETTER D WITH CARON */
pattern XK_Dcaron = MkKeySymbol #const XK_Dcaron
-- #define XK_Dstroke                       0x01d0  /* U+0110 LATIN CAPITAL LETTER D WITH STROKE */
pattern XK_Dstroke = MkKeySymbol #const XK_Dstroke
-- #define XK_Nacute                        0x01d1  /* U+0143 LATIN CAPITAL LETTER N WITH ACUTE */
pattern XK_Nacute = MkKeySymbol #const XK_Nacute
-- #define XK_Ncaron                        0x01d2  /* U+0147 LATIN CAPITAL LETTER N WITH CARON */
pattern XK_Ncaron = MkKeySymbol #const XK_Ncaron
-- #define XK_Odoubleacute                  0x01d5  /* U+0150 LATIN CAPITAL LETTER O WITH DOUBLE ACUTE */
pattern XK_Odoubleacute = MkKeySymbol #const XK_Odoubleacute
-- #define XK_Rcaron                        0x01d8  /* U+0158 LATIN CAPITAL LETTER R WITH CARON */
pattern XK_Rcaron = MkKeySymbol #const XK_Rcaron
-- #define XK_Uring                         0x01d9  /* U+016E LATIN CAPITAL LETTER U WITH RING ABOVE */
pattern XK_Uring = MkKeySymbol #const XK_Uring
-- #define XK_Udoubleacute                  0x01db  /* U+0170 LATIN CAPITAL LETTER U WITH DOUBLE ACUTE */
pattern XK_Udoubleacute = MkKeySymbol #const XK_Udoubleacute
-- #define XK_Tcedilla                      0x01de  /* U+0162 LATIN CAPITAL LETTER T WITH CEDILLA */
pattern XK_Tcedilla = MkKeySymbol #const XK_Tcedilla
-- #define XK_racute                        0x01e0  /* U+0155 LATIN SMALL LETTER R WITH ACUTE */
pattern XK_racute = MkKeySymbol #const XK_racute
-- #define XK_abreve                        0x01e3  /* U+0103 LATIN SMALL LETTER A WITH BREVE */
pattern XK_abreve = MkKeySymbol #const XK_abreve
-- #define XK_lacute                        0x01e5  /* U+013A LATIN SMALL LETTER L WITH ACUTE */
pattern XK_lacute = MkKeySymbol #const XK_lacute
-- #define XK_cacute                        0x01e6  /* U+0107 LATIN SMALL LETTER C WITH ACUTE */
pattern XK_cacute = MkKeySymbol #const XK_cacute
-- #define XK_ccaron                        0x01e8  /* U+010D LATIN SMALL LETTER C WITH CARON */
pattern XK_ccaron = MkKeySymbol #const XK_ccaron
-- #define XK_eogonek                       0x01ea  /* U+0119 LATIN SMALL LETTER E WITH OGONEK */
pattern XK_eogonek = MkKeySymbol #const XK_eogonek
-- #define XK_ecaron                        0x01ec  /* U+011B LATIN SMALL LETTER E WITH CARON */
pattern XK_ecaron = MkKeySymbol #const XK_ecaron
-- #define XK_dcaron                        0x01ef  /* U+010F LATIN SMALL LETTER D WITH CARON */
pattern XK_dcaron = MkKeySymbol #const XK_dcaron
-- #define XK_dstroke                       0x01f0  /* U+0111 LATIN SMALL LETTER D WITH STROKE */
pattern XK_dstroke = MkKeySymbol #const XK_dstroke
-- #define XK_nacute                        0x01f1  /* U+0144 LATIN SMALL LETTER N WITH ACUTE */
pattern XK_nacute = MkKeySymbol #const XK_nacute
-- #define XK_ncaron                        0x01f2  /* U+0148 LATIN SMALL LETTER N WITH CARON */
pattern XK_ncaron = MkKeySymbol #const XK_ncaron
-- #define XK_odoubleacute                  0x01f5  /* U+0151 LATIN SMALL LETTER O WITH DOUBLE ACUTE */
pattern XK_odoubleacute = MkKeySymbol #const XK_odoubleacute
-- #define XK_rcaron                        0x01f8  /* U+0159 LATIN SMALL LETTER R WITH CARON */
pattern XK_rcaron = MkKeySymbol #const XK_rcaron
-- #define XK_uring                         0x01f9  /* U+016F LATIN SMALL LETTER U WITH RING ABOVE */
pattern XK_uring = MkKeySymbol #const XK_uring
-- #define XK_udoubleacute                  0x01fb  /* U+0171 LATIN SMALL LETTER U WITH DOUBLE ACUTE */
pattern XK_udoubleacute = MkKeySymbol #const XK_udoubleacute
-- #define XK_tcedilla                      0x01fe  /* U+0163 LATIN SMALL LETTER T WITH CEDILLA */
pattern XK_tcedilla = MkKeySymbol #const XK_tcedilla
-- #define XK_abovedot                      0x01ff  /* U+02D9 DOT ABOVE */
pattern XK_abovedot = MkKeySymbol #const XK_abovedot
-- #define XK_Hstroke                       0x02a1  /* U+0126 LATIN CAPITAL LETTER H WITH STROKE */
pattern XK_Hstroke = MkKeySymbol #const XK_Hstroke
-- #define XK_Hcircumflex                   0x02a6  /* U+0124 LATIN CAPITAL LETTER H WITH CIRCUMFLEX */
pattern XK_Hcircumflex = MkKeySymbol #const XK_Hcircumflex
-- #define XK_Iabovedot                     0x02a9  /* U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE */
pattern XK_Iabovedot = MkKeySymbol #const XK_Iabovedot
-- #define XK_Gbreve                        0x02ab  /* U+011E LATIN CAPITAL LETTER G WITH BREVE */
pattern XK_Gbreve = MkKeySymbol #const XK_Gbreve
-- #define XK_Jcircumflex                   0x02ac  /* U+0134 LATIN CAPITAL LETTER J WITH CIRCUMFLEX */
pattern XK_Jcircumflex = MkKeySymbol #const XK_Jcircumflex
-- #define XK_hstroke                       0x02b1  /* U+0127 LATIN SMALL LETTER H WITH STROKE */
pattern XK_hstroke = MkKeySymbol #const XK_hstroke
-- #define XK_hcircumflex                   0x02b6  /* U+0125 LATIN SMALL LETTER H WITH CIRCUMFLEX */
pattern XK_hcircumflex = MkKeySymbol #const XK_hcircumflex
-- #define XK_idotless                      0x02b9  /* U+0131 LATIN SMALL LETTER DOTLESS I */
pattern XK_idotless = MkKeySymbol #const XK_idotless
-- #define XK_gbreve                        0x02bb  /* U+011F LATIN SMALL LETTER G WITH BREVE */
pattern XK_gbreve = MkKeySymbol #const XK_gbreve
-- #define XK_jcircumflex                   0x02bc  /* U+0135 LATIN SMALL LETTER J WITH CIRCUMFLEX */
pattern XK_jcircumflex = MkKeySymbol #const XK_jcircumflex
-- #define XK_Cabovedot                     0x02c5  /* U+010A LATIN CAPITAL LETTER C WITH DOT ABOVE */
pattern XK_Cabovedot = MkKeySymbol #const XK_Cabovedot
-- #define XK_Ccircumflex                   0x02c6  /* U+0108 LATIN CAPITAL LETTER C WITH CIRCUMFLEX */
pattern XK_Ccircumflex = MkKeySymbol #const XK_Ccircumflex
-- #define XK_Gabovedot                     0x02d5  /* U+0120 LATIN CAPITAL LETTER G WITH DOT ABOVE */
pattern XK_Gabovedot = MkKeySymbol #const XK_Gabovedot
-- #define XK_Gcircumflex                   0x02d8  /* U+011C LATIN CAPITAL LETTER G WITH CIRCUMFLEX */
pattern XK_Gcircumflex = MkKeySymbol #const XK_Gcircumflex
-- #define XK_Ubreve                        0x02dd  /* U+016C LATIN CAPITAL LETTER U WITH BREVE */
pattern XK_Ubreve = MkKeySymbol #const XK_Ubreve
-- #define XK_Scircumflex                   0x02de  /* U+015C LATIN CAPITAL LETTER S WITH CIRCUMFLEX */
pattern XK_Scircumflex = MkKeySymbol #const XK_Scircumflex
-- #define XK_cabovedot                     0x02e5  /* U+010B LATIN SMALL LETTER C WITH DOT ABOVE */
pattern XK_cabovedot = MkKeySymbol #const XK_cabovedot
-- #define XK_ccircumflex                   0x02e6  /* U+0109 LATIN SMALL LETTER C WITH CIRCUMFLEX */
pattern XK_ccircumflex = MkKeySymbol #const XK_ccircumflex
-- #define XK_gabovedot                     0x02f5  /* U+0121 LATIN SMALL LETTER G WITH DOT ABOVE */
pattern XK_gabovedot = MkKeySymbol #const XK_gabovedot
-- #define XK_gcircumflex                   0x02f8  /* U+011D LATIN SMALL LETTER G WITH CIRCUMFLEX */
pattern XK_gcircumflex = MkKeySymbol #const XK_gcircumflex
-- #define XK_ubreve                        0x02fd  /* U+016D LATIN SMALL LETTER U WITH BREVE */
pattern XK_ubreve = MkKeySymbol #const XK_ubreve
-- #define XK_scircumflex                   0x02fe  /* U+015D LATIN SMALL LETTER S WITH CIRCUMFLEX */
pattern XK_scircumflex = MkKeySymbol #const XK_scircumflex
-- #define XK_kra                           0x03a2  /* U+0138 LATIN SMALL LETTER KRA */
pattern XK_kra = MkKeySymbol #const XK_kra
-- #define XK_kappa                         0x03a2  /* deprecated */
pattern XK_kappa = MkKeySymbol #const XK_kappa
-- #define XK_Rcedilla                      0x03a3  /* U+0156 LATIN CAPITAL LETTER R WITH CEDILLA */
pattern XK_Rcedilla = MkKeySymbol #const XK_Rcedilla
-- #define XK_Itilde                        0x03a5  /* U+0128 LATIN CAPITAL LETTER I WITH TILDE */
pattern XK_Itilde = MkKeySymbol #const XK_Itilde
-- #define XK_Lcedilla                      0x03a6  /* U+013B LATIN CAPITAL LETTER L WITH CEDILLA */
pattern XK_Lcedilla = MkKeySymbol #const XK_Lcedilla
-- #define XK_Emacron                       0x03aa  /* U+0112 LATIN CAPITAL LETTER E WITH MACRON */
pattern XK_Emacron = MkKeySymbol #const XK_Emacron
-- #define XK_Gcedilla                      0x03ab  /* U+0122 LATIN CAPITAL LETTER G WITH CEDILLA */
pattern XK_Gcedilla = MkKeySymbol #const XK_Gcedilla
-- #define XK_Tslash                        0x03ac  /* U+0166 LATIN CAPITAL LETTER T WITH STROKE */
pattern XK_Tslash = MkKeySymbol #const XK_Tslash
-- #define XK_rcedilla                      0x03b3  /* U+0157 LATIN SMALL LETTER R WITH CEDILLA */
pattern XK_rcedilla = MkKeySymbol #const XK_rcedilla
-- #define XK_itilde                        0x03b5  /* U+0129 LATIN SMALL LETTER I WITH TILDE */
pattern XK_itilde = MkKeySymbol #const XK_itilde
-- #define XK_lcedilla                      0x03b6  /* U+013C LATIN SMALL LETTER L WITH CEDILLA */
pattern XK_lcedilla = MkKeySymbol #const XK_lcedilla
-- #define XK_emacron                       0x03ba  /* U+0113 LATIN SMALL LETTER E WITH MACRON */
pattern XK_emacron = MkKeySymbol #const XK_emacron
-- #define XK_gcedilla                      0x03bb  /* U+0123 LATIN SMALL LETTER G WITH CEDILLA */
pattern XK_gcedilla = MkKeySymbol #const XK_gcedilla
-- #define XK_tslash                        0x03bc  /* U+0167 LATIN SMALL LETTER T WITH STROKE */
pattern XK_tslash = MkKeySymbol #const XK_tslash
-- #define XK_ENG                           0x03bd  /* U+014A LATIN CAPITAL LETTER ENG */
pattern XK_ENG = MkKeySymbol #const XK_ENG
-- #define XK_eng                           0x03bf  /* U+014B LATIN SMALL LETTER ENG */
pattern XK_eng = MkKeySymbol #const XK_eng
-- #define XK_Amacron                       0x03c0  /* U+0100 LATIN CAPITAL LETTER A WITH MACRON */
pattern XK_Amacron = MkKeySymbol #const XK_Amacron
-- #define XK_Iogonek                       0x03c7  /* U+012E LATIN CAPITAL LETTER I WITH OGONEK */
pattern XK_Iogonek = MkKeySymbol #const XK_Iogonek
-- #define XK_Eabovedot                     0x03cc  /* U+0116 LATIN CAPITAL LETTER E WITH DOT ABOVE */
pattern XK_Eabovedot = MkKeySymbol #const XK_Eabovedot
-- #define XK_Imacron                       0x03cf  /* U+012A LATIN CAPITAL LETTER I WITH MACRON */
pattern XK_Imacron = MkKeySymbol #const XK_Imacron
-- #define XK_Ncedilla                      0x03d1  /* U+0145 LATIN CAPITAL LETTER N WITH CEDILLA */
pattern XK_Ncedilla = MkKeySymbol #const XK_Ncedilla
-- #define XK_Omacron                       0x03d2  /* U+014C LATIN CAPITAL LETTER O WITH MACRON */
pattern XK_Omacron = MkKeySymbol #const XK_Omacron
-- #define XK_Kcedilla                      0x03d3  /* U+0136 LATIN CAPITAL LETTER K WITH CEDILLA */
pattern XK_Kcedilla = MkKeySymbol #const XK_Kcedilla
-- #define XK_Uogonek                       0x03d9  /* U+0172 LATIN CAPITAL LETTER U WITH OGONEK */
pattern XK_Uogonek = MkKeySymbol #const XK_Uogonek
-- #define XK_Utilde                        0x03dd  /* U+0168 LATIN CAPITAL LETTER U WITH TILDE */
pattern XK_Utilde = MkKeySymbol #const XK_Utilde
-- #define XK_Umacron                       0x03de  /* U+016A LATIN CAPITAL LETTER U WITH MACRON */
pattern XK_Umacron = MkKeySymbol #const XK_Umacron
-- #define XK_amacron                       0x03e0  /* U+0101 LATIN SMALL LETTER A WITH MACRON */
pattern XK_amacron = MkKeySymbol #const XK_amacron
-- #define XK_iogonek                       0x03e7  /* U+012F LATIN SMALL LETTER I WITH OGONEK */
pattern XK_iogonek = MkKeySymbol #const XK_iogonek
-- #define XK_eabovedot                     0x03ec  /* U+0117 LATIN SMALL LETTER E WITH DOT ABOVE */
pattern XK_eabovedot = MkKeySymbol #const XK_eabovedot
-- #define XK_imacron                       0x03ef  /* U+012B LATIN SMALL LETTER I WITH MACRON */
pattern XK_imacron = MkKeySymbol #const XK_imacron
-- #define XK_ncedilla                      0x03f1  /* U+0146 LATIN SMALL LETTER N WITH CEDILLA */
pattern XK_ncedilla = MkKeySymbol #const XK_ncedilla
-- #define XK_omacron                       0x03f2  /* U+014D LATIN SMALL LETTER O WITH MACRON */
pattern XK_omacron = MkKeySymbol #const XK_omacron
-- #define XK_kcedilla                      0x03f3  /* U+0137 LATIN SMALL LETTER K WITH CEDILLA */
pattern XK_kcedilla = MkKeySymbol #const XK_kcedilla
-- #define XK_uogonek                       0x03f9  /* U+0173 LATIN SMALL LETTER U WITH OGONEK */
pattern XK_uogonek = MkKeySymbol #const XK_uogonek
-- #define XK_utilde                        0x03fd  /* U+0169 LATIN SMALL LETTER U WITH TILDE */
pattern XK_utilde = MkKeySymbol #const XK_utilde
-- #define XK_umacron                       0x03fe  /* U+016B LATIN SMALL LETTER U WITH MACRON */
pattern XK_umacron = MkKeySymbol #const XK_umacron
-- #define XK_Wcircumflex                0x1000174  /* U+0174 LATIN CAPITAL LETTER W WITH CIRCUMFLEX */
pattern XK_Wcircumflex = MkKeySymbol #const XK_Wcircumflex
-- #define XK_wcircumflex                0x1000175  /* U+0175 LATIN SMALL LETTER W WITH CIRCUMFLEX */
pattern XK_wcircumflex = MkKeySymbol #const XK_wcircumflex
-- #define XK_Ycircumflex                0x1000176  /* U+0176 LATIN CAPITAL LETTER Y WITH CIRCUMFLEX */
pattern XK_Ycircumflex = MkKeySymbol #const XK_Ycircumflex
-- #define XK_ycircumflex                0x1000177  /* U+0177 LATIN SMALL LETTER Y WITH CIRCUMFLEX */
pattern XK_ycircumflex = MkKeySymbol #const XK_ycircumflex
-- #define XK_Babovedot                  0x1001e02  /* U+1E02 LATIN CAPITAL LETTER B WITH DOT ABOVE */
pattern XK_Babovedot = MkKeySymbol #const XK_Babovedot
-- #define XK_babovedot                  0x1001e03  /* U+1E03 LATIN SMALL LETTER B WITH DOT ABOVE */
pattern XK_babovedot = MkKeySymbol #const XK_babovedot
-- #define XK_Dabovedot                  0x1001e0a  /* U+1E0A LATIN CAPITAL LETTER D WITH DOT ABOVE */
pattern XK_Dabovedot = MkKeySymbol #const XK_Dabovedot
-- #define XK_dabovedot                  0x1001e0b  /* U+1E0B LATIN SMALL LETTER D WITH DOT ABOVE */
pattern XK_dabovedot = MkKeySymbol #const XK_dabovedot
-- #define XK_Fabovedot                  0x1001e1e  /* U+1E1E LATIN CAPITAL LETTER F WITH DOT ABOVE */
pattern XK_Fabovedot = MkKeySymbol #const XK_Fabovedot
-- #define XK_fabovedot                  0x1001e1f  /* U+1E1F LATIN SMALL LETTER F WITH DOT ABOVE */
pattern XK_fabovedot = MkKeySymbol #const XK_fabovedot
-- #define XK_Mabovedot                  0x1001e40  /* U+1E40 LATIN CAPITAL LETTER M WITH DOT ABOVE */
pattern XK_Mabovedot = MkKeySymbol #const XK_Mabovedot
-- #define XK_mabovedot                  0x1001e41  /* U+1E41 LATIN SMALL LETTER M WITH DOT ABOVE */
pattern XK_mabovedot = MkKeySymbol #const XK_mabovedot
-- #define XK_Pabovedot                  0x1001e56  /* U+1E56 LATIN CAPITAL LETTER P WITH DOT ABOVE */
pattern XK_Pabovedot = MkKeySymbol #const XK_Pabovedot
-- #define XK_pabovedot                  0x1001e57  /* U+1E57 LATIN SMALL LETTER P WITH DOT ABOVE */
pattern XK_pabovedot = MkKeySymbol #const XK_pabovedot
-- #define XK_Sabovedot                  0x1001e60  /* U+1E60 LATIN CAPITAL LETTER S WITH DOT ABOVE */
pattern XK_Sabovedot = MkKeySymbol #const XK_Sabovedot
-- #define XK_sabovedot                  0x1001e61  /* U+1E61 LATIN SMALL LETTER S WITH DOT ABOVE */
pattern XK_sabovedot = MkKeySymbol #const XK_sabovedot
-- #define XK_Tabovedot                  0x1001e6a  /* U+1E6A LATIN CAPITAL LETTER T WITH DOT ABOVE */
pattern XK_Tabovedot = MkKeySymbol #const XK_Tabovedot
-- #define XK_tabovedot                  0x1001e6b  /* U+1E6B LATIN SMALL LETTER T WITH DOT ABOVE */
pattern XK_tabovedot = MkKeySymbol #const XK_tabovedot
-- #define XK_Wgrave                     0x1001e80  /* U+1E80 LATIN CAPITAL LETTER W WITH GRAVE */
pattern XK_Wgrave = MkKeySymbol #const XK_Wgrave
-- #define XK_wgrave                     0x1001e81  /* U+1E81 LATIN SMALL LETTER W WITH GRAVE */
pattern XK_wgrave = MkKeySymbol #const XK_wgrave
-- #define XK_Wacute                     0x1001e82  /* U+1E82 LATIN CAPITAL LETTER W WITH ACUTE */
pattern XK_Wacute = MkKeySymbol #const XK_Wacute
-- #define XK_wacute                     0x1001e83  /* U+1E83 LATIN SMALL LETTER W WITH ACUTE */
pattern XK_wacute = MkKeySymbol #const XK_wacute
-- #define XK_Wdiaeresis                 0x1001e84  /* U+1E84 LATIN CAPITAL LETTER W WITH DIAERESIS */
pattern XK_Wdiaeresis = MkKeySymbol #const XK_Wdiaeresis
-- #define XK_wdiaeresis                 0x1001e85  /* U+1E85 LATIN SMALL LETTER W WITH DIAERESIS */
pattern XK_wdiaeresis = MkKeySymbol #const XK_wdiaeresis
-- #define XK_Ygrave                     0x1001ef2  /* U+1EF2 LATIN CAPITAL LETTER Y WITH GRAVE */
pattern XK_Ygrave = MkKeySymbol #const XK_Ygrave
-- #define XK_ygrave                     0x1001ef3  /* U+1EF3 LATIN SMALL LETTER Y WITH GRAVE */
pattern XK_ygrave = MkKeySymbol #const XK_ygrave
-- #define XK_OE                            0x13bc  /* U+0152 LATIN CAPITAL LIGATURE OE */
pattern XK_OE = MkKeySymbol #const XK_OE
-- #define XK_oe                            0x13bd  /* U+0153 LATIN SMALL LIGATURE OE */
pattern XK_oe = MkKeySymbol #const XK_oe
-- #define XK_Ydiaeresis                    0x13be  /* U+0178 LATIN CAPITAL LETTER Y WITH DIAERESIS */
pattern XK_Ydiaeresis = MkKeySymbol #const XK_Ydiaeresis
-- #define XK_overline                      0x047e  /* U+203E OVERLINE */
pattern XK_overline = MkKeySymbol #const XK_overline
-- #define XK_kana_fullstop                 0x04a1  /* U+3002 IDEOGRAPHIC FULL STOP */
pattern XK_kana_fullstop = MkKeySymbol #const XK_kana_fullstop
-- #define XK_kana_openingbracket           0x04a2  /* U+300C LEFT CORNER BRACKET */
pattern XK_kana_openingbracket = MkKeySymbol #const XK_kana_openingbracket
-- #define XK_kana_closingbracket           0x04a3  /* U+300D RIGHT CORNER BRACKET */
pattern XK_kana_closingbracket = MkKeySymbol #const XK_kana_closingbracket
-- #define XK_kana_comma                    0x04a4  /* U+3001 IDEOGRAPHIC COMMA */
pattern XK_kana_comma = MkKeySymbol #const XK_kana_comma
-- #define XK_kana_conjunctive              0x04a5  /* U+30FB KATAKANA MIDDLE DOT */
pattern XK_kana_conjunctive = MkKeySymbol #const XK_kana_conjunctive
-- #define XK_kana_middledot                0x04a5  /* deprecated */
pattern XK_kana_middledot = MkKeySymbol #const XK_kana_middledot
-- #define XK_kana_WO                       0x04a6  /* U+30F2 KATAKANA LETTER WO */
pattern XK_kana_WO = MkKeySymbol #const XK_kana_WO
-- #define XK_kana_a                        0x04a7  /* U+30A1 KATAKANA LETTER SMALL A */
pattern XK_kana_a = MkKeySymbol #const XK_kana_a
-- #define XK_kana_i                        0x04a8  /* U+30A3 KATAKANA LETTER SMALL I */
pattern XK_kana_i = MkKeySymbol #const XK_kana_i
-- #define XK_kana_u                        0x04a9  /* U+30A5 KATAKANA LETTER SMALL U */
pattern XK_kana_u = MkKeySymbol #const XK_kana_u
-- #define XK_kana_e                        0x04aa  /* U+30A7 KATAKANA LETTER SMALL E */
pattern XK_kana_e = MkKeySymbol #const XK_kana_e
-- #define XK_kana_o                        0x04ab  /* U+30A9 KATAKANA LETTER SMALL O */
pattern XK_kana_o = MkKeySymbol #const XK_kana_o
-- #define XK_kana_ya                       0x04ac  /* U+30E3 KATAKANA LETTER SMALL YA */
pattern XK_kana_ya = MkKeySymbol #const XK_kana_ya
-- #define XK_kana_yu                       0x04ad  /* U+30E5 KATAKANA LETTER SMALL YU */
pattern XK_kana_yu = MkKeySymbol #const XK_kana_yu
-- #define XK_kana_yo                       0x04ae  /* U+30E7 KATAKANA LETTER SMALL YO */
pattern XK_kana_yo = MkKeySymbol #const XK_kana_yo
-- #define XK_kana_tsu                      0x04af  /* U+30C3 KATAKANA LETTER SMALL TU */
pattern XK_kana_tsu = MkKeySymbol #const XK_kana_tsu
-- #define XK_kana_tu                       0x04af  /* deprecated */
pattern XK_kana_tu = MkKeySymbol #const XK_kana_tu
-- #define XK_prolongedsound                0x04b0  /* U+30FC KATAKANA-HIRAGANA PROLONGED SOUND MARK */
pattern XK_prolongedsound = MkKeySymbol #const XK_prolongedsound
-- #define XK_kana_A                        0x04b1  /* U+30A2 KATAKANA LETTER A */
pattern XK_kana_A = MkKeySymbol #const XK_kana_A
-- #define XK_kana_I                        0x04b2  /* U+30A4 KATAKANA LETTER I */
pattern XK_kana_I = MkKeySymbol #const XK_kana_I
-- #define XK_kana_U                        0x04b3  /* U+30A6 KATAKANA LETTER U */
pattern XK_kana_U = MkKeySymbol #const XK_kana_U
-- #define XK_kana_E                        0x04b4  /* U+30A8 KATAKANA LETTER E */
pattern XK_kana_E = MkKeySymbol #const XK_kana_E
-- #define XK_kana_O                        0x04b5  /* U+30AA KATAKANA LETTER O */
pattern XK_kana_O = MkKeySymbol #const XK_kana_O
-- #define XK_kana_KA                       0x04b6  /* U+30AB KATAKANA LETTER KA */
pattern XK_kana_KA = MkKeySymbol #const XK_kana_KA
-- #define XK_kana_KI                       0x04b7  /* U+30AD KATAKANA LETTER KI */
pattern XK_kana_KI = MkKeySymbol #const XK_kana_KI
-- #define XK_kana_KU                       0x04b8  /* U+30AF KATAKANA LETTER KU */
pattern XK_kana_KU = MkKeySymbol #const XK_kana_KU
-- #define XK_kana_KE                       0x04b9  /* U+30B1 KATAKANA LETTER KE */
pattern XK_kana_KE = MkKeySymbol #const XK_kana_KE
-- #define XK_kana_KO                       0x04ba  /* U+30B3 KATAKANA LETTER KO */
pattern XK_kana_KO = MkKeySymbol #const XK_kana_KO
-- #define XK_kana_SA                       0x04bb  /* U+30B5 KATAKANA LETTER SA */
pattern XK_kana_SA = MkKeySymbol #const XK_kana_SA
-- #define XK_kana_SHI                      0x04bc  /* U+30B7 KATAKANA LETTER SI */
pattern XK_kana_SHI = MkKeySymbol #const XK_kana_SHI
-- #define XK_kana_SU                       0x04bd  /* U+30B9 KATAKANA LETTER SU */
pattern XK_kana_SU = MkKeySymbol #const XK_kana_SU
-- #define XK_kana_SE                       0x04be  /* U+30BB KATAKANA LETTER SE */
pattern XK_kana_SE = MkKeySymbol #const XK_kana_SE
-- #define XK_kana_SO                       0x04bf  /* U+30BD KATAKANA LETTER SO */
pattern XK_kana_SO = MkKeySymbol #const XK_kana_SO
-- #define XK_kana_TA                       0x04c0  /* U+30BF KATAKANA LETTER TA */
pattern XK_kana_TA = MkKeySymbol #const XK_kana_TA
-- #define XK_kana_CHI                      0x04c1  /* U+30C1 KATAKANA LETTER TI */
pattern XK_kana_CHI = MkKeySymbol #const XK_kana_CHI
-- #define XK_kana_TI                       0x04c1  /* deprecated */
pattern XK_kana_TI = MkKeySymbol #const XK_kana_TI
-- #define XK_kana_TSU                      0x04c2  /* U+30C4 KATAKANA LETTER TU */
pattern XK_kana_TSU = MkKeySymbol #const XK_kana_TSU
-- #define XK_kana_TU                       0x04c2  /* deprecated */
pattern XK_kana_TU = MkKeySymbol #const XK_kana_TU
-- #define XK_kana_TE                       0x04c3  /* U+30C6 KATAKANA LETTER TE */
pattern XK_kana_TE = MkKeySymbol #const XK_kana_TE
-- #define XK_kana_TO                       0x04c4  /* U+30C8 KATAKANA LETTER TO */
pattern XK_kana_TO = MkKeySymbol #const XK_kana_TO
-- #define XK_kana_NA                       0x04c5  /* U+30CA KATAKANA LETTER NA */
pattern XK_kana_NA = MkKeySymbol #const XK_kana_NA
-- #define XK_kana_NI                       0x04c6  /* U+30CB KATAKANA LETTER NI */
pattern XK_kana_NI = MkKeySymbol #const XK_kana_NI
-- #define XK_kana_NU                       0x04c7  /* U+30CC KATAKANA LETTER NU */
pattern XK_kana_NU = MkKeySymbol #const XK_kana_NU
-- #define XK_kana_NE                       0x04c8  /* U+30CD KATAKANA LETTER NE */
pattern XK_kana_NE = MkKeySymbol #const XK_kana_NE
-- #define XK_kana_NO                       0x04c9  /* U+30CE KATAKANA LETTER NO */
pattern XK_kana_NO = MkKeySymbol #const XK_kana_NO
-- #define XK_kana_HA                       0x04ca  /* U+30CF KATAKANA LETTER HA */
pattern XK_kana_HA = MkKeySymbol #const XK_kana_HA
-- #define XK_kana_HI                       0x04cb  /* U+30D2 KATAKANA LETTER HI */
pattern XK_kana_HI = MkKeySymbol #const XK_kana_HI
-- #define XK_kana_FU                       0x04cc  /* U+30D5 KATAKANA LETTER HU */
pattern XK_kana_FU = MkKeySymbol #const XK_kana_FU
-- #define XK_kana_HU                       0x04cc  /* deprecated */
pattern XK_kana_HU = MkKeySymbol #const XK_kana_HU
-- #define XK_kana_HE                       0x04cd  /* U+30D8 KATAKANA LETTER HE */
pattern XK_kana_HE = MkKeySymbol #const XK_kana_HE
-- #define XK_kana_HO                       0x04ce  /* U+30DB KATAKANA LETTER HO */
pattern XK_kana_HO = MkKeySymbol #const XK_kana_HO
-- #define XK_kana_MA                       0x04cf  /* U+30DE KATAKANA LETTER MA */
pattern XK_kana_MA = MkKeySymbol #const XK_kana_MA
-- #define XK_kana_MI                       0x04d0  /* U+30DF KATAKANA LETTER MI */
pattern XK_kana_MI = MkKeySymbol #const XK_kana_MI
-- #define XK_kana_MU                       0x04d1  /* U+30E0 KATAKANA LETTER MU */
pattern XK_kana_MU = MkKeySymbol #const XK_kana_MU
-- #define XK_kana_ME                       0x04d2  /* U+30E1 KATAKANA LETTER ME */
pattern XK_kana_ME = MkKeySymbol #const XK_kana_ME
-- #define XK_kana_MO                       0x04d3  /* U+30E2 KATAKANA LETTER MO */
pattern XK_kana_MO = MkKeySymbol #const XK_kana_MO
-- #define XK_kana_YA                       0x04d4  /* U+30E4 KATAKANA LETTER YA */
pattern XK_kana_YA = MkKeySymbol #const XK_kana_YA
-- #define XK_kana_YU                       0x04d5  /* U+30E6 KATAKANA LETTER YU */
pattern XK_kana_YU = MkKeySymbol #const XK_kana_YU
-- #define XK_kana_YO                       0x04d6  /* U+30E8 KATAKANA LETTER YO */
pattern XK_kana_YO = MkKeySymbol #const XK_kana_YO
-- #define XK_kana_RA                       0x04d7  /* U+30E9 KATAKANA LETTER RA */
pattern XK_kana_RA = MkKeySymbol #const XK_kana_RA
-- #define XK_kana_RI                       0x04d8  /* U+30EA KATAKANA LETTER RI */
pattern XK_kana_RI = MkKeySymbol #const XK_kana_RI
-- #define XK_kana_RU                       0x04d9  /* U+30EB KATAKANA LETTER RU */
pattern XK_kana_RU = MkKeySymbol #const XK_kana_RU
-- #define XK_kana_RE                       0x04da  /* U+30EC KATAKANA LETTER RE */
pattern XK_kana_RE = MkKeySymbol #const XK_kana_RE
-- #define XK_kana_RO                       0x04db  /* U+30ED KATAKANA LETTER RO */
pattern XK_kana_RO = MkKeySymbol #const XK_kana_RO
-- #define XK_kana_WA                       0x04dc  /* U+30EF KATAKANA LETTER WA */
pattern XK_kana_WA = MkKeySymbol #const XK_kana_WA
-- #define XK_kana_N                        0x04dd  /* U+30F3 KATAKANA LETTER N */
pattern XK_kana_N = MkKeySymbol #const XK_kana_N
-- #define XK_voicedsound                   0x04de  /* U+309B KATAKANA-HIRAGANA VOICED SOUND MARK */
pattern XK_voicedsound = MkKeySymbol #const XK_voicedsound
-- #define XK_semivoicedsound               0x04df  /* U+309C KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK */
pattern XK_semivoicedsound = MkKeySymbol #const XK_semivoicedsound
-- #define XK_kana_switch                   0xff7e  /* Alias for mode_switch */
pattern XK_kana_switch = MkKeySymbol #const XK_kana_switch
-- #define XK_Farsi_0                    0x10006f0  /* U+06F0 EXTENDED ARABIC-INDIC DIGIT ZERO */
pattern XK_Farsi_0 = MkKeySymbol #const XK_Farsi_0
-- #define XK_Farsi_1                    0x10006f1  /* U+06F1 EXTENDED ARABIC-INDIC DIGIT ONE */
pattern XK_Farsi_1 = MkKeySymbol #const XK_Farsi_1
-- #define XK_Farsi_2                    0x10006f2  /* U+06F2 EXTENDED ARABIC-INDIC DIGIT TWO */
pattern XK_Farsi_2 = MkKeySymbol #const XK_Farsi_2
-- #define XK_Farsi_3                    0x10006f3  /* U+06F3 EXTENDED ARABIC-INDIC DIGIT THREE */
pattern XK_Farsi_3 = MkKeySymbol #const XK_Farsi_3
-- #define XK_Farsi_4                    0x10006f4  /* U+06F4 EXTENDED ARABIC-INDIC DIGIT FOUR */
pattern XK_Farsi_4 = MkKeySymbol #const XK_Farsi_4
-- #define XK_Farsi_5                    0x10006f5  /* U+06F5 EXTENDED ARABIC-INDIC DIGIT FIVE */
pattern XK_Farsi_5 = MkKeySymbol #const XK_Farsi_5
-- #define XK_Farsi_6                    0x10006f6  /* U+06F6 EXTENDED ARABIC-INDIC DIGIT SIX */
pattern XK_Farsi_6 = MkKeySymbol #const XK_Farsi_6
-- #define XK_Farsi_7                    0x10006f7  /* U+06F7 EXTENDED ARABIC-INDIC DIGIT SEVEN */
pattern XK_Farsi_7 = MkKeySymbol #const XK_Farsi_7
-- #define XK_Farsi_8                    0x10006f8  /* U+06F8 EXTENDED ARABIC-INDIC DIGIT EIGHT */
pattern XK_Farsi_8 = MkKeySymbol #const XK_Farsi_8
-- #define XK_Farsi_9                    0x10006f9  /* U+06F9 EXTENDED ARABIC-INDIC DIGIT NINE */
pattern XK_Farsi_9 = MkKeySymbol #const XK_Farsi_9
-- #define XK_Arabic_percent             0x100066a  /* U+066A ARABIC PERCENT SIGN */
pattern XK_Arabic_percent = MkKeySymbol #const XK_Arabic_percent
-- #define XK_Arabic_superscript_alef    0x1000670  /* U+0670 ARABIC LETTER SUPERSCRIPT ALEF */
pattern XK_Arabic_superscript_alef = MkKeySymbol #const XK_Arabic_superscript_alef
-- #define XK_Arabic_tteh                0x1000679  /* U+0679 ARABIC LETTER TTEH */
pattern XK_Arabic_tteh = MkKeySymbol #const XK_Arabic_tteh
-- #define XK_Arabic_peh                 0x100067e  /* U+067E ARABIC LETTER PEH */
pattern XK_Arabic_peh = MkKeySymbol #const XK_Arabic_peh
-- #define XK_Arabic_tcheh               0x1000686  /* U+0686 ARABIC LETTER TCHEH */
pattern XK_Arabic_tcheh = MkKeySymbol #const XK_Arabic_tcheh
-- #define XK_Arabic_ddal                0x1000688  /* U+0688 ARABIC LETTER DDAL */
pattern XK_Arabic_ddal = MkKeySymbol #const XK_Arabic_ddal
-- #define XK_Arabic_rreh                0x1000691  /* U+0691 ARABIC LETTER RREH */
pattern XK_Arabic_rreh = MkKeySymbol #const XK_Arabic_rreh
-- #define XK_Arabic_comma                  0x05ac  /* U+060C ARABIC COMMA */
pattern XK_Arabic_comma = MkKeySymbol #const XK_Arabic_comma
-- #define XK_Arabic_fullstop            0x10006d4  /* U+06D4 ARABIC FULL STOP */
pattern XK_Arabic_fullstop = MkKeySymbol #const XK_Arabic_fullstop
-- #define XK_Arabic_0                   0x1000660  /* U+0660 ARABIC-INDIC DIGIT ZERO */
pattern XK_Arabic_0 = MkKeySymbol #const XK_Arabic_0
-- #define XK_Arabic_1                   0x1000661  /* U+0661 ARABIC-INDIC DIGIT ONE */
pattern XK_Arabic_1 = MkKeySymbol #const XK_Arabic_1
-- #define XK_Arabic_2                   0x1000662  /* U+0662 ARABIC-INDIC DIGIT TWO */
pattern XK_Arabic_2 = MkKeySymbol #const XK_Arabic_2
-- #define XK_Arabic_3                   0x1000663  /* U+0663 ARABIC-INDIC DIGIT THREE */
pattern XK_Arabic_3 = MkKeySymbol #const XK_Arabic_3
-- #define XK_Arabic_4                   0x1000664  /* U+0664 ARABIC-INDIC DIGIT FOUR */
pattern XK_Arabic_4 = MkKeySymbol #const XK_Arabic_4
-- #define XK_Arabic_5                   0x1000665  /* U+0665 ARABIC-INDIC DIGIT FIVE */
pattern XK_Arabic_5 = MkKeySymbol #const XK_Arabic_5
-- #define XK_Arabic_6                   0x1000666  /* U+0666 ARABIC-INDIC DIGIT SIX */
pattern XK_Arabic_6 = MkKeySymbol #const XK_Arabic_6
-- #define XK_Arabic_7                   0x1000667  /* U+0667 ARABIC-INDIC DIGIT SEVEN */
pattern XK_Arabic_7 = MkKeySymbol #const XK_Arabic_7
-- #define XK_Arabic_8                   0x1000668  /* U+0668 ARABIC-INDIC DIGIT EIGHT */
pattern XK_Arabic_8 = MkKeySymbol #const XK_Arabic_8
-- #define XK_Arabic_9                   0x1000669  /* U+0669 ARABIC-INDIC DIGIT NINE */
pattern XK_Arabic_9 = MkKeySymbol #const XK_Arabic_9
-- #define XK_Arabic_semicolon              0x05bb  /* U+061B ARABIC SEMICOLON */
pattern XK_Arabic_semicolon = MkKeySymbol #const XK_Arabic_semicolon
-- #define XK_Arabic_question_mark          0x05bf  /* U+061F ARABIC QUESTION MARK */
pattern XK_Arabic_question_mark = MkKeySymbol #const XK_Arabic_question_mark
-- #define XK_Arabic_hamza                  0x05c1  /* U+0621 ARABIC LETTER HAMZA */
pattern XK_Arabic_hamza = MkKeySymbol #const XK_Arabic_hamza
-- #define XK_Arabic_maddaonalef            0x05c2  /* U+0622 ARABIC LETTER ALEF WITH MADDA ABOVE */
pattern XK_Arabic_maddaonalef = MkKeySymbol #const XK_Arabic_maddaonalef
-- #define XK_Arabic_hamzaonalef            0x05c3  /* U+0623 ARABIC LETTER ALEF WITH HAMZA ABOVE */
pattern XK_Arabic_hamzaonalef = MkKeySymbol #const XK_Arabic_hamzaonalef
-- #define XK_Arabic_hamzaonwaw             0x05c4  /* U+0624 ARABIC LETTER WAW WITH HAMZA ABOVE */
pattern XK_Arabic_hamzaonwaw = MkKeySymbol #const XK_Arabic_hamzaonwaw
-- #define XK_Arabic_hamzaunderalef         0x05c5  /* U+0625 ARABIC LETTER ALEF WITH HAMZA BELOW */
pattern XK_Arabic_hamzaunderalef = MkKeySymbol #const XK_Arabic_hamzaunderalef
-- #define XK_Arabic_hamzaonyeh             0x05c6  /* U+0626 ARABIC LETTER YEH WITH HAMZA ABOVE */
pattern XK_Arabic_hamzaonyeh = MkKeySymbol #const XK_Arabic_hamzaonyeh
-- #define XK_Arabic_alef                   0x05c7  /* U+0627 ARABIC LETTER ALEF */
pattern XK_Arabic_alef = MkKeySymbol #const XK_Arabic_alef
-- #define XK_Arabic_beh                    0x05c8  /* U+0628 ARABIC LETTER BEH */
pattern XK_Arabic_beh = MkKeySymbol #const XK_Arabic_beh
-- #define XK_Arabic_tehmarbuta             0x05c9  /* U+0629 ARABIC LETTER TEH MARBUTA */
pattern XK_Arabic_tehmarbuta = MkKeySymbol #const XK_Arabic_tehmarbuta
-- #define XK_Arabic_teh                    0x05ca  /* U+062A ARABIC LETTER TEH */
pattern XK_Arabic_teh = MkKeySymbol #const XK_Arabic_teh
-- #define XK_Arabic_theh                   0x05cb  /* U+062B ARABIC LETTER THEH */
pattern XK_Arabic_theh = MkKeySymbol #const XK_Arabic_theh
-- #define XK_Arabic_jeem                   0x05cc  /* U+062C ARABIC LETTER JEEM */
pattern XK_Arabic_jeem = MkKeySymbol #const XK_Arabic_jeem
-- #define XK_Arabic_hah                    0x05cd  /* U+062D ARABIC LETTER HAH */
pattern XK_Arabic_hah = MkKeySymbol #const XK_Arabic_hah
-- #define XK_Arabic_khah                   0x05ce  /* U+062E ARABIC LETTER KHAH */
pattern XK_Arabic_khah = MkKeySymbol #const XK_Arabic_khah
-- #define XK_Arabic_dal                    0x05cf  /* U+062F ARABIC LETTER DAL */
pattern XK_Arabic_dal = MkKeySymbol #const XK_Arabic_dal
-- #define XK_Arabic_thal                   0x05d0  /* U+0630 ARABIC LETTER THAL */
pattern XK_Arabic_thal = MkKeySymbol #const XK_Arabic_thal
-- #define XK_Arabic_ra                     0x05d1  /* U+0631 ARABIC LETTER REH */
pattern XK_Arabic_ra = MkKeySymbol #const XK_Arabic_ra
-- #define XK_Arabic_zain                   0x05d2  /* U+0632 ARABIC LETTER ZAIN */
pattern XK_Arabic_zain = MkKeySymbol #const XK_Arabic_zain
-- #define XK_Arabic_seen                   0x05d3  /* U+0633 ARABIC LETTER SEEN */
pattern XK_Arabic_seen = MkKeySymbol #const XK_Arabic_seen
-- #define XK_Arabic_sheen                  0x05d4  /* U+0634 ARABIC LETTER SHEEN */
pattern XK_Arabic_sheen = MkKeySymbol #const XK_Arabic_sheen
-- #define XK_Arabic_sad                    0x05d5  /* U+0635 ARABIC LETTER SAD */
pattern XK_Arabic_sad = MkKeySymbol #const XK_Arabic_sad
-- #define XK_Arabic_dad                    0x05d6  /* U+0636 ARABIC LETTER DAD */
pattern XK_Arabic_dad = MkKeySymbol #const XK_Arabic_dad
-- #define XK_Arabic_tah                    0x05d7  /* U+0637 ARABIC LETTER TAH */
pattern XK_Arabic_tah = MkKeySymbol #const XK_Arabic_tah
-- #define XK_Arabic_zah                    0x05d8  /* U+0638 ARABIC LETTER ZAH */
pattern XK_Arabic_zah = MkKeySymbol #const XK_Arabic_zah
-- #define XK_Arabic_ain                    0x05d9  /* U+0639 ARABIC LETTER AIN */
pattern XK_Arabic_ain = MkKeySymbol #const XK_Arabic_ain
-- #define XK_Arabic_ghain                  0x05da  /* U+063A ARABIC LETTER GHAIN */
pattern XK_Arabic_ghain = MkKeySymbol #const XK_Arabic_ghain
-- #define XK_Arabic_tatweel                0x05e0  /* U+0640 ARABIC TATWEEL */
pattern XK_Arabic_tatweel = MkKeySymbol #const XK_Arabic_tatweel
-- #define XK_Arabic_feh                    0x05e1  /* U+0641 ARABIC LETTER FEH */
pattern XK_Arabic_feh = MkKeySymbol #const XK_Arabic_feh
-- #define XK_Arabic_qaf                    0x05e2  /* U+0642 ARABIC LETTER QAF */
pattern XK_Arabic_qaf = MkKeySymbol #const XK_Arabic_qaf
-- #define XK_Arabic_kaf                    0x05e3  /* U+0643 ARABIC LETTER KAF */
pattern XK_Arabic_kaf = MkKeySymbol #const XK_Arabic_kaf
-- #define XK_Arabic_lam                    0x05e4  /* U+0644 ARABIC LETTER LAM */
pattern XK_Arabic_lam = MkKeySymbol #const XK_Arabic_lam
-- #define XK_Arabic_meem                   0x05e5  /* U+0645 ARABIC LETTER MEEM */
pattern XK_Arabic_meem = MkKeySymbol #const XK_Arabic_meem
-- #define XK_Arabic_noon                   0x05e6  /* U+0646 ARABIC LETTER NOON */
pattern XK_Arabic_noon = MkKeySymbol #const XK_Arabic_noon
-- #define XK_Arabic_ha                     0x05e7  /* U+0647 ARABIC LETTER HEH */
pattern XK_Arabic_ha = MkKeySymbol #const XK_Arabic_ha
-- #define XK_Arabic_heh                    0x05e7  /* deprecated */
pattern XK_Arabic_heh = MkKeySymbol #const XK_Arabic_heh
-- #define XK_Arabic_waw                    0x05e8  /* U+0648 ARABIC LETTER WAW */
pattern XK_Arabic_waw = MkKeySymbol #const XK_Arabic_waw
-- #define XK_Arabic_alefmaksura            0x05e9  /* U+0649 ARABIC LETTER ALEF MAKSURA */
pattern XK_Arabic_alefmaksura = MkKeySymbol #const XK_Arabic_alefmaksura
-- #define XK_Arabic_yeh                    0x05ea  /* U+064A ARABIC LETTER YEH */
pattern XK_Arabic_yeh = MkKeySymbol #const XK_Arabic_yeh
-- #define XK_Arabic_fathatan               0x05eb  /* U+064B ARABIC FATHATAN */
pattern XK_Arabic_fathatan = MkKeySymbol #const XK_Arabic_fathatan
-- #define XK_Arabic_dammatan               0x05ec  /* U+064C ARABIC DAMMATAN */
pattern XK_Arabic_dammatan = MkKeySymbol #const XK_Arabic_dammatan
-- #define XK_Arabic_kasratan               0x05ed  /* U+064D ARABIC KASRATAN */
pattern XK_Arabic_kasratan = MkKeySymbol #const XK_Arabic_kasratan
-- #define XK_Arabic_fatha                  0x05ee  /* U+064E ARABIC FATHA */
pattern XK_Arabic_fatha = MkKeySymbol #const XK_Arabic_fatha
-- #define XK_Arabic_damma                  0x05ef  /* U+064F ARABIC DAMMA */
pattern XK_Arabic_damma = MkKeySymbol #const XK_Arabic_damma
-- #define XK_Arabic_kasra                  0x05f0  /* U+0650 ARABIC KASRA */
pattern XK_Arabic_kasra = MkKeySymbol #const XK_Arabic_kasra
-- #define XK_Arabic_shadda                 0x05f1  /* U+0651 ARABIC SHADDA */
pattern XK_Arabic_shadda = MkKeySymbol #const XK_Arabic_shadda
-- #define XK_Arabic_sukun                  0x05f2  /* U+0652 ARABIC SUKUN */
pattern XK_Arabic_sukun = MkKeySymbol #const XK_Arabic_sukun
-- #define XK_Arabic_madda_above         0x1000653  /* U+0653 ARABIC MADDAH ABOVE */
pattern XK_Arabic_madda_above = MkKeySymbol #const XK_Arabic_madda_above
-- #define XK_Arabic_hamza_above         0x1000654  /* U+0654 ARABIC HAMZA ABOVE */
pattern XK_Arabic_hamza_above = MkKeySymbol #const XK_Arabic_hamza_above
-- #define XK_Arabic_hamza_below         0x1000655  /* U+0655 ARABIC HAMZA BELOW */
pattern XK_Arabic_hamza_below = MkKeySymbol #const XK_Arabic_hamza_below
-- #define XK_Arabic_jeh                 0x1000698  /* U+0698 ARABIC LETTER JEH */
pattern XK_Arabic_jeh = MkKeySymbol #const XK_Arabic_jeh
-- #define XK_Arabic_veh                 0x10006a4  /* U+06A4 ARABIC LETTER VEH */
pattern XK_Arabic_veh = MkKeySymbol #const XK_Arabic_veh
-- #define XK_Arabic_keheh               0x10006a9  /* U+06A9 ARABIC LETTER KEHEH */
pattern XK_Arabic_keheh = MkKeySymbol #const XK_Arabic_keheh
-- #define XK_Arabic_gaf                 0x10006af  /* U+06AF ARABIC LETTER GAF */
pattern XK_Arabic_gaf = MkKeySymbol #const XK_Arabic_gaf
-- #define XK_Arabic_noon_ghunna         0x10006ba  /* U+06BA ARABIC LETTER NOON GHUNNA */
pattern XK_Arabic_noon_ghunna = MkKeySymbol #const XK_Arabic_noon_ghunna
-- #define XK_Arabic_heh_doachashmee     0x10006be  /* U+06BE ARABIC LETTER HEH DOACHASHMEE */
pattern XK_Arabic_heh_doachashmee = MkKeySymbol #const XK_Arabic_heh_doachashmee
-- #define XK_Farsi_yeh                  0x10006cc  /* U+06CC ARABIC LETTER FARSI YEH */
pattern XK_Farsi_yeh = MkKeySymbol #const XK_Farsi_yeh
-- #define XK_Arabic_farsi_yeh           0x10006cc  /* U+06CC ARABIC LETTER FARSI YEH */
pattern XK_Arabic_farsi_yeh = MkKeySymbol #const XK_Arabic_farsi_yeh
-- #define XK_Arabic_yeh_baree           0x10006d2  /* U+06D2 ARABIC LETTER YEH BARREE */
pattern XK_Arabic_yeh_baree = MkKeySymbol #const XK_Arabic_yeh_baree
-- #define XK_Arabic_heh_goal            0x10006c1  /* U+06C1 ARABIC LETTER HEH GOAL */
pattern XK_Arabic_heh_goal = MkKeySymbol #const XK_Arabic_heh_goal
-- #define XK_Arabic_switch                 0xff7e  /* Alias for mode_switch */
pattern XK_Arabic_switch = MkKeySymbol #const XK_Arabic_switch
-- #define XK_Cyrillic_GHE_bar           0x1000492  /* U+0492 CYRILLIC CAPITAL LETTER GHE WITH STROKE */
pattern XK_Cyrillic_GHE_bar = MkKeySymbol #const XK_Cyrillic_GHE_bar
-- #define XK_Cyrillic_ghe_bar           0x1000493  /* U+0493 CYRILLIC SMALL LETTER GHE WITH STROKE */
pattern XK_Cyrillic_ghe_bar = MkKeySymbol #const XK_Cyrillic_ghe_bar
-- #define XK_Cyrillic_ZHE_descender     0x1000496  /* U+0496 CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER */
pattern XK_Cyrillic_ZHE_descender = MkKeySymbol #const XK_Cyrillic_ZHE_descender
-- #define XK_Cyrillic_zhe_descender     0x1000497  /* U+0497 CYRILLIC SMALL LETTER ZHE WITH DESCENDER */
pattern XK_Cyrillic_zhe_descender = MkKeySymbol #const XK_Cyrillic_zhe_descender
-- #define XK_Cyrillic_KA_descender      0x100049a  /* U+049A CYRILLIC CAPITAL LETTER KA WITH DESCENDER */
pattern XK_Cyrillic_KA_descender = MkKeySymbol #const XK_Cyrillic_KA_descender
-- #define XK_Cyrillic_ka_descender      0x100049b  /* U+049B CYRILLIC SMALL LETTER KA WITH DESCENDER */
pattern XK_Cyrillic_ka_descender = MkKeySymbol #const XK_Cyrillic_ka_descender
-- #define XK_Cyrillic_KA_vertstroke     0x100049c  /* U+049C CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE */
pattern XK_Cyrillic_KA_vertstroke = MkKeySymbol #const XK_Cyrillic_KA_vertstroke
-- #define XK_Cyrillic_ka_vertstroke     0x100049d  /* U+049D CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE */
pattern XK_Cyrillic_ka_vertstroke = MkKeySymbol #const XK_Cyrillic_ka_vertstroke
-- #define XK_Cyrillic_EN_descender      0x10004a2  /* U+04A2 CYRILLIC CAPITAL LETTER EN WITH DESCENDER */
pattern XK_Cyrillic_EN_descender = MkKeySymbol #const XK_Cyrillic_EN_descender
-- #define XK_Cyrillic_en_descender      0x10004a3  /* U+04A3 CYRILLIC SMALL LETTER EN WITH DESCENDER */
pattern XK_Cyrillic_en_descender = MkKeySymbol #const XK_Cyrillic_en_descender
-- #define XK_Cyrillic_U_straight        0x10004ae  /* U+04AE CYRILLIC CAPITAL LETTER STRAIGHT U */
pattern XK_Cyrillic_U_straight = MkKeySymbol #const XK_Cyrillic_U_straight
-- #define XK_Cyrillic_u_straight        0x10004af  /* U+04AF CYRILLIC SMALL LETTER STRAIGHT U */
pattern XK_Cyrillic_u_straight = MkKeySymbol #const XK_Cyrillic_u_straight
-- #define XK_Cyrillic_U_straight_bar    0x10004b0  /* U+04B0 CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE */
pattern XK_Cyrillic_U_straight_bar = MkKeySymbol #const XK_Cyrillic_U_straight_bar
-- #define XK_Cyrillic_u_straight_bar    0x10004b1  /* U+04B1 CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE */
pattern XK_Cyrillic_u_straight_bar = MkKeySymbol #const XK_Cyrillic_u_straight_bar
-- #define XK_Cyrillic_HA_descender      0x10004b2  /* U+04B2 CYRILLIC CAPITAL LETTER HA WITH DESCENDER */
pattern XK_Cyrillic_HA_descender = MkKeySymbol #const XK_Cyrillic_HA_descender
-- #define XK_Cyrillic_ha_descender      0x10004b3  /* U+04B3 CYRILLIC SMALL LETTER HA WITH DESCENDER */
pattern XK_Cyrillic_ha_descender = MkKeySymbol #const XK_Cyrillic_ha_descender
-- #define XK_Cyrillic_CHE_descender     0x10004b6  /* U+04B6 CYRILLIC CAPITAL LETTER CHE WITH DESCENDER */
pattern XK_Cyrillic_CHE_descender = MkKeySymbol #const XK_Cyrillic_CHE_descender
-- #define XK_Cyrillic_che_descender     0x10004b7  /* U+04B7 CYRILLIC SMALL LETTER CHE WITH DESCENDER */
pattern XK_Cyrillic_che_descender = MkKeySymbol #const XK_Cyrillic_che_descender
-- #define XK_Cyrillic_CHE_vertstroke    0x10004b8  /* U+04B8 CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE */
pattern XK_Cyrillic_CHE_vertstroke = MkKeySymbol #const XK_Cyrillic_CHE_vertstroke
-- #define XK_Cyrillic_che_vertstroke    0x10004b9  /* U+04B9 CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE */
pattern XK_Cyrillic_che_vertstroke = MkKeySymbol #const XK_Cyrillic_che_vertstroke
-- #define XK_Cyrillic_SHHA              0x10004ba  /* U+04BA CYRILLIC CAPITAL LETTER SHHA */
pattern XK_Cyrillic_SHHA = MkKeySymbol #const XK_Cyrillic_SHHA
-- #define XK_Cyrillic_shha              0x10004bb  /* U+04BB CYRILLIC SMALL LETTER SHHA */
pattern XK_Cyrillic_shha = MkKeySymbol #const XK_Cyrillic_shha
-- #define XK_Cyrillic_SCHWA             0x10004d8  /* U+04D8 CYRILLIC CAPITAL LETTER SCHWA */
pattern XK_Cyrillic_SCHWA = MkKeySymbol #const XK_Cyrillic_SCHWA
-- #define XK_Cyrillic_schwa             0x10004d9  /* U+04D9 CYRILLIC SMALL LETTER SCHWA */
pattern XK_Cyrillic_schwa = MkKeySymbol #const XK_Cyrillic_schwa
-- #define XK_Cyrillic_I_macron          0x10004e2  /* U+04E2 CYRILLIC CAPITAL LETTER I WITH MACRON */
pattern XK_Cyrillic_I_macron = MkKeySymbol #const XK_Cyrillic_I_macron
-- #define XK_Cyrillic_i_macron          0x10004e3  /* U+04E3 CYRILLIC SMALL LETTER I WITH MACRON */
pattern XK_Cyrillic_i_macron = MkKeySymbol #const XK_Cyrillic_i_macron
-- #define XK_Cyrillic_O_bar             0x10004e8  /* U+04E8 CYRILLIC CAPITAL LETTER BARRED O */
pattern XK_Cyrillic_O_bar = MkKeySymbol #const XK_Cyrillic_O_bar
-- #define XK_Cyrillic_o_bar             0x10004e9  /* U+04E9 CYRILLIC SMALL LETTER BARRED O */
pattern XK_Cyrillic_o_bar = MkKeySymbol #const XK_Cyrillic_o_bar
-- #define XK_Cyrillic_U_macron          0x10004ee  /* U+04EE CYRILLIC CAPITAL LETTER U WITH MACRON */
pattern XK_Cyrillic_U_macron = MkKeySymbol #const XK_Cyrillic_U_macron
-- #define XK_Cyrillic_u_macron          0x10004ef  /* U+04EF CYRILLIC SMALL LETTER U WITH MACRON */
pattern XK_Cyrillic_u_macron = MkKeySymbol #const XK_Cyrillic_u_macron
-- #define XK_Serbian_dje                   0x06a1  /* U+0452 CYRILLIC SMALL LETTER DJE */
pattern XK_Serbian_dje = MkKeySymbol #const XK_Serbian_dje
-- #define XK_Macedonia_gje                 0x06a2  /* U+0453 CYRILLIC SMALL LETTER GJE */
pattern XK_Macedonia_gje = MkKeySymbol #const XK_Macedonia_gje
-- #define XK_Cyrillic_io                   0x06a3  /* U+0451 CYRILLIC SMALL LETTER IO */
pattern XK_Cyrillic_io = MkKeySymbol #const XK_Cyrillic_io
-- #define XK_Ukrainian_ie                  0x06a4  /* U+0454 CYRILLIC SMALL LETTER UKRAINIAN IE */
pattern XK_Ukrainian_ie = MkKeySymbol #const XK_Ukrainian_ie
-- #define XK_Ukranian_je                   0x06a4  /* deprecated */
pattern XK_Ukranian_je = MkKeySymbol #const XK_Ukranian_je
-- #define XK_Macedonia_dse                 0x06a5  /* U+0455 CYRILLIC SMALL LETTER DZE */
pattern XK_Macedonia_dse = MkKeySymbol #const XK_Macedonia_dse
-- #define XK_Ukrainian_i                   0x06a6  /* U+0456 CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I */
pattern XK_Ukrainian_i = MkKeySymbol #const XK_Ukrainian_i
-- #define XK_Ukranian_i                    0x06a6  /* deprecated */
pattern XK_Ukranian_i = MkKeySymbol #const XK_Ukranian_i
-- #define XK_Ukrainian_yi                  0x06a7  /* U+0457 CYRILLIC SMALL LETTER YI */
pattern XK_Ukrainian_yi = MkKeySymbol #const XK_Ukrainian_yi
-- #define XK_Ukranian_yi                   0x06a7  /* deprecated */
pattern XK_Ukranian_yi = MkKeySymbol #const XK_Ukranian_yi
-- #define XK_Cyrillic_je                   0x06a8  /* U+0458 CYRILLIC SMALL LETTER JE */
pattern XK_Cyrillic_je = MkKeySymbol #const XK_Cyrillic_je
-- #define XK_Serbian_je                    0x06a8  /* deprecated */
pattern XK_Serbian_je = MkKeySymbol #const XK_Serbian_je
-- #define XK_Cyrillic_lje                  0x06a9  /* U+0459 CYRILLIC SMALL LETTER LJE */
pattern XK_Cyrillic_lje = MkKeySymbol #const XK_Cyrillic_lje
-- #define XK_Serbian_lje                   0x06a9  /* deprecated */
pattern XK_Serbian_lje = MkKeySymbol #const XK_Serbian_lje
-- #define XK_Cyrillic_nje                  0x06aa  /* U+045A CYRILLIC SMALL LETTER NJE */
pattern XK_Cyrillic_nje = MkKeySymbol #const XK_Cyrillic_nje
-- #define XK_Serbian_nje                   0x06aa  /* deprecated */
pattern XK_Serbian_nje = MkKeySymbol #const XK_Serbian_nje
-- #define XK_Serbian_tshe                  0x06ab  /* U+045B CYRILLIC SMALL LETTER TSHE */
pattern XK_Serbian_tshe = MkKeySymbol #const XK_Serbian_tshe
-- #define XK_Macedonia_kje                 0x06ac  /* U+045C CYRILLIC SMALL LETTER KJE */
pattern XK_Macedonia_kje = MkKeySymbol #const XK_Macedonia_kje
-- #define XK_Ukrainian_ghe_with_upturn     0x06ad  /* U+0491 CYRILLIC SMALL LETTER GHE WITH UPTURN */
pattern XK_Ukrainian_ghe_with_upturn = MkKeySymbol #const XK_Ukrainian_ghe_with_upturn
-- #define XK_Byelorussian_shortu           0x06ae  /* U+045E CYRILLIC SMALL LETTER SHORT U */
pattern XK_Byelorussian_shortu = MkKeySymbol #const XK_Byelorussian_shortu
-- #define XK_Cyrillic_dzhe                 0x06af  /* U+045F CYRILLIC SMALL LETTER DZHE */
pattern XK_Cyrillic_dzhe = MkKeySymbol #const XK_Cyrillic_dzhe
-- #define XK_Serbian_dze                   0x06af  /* deprecated */
pattern XK_Serbian_dze = MkKeySymbol #const XK_Serbian_dze
-- #define XK_numerosign                    0x06b0  /* U+2116 NUMERO SIGN */
pattern XK_numerosign = MkKeySymbol #const XK_numerosign
-- #define XK_Serbian_DJE                   0x06b1  /* U+0402 CYRILLIC CAPITAL LETTER DJE */
pattern XK_Serbian_DJE = MkKeySymbol #const XK_Serbian_DJE
-- #define XK_Macedonia_GJE                 0x06b2  /* U+0403 CYRILLIC CAPITAL LETTER GJE */
pattern XK_Macedonia_GJE = MkKeySymbol #const XK_Macedonia_GJE
-- #define XK_Cyrillic_IO                   0x06b3  /* U+0401 CYRILLIC CAPITAL LETTER IO */
pattern XK_Cyrillic_IO = MkKeySymbol #const XK_Cyrillic_IO
-- #define XK_Ukrainian_IE                  0x06b4  /* U+0404 CYRILLIC CAPITAL LETTER UKRAINIAN IE */
pattern XK_Ukrainian_IE = MkKeySymbol #const XK_Ukrainian_IE
-- #define XK_Ukranian_JE                   0x06b4  /* deprecated */
pattern XK_Ukranian_JE = MkKeySymbol #const XK_Ukranian_JE
-- #define XK_Macedonia_DSE                 0x06b5  /* U+0405 CYRILLIC CAPITAL LETTER DZE */
pattern XK_Macedonia_DSE = MkKeySymbol #const XK_Macedonia_DSE
-- #define XK_Ukrainian_I                   0x06b6  /* U+0406 CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I */
pattern XK_Ukrainian_I = MkKeySymbol #const XK_Ukrainian_I
-- #define XK_Ukranian_I                    0x06b6  /* deprecated */
pattern XK_Ukranian_I = MkKeySymbol #const XK_Ukranian_I
-- #define XK_Ukrainian_YI                  0x06b7  /* U+0407 CYRILLIC CAPITAL LETTER YI */
pattern XK_Ukrainian_YI = MkKeySymbol #const XK_Ukrainian_YI
-- #define XK_Ukranian_YI                   0x06b7  /* deprecated */
pattern XK_Ukranian_YI = MkKeySymbol #const XK_Ukranian_YI
-- #define XK_Cyrillic_JE                   0x06b8  /* U+0408 CYRILLIC CAPITAL LETTER JE */
pattern XK_Cyrillic_JE = MkKeySymbol #const XK_Cyrillic_JE
-- #define XK_Serbian_JE                    0x06b8  /* deprecated */
pattern XK_Serbian_JE = MkKeySymbol #const XK_Serbian_JE
-- #define XK_Cyrillic_LJE                  0x06b9  /* U+0409 CYRILLIC CAPITAL LETTER LJE */
pattern XK_Cyrillic_LJE = MkKeySymbol #const XK_Cyrillic_LJE
-- #define XK_Serbian_LJE                   0x06b9  /* deprecated */
pattern XK_Serbian_LJE = MkKeySymbol #const XK_Serbian_LJE
-- #define XK_Cyrillic_NJE                  0x06ba  /* U+040A CYRILLIC CAPITAL LETTER NJE */
pattern XK_Cyrillic_NJE = MkKeySymbol #const XK_Cyrillic_NJE
-- #define XK_Serbian_NJE                   0x06ba  /* deprecated */
pattern XK_Serbian_NJE = MkKeySymbol #const XK_Serbian_NJE
-- #define XK_Serbian_TSHE                  0x06bb  /* U+040B CYRILLIC CAPITAL LETTER TSHE */
pattern XK_Serbian_TSHE = MkKeySymbol #const XK_Serbian_TSHE
-- #define XK_Macedonia_KJE                 0x06bc  /* U+040C CYRILLIC CAPITAL LETTER KJE */
pattern XK_Macedonia_KJE = MkKeySymbol #const XK_Macedonia_KJE
-- #define XK_Ukrainian_GHE_WITH_UPTURN     0x06bd  /* U+0490 CYRILLIC CAPITAL LETTER GHE WITH UPTURN */
pattern XK_Ukrainian_GHE_WITH_UPTURN = MkKeySymbol #const XK_Ukrainian_GHE_WITH_UPTURN
-- #define XK_Byelorussian_SHORTU           0x06be  /* U+040E CYRILLIC CAPITAL LETTER SHORT U */
pattern XK_Byelorussian_SHORTU = MkKeySymbol #const XK_Byelorussian_SHORTU
-- #define XK_Cyrillic_DZHE                 0x06bf  /* U+040F CYRILLIC CAPITAL LETTER DZHE */
pattern XK_Cyrillic_DZHE = MkKeySymbol #const XK_Cyrillic_DZHE
-- #define XK_Serbian_DZE                   0x06bf  /* deprecated */
pattern XK_Serbian_DZE = MkKeySymbol #const XK_Serbian_DZE
-- #define XK_Cyrillic_yu                   0x06c0  /* U+044E CYRILLIC SMALL LETTER YU */
pattern XK_Cyrillic_yu = MkKeySymbol #const XK_Cyrillic_yu
-- #define XK_Cyrillic_a                    0x06c1  /* U+0430 CYRILLIC SMALL LETTER A */
pattern XK_Cyrillic_a = MkKeySymbol #const XK_Cyrillic_a
-- #define XK_Cyrillic_be                   0x06c2  /* U+0431 CYRILLIC SMALL LETTER BE */
pattern XK_Cyrillic_be = MkKeySymbol #const XK_Cyrillic_be
-- #define XK_Cyrillic_tse                  0x06c3  /* U+0446 CYRILLIC SMALL LETTER TSE */
pattern XK_Cyrillic_tse = MkKeySymbol #const XK_Cyrillic_tse
-- #define XK_Cyrillic_de                   0x06c4  /* U+0434 CYRILLIC SMALL LETTER DE */
pattern XK_Cyrillic_de = MkKeySymbol #const XK_Cyrillic_de
-- #define XK_Cyrillic_ie                   0x06c5  /* U+0435 CYRILLIC SMALL LETTER IE */
pattern XK_Cyrillic_ie = MkKeySymbol #const XK_Cyrillic_ie
-- #define XK_Cyrillic_ef                   0x06c6  /* U+0444 CYRILLIC SMALL LETTER EF */
pattern XK_Cyrillic_ef = MkKeySymbol #const XK_Cyrillic_ef
-- #define XK_Cyrillic_ghe                  0x06c7  /* U+0433 CYRILLIC SMALL LETTER GHE */
pattern XK_Cyrillic_ghe = MkKeySymbol #const XK_Cyrillic_ghe
-- #define XK_Cyrillic_ha                   0x06c8  /* U+0445 CYRILLIC SMALL LETTER HA */
pattern XK_Cyrillic_ha = MkKeySymbol #const XK_Cyrillic_ha
-- #define XK_Cyrillic_i                    0x06c9  /* U+0438 CYRILLIC SMALL LETTER I */
pattern XK_Cyrillic_i = MkKeySymbol #const XK_Cyrillic_i
-- #define XK_Cyrillic_shorti               0x06ca  /* U+0439 CYRILLIC SMALL LETTER SHORT I */
pattern XK_Cyrillic_shorti = MkKeySymbol #const XK_Cyrillic_shorti
-- #define XK_Cyrillic_ka                   0x06cb  /* U+043A CYRILLIC SMALL LETTER KA */
pattern XK_Cyrillic_ka = MkKeySymbol #const XK_Cyrillic_ka
-- #define XK_Cyrillic_el                   0x06cc  /* U+043B CYRILLIC SMALL LETTER EL */
pattern XK_Cyrillic_el = MkKeySymbol #const XK_Cyrillic_el
-- #define XK_Cyrillic_em                   0x06cd  /* U+043C CYRILLIC SMALL LETTER EM */
pattern XK_Cyrillic_em = MkKeySymbol #const XK_Cyrillic_em
-- #define XK_Cyrillic_en                   0x06ce  /* U+043D CYRILLIC SMALL LETTER EN */
pattern XK_Cyrillic_en = MkKeySymbol #const XK_Cyrillic_en
-- #define XK_Cyrillic_o                    0x06cf  /* U+043E CYRILLIC SMALL LETTER O */
pattern XK_Cyrillic_o = MkKeySymbol #const XK_Cyrillic_o
-- #define XK_Cyrillic_pe                   0x06d0  /* U+043F CYRILLIC SMALL LETTER PE */
pattern XK_Cyrillic_pe = MkKeySymbol #const XK_Cyrillic_pe
-- #define XK_Cyrillic_ya                   0x06d1  /* U+044F CYRILLIC SMALL LETTER YA */
pattern XK_Cyrillic_ya = MkKeySymbol #const XK_Cyrillic_ya
-- #define XK_Cyrillic_er                   0x06d2  /* U+0440 CYRILLIC SMALL LETTER ER */
pattern XK_Cyrillic_er = MkKeySymbol #const XK_Cyrillic_er
-- #define XK_Cyrillic_es                   0x06d3  /* U+0441 CYRILLIC SMALL LETTER ES */
pattern XK_Cyrillic_es = MkKeySymbol #const XK_Cyrillic_es
-- #define XK_Cyrillic_te                   0x06d4  /* U+0442 CYRILLIC SMALL LETTER TE */
pattern XK_Cyrillic_te = MkKeySymbol #const XK_Cyrillic_te
-- #define XK_Cyrillic_u                    0x06d5  /* U+0443 CYRILLIC SMALL LETTER U */
pattern XK_Cyrillic_u = MkKeySymbol #const XK_Cyrillic_u
-- #define XK_Cyrillic_zhe                  0x06d6  /* U+0436 CYRILLIC SMALL LETTER ZHE */
pattern XK_Cyrillic_zhe = MkKeySymbol #const XK_Cyrillic_zhe
-- #define XK_Cyrillic_ve                   0x06d7  /* U+0432 CYRILLIC SMALL LETTER VE */
pattern XK_Cyrillic_ve = MkKeySymbol #const XK_Cyrillic_ve
-- #define XK_Cyrillic_softsign             0x06d8  /* U+044C CYRILLIC SMALL LETTER SOFT SIGN */
pattern XK_Cyrillic_softsign = MkKeySymbol #const XK_Cyrillic_softsign
-- #define XK_Cyrillic_yeru                 0x06d9  /* U+044B CYRILLIC SMALL LETTER YERU */
pattern XK_Cyrillic_yeru = MkKeySymbol #const XK_Cyrillic_yeru
-- #define XK_Cyrillic_ze                   0x06da  /* U+0437 CYRILLIC SMALL LETTER ZE */
pattern XK_Cyrillic_ze = MkKeySymbol #const XK_Cyrillic_ze
-- #define XK_Cyrillic_sha                  0x06db  /* U+0448 CYRILLIC SMALL LETTER SHA */
pattern XK_Cyrillic_sha = MkKeySymbol #const XK_Cyrillic_sha
-- #define XK_Cyrillic_e                    0x06dc  /* U+044D CYRILLIC SMALL LETTER E */
pattern XK_Cyrillic_e = MkKeySymbol #const XK_Cyrillic_e
-- #define XK_Cyrillic_shcha                0x06dd  /* U+0449 CYRILLIC SMALL LETTER SHCHA */
pattern XK_Cyrillic_shcha = MkKeySymbol #const XK_Cyrillic_shcha
-- #define XK_Cyrillic_che                  0x06de  /* U+0447 CYRILLIC SMALL LETTER CHE */
pattern XK_Cyrillic_che = MkKeySymbol #const XK_Cyrillic_che
-- #define XK_Cyrillic_hardsign             0x06df  /* U+044A CYRILLIC SMALL LETTER HARD SIGN */
pattern XK_Cyrillic_hardsign = MkKeySymbol #const XK_Cyrillic_hardsign
-- #define XK_Cyrillic_YU                   0x06e0  /* U+042E CYRILLIC CAPITAL LETTER YU */
pattern XK_Cyrillic_YU = MkKeySymbol #const XK_Cyrillic_YU
-- #define XK_Cyrillic_A                    0x06e1  /* U+0410 CYRILLIC CAPITAL LETTER A */
pattern XK_Cyrillic_A = MkKeySymbol #const XK_Cyrillic_A
-- #define XK_Cyrillic_BE                   0x06e2  /* U+0411 CYRILLIC CAPITAL LETTER BE */
pattern XK_Cyrillic_BE = MkKeySymbol #const XK_Cyrillic_BE
-- #define XK_Cyrillic_TSE                  0x06e3  /* U+0426 CYRILLIC CAPITAL LETTER TSE */
pattern XK_Cyrillic_TSE = MkKeySymbol #const XK_Cyrillic_TSE
-- #define XK_Cyrillic_DE                   0x06e4  /* U+0414 CYRILLIC CAPITAL LETTER DE */
pattern XK_Cyrillic_DE = MkKeySymbol #const XK_Cyrillic_DE
-- #define XK_Cyrillic_IE                   0x06e5  /* U+0415 CYRILLIC CAPITAL LETTER IE */
pattern XK_Cyrillic_IE = MkKeySymbol #const XK_Cyrillic_IE
-- #define XK_Cyrillic_EF                   0x06e6  /* U+0424 CYRILLIC CAPITAL LETTER EF */
pattern XK_Cyrillic_EF = MkKeySymbol #const XK_Cyrillic_EF
-- #define XK_Cyrillic_GHE                  0x06e7  /* U+0413 CYRILLIC CAPITAL LETTER GHE */
pattern XK_Cyrillic_GHE = MkKeySymbol #const XK_Cyrillic_GHE
-- #define XK_Cyrillic_HA                   0x06e8  /* U+0425 CYRILLIC CAPITAL LETTER HA */
pattern XK_Cyrillic_HA = MkKeySymbol #const XK_Cyrillic_HA
-- #define XK_Cyrillic_I                    0x06e9  /* U+0418 CYRILLIC CAPITAL LETTER I */
pattern XK_Cyrillic_I = MkKeySymbol #const XK_Cyrillic_I
-- #define XK_Cyrillic_SHORTI               0x06ea  /* U+0419 CYRILLIC CAPITAL LETTER SHORT I */
pattern XK_Cyrillic_SHORTI = MkKeySymbol #const XK_Cyrillic_SHORTI
-- #define XK_Cyrillic_KA                   0x06eb  /* U+041A CYRILLIC CAPITAL LETTER KA */
pattern XK_Cyrillic_KA = MkKeySymbol #const XK_Cyrillic_KA
-- #define XK_Cyrillic_EL                   0x06ec  /* U+041B CYRILLIC CAPITAL LETTER EL */
pattern XK_Cyrillic_EL = MkKeySymbol #const XK_Cyrillic_EL
-- #define XK_Cyrillic_EM                   0x06ed  /* U+041C CYRILLIC CAPITAL LETTER EM */
pattern XK_Cyrillic_EM = MkKeySymbol #const XK_Cyrillic_EM
-- #define XK_Cyrillic_EN                   0x06ee  /* U+041D CYRILLIC CAPITAL LETTER EN */
pattern XK_Cyrillic_EN = MkKeySymbol #const XK_Cyrillic_EN
-- #define XK_Cyrillic_O                    0x06ef  /* U+041E CYRILLIC CAPITAL LETTER O */
pattern XK_Cyrillic_O = MkKeySymbol #const XK_Cyrillic_O
-- #define XK_Cyrillic_PE                   0x06f0  /* U+041F CYRILLIC CAPITAL LETTER PE */
pattern XK_Cyrillic_PE = MkKeySymbol #const XK_Cyrillic_PE
-- #define XK_Cyrillic_YA                   0x06f1  /* U+042F CYRILLIC CAPITAL LETTER YA */
pattern XK_Cyrillic_YA = MkKeySymbol #const XK_Cyrillic_YA
-- #define XK_Cyrillic_ER                   0x06f2  /* U+0420 CYRILLIC CAPITAL LETTER ER */
pattern XK_Cyrillic_ER = MkKeySymbol #const XK_Cyrillic_ER
-- #define XK_Cyrillic_ES                   0x06f3  /* U+0421 CYRILLIC CAPITAL LETTER ES */
pattern XK_Cyrillic_ES = MkKeySymbol #const XK_Cyrillic_ES
-- #define XK_Cyrillic_TE                   0x06f4  /* U+0422 CYRILLIC CAPITAL LETTER TE */
pattern XK_Cyrillic_TE = MkKeySymbol #const XK_Cyrillic_TE
-- #define XK_Cyrillic_U                    0x06f5  /* U+0423 CYRILLIC CAPITAL LETTER U */
pattern XK_Cyrillic_U = MkKeySymbol #const XK_Cyrillic_U
-- #define XK_Cyrillic_ZHE                  0x06f6  /* U+0416 CYRILLIC CAPITAL LETTER ZHE */
pattern XK_Cyrillic_ZHE = MkKeySymbol #const XK_Cyrillic_ZHE
-- #define XK_Cyrillic_VE                   0x06f7  /* U+0412 CYRILLIC CAPITAL LETTER VE */
pattern XK_Cyrillic_VE = MkKeySymbol #const XK_Cyrillic_VE
-- #define XK_Cyrillic_SOFTSIGN             0x06f8  /* U+042C CYRILLIC CAPITAL LETTER SOFT SIGN */
pattern XK_Cyrillic_SOFTSIGN = MkKeySymbol #const XK_Cyrillic_SOFTSIGN
-- #define XK_Cyrillic_YERU                 0x06f9  /* U+042B CYRILLIC CAPITAL LETTER YERU */
pattern XK_Cyrillic_YERU = MkKeySymbol #const XK_Cyrillic_YERU
-- #define XK_Cyrillic_ZE                   0x06fa  /* U+0417 CYRILLIC CAPITAL LETTER ZE */
pattern XK_Cyrillic_ZE = MkKeySymbol #const XK_Cyrillic_ZE
-- #define XK_Cyrillic_SHA                  0x06fb  /* U+0428 CYRILLIC CAPITAL LETTER SHA */
pattern XK_Cyrillic_SHA = MkKeySymbol #const XK_Cyrillic_SHA
-- #define XK_Cyrillic_E                    0x06fc  /* U+042D CYRILLIC CAPITAL LETTER E */
pattern XK_Cyrillic_E = MkKeySymbol #const XK_Cyrillic_E
-- #define XK_Cyrillic_SHCHA                0x06fd  /* U+0429 CYRILLIC CAPITAL LETTER SHCHA */
pattern XK_Cyrillic_SHCHA = MkKeySymbol #const XK_Cyrillic_SHCHA
-- #define XK_Cyrillic_CHE                  0x06fe  /* U+0427 CYRILLIC CAPITAL LETTER CHE */
pattern XK_Cyrillic_CHE = MkKeySymbol #const XK_Cyrillic_CHE
-- #define XK_Cyrillic_HARDSIGN             0x06ff  /* U+042A CYRILLIC CAPITAL LETTER HARD SIGN */
pattern XK_Cyrillic_HARDSIGN = MkKeySymbol #const XK_Cyrillic_HARDSIGN
-- #define XK_Greek_ALPHAaccent             0x07a1  /* U+0386 GREEK CAPITAL LETTER ALPHA WITH TONOS */
pattern XK_Greek_ALPHAaccent = MkKeySymbol #const XK_Greek_ALPHAaccent
-- #define XK_Greek_EPSILONaccent           0x07a2  /* U+0388 GREEK CAPITAL LETTER EPSILON WITH TONOS */
pattern XK_Greek_EPSILONaccent = MkKeySymbol #const XK_Greek_EPSILONaccent
-- #define XK_Greek_ETAaccent               0x07a3  /* U+0389 GREEK CAPITAL LETTER ETA WITH TONOS */
pattern XK_Greek_ETAaccent = MkKeySymbol #const XK_Greek_ETAaccent
-- #define XK_Greek_IOTAaccent              0x07a4  /* U+038A GREEK CAPITAL LETTER IOTA WITH TONOS */
pattern XK_Greek_IOTAaccent = MkKeySymbol #const XK_Greek_IOTAaccent
-- #define XK_Greek_IOTAdieresis            0x07a5  /* U+03AA GREEK CAPITAL LETTER IOTA WITH DIALYTIKA */
pattern XK_Greek_IOTAdieresis = MkKeySymbol #const XK_Greek_IOTAdieresis
-- #define XK_Greek_IOTAdiaeresis           0x07a5  /* old typo */
pattern XK_Greek_IOTAdiaeresis = MkKeySymbol #const XK_Greek_IOTAdiaeresis
-- #define XK_Greek_OMICRONaccent           0x07a7  /* U+038C GREEK CAPITAL LETTER OMICRON WITH TONOS */
pattern XK_Greek_OMICRONaccent = MkKeySymbol #const XK_Greek_OMICRONaccent
-- #define XK_Greek_UPSILONaccent           0x07a8  /* U+038E GREEK CAPITAL LETTER UPSILON WITH TONOS */
pattern XK_Greek_UPSILONaccent = MkKeySymbol #const XK_Greek_UPSILONaccent
-- #define XK_Greek_UPSILONdieresis         0x07a9  /* U+03AB GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA */
pattern XK_Greek_UPSILONdieresis = MkKeySymbol #const XK_Greek_UPSILONdieresis
-- #define XK_Greek_OMEGAaccent             0x07ab  /* U+038F GREEK CAPITAL LETTER OMEGA WITH TONOS */
pattern XK_Greek_OMEGAaccent = MkKeySymbol #const XK_Greek_OMEGAaccent
-- #define XK_Greek_accentdieresis          0x07ae  /* U+0385 GREEK DIALYTIKA TONOS */
pattern XK_Greek_accentdieresis = MkKeySymbol #const XK_Greek_accentdieresis
-- #define XK_Greek_horizbar                0x07af  /* U+2015 HORIZONTAL BAR */
pattern XK_Greek_horizbar = MkKeySymbol #const XK_Greek_horizbar
-- #define XK_Greek_alphaaccent             0x07b1  /* U+03AC GREEK SMALL LETTER ALPHA WITH TONOS */
pattern XK_Greek_alphaaccent = MkKeySymbol #const XK_Greek_alphaaccent
-- #define XK_Greek_epsilonaccent           0x07b2  /* U+03AD GREEK SMALL LETTER EPSILON WITH TONOS */
pattern XK_Greek_epsilonaccent = MkKeySymbol #const XK_Greek_epsilonaccent
-- #define XK_Greek_etaaccent               0x07b3  /* U+03AE GREEK SMALL LETTER ETA WITH TONOS */
pattern XK_Greek_etaaccent = MkKeySymbol #const XK_Greek_etaaccent
-- #define XK_Greek_iotaaccent              0x07b4  /* U+03AF GREEK SMALL LETTER IOTA WITH TONOS */
pattern XK_Greek_iotaaccent = MkKeySymbol #const XK_Greek_iotaaccent
-- #define XK_Greek_iotadieresis            0x07b5  /* U+03CA GREEK SMALL LETTER IOTA WITH DIALYTIKA */
pattern XK_Greek_iotadieresis = MkKeySymbol #const XK_Greek_iotadieresis
-- #define XK_Greek_iotaaccentdieresis      0x07b6  /* U+0390 GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS */
pattern XK_Greek_iotaaccentdieresis = MkKeySymbol #const XK_Greek_iotaaccentdieresis
-- #define XK_Greek_omicronaccent           0x07b7  /* U+03CC GREEK SMALL LETTER OMICRON WITH TONOS */
pattern XK_Greek_omicronaccent = MkKeySymbol #const XK_Greek_omicronaccent
-- #define XK_Greek_upsilonaccent           0x07b8  /* U+03CD GREEK SMALL LETTER UPSILON WITH TONOS */
pattern XK_Greek_upsilonaccent = MkKeySymbol #const XK_Greek_upsilonaccent
-- #define XK_Greek_upsilondieresis         0x07b9  /* U+03CB GREEK SMALL LETTER UPSILON WITH DIALYTIKA */
pattern XK_Greek_upsilondieresis = MkKeySymbol #const XK_Greek_upsilondieresis
-- #define XK_Greek_upsilonaccentdieresis   0x07ba  /* U+03B0 GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS */
pattern XK_Greek_upsilonaccentdieresis = MkKeySymbol #const XK_Greek_upsilonaccentdieresis
-- #define XK_Greek_omegaaccent             0x07bb  /* U+03CE GREEK SMALL LETTER OMEGA WITH TONOS */
pattern XK_Greek_omegaaccent = MkKeySymbol #const XK_Greek_omegaaccent
-- #define XK_Greek_ALPHA                   0x07c1  /* U+0391 GREEK CAPITAL LETTER ALPHA */
pattern XK_Greek_ALPHA = MkKeySymbol #const XK_Greek_ALPHA
-- #define XK_Greek_BETA                    0x07c2  /* U+0392 GREEK CAPITAL LETTER BETA */
pattern XK_Greek_BETA = MkKeySymbol #const XK_Greek_BETA
-- #define XK_Greek_GAMMA                   0x07c3  /* U+0393 GREEK CAPITAL LETTER GAMMA */
pattern XK_Greek_GAMMA = MkKeySymbol #const XK_Greek_GAMMA
-- #define XK_Greek_DELTA                   0x07c4  /* U+0394 GREEK CAPITAL LETTER DELTA */
pattern XK_Greek_DELTA = MkKeySymbol #const XK_Greek_DELTA
-- #define XK_Greek_EPSILON                 0x07c5  /* U+0395 GREEK CAPITAL LETTER EPSILON */
pattern XK_Greek_EPSILON = MkKeySymbol #const XK_Greek_EPSILON
-- #define XK_Greek_ZETA                    0x07c6  /* U+0396 GREEK CAPITAL LETTER ZETA */
pattern XK_Greek_ZETA = MkKeySymbol #const XK_Greek_ZETA
-- #define XK_Greek_ETA                     0x07c7  /* U+0397 GREEK CAPITAL LETTER ETA */
pattern XK_Greek_ETA = MkKeySymbol #const XK_Greek_ETA
-- #define XK_Greek_THETA                   0x07c8  /* U+0398 GREEK CAPITAL LETTER THETA */
pattern XK_Greek_THETA = MkKeySymbol #const XK_Greek_THETA
-- #define XK_Greek_IOTA                    0x07c9  /* U+0399 GREEK CAPITAL LETTER IOTA */
pattern XK_Greek_IOTA = MkKeySymbol #const XK_Greek_IOTA
-- #define XK_Greek_KAPPA                   0x07ca  /* U+039A GREEK CAPITAL LETTER KAPPA */
pattern XK_Greek_KAPPA = MkKeySymbol #const XK_Greek_KAPPA
-- #define XK_Greek_LAMDA                   0x07cb  /* U+039B GREEK CAPITAL LETTER LAMDA */
pattern XK_Greek_LAMDA = MkKeySymbol #const XK_Greek_LAMDA
-- #define XK_Greek_LAMBDA                  0x07cb  /* U+039B GREEK CAPITAL LETTER LAMDA */
pattern XK_Greek_LAMBDA = MkKeySymbol #const XK_Greek_LAMBDA
-- #define XK_Greek_MU                      0x07cc  /* U+039C GREEK CAPITAL LETTER MU */
pattern XK_Greek_MU = MkKeySymbol #const XK_Greek_MU
-- #define XK_Greek_NU                      0x07cd  /* U+039D GREEK CAPITAL LETTER NU */
pattern XK_Greek_NU = MkKeySymbol #const XK_Greek_NU
-- #define XK_Greek_XI                      0x07ce  /* U+039E GREEK CAPITAL LETTER XI */
pattern XK_Greek_XI = MkKeySymbol #const XK_Greek_XI
-- #define XK_Greek_OMICRON                 0x07cf  /* U+039F GREEK CAPITAL LETTER OMICRON */
pattern XK_Greek_OMICRON = MkKeySymbol #const XK_Greek_OMICRON
-- #define XK_Greek_PI                      0x07d0  /* U+03A0 GREEK CAPITAL LETTER PI */
pattern XK_Greek_PI = MkKeySymbol #const XK_Greek_PI
-- #define XK_Greek_RHO                     0x07d1  /* U+03A1 GREEK CAPITAL LETTER RHO */
pattern XK_Greek_RHO = MkKeySymbol #const XK_Greek_RHO
-- #define XK_Greek_SIGMA                   0x07d2  /* U+03A3 GREEK CAPITAL LETTER SIGMA */
pattern XK_Greek_SIGMA = MkKeySymbol #const XK_Greek_SIGMA
-- #define XK_Greek_TAU                     0x07d4  /* U+03A4 GREEK CAPITAL LETTER TAU */
pattern XK_Greek_TAU = MkKeySymbol #const XK_Greek_TAU
-- #define XK_Greek_UPSILON                 0x07d5  /* U+03A5 GREEK CAPITAL LETTER UPSILON */
pattern XK_Greek_UPSILON = MkKeySymbol #const XK_Greek_UPSILON
-- #define XK_Greek_PHI                     0x07d6  /* U+03A6 GREEK CAPITAL LETTER PHI */
pattern XK_Greek_PHI = MkKeySymbol #const XK_Greek_PHI
-- #define XK_Greek_CHI                     0x07d7  /* U+03A7 GREEK CAPITAL LETTER CHI */
pattern XK_Greek_CHI = MkKeySymbol #const XK_Greek_CHI
-- #define XK_Greek_PSI                     0x07d8  /* U+03A8 GREEK CAPITAL LETTER PSI */
pattern XK_Greek_PSI = MkKeySymbol #const XK_Greek_PSI
-- #define XK_Greek_OMEGA                   0x07d9  /* U+03A9 GREEK CAPITAL LETTER OMEGA */
pattern XK_Greek_OMEGA = MkKeySymbol #const XK_Greek_OMEGA
-- #define XK_Greek_alpha                   0x07e1  /* U+03B1 GREEK SMALL LETTER ALPHA */
pattern XK_Greek_alpha = MkKeySymbol #const XK_Greek_alpha
-- #define XK_Greek_beta                    0x07e2  /* U+03B2 GREEK SMALL LETTER BETA */
pattern XK_Greek_beta = MkKeySymbol #const XK_Greek_beta
-- #define XK_Greek_gamma                   0x07e3  /* U+03B3 GREEK SMALL LETTER GAMMA */
pattern XK_Greek_gamma = MkKeySymbol #const XK_Greek_gamma
-- #define XK_Greek_delta                   0x07e4  /* U+03B4 GREEK SMALL LETTER DELTA */
pattern XK_Greek_delta = MkKeySymbol #const XK_Greek_delta
-- #define XK_Greek_epsilon                 0x07e5  /* U+03B5 GREEK SMALL LETTER EPSILON */
pattern XK_Greek_epsilon = MkKeySymbol #const XK_Greek_epsilon
-- #define XK_Greek_zeta                    0x07e6  /* U+03B6 GREEK SMALL LETTER ZETA */
pattern XK_Greek_zeta = MkKeySymbol #const XK_Greek_zeta
-- #define XK_Greek_eta                     0x07e7  /* U+03B7 GREEK SMALL LETTER ETA */
pattern XK_Greek_eta = MkKeySymbol #const XK_Greek_eta
-- #define XK_Greek_theta                   0x07e8  /* U+03B8 GREEK SMALL LETTER THETA */
pattern XK_Greek_theta = MkKeySymbol #const XK_Greek_theta
-- #define XK_Greek_iota                    0x07e9  /* U+03B9 GREEK SMALL LETTER IOTA */
pattern XK_Greek_iota = MkKeySymbol #const XK_Greek_iota
-- #define XK_Greek_kappa                   0x07ea  /* U+03BA GREEK SMALL LETTER KAPPA */
pattern XK_Greek_kappa = MkKeySymbol #const XK_Greek_kappa
-- #define XK_Greek_lamda                   0x07eb  /* U+03BB GREEK SMALL LETTER LAMDA */
pattern XK_Greek_lamda = MkKeySymbol #const XK_Greek_lamda
-- #define XK_Greek_lambda                  0x07eb  /* U+03BB GREEK SMALL LETTER LAMDA */
pattern XK_Greek_lambda = MkKeySymbol #const XK_Greek_lambda
-- #define XK_Greek_mu                      0x07ec  /* U+03BC GREEK SMALL LETTER MU */
pattern XK_Greek_mu = MkKeySymbol #const XK_Greek_mu
-- #define XK_Greek_nu                      0x07ed  /* U+03BD GREEK SMALL LETTER NU */
pattern XK_Greek_nu = MkKeySymbol #const XK_Greek_nu
-- #define XK_Greek_xi                      0x07ee  /* U+03BE GREEK SMALL LETTER XI */
pattern XK_Greek_xi = MkKeySymbol #const XK_Greek_xi
-- #define XK_Greek_omicron                 0x07ef  /* U+03BF GREEK SMALL LETTER OMICRON */
pattern XK_Greek_omicron = MkKeySymbol #const XK_Greek_omicron
-- #define XK_Greek_pi                      0x07f0  /* U+03C0 GREEK SMALL LETTER PI */
pattern XK_Greek_pi = MkKeySymbol #const XK_Greek_pi
-- #define XK_Greek_rho                     0x07f1  /* U+03C1 GREEK SMALL LETTER RHO */
pattern XK_Greek_rho = MkKeySymbol #const XK_Greek_rho
-- #define XK_Greek_sigma                   0x07f2  /* U+03C3 GREEK SMALL LETTER SIGMA */
pattern XK_Greek_sigma = MkKeySymbol #const XK_Greek_sigma
-- #define XK_Greek_finalsmallsigma         0x07f3  /* U+03C2 GREEK SMALL LETTER FINAL SIGMA */
pattern XK_Greek_finalsmallsigma = MkKeySymbol #const XK_Greek_finalsmallsigma
-- #define XK_Greek_tau                     0x07f4  /* U+03C4 GREEK SMALL LETTER TAU */
pattern XK_Greek_tau = MkKeySymbol #const XK_Greek_tau
-- #define XK_Greek_upsilon                 0x07f5  /* U+03C5 GREEK SMALL LETTER UPSILON */
pattern XK_Greek_upsilon = MkKeySymbol #const XK_Greek_upsilon
-- #define XK_Greek_phi                     0x07f6  /* U+03C6 GREEK SMALL LETTER PHI */
pattern XK_Greek_phi = MkKeySymbol #const XK_Greek_phi
-- #define XK_Greek_chi                     0x07f7  /* U+03C7 GREEK SMALL LETTER CHI */
pattern XK_Greek_chi = MkKeySymbol #const XK_Greek_chi
-- #define XK_Greek_psi                     0x07f8  /* U+03C8 GREEK SMALL LETTER PSI */
pattern XK_Greek_psi = MkKeySymbol #const XK_Greek_psi
-- #define XK_Greek_omega                   0x07f9  /* U+03C9 GREEK SMALL LETTER OMEGA */
pattern XK_Greek_omega = MkKeySymbol #const XK_Greek_omega
-- #define XK_Greek_switch                  0xff7e  /* Alias for mode_switch */
pattern XK_Greek_switch = MkKeySymbol #const XK_Greek_switch
-- #define XK_leftradical                   0x08a1  /* U+23B7 RADICAL SYMBOL BOTTOM */
pattern XK_leftradical = MkKeySymbol #const XK_leftradical
-- #define XK_topleftradical                0x08a2  /*(U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT)*/
pattern XK_topleftradical = MkKeySymbol #const XK_topleftradical
-- #define XK_horizconnector                0x08a3  /*(U+2500 BOX DRAWINGS LIGHT HORIZONTAL)*/
pattern XK_horizconnector = MkKeySymbol #const XK_horizconnector
-- #define XK_topintegral                   0x08a4  /* U+2320 TOP HALF INTEGRAL */
pattern XK_topintegral = MkKeySymbol #const XK_topintegral
-- #define XK_botintegral                   0x08a5  /* U+2321 BOTTOM HALF INTEGRAL */
pattern XK_botintegral = MkKeySymbol #const XK_botintegral
-- #define XK_vertconnector                 0x08a6  /*(U+2502 BOX DRAWINGS LIGHT VERTICAL)*/
pattern XK_vertconnector = MkKeySymbol #const XK_vertconnector
-- #define XK_topleftsqbracket              0x08a7  /* U+23A1 LEFT SQUARE BRACKET UPPER CORNER */
pattern XK_topleftsqbracket = MkKeySymbol #const XK_topleftsqbracket
-- #define XK_botleftsqbracket              0x08a8  /* U+23A3 LEFT SQUARE BRACKET LOWER CORNER */
pattern XK_botleftsqbracket = MkKeySymbol #const XK_botleftsqbracket
-- #define XK_toprightsqbracket             0x08a9  /* U+23A4 RIGHT SQUARE BRACKET UPPER CORNER */
pattern XK_toprightsqbracket = MkKeySymbol #const XK_toprightsqbracket
-- #define XK_botrightsqbracket             0x08aa  /* U+23A6 RIGHT SQUARE BRACKET LOWER CORNER */
pattern XK_botrightsqbracket = MkKeySymbol #const XK_botrightsqbracket
-- #define XK_topleftparens                 0x08ab  /* U+239B LEFT PARENTHESIS UPPER HOOK */
pattern XK_topleftparens = MkKeySymbol #const XK_topleftparens
-- #define XK_botleftparens                 0x08ac  /* U+239D LEFT PARENTHESIS LOWER HOOK */
pattern XK_botleftparens = MkKeySymbol #const XK_botleftparens
-- #define XK_toprightparens                0x08ad  /* U+239E RIGHT PARENTHESIS UPPER HOOK */
pattern XK_toprightparens = MkKeySymbol #const XK_toprightparens
-- #define XK_botrightparens                0x08ae  /* U+23A0 RIGHT PARENTHESIS LOWER HOOK */
pattern XK_botrightparens = MkKeySymbol #const XK_botrightparens
-- #define XK_leftmiddlecurlybrace          0x08af  /* U+23A8 LEFT CURLY BRACKET MIDDLE PIECE */
pattern XK_leftmiddlecurlybrace = MkKeySymbol #const XK_leftmiddlecurlybrace
-- #define XK_rightmiddlecurlybrace         0x08b0  /* U+23AC RIGHT CURLY BRACKET MIDDLE PIECE */
pattern XK_rightmiddlecurlybrace = MkKeySymbol #const XK_rightmiddlecurlybrace
-- #define XK_topleftsummation              0x08b1
pattern XK_topleftsummation = MkKeySymbol #const XK_topleftsummation
-- #define XK_botleftsummation              0x08b2
pattern XK_botleftsummation = MkKeySymbol #const XK_botleftsummation
-- #define XK_topvertsummationconnector     0x08b3
pattern XK_topvertsummationconnector = MkKeySymbol #const XK_topvertsummationconnector
-- #define XK_botvertsummationconnector     0x08b4
pattern XK_botvertsummationconnector = MkKeySymbol #const XK_botvertsummationconnector
-- #define XK_toprightsummation             0x08b5
pattern XK_toprightsummation = MkKeySymbol #const XK_toprightsummation
-- #define XK_botrightsummation             0x08b6
pattern XK_botrightsummation = MkKeySymbol #const XK_botrightsummation
-- #define XK_rightmiddlesummation          0x08b7
pattern XK_rightmiddlesummation = MkKeySymbol #const XK_rightmiddlesummation
-- #define XK_lessthanequal                 0x08bc  /* U+2264 LESS-THAN OR EQUAL TO */
pattern XK_lessthanequal = MkKeySymbol #const XK_lessthanequal
-- #define XK_notequal                      0x08bd  /* U+2260 NOT EQUAL TO */
pattern XK_notequal = MkKeySymbol #const XK_notequal
-- #define XK_greaterthanequal              0x08be  /* U+2265 GREATER-THAN OR EQUAL TO */
pattern XK_greaterthanequal = MkKeySymbol #const XK_greaterthanequal
-- #define XK_integral                      0x08bf  /* U+222B INTEGRAL */
pattern XK_integral = MkKeySymbol #const XK_integral
-- #define XK_therefore                     0x08c0  /* U+2234 THEREFORE */
pattern XK_therefore = MkKeySymbol #const XK_therefore
-- #define XK_variation                     0x08c1  /* U+221D PROPORTIONAL TO */
pattern XK_variation = MkKeySymbol #const XK_variation
-- #define XK_infinity                      0x08c2  /* U+221E INFINITY */
pattern XK_infinity = MkKeySymbol #const XK_infinity
-- #define XK_nabla                         0x08c5  /* U+2207 NABLA */
pattern XK_nabla = MkKeySymbol #const XK_nabla
-- #define XK_approximate                   0x08c8  /* U+223C TILDE OPERATOR */
pattern XK_approximate = MkKeySymbol #const XK_approximate
-- #define XK_similarequal                  0x08c9  /* U+2243 ASYMPTOTICALLY EQUAL TO */
pattern XK_similarequal = MkKeySymbol #const XK_similarequal
-- #define XK_ifonlyif                      0x08cd  /* U+21D4 LEFT RIGHT DOUBLE ARROW */
pattern XK_ifonlyif = MkKeySymbol #const XK_ifonlyif
-- #define XK_implies                       0x08ce  /* U+21D2 RIGHTWARDS DOUBLE ARROW */
pattern XK_implies = MkKeySymbol #const XK_implies
-- #define XK_identical                     0x08cf  /* U+2261 IDENTICAL TO */
pattern XK_identical = MkKeySymbol #const XK_identical
-- #define XK_radical                       0x08d6  /* U+221A SQUARE ROOT */
pattern XK_radical = MkKeySymbol #const XK_radical
-- #define XK_includedin                    0x08da  /* U+2282 SUBSET OF */
pattern XK_includedin = MkKeySymbol #const XK_includedin
-- #define XK_includes                      0x08db  /* U+2283 SUPERSET OF */
pattern XK_includes = MkKeySymbol #const XK_includes
-- #define XK_intersection                  0x08dc  /* U+2229 INTERSECTION */
pattern XK_intersection = MkKeySymbol #const XK_intersection
-- #define XK_union                         0x08dd  /* U+222A UNION */
pattern XK_union = MkKeySymbol #const XK_union
-- #define XK_logicaland                    0x08de  /* U+2227 LOGICAL AND */
pattern XK_logicaland = MkKeySymbol #const XK_logicaland
-- #define XK_logicalor                     0x08df  /* U+2228 LOGICAL OR */
pattern XK_logicalor = MkKeySymbol #const XK_logicalor
-- #define XK_partialderivative             0x08ef  /* U+2202 PARTIAL DIFFERENTIAL */
pattern XK_partialderivative = MkKeySymbol #const XK_partialderivative
-- #define XK_function                      0x08f6  /* U+0192 LATIN SMALL LETTER F WITH HOOK */
pattern XK_function = MkKeySymbol #const XK_function
-- #define XK_leftarrow                     0x08fb  /* U+2190 LEFTWARDS ARROW */
pattern XK_leftarrow = MkKeySymbol #const XK_leftarrow
-- #define XK_uparrow                       0x08fc  /* U+2191 UPWARDS ARROW */
pattern XK_uparrow = MkKeySymbol #const XK_uparrow
-- #define XK_rightarrow                    0x08fd  /* U+2192 RIGHTWARDS ARROW */
pattern XK_rightarrow = MkKeySymbol #const XK_rightarrow
-- #define XK_downarrow                     0x08fe  /* U+2193 DOWNWARDS ARROW */
pattern XK_downarrow = MkKeySymbol #const XK_downarrow
-- #define XK_blank                         0x09df
pattern XK_blank = MkKeySymbol #const XK_blank
-- #define XK_soliddiamond                  0x09e0  /* U+25C6 BLACK DIAMOND */
pattern XK_soliddiamond = MkKeySymbol #const XK_soliddiamond
-- #define XK_checkerboard                  0x09e1  /* U+2592 MEDIUM SHADE */
pattern XK_checkerboard = MkKeySymbol #const XK_checkerboard
-- #define XK_ht                            0x09e2  /* U+2409 SYMBOL FOR HORIZONTAL TABULATION */
pattern XK_ht = MkKeySymbol #const XK_ht
-- #define XK_ff                            0x09e3  /* U+240C SYMBOL FOR FORM FEED */
pattern XK_ff = MkKeySymbol #const XK_ff
-- #define XK_cr                            0x09e4  /* U+240D SYMBOL FOR CARRIAGE RETURN */
pattern XK_cr = MkKeySymbol #const XK_cr
-- #define XK_lf                            0x09e5  /* U+240A SYMBOL FOR LINE FEED */
pattern XK_lf = MkKeySymbol #const XK_lf
-- #define XK_nl                            0x09e8  /* U+2424 SYMBOL FOR NEWLINE */
pattern XK_nl = MkKeySymbol #const XK_nl
-- #define XK_vt                            0x09e9  /* U+240B SYMBOL FOR VERTICAL TABULATION */
pattern XK_vt = MkKeySymbol #const XK_vt
-- #define XK_lowrightcorner                0x09ea  /* U+2518 BOX DRAWINGS LIGHT UP AND LEFT */
pattern XK_lowrightcorner = MkKeySymbol #const XK_lowrightcorner
-- #define XK_uprightcorner                 0x09eb  /* U+2510 BOX DRAWINGS LIGHT DOWN AND LEFT */
pattern XK_uprightcorner = MkKeySymbol #const XK_uprightcorner
-- #define XK_upleftcorner                  0x09ec  /* U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT */
pattern XK_upleftcorner = MkKeySymbol #const XK_upleftcorner
-- #define XK_lowleftcorner                 0x09ed  /* U+2514 BOX DRAWINGS LIGHT UP AND RIGHT */
pattern XK_lowleftcorner = MkKeySymbol #const XK_lowleftcorner
-- #define XK_crossinglines                 0x09ee  /* U+253C BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL */
pattern XK_crossinglines = MkKeySymbol #const XK_crossinglines
-- #define XK_horizlinescan1                0x09ef  /* U+23BA HORIZONTAL SCAN LINE-1 */
pattern XK_horizlinescan1 = MkKeySymbol #const XK_horizlinescan1
-- #define XK_horizlinescan3                0x09f0  /* U+23BB HORIZONTAL SCAN LINE-3 */
pattern XK_horizlinescan3 = MkKeySymbol #const XK_horizlinescan3
-- #define XK_horizlinescan5                0x09f1  /* U+2500 BOX DRAWINGS LIGHT HORIZONTAL */
pattern XK_horizlinescan5 = MkKeySymbol #const XK_horizlinescan5
-- #define XK_horizlinescan7                0x09f2  /* U+23BC HORIZONTAL SCAN LINE-7 */
pattern XK_horizlinescan7 = MkKeySymbol #const XK_horizlinescan7
-- #define XK_horizlinescan9                0x09f3  /* U+23BD HORIZONTAL SCAN LINE-9 */
pattern XK_horizlinescan9 = MkKeySymbol #const XK_horizlinescan9
-- #define XK_leftt                         0x09f4  /* U+251C BOX DRAWINGS LIGHT VERTICAL AND RIGHT */
pattern XK_leftt = MkKeySymbol #const XK_leftt
-- #define XK_rightt                        0x09f5  /* U+2524 BOX DRAWINGS LIGHT VERTICAL AND LEFT */
pattern XK_rightt = MkKeySymbol #const XK_rightt
-- #define XK_bott                          0x09f6  /* U+2534 BOX DRAWINGS LIGHT UP AND HORIZONTAL */
pattern XK_bott = MkKeySymbol #const XK_bott
-- #define XK_topt                          0x09f7  /* U+252C BOX DRAWINGS LIGHT DOWN AND HORIZONTAL */
pattern XK_topt = MkKeySymbol #const XK_topt
-- #define XK_vertbar                       0x09f8  /* U+2502 BOX DRAWINGS LIGHT VERTICAL */
pattern XK_vertbar = MkKeySymbol #const XK_vertbar
-- #define XK_emspace                       0x0aa1  /* U+2003 EM SPACE */
pattern XK_emspace = MkKeySymbol #const XK_emspace
-- #define XK_enspace                       0x0aa2  /* U+2002 EN SPACE */
pattern XK_enspace = MkKeySymbol #const XK_enspace
-- #define XK_em3space                      0x0aa3  /* U+2004 THREE-PER-EM SPACE */
pattern XK_em3space = MkKeySymbol #const XK_em3space
-- #define XK_em4space                      0x0aa4  /* U+2005 FOUR-PER-EM SPACE */
pattern XK_em4space = MkKeySymbol #const XK_em4space
-- #define XK_digitspace                    0x0aa5  /* U+2007 FIGURE SPACE */
pattern XK_digitspace = MkKeySymbol #const XK_digitspace
-- #define XK_punctspace                    0x0aa6  /* U+2008 PUNCTUATION SPACE */
pattern XK_punctspace = MkKeySymbol #const XK_punctspace
-- #define XK_thinspace                     0x0aa7  /* U+2009 THIN SPACE */
pattern XK_thinspace = MkKeySymbol #const XK_thinspace
-- #define XK_hairspace                     0x0aa8  /* U+200A HAIR SPACE */
pattern XK_hairspace = MkKeySymbol #const XK_hairspace
-- #define XK_emdash                        0x0aa9  /* U+2014 EM DASH */
pattern XK_emdash = MkKeySymbol #const XK_emdash
-- #define XK_endash                        0x0aaa  /* U+2013 EN DASH */
pattern XK_endash = MkKeySymbol #const XK_endash
-- #define XK_signifblank                   0x0aac  /*(U+2423 OPEN BOX)*/
pattern XK_signifblank = MkKeySymbol #const XK_signifblank
-- #define XK_ellipsis                      0x0aae  /* U+2026 HORIZONTAL ELLIPSIS */
pattern XK_ellipsis = MkKeySymbol #const XK_ellipsis
-- #define XK_doubbaselinedot               0x0aaf  /* U+2025 TWO DOT LEADER */
pattern XK_doubbaselinedot = MkKeySymbol #const XK_doubbaselinedot
-- #define XK_onethird                      0x0ab0  /* U+2153 VULGAR FRACTION ONE THIRD */
pattern XK_onethird = MkKeySymbol #const XK_onethird
-- #define XK_twothirds                     0x0ab1  /* U+2154 VULGAR FRACTION TWO THIRDS */
pattern XK_twothirds = MkKeySymbol #const XK_twothirds
-- #define XK_onefifth                      0x0ab2  /* U+2155 VULGAR FRACTION ONE FIFTH */
pattern XK_onefifth = MkKeySymbol #const XK_onefifth
-- #define XK_twofifths                     0x0ab3  /* U+2156 VULGAR FRACTION TWO FIFTHS */
pattern XK_twofifths = MkKeySymbol #const XK_twofifths
-- #define XK_threefifths                   0x0ab4  /* U+2157 VULGAR FRACTION THREE FIFTHS */
pattern XK_threefifths = MkKeySymbol #const XK_threefifths
-- #define XK_fourfifths                    0x0ab5  /* U+2158 VULGAR FRACTION FOUR FIFTHS */
pattern XK_fourfifths = MkKeySymbol #const XK_fourfifths
-- #define XK_onesixth                      0x0ab6  /* U+2159 VULGAR FRACTION ONE SIXTH */
pattern XK_onesixth = MkKeySymbol #const XK_onesixth
-- #define XK_fivesixths                    0x0ab7  /* U+215A VULGAR FRACTION FIVE SIXTHS */
pattern XK_fivesixths = MkKeySymbol #const XK_fivesixths
-- #define XK_careof                        0x0ab8  /* U+2105 CARE OF */
pattern XK_careof = MkKeySymbol #const XK_careof
-- #define XK_figdash                       0x0abb  /* U+2012 FIGURE DASH */
pattern XK_figdash = MkKeySymbol #const XK_figdash
-- #define XK_leftanglebracket              0x0abc  /*(U+27E8 MATHEMATICAL LEFT ANGLE BRACKET)*/
pattern XK_leftanglebracket = MkKeySymbol #const XK_leftanglebracket
-- #define XK_decimalpoint                  0x0abd  /*(U+002E FULL STOP)*/
pattern XK_decimalpoint = MkKeySymbol #const XK_decimalpoint
-- #define XK_rightanglebracket             0x0abe  /*(U+27E9 MATHEMATICAL RIGHT ANGLE BRACKET)*/
pattern XK_rightanglebracket = MkKeySymbol #const XK_rightanglebracket
-- #define XK_marker                        0x0abf
pattern XK_marker = MkKeySymbol #const XK_marker
-- #define XK_oneeighth                     0x0ac3  /* U+215B VULGAR FRACTION ONE EIGHTH */
pattern XK_oneeighth = MkKeySymbol #const XK_oneeighth
-- #define XK_threeeighths                  0x0ac4  /* U+215C VULGAR FRACTION THREE EIGHTHS */
pattern XK_threeeighths = MkKeySymbol #const XK_threeeighths
-- #define XK_fiveeighths                   0x0ac5  /* U+215D VULGAR FRACTION FIVE EIGHTHS */
pattern XK_fiveeighths = MkKeySymbol #const XK_fiveeighths
-- #define XK_seveneighths                  0x0ac6  /* U+215E VULGAR FRACTION SEVEN EIGHTHS */
pattern XK_seveneighths = MkKeySymbol #const XK_seveneighths
-- #define XK_trademark                     0x0ac9  /* U+2122 TRADE MARK SIGN */
pattern XK_trademark = MkKeySymbol #const XK_trademark
-- #define XK_signaturemark                 0x0aca  /*(U+2613 SALTIRE)*/
pattern XK_signaturemark = MkKeySymbol #const XK_signaturemark
-- #define XK_trademarkincircle             0x0acb
pattern XK_trademarkincircle = MkKeySymbol #const XK_trademarkincircle
-- #define XK_leftopentriangle              0x0acc  /*(U+25C1 WHITE LEFT-POINTING TRIANGLE)*/
pattern XK_leftopentriangle = MkKeySymbol #const XK_leftopentriangle
-- #define XK_rightopentriangle             0x0acd  /*(U+25B7 WHITE RIGHT-POINTING TRIANGLE)*/
pattern XK_rightopentriangle = MkKeySymbol #const XK_rightopentriangle
-- #define XK_emopencircle                  0x0ace  /*(U+25CB WHITE CIRCLE)*/
pattern XK_emopencircle = MkKeySymbol #const XK_emopencircle
-- #define XK_emopenrectangle               0x0acf  /*(U+25AF WHITE VERTICAL RECTANGLE)*/
pattern XK_emopenrectangle = MkKeySymbol #const XK_emopenrectangle
-- #define XK_leftsinglequotemark           0x0ad0  /* U+2018 LEFT SINGLE QUOTATION MARK */
pattern XK_leftsinglequotemark = MkKeySymbol #const XK_leftsinglequotemark
-- #define XK_rightsinglequotemark          0x0ad1  /* U+2019 RIGHT SINGLE QUOTATION MARK */
pattern XK_rightsinglequotemark = MkKeySymbol #const XK_rightsinglequotemark
-- #define XK_leftdoublequotemark           0x0ad2  /* U+201C LEFT DOUBLE QUOTATION MARK */
pattern XK_leftdoublequotemark = MkKeySymbol #const XK_leftdoublequotemark
-- #define XK_rightdoublequotemark          0x0ad3  /* U+201D RIGHT DOUBLE QUOTATION MARK */
pattern XK_rightdoublequotemark = MkKeySymbol #const XK_rightdoublequotemark
-- #define XK_prescription                  0x0ad4  /* U+211E PRESCRIPTION TAKE */
pattern XK_prescription = MkKeySymbol #const XK_prescription
-- #define XK_permille                      0x0ad5  /* U+2030 PER MILLE SIGN */
pattern XK_permille = MkKeySymbol #const XK_permille
-- #define XK_minutes                       0x0ad6  /* U+2032 PRIME */
pattern XK_minutes = MkKeySymbol #const XK_minutes
-- #define XK_seconds                       0x0ad7  /* U+2033 DOUBLE PRIME */
pattern XK_seconds = MkKeySymbol #const XK_seconds
-- #define XK_latincross                    0x0ad9  /* U+271D LATIN CROSS */
pattern XK_latincross = MkKeySymbol #const XK_latincross
-- #define XK_hexagram                      0x0ada
pattern XK_hexagram = MkKeySymbol #const XK_hexagram
-- #define XK_filledrectbullet              0x0adb  /*(U+25AC BLACK RECTANGLE)*/
pattern XK_filledrectbullet = MkKeySymbol #const XK_filledrectbullet
-- #define XK_filledlefttribullet           0x0adc  /*(U+25C0 BLACK LEFT-POINTING TRIANGLE)*/
pattern XK_filledlefttribullet = MkKeySymbol #const XK_filledlefttribullet
-- #define XK_filledrighttribullet          0x0add  /*(U+25B6 BLACK RIGHT-POINTING TRIANGLE)*/
pattern XK_filledrighttribullet = MkKeySymbol #const XK_filledrighttribullet
-- #define XK_emfilledcircle                0x0ade  /*(U+25CF BLACK CIRCLE)*/
pattern XK_emfilledcircle = MkKeySymbol #const XK_emfilledcircle
-- #define XK_emfilledrect                  0x0adf  /*(U+25AE BLACK VERTICAL RECTANGLE)*/
pattern XK_emfilledrect = MkKeySymbol #const XK_emfilledrect
-- #define XK_enopencircbullet              0x0ae0  /*(U+25E6 WHITE BULLET)*/
pattern XK_enopencircbullet = MkKeySymbol #const XK_enopencircbullet
-- #define XK_enopensquarebullet            0x0ae1  /*(U+25AB WHITE SMALL SQUARE)*/
pattern XK_enopensquarebullet = MkKeySymbol #const XK_enopensquarebullet
-- #define XK_openrectbullet                0x0ae2  /*(U+25AD WHITE RECTANGLE)*/
pattern XK_openrectbullet = MkKeySymbol #const XK_openrectbullet
-- #define XK_opentribulletup               0x0ae3  /*(U+25B3 WHITE UP-POINTING TRIANGLE)*/
pattern XK_opentribulletup = MkKeySymbol #const XK_opentribulletup
-- #define XK_opentribulletdown             0x0ae4  /*(U+25BD WHITE DOWN-POINTING TRIANGLE)*/
pattern XK_opentribulletdown = MkKeySymbol #const XK_opentribulletdown
-- #define XK_openstar                      0x0ae5  /*(U+2606 WHITE STAR)*/
pattern XK_openstar = MkKeySymbol #const XK_openstar
-- #define XK_enfilledcircbullet            0x0ae6  /*(U+2022 BULLET)*/
pattern XK_enfilledcircbullet = MkKeySymbol #const XK_enfilledcircbullet
-- #define XK_enfilledsqbullet              0x0ae7  /*(U+25AA BLACK SMALL SQUARE)*/
pattern XK_enfilledsqbullet = MkKeySymbol #const XK_enfilledsqbullet
-- #define XK_filledtribulletup             0x0ae8  /*(U+25B2 BLACK UP-POINTING TRIANGLE)*/
pattern XK_filledtribulletup = MkKeySymbol #const XK_filledtribulletup
-- #define XK_filledtribulletdown           0x0ae9  /*(U+25BC BLACK DOWN-POINTING TRIANGLE)*/
pattern XK_filledtribulletdown = MkKeySymbol #const XK_filledtribulletdown
-- #define XK_leftpointer                   0x0aea  /*(U+261C WHITE LEFT POINTING INDEX)*/
pattern XK_leftpointer = MkKeySymbol #const XK_leftpointer
-- #define XK_rightpointer                  0x0aeb  /*(U+261E WHITE RIGHT POINTING INDEX)*/
pattern XK_rightpointer = MkKeySymbol #const XK_rightpointer
-- #define XK_club                          0x0aec  /* U+2663 BLACK CLUB SUIT */
pattern XK_club = MkKeySymbol #const XK_club
-- #define XK_diamond                       0x0aed  /* U+2666 BLACK DIAMOND SUIT */
pattern XK_diamond = MkKeySymbol #const XK_diamond
-- #define XK_heart                         0x0aee  /* U+2665 BLACK HEART SUIT */
pattern XK_heart = MkKeySymbol #const XK_heart
-- #define XK_maltesecross                  0x0af0  /* U+2720 MALTESE CROSS */
pattern XK_maltesecross = MkKeySymbol #const XK_maltesecross
-- #define XK_dagger                        0x0af1  /* U+2020 DAGGER */
pattern XK_dagger = MkKeySymbol #const XK_dagger
-- #define XK_doubledagger                  0x0af2  /* U+2021 DOUBLE DAGGER */
pattern XK_doubledagger = MkKeySymbol #const XK_doubledagger
-- #define XK_checkmark                     0x0af3  /* U+2713 CHECK MARK */
pattern XK_checkmark = MkKeySymbol #const XK_checkmark
-- #define XK_ballotcross                   0x0af4  /* U+2717 BALLOT X */
pattern XK_ballotcross = MkKeySymbol #const XK_ballotcross
-- #define XK_musicalsharp                  0x0af5  /* U+266F MUSIC SHARP SIGN */
pattern XK_musicalsharp = MkKeySymbol #const XK_musicalsharp
-- #define XK_musicalflat                   0x0af6  /* U+266D MUSIC FLAT SIGN */
pattern XK_musicalflat = MkKeySymbol #const XK_musicalflat
-- #define XK_malesymbol                    0x0af7  /* U+2642 MALE SIGN */
pattern XK_malesymbol = MkKeySymbol #const XK_malesymbol
-- #define XK_femalesymbol                  0x0af8  /* U+2640 FEMALE SIGN */
pattern XK_femalesymbol = MkKeySymbol #const XK_femalesymbol
-- #define XK_telephone                     0x0af9  /* U+260E BLACK TELEPHONE */
pattern XK_telephone = MkKeySymbol #const XK_telephone
-- #define XK_telephonerecorder             0x0afa  /* U+2315 TELEPHONE RECORDER */
pattern XK_telephonerecorder = MkKeySymbol #const XK_telephonerecorder
-- #define XK_phonographcopyright           0x0afb  /* U+2117 SOUND RECORDING COPYRIGHT */
pattern XK_phonographcopyright = MkKeySymbol #const XK_phonographcopyright
-- #define XK_caret                         0x0afc  /* U+2038 CARET */
pattern XK_caret = MkKeySymbol #const XK_caret
-- #define XK_singlelowquotemark            0x0afd  /* U+201A SINGLE LOW-9 QUOTATION MARK */
pattern XK_singlelowquotemark = MkKeySymbol #const XK_singlelowquotemark
-- #define XK_doublelowquotemark            0x0afe  /* U+201E DOUBLE LOW-9 QUOTATION MARK */
pattern XK_doublelowquotemark = MkKeySymbol #const XK_doublelowquotemark
-- #define XK_cursor                        0x0aff
pattern XK_cursor = MkKeySymbol #const XK_cursor
-- #define XK_leftcaret                     0x0ba3  /*(U+003C LESS-THAN SIGN)*/
pattern XK_leftcaret = MkKeySymbol #const XK_leftcaret
-- #define XK_rightcaret                    0x0ba6  /*(U+003E GREATER-THAN SIGN)*/
pattern XK_rightcaret = MkKeySymbol #const XK_rightcaret
-- #define XK_downcaret                     0x0ba8  /*(U+2228 LOGICAL OR)*/
pattern XK_downcaret = MkKeySymbol #const XK_downcaret
-- #define XK_upcaret                       0x0ba9  /*(U+2227 LOGICAL AND)*/
pattern XK_upcaret = MkKeySymbol #const XK_upcaret
-- #define XK_overbar                       0x0bc0  /*(U+00AF MACRON)*/
pattern XK_overbar = MkKeySymbol #const XK_overbar
-- #define XK_downtack                      0x0bc2  /* U+22A4 DOWN TACK */
pattern XK_downtack = MkKeySymbol #const XK_downtack
-- #define XK_upshoe                        0x0bc3  /*(U+2229 INTERSECTION)*/
pattern XK_upshoe = MkKeySymbol #const XK_upshoe
-- #define XK_downstile                     0x0bc4  /* U+230A LEFT FLOOR */
pattern XK_downstile = MkKeySymbol #const XK_downstile
-- #define XK_underbar                      0x0bc6  /*(U+005F LOW LINE)*/
pattern XK_underbar = MkKeySymbol #const XK_underbar
-- #define XK_jot                           0x0bca  /* U+2218 RING OPERATOR */
pattern XK_jot = MkKeySymbol #const XK_jot
-- #define XK_quad                          0x0bcc  /* U+2395 APL FUNCTIONAL SYMBOL QUAD */
pattern XK_quad = MkKeySymbol #const XK_quad
-- #define XK_uptack                        0x0bce  /* U+22A5 UP TACK */
pattern XK_uptack = MkKeySymbol #const XK_uptack
-- #define XK_circle                        0x0bcf  /* U+25CB WHITE CIRCLE */
pattern XK_circle = MkKeySymbol #const XK_circle
-- #define XK_upstile                       0x0bd3  /* U+2308 LEFT CEILING */
pattern XK_upstile = MkKeySymbol #const XK_upstile
-- #define XK_downshoe                      0x0bd6  /*(U+222A UNION)*/
pattern XK_downshoe = MkKeySymbol #const XK_downshoe
-- #define XK_rightshoe                     0x0bd8  /*(U+2283 SUPERSET OF)*/
pattern XK_rightshoe = MkKeySymbol #const XK_rightshoe
-- #define XK_leftshoe                      0x0bda  /*(U+2282 SUBSET OF)*/
pattern XK_leftshoe = MkKeySymbol #const XK_leftshoe
-- #define XK_lefttack                      0x0bdc  /* U+22A3 LEFT TACK */
pattern XK_lefttack = MkKeySymbol #const XK_lefttack
-- #define XK_righttack                     0x0bfc  /* U+22A2 RIGHT TACK */
pattern XK_righttack = MkKeySymbol #const XK_righttack
-- #define XK_hebrew_doublelowline          0x0cdf  /* U+2017 DOUBLE LOW LINE */
pattern XK_hebrew_doublelowline = MkKeySymbol #const XK_hebrew_doublelowline
-- #define XK_hebrew_aleph                  0x0ce0  /* U+05D0 HEBREW LETTER ALEF */
pattern XK_hebrew_aleph = MkKeySymbol #const XK_hebrew_aleph
-- #define XK_hebrew_bet                    0x0ce1  /* U+05D1 HEBREW LETTER BET */
pattern XK_hebrew_bet = MkKeySymbol #const XK_hebrew_bet
-- #define XK_hebrew_beth                   0x0ce1  /* deprecated */
pattern XK_hebrew_beth = MkKeySymbol #const XK_hebrew_beth
-- #define XK_hebrew_gimel                  0x0ce2  /* U+05D2 HEBREW LETTER GIMEL */
pattern XK_hebrew_gimel = MkKeySymbol #const XK_hebrew_gimel
-- #define XK_hebrew_gimmel                 0x0ce2  /* deprecated */
pattern XK_hebrew_gimmel = MkKeySymbol #const XK_hebrew_gimmel
-- #define XK_hebrew_dalet                  0x0ce3  /* U+05D3 HEBREW LETTER DALET */
pattern XK_hebrew_dalet = MkKeySymbol #const XK_hebrew_dalet
-- #define XK_hebrew_daleth                 0x0ce3  /* deprecated */
pattern XK_hebrew_daleth = MkKeySymbol #const XK_hebrew_daleth
-- #define XK_hebrew_he                     0x0ce4  /* U+05D4 HEBREW LETTER HE */
pattern XK_hebrew_he = MkKeySymbol #const XK_hebrew_he
-- #define XK_hebrew_waw                    0x0ce5  /* U+05D5 HEBREW LETTER VAV */
pattern XK_hebrew_waw = MkKeySymbol #const XK_hebrew_waw
-- #define XK_hebrew_zain                   0x0ce6  /* U+05D6 HEBREW LETTER ZAYIN */
pattern XK_hebrew_zain = MkKeySymbol #const XK_hebrew_zain
-- #define XK_hebrew_zayin                  0x0ce6  /* deprecated */
pattern XK_hebrew_zayin = MkKeySymbol #const XK_hebrew_zayin
-- #define XK_hebrew_chet                   0x0ce7  /* U+05D7 HEBREW LETTER HET */
pattern XK_hebrew_chet = MkKeySymbol #const XK_hebrew_chet
-- #define XK_hebrew_het                    0x0ce7  /* deprecated */
pattern XK_hebrew_het = MkKeySymbol #const XK_hebrew_het
-- #define XK_hebrew_tet                    0x0ce8  /* U+05D8 HEBREW LETTER TET */
pattern XK_hebrew_tet = MkKeySymbol #const XK_hebrew_tet
-- #define XK_hebrew_teth                   0x0ce8  /* deprecated */
pattern XK_hebrew_teth = MkKeySymbol #const XK_hebrew_teth
-- #define XK_hebrew_yod                    0x0ce9  /* U+05D9 HEBREW LETTER YOD */
pattern XK_hebrew_yod = MkKeySymbol #const XK_hebrew_yod
-- #define XK_hebrew_finalkaph              0x0cea  /* U+05DA HEBREW LETTER FINAL KAF */
pattern XK_hebrew_finalkaph = MkKeySymbol #const XK_hebrew_finalkaph
-- #define XK_hebrew_kaph                   0x0ceb  /* U+05DB HEBREW LETTER KAF */
pattern XK_hebrew_kaph = MkKeySymbol #const XK_hebrew_kaph
-- #define XK_hebrew_lamed                  0x0cec  /* U+05DC HEBREW LETTER LAMED */
pattern XK_hebrew_lamed = MkKeySymbol #const XK_hebrew_lamed
-- #define XK_hebrew_finalmem               0x0ced  /* U+05DD HEBREW LETTER FINAL MEM */
pattern XK_hebrew_finalmem = MkKeySymbol #const XK_hebrew_finalmem
-- #define XK_hebrew_mem                    0x0cee  /* U+05DE HEBREW LETTER MEM */
pattern XK_hebrew_mem = MkKeySymbol #const XK_hebrew_mem
-- #define XK_hebrew_finalnun               0x0cef  /* U+05DF HEBREW LETTER FINAL NUN */
pattern XK_hebrew_finalnun = MkKeySymbol #const XK_hebrew_finalnun
-- #define XK_hebrew_nun                    0x0cf0  /* U+05E0 HEBREW LETTER NUN */
pattern XK_hebrew_nun = MkKeySymbol #const XK_hebrew_nun
-- #define XK_hebrew_samech                 0x0cf1  /* U+05E1 HEBREW LETTER SAMEKH */
pattern XK_hebrew_samech = MkKeySymbol #const XK_hebrew_samech
-- #define XK_hebrew_samekh                 0x0cf1  /* deprecated */
pattern XK_hebrew_samekh = MkKeySymbol #const XK_hebrew_samekh
-- #define XK_hebrew_ayin                   0x0cf2  /* U+05E2 HEBREW LETTER AYIN */
pattern XK_hebrew_ayin = MkKeySymbol #const XK_hebrew_ayin
-- #define XK_hebrew_finalpe                0x0cf3  /* U+05E3 HEBREW LETTER FINAL PE */
pattern XK_hebrew_finalpe = MkKeySymbol #const XK_hebrew_finalpe
-- #define XK_hebrew_pe                     0x0cf4  /* U+05E4 HEBREW LETTER PE */
pattern XK_hebrew_pe = MkKeySymbol #const XK_hebrew_pe
-- #define XK_hebrew_finalzade              0x0cf5  /* U+05E5 HEBREW LETTER FINAL TSADI */
pattern XK_hebrew_finalzade = MkKeySymbol #const XK_hebrew_finalzade
-- #define XK_hebrew_finalzadi              0x0cf5  /* deprecated */
pattern XK_hebrew_finalzadi = MkKeySymbol #const XK_hebrew_finalzadi
-- #define XK_hebrew_zade                   0x0cf6  /* U+05E6 HEBREW LETTER TSADI */
pattern XK_hebrew_zade = MkKeySymbol #const XK_hebrew_zade
-- #define XK_hebrew_zadi                   0x0cf6  /* deprecated */
pattern XK_hebrew_zadi = MkKeySymbol #const XK_hebrew_zadi
-- #define XK_hebrew_qoph                   0x0cf7  /* U+05E7 HEBREW LETTER QOF */
pattern XK_hebrew_qoph = MkKeySymbol #const XK_hebrew_qoph
-- #define XK_hebrew_kuf                    0x0cf7  /* deprecated */
pattern XK_hebrew_kuf = MkKeySymbol #const XK_hebrew_kuf
-- #define XK_hebrew_resh                   0x0cf8  /* U+05E8 HEBREW LETTER RESH */
pattern XK_hebrew_resh = MkKeySymbol #const XK_hebrew_resh
-- #define XK_hebrew_shin                   0x0cf9  /* U+05E9 HEBREW LETTER SHIN */
pattern XK_hebrew_shin = MkKeySymbol #const XK_hebrew_shin
-- #define XK_hebrew_taw                    0x0cfa  /* U+05EA HEBREW LETTER TAV */
pattern XK_hebrew_taw = MkKeySymbol #const XK_hebrew_taw
-- #define XK_hebrew_taf                    0x0cfa  /* deprecated */
pattern XK_hebrew_taf = MkKeySymbol #const XK_hebrew_taf
-- #define XK_Hebrew_switch                 0xff7e  /* Alias for mode_switch */
pattern XK_Hebrew_switch = MkKeySymbol #const XK_Hebrew_switch
-- #define XK_Thai_kokai                    0x0da1  /* U+0E01 THAI CHARACTER KO KAI */
pattern XK_Thai_kokai = MkKeySymbol #const XK_Thai_kokai
-- #define XK_Thai_khokhai                  0x0da2  /* U+0E02 THAI CHARACTER KHO KHAI */
pattern XK_Thai_khokhai = MkKeySymbol #const XK_Thai_khokhai
-- #define XK_Thai_khokhuat                 0x0da3  /* U+0E03 THAI CHARACTER KHO KHUAT */
pattern XK_Thai_khokhuat = MkKeySymbol #const XK_Thai_khokhuat
-- #define XK_Thai_khokhwai                 0x0da4  /* U+0E04 THAI CHARACTER KHO KHWAI */
pattern XK_Thai_khokhwai = MkKeySymbol #const XK_Thai_khokhwai
-- #define XK_Thai_khokhon                  0x0da5  /* U+0E05 THAI CHARACTER KHO KHON */
pattern XK_Thai_khokhon = MkKeySymbol #const XK_Thai_khokhon
-- #define XK_Thai_khorakhang               0x0da6  /* U+0E06 THAI CHARACTER KHO RAKHANG */
pattern XK_Thai_khorakhang = MkKeySymbol #const XK_Thai_khorakhang
-- #define XK_Thai_ngongu                   0x0da7  /* U+0E07 THAI CHARACTER NGO NGU */
pattern XK_Thai_ngongu = MkKeySymbol #const XK_Thai_ngongu
-- #define XK_Thai_chochan                  0x0da8  /* U+0E08 THAI CHARACTER CHO CHAN */
pattern XK_Thai_chochan = MkKeySymbol #const XK_Thai_chochan
-- #define XK_Thai_choching                 0x0da9  /* U+0E09 THAI CHARACTER CHO CHING */
pattern XK_Thai_choching = MkKeySymbol #const XK_Thai_choching
-- #define XK_Thai_chochang                 0x0daa  /* U+0E0A THAI CHARACTER CHO CHANG */
pattern XK_Thai_chochang = MkKeySymbol #const XK_Thai_chochang
-- #define XK_Thai_soso                     0x0dab  /* U+0E0B THAI CHARACTER SO SO */
pattern XK_Thai_soso = MkKeySymbol #const XK_Thai_soso
-- #define XK_Thai_chochoe                  0x0dac  /* U+0E0C THAI CHARACTER CHO CHOE */
pattern XK_Thai_chochoe = MkKeySymbol #const XK_Thai_chochoe
-- #define XK_Thai_yoying                   0x0dad  /* U+0E0D THAI CHARACTER YO YING */
pattern XK_Thai_yoying = MkKeySymbol #const XK_Thai_yoying
-- #define XK_Thai_dochada                  0x0dae  /* U+0E0E THAI CHARACTER DO CHADA */
pattern XK_Thai_dochada = MkKeySymbol #const XK_Thai_dochada
-- #define XK_Thai_topatak                  0x0daf  /* U+0E0F THAI CHARACTER TO PATAK */
pattern XK_Thai_topatak = MkKeySymbol #const XK_Thai_topatak
-- #define XK_Thai_thothan                  0x0db0  /* U+0E10 THAI CHARACTER THO THAN */
pattern XK_Thai_thothan = MkKeySymbol #const XK_Thai_thothan
-- #define XK_Thai_thonangmontho            0x0db1  /* U+0E11 THAI CHARACTER THO NANGMONTHO */
pattern XK_Thai_thonangmontho = MkKeySymbol #const XK_Thai_thonangmontho
-- #define XK_Thai_thophuthao               0x0db2  /* U+0E12 THAI CHARACTER THO PHUTHAO */
pattern XK_Thai_thophuthao = MkKeySymbol #const XK_Thai_thophuthao
-- #define XK_Thai_nonen                    0x0db3  /* U+0E13 THAI CHARACTER NO NEN */
pattern XK_Thai_nonen = MkKeySymbol #const XK_Thai_nonen
-- #define XK_Thai_dodek                    0x0db4  /* U+0E14 THAI CHARACTER DO DEK */
pattern XK_Thai_dodek = MkKeySymbol #const XK_Thai_dodek
-- #define XK_Thai_totao                    0x0db5  /* U+0E15 THAI CHARACTER TO TAO */
pattern XK_Thai_totao = MkKeySymbol #const XK_Thai_totao
-- #define XK_Thai_thothung                 0x0db6  /* U+0E16 THAI CHARACTER THO THUNG */
pattern XK_Thai_thothung = MkKeySymbol #const XK_Thai_thothung
-- #define XK_Thai_thothahan                0x0db7  /* U+0E17 THAI CHARACTER THO THAHAN */
pattern XK_Thai_thothahan = MkKeySymbol #const XK_Thai_thothahan
-- #define XK_Thai_thothong                 0x0db8  /* U+0E18 THAI CHARACTER THO THONG */
pattern XK_Thai_thothong = MkKeySymbol #const XK_Thai_thothong
-- #define XK_Thai_nonu                     0x0db9  /* U+0E19 THAI CHARACTER NO NU */
pattern XK_Thai_nonu = MkKeySymbol #const XK_Thai_nonu
-- #define XK_Thai_bobaimai                 0x0dba  /* U+0E1A THAI CHARACTER BO BAIMAI */
pattern XK_Thai_bobaimai = MkKeySymbol #const XK_Thai_bobaimai
-- #define XK_Thai_popla                    0x0dbb  /* U+0E1B THAI CHARACTER PO PLA */
pattern XK_Thai_popla = MkKeySymbol #const XK_Thai_popla
-- #define XK_Thai_phophung                 0x0dbc  /* U+0E1C THAI CHARACTER PHO PHUNG */
pattern XK_Thai_phophung = MkKeySymbol #const XK_Thai_phophung
-- #define XK_Thai_fofa                     0x0dbd  /* U+0E1D THAI CHARACTER FO FA */
pattern XK_Thai_fofa = MkKeySymbol #const XK_Thai_fofa
-- #define XK_Thai_phophan                  0x0dbe  /* U+0E1E THAI CHARACTER PHO PHAN */
pattern XK_Thai_phophan = MkKeySymbol #const XK_Thai_phophan
-- #define XK_Thai_fofan                    0x0dbf  /* U+0E1F THAI CHARACTER FO FAN */
pattern XK_Thai_fofan = MkKeySymbol #const XK_Thai_fofan
-- #define XK_Thai_phosamphao               0x0dc0  /* U+0E20 THAI CHARACTER PHO SAMPHAO */
pattern XK_Thai_phosamphao = MkKeySymbol #const XK_Thai_phosamphao
-- #define XK_Thai_moma                     0x0dc1  /* U+0E21 THAI CHARACTER MO MA */
pattern XK_Thai_moma = MkKeySymbol #const XK_Thai_moma
-- #define XK_Thai_yoyak                    0x0dc2  /* U+0E22 THAI CHARACTER YO YAK */
pattern XK_Thai_yoyak = MkKeySymbol #const XK_Thai_yoyak
-- #define XK_Thai_rorua                    0x0dc3  /* U+0E23 THAI CHARACTER RO RUA */
pattern XK_Thai_rorua = MkKeySymbol #const XK_Thai_rorua
-- #define XK_Thai_ru                       0x0dc4  /* U+0E24 THAI CHARACTER RU */
pattern XK_Thai_ru = MkKeySymbol #const XK_Thai_ru
-- #define XK_Thai_loling                   0x0dc5  /* U+0E25 THAI CHARACTER LO LING */
pattern XK_Thai_loling = MkKeySymbol #const XK_Thai_loling
-- #define XK_Thai_lu                       0x0dc6  /* U+0E26 THAI CHARACTER LU */
pattern XK_Thai_lu = MkKeySymbol #const XK_Thai_lu
-- #define XK_Thai_wowaen                   0x0dc7  /* U+0E27 THAI CHARACTER WO WAEN */
pattern XK_Thai_wowaen = MkKeySymbol #const XK_Thai_wowaen
-- #define XK_Thai_sosala                   0x0dc8  /* U+0E28 THAI CHARACTER SO SALA */
pattern XK_Thai_sosala = MkKeySymbol #const XK_Thai_sosala
-- #define XK_Thai_sorusi                   0x0dc9  /* U+0E29 THAI CHARACTER SO RUSI */
pattern XK_Thai_sorusi = MkKeySymbol #const XK_Thai_sorusi
-- #define XK_Thai_sosua                    0x0dca  /* U+0E2A THAI CHARACTER SO SUA */
pattern XK_Thai_sosua = MkKeySymbol #const XK_Thai_sosua
-- #define XK_Thai_hohip                    0x0dcb  /* U+0E2B THAI CHARACTER HO HIP */
pattern XK_Thai_hohip = MkKeySymbol #const XK_Thai_hohip
-- #define XK_Thai_lochula                  0x0dcc  /* U+0E2C THAI CHARACTER LO CHULA */
pattern XK_Thai_lochula = MkKeySymbol #const XK_Thai_lochula
-- #define XK_Thai_oang                     0x0dcd  /* U+0E2D THAI CHARACTER O ANG */
pattern XK_Thai_oang = MkKeySymbol #const XK_Thai_oang
-- #define XK_Thai_honokhuk                 0x0dce  /* U+0E2E THAI CHARACTER HO NOKHUK */
pattern XK_Thai_honokhuk = MkKeySymbol #const XK_Thai_honokhuk
-- #define XK_Thai_paiyannoi                0x0dcf  /* U+0E2F THAI CHARACTER PAIYANNOI */
pattern XK_Thai_paiyannoi = MkKeySymbol #const XK_Thai_paiyannoi
-- #define XK_Thai_saraa                    0x0dd0  /* U+0E30 THAI CHARACTER SARA A */
pattern XK_Thai_saraa = MkKeySymbol #const XK_Thai_saraa
-- #define XK_Thai_maihanakat               0x0dd1  /* U+0E31 THAI CHARACTER MAI HAN-AKAT */
pattern XK_Thai_maihanakat = MkKeySymbol #const XK_Thai_maihanakat
-- #define XK_Thai_saraaa                   0x0dd2  /* U+0E32 THAI CHARACTER SARA AA */
pattern XK_Thai_saraaa = MkKeySymbol #const XK_Thai_saraaa
-- #define XK_Thai_saraam                   0x0dd3  /* U+0E33 THAI CHARACTER SARA AM */
pattern XK_Thai_saraam = MkKeySymbol #const XK_Thai_saraam
-- #define XK_Thai_sarai                    0x0dd4  /* U+0E34 THAI CHARACTER SARA I */
pattern XK_Thai_sarai = MkKeySymbol #const XK_Thai_sarai
-- #define XK_Thai_saraii                   0x0dd5  /* U+0E35 THAI CHARACTER SARA II */
pattern XK_Thai_saraii = MkKeySymbol #const XK_Thai_saraii
-- #define XK_Thai_saraue                   0x0dd6  /* U+0E36 THAI CHARACTER SARA UE */
pattern XK_Thai_saraue = MkKeySymbol #const XK_Thai_saraue
-- #define XK_Thai_sarauee                  0x0dd7  /* U+0E37 THAI CHARACTER SARA UEE */
pattern XK_Thai_sarauee = MkKeySymbol #const XK_Thai_sarauee
-- #define XK_Thai_sarau                    0x0dd8  /* U+0E38 THAI CHARACTER SARA U */
pattern XK_Thai_sarau = MkKeySymbol #const XK_Thai_sarau
-- #define XK_Thai_sarauu                   0x0dd9  /* U+0E39 THAI CHARACTER SARA UU */
pattern XK_Thai_sarauu = MkKeySymbol #const XK_Thai_sarauu
-- #define XK_Thai_phinthu                  0x0dda  /* U+0E3A THAI CHARACTER PHINTHU */
pattern XK_Thai_phinthu = MkKeySymbol #const XK_Thai_phinthu
-- #define XK_Thai_maihanakat_maitho        0x0dde
pattern XK_Thai_maihanakat_maitho = MkKeySymbol #const XK_Thai_maihanakat_maitho
-- #define XK_Thai_baht                     0x0ddf  /* U+0E3F THAI CURRENCY SYMBOL BAHT */
pattern XK_Thai_baht = MkKeySymbol #const XK_Thai_baht
-- #define XK_Thai_sarae                    0x0de0  /* U+0E40 THAI CHARACTER SARA E */
pattern XK_Thai_sarae = MkKeySymbol #const XK_Thai_sarae
-- #define XK_Thai_saraae                   0x0de1  /* U+0E41 THAI CHARACTER SARA AE */
pattern XK_Thai_saraae = MkKeySymbol #const XK_Thai_saraae
-- #define XK_Thai_sarao                    0x0de2  /* U+0E42 THAI CHARACTER SARA O */
pattern XK_Thai_sarao = MkKeySymbol #const XK_Thai_sarao
-- #define XK_Thai_saraaimaimuan            0x0de3  /* U+0E43 THAI CHARACTER SARA AI MAIMUAN */
pattern XK_Thai_saraaimaimuan = MkKeySymbol #const XK_Thai_saraaimaimuan
-- #define XK_Thai_saraaimaimalai           0x0de4  /* U+0E44 THAI CHARACTER SARA AI MAIMALAI */
pattern XK_Thai_saraaimaimalai = MkKeySymbol #const XK_Thai_saraaimaimalai
-- #define XK_Thai_lakkhangyao              0x0de5  /* U+0E45 THAI CHARACTER LAKKHANGYAO */
pattern XK_Thai_lakkhangyao = MkKeySymbol #const XK_Thai_lakkhangyao
-- #define XK_Thai_maiyamok                 0x0de6  /* U+0E46 THAI CHARACTER MAIYAMOK */
pattern XK_Thai_maiyamok = MkKeySymbol #const XK_Thai_maiyamok
-- #define XK_Thai_maitaikhu                0x0de7  /* U+0E47 THAI CHARACTER MAITAIKHU */
pattern XK_Thai_maitaikhu = MkKeySymbol #const XK_Thai_maitaikhu
-- #define XK_Thai_maiek                    0x0de8  /* U+0E48 THAI CHARACTER MAI EK */
pattern XK_Thai_maiek = MkKeySymbol #const XK_Thai_maiek
-- #define XK_Thai_maitho                   0x0de9  /* U+0E49 THAI CHARACTER MAI THO */
pattern XK_Thai_maitho = MkKeySymbol #const XK_Thai_maitho
-- #define XK_Thai_maitri                   0x0dea  /* U+0E4A THAI CHARACTER MAI TRI */
pattern XK_Thai_maitri = MkKeySymbol #const XK_Thai_maitri
-- #define XK_Thai_maichattawa              0x0deb  /* U+0E4B THAI CHARACTER MAI CHATTAWA */
pattern XK_Thai_maichattawa = MkKeySymbol #const XK_Thai_maichattawa
-- #define XK_Thai_thanthakhat              0x0dec  /* U+0E4C THAI CHARACTER THANTHAKHAT */
pattern XK_Thai_thanthakhat = MkKeySymbol #const XK_Thai_thanthakhat
-- #define XK_Thai_nikhahit                 0x0ded  /* U+0E4D THAI CHARACTER NIKHAHIT */
pattern XK_Thai_nikhahit = MkKeySymbol #const XK_Thai_nikhahit
-- #define XK_Thai_leksun                   0x0df0  /* U+0E50 THAI DIGIT ZERO */
pattern XK_Thai_leksun = MkKeySymbol #const XK_Thai_leksun
-- #define XK_Thai_leknung                  0x0df1  /* U+0E51 THAI DIGIT ONE */
pattern XK_Thai_leknung = MkKeySymbol #const XK_Thai_leknung
-- #define XK_Thai_leksong                  0x0df2  /* U+0E52 THAI DIGIT TWO */
pattern XK_Thai_leksong = MkKeySymbol #const XK_Thai_leksong
-- #define XK_Thai_leksam                   0x0df3  /* U+0E53 THAI DIGIT THREE */
pattern XK_Thai_leksam = MkKeySymbol #const XK_Thai_leksam
-- #define XK_Thai_leksi                    0x0df4  /* U+0E54 THAI DIGIT FOUR */
pattern XK_Thai_leksi = MkKeySymbol #const XK_Thai_leksi
-- #define XK_Thai_lekha                    0x0df5  /* U+0E55 THAI DIGIT FIVE */
pattern XK_Thai_lekha = MkKeySymbol #const XK_Thai_lekha
-- #define XK_Thai_lekhok                   0x0df6  /* U+0E56 THAI DIGIT SIX */
pattern XK_Thai_lekhok = MkKeySymbol #const XK_Thai_lekhok
-- #define XK_Thai_lekchet                  0x0df7  /* U+0E57 THAI DIGIT SEVEN */
pattern XK_Thai_lekchet = MkKeySymbol #const XK_Thai_lekchet
-- #define XK_Thai_lekpaet                  0x0df8  /* U+0E58 THAI DIGIT EIGHT */
pattern XK_Thai_lekpaet = MkKeySymbol #const XK_Thai_lekpaet
-- #define XK_Thai_lekkao                   0x0df9  /* U+0E59 THAI DIGIT NINE */
pattern XK_Thai_lekkao = MkKeySymbol #const XK_Thai_lekkao
-- #define XK_Hangul                        0xff31  /* Hangul start/stop(toggle) */
pattern XK_Hangul = MkKeySymbol #const XK_Hangul
-- #define XK_Hangul_Start                  0xff32  /* Hangul start */
pattern XK_Hangul_Start = MkKeySymbol #const XK_Hangul_Start
-- #define XK_Hangul_End                    0xff33  /* Hangul end, English start */
pattern XK_Hangul_End = MkKeySymbol #const XK_Hangul_End
-- #define XK_Hangul_Hanja                  0xff34  /* Start Hangul->Hanja Conversion */
pattern XK_Hangul_Hanja = MkKeySymbol #const XK_Hangul_Hanja
-- #define XK_Hangul_Jamo                   0xff35  /* Hangul Jamo mode */
pattern XK_Hangul_Jamo = MkKeySymbol #const XK_Hangul_Jamo
-- #define XK_Hangul_Romaja                 0xff36  /* Hangul Romaja mode */
pattern XK_Hangul_Romaja = MkKeySymbol #const XK_Hangul_Romaja
-- #define XK_Hangul_Codeinput              0xff37  /* Hangul code input mode */
pattern XK_Hangul_Codeinput = MkKeySymbol #const XK_Hangul_Codeinput
-- #define XK_Hangul_Jeonja                 0xff38  /* Jeonja mode */
pattern XK_Hangul_Jeonja = MkKeySymbol #const XK_Hangul_Jeonja
-- #define XK_Hangul_Banja                  0xff39  /* Banja mode */
pattern XK_Hangul_Banja = MkKeySymbol #const XK_Hangul_Banja
-- #define XK_Hangul_PreHanja               0xff3a  /* Pre Hanja conversion */
pattern XK_Hangul_PreHanja = MkKeySymbol #const XK_Hangul_PreHanja
-- #define XK_Hangul_PostHanja              0xff3b  /* Post Hanja conversion */
pattern XK_Hangul_PostHanja = MkKeySymbol #const XK_Hangul_PostHanja
-- #define XK_Hangul_SingleCandidate        0xff3c  /* Single candidate */
pattern XK_Hangul_SingleCandidate = MkKeySymbol #const XK_Hangul_SingleCandidate
-- #define XK_Hangul_MultipleCandidate      0xff3d  /* Multiple candidate */
pattern XK_Hangul_MultipleCandidate = MkKeySymbol #const XK_Hangul_MultipleCandidate
-- #define XK_Hangul_PreviousCandidate      0xff3e  /* Previous candidate */
pattern XK_Hangul_PreviousCandidate = MkKeySymbol #const XK_Hangul_PreviousCandidate
-- #define XK_Hangul_Special                0xff3f  /* Special symbols */
pattern XK_Hangul_Special = MkKeySymbol #const XK_Hangul_Special
-- #define XK_Hangul_switch                 0xff7e  /* Alias for mode_switch */
pattern XK_Hangul_switch = MkKeySymbol #const XK_Hangul_switch
-- #define XK_Hangul_Kiyeog                 0x0ea1
pattern XK_Hangul_Kiyeog = MkKeySymbol #const XK_Hangul_Kiyeog
-- #define XK_Hangul_SsangKiyeog            0x0ea2
pattern XK_Hangul_SsangKiyeog = MkKeySymbol #const XK_Hangul_SsangKiyeog
-- #define XK_Hangul_KiyeogSios             0x0ea3
pattern XK_Hangul_KiyeogSios = MkKeySymbol #const XK_Hangul_KiyeogSios
-- #define XK_Hangul_Nieun                  0x0ea4
pattern XK_Hangul_Nieun = MkKeySymbol #const XK_Hangul_Nieun
-- #define XK_Hangul_NieunJieuj             0x0ea5
pattern XK_Hangul_NieunJieuj = MkKeySymbol #const XK_Hangul_NieunJieuj
-- #define XK_Hangul_NieunHieuh             0x0ea6
pattern XK_Hangul_NieunHieuh = MkKeySymbol #const XK_Hangul_NieunHieuh
-- #define XK_Hangul_Dikeud                 0x0ea7
pattern XK_Hangul_Dikeud = MkKeySymbol #const XK_Hangul_Dikeud
-- #define XK_Hangul_SsangDikeud            0x0ea8
pattern XK_Hangul_SsangDikeud = MkKeySymbol #const XK_Hangul_SsangDikeud
-- #define XK_Hangul_Rieul                  0x0ea9
pattern XK_Hangul_Rieul = MkKeySymbol #const XK_Hangul_Rieul
-- #define XK_Hangul_RieulKiyeog            0x0eaa
pattern XK_Hangul_RieulKiyeog = MkKeySymbol #const XK_Hangul_RieulKiyeog
-- #define XK_Hangul_RieulMieum             0x0eab
pattern XK_Hangul_RieulMieum = MkKeySymbol #const XK_Hangul_RieulMieum
-- #define XK_Hangul_RieulPieub             0x0eac
pattern XK_Hangul_RieulPieub = MkKeySymbol #const XK_Hangul_RieulPieub
-- #define XK_Hangul_RieulSios              0x0ead
pattern XK_Hangul_RieulSios = MkKeySymbol #const XK_Hangul_RieulSios
-- #define XK_Hangul_RieulTieut             0x0eae
pattern XK_Hangul_RieulTieut = MkKeySymbol #const XK_Hangul_RieulTieut
-- #define XK_Hangul_RieulPhieuf            0x0eaf
pattern XK_Hangul_RieulPhieuf = MkKeySymbol #const XK_Hangul_RieulPhieuf
-- #define XK_Hangul_RieulHieuh             0x0eb0
pattern XK_Hangul_RieulHieuh = MkKeySymbol #const XK_Hangul_RieulHieuh
-- #define XK_Hangul_Mieum                  0x0eb1
pattern XK_Hangul_Mieum = MkKeySymbol #const XK_Hangul_Mieum
-- #define XK_Hangul_Pieub                  0x0eb2
pattern XK_Hangul_Pieub = MkKeySymbol #const XK_Hangul_Pieub
-- #define XK_Hangul_SsangPieub             0x0eb3
pattern XK_Hangul_SsangPieub = MkKeySymbol #const XK_Hangul_SsangPieub
-- #define XK_Hangul_PieubSios              0x0eb4
pattern XK_Hangul_PieubSios = MkKeySymbol #const XK_Hangul_PieubSios
-- #define XK_Hangul_Sios                   0x0eb5
pattern XK_Hangul_Sios = MkKeySymbol #const XK_Hangul_Sios
-- #define XK_Hangul_SsangSios              0x0eb6
pattern XK_Hangul_SsangSios = MkKeySymbol #const XK_Hangul_SsangSios
-- #define XK_Hangul_Ieung                  0x0eb7
pattern XK_Hangul_Ieung = MkKeySymbol #const XK_Hangul_Ieung
-- #define XK_Hangul_Jieuj                  0x0eb8
pattern XK_Hangul_Jieuj = MkKeySymbol #const XK_Hangul_Jieuj
-- #define XK_Hangul_SsangJieuj             0x0eb9
pattern XK_Hangul_SsangJieuj = MkKeySymbol #const XK_Hangul_SsangJieuj
-- #define XK_Hangul_Cieuc                  0x0eba
pattern XK_Hangul_Cieuc = MkKeySymbol #const XK_Hangul_Cieuc
-- #define XK_Hangul_Khieuq                 0x0ebb
pattern XK_Hangul_Khieuq = MkKeySymbol #const XK_Hangul_Khieuq
-- #define XK_Hangul_Tieut                  0x0ebc
pattern XK_Hangul_Tieut = MkKeySymbol #const XK_Hangul_Tieut
-- #define XK_Hangul_Phieuf                 0x0ebd
pattern XK_Hangul_Phieuf = MkKeySymbol #const XK_Hangul_Phieuf
-- #define XK_Hangul_Hieuh                  0x0ebe
pattern XK_Hangul_Hieuh = MkKeySymbol #const XK_Hangul_Hieuh
-- #define XK_Hangul_A                      0x0ebf
pattern XK_Hangul_A = MkKeySymbol #const XK_Hangul_A
-- #define XK_Hangul_AE                     0x0ec0
pattern XK_Hangul_AE = MkKeySymbol #const XK_Hangul_AE
-- #define XK_Hangul_YA                     0x0ec1
pattern XK_Hangul_YA = MkKeySymbol #const XK_Hangul_YA
-- #define XK_Hangul_YAE                    0x0ec2
pattern XK_Hangul_YAE = MkKeySymbol #const XK_Hangul_YAE
-- #define XK_Hangul_EO                     0x0ec3
pattern XK_Hangul_EO = MkKeySymbol #const XK_Hangul_EO
-- #define XK_Hangul_E                      0x0ec4
pattern XK_Hangul_E = MkKeySymbol #const XK_Hangul_E
-- #define XK_Hangul_YEO                    0x0ec5
pattern XK_Hangul_YEO = MkKeySymbol #const XK_Hangul_YEO
-- #define XK_Hangul_YE                     0x0ec6
pattern XK_Hangul_YE = MkKeySymbol #const XK_Hangul_YE
-- #define XK_Hangul_O                      0x0ec7
pattern XK_Hangul_O = MkKeySymbol #const XK_Hangul_O
-- #define XK_Hangul_WA                     0x0ec8
pattern XK_Hangul_WA = MkKeySymbol #const XK_Hangul_WA
-- #define XK_Hangul_WAE                    0x0ec9
pattern XK_Hangul_WAE = MkKeySymbol #const XK_Hangul_WAE
-- #define XK_Hangul_OE                     0x0eca
pattern XK_Hangul_OE = MkKeySymbol #const XK_Hangul_OE
-- #define XK_Hangul_YO                     0x0ecb
pattern XK_Hangul_YO = MkKeySymbol #const XK_Hangul_YO
-- #define XK_Hangul_U                      0x0ecc
pattern XK_Hangul_U = MkKeySymbol #const XK_Hangul_U
-- #define XK_Hangul_WEO                    0x0ecd
pattern XK_Hangul_WEO = MkKeySymbol #const XK_Hangul_WEO
-- #define XK_Hangul_WE                     0x0ece
pattern XK_Hangul_WE = MkKeySymbol #const XK_Hangul_WE
-- #define XK_Hangul_WI                     0x0ecf
pattern XK_Hangul_WI = MkKeySymbol #const XK_Hangul_WI
-- #define XK_Hangul_YU                     0x0ed0
pattern XK_Hangul_YU = MkKeySymbol #const XK_Hangul_YU
-- #define XK_Hangul_EU                     0x0ed1
pattern XK_Hangul_EU = MkKeySymbol #const XK_Hangul_EU
-- #define XK_Hangul_YI                     0x0ed2
pattern XK_Hangul_YI = MkKeySymbol #const XK_Hangul_YI
-- #define XK_Hangul_I                      0x0ed3
pattern XK_Hangul_I = MkKeySymbol #const XK_Hangul_I
-- #define XK_Hangul_J_Kiyeog               0x0ed4
pattern XK_Hangul_J_Kiyeog = MkKeySymbol #const XK_Hangul_J_Kiyeog
-- #define XK_Hangul_J_SsangKiyeog          0x0ed5
pattern XK_Hangul_J_SsangKiyeog = MkKeySymbol #const XK_Hangul_J_SsangKiyeog
-- #define XK_Hangul_J_KiyeogSios           0x0ed6
pattern XK_Hangul_J_KiyeogSios = MkKeySymbol #const XK_Hangul_J_KiyeogSios
-- #define XK_Hangul_J_Nieun                0x0ed7
pattern XK_Hangul_J_Nieun = MkKeySymbol #const XK_Hangul_J_Nieun
-- #define XK_Hangul_J_NieunJieuj           0x0ed8
pattern XK_Hangul_J_NieunJieuj = MkKeySymbol #const XK_Hangul_J_NieunJieuj
-- #define XK_Hangul_J_NieunHieuh           0x0ed9
pattern XK_Hangul_J_NieunHieuh = MkKeySymbol #const XK_Hangul_J_NieunHieuh
-- #define XK_Hangul_J_Dikeud               0x0eda
pattern XK_Hangul_J_Dikeud = MkKeySymbol #const XK_Hangul_J_Dikeud
-- #define XK_Hangul_J_Rieul                0x0edb
pattern XK_Hangul_J_Rieul = MkKeySymbol #const XK_Hangul_J_Rieul
-- #define XK_Hangul_J_RieulKiyeog          0x0edc
pattern XK_Hangul_J_RieulKiyeog = MkKeySymbol #const XK_Hangul_J_RieulKiyeog
-- #define XK_Hangul_J_RieulMieum           0x0edd
pattern XK_Hangul_J_RieulMieum = MkKeySymbol #const XK_Hangul_J_RieulMieum
-- #define XK_Hangul_J_RieulPieub           0x0ede
pattern XK_Hangul_J_RieulPieub = MkKeySymbol #const XK_Hangul_J_RieulPieub
-- #define XK_Hangul_J_RieulSios            0x0edf
pattern XK_Hangul_J_RieulSios = MkKeySymbol #const XK_Hangul_J_RieulSios
-- #define XK_Hangul_J_RieulTieut           0x0ee0
pattern XK_Hangul_J_RieulTieut = MkKeySymbol #const XK_Hangul_J_RieulTieut
-- #define XK_Hangul_J_RieulPhieuf          0x0ee1
pattern XK_Hangul_J_RieulPhieuf = MkKeySymbol #const XK_Hangul_J_RieulPhieuf
-- #define XK_Hangul_J_RieulHieuh           0x0ee2
pattern XK_Hangul_J_RieulHieuh = MkKeySymbol #const XK_Hangul_J_RieulHieuh
-- #define XK_Hangul_J_Mieum                0x0ee3
pattern XK_Hangul_J_Mieum = MkKeySymbol #const XK_Hangul_J_Mieum
-- #define XK_Hangul_J_Pieub                0x0ee4
pattern XK_Hangul_J_Pieub = MkKeySymbol #const XK_Hangul_J_Pieub
-- #define XK_Hangul_J_PieubSios            0x0ee5
pattern XK_Hangul_J_PieubSios = MkKeySymbol #const XK_Hangul_J_PieubSios
-- #define XK_Hangul_J_Sios                 0x0ee6
pattern XK_Hangul_J_Sios = MkKeySymbol #const XK_Hangul_J_Sios
-- #define XK_Hangul_J_SsangSios            0x0ee7
pattern XK_Hangul_J_SsangSios = MkKeySymbol #const XK_Hangul_J_SsangSios
-- #define XK_Hangul_J_Ieung                0x0ee8
pattern XK_Hangul_J_Ieung = MkKeySymbol #const XK_Hangul_J_Ieung
-- #define XK_Hangul_J_Jieuj                0x0ee9
pattern XK_Hangul_J_Jieuj = MkKeySymbol #const XK_Hangul_J_Jieuj
-- #define XK_Hangul_J_Cieuc                0x0eea
pattern XK_Hangul_J_Cieuc = MkKeySymbol #const XK_Hangul_J_Cieuc
-- #define XK_Hangul_J_Khieuq               0x0eeb
pattern XK_Hangul_J_Khieuq = MkKeySymbol #const XK_Hangul_J_Khieuq
-- #define XK_Hangul_J_Tieut                0x0eec
pattern XK_Hangul_J_Tieut = MkKeySymbol #const XK_Hangul_J_Tieut
-- #define XK_Hangul_J_Phieuf               0x0eed
pattern XK_Hangul_J_Phieuf = MkKeySymbol #const XK_Hangul_J_Phieuf
-- #define XK_Hangul_J_Hieuh                0x0eee
pattern XK_Hangul_J_Hieuh = MkKeySymbol #const XK_Hangul_J_Hieuh
-- #define XK_Hangul_RieulYeorinHieuh       0x0eef
pattern XK_Hangul_RieulYeorinHieuh = MkKeySymbol #const XK_Hangul_RieulYeorinHieuh
-- #define XK_Hangul_SunkyeongeumMieum      0x0ef0
pattern XK_Hangul_SunkyeongeumMieum = MkKeySymbol #const XK_Hangul_SunkyeongeumMieum
-- #define XK_Hangul_SunkyeongeumPieub      0x0ef1
pattern XK_Hangul_SunkyeongeumPieub = MkKeySymbol #const XK_Hangul_SunkyeongeumPieub
-- #define XK_Hangul_PanSios                0x0ef2
pattern XK_Hangul_PanSios = MkKeySymbol #const XK_Hangul_PanSios
-- #define XK_Hangul_KkogjiDalrinIeung      0x0ef3
pattern XK_Hangul_KkogjiDalrinIeung = MkKeySymbol #const XK_Hangul_KkogjiDalrinIeung
-- #define XK_Hangul_SunkyeongeumPhieuf     0x0ef4
pattern XK_Hangul_SunkyeongeumPhieuf = MkKeySymbol #const XK_Hangul_SunkyeongeumPhieuf
-- #define XK_Hangul_YeorinHieuh            0x0ef5
pattern XK_Hangul_YeorinHieuh = MkKeySymbol #const XK_Hangul_YeorinHieuh
-- #define XK_Hangul_AraeA                  0x0ef6
pattern XK_Hangul_AraeA = MkKeySymbol #const XK_Hangul_AraeA
-- #define XK_Hangul_AraeAE                 0x0ef7
pattern XK_Hangul_AraeAE = MkKeySymbol #const XK_Hangul_AraeAE
-- #define XK_Hangul_J_PanSios              0x0ef8
pattern XK_Hangul_J_PanSios = MkKeySymbol #const XK_Hangul_J_PanSios
-- #define XK_Hangul_J_KkogjiDalrinIeung    0x0ef9
pattern XK_Hangul_J_KkogjiDalrinIeung = MkKeySymbol #const XK_Hangul_J_KkogjiDalrinIeung
-- #define XK_Hangul_J_YeorinHieuh          0x0efa
pattern XK_Hangul_J_YeorinHieuh = MkKeySymbol #const XK_Hangul_J_YeorinHieuh
-- #define XK_Korean_Won                    0x0eff  /*(U+20A9 WON SIGN)*/
pattern XK_Korean_Won = MkKeySymbol #const XK_Korean_Won
-- #define XK_Armenian_ligature_ew       0x1000587  /* U+0587 ARMENIAN SMALL LIGATURE ECH YIWN */
pattern XK_Armenian_ligature_ew = MkKeySymbol #const XK_Armenian_ligature_ew
-- #define XK_Armenian_full_stop         0x1000589  /* U+0589 ARMENIAN FULL STOP */
pattern XK_Armenian_full_stop = MkKeySymbol #const XK_Armenian_full_stop
-- #define XK_Armenian_verjaket          0x1000589  /* U+0589 ARMENIAN FULL STOP */
pattern XK_Armenian_verjaket = MkKeySymbol #const XK_Armenian_verjaket
-- #define XK_Armenian_separation_mark   0x100055d  /* U+055D ARMENIAN COMMA */
pattern XK_Armenian_separation_mark = MkKeySymbol #const XK_Armenian_separation_mark
-- #define XK_Armenian_but               0x100055d  /* U+055D ARMENIAN COMMA */
pattern XK_Armenian_but = MkKeySymbol #const XK_Armenian_but
-- #define XK_Armenian_hyphen            0x100058a  /* U+058A ARMENIAN HYPHEN */
pattern XK_Armenian_hyphen = MkKeySymbol #const XK_Armenian_hyphen
-- #define XK_Armenian_yentamna          0x100058a  /* U+058A ARMENIAN HYPHEN */
pattern XK_Armenian_yentamna = MkKeySymbol #const XK_Armenian_yentamna
-- #define XK_Armenian_exclam            0x100055c  /* U+055C ARMENIAN EXCLAMATION MARK */
pattern XK_Armenian_exclam = MkKeySymbol #const XK_Armenian_exclam
-- #define XK_Armenian_amanak            0x100055c  /* U+055C ARMENIAN EXCLAMATION MARK */
pattern XK_Armenian_amanak = MkKeySymbol #const XK_Armenian_amanak
-- #define XK_Armenian_accent            0x100055b  /* U+055B ARMENIAN EMPHASIS MARK */
pattern XK_Armenian_accent = MkKeySymbol #const XK_Armenian_accent
-- #define XK_Armenian_shesht            0x100055b  /* U+055B ARMENIAN EMPHASIS MARK */
pattern XK_Armenian_shesht = MkKeySymbol #const XK_Armenian_shesht
-- #define XK_Armenian_question          0x100055e  /* U+055E ARMENIAN QUESTION MARK */
pattern XK_Armenian_question = MkKeySymbol #const XK_Armenian_question
-- #define XK_Armenian_paruyk            0x100055e  /* U+055E ARMENIAN QUESTION MARK */
pattern XK_Armenian_paruyk = MkKeySymbol #const XK_Armenian_paruyk
-- #define XK_Armenian_AYB               0x1000531  /* U+0531 ARMENIAN CAPITAL LETTER AYB */
pattern XK_Armenian_AYB = MkKeySymbol #const XK_Armenian_AYB
-- #define XK_Armenian_ayb               0x1000561  /* U+0561 ARMENIAN SMALL LETTER AYB */
pattern XK_Armenian_ayb = MkKeySymbol #const XK_Armenian_ayb
-- #define XK_Armenian_BEN               0x1000532  /* U+0532 ARMENIAN CAPITAL LETTER BEN */
pattern XK_Armenian_BEN = MkKeySymbol #const XK_Armenian_BEN
-- #define XK_Armenian_ben               0x1000562  /* U+0562 ARMENIAN SMALL LETTER BEN */
pattern XK_Armenian_ben = MkKeySymbol #const XK_Armenian_ben
-- #define XK_Armenian_GIM               0x1000533  /* U+0533 ARMENIAN CAPITAL LETTER GIM */
pattern XK_Armenian_GIM = MkKeySymbol #const XK_Armenian_GIM
-- #define XK_Armenian_gim               0x1000563  /* U+0563 ARMENIAN SMALL LETTER GIM */
pattern XK_Armenian_gim = MkKeySymbol #const XK_Armenian_gim
-- #define XK_Armenian_DA                0x1000534  /* U+0534 ARMENIAN CAPITAL LETTER DA */
pattern XK_Armenian_DA = MkKeySymbol #const XK_Armenian_DA
-- #define XK_Armenian_da                0x1000564  /* U+0564 ARMENIAN SMALL LETTER DA */
pattern XK_Armenian_da = MkKeySymbol #const XK_Armenian_da
-- #define XK_Armenian_YECH              0x1000535  /* U+0535 ARMENIAN CAPITAL LETTER ECH */
pattern XK_Armenian_YECH = MkKeySymbol #const XK_Armenian_YECH
-- #define XK_Armenian_yech              0x1000565  /* U+0565 ARMENIAN SMALL LETTER ECH */
pattern XK_Armenian_yech = MkKeySymbol #const XK_Armenian_yech
-- #define XK_Armenian_ZA                0x1000536  /* U+0536 ARMENIAN CAPITAL LETTER ZA */
pattern XK_Armenian_ZA = MkKeySymbol #const XK_Armenian_ZA
-- #define XK_Armenian_za                0x1000566  /* U+0566 ARMENIAN SMALL LETTER ZA */
pattern XK_Armenian_za = MkKeySymbol #const XK_Armenian_za
-- #define XK_Armenian_E                 0x1000537  /* U+0537 ARMENIAN CAPITAL LETTER EH */
pattern XK_Armenian_E = MkKeySymbol #const XK_Armenian_E
-- #define XK_Armenian_e                 0x1000567  /* U+0567 ARMENIAN SMALL LETTER EH */
pattern XK_Armenian_e = MkKeySymbol #const XK_Armenian_e
-- #define XK_Armenian_AT                0x1000538  /* U+0538 ARMENIAN CAPITAL LETTER ET */
pattern XK_Armenian_AT = MkKeySymbol #const XK_Armenian_AT
-- #define XK_Armenian_at                0x1000568  /* U+0568 ARMENIAN SMALL LETTER ET */
pattern XK_Armenian_at = MkKeySymbol #const XK_Armenian_at
-- #define XK_Armenian_TO                0x1000539  /* U+0539 ARMENIAN CAPITAL LETTER TO */
pattern XK_Armenian_TO = MkKeySymbol #const XK_Armenian_TO
-- #define XK_Armenian_to                0x1000569  /* U+0569 ARMENIAN SMALL LETTER TO */
pattern XK_Armenian_to = MkKeySymbol #const XK_Armenian_to
-- #define XK_Armenian_ZHE               0x100053a  /* U+053A ARMENIAN CAPITAL LETTER ZHE */
pattern XK_Armenian_ZHE = MkKeySymbol #const XK_Armenian_ZHE
-- #define XK_Armenian_zhe               0x100056a  /* U+056A ARMENIAN SMALL LETTER ZHE */
pattern XK_Armenian_zhe = MkKeySymbol #const XK_Armenian_zhe
-- #define XK_Armenian_INI               0x100053b  /* U+053B ARMENIAN CAPITAL LETTER INI */
pattern XK_Armenian_INI = MkKeySymbol #const XK_Armenian_INI
-- #define XK_Armenian_ini               0x100056b  /* U+056B ARMENIAN SMALL LETTER INI */
pattern XK_Armenian_ini = MkKeySymbol #const XK_Armenian_ini
-- #define XK_Armenian_LYUN              0x100053c  /* U+053C ARMENIAN CAPITAL LETTER LIWN */
pattern XK_Armenian_LYUN = MkKeySymbol #const XK_Armenian_LYUN
-- #define XK_Armenian_lyun              0x100056c  /* U+056C ARMENIAN SMALL LETTER LIWN */
pattern XK_Armenian_lyun = MkKeySymbol #const XK_Armenian_lyun
-- #define XK_Armenian_KHE               0x100053d  /* U+053D ARMENIAN CAPITAL LETTER XEH */
pattern XK_Armenian_KHE = MkKeySymbol #const XK_Armenian_KHE
-- #define XK_Armenian_khe               0x100056d  /* U+056D ARMENIAN SMALL LETTER XEH */
pattern XK_Armenian_khe = MkKeySymbol #const XK_Armenian_khe
-- #define XK_Armenian_TSA               0x100053e  /* U+053E ARMENIAN CAPITAL LETTER CA */
pattern XK_Armenian_TSA = MkKeySymbol #const XK_Armenian_TSA
-- #define XK_Armenian_tsa               0x100056e  /* U+056E ARMENIAN SMALL LETTER CA */
pattern XK_Armenian_tsa = MkKeySymbol #const XK_Armenian_tsa
-- #define XK_Armenian_KEN               0x100053f  /* U+053F ARMENIAN CAPITAL LETTER KEN */
pattern XK_Armenian_KEN = MkKeySymbol #const XK_Armenian_KEN
-- #define XK_Armenian_ken               0x100056f  /* U+056F ARMENIAN SMALL LETTER KEN */
pattern XK_Armenian_ken = MkKeySymbol #const XK_Armenian_ken
-- #define XK_Armenian_HO                0x1000540  /* U+0540 ARMENIAN CAPITAL LETTER HO */
pattern XK_Armenian_HO = MkKeySymbol #const XK_Armenian_HO
-- #define XK_Armenian_ho                0x1000570  /* U+0570 ARMENIAN SMALL LETTER HO */
pattern XK_Armenian_ho = MkKeySymbol #const XK_Armenian_ho
-- #define XK_Armenian_DZA               0x1000541  /* U+0541 ARMENIAN CAPITAL LETTER JA */
pattern XK_Armenian_DZA = MkKeySymbol #const XK_Armenian_DZA
-- #define XK_Armenian_dza               0x1000571  /* U+0571 ARMENIAN SMALL LETTER JA */
pattern XK_Armenian_dza = MkKeySymbol #const XK_Armenian_dza
-- #define XK_Armenian_GHAT              0x1000542  /* U+0542 ARMENIAN CAPITAL LETTER GHAD */
pattern XK_Armenian_GHAT = MkKeySymbol #const XK_Armenian_GHAT
-- #define XK_Armenian_ghat              0x1000572  /* U+0572 ARMENIAN SMALL LETTER GHAD */
pattern XK_Armenian_ghat = MkKeySymbol #const XK_Armenian_ghat
-- #define XK_Armenian_TCHE              0x1000543  /* U+0543 ARMENIAN CAPITAL LETTER CHEH */
pattern XK_Armenian_TCHE = MkKeySymbol #const XK_Armenian_TCHE
-- #define XK_Armenian_tche              0x1000573  /* U+0573 ARMENIAN SMALL LETTER CHEH */
pattern XK_Armenian_tche = MkKeySymbol #const XK_Armenian_tche
-- #define XK_Armenian_MEN               0x1000544  /* U+0544 ARMENIAN CAPITAL LETTER MEN */
pattern XK_Armenian_MEN = MkKeySymbol #const XK_Armenian_MEN
-- #define XK_Armenian_men               0x1000574  /* U+0574 ARMENIAN SMALL LETTER MEN */
pattern XK_Armenian_men = MkKeySymbol #const XK_Armenian_men
-- #define XK_Armenian_HI                0x1000545  /* U+0545 ARMENIAN CAPITAL LETTER YI */
pattern XK_Armenian_HI = MkKeySymbol #const XK_Armenian_HI
-- #define XK_Armenian_hi                0x1000575  /* U+0575 ARMENIAN SMALL LETTER YI */
pattern XK_Armenian_hi = MkKeySymbol #const XK_Armenian_hi
-- #define XK_Armenian_NU                0x1000546  /* U+0546 ARMENIAN CAPITAL LETTER NOW */
pattern XK_Armenian_NU = MkKeySymbol #const XK_Armenian_NU
-- #define XK_Armenian_nu                0x1000576  /* U+0576 ARMENIAN SMALL LETTER NOW */
pattern XK_Armenian_nu = MkKeySymbol #const XK_Armenian_nu
-- #define XK_Armenian_SHA               0x1000547  /* U+0547 ARMENIAN CAPITAL LETTER SHA */
pattern XK_Armenian_SHA = MkKeySymbol #const XK_Armenian_SHA
-- #define XK_Armenian_sha               0x1000577  /* U+0577 ARMENIAN SMALL LETTER SHA */
pattern XK_Armenian_sha = MkKeySymbol #const XK_Armenian_sha
-- #define XK_Armenian_VO                0x1000548  /* U+0548 ARMENIAN CAPITAL LETTER VO */
pattern XK_Armenian_VO = MkKeySymbol #const XK_Armenian_VO
-- #define XK_Armenian_vo                0x1000578  /* U+0578 ARMENIAN SMALL LETTER VO */
pattern XK_Armenian_vo = MkKeySymbol #const XK_Armenian_vo
-- #define XK_Armenian_CHA               0x1000549  /* U+0549 ARMENIAN CAPITAL LETTER CHA */
pattern XK_Armenian_CHA = MkKeySymbol #const XK_Armenian_CHA
-- #define XK_Armenian_cha               0x1000579  /* U+0579 ARMENIAN SMALL LETTER CHA */
pattern XK_Armenian_cha = MkKeySymbol #const XK_Armenian_cha
-- #define XK_Armenian_PE                0x100054a  /* U+054A ARMENIAN CAPITAL LETTER PEH */
pattern XK_Armenian_PE = MkKeySymbol #const XK_Armenian_PE
-- #define XK_Armenian_pe                0x100057a  /* U+057A ARMENIAN SMALL LETTER PEH */
pattern XK_Armenian_pe = MkKeySymbol #const XK_Armenian_pe
-- #define XK_Armenian_JE                0x100054b  /* U+054B ARMENIAN CAPITAL LETTER JHEH */
pattern XK_Armenian_JE = MkKeySymbol #const XK_Armenian_JE
-- #define XK_Armenian_je                0x100057b  /* U+057B ARMENIAN SMALL LETTER JHEH */
pattern XK_Armenian_je = MkKeySymbol #const XK_Armenian_je
-- #define XK_Armenian_RA                0x100054c  /* U+054C ARMENIAN CAPITAL LETTER RA */
pattern XK_Armenian_RA = MkKeySymbol #const XK_Armenian_RA
-- #define XK_Armenian_ra                0x100057c  /* U+057C ARMENIAN SMALL LETTER RA */
pattern XK_Armenian_ra = MkKeySymbol #const XK_Armenian_ra
-- #define XK_Armenian_SE                0x100054d  /* U+054D ARMENIAN CAPITAL LETTER SEH */
pattern XK_Armenian_SE = MkKeySymbol #const XK_Armenian_SE
-- #define XK_Armenian_se                0x100057d  /* U+057D ARMENIAN SMALL LETTER SEH */
pattern XK_Armenian_se = MkKeySymbol #const XK_Armenian_se
-- #define XK_Armenian_VEV               0x100054e  /* U+054E ARMENIAN CAPITAL LETTER VEW */
pattern XK_Armenian_VEV = MkKeySymbol #const XK_Armenian_VEV
-- #define XK_Armenian_vev               0x100057e  /* U+057E ARMENIAN SMALL LETTER VEW */
pattern XK_Armenian_vev = MkKeySymbol #const XK_Armenian_vev
-- #define XK_Armenian_TYUN              0x100054f  /* U+054F ARMENIAN CAPITAL LETTER TIWN */
pattern XK_Armenian_TYUN = MkKeySymbol #const XK_Armenian_TYUN
-- #define XK_Armenian_tyun              0x100057f  /* U+057F ARMENIAN SMALL LETTER TIWN */
pattern XK_Armenian_tyun = MkKeySymbol #const XK_Armenian_tyun
-- #define XK_Armenian_RE                0x1000550  /* U+0550 ARMENIAN CAPITAL LETTER REH */
pattern XK_Armenian_RE = MkKeySymbol #const XK_Armenian_RE
-- #define XK_Armenian_re                0x1000580  /* U+0580 ARMENIAN SMALL LETTER REH */
pattern XK_Armenian_re = MkKeySymbol #const XK_Armenian_re
-- #define XK_Armenian_TSO               0x1000551  /* U+0551 ARMENIAN CAPITAL LETTER CO */
pattern XK_Armenian_TSO = MkKeySymbol #const XK_Armenian_TSO
-- #define XK_Armenian_tso               0x1000581  /* U+0581 ARMENIAN SMALL LETTER CO */
pattern XK_Armenian_tso = MkKeySymbol #const XK_Armenian_tso
-- #define XK_Armenian_VYUN              0x1000552  /* U+0552 ARMENIAN CAPITAL LETTER YIWN */
pattern XK_Armenian_VYUN = MkKeySymbol #const XK_Armenian_VYUN
-- #define XK_Armenian_vyun              0x1000582  /* U+0582 ARMENIAN SMALL LETTER YIWN */
pattern XK_Armenian_vyun = MkKeySymbol #const XK_Armenian_vyun
-- #define XK_Armenian_PYUR              0x1000553  /* U+0553 ARMENIAN CAPITAL LETTER PIWR */
pattern XK_Armenian_PYUR = MkKeySymbol #const XK_Armenian_PYUR
-- #define XK_Armenian_pyur              0x1000583  /* U+0583 ARMENIAN SMALL LETTER PIWR */
pattern XK_Armenian_pyur = MkKeySymbol #const XK_Armenian_pyur
-- #define XK_Armenian_KE                0x1000554  /* U+0554 ARMENIAN CAPITAL LETTER KEH */
pattern XK_Armenian_KE = MkKeySymbol #const XK_Armenian_KE
-- #define XK_Armenian_ke                0x1000584  /* U+0584 ARMENIAN SMALL LETTER KEH */
pattern XK_Armenian_ke = MkKeySymbol #const XK_Armenian_ke
-- #define XK_Armenian_O                 0x1000555  /* U+0555 ARMENIAN CAPITAL LETTER OH */
pattern XK_Armenian_O = MkKeySymbol #const XK_Armenian_O
-- #define XK_Armenian_o                 0x1000585  /* U+0585 ARMENIAN SMALL LETTER OH */
pattern XK_Armenian_o = MkKeySymbol #const XK_Armenian_o
-- #define XK_Armenian_FE                0x1000556  /* U+0556 ARMENIAN CAPITAL LETTER FEH */
pattern XK_Armenian_FE = MkKeySymbol #const XK_Armenian_FE
-- #define XK_Armenian_fe                0x1000586  /* U+0586 ARMENIAN SMALL LETTER FEH */
pattern XK_Armenian_fe = MkKeySymbol #const XK_Armenian_fe
-- #define XK_Armenian_apostrophe        0x100055a  /* U+055A ARMENIAN APOSTROPHE */
pattern XK_Armenian_apostrophe = MkKeySymbol #const XK_Armenian_apostrophe
-- #define XK_Georgian_an                0x10010d0  /* U+10D0 GEORGIAN LETTER AN */
pattern XK_Georgian_an = MkKeySymbol #const XK_Georgian_an
-- #define XK_Georgian_ban               0x10010d1  /* U+10D1 GEORGIAN LETTER BAN */
pattern XK_Georgian_ban = MkKeySymbol #const XK_Georgian_ban
-- #define XK_Georgian_gan               0x10010d2  /* U+10D2 GEORGIAN LETTER GAN */
pattern XK_Georgian_gan = MkKeySymbol #const XK_Georgian_gan
-- #define XK_Georgian_don               0x10010d3  /* U+10D3 GEORGIAN LETTER DON */
pattern XK_Georgian_don = MkKeySymbol #const XK_Georgian_don
-- #define XK_Georgian_en                0x10010d4  /* U+10D4 GEORGIAN LETTER EN */
pattern XK_Georgian_en = MkKeySymbol #const XK_Georgian_en
-- #define XK_Georgian_vin               0x10010d5  /* U+10D5 GEORGIAN LETTER VIN */
pattern XK_Georgian_vin = MkKeySymbol #const XK_Georgian_vin
-- #define XK_Georgian_zen               0x10010d6  /* U+10D6 GEORGIAN LETTER ZEN */
pattern XK_Georgian_zen = MkKeySymbol #const XK_Georgian_zen
-- #define XK_Georgian_tan               0x10010d7  /* U+10D7 GEORGIAN LETTER TAN */
pattern XK_Georgian_tan = MkKeySymbol #const XK_Georgian_tan
-- #define XK_Georgian_in                0x10010d8  /* U+10D8 GEORGIAN LETTER IN */
pattern XK_Georgian_in = MkKeySymbol #const XK_Georgian_in
-- #define XK_Georgian_kan               0x10010d9  /* U+10D9 GEORGIAN LETTER KAN */
pattern XK_Georgian_kan = MkKeySymbol #const XK_Georgian_kan
-- #define XK_Georgian_las               0x10010da  /* U+10DA GEORGIAN LETTER LAS */
pattern XK_Georgian_las = MkKeySymbol #const XK_Georgian_las
-- #define XK_Georgian_man               0x10010db  /* U+10DB GEORGIAN LETTER MAN */
pattern XK_Georgian_man = MkKeySymbol #const XK_Georgian_man
-- #define XK_Georgian_nar               0x10010dc  /* U+10DC GEORGIAN LETTER NAR */
pattern XK_Georgian_nar = MkKeySymbol #const XK_Georgian_nar
-- #define XK_Georgian_on                0x10010dd  /* U+10DD GEORGIAN LETTER ON */
pattern XK_Georgian_on = MkKeySymbol #const XK_Georgian_on
-- #define XK_Georgian_par               0x10010de  /* U+10DE GEORGIAN LETTER PAR */
pattern XK_Georgian_par = MkKeySymbol #const XK_Georgian_par
-- #define XK_Georgian_zhar              0x10010df  /* U+10DF GEORGIAN LETTER ZHAR */
pattern XK_Georgian_zhar = MkKeySymbol #const XK_Georgian_zhar
-- #define XK_Georgian_rae               0x10010e0  /* U+10E0 GEORGIAN LETTER RAE */
pattern XK_Georgian_rae = MkKeySymbol #const XK_Georgian_rae
-- #define XK_Georgian_san               0x10010e1  /* U+10E1 GEORGIAN LETTER SAN */
pattern XK_Georgian_san = MkKeySymbol #const XK_Georgian_san
-- #define XK_Georgian_tar               0x10010e2  /* U+10E2 GEORGIAN LETTER TAR */
pattern XK_Georgian_tar = MkKeySymbol #const XK_Georgian_tar
-- #define XK_Georgian_un                0x10010e3  /* U+10E3 GEORGIAN LETTER UN */
pattern XK_Georgian_un = MkKeySymbol #const XK_Georgian_un
-- #define XK_Georgian_phar              0x10010e4  /* U+10E4 GEORGIAN LETTER PHAR */
pattern XK_Georgian_phar = MkKeySymbol #const XK_Georgian_phar
-- #define XK_Georgian_khar              0x10010e5  /* U+10E5 GEORGIAN LETTER KHAR */
pattern XK_Georgian_khar = MkKeySymbol #const XK_Georgian_khar
-- #define XK_Georgian_ghan              0x10010e6  /* U+10E6 GEORGIAN LETTER GHAN */
pattern XK_Georgian_ghan = MkKeySymbol #const XK_Georgian_ghan
-- #define XK_Georgian_qar               0x10010e7  /* U+10E7 GEORGIAN LETTER QAR */
pattern XK_Georgian_qar = MkKeySymbol #const XK_Georgian_qar
-- #define XK_Georgian_shin              0x10010e8  /* U+10E8 GEORGIAN LETTER SHIN */
pattern XK_Georgian_shin = MkKeySymbol #const XK_Georgian_shin
-- #define XK_Georgian_chin              0x10010e9  /* U+10E9 GEORGIAN LETTER CHIN */
pattern XK_Georgian_chin = MkKeySymbol #const XK_Georgian_chin
-- #define XK_Georgian_can               0x10010ea  /* U+10EA GEORGIAN LETTER CAN */
pattern XK_Georgian_can = MkKeySymbol #const XK_Georgian_can
-- #define XK_Georgian_jil               0x10010eb  /* U+10EB GEORGIAN LETTER JIL */
pattern XK_Georgian_jil = MkKeySymbol #const XK_Georgian_jil
-- #define XK_Georgian_cil               0x10010ec  /* U+10EC GEORGIAN LETTER CIL */
pattern XK_Georgian_cil = MkKeySymbol #const XK_Georgian_cil
-- #define XK_Georgian_char              0x10010ed  /* U+10ED GEORGIAN LETTER CHAR */
pattern XK_Georgian_char = MkKeySymbol #const XK_Georgian_char
-- #define XK_Georgian_xan               0x10010ee  /* U+10EE GEORGIAN LETTER XAN */
pattern XK_Georgian_xan = MkKeySymbol #const XK_Georgian_xan
-- #define XK_Georgian_jhan              0x10010ef  /* U+10EF GEORGIAN LETTER JHAN */
pattern XK_Georgian_jhan = MkKeySymbol #const XK_Georgian_jhan
-- #define XK_Georgian_hae               0x10010f0  /* U+10F0 GEORGIAN LETTER HAE */
pattern XK_Georgian_hae = MkKeySymbol #const XK_Georgian_hae
-- #define XK_Georgian_he                0x10010f1  /* U+10F1 GEORGIAN LETTER HE */
pattern XK_Georgian_he = MkKeySymbol #const XK_Georgian_he
-- #define XK_Georgian_hie               0x10010f2  /* U+10F2 GEORGIAN LETTER HIE */
pattern XK_Georgian_hie = MkKeySymbol #const XK_Georgian_hie
-- #define XK_Georgian_we                0x10010f3  /* U+10F3 GEORGIAN LETTER WE */
pattern XK_Georgian_we = MkKeySymbol #const XK_Georgian_we
-- #define XK_Georgian_har               0x10010f4  /* U+10F4 GEORGIAN LETTER HAR */
pattern XK_Georgian_har = MkKeySymbol #const XK_Georgian_har
-- #define XK_Georgian_hoe               0x10010f5  /* U+10F5 GEORGIAN LETTER HOE */
pattern XK_Georgian_hoe = MkKeySymbol #const XK_Georgian_hoe
-- #define XK_Georgian_fi                0x10010f6  /* U+10F6 GEORGIAN LETTER FI */
pattern XK_Georgian_fi = MkKeySymbol #const XK_Georgian_fi
-- #define XK_Xabovedot                  0x1001e8a  /* U+1E8A LATIN CAPITAL LETTER X WITH DOT ABOVE */
pattern XK_Xabovedot = MkKeySymbol #const XK_Xabovedot
-- #define XK_Ibreve                     0x100012c  /* U+012C LATIN CAPITAL LETTER I WITH BREVE */
pattern XK_Ibreve = MkKeySymbol #const XK_Ibreve
-- #define XK_Zstroke                    0x10001b5  /* U+01B5 LATIN CAPITAL LETTER Z WITH STROKE */
pattern XK_Zstroke = MkKeySymbol #const XK_Zstroke
-- #define XK_Gcaron                     0x10001e6  /* U+01E6 LATIN CAPITAL LETTER G WITH CARON */
pattern XK_Gcaron = MkKeySymbol #const XK_Gcaron
-- #define XK_Ocaron                     0x10001d1  /* U+01D2 LATIN CAPITAL LETTER O WITH CARON */
pattern XK_Ocaron = MkKeySymbol #const XK_Ocaron
-- #define XK_Obarred                    0x100019f  /* U+019F LATIN CAPITAL LETTER O WITH MIDDLE TILDE */
pattern XK_Obarred = MkKeySymbol #const XK_Obarred
-- #define XK_xabovedot                  0x1001e8b  /* U+1E8B LATIN SMALL LETTER X WITH DOT ABOVE */
pattern XK_xabovedot = MkKeySymbol #const XK_xabovedot
-- #define XK_ibreve                     0x100012d  /* U+012D LATIN SMALL LETTER I WITH BREVE */
pattern XK_ibreve = MkKeySymbol #const XK_ibreve
-- #define XK_zstroke                    0x10001b6  /* U+01B6 LATIN SMALL LETTER Z WITH STROKE */
pattern XK_zstroke = MkKeySymbol #const XK_zstroke
-- #define XK_gcaron                     0x10001e7  /* U+01E7 LATIN SMALL LETTER G WITH CARON */
pattern XK_gcaron = MkKeySymbol #const XK_gcaron
-- #define XK_ocaron                     0x10001d2  /* U+01D2 LATIN SMALL LETTER O WITH CARON */
pattern XK_ocaron = MkKeySymbol #const XK_ocaron
-- #define XK_obarred                    0x1000275  /* U+0275 LATIN SMALL LETTER BARRED O */
pattern XK_obarred = MkKeySymbol #const XK_obarred
-- #define XK_SCHWA                      0x100018f  /* U+018F LATIN CAPITAL LETTER SCHWA */
pattern XK_SCHWA = MkKeySymbol #const XK_SCHWA
-- #define XK_schwa                      0x1000259  /* U+0259 LATIN SMALL LETTER SCHWA */
pattern XK_schwa = MkKeySymbol #const XK_schwa
-- #define XK_EZH                        0x10001b7  /* U+01B7 LATIN CAPITAL LETTER EZH */
pattern XK_EZH = MkKeySymbol #const XK_EZH
-- #define XK_ezh                        0x1000292  /* U+0292 LATIN SMALL LETTER EZH */
pattern XK_ezh = MkKeySymbol #const XK_ezh
-- #define XK_Lbelowdot                  0x1001e36  /* U+1E36 LATIN CAPITAL LETTER L WITH DOT BELOW */
pattern XK_Lbelowdot = MkKeySymbol #const XK_Lbelowdot
-- #define XK_lbelowdot                  0x1001e37  /* U+1E37 LATIN SMALL LETTER L WITH DOT BELOW */
pattern XK_lbelowdot = MkKeySymbol #const XK_lbelowdot
-- #define XK_Abelowdot                  0x1001ea0  /* U+1EA0 LATIN CAPITAL LETTER A WITH DOT BELOW */
pattern XK_Abelowdot = MkKeySymbol #const XK_Abelowdot
-- #define XK_abelowdot                  0x1001ea1  /* U+1EA1 LATIN SMALL LETTER A WITH DOT BELOW */
pattern XK_abelowdot = MkKeySymbol #const XK_abelowdot
-- #define XK_Ahook                      0x1001ea2  /* U+1EA2 LATIN CAPITAL LETTER A WITH HOOK ABOVE */
pattern XK_Ahook = MkKeySymbol #const XK_Ahook
-- #define XK_ahook                      0x1001ea3  /* U+1EA3 LATIN SMALL LETTER A WITH HOOK ABOVE */
pattern XK_ahook = MkKeySymbol #const XK_ahook
-- #define XK_Acircumflexacute           0x1001ea4  /* U+1EA4 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE */
pattern XK_Acircumflexacute = MkKeySymbol #const XK_Acircumflexacute
-- #define XK_acircumflexacute           0x1001ea5  /* U+1EA5 LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE */
pattern XK_acircumflexacute = MkKeySymbol #const XK_acircumflexacute
-- #define XK_Acircumflexgrave           0x1001ea6  /* U+1EA6 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE */
pattern XK_Acircumflexgrave = MkKeySymbol #const XK_Acircumflexgrave
-- #define XK_acircumflexgrave           0x1001ea7  /* U+1EA7 LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE */
pattern XK_acircumflexgrave = MkKeySymbol #const XK_acircumflexgrave
-- #define XK_Acircumflexhook            0x1001ea8  /* U+1EA8 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE */
pattern XK_Acircumflexhook = MkKeySymbol #const XK_Acircumflexhook
-- #define XK_acircumflexhook            0x1001ea9  /* U+1EA9 LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE */
pattern XK_acircumflexhook = MkKeySymbol #const XK_acircumflexhook
-- #define XK_Acircumflextilde           0x1001eaa  /* U+1EAA LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE */
pattern XK_Acircumflextilde = MkKeySymbol #const XK_Acircumflextilde
-- #define XK_acircumflextilde           0x1001eab  /* U+1EAB LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE */
pattern XK_acircumflextilde = MkKeySymbol #const XK_acircumflextilde
-- #define XK_Acircumflexbelowdot        0x1001eac  /* U+1EAC LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW */
pattern XK_Acircumflexbelowdot = MkKeySymbol #const XK_Acircumflexbelowdot
-- #define XK_acircumflexbelowdot        0x1001ead  /* U+1EAD LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW */
pattern XK_acircumflexbelowdot = MkKeySymbol #const XK_acircumflexbelowdot
-- #define XK_Abreveacute                0x1001eae  /* U+1EAE LATIN CAPITAL LETTER A WITH BREVE AND ACUTE */
pattern XK_Abreveacute = MkKeySymbol #const XK_Abreveacute
-- #define XK_abreveacute                0x1001eaf  /* U+1EAF LATIN SMALL LETTER A WITH BREVE AND ACUTE */
pattern XK_abreveacute = MkKeySymbol #const XK_abreveacute
-- #define XK_Abrevegrave                0x1001eb0  /* U+1EB0 LATIN CAPITAL LETTER A WITH BREVE AND GRAVE */
pattern XK_Abrevegrave = MkKeySymbol #const XK_Abrevegrave
-- #define XK_abrevegrave                0x1001eb1  /* U+1EB1 LATIN SMALL LETTER A WITH BREVE AND GRAVE */
pattern XK_abrevegrave = MkKeySymbol #const XK_abrevegrave
-- #define XK_Abrevehook                 0x1001eb2  /* U+1EB2 LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE */
pattern XK_Abrevehook = MkKeySymbol #const XK_Abrevehook
-- #define XK_abrevehook                 0x1001eb3  /* U+1EB3 LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE */
pattern XK_abrevehook = MkKeySymbol #const XK_abrevehook
-- #define XK_Abrevetilde                0x1001eb4  /* U+1EB4 LATIN CAPITAL LETTER A WITH BREVE AND TILDE */
pattern XK_Abrevetilde = MkKeySymbol #const XK_Abrevetilde
-- #define XK_abrevetilde                0x1001eb5  /* U+1EB5 LATIN SMALL LETTER A WITH BREVE AND TILDE */
pattern XK_abrevetilde = MkKeySymbol #const XK_abrevetilde
-- #define XK_Abrevebelowdot             0x1001eb6  /* U+1EB6 LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW */
pattern XK_Abrevebelowdot = MkKeySymbol #const XK_Abrevebelowdot
-- #define XK_abrevebelowdot             0x1001eb7  /* U+1EB7 LATIN SMALL LETTER A WITH BREVE AND DOT BELOW */
pattern XK_abrevebelowdot = MkKeySymbol #const XK_abrevebelowdot
-- #define XK_Ebelowdot                  0x1001eb8  /* U+1EB8 LATIN CAPITAL LETTER E WITH DOT BELOW */
pattern XK_Ebelowdot = MkKeySymbol #const XK_Ebelowdot
-- #define XK_ebelowdot                  0x1001eb9  /* U+1EB9 LATIN SMALL LETTER E WITH DOT BELOW */
pattern XK_ebelowdot = MkKeySymbol #const XK_ebelowdot
-- #define XK_Ehook                      0x1001eba  /* U+1EBA LATIN CAPITAL LETTER E WITH HOOK ABOVE */
pattern XK_Ehook = MkKeySymbol #const XK_Ehook
-- #define XK_ehook                      0x1001ebb  /* U+1EBB LATIN SMALL LETTER E WITH HOOK ABOVE */
pattern XK_ehook = MkKeySymbol #const XK_ehook
-- #define XK_Etilde                     0x1001ebc  /* U+1EBC LATIN CAPITAL LETTER E WITH TILDE */
pattern XK_Etilde = MkKeySymbol #const XK_Etilde
-- #define XK_etilde                     0x1001ebd  /* U+1EBD LATIN SMALL LETTER E WITH TILDE */
pattern XK_etilde = MkKeySymbol #const XK_etilde
-- #define XK_Ecircumflexacute           0x1001ebe  /* U+1EBE LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE */
pattern XK_Ecircumflexacute = MkKeySymbol #const XK_Ecircumflexacute
-- #define XK_ecircumflexacute           0x1001ebf  /* U+1EBF LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE */
pattern XK_ecircumflexacute = MkKeySymbol #const XK_ecircumflexacute
-- #define XK_Ecircumflexgrave           0x1001ec0  /* U+1EC0 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE */
pattern XK_Ecircumflexgrave = MkKeySymbol #const XK_Ecircumflexgrave
-- #define XK_ecircumflexgrave           0x1001ec1  /* U+1EC1 LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE */
pattern XK_ecircumflexgrave = MkKeySymbol #const XK_ecircumflexgrave
-- #define XK_Ecircumflexhook            0x1001ec2  /* U+1EC2 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE */
pattern XK_Ecircumflexhook = MkKeySymbol #const XK_Ecircumflexhook
-- #define XK_ecircumflexhook            0x1001ec3  /* U+1EC3 LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE */
pattern XK_ecircumflexhook = MkKeySymbol #const XK_ecircumflexhook
-- #define XK_Ecircumflextilde           0x1001ec4  /* U+1EC4 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE */
pattern XK_Ecircumflextilde = MkKeySymbol #const XK_Ecircumflextilde
-- #define XK_ecircumflextilde           0x1001ec5  /* U+1EC5 LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE */
pattern XK_ecircumflextilde = MkKeySymbol #const XK_ecircumflextilde
-- #define XK_Ecircumflexbelowdot        0x1001ec6  /* U+1EC6 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW */
pattern XK_Ecircumflexbelowdot = MkKeySymbol #const XK_Ecircumflexbelowdot
-- #define XK_ecircumflexbelowdot        0x1001ec7  /* U+1EC7 LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW */
pattern XK_ecircumflexbelowdot = MkKeySymbol #const XK_ecircumflexbelowdot
-- #define XK_Ihook                      0x1001ec8  /* U+1EC8 LATIN CAPITAL LETTER I WITH HOOK ABOVE */
pattern XK_Ihook = MkKeySymbol #const XK_Ihook
-- #define XK_ihook                      0x1001ec9  /* U+1EC9 LATIN SMALL LETTER I WITH HOOK ABOVE */
pattern XK_ihook = MkKeySymbol #const XK_ihook
-- #define XK_Ibelowdot                  0x1001eca  /* U+1ECA LATIN CAPITAL LETTER I WITH DOT BELOW */
pattern XK_Ibelowdot = MkKeySymbol #const XK_Ibelowdot
-- #define XK_ibelowdot                  0x1001ecb  /* U+1ECB LATIN SMALL LETTER I WITH DOT BELOW */
pattern XK_ibelowdot = MkKeySymbol #const XK_ibelowdot
-- #define XK_Obelowdot                  0x1001ecc  /* U+1ECC LATIN CAPITAL LETTER O WITH DOT BELOW */
pattern XK_Obelowdot = MkKeySymbol #const XK_Obelowdot
-- #define XK_obelowdot                  0x1001ecd  /* U+1ECD LATIN SMALL LETTER O WITH DOT BELOW */
pattern XK_obelowdot = MkKeySymbol #const XK_obelowdot
-- #define XK_Ohook                      0x1001ece  /* U+1ECE LATIN CAPITAL LETTER O WITH HOOK ABOVE */
pattern XK_Ohook = MkKeySymbol #const XK_Ohook
-- #define XK_ohook                      0x1001ecf  /* U+1ECF LATIN SMALL LETTER O WITH HOOK ABOVE */
pattern XK_ohook = MkKeySymbol #const XK_ohook
-- #define XK_Ocircumflexacute           0x1001ed0  /* U+1ED0 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE */
pattern XK_Ocircumflexacute = MkKeySymbol #const XK_Ocircumflexacute
-- #define XK_ocircumflexacute           0x1001ed1  /* U+1ED1 LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE */
pattern XK_ocircumflexacute = MkKeySymbol #const XK_ocircumflexacute
-- #define XK_Ocircumflexgrave           0x1001ed2  /* U+1ED2 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE */
pattern XK_Ocircumflexgrave = MkKeySymbol #const XK_Ocircumflexgrave
-- #define XK_ocircumflexgrave           0x1001ed3  /* U+1ED3 LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE */
pattern XK_ocircumflexgrave = MkKeySymbol #const XK_ocircumflexgrave
-- #define XK_Ocircumflexhook            0x1001ed4  /* U+1ED4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE */
pattern XK_Ocircumflexhook = MkKeySymbol #const XK_Ocircumflexhook
-- #define XK_ocircumflexhook            0x1001ed5  /* U+1ED5 LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE */
pattern XK_ocircumflexhook = MkKeySymbol #const XK_ocircumflexhook
-- #define XK_Ocircumflextilde           0x1001ed6  /* U+1ED6 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE */
pattern XK_Ocircumflextilde = MkKeySymbol #const XK_Ocircumflextilde
-- #define XK_ocircumflextilde           0x1001ed7  /* U+1ED7 LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE */
pattern XK_ocircumflextilde = MkKeySymbol #const XK_ocircumflextilde
-- #define XK_Ocircumflexbelowdot        0x1001ed8  /* U+1ED8 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW */
pattern XK_Ocircumflexbelowdot = MkKeySymbol #const XK_Ocircumflexbelowdot
-- #define XK_ocircumflexbelowdot        0x1001ed9  /* U+1ED9 LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW */
pattern XK_ocircumflexbelowdot = MkKeySymbol #const XK_ocircumflexbelowdot
-- #define XK_Ohornacute                 0x1001eda  /* U+1EDA LATIN CAPITAL LETTER O WITH HORN AND ACUTE */
pattern XK_Ohornacute = MkKeySymbol #const XK_Ohornacute
-- #define XK_ohornacute                 0x1001edb  /* U+1EDB LATIN SMALL LETTER O WITH HORN AND ACUTE */
pattern XK_ohornacute = MkKeySymbol #const XK_ohornacute
-- #define XK_Ohorngrave                 0x1001edc  /* U+1EDC LATIN CAPITAL LETTER O WITH HORN AND GRAVE */
pattern XK_Ohorngrave = MkKeySymbol #const XK_Ohorngrave
-- #define XK_ohorngrave                 0x1001edd  /* U+1EDD LATIN SMALL LETTER O WITH HORN AND GRAVE */
pattern XK_ohorngrave = MkKeySymbol #const XK_ohorngrave
-- #define XK_Ohornhook                  0x1001ede  /* U+1EDE LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE */
pattern XK_Ohornhook = MkKeySymbol #const XK_Ohornhook
-- #define XK_ohornhook                  0x1001edf  /* U+1EDF LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE */
pattern XK_ohornhook = MkKeySymbol #const XK_ohornhook
-- #define XK_Ohorntilde                 0x1001ee0  /* U+1EE0 LATIN CAPITAL LETTER O WITH HORN AND TILDE */
pattern XK_Ohorntilde = MkKeySymbol #const XK_Ohorntilde
-- #define XK_ohorntilde                 0x1001ee1  /* U+1EE1 LATIN SMALL LETTER O WITH HORN AND TILDE */
pattern XK_ohorntilde = MkKeySymbol #const XK_ohorntilde
-- #define XK_Ohornbelowdot              0x1001ee2  /* U+1EE2 LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW */
pattern XK_Ohornbelowdot = MkKeySymbol #const XK_Ohornbelowdot
-- #define XK_ohornbelowdot              0x1001ee3  /* U+1EE3 LATIN SMALL LETTER O WITH HORN AND DOT BELOW */
pattern XK_ohornbelowdot = MkKeySymbol #const XK_ohornbelowdot
-- #define XK_Ubelowdot                  0x1001ee4  /* U+1EE4 LATIN CAPITAL LETTER U WITH DOT BELOW */
pattern XK_Ubelowdot = MkKeySymbol #const XK_Ubelowdot
-- #define XK_ubelowdot                  0x1001ee5  /* U+1EE5 LATIN SMALL LETTER U WITH DOT BELOW */
pattern XK_ubelowdot = MkKeySymbol #const XK_ubelowdot
-- #define XK_Uhook                      0x1001ee6  /* U+1EE6 LATIN CAPITAL LETTER U WITH HOOK ABOVE */
pattern XK_Uhook = MkKeySymbol #const XK_Uhook
-- #define XK_uhook                      0x1001ee7  /* U+1EE7 LATIN SMALL LETTER U WITH HOOK ABOVE */
pattern XK_uhook = MkKeySymbol #const XK_uhook
-- #define XK_Uhornacute                 0x1001ee8  /* U+1EE8 LATIN CAPITAL LETTER U WITH HORN AND ACUTE */
pattern XK_Uhornacute = MkKeySymbol #const XK_Uhornacute
-- #define XK_uhornacute                 0x1001ee9  /* U+1EE9 LATIN SMALL LETTER U WITH HORN AND ACUTE */
pattern XK_uhornacute = MkKeySymbol #const XK_uhornacute
-- #define XK_Uhorngrave                 0x1001eea  /* U+1EEA LATIN CAPITAL LETTER U WITH HORN AND GRAVE */
pattern XK_Uhorngrave = MkKeySymbol #const XK_Uhorngrave
-- #define XK_uhorngrave                 0x1001eeb  /* U+1EEB LATIN SMALL LETTER U WITH HORN AND GRAVE */
pattern XK_uhorngrave = MkKeySymbol #const XK_uhorngrave
-- #define XK_Uhornhook                  0x1001eec  /* U+1EEC LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE */
pattern XK_Uhornhook = MkKeySymbol #const XK_Uhornhook
-- #define XK_uhornhook                  0x1001eed  /* U+1EED LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE */
pattern XK_uhornhook = MkKeySymbol #const XK_uhornhook
-- #define XK_Uhorntilde                 0x1001eee  /* U+1EEE LATIN CAPITAL LETTER U WITH HORN AND TILDE */
pattern XK_Uhorntilde = MkKeySymbol #const XK_Uhorntilde
-- #define XK_uhorntilde                 0x1001eef  /* U+1EEF LATIN SMALL LETTER U WITH HORN AND TILDE */
pattern XK_uhorntilde = MkKeySymbol #const XK_uhorntilde
-- #define XK_Uhornbelowdot              0x1001ef0  /* U+1EF0 LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW */
pattern XK_Uhornbelowdot = MkKeySymbol #const XK_Uhornbelowdot
-- #define XK_uhornbelowdot              0x1001ef1  /* U+1EF1 LATIN SMALL LETTER U WITH HORN AND DOT BELOW */
pattern XK_uhornbelowdot = MkKeySymbol #const XK_uhornbelowdot
-- #define XK_Ybelowdot                  0x1001ef4  /* U+1EF4 LATIN CAPITAL LETTER Y WITH DOT BELOW */
pattern XK_Ybelowdot = MkKeySymbol #const XK_Ybelowdot
-- #define XK_ybelowdot                  0x1001ef5  /* U+1EF5 LATIN SMALL LETTER Y WITH DOT BELOW */
pattern XK_ybelowdot = MkKeySymbol #const XK_ybelowdot
-- #define XK_Yhook                      0x1001ef6  /* U+1EF6 LATIN CAPITAL LETTER Y WITH HOOK ABOVE */
pattern XK_Yhook = MkKeySymbol #const XK_Yhook
-- #define XK_yhook                      0x1001ef7  /* U+1EF7 LATIN SMALL LETTER Y WITH HOOK ABOVE */
pattern XK_yhook = MkKeySymbol #const XK_yhook
-- #define XK_Ytilde                     0x1001ef8  /* U+1EF8 LATIN CAPITAL LETTER Y WITH TILDE */
pattern XK_Ytilde = MkKeySymbol #const XK_Ytilde
-- #define XK_ytilde                     0x1001ef9  /* U+1EF9 LATIN SMALL LETTER Y WITH TILDE */
pattern XK_ytilde = MkKeySymbol #const XK_ytilde
-- #define XK_Ohorn                      0x10001a0  /* U+01A0 LATIN CAPITAL LETTER O WITH HORN */
pattern XK_Ohorn = MkKeySymbol #const XK_Ohorn
-- #define XK_ohorn                      0x10001a1  /* U+01A1 LATIN SMALL LETTER O WITH HORN */
pattern XK_ohorn = MkKeySymbol #const XK_ohorn
-- #define XK_Uhorn                      0x10001af  /* U+01AF LATIN CAPITAL LETTER U WITH HORN */
pattern XK_Uhorn = MkKeySymbol #const XK_Uhorn
-- #define XK_uhorn                      0x10001b0  /* U+01B0 LATIN SMALL LETTER U WITH HORN */
pattern XK_uhorn = MkKeySymbol #const XK_uhorn
-- #define XK_EcuSign                    0x10020a0  /* U+20A0 EURO-CURRENCY SIGN */
pattern XK_EcuSign = MkKeySymbol #const XK_EcuSign
-- #define XK_ColonSign                  0x10020a1  /* U+20A1 COLON SIGN */
pattern XK_ColonSign = MkKeySymbol #const XK_ColonSign
-- #define XK_CruzeiroSign               0x10020a2  /* U+20A2 CRUZEIRO SIGN */
pattern XK_CruzeiroSign = MkKeySymbol #const XK_CruzeiroSign
-- #define XK_FFrancSign                 0x10020a3  /* U+20A3 FRENCH FRANC SIGN */
pattern XK_FFrancSign = MkKeySymbol #const XK_FFrancSign
-- #define XK_LiraSign                   0x10020a4  /* U+20A4 LIRA SIGN */
pattern XK_LiraSign = MkKeySymbol #const XK_LiraSign
-- #define XK_MillSign                   0x10020a5  /* U+20A5 MILL SIGN */
pattern XK_MillSign = MkKeySymbol #const XK_MillSign
-- #define XK_NairaSign                  0x10020a6  /* U+20A6 NAIRA SIGN */
pattern XK_NairaSign = MkKeySymbol #const XK_NairaSign
-- #define XK_PesetaSign                 0x10020a7  /* U+20A7 PESETA SIGN */
pattern XK_PesetaSign = MkKeySymbol #const XK_PesetaSign
-- #define XK_RupeeSign                  0x10020a8  /* U+20A8 RUPEE SIGN */
pattern XK_RupeeSign = MkKeySymbol #const XK_RupeeSign
-- #define XK_WonSign                    0x10020a9  /* U+20A9 WON SIGN */
pattern XK_WonSign = MkKeySymbol #const XK_WonSign
-- #define XK_NewSheqelSign              0x10020aa  /* U+20AA NEW SHEQEL SIGN */
pattern XK_NewSheqelSign = MkKeySymbol #const XK_NewSheqelSign
-- #define XK_DongSign                   0x10020ab  /* U+20AB DONG SIGN */
pattern XK_DongSign = MkKeySymbol #const XK_DongSign
-- #define XK_EuroSign                      0x20ac  /* U+20AC EURO SIGN */
pattern XK_EuroSign = MkKeySymbol #const XK_EuroSign
-- #define XK_zerosuperior               0x1002070  /* U+2070 SUPERSCRIPT ZERO */
pattern XK_zerosuperior = MkKeySymbol #const XK_zerosuperior
-- #define XK_foursuperior               0x1002074  /* U+2074 SUPERSCRIPT FOUR */
pattern XK_foursuperior = MkKeySymbol #const XK_foursuperior
-- #define XK_fivesuperior               0x1002075  /* U+2075 SUPERSCRIPT FIVE */
pattern XK_fivesuperior = MkKeySymbol #const XK_fivesuperior
-- #define XK_sixsuperior                0x1002076  /* U+2076 SUPERSCRIPT SIX */
pattern XK_sixsuperior = MkKeySymbol #const XK_sixsuperior
-- #define XK_sevensuperior              0x1002077  /* U+2077 SUPERSCRIPT SEVEN */
pattern XK_sevensuperior = MkKeySymbol #const XK_sevensuperior
-- #define XK_eightsuperior              0x1002078  /* U+2078 SUPERSCRIPT EIGHT */
pattern XK_eightsuperior = MkKeySymbol #const XK_eightsuperior
-- #define XK_ninesuperior               0x1002079  /* U+2079 SUPERSCRIPT NINE */
pattern XK_ninesuperior = MkKeySymbol #const XK_ninesuperior
-- #define XK_zerosubscript              0x1002080  /* U+2080 SUBSCRIPT ZERO */
pattern XK_zerosubscript = MkKeySymbol #const XK_zerosubscript
-- #define XK_onesubscript               0x1002081  /* U+2081 SUBSCRIPT ONE */
pattern XK_onesubscript = MkKeySymbol #const XK_onesubscript
-- #define XK_twosubscript               0x1002082  /* U+2082 SUBSCRIPT TWO */
pattern XK_twosubscript = MkKeySymbol #const XK_twosubscript
-- #define XK_threesubscript             0x1002083  /* U+2083 SUBSCRIPT THREE */
pattern XK_threesubscript = MkKeySymbol #const XK_threesubscript
-- #define XK_foursubscript              0x1002084  /* U+2084 SUBSCRIPT FOUR */
pattern XK_foursubscript = MkKeySymbol #const XK_foursubscript
-- #define XK_fivesubscript              0x1002085  /* U+2085 SUBSCRIPT FIVE */
pattern XK_fivesubscript = MkKeySymbol #const XK_fivesubscript
-- #define XK_sixsubscript               0x1002086  /* U+2086 SUBSCRIPT SIX */
pattern XK_sixsubscript = MkKeySymbol #const XK_sixsubscript
-- #define XK_sevensubscript             0x1002087  /* U+2087 SUBSCRIPT SEVEN */
pattern XK_sevensubscript = MkKeySymbol #const XK_sevensubscript
-- #define XK_eightsubscript             0x1002088  /* U+2088 SUBSCRIPT EIGHT */
pattern XK_eightsubscript = MkKeySymbol #const XK_eightsubscript
-- #define XK_ninesubscript              0x1002089  /* U+2089 SUBSCRIPT NINE */
pattern XK_ninesubscript = MkKeySymbol #const XK_ninesubscript
-- #define XK_partdifferential           0x1002202  /* U+2202 PARTIAL DIFFERENTIAL */
pattern XK_partdifferential = MkKeySymbol #const XK_partdifferential
-- #define XK_emptyset                   0x1002205  /* U+2205 NULL SET */
pattern XK_emptyset = MkKeySymbol #const XK_emptyset
-- #define XK_elementof                  0x1002208  /* U+2208 ELEMENT OF */
pattern XK_elementof = MkKeySymbol #const XK_elementof
-- #define XK_notelementof               0x1002209  /* U+2209 NOT AN ELEMENT OF */
pattern XK_notelementof = MkKeySymbol #const XK_notelementof
-- #define XK_containsas                 0x100220B  /* U+220B CONTAINS AS MEMBER */
pattern XK_containsas = MkKeySymbol #const XK_containsas
-- #define XK_squareroot                 0x100221A  /* U+221A SQUARE ROOT */
pattern XK_squareroot = MkKeySymbol #const XK_squareroot
-- #define XK_cuberoot                   0x100221B  /* U+221B CUBE ROOT */
pattern XK_cuberoot = MkKeySymbol #const XK_cuberoot
-- #define XK_fourthroot                 0x100221C  /* U+221C FOURTH ROOT */
pattern XK_fourthroot = MkKeySymbol #const XK_fourthroot
-- #define XK_dintegral                  0x100222C  /* U+222C DOUBLE INTEGRAL */
pattern XK_dintegral = MkKeySymbol #const XK_dintegral
-- #define XK_tintegral                  0x100222D  /* U+222D TRIPLE INTEGRAL */
pattern XK_tintegral = MkKeySymbol #const XK_tintegral
-- #define XK_because                    0x1002235  /* U+2235 BECAUSE */
pattern XK_because = MkKeySymbol #const XK_because
-- #define XK_approxeq                   0x1002248  /* U+2245 ALMOST EQUAL TO */
pattern XK_approxeq = MkKeySymbol #const XK_approxeq
-- #define XK_notapproxeq                0x1002247  /* U+2247 NOT ALMOST EQUAL TO */
pattern XK_notapproxeq = MkKeySymbol #const XK_notapproxeq
-- #define XK_notidentical               0x1002262  /* U+2262 NOT IDENTICAL TO */
pattern XK_notidentical = MkKeySymbol #const XK_notidentical
-- #define XK_stricteq                   0x1002263  /* U+2263 STRICTLY EQUIVALENT TO */
pattern XK_stricteq = MkKeySymbol #const XK_stricteq
-- #define XK_braille_dot_1                 0xfff1
pattern XK_braille_dot_1 = MkKeySymbol #const XK_braille_dot_1
-- #define XK_braille_dot_2                 0xfff2
pattern XK_braille_dot_2 = MkKeySymbol #const XK_braille_dot_2
-- #define XK_braille_dot_3                 0xfff3
pattern XK_braille_dot_3 = MkKeySymbol #const XK_braille_dot_3
-- #define XK_braille_dot_4                 0xfff4
pattern XK_braille_dot_4 = MkKeySymbol #const XK_braille_dot_4
-- #define XK_braille_dot_5                 0xfff5
pattern XK_braille_dot_5 = MkKeySymbol #const XK_braille_dot_5
-- #define XK_braille_dot_6                 0xfff6
pattern XK_braille_dot_6 = MkKeySymbol #const XK_braille_dot_6
-- #define XK_braille_dot_7                 0xfff7
pattern XK_braille_dot_7 = MkKeySymbol #const XK_braille_dot_7
-- #define XK_braille_dot_8                 0xfff8
pattern XK_braille_dot_8 = MkKeySymbol #const XK_braille_dot_8
-- #define XK_braille_dot_9                 0xfff9
pattern XK_braille_dot_9 = MkKeySymbol #const XK_braille_dot_9
-- #define XK_braille_dot_10                0xfffa
pattern XK_braille_dot_10 = MkKeySymbol #const XK_braille_dot_10
-- #define XK_braille_blank              0x1002800  /* U+2800 BRAILLE PATTERN BLANK */
pattern XK_braille_blank = MkKeySymbol #const XK_braille_blank
-- #define XK_braille_dots_1             0x1002801  /* U+2801 BRAILLE PATTERN DOTS-1 */
pattern XK_braille_dots_1 = MkKeySymbol #const XK_braille_dots_1
-- #define XK_braille_dots_2             0x1002802  /* U+2802 BRAILLE PATTERN DOTS-2 */
pattern XK_braille_dots_2 = MkKeySymbol #const XK_braille_dots_2
-- #define XK_braille_dots_12            0x1002803  /* U+2803 BRAILLE PATTERN DOTS-12 */
pattern XK_braille_dots_12 = MkKeySymbol #const XK_braille_dots_12
-- #define XK_braille_dots_3             0x1002804  /* U+2804 BRAILLE PATTERN DOTS-3 */
pattern XK_braille_dots_3 = MkKeySymbol #const XK_braille_dots_3
-- #define XK_braille_dots_13            0x1002805  /* U+2805 BRAILLE PATTERN DOTS-13 */
pattern XK_braille_dots_13 = MkKeySymbol #const XK_braille_dots_13
-- #define XK_braille_dots_23            0x1002806  /* U+2806 BRAILLE PATTERN DOTS-23 */
pattern XK_braille_dots_23 = MkKeySymbol #const XK_braille_dots_23
-- #define XK_braille_dots_123           0x1002807  /* U+2807 BRAILLE PATTERN DOTS-123 */
pattern XK_braille_dots_123 = MkKeySymbol #const XK_braille_dots_123
-- #define XK_braille_dots_4             0x1002808  /* U+2808 BRAILLE PATTERN DOTS-4 */
pattern XK_braille_dots_4 = MkKeySymbol #const XK_braille_dots_4
-- #define XK_braille_dots_14            0x1002809  /* U+2809 BRAILLE PATTERN DOTS-14 */
pattern XK_braille_dots_14 = MkKeySymbol #const XK_braille_dots_14
-- #define XK_braille_dots_24            0x100280a  /* U+280a BRAILLE PATTERN DOTS-24 */
pattern XK_braille_dots_24 = MkKeySymbol #const XK_braille_dots_24
-- #define XK_braille_dots_124           0x100280b  /* U+280b BRAILLE PATTERN DOTS-124 */
pattern XK_braille_dots_124 = MkKeySymbol #const XK_braille_dots_124
-- #define XK_braille_dots_34            0x100280c  /* U+280c BRAILLE PATTERN DOTS-34 */
pattern XK_braille_dots_34 = MkKeySymbol #const XK_braille_dots_34
-- #define XK_braille_dots_134           0x100280d  /* U+280d BRAILLE PATTERN DOTS-134 */
pattern XK_braille_dots_134 = MkKeySymbol #const XK_braille_dots_134
-- #define XK_braille_dots_234           0x100280e  /* U+280e BRAILLE PATTERN DOTS-234 */
pattern XK_braille_dots_234 = MkKeySymbol #const XK_braille_dots_234
-- #define XK_braille_dots_1234          0x100280f  /* U+280f BRAILLE PATTERN DOTS-1234 */
pattern XK_braille_dots_1234 = MkKeySymbol #const XK_braille_dots_1234
-- #define XK_braille_dots_5             0x1002810  /* U+2810 BRAILLE PATTERN DOTS-5 */
pattern XK_braille_dots_5 = MkKeySymbol #const XK_braille_dots_5
-- #define XK_braille_dots_15            0x1002811  /* U+2811 BRAILLE PATTERN DOTS-15 */
pattern XK_braille_dots_15 = MkKeySymbol #const XK_braille_dots_15
-- #define XK_braille_dots_25            0x1002812  /* U+2812 BRAILLE PATTERN DOTS-25 */
pattern XK_braille_dots_25 = MkKeySymbol #const XK_braille_dots_25
-- #define XK_braille_dots_125           0x1002813  /* U+2813 BRAILLE PATTERN DOTS-125 */
pattern XK_braille_dots_125 = MkKeySymbol #const XK_braille_dots_125
-- #define XK_braille_dots_35            0x1002814  /* U+2814 BRAILLE PATTERN DOTS-35 */
pattern XK_braille_dots_35 = MkKeySymbol #const XK_braille_dots_35
-- #define XK_braille_dots_135           0x1002815  /* U+2815 BRAILLE PATTERN DOTS-135 */
pattern XK_braille_dots_135 = MkKeySymbol #const XK_braille_dots_135
-- #define XK_braille_dots_235           0x1002816  /* U+2816 BRAILLE PATTERN DOTS-235 */
pattern XK_braille_dots_235 = MkKeySymbol #const XK_braille_dots_235
-- #define XK_braille_dots_1235          0x1002817  /* U+2817 BRAILLE PATTERN DOTS-1235 */
pattern XK_braille_dots_1235 = MkKeySymbol #const XK_braille_dots_1235
-- #define XK_braille_dots_45            0x1002818  /* U+2818 BRAILLE PATTERN DOTS-45 */
pattern XK_braille_dots_45 = MkKeySymbol #const XK_braille_dots_45
-- #define XK_braille_dots_145           0x1002819  /* U+2819 BRAILLE PATTERN DOTS-145 */
pattern XK_braille_dots_145 = MkKeySymbol #const XK_braille_dots_145
-- #define XK_braille_dots_245           0x100281a  /* U+281a BRAILLE PATTERN DOTS-245 */
pattern XK_braille_dots_245 = MkKeySymbol #const XK_braille_dots_245
-- #define XK_braille_dots_1245          0x100281b  /* U+281b BRAILLE PATTERN DOTS-1245 */
pattern XK_braille_dots_1245 = MkKeySymbol #const XK_braille_dots_1245
-- #define XK_braille_dots_345           0x100281c  /* U+281c BRAILLE PATTERN DOTS-345 */
pattern XK_braille_dots_345 = MkKeySymbol #const XK_braille_dots_345
-- #define XK_braille_dots_1345          0x100281d  /* U+281d BRAILLE PATTERN DOTS-1345 */
pattern XK_braille_dots_1345 = MkKeySymbol #const XK_braille_dots_1345
-- #define XK_braille_dots_2345          0x100281e  /* U+281e BRAILLE PATTERN DOTS-2345 */
pattern XK_braille_dots_2345 = MkKeySymbol #const XK_braille_dots_2345
-- #define XK_braille_dots_12345         0x100281f  /* U+281f BRAILLE PATTERN DOTS-12345 */
pattern XK_braille_dots_12345 = MkKeySymbol #const XK_braille_dots_12345
-- #define XK_braille_dots_6             0x1002820  /* U+2820 BRAILLE PATTERN DOTS-6 */
pattern XK_braille_dots_6 = MkKeySymbol #const XK_braille_dots_6
-- #define XK_braille_dots_16            0x1002821  /* U+2821 BRAILLE PATTERN DOTS-16 */
pattern XK_braille_dots_16 = MkKeySymbol #const XK_braille_dots_16
-- #define XK_braille_dots_26            0x1002822  /* U+2822 BRAILLE PATTERN DOTS-26 */
pattern XK_braille_dots_26 = MkKeySymbol #const XK_braille_dots_26
-- #define XK_braille_dots_126           0x1002823  /* U+2823 BRAILLE PATTERN DOTS-126 */
pattern XK_braille_dots_126 = MkKeySymbol #const XK_braille_dots_126
-- #define XK_braille_dots_36            0x1002824  /* U+2824 BRAILLE PATTERN DOTS-36 */
pattern XK_braille_dots_36 = MkKeySymbol #const XK_braille_dots_36
-- #define XK_braille_dots_136           0x1002825  /* U+2825 BRAILLE PATTERN DOTS-136 */
pattern XK_braille_dots_136 = MkKeySymbol #const XK_braille_dots_136
-- #define XK_braille_dots_236           0x1002826  /* U+2826 BRAILLE PATTERN DOTS-236 */
pattern XK_braille_dots_236 = MkKeySymbol #const XK_braille_dots_236
-- #define XK_braille_dots_1236          0x1002827  /* U+2827 BRAILLE PATTERN DOTS-1236 */
pattern XK_braille_dots_1236 = MkKeySymbol #const XK_braille_dots_1236
-- #define XK_braille_dots_46            0x1002828  /* U+2828 BRAILLE PATTERN DOTS-46 */
pattern XK_braille_dots_46 = MkKeySymbol #const XK_braille_dots_46
-- #define XK_braille_dots_146           0x1002829  /* U+2829 BRAILLE PATTERN DOTS-146 */
pattern XK_braille_dots_146 = MkKeySymbol #const XK_braille_dots_146
-- #define XK_braille_dots_246           0x100282a  /* U+282a BRAILLE PATTERN DOTS-246 */
pattern XK_braille_dots_246 = MkKeySymbol #const XK_braille_dots_246
-- #define XK_braille_dots_1246          0x100282b  /* U+282b BRAILLE PATTERN DOTS-1246 */
pattern XK_braille_dots_1246 = MkKeySymbol #const XK_braille_dots_1246
-- #define XK_braille_dots_346           0x100282c  /* U+282c BRAILLE PATTERN DOTS-346 */
pattern XK_braille_dots_346 = MkKeySymbol #const XK_braille_dots_346
-- #define XK_braille_dots_1346          0x100282d  /* U+282d BRAILLE PATTERN DOTS-1346 */
pattern XK_braille_dots_1346 = MkKeySymbol #const XK_braille_dots_1346
-- #define XK_braille_dots_2346          0x100282e  /* U+282e BRAILLE PATTERN DOTS-2346 */
pattern XK_braille_dots_2346 = MkKeySymbol #const XK_braille_dots_2346
-- #define XK_braille_dots_12346         0x100282f  /* U+282f BRAILLE PATTERN DOTS-12346 */
pattern XK_braille_dots_12346 = MkKeySymbol #const XK_braille_dots_12346
-- #define XK_braille_dots_56            0x1002830  /* U+2830 BRAILLE PATTERN DOTS-56 */
pattern XK_braille_dots_56 = MkKeySymbol #const XK_braille_dots_56
-- #define XK_braille_dots_156           0x1002831  /* U+2831 BRAILLE PATTERN DOTS-156 */
pattern XK_braille_dots_156 = MkKeySymbol #const XK_braille_dots_156
-- #define XK_braille_dots_256           0x1002832  /* U+2832 BRAILLE PATTERN DOTS-256 */
pattern XK_braille_dots_256 = MkKeySymbol #const XK_braille_dots_256
-- #define XK_braille_dots_1256          0x1002833  /* U+2833 BRAILLE PATTERN DOTS-1256 */
pattern XK_braille_dots_1256 = MkKeySymbol #const XK_braille_dots_1256
-- #define XK_braille_dots_356           0x1002834  /* U+2834 BRAILLE PATTERN DOTS-356 */
pattern XK_braille_dots_356 = MkKeySymbol #const XK_braille_dots_356
-- #define XK_braille_dots_1356          0x1002835  /* U+2835 BRAILLE PATTERN DOTS-1356 */
pattern XK_braille_dots_1356 = MkKeySymbol #const XK_braille_dots_1356
-- #define XK_braille_dots_2356          0x1002836  /* U+2836 BRAILLE PATTERN DOTS-2356 */
pattern XK_braille_dots_2356 = MkKeySymbol #const XK_braille_dots_2356
-- #define XK_braille_dots_12356         0x1002837  /* U+2837 BRAILLE PATTERN DOTS-12356 */
pattern XK_braille_dots_12356 = MkKeySymbol #const XK_braille_dots_12356
-- #define XK_braille_dots_456           0x1002838  /* U+2838 BRAILLE PATTERN DOTS-456 */
pattern XK_braille_dots_456 = MkKeySymbol #const XK_braille_dots_456
-- #define XK_braille_dots_1456          0x1002839  /* U+2839 BRAILLE PATTERN DOTS-1456 */
pattern XK_braille_dots_1456 = MkKeySymbol #const XK_braille_dots_1456
-- #define XK_braille_dots_2456          0x100283a  /* U+283a BRAILLE PATTERN DOTS-2456 */
pattern XK_braille_dots_2456 = MkKeySymbol #const XK_braille_dots_2456
-- #define XK_braille_dots_12456         0x100283b  /* U+283b BRAILLE PATTERN DOTS-12456 */
pattern XK_braille_dots_12456 = MkKeySymbol #const XK_braille_dots_12456
-- #define XK_braille_dots_3456          0x100283c  /* U+283c BRAILLE PATTERN DOTS-3456 */
pattern XK_braille_dots_3456 = MkKeySymbol #const XK_braille_dots_3456
-- #define XK_braille_dots_13456         0x100283d  /* U+283d BRAILLE PATTERN DOTS-13456 */
pattern XK_braille_dots_13456 = MkKeySymbol #const XK_braille_dots_13456
-- #define XK_braille_dots_23456         0x100283e  /* U+283e BRAILLE PATTERN DOTS-23456 */
pattern XK_braille_dots_23456 = MkKeySymbol #const XK_braille_dots_23456
-- #define XK_braille_dots_123456        0x100283f  /* U+283f BRAILLE PATTERN DOTS-123456 */
pattern XK_braille_dots_123456 = MkKeySymbol #const XK_braille_dots_123456
-- #define XK_braille_dots_7             0x1002840  /* U+2840 BRAILLE PATTERN DOTS-7 */
pattern XK_braille_dots_7 = MkKeySymbol #const XK_braille_dots_7
-- #define XK_braille_dots_17            0x1002841  /* U+2841 BRAILLE PATTERN DOTS-17 */
pattern XK_braille_dots_17 = MkKeySymbol #const XK_braille_dots_17
-- #define XK_braille_dots_27            0x1002842  /* U+2842 BRAILLE PATTERN DOTS-27 */
pattern XK_braille_dots_27 = MkKeySymbol #const XK_braille_dots_27
-- #define XK_braille_dots_127           0x1002843  /* U+2843 BRAILLE PATTERN DOTS-127 */
pattern XK_braille_dots_127 = MkKeySymbol #const XK_braille_dots_127
-- #define XK_braille_dots_37            0x1002844  /* U+2844 BRAILLE PATTERN DOTS-37 */
pattern XK_braille_dots_37 = MkKeySymbol #const XK_braille_dots_37
-- #define XK_braille_dots_137           0x1002845  /* U+2845 BRAILLE PATTERN DOTS-137 */
pattern XK_braille_dots_137 = MkKeySymbol #const XK_braille_dots_137
-- #define XK_braille_dots_237           0x1002846  /* U+2846 BRAILLE PATTERN DOTS-237 */
pattern XK_braille_dots_237 = MkKeySymbol #const XK_braille_dots_237
-- #define XK_braille_dots_1237          0x1002847  /* U+2847 BRAILLE PATTERN DOTS-1237 */
pattern XK_braille_dots_1237 = MkKeySymbol #const XK_braille_dots_1237
-- #define XK_braille_dots_47            0x1002848  /* U+2848 BRAILLE PATTERN DOTS-47 */
pattern XK_braille_dots_47 = MkKeySymbol #const XK_braille_dots_47
-- #define XK_braille_dots_147           0x1002849  /* U+2849 BRAILLE PATTERN DOTS-147 */
pattern XK_braille_dots_147 = MkKeySymbol #const XK_braille_dots_147
-- #define XK_braille_dots_247           0x100284a  /* U+284a BRAILLE PATTERN DOTS-247 */
pattern XK_braille_dots_247 = MkKeySymbol #const XK_braille_dots_247
-- #define XK_braille_dots_1247          0x100284b  /* U+284b BRAILLE PATTERN DOTS-1247 */
pattern XK_braille_dots_1247 = MkKeySymbol #const XK_braille_dots_1247
-- #define XK_braille_dots_347           0x100284c  /* U+284c BRAILLE PATTERN DOTS-347 */
pattern XK_braille_dots_347 = MkKeySymbol #const XK_braille_dots_347
-- #define XK_braille_dots_1347          0x100284d  /* U+284d BRAILLE PATTERN DOTS-1347 */
pattern XK_braille_dots_1347 = MkKeySymbol #const XK_braille_dots_1347
-- #define XK_braille_dots_2347          0x100284e  /* U+284e BRAILLE PATTERN DOTS-2347 */
pattern XK_braille_dots_2347 = MkKeySymbol #const XK_braille_dots_2347
-- #define XK_braille_dots_12347         0x100284f  /* U+284f BRAILLE PATTERN DOTS-12347 */
pattern XK_braille_dots_12347 = MkKeySymbol #const XK_braille_dots_12347
-- #define XK_braille_dots_57            0x1002850  /* U+2850 BRAILLE PATTERN DOTS-57 */
pattern XK_braille_dots_57 = MkKeySymbol #const XK_braille_dots_57
-- #define XK_braille_dots_157           0x1002851  /* U+2851 BRAILLE PATTERN DOTS-157 */
pattern XK_braille_dots_157 = MkKeySymbol #const XK_braille_dots_157
-- #define XK_braille_dots_257           0x1002852  /* U+2852 BRAILLE PATTERN DOTS-257 */
pattern XK_braille_dots_257 = MkKeySymbol #const XK_braille_dots_257
-- #define XK_braille_dots_1257          0x1002853  /* U+2853 BRAILLE PATTERN DOTS-1257 */
pattern XK_braille_dots_1257 = MkKeySymbol #const XK_braille_dots_1257
-- #define XK_braille_dots_357           0x1002854  /* U+2854 BRAILLE PATTERN DOTS-357 */
pattern XK_braille_dots_357 = MkKeySymbol #const XK_braille_dots_357
-- #define XK_braille_dots_1357          0x1002855  /* U+2855 BRAILLE PATTERN DOTS-1357 */
pattern XK_braille_dots_1357 = MkKeySymbol #const XK_braille_dots_1357
-- #define XK_braille_dots_2357          0x1002856  /* U+2856 BRAILLE PATTERN DOTS-2357 */
pattern XK_braille_dots_2357 = MkKeySymbol #const XK_braille_dots_2357
-- #define XK_braille_dots_12357         0x1002857  /* U+2857 BRAILLE PATTERN DOTS-12357 */
pattern XK_braille_dots_12357 = MkKeySymbol #const XK_braille_dots_12357
-- #define XK_braille_dots_457           0x1002858  /* U+2858 BRAILLE PATTERN DOTS-457 */
pattern XK_braille_dots_457 = MkKeySymbol #const XK_braille_dots_457
-- #define XK_braille_dots_1457          0x1002859  /* U+2859 BRAILLE PATTERN DOTS-1457 */
pattern XK_braille_dots_1457 = MkKeySymbol #const XK_braille_dots_1457
-- #define XK_braille_dots_2457          0x100285a  /* U+285a BRAILLE PATTERN DOTS-2457 */
pattern XK_braille_dots_2457 = MkKeySymbol #const XK_braille_dots_2457
-- #define XK_braille_dots_12457         0x100285b  /* U+285b BRAILLE PATTERN DOTS-12457 */
pattern XK_braille_dots_12457 = MkKeySymbol #const XK_braille_dots_12457
-- #define XK_braille_dots_3457          0x100285c  /* U+285c BRAILLE PATTERN DOTS-3457 */
pattern XK_braille_dots_3457 = MkKeySymbol #const XK_braille_dots_3457
-- #define XK_braille_dots_13457         0x100285d  /* U+285d BRAILLE PATTERN DOTS-13457 */
pattern XK_braille_dots_13457 = MkKeySymbol #const XK_braille_dots_13457
-- #define XK_braille_dots_23457         0x100285e  /* U+285e BRAILLE PATTERN DOTS-23457 */
pattern XK_braille_dots_23457 = MkKeySymbol #const XK_braille_dots_23457
-- #define XK_braille_dots_123457        0x100285f  /* U+285f BRAILLE PATTERN DOTS-123457 */
pattern XK_braille_dots_123457 = MkKeySymbol #const XK_braille_dots_123457
-- #define XK_braille_dots_67            0x1002860  /* U+2860 BRAILLE PATTERN DOTS-67 */
pattern XK_braille_dots_67 = MkKeySymbol #const XK_braille_dots_67
-- #define XK_braille_dots_167           0x1002861  /* U+2861 BRAILLE PATTERN DOTS-167 */
pattern XK_braille_dots_167 = MkKeySymbol #const XK_braille_dots_167
-- #define XK_braille_dots_267           0x1002862  /* U+2862 BRAILLE PATTERN DOTS-267 */
pattern XK_braille_dots_267 = MkKeySymbol #const XK_braille_dots_267
-- #define XK_braille_dots_1267          0x1002863  /* U+2863 BRAILLE PATTERN DOTS-1267 */
pattern XK_braille_dots_1267 = MkKeySymbol #const XK_braille_dots_1267
-- #define XK_braille_dots_367           0x1002864  /* U+2864 BRAILLE PATTERN DOTS-367 */
pattern XK_braille_dots_367 = MkKeySymbol #const XK_braille_dots_367
-- #define XK_braille_dots_1367          0x1002865  /* U+2865 BRAILLE PATTERN DOTS-1367 */
pattern XK_braille_dots_1367 = MkKeySymbol #const XK_braille_dots_1367
-- #define XK_braille_dots_2367          0x1002866  /* U+2866 BRAILLE PATTERN DOTS-2367 */
pattern XK_braille_dots_2367 = MkKeySymbol #const XK_braille_dots_2367
-- #define XK_braille_dots_12367         0x1002867  /* U+2867 BRAILLE PATTERN DOTS-12367 */
pattern XK_braille_dots_12367 = MkKeySymbol #const XK_braille_dots_12367
-- #define XK_braille_dots_467           0x1002868  /* U+2868 BRAILLE PATTERN DOTS-467 */
pattern XK_braille_dots_467 = MkKeySymbol #const XK_braille_dots_467
-- #define XK_braille_dots_1467          0x1002869  /* U+2869 BRAILLE PATTERN DOTS-1467 */
pattern XK_braille_dots_1467 = MkKeySymbol #const XK_braille_dots_1467
-- #define XK_braille_dots_2467          0x100286a  /* U+286a BRAILLE PATTERN DOTS-2467 */
pattern XK_braille_dots_2467 = MkKeySymbol #const XK_braille_dots_2467
-- #define XK_braille_dots_12467         0x100286b  /* U+286b BRAILLE PATTERN DOTS-12467 */
pattern XK_braille_dots_12467 = MkKeySymbol #const XK_braille_dots_12467
-- #define XK_braille_dots_3467          0x100286c  /* U+286c BRAILLE PATTERN DOTS-3467 */
pattern XK_braille_dots_3467 = MkKeySymbol #const XK_braille_dots_3467
-- #define XK_braille_dots_13467         0x100286d  /* U+286d BRAILLE PATTERN DOTS-13467 */
pattern XK_braille_dots_13467 = MkKeySymbol #const XK_braille_dots_13467
-- #define XK_braille_dots_23467         0x100286e  /* U+286e BRAILLE PATTERN DOTS-23467 */
pattern XK_braille_dots_23467 = MkKeySymbol #const XK_braille_dots_23467
-- #define XK_braille_dots_123467        0x100286f  /* U+286f BRAILLE PATTERN DOTS-123467 */
pattern XK_braille_dots_123467 = MkKeySymbol #const XK_braille_dots_123467
-- #define XK_braille_dots_567           0x1002870  /* U+2870 BRAILLE PATTERN DOTS-567 */
pattern XK_braille_dots_567 = MkKeySymbol #const XK_braille_dots_567
-- #define XK_braille_dots_1567          0x1002871  /* U+2871 BRAILLE PATTERN DOTS-1567 */
pattern XK_braille_dots_1567 = MkKeySymbol #const XK_braille_dots_1567
-- #define XK_braille_dots_2567          0x1002872  /* U+2872 BRAILLE PATTERN DOTS-2567 */
pattern XK_braille_dots_2567 = MkKeySymbol #const XK_braille_dots_2567
-- #define XK_braille_dots_12567         0x1002873  /* U+2873 BRAILLE PATTERN DOTS-12567 */
pattern XK_braille_dots_12567 = MkKeySymbol #const XK_braille_dots_12567
-- #define XK_braille_dots_3567          0x1002874  /* U+2874 BRAILLE PATTERN DOTS-3567 */
pattern XK_braille_dots_3567 = MkKeySymbol #const XK_braille_dots_3567
-- #define XK_braille_dots_13567         0x1002875  /* U+2875 BRAILLE PATTERN DOTS-13567 */
pattern XK_braille_dots_13567 = MkKeySymbol #const XK_braille_dots_13567
-- #define XK_braille_dots_23567         0x1002876  /* U+2876 BRAILLE PATTERN DOTS-23567 */
pattern XK_braille_dots_23567 = MkKeySymbol #const XK_braille_dots_23567
-- #define XK_braille_dots_123567        0x1002877  /* U+2877 BRAILLE PATTERN DOTS-123567 */
pattern XK_braille_dots_123567 = MkKeySymbol #const XK_braille_dots_123567
-- #define XK_braille_dots_4567          0x1002878  /* U+2878 BRAILLE PATTERN DOTS-4567 */
pattern XK_braille_dots_4567 = MkKeySymbol #const XK_braille_dots_4567
-- #define XK_braille_dots_14567         0x1002879  /* U+2879 BRAILLE PATTERN DOTS-14567 */
pattern XK_braille_dots_14567 = MkKeySymbol #const XK_braille_dots_14567
-- #define XK_braille_dots_24567         0x100287a  /* U+287a BRAILLE PATTERN DOTS-24567 */
pattern XK_braille_dots_24567 = MkKeySymbol #const XK_braille_dots_24567
-- #define XK_braille_dots_124567        0x100287b  /* U+287b BRAILLE PATTERN DOTS-124567 */
pattern XK_braille_dots_124567 = MkKeySymbol #const XK_braille_dots_124567
-- #define XK_braille_dots_34567         0x100287c  /* U+287c BRAILLE PATTERN DOTS-34567 */
pattern XK_braille_dots_34567 = MkKeySymbol #const XK_braille_dots_34567
-- #define XK_braille_dots_134567        0x100287d  /* U+287d BRAILLE PATTERN DOTS-134567 */
pattern XK_braille_dots_134567 = MkKeySymbol #const XK_braille_dots_134567
-- #define XK_braille_dots_234567        0x100287e  /* U+287e BRAILLE PATTERN DOTS-234567 */
pattern XK_braille_dots_234567 = MkKeySymbol #const XK_braille_dots_234567
-- #define XK_braille_dots_1234567       0x100287f  /* U+287f BRAILLE PATTERN DOTS-1234567 */
pattern XK_braille_dots_1234567 = MkKeySymbol #const XK_braille_dots_1234567
-- #define XK_braille_dots_8             0x1002880  /* U+2880 BRAILLE PATTERN DOTS-8 */
pattern XK_braille_dots_8 = MkKeySymbol #const XK_braille_dots_8
-- #define XK_braille_dots_18            0x1002881  /* U+2881 BRAILLE PATTERN DOTS-18 */
pattern XK_braille_dots_18 = MkKeySymbol #const XK_braille_dots_18
-- #define XK_braille_dots_28            0x1002882  /* U+2882 BRAILLE PATTERN DOTS-28 */
pattern XK_braille_dots_28 = MkKeySymbol #const XK_braille_dots_28
-- #define XK_braille_dots_128           0x1002883  /* U+2883 BRAILLE PATTERN DOTS-128 */
pattern XK_braille_dots_128 = MkKeySymbol #const XK_braille_dots_128
-- #define XK_braille_dots_38            0x1002884  /* U+2884 BRAILLE PATTERN DOTS-38 */
pattern XK_braille_dots_38 = MkKeySymbol #const XK_braille_dots_38
-- #define XK_braille_dots_138           0x1002885  /* U+2885 BRAILLE PATTERN DOTS-138 */
pattern XK_braille_dots_138 = MkKeySymbol #const XK_braille_dots_138
-- #define XK_braille_dots_238           0x1002886  /* U+2886 BRAILLE PATTERN DOTS-238 */
pattern XK_braille_dots_238 = MkKeySymbol #const XK_braille_dots_238
-- #define XK_braille_dots_1238          0x1002887  /* U+2887 BRAILLE PATTERN DOTS-1238 */
pattern XK_braille_dots_1238 = MkKeySymbol #const XK_braille_dots_1238
-- #define XK_braille_dots_48            0x1002888  /* U+2888 BRAILLE PATTERN DOTS-48 */
pattern XK_braille_dots_48 = MkKeySymbol #const XK_braille_dots_48
-- #define XK_braille_dots_148           0x1002889  /* U+2889 BRAILLE PATTERN DOTS-148 */
pattern XK_braille_dots_148 = MkKeySymbol #const XK_braille_dots_148
-- #define XK_braille_dots_248           0x100288a  /* U+288a BRAILLE PATTERN DOTS-248 */
pattern XK_braille_dots_248 = MkKeySymbol #const XK_braille_dots_248
-- #define XK_braille_dots_1248          0x100288b  /* U+288b BRAILLE PATTERN DOTS-1248 */
pattern XK_braille_dots_1248 = MkKeySymbol #const XK_braille_dots_1248
-- #define XK_braille_dots_348           0x100288c  /* U+288c BRAILLE PATTERN DOTS-348 */
pattern XK_braille_dots_348 = MkKeySymbol #const XK_braille_dots_348
-- #define XK_braille_dots_1348          0x100288d  /* U+288d BRAILLE PATTERN DOTS-1348 */
pattern XK_braille_dots_1348 = MkKeySymbol #const XK_braille_dots_1348
-- #define XK_braille_dots_2348          0x100288e  /* U+288e BRAILLE PATTERN DOTS-2348 */
pattern XK_braille_dots_2348 = MkKeySymbol #const XK_braille_dots_2348
-- #define XK_braille_dots_12348         0x100288f  /* U+288f BRAILLE PATTERN DOTS-12348 */
pattern XK_braille_dots_12348 = MkKeySymbol #const XK_braille_dots_12348
-- #define XK_braille_dots_58            0x1002890  /* U+2890 BRAILLE PATTERN DOTS-58 */
pattern XK_braille_dots_58 = MkKeySymbol #const XK_braille_dots_58
-- #define XK_braille_dots_158           0x1002891  /* U+2891 BRAILLE PATTERN DOTS-158 */
pattern XK_braille_dots_158 = MkKeySymbol #const XK_braille_dots_158
-- #define XK_braille_dots_258           0x1002892  /* U+2892 BRAILLE PATTERN DOTS-258 */
pattern XK_braille_dots_258 = MkKeySymbol #const XK_braille_dots_258
-- #define XK_braille_dots_1258          0x1002893  /* U+2893 BRAILLE PATTERN DOTS-1258 */
pattern XK_braille_dots_1258 = MkKeySymbol #const XK_braille_dots_1258
-- #define XK_braille_dots_358           0x1002894  /* U+2894 BRAILLE PATTERN DOTS-358 */
pattern XK_braille_dots_358 = MkKeySymbol #const XK_braille_dots_358
-- #define XK_braille_dots_1358          0x1002895  /* U+2895 BRAILLE PATTERN DOTS-1358 */
pattern XK_braille_dots_1358 = MkKeySymbol #const XK_braille_dots_1358
-- #define XK_braille_dots_2358          0x1002896  /* U+2896 BRAILLE PATTERN DOTS-2358 */
pattern XK_braille_dots_2358 = MkKeySymbol #const XK_braille_dots_2358
-- #define XK_braille_dots_12358         0x1002897  /* U+2897 BRAILLE PATTERN DOTS-12358 */
pattern XK_braille_dots_12358 = MkKeySymbol #const XK_braille_dots_12358
-- #define XK_braille_dots_458           0x1002898  /* U+2898 BRAILLE PATTERN DOTS-458 */
pattern XK_braille_dots_458 = MkKeySymbol #const XK_braille_dots_458
-- #define XK_braille_dots_1458          0x1002899  /* U+2899 BRAILLE PATTERN DOTS-1458 */
pattern XK_braille_dots_1458 = MkKeySymbol #const XK_braille_dots_1458
-- #define XK_braille_dots_2458          0x100289a  /* U+289a BRAILLE PATTERN DOTS-2458 */
pattern XK_braille_dots_2458 = MkKeySymbol #const XK_braille_dots_2458
-- #define XK_braille_dots_12458         0x100289b  /* U+289b BRAILLE PATTERN DOTS-12458 */
pattern XK_braille_dots_12458 = MkKeySymbol #const XK_braille_dots_12458
-- #define XK_braille_dots_3458          0x100289c  /* U+289c BRAILLE PATTERN DOTS-3458 */
pattern XK_braille_dots_3458 = MkKeySymbol #const XK_braille_dots_3458
-- #define XK_braille_dots_13458         0x100289d  /* U+289d BRAILLE PATTERN DOTS-13458 */
pattern XK_braille_dots_13458 = MkKeySymbol #const XK_braille_dots_13458
-- #define XK_braille_dots_23458         0x100289e  /* U+289e BRAILLE PATTERN DOTS-23458 */
pattern XK_braille_dots_23458 = MkKeySymbol #const XK_braille_dots_23458
-- #define XK_braille_dots_123458        0x100289f  /* U+289f BRAILLE PATTERN DOTS-123458 */
pattern XK_braille_dots_123458 = MkKeySymbol #const XK_braille_dots_123458
-- #define XK_braille_dots_68            0x10028a0  /* U+28a0 BRAILLE PATTERN DOTS-68 */
pattern XK_braille_dots_68 = MkKeySymbol #const XK_braille_dots_68
-- #define XK_braille_dots_168           0x10028a1  /* U+28a1 BRAILLE PATTERN DOTS-168 */
pattern XK_braille_dots_168 = MkKeySymbol #const XK_braille_dots_168
-- #define XK_braille_dots_268           0x10028a2  /* U+28a2 BRAILLE PATTERN DOTS-268 */
pattern XK_braille_dots_268 = MkKeySymbol #const XK_braille_dots_268
-- #define XK_braille_dots_1268          0x10028a3  /* U+28a3 BRAILLE PATTERN DOTS-1268 */
pattern XK_braille_dots_1268 = MkKeySymbol #const XK_braille_dots_1268
-- #define XK_braille_dots_368           0x10028a4  /* U+28a4 BRAILLE PATTERN DOTS-368 */
pattern XK_braille_dots_368 = MkKeySymbol #const XK_braille_dots_368
-- #define XK_braille_dots_1368          0x10028a5  /* U+28a5 BRAILLE PATTERN DOTS-1368 */
pattern XK_braille_dots_1368 = MkKeySymbol #const XK_braille_dots_1368
-- #define XK_braille_dots_2368          0x10028a6  /* U+28a6 BRAILLE PATTERN DOTS-2368 */
pattern XK_braille_dots_2368 = MkKeySymbol #const XK_braille_dots_2368
-- #define XK_braille_dots_12368         0x10028a7  /* U+28a7 BRAILLE PATTERN DOTS-12368 */
pattern XK_braille_dots_12368 = MkKeySymbol #const XK_braille_dots_12368
-- #define XK_braille_dots_468           0x10028a8  /* U+28a8 BRAILLE PATTERN DOTS-468 */
pattern XK_braille_dots_468 = MkKeySymbol #const XK_braille_dots_468
-- #define XK_braille_dots_1468          0x10028a9  /* U+28a9 BRAILLE PATTERN DOTS-1468 */
pattern XK_braille_dots_1468 = MkKeySymbol #const XK_braille_dots_1468
-- #define XK_braille_dots_2468          0x10028aa  /* U+28aa BRAILLE PATTERN DOTS-2468 */
pattern XK_braille_dots_2468 = MkKeySymbol #const XK_braille_dots_2468
-- #define XK_braille_dots_12468         0x10028ab  /* U+28ab BRAILLE PATTERN DOTS-12468 */
pattern XK_braille_dots_12468 = MkKeySymbol #const XK_braille_dots_12468
-- #define XK_braille_dots_3468          0x10028ac  /* U+28ac BRAILLE PATTERN DOTS-3468 */
pattern XK_braille_dots_3468 = MkKeySymbol #const XK_braille_dots_3468
-- #define XK_braille_dots_13468         0x10028ad  /* U+28ad BRAILLE PATTERN DOTS-13468 */
pattern XK_braille_dots_13468 = MkKeySymbol #const XK_braille_dots_13468
-- #define XK_braille_dots_23468         0x10028ae  /* U+28ae BRAILLE PATTERN DOTS-23468 */
pattern XK_braille_dots_23468 = MkKeySymbol #const XK_braille_dots_23468
-- #define XK_braille_dots_123468        0x10028af  /* U+28af BRAILLE PATTERN DOTS-123468 */
pattern XK_braille_dots_123468 = MkKeySymbol #const XK_braille_dots_123468
-- #define XK_braille_dots_568           0x10028b0  /* U+28b0 BRAILLE PATTERN DOTS-568 */
pattern XK_braille_dots_568 = MkKeySymbol #const XK_braille_dots_568
-- #define XK_braille_dots_1568          0x10028b1  /* U+28b1 BRAILLE PATTERN DOTS-1568 */
pattern XK_braille_dots_1568 = MkKeySymbol #const XK_braille_dots_1568
-- #define XK_braille_dots_2568          0x10028b2  /* U+28b2 BRAILLE PATTERN DOTS-2568 */
pattern XK_braille_dots_2568 = MkKeySymbol #const XK_braille_dots_2568
-- #define XK_braille_dots_12568         0x10028b3  /* U+28b3 BRAILLE PATTERN DOTS-12568 */
pattern XK_braille_dots_12568 = MkKeySymbol #const XK_braille_dots_12568
-- #define XK_braille_dots_3568          0x10028b4  /* U+28b4 BRAILLE PATTERN DOTS-3568 */
pattern XK_braille_dots_3568 = MkKeySymbol #const XK_braille_dots_3568
-- #define XK_braille_dots_13568         0x10028b5  /* U+28b5 BRAILLE PATTERN DOTS-13568 */
pattern XK_braille_dots_13568 = MkKeySymbol #const XK_braille_dots_13568
-- #define XK_braille_dots_23568         0x10028b6  /* U+28b6 BRAILLE PATTERN DOTS-23568 */
pattern XK_braille_dots_23568 = MkKeySymbol #const XK_braille_dots_23568
-- #define XK_braille_dots_123568        0x10028b7  /* U+28b7 BRAILLE PATTERN DOTS-123568 */
pattern XK_braille_dots_123568 = MkKeySymbol #const XK_braille_dots_123568
-- #define XK_braille_dots_4568          0x10028b8  /* U+28b8 BRAILLE PATTERN DOTS-4568 */
pattern XK_braille_dots_4568 = MkKeySymbol #const XK_braille_dots_4568
-- #define XK_braille_dots_14568         0x10028b9  /* U+28b9 BRAILLE PATTERN DOTS-14568 */
pattern XK_braille_dots_14568 = MkKeySymbol #const XK_braille_dots_14568
-- #define XK_braille_dots_24568         0x10028ba  /* U+28ba BRAILLE PATTERN DOTS-24568 */
pattern XK_braille_dots_24568 = MkKeySymbol #const XK_braille_dots_24568
-- #define XK_braille_dots_124568        0x10028bb  /* U+28bb BRAILLE PATTERN DOTS-124568 */
pattern XK_braille_dots_124568 = MkKeySymbol #const XK_braille_dots_124568
-- #define XK_braille_dots_34568         0x10028bc  /* U+28bc BRAILLE PATTERN DOTS-34568 */
pattern XK_braille_dots_34568 = MkKeySymbol #const XK_braille_dots_34568
-- #define XK_braille_dots_134568        0x10028bd  /* U+28bd BRAILLE PATTERN DOTS-134568 */
pattern XK_braille_dots_134568 = MkKeySymbol #const XK_braille_dots_134568
-- #define XK_braille_dots_234568        0x10028be  /* U+28be BRAILLE PATTERN DOTS-234568 */
pattern XK_braille_dots_234568 = MkKeySymbol #const XK_braille_dots_234568
-- #define XK_braille_dots_1234568       0x10028bf  /* U+28bf BRAILLE PATTERN DOTS-1234568 */
pattern XK_braille_dots_1234568 = MkKeySymbol #const XK_braille_dots_1234568
-- #define XK_braille_dots_78            0x10028c0  /* U+28c0 BRAILLE PATTERN DOTS-78 */
pattern XK_braille_dots_78 = MkKeySymbol #const XK_braille_dots_78
-- #define XK_braille_dots_178           0x10028c1  /* U+28c1 BRAILLE PATTERN DOTS-178 */
pattern XK_braille_dots_178 = MkKeySymbol #const XK_braille_dots_178
-- #define XK_braille_dots_278           0x10028c2  /* U+28c2 BRAILLE PATTERN DOTS-278 */
pattern XK_braille_dots_278 = MkKeySymbol #const XK_braille_dots_278
-- #define XK_braille_dots_1278          0x10028c3  /* U+28c3 BRAILLE PATTERN DOTS-1278 */
pattern XK_braille_dots_1278 = MkKeySymbol #const XK_braille_dots_1278
-- #define XK_braille_dots_378           0x10028c4  /* U+28c4 BRAILLE PATTERN DOTS-378 */
pattern XK_braille_dots_378 = MkKeySymbol #const XK_braille_dots_378
-- #define XK_braille_dots_1378          0x10028c5  /* U+28c5 BRAILLE PATTERN DOTS-1378 */
pattern XK_braille_dots_1378 = MkKeySymbol #const XK_braille_dots_1378
-- #define XK_braille_dots_2378          0x10028c6  /* U+28c6 BRAILLE PATTERN DOTS-2378 */
pattern XK_braille_dots_2378 = MkKeySymbol #const XK_braille_dots_2378
-- #define XK_braille_dots_12378         0x10028c7  /* U+28c7 BRAILLE PATTERN DOTS-12378 */
pattern XK_braille_dots_12378 = MkKeySymbol #const XK_braille_dots_12378
-- #define XK_braille_dots_478           0x10028c8  /* U+28c8 BRAILLE PATTERN DOTS-478 */
pattern XK_braille_dots_478 = MkKeySymbol #const XK_braille_dots_478
-- #define XK_braille_dots_1478          0x10028c9  /* U+28c9 BRAILLE PATTERN DOTS-1478 */
pattern XK_braille_dots_1478 = MkKeySymbol #const XK_braille_dots_1478
-- #define XK_braille_dots_2478          0x10028ca  /* U+28ca BRAILLE PATTERN DOTS-2478 */
pattern XK_braille_dots_2478 = MkKeySymbol #const XK_braille_dots_2478
-- #define XK_braille_dots_12478         0x10028cb  /* U+28cb BRAILLE PATTERN DOTS-12478 */
pattern XK_braille_dots_12478 = MkKeySymbol #const XK_braille_dots_12478
-- #define XK_braille_dots_3478          0x10028cc  /* U+28cc BRAILLE PATTERN DOTS-3478 */
pattern XK_braille_dots_3478 = MkKeySymbol #const XK_braille_dots_3478
-- #define XK_braille_dots_13478         0x10028cd  /* U+28cd BRAILLE PATTERN DOTS-13478 */
pattern XK_braille_dots_13478 = MkKeySymbol #const XK_braille_dots_13478
-- #define XK_braille_dots_23478         0x10028ce  /* U+28ce BRAILLE PATTERN DOTS-23478 */
pattern XK_braille_dots_23478 = MkKeySymbol #const XK_braille_dots_23478
-- #define XK_braille_dots_123478        0x10028cf  /* U+28cf BRAILLE PATTERN DOTS-123478 */
pattern XK_braille_dots_123478 = MkKeySymbol #const XK_braille_dots_123478
-- #define XK_braille_dots_578           0x10028d0  /* U+28d0 BRAILLE PATTERN DOTS-578 */
pattern XK_braille_dots_578 = MkKeySymbol #const XK_braille_dots_578
-- #define XK_braille_dots_1578          0x10028d1  /* U+28d1 BRAILLE PATTERN DOTS-1578 */
pattern XK_braille_dots_1578 = MkKeySymbol #const XK_braille_dots_1578
-- #define XK_braille_dots_2578          0x10028d2  /* U+28d2 BRAILLE PATTERN DOTS-2578 */
pattern XK_braille_dots_2578 = MkKeySymbol #const XK_braille_dots_2578
-- #define XK_braille_dots_12578         0x10028d3  /* U+28d3 BRAILLE PATTERN DOTS-12578 */
pattern XK_braille_dots_12578 = MkKeySymbol #const XK_braille_dots_12578
-- #define XK_braille_dots_3578          0x10028d4  /* U+28d4 BRAILLE PATTERN DOTS-3578 */
pattern XK_braille_dots_3578 = MkKeySymbol #const XK_braille_dots_3578
-- #define XK_braille_dots_13578         0x10028d5  /* U+28d5 BRAILLE PATTERN DOTS-13578 */
pattern XK_braille_dots_13578 = MkKeySymbol #const XK_braille_dots_13578
-- #define XK_braille_dots_23578         0x10028d6  /* U+28d6 BRAILLE PATTERN DOTS-23578 */
pattern XK_braille_dots_23578 = MkKeySymbol #const XK_braille_dots_23578
-- #define XK_braille_dots_123578        0x10028d7  /* U+28d7 BRAILLE PATTERN DOTS-123578 */
pattern XK_braille_dots_123578 = MkKeySymbol #const XK_braille_dots_123578
-- #define XK_braille_dots_4578          0x10028d8  /* U+28d8 BRAILLE PATTERN DOTS-4578 */
pattern XK_braille_dots_4578 = MkKeySymbol #const XK_braille_dots_4578
-- #define XK_braille_dots_14578         0x10028d9  /* U+28d9 BRAILLE PATTERN DOTS-14578 */
pattern XK_braille_dots_14578 = MkKeySymbol #const XK_braille_dots_14578
-- #define XK_braille_dots_24578         0x10028da  /* U+28da BRAILLE PATTERN DOTS-24578 */
pattern XK_braille_dots_24578 = MkKeySymbol #const XK_braille_dots_24578
-- #define XK_braille_dots_124578        0x10028db  /* U+28db BRAILLE PATTERN DOTS-124578 */
pattern XK_braille_dots_124578 = MkKeySymbol #const XK_braille_dots_124578
-- #define XK_braille_dots_34578         0x10028dc  /* U+28dc BRAILLE PATTERN DOTS-34578 */
pattern XK_braille_dots_34578 = MkKeySymbol #const XK_braille_dots_34578
-- #define XK_braille_dots_134578        0x10028dd  /* U+28dd BRAILLE PATTERN DOTS-134578 */
pattern XK_braille_dots_134578 = MkKeySymbol #const XK_braille_dots_134578
-- #define XK_braille_dots_234578        0x10028de  /* U+28de BRAILLE PATTERN DOTS-234578 */
pattern XK_braille_dots_234578 = MkKeySymbol #const XK_braille_dots_234578
-- #define XK_braille_dots_1234578       0x10028df  /* U+28df BRAILLE PATTERN DOTS-1234578 */
pattern XK_braille_dots_1234578 = MkKeySymbol #const XK_braille_dots_1234578
-- #define XK_braille_dots_678           0x10028e0  /* U+28e0 BRAILLE PATTERN DOTS-678 */
pattern XK_braille_dots_678 = MkKeySymbol #const XK_braille_dots_678
-- #define XK_braille_dots_1678          0x10028e1  /* U+28e1 BRAILLE PATTERN DOTS-1678 */
pattern XK_braille_dots_1678 = MkKeySymbol #const XK_braille_dots_1678
-- #define XK_braille_dots_2678          0x10028e2  /* U+28e2 BRAILLE PATTERN DOTS-2678 */
pattern XK_braille_dots_2678 = MkKeySymbol #const XK_braille_dots_2678
-- #define XK_braille_dots_12678         0x10028e3  /* U+28e3 BRAILLE PATTERN DOTS-12678 */
pattern XK_braille_dots_12678 = MkKeySymbol #const XK_braille_dots_12678
-- #define XK_braille_dots_3678          0x10028e4  /* U+28e4 BRAILLE PATTERN DOTS-3678 */
pattern XK_braille_dots_3678 = MkKeySymbol #const XK_braille_dots_3678
-- #define XK_braille_dots_13678         0x10028e5  /* U+28e5 BRAILLE PATTERN DOTS-13678 */
pattern XK_braille_dots_13678 = MkKeySymbol #const XK_braille_dots_13678
-- #define XK_braille_dots_23678         0x10028e6  /* U+28e6 BRAILLE PATTERN DOTS-23678 */
pattern XK_braille_dots_23678 = MkKeySymbol #const XK_braille_dots_23678
-- #define XK_braille_dots_123678        0x10028e7  /* U+28e7 BRAILLE PATTERN DOTS-123678 */
pattern XK_braille_dots_123678 = MkKeySymbol #const XK_braille_dots_123678
-- #define XK_braille_dots_4678          0x10028e8  /* U+28e8 BRAILLE PATTERN DOTS-4678 */
pattern XK_braille_dots_4678 = MkKeySymbol #const XK_braille_dots_4678
-- #define XK_braille_dots_14678         0x10028e9  /* U+28e9 BRAILLE PATTERN DOTS-14678 */
pattern XK_braille_dots_14678 = MkKeySymbol #const XK_braille_dots_14678
-- #define XK_braille_dots_24678         0x10028ea  /* U+28ea BRAILLE PATTERN DOTS-24678 */
pattern XK_braille_dots_24678 = MkKeySymbol #const XK_braille_dots_24678
-- #define XK_braille_dots_124678        0x10028eb  /* U+28eb BRAILLE PATTERN DOTS-124678 */
pattern XK_braille_dots_124678 = MkKeySymbol #const XK_braille_dots_124678
-- #define XK_braille_dots_34678         0x10028ec  /* U+28ec BRAILLE PATTERN DOTS-34678 */
pattern XK_braille_dots_34678 = MkKeySymbol #const XK_braille_dots_34678
-- #define XK_braille_dots_134678        0x10028ed  /* U+28ed BRAILLE PATTERN DOTS-134678 */
pattern XK_braille_dots_134678 = MkKeySymbol #const XK_braille_dots_134678
-- #define XK_braille_dots_234678        0x10028ee  /* U+28ee BRAILLE PATTERN DOTS-234678 */
pattern XK_braille_dots_234678 = MkKeySymbol #const XK_braille_dots_234678
-- #define XK_braille_dots_1234678       0x10028ef  /* U+28ef BRAILLE PATTERN DOTS-1234678 */
pattern XK_braille_dots_1234678 = MkKeySymbol #const XK_braille_dots_1234678
-- #define XK_braille_dots_5678          0x10028f0  /* U+28f0 BRAILLE PATTERN DOTS-5678 */
pattern XK_braille_dots_5678 = MkKeySymbol #const XK_braille_dots_5678
-- #define XK_braille_dots_15678         0x10028f1  /* U+28f1 BRAILLE PATTERN DOTS-15678 */
pattern XK_braille_dots_15678 = MkKeySymbol #const XK_braille_dots_15678
-- #define XK_braille_dots_25678         0x10028f2  /* U+28f2 BRAILLE PATTERN DOTS-25678 */
pattern XK_braille_dots_25678 = MkKeySymbol #const XK_braille_dots_25678
-- #define XK_braille_dots_125678        0x10028f3  /* U+28f3 BRAILLE PATTERN DOTS-125678 */
pattern XK_braille_dots_125678 = MkKeySymbol #const XK_braille_dots_125678
-- #define XK_braille_dots_35678         0x10028f4  /* U+28f4 BRAILLE PATTERN DOTS-35678 */
pattern XK_braille_dots_35678 = MkKeySymbol #const XK_braille_dots_35678
-- #define XK_braille_dots_135678        0x10028f5  /* U+28f5 BRAILLE PATTERN DOTS-135678 */
pattern XK_braille_dots_135678 = MkKeySymbol #const XK_braille_dots_135678
-- #define XK_braille_dots_235678        0x10028f6  /* U+28f6 BRAILLE PATTERN DOTS-235678 */
pattern XK_braille_dots_235678 = MkKeySymbol #const XK_braille_dots_235678
-- #define XK_braille_dots_1235678       0x10028f7  /* U+28f7 BRAILLE PATTERN DOTS-1235678 */
pattern XK_braille_dots_1235678 = MkKeySymbol #const XK_braille_dots_1235678
-- #define XK_braille_dots_45678         0x10028f8  /* U+28f8 BRAILLE PATTERN DOTS-45678 */
pattern XK_braille_dots_45678 = MkKeySymbol #const XK_braille_dots_45678
-- #define XK_braille_dots_145678        0x10028f9  /* U+28f9 BRAILLE PATTERN DOTS-145678 */
pattern XK_braille_dots_145678 = MkKeySymbol #const XK_braille_dots_145678
-- #define XK_braille_dots_245678        0x10028fa  /* U+28fa BRAILLE PATTERN DOTS-245678 */
pattern XK_braille_dots_245678 = MkKeySymbol #const XK_braille_dots_245678
-- #define XK_braille_dots_1245678       0x10028fb  /* U+28fb BRAILLE PATTERN DOTS-1245678 */
pattern XK_braille_dots_1245678 = MkKeySymbol #const XK_braille_dots_1245678
-- #define XK_braille_dots_345678        0x10028fc  /* U+28fc BRAILLE PATTERN DOTS-345678 */
pattern XK_braille_dots_345678 = MkKeySymbol #const XK_braille_dots_345678
-- #define XK_braille_dots_1345678       0x10028fd  /* U+28fd BRAILLE PATTERN DOTS-1345678 */
pattern XK_braille_dots_1345678 = MkKeySymbol #const XK_braille_dots_1345678
-- #define XK_braille_dots_2345678       0x10028fe  /* U+28fe BRAILLE PATTERN DOTS-2345678 */
pattern XK_braille_dots_2345678 = MkKeySymbol #const XK_braille_dots_2345678
-- #define XK_braille_dots_12345678      0x10028ff  /* U+28ff BRAILLE PATTERN DOTS-12345678 */
pattern XK_braille_dots_12345678 = MkKeySymbol #const XK_braille_dots_12345678
-- #define XK_Sinh_ng            0x1000d82  /* U+0D82 SINHALA ANUSVARAYA */
pattern XK_Sinh_ng = MkKeySymbol #const XK_Sinh_ng
-- #define XK_Sinh_h2            0x1000d83  /* U+0D83 SINHALA VISARGAYA */
pattern XK_Sinh_h2 = MkKeySymbol #const XK_Sinh_h2
-- #define XK_Sinh_a             0x1000d85  /* U+0D85 SINHALA AYANNA */
pattern XK_Sinh_a = MkKeySymbol #const XK_Sinh_a
-- #define XK_Sinh_aa            0x1000d86  /* U+0D86 SINHALA AAYANNA */
pattern XK_Sinh_aa = MkKeySymbol #const XK_Sinh_aa
-- #define XK_Sinh_ae            0x1000d87  /* U+0D87 SINHALA AEYANNA */
pattern XK_Sinh_ae = MkKeySymbol #const XK_Sinh_ae
-- #define XK_Sinh_aee           0x1000d88  /* U+0D88 SINHALA AEEYANNA */
pattern XK_Sinh_aee = MkKeySymbol #const XK_Sinh_aee
-- #define XK_Sinh_i             0x1000d89  /* U+0D89 SINHALA IYANNA */
pattern XK_Sinh_i = MkKeySymbol #const XK_Sinh_i
-- #define XK_Sinh_ii            0x1000d8a  /* U+0D8A SINHALA IIYANNA */
pattern XK_Sinh_ii = MkKeySymbol #const XK_Sinh_ii
-- #define XK_Sinh_u             0x1000d8b  /* U+0D8B SINHALA UYANNA */
pattern XK_Sinh_u = MkKeySymbol #const XK_Sinh_u
-- #define XK_Sinh_uu            0x1000d8c  /* U+0D8C SINHALA UUYANNA */
pattern XK_Sinh_uu = MkKeySymbol #const XK_Sinh_uu
-- #define XK_Sinh_ri            0x1000d8d  /* U+0D8D SINHALA IRUYANNA */
pattern XK_Sinh_ri = MkKeySymbol #const XK_Sinh_ri
-- #define XK_Sinh_rii           0x1000d8e  /* U+0D8E SINHALA IRUUYANNA */
pattern XK_Sinh_rii = MkKeySymbol #const XK_Sinh_rii
-- #define XK_Sinh_lu            0x1000d8f  /* U+0D8F SINHALA ILUYANNA */
pattern XK_Sinh_lu = MkKeySymbol #const XK_Sinh_lu
-- #define XK_Sinh_luu           0x1000d90  /* U+0D90 SINHALA ILUUYANNA */
pattern XK_Sinh_luu = MkKeySymbol #const XK_Sinh_luu
-- #define XK_Sinh_e             0x1000d91  /* U+0D91 SINHALA EYANNA */
pattern XK_Sinh_e = MkKeySymbol #const XK_Sinh_e
-- #define XK_Sinh_ee            0x1000d92  /* U+0D92 SINHALA EEYANNA */
pattern XK_Sinh_ee = MkKeySymbol #const XK_Sinh_ee
-- #define XK_Sinh_ai            0x1000d93  /* U+0D93 SINHALA AIYANNA */
pattern XK_Sinh_ai = MkKeySymbol #const XK_Sinh_ai
-- #define XK_Sinh_o             0x1000d94  /* U+0D94 SINHALA OYANNA */
pattern XK_Sinh_o = MkKeySymbol #const XK_Sinh_o
-- #define XK_Sinh_oo            0x1000d95  /* U+0D95 SINHALA OOYANNA */
pattern XK_Sinh_oo = MkKeySymbol #const XK_Sinh_oo
-- #define XK_Sinh_au            0x1000d96  /* U+0D96 SINHALA AUYANNA */
pattern XK_Sinh_au = MkKeySymbol #const XK_Sinh_au
-- #define XK_Sinh_ka            0x1000d9a  /* U+0D9A SINHALA KAYANNA */
pattern XK_Sinh_ka = MkKeySymbol #const XK_Sinh_ka
-- #define XK_Sinh_kha           0x1000d9b  /* U+0D9B SINHALA MAHA. KAYANNA */
pattern XK_Sinh_kha = MkKeySymbol #const XK_Sinh_kha
-- #define XK_Sinh_ga            0x1000d9c  /* U+0D9C SINHALA GAYANNA */
pattern XK_Sinh_ga = MkKeySymbol #const XK_Sinh_ga
-- #define XK_Sinh_gha           0x1000d9d  /* U+0D9D SINHALA MAHA. GAYANNA */
pattern XK_Sinh_gha = MkKeySymbol #const XK_Sinh_gha
-- #define XK_Sinh_ng2           0x1000d9e  /* U+0D9E SINHALA KANTAJA NAASIKYAYA */
pattern XK_Sinh_ng2 = MkKeySymbol #const XK_Sinh_ng2
-- #define XK_Sinh_nga           0x1000d9f  /* U+0D9F SINHALA SANYAKA GAYANNA */
pattern XK_Sinh_nga = MkKeySymbol #const XK_Sinh_nga
-- #define XK_Sinh_ca            0x1000da0  /* U+0DA0 SINHALA CAYANNA */
pattern XK_Sinh_ca = MkKeySymbol #const XK_Sinh_ca
-- #define XK_Sinh_cha           0x1000da1  /* U+0DA1 SINHALA MAHA. CAYANNA */
pattern XK_Sinh_cha = MkKeySymbol #const XK_Sinh_cha
-- #define XK_Sinh_ja            0x1000da2  /* U+0DA2 SINHALA JAYANNA */
pattern XK_Sinh_ja = MkKeySymbol #const XK_Sinh_ja
-- #define XK_Sinh_jha           0x1000da3  /* U+0DA3 SINHALA MAHA. JAYANNA */
pattern XK_Sinh_jha = MkKeySymbol #const XK_Sinh_jha
-- #define XK_Sinh_nya           0x1000da4  /* U+0DA4 SINHALA TAALUJA NAASIKYAYA */
pattern XK_Sinh_nya = MkKeySymbol #const XK_Sinh_nya
-- #define XK_Sinh_jnya          0x1000da5  /* U+0DA5 SINHALA TAALUJA SANYOOGA NAASIKYAYA */
pattern XK_Sinh_jnya = MkKeySymbol #const XK_Sinh_jnya
-- #define XK_Sinh_nja           0x1000da6  /* U+0DA6 SINHALA SANYAKA JAYANNA */
pattern XK_Sinh_nja = MkKeySymbol #const XK_Sinh_nja
-- #define XK_Sinh_tta           0x1000da7  /* U+0DA7 SINHALA TTAYANNA */
pattern XK_Sinh_tta = MkKeySymbol #const XK_Sinh_tta
-- #define XK_Sinh_ttha          0x1000da8  /* U+0DA8 SINHALA MAHA. TTAYANNA */
pattern XK_Sinh_ttha = MkKeySymbol #const XK_Sinh_ttha
-- #define XK_Sinh_dda           0x1000da9  /* U+0DA9 SINHALA DDAYANNA */
pattern XK_Sinh_dda = MkKeySymbol #const XK_Sinh_dda
-- #define XK_Sinh_ddha          0x1000daa  /* U+0DAA SINHALA MAHA. DDAYANNA */
pattern XK_Sinh_ddha = MkKeySymbol #const XK_Sinh_ddha
-- #define XK_Sinh_nna           0x1000dab  /* U+0DAB SINHALA MUURDHAJA NAYANNA */
pattern XK_Sinh_nna = MkKeySymbol #const XK_Sinh_nna
-- #define XK_Sinh_ndda          0x1000dac  /* U+0DAC SINHALA SANYAKA DDAYANNA */
pattern XK_Sinh_ndda = MkKeySymbol #const XK_Sinh_ndda
-- #define XK_Sinh_tha           0x1000dad  /* U+0DAD SINHALA TAYANNA */
pattern XK_Sinh_tha = MkKeySymbol #const XK_Sinh_tha
-- #define XK_Sinh_thha          0x1000dae  /* U+0DAE SINHALA MAHA. TAYANNA */
pattern XK_Sinh_thha = MkKeySymbol #const XK_Sinh_thha
-- #define XK_Sinh_dha           0x1000daf  /* U+0DAF SINHALA DAYANNA */
pattern XK_Sinh_dha = MkKeySymbol #const XK_Sinh_dha
-- #define XK_Sinh_dhha          0x1000db0  /* U+0DB0 SINHALA MAHA. DAYANNA */
pattern XK_Sinh_dhha = MkKeySymbol #const XK_Sinh_dhha
-- #define XK_Sinh_na            0x1000db1  /* U+0DB1 SINHALA DANTAJA NAYANNA */
pattern XK_Sinh_na = MkKeySymbol #const XK_Sinh_na
-- #define XK_Sinh_ndha          0x1000db3  /* U+0DB3 SINHALA SANYAKA DAYANNA */
pattern XK_Sinh_ndha = MkKeySymbol #const XK_Sinh_ndha
-- #define XK_Sinh_pa            0x1000db4  /* U+0DB4 SINHALA PAYANNA */
pattern XK_Sinh_pa = MkKeySymbol #const XK_Sinh_pa
-- #define XK_Sinh_pha           0x1000db5  /* U+0DB5 SINHALA MAHA. PAYANNA */
pattern XK_Sinh_pha = MkKeySymbol #const XK_Sinh_pha
-- #define XK_Sinh_ba            0x1000db6  /* U+0DB6 SINHALA BAYANNA */
pattern XK_Sinh_ba = MkKeySymbol #const XK_Sinh_ba
-- #define XK_Sinh_bha           0x1000db7  /* U+0DB7 SINHALA MAHA. BAYANNA */
pattern XK_Sinh_bha = MkKeySymbol #const XK_Sinh_bha
-- #define XK_Sinh_ma            0x1000db8  /* U+0DB8 SINHALA MAYANNA */
pattern XK_Sinh_ma = MkKeySymbol #const XK_Sinh_ma
-- #define XK_Sinh_mba           0x1000db9  /* U+0DB9 SINHALA AMBA BAYANNA */
pattern XK_Sinh_mba = MkKeySymbol #const XK_Sinh_mba
-- #define XK_Sinh_ya            0x1000dba  /* U+0DBA SINHALA YAYANNA */
pattern XK_Sinh_ya = MkKeySymbol #const XK_Sinh_ya
-- #define XK_Sinh_ra            0x1000dbb  /* U+0DBB SINHALA RAYANNA */
pattern XK_Sinh_ra = MkKeySymbol #const XK_Sinh_ra
-- #define XK_Sinh_la            0x1000dbd  /* U+0DBD SINHALA DANTAJA LAYANNA */
pattern XK_Sinh_la = MkKeySymbol #const XK_Sinh_la
-- #define XK_Sinh_va            0x1000dc0  /* U+0DC0 SINHALA VAYANNA */
pattern XK_Sinh_va = MkKeySymbol #const XK_Sinh_va
-- #define XK_Sinh_sha           0x1000dc1  /* U+0DC1 SINHALA TAALUJA SAYANNA */
pattern XK_Sinh_sha = MkKeySymbol #const XK_Sinh_sha
-- #define XK_Sinh_ssha          0x1000dc2  /* U+0DC2 SINHALA MUURDHAJA SAYANNA */
pattern XK_Sinh_ssha = MkKeySymbol #const XK_Sinh_ssha
-- #define XK_Sinh_sa            0x1000dc3  /* U+0DC3 SINHALA DANTAJA SAYANNA */
pattern XK_Sinh_sa = MkKeySymbol #const XK_Sinh_sa
-- #define XK_Sinh_ha            0x1000dc4  /* U+0DC4 SINHALA HAYANNA */
pattern XK_Sinh_ha = MkKeySymbol #const XK_Sinh_ha
-- #define XK_Sinh_lla           0x1000dc5  /* U+0DC5 SINHALA MUURDHAJA LAYANNA */
pattern XK_Sinh_lla = MkKeySymbol #const XK_Sinh_lla
-- #define XK_Sinh_fa            0x1000dc6  /* U+0DC6 SINHALA FAYANNA */
pattern XK_Sinh_fa = MkKeySymbol #const XK_Sinh_fa
-- #define XK_Sinh_al            0x1000dca  /* U+0DCA SINHALA AL-LAKUNA */
pattern XK_Sinh_al = MkKeySymbol #const XK_Sinh_al
-- #define XK_Sinh_aa2           0x1000dcf  /* U+0DCF SINHALA AELA-PILLA */
pattern XK_Sinh_aa2 = MkKeySymbol #const XK_Sinh_aa2
-- #define XK_Sinh_ae2           0x1000dd0  /* U+0DD0 SINHALA AEDA-PILLA */
pattern XK_Sinh_ae2 = MkKeySymbol #const XK_Sinh_ae2
-- #define XK_Sinh_aee2          0x1000dd1  /* U+0DD1 SINHALA DIGA AEDA-PILLA */
pattern XK_Sinh_aee2 = MkKeySymbol #const XK_Sinh_aee2
-- #define XK_Sinh_i2            0x1000dd2  /* U+0DD2 SINHALA IS-PILLA */
pattern XK_Sinh_i2 = MkKeySymbol #const XK_Sinh_i2
-- #define XK_Sinh_ii2           0x1000dd3  /* U+0DD3 SINHALA DIGA IS-PILLA */
pattern XK_Sinh_ii2 = MkKeySymbol #const XK_Sinh_ii2
-- #define XK_Sinh_u2            0x1000dd4  /* U+0DD4 SINHALA PAA-PILLA */
pattern XK_Sinh_u2 = MkKeySymbol #const XK_Sinh_u2
-- #define XK_Sinh_uu2           0x1000dd6  /* U+0DD6 SINHALA DIGA PAA-PILLA */
pattern XK_Sinh_uu2 = MkKeySymbol #const XK_Sinh_uu2
-- #define XK_Sinh_ru2           0x1000dd8  /* U+0DD8 SINHALA GAETTA-PILLA */
pattern XK_Sinh_ru2 = MkKeySymbol #const XK_Sinh_ru2
-- #define XK_Sinh_e2            0x1000dd9  /* U+0DD9 SINHALA KOMBUVA */
pattern XK_Sinh_e2 = MkKeySymbol #const XK_Sinh_e2
-- #define XK_Sinh_ee2           0x1000dda  /* U+0DDA SINHALA DIGA KOMBUVA */
pattern XK_Sinh_ee2 = MkKeySymbol #const XK_Sinh_ee2
-- #define XK_Sinh_ai2           0x1000ddb  /* U+0DDB SINHALA KOMBU DEKA */
pattern XK_Sinh_ai2 = MkKeySymbol #const XK_Sinh_ai2
-- #define XK_Sinh_o2            0x1000ddc  /* U+0DDC SINHALA KOMBUVA HAA AELA-PILLA*/
pattern XK_Sinh_o2 = MkKeySymbol #const XK_Sinh_o2
-- #define XK_Sinh_oo2           0x1000ddd  /* U+0DDD SINHALA KOMBUVA HAA DIGA AELA-PILLA*/
pattern XK_Sinh_oo2 = MkKeySymbol #const XK_Sinh_oo2
-- #define XK_Sinh_au2           0x1000dde  /* U+0DDE SINHALA KOMBUVA HAA GAYANUKITTA */
pattern XK_Sinh_au2 = MkKeySymbol #const XK_Sinh_au2
-- #define XK_Sinh_lu2           0x1000ddf  /* U+0DDF SINHALA GAYANUKITTA */
pattern XK_Sinh_lu2 = MkKeySymbol #const XK_Sinh_lu2
-- #define XK_Sinh_ruu2          0x1000df2  /* U+0DF2 SINHALA DIGA GAETTA-PILLA */
pattern XK_Sinh_ruu2 = MkKeySymbol #const XK_Sinh_ruu2
-- #define XK_Sinh_luu2          0x1000df3  /* U+0DF3 SINHALA DIGA GAYANUKITTA */
pattern XK_Sinh_luu2 = MkKeySymbol #const XK_Sinh_luu2
-- #define XK_Sinh_kunddaliya    0x1000df4  /* U+0DF4 SINHALA KUNDDALIYA */
pattern XK_Sinh_kunddaliya = MkKeySymbol #const XK_Sinh_kunddaliya
