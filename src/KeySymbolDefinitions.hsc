
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE OverloadedStrings  #-}

module KeySymbolDefinitions where

import Foreign
import qualified Data.Text as T

newtype KeySymbol = MkKeySymbol { unKeySymbol :: Word32 } deriving (Eq, Show)

-- need to convert the Text to String to use the below
-- instance show KeySymbol where show = showKeySymbol

-- https://www.schoolofhaskell.com/user/icelandj/Pattern%20synonyms

-- generated using the below commands:
--- sh generate_key_symbol_definitions.sh >|/tmp/keysymsout.hsc



#include "libxkbcommon/xkbcommon/xkbcommon-keysyms.h"

-- #define XKB_KEY_NoSymbol                    0x000000  /* Special KeySym */
pattern XKB_KEY_NoSymbol :: KeySymbol
pattern XKB_KEY_NoSymbol = MkKeySymbol #{const XKB_KEY_NoSymbol} 
-- #define XKB_KEY_VoidSymbol                  0xffffff  /* Void symbol */
pattern XKB_KEY_VoidSymbol :: KeySymbol
pattern XKB_KEY_VoidSymbol = MkKeySymbol #{const XKB_KEY_VoidSymbol} 
-- #define XKB_KEY_BackSpace                     0xff08  /* Back space, back char */
pattern XKB_KEY_BackSpace :: KeySymbol
pattern XKB_KEY_BackSpace = MkKeySymbol #{const XKB_KEY_BackSpace} 
-- #define XKB_KEY_Tab                           0xff09
pattern XKB_KEY_Tab :: KeySymbol
pattern XKB_KEY_Tab = MkKeySymbol #{const XKB_KEY_Tab} 
-- #define XKB_KEY_Linefeed                      0xff0a  /* Linefeed, LF */
pattern XKB_KEY_Linefeed :: KeySymbol
pattern XKB_KEY_Linefeed = MkKeySymbol #{const XKB_KEY_Linefeed} 
-- #define XKB_KEY_Clear                         0xff0b
pattern XKB_KEY_Clear :: KeySymbol
pattern XKB_KEY_Clear = MkKeySymbol #{const XKB_KEY_Clear} 
-- #define XKB_KEY_Return                        0xff0d  /* Return, enter */
pattern XKB_KEY_Return :: KeySymbol
pattern XKB_KEY_Return = MkKeySymbol #{const XKB_KEY_Return} 
-- #define XKB_KEY_Pause                         0xff13  /* Pause, hold */
pattern XKB_KEY_Pause :: KeySymbol
pattern XKB_KEY_Pause = MkKeySymbol #{const XKB_KEY_Pause} 
-- #define XKB_KEY_Scroll_Lock                   0xff14
pattern XKB_KEY_Scroll_Lock :: KeySymbol
pattern XKB_KEY_Scroll_Lock = MkKeySymbol #{const XKB_KEY_Scroll_Lock} 
-- #define XKB_KEY_Sys_Req                       0xff15
pattern XKB_KEY_Sys_Req :: KeySymbol
pattern XKB_KEY_Sys_Req = MkKeySymbol #{const XKB_KEY_Sys_Req} 
-- #define XKB_KEY_Escape                        0xff1b
pattern XKB_KEY_Escape :: KeySymbol
pattern XKB_KEY_Escape = MkKeySymbol #{const XKB_KEY_Escape} 
-- #define XKB_KEY_Delete                        0xffff  /* Delete, rubout */
pattern XKB_KEY_Delete :: KeySymbol
pattern XKB_KEY_Delete = MkKeySymbol #{const XKB_KEY_Delete} 
-- #define XKB_KEY_Multi_key                     0xff20  /* Multi-key character compose */
pattern XKB_KEY_Multi_key :: KeySymbol
pattern XKB_KEY_Multi_key = MkKeySymbol #{const XKB_KEY_Multi_key} 
-- #define XKB_KEY_Codeinput                     0xff37
pattern XKB_KEY_Codeinput :: KeySymbol
pattern XKB_KEY_Codeinput = MkKeySymbol #{const XKB_KEY_Codeinput} 
-- #define XKB_KEY_SingleCandidate               0xff3c
pattern XKB_KEY_SingleCandidate :: KeySymbol
pattern XKB_KEY_SingleCandidate = MkKeySymbol #{const XKB_KEY_SingleCandidate} 
-- #define XKB_KEY_MultipleCandidate             0xff3d
pattern XKB_KEY_MultipleCandidate :: KeySymbol
pattern XKB_KEY_MultipleCandidate = MkKeySymbol #{const XKB_KEY_MultipleCandidate} 
-- #define XKB_KEY_PreviousCandidate             0xff3e
pattern XKB_KEY_PreviousCandidate :: KeySymbol
pattern XKB_KEY_PreviousCandidate = MkKeySymbol #{const XKB_KEY_PreviousCandidate} 
-- #define XKB_KEY_Kanji                         0xff21  /* Kanji, Kanji convert */
pattern XKB_KEY_Kanji :: KeySymbol
pattern XKB_KEY_Kanji = MkKeySymbol #{const XKB_KEY_Kanji} 
-- #define XKB_KEY_Muhenkan                      0xff22  /* Cancel Conversion */
pattern XKB_KEY_Muhenkan :: KeySymbol
pattern XKB_KEY_Muhenkan = MkKeySymbol #{const XKB_KEY_Muhenkan} 
-- #define XKB_KEY_Henkan_Mode                   0xff23  /* Start/Stop Conversion */
pattern XKB_KEY_Henkan_Mode :: KeySymbol
pattern XKB_KEY_Henkan_Mode = MkKeySymbol #{const XKB_KEY_Henkan_Mode} 
-- #define XKB_KEY_Henkan                        0xff23  /* Alias for Henkan_Mode */
pattern XKB_KEY_Henkan :: KeySymbol
pattern XKB_KEY_Henkan = MkKeySymbol #{const XKB_KEY_Henkan} 
-- #define XKB_KEY_Romaji                        0xff24  /* to Romaji */
pattern XKB_KEY_Romaji :: KeySymbol
pattern XKB_KEY_Romaji = MkKeySymbol #{const XKB_KEY_Romaji} 
-- #define XKB_KEY_Hiragana                      0xff25  /* to Hiragana */
pattern XKB_KEY_Hiragana :: KeySymbol
pattern XKB_KEY_Hiragana = MkKeySymbol #{const XKB_KEY_Hiragana} 
-- #define XKB_KEY_Katakana                      0xff26  /* to Katakana */
pattern XKB_KEY_Katakana :: KeySymbol
pattern XKB_KEY_Katakana = MkKeySymbol #{const XKB_KEY_Katakana} 
-- #define XKB_KEY_Hiragana_Katakana             0xff27  /* Hiragana/Katakana toggle */
pattern XKB_KEY_Hiragana_Katakana :: KeySymbol
pattern XKB_KEY_Hiragana_Katakana = MkKeySymbol #{const XKB_KEY_Hiragana_Katakana} 
-- #define XKB_KEY_Zenkaku                       0xff28  /* to Zenkaku */
pattern XKB_KEY_Zenkaku :: KeySymbol
pattern XKB_KEY_Zenkaku = MkKeySymbol #{const XKB_KEY_Zenkaku} 
-- #define XKB_KEY_Hankaku                       0xff29  /* to Hankaku */
pattern XKB_KEY_Hankaku :: KeySymbol
pattern XKB_KEY_Hankaku = MkKeySymbol #{const XKB_KEY_Hankaku} 
-- #define XKB_KEY_Zenkaku_Hankaku               0xff2a  /* Zenkaku/Hankaku toggle */
pattern XKB_KEY_Zenkaku_Hankaku :: KeySymbol
pattern XKB_KEY_Zenkaku_Hankaku = MkKeySymbol #{const XKB_KEY_Zenkaku_Hankaku} 
-- #define XKB_KEY_Touroku                       0xff2b  /* Add to Dictionary */
pattern XKB_KEY_Touroku :: KeySymbol
pattern XKB_KEY_Touroku = MkKeySymbol #{const XKB_KEY_Touroku} 
-- #define XKB_KEY_Massyo                        0xff2c  /* Delete from Dictionary */
pattern XKB_KEY_Massyo :: KeySymbol
pattern XKB_KEY_Massyo = MkKeySymbol #{const XKB_KEY_Massyo} 
-- #define XKB_KEY_Kana_Lock                     0xff2d  /* Kana Lock */
pattern XKB_KEY_Kana_Lock :: KeySymbol
pattern XKB_KEY_Kana_Lock = MkKeySymbol #{const XKB_KEY_Kana_Lock} 
-- #define XKB_KEY_Kana_Shift                    0xff2e  /* Kana Shift */
pattern XKB_KEY_Kana_Shift :: KeySymbol
pattern XKB_KEY_Kana_Shift = MkKeySymbol #{const XKB_KEY_Kana_Shift} 
-- #define XKB_KEY_Eisu_Shift                    0xff2f  /* Alphanumeric Shift */
pattern XKB_KEY_Eisu_Shift :: KeySymbol
pattern XKB_KEY_Eisu_Shift = MkKeySymbol #{const XKB_KEY_Eisu_Shift} 
-- #define XKB_KEY_Eisu_toggle                   0xff30  /* Alphanumeric toggle */
pattern XKB_KEY_Eisu_toggle :: KeySymbol
pattern XKB_KEY_Eisu_toggle = MkKeySymbol #{const XKB_KEY_Eisu_toggle} 
-- #define XKB_KEY_Kanji_Bangou                  0xff37  /* Codeinput */
pattern XKB_KEY_Kanji_Bangou :: KeySymbol
pattern XKB_KEY_Kanji_Bangou = MkKeySymbol #{const XKB_KEY_Kanji_Bangou} 
-- #define XKB_KEY_Zen_Koho                      0xff3d  /* Multiple/All Candidate(s) */
pattern XKB_KEY_Zen_Koho :: KeySymbol
pattern XKB_KEY_Zen_Koho = MkKeySymbol #{const XKB_KEY_Zen_Koho} 
-- #define XKB_KEY_Mae_Koho                      0xff3e  /* Previous Candidate */
pattern XKB_KEY_Mae_Koho :: KeySymbol
pattern XKB_KEY_Mae_Koho = MkKeySymbol #{const XKB_KEY_Mae_Koho} 
-- #define XKB_KEY_Home                          0xff50
pattern XKB_KEY_Home :: KeySymbol
pattern XKB_KEY_Home = MkKeySymbol #{const XKB_KEY_Home} 
-- #define XKB_KEY_Left                          0xff51  /* Move left, left arrow */
pattern XKB_KEY_Left :: KeySymbol
pattern XKB_KEY_Left = MkKeySymbol #{const XKB_KEY_Left} 
-- #define XKB_KEY_Up                            0xff52  /* Move up, up arrow */
pattern XKB_KEY_Up :: KeySymbol
pattern XKB_KEY_Up = MkKeySymbol #{const XKB_KEY_Up} 
-- #define XKB_KEY_Right                         0xff53  /* Move right, right arrow */
pattern XKB_KEY_Right :: KeySymbol
pattern XKB_KEY_Right = MkKeySymbol #{const XKB_KEY_Right} 
-- #define XKB_KEY_Down                          0xff54  /* Move down, down arrow */
pattern XKB_KEY_Down :: KeySymbol
pattern XKB_KEY_Down = MkKeySymbol #{const XKB_KEY_Down} 
-- #define XKB_KEY_Prior                         0xff55  /* Prior, previous */
pattern XKB_KEY_Prior :: KeySymbol
pattern XKB_KEY_Prior = MkKeySymbol #{const XKB_KEY_Prior} 
-- #define XKB_KEY_Page_Up                       0xff55
pattern XKB_KEY_Page_Up :: KeySymbol
pattern XKB_KEY_Page_Up = MkKeySymbol #{const XKB_KEY_Page_Up} 
-- #define XKB_KEY_Next                          0xff56  /* Next */
pattern XKB_KEY_Next :: KeySymbol
pattern XKB_KEY_Next = MkKeySymbol #{const XKB_KEY_Next} 
-- #define XKB_KEY_Page_Down                     0xff56
pattern XKB_KEY_Page_Down :: KeySymbol
pattern XKB_KEY_Page_Down = MkKeySymbol #{const XKB_KEY_Page_Down} 
-- #define XKB_KEY_End                           0xff57  /* EOL */
pattern XKB_KEY_End :: KeySymbol
pattern XKB_KEY_End = MkKeySymbol #{const XKB_KEY_End} 
-- #define XKB_KEY_Begin                         0xff58  /* BOL */
pattern XKB_KEY_Begin :: KeySymbol
pattern XKB_KEY_Begin = MkKeySymbol #{const XKB_KEY_Begin} 
-- #define XKB_KEY_Select                        0xff60  /* Select, mark */
pattern XKB_KEY_Select :: KeySymbol
pattern XKB_KEY_Select = MkKeySymbol #{const XKB_KEY_Select} 
-- #define XKB_KEY_Print                         0xff61
pattern XKB_KEY_Print :: KeySymbol
pattern XKB_KEY_Print = MkKeySymbol #{const XKB_KEY_Print} 
-- #define XKB_KEY_Execute                       0xff62  /* Execute, run, do */
pattern XKB_KEY_Execute :: KeySymbol
pattern XKB_KEY_Execute = MkKeySymbol #{const XKB_KEY_Execute} 
-- #define XKB_KEY_Insert                        0xff63  /* Insert, insert here */
pattern XKB_KEY_Insert :: KeySymbol
pattern XKB_KEY_Insert = MkKeySymbol #{const XKB_KEY_Insert} 
-- #define XKB_KEY_Undo                          0xff65
pattern XKB_KEY_Undo :: KeySymbol
pattern XKB_KEY_Undo = MkKeySymbol #{const XKB_KEY_Undo} 
-- #define XKB_KEY_Redo                          0xff66  /* Redo, again */
pattern XKB_KEY_Redo :: KeySymbol
pattern XKB_KEY_Redo = MkKeySymbol #{const XKB_KEY_Redo} 
-- #define XKB_KEY_Menu                          0xff67
pattern XKB_KEY_Menu :: KeySymbol
pattern XKB_KEY_Menu = MkKeySymbol #{const XKB_KEY_Menu} 
-- #define XKB_KEY_Find                          0xff68  /* Find, search */
pattern XKB_KEY_Find :: KeySymbol
pattern XKB_KEY_Find = MkKeySymbol #{const XKB_KEY_Find} 
-- #define XKB_KEY_Cancel                        0xff69  /* Cancel, stop, abort, exit */
pattern XKB_KEY_Cancel :: KeySymbol
pattern XKB_KEY_Cancel = MkKeySymbol #{const XKB_KEY_Cancel} 
-- #define XKB_KEY_Help                          0xff6a  /* Help */
pattern XKB_KEY_Help :: KeySymbol
pattern XKB_KEY_Help = MkKeySymbol #{const XKB_KEY_Help} 
-- #define XKB_KEY_Break                         0xff6b
pattern XKB_KEY_Break :: KeySymbol
pattern XKB_KEY_Break = MkKeySymbol #{const XKB_KEY_Break} 
-- #define XKB_KEY_Mode_switch                   0xff7e  /* Character set switch */
pattern XKB_KEY_Mode_switch :: KeySymbol
pattern XKB_KEY_Mode_switch = MkKeySymbol #{const XKB_KEY_Mode_switch} 
-- #define XKB_KEY_script_switch                 0xff7e  /* Alias for mode_switch */
pattern XKB_KEY_script_switch :: KeySymbol
pattern XKB_KEY_script_switch = MkKeySymbol #{const XKB_KEY_script_switch} 
-- #define XKB_KEY_Num_Lock                      0xff7f
pattern XKB_KEY_Num_Lock :: KeySymbol
pattern XKB_KEY_Num_Lock = MkKeySymbol #{const XKB_KEY_Num_Lock} 
-- #define XKB_KEY_KP_Space                      0xff80  /* Space */
pattern XKB_KEY_KP_Space :: KeySymbol
pattern XKB_KEY_KP_Space = MkKeySymbol #{const XKB_KEY_KP_Space} 
-- #define XKB_KEY_KP_Tab                        0xff89
pattern XKB_KEY_KP_Tab :: KeySymbol
pattern XKB_KEY_KP_Tab = MkKeySymbol #{const XKB_KEY_KP_Tab} 
-- #define XKB_KEY_KP_Enter                      0xff8d  /* Enter */
pattern XKB_KEY_KP_Enter :: KeySymbol
pattern XKB_KEY_KP_Enter = MkKeySymbol #{const XKB_KEY_KP_Enter} 
-- #define XKB_KEY_KP_F1                         0xff91  /* PF1, KP_A, ... */
pattern XKB_KEY_KP_F1 :: KeySymbol
pattern XKB_KEY_KP_F1 = MkKeySymbol #{const XKB_KEY_KP_F1} 
-- #define XKB_KEY_KP_F2                         0xff92
pattern XKB_KEY_KP_F2 :: KeySymbol
pattern XKB_KEY_KP_F2 = MkKeySymbol #{const XKB_KEY_KP_F2} 
-- #define XKB_KEY_KP_F3                         0xff93
pattern XKB_KEY_KP_F3 :: KeySymbol
pattern XKB_KEY_KP_F3 = MkKeySymbol #{const XKB_KEY_KP_F3} 
-- #define XKB_KEY_KP_F4                         0xff94
pattern XKB_KEY_KP_F4 :: KeySymbol
pattern XKB_KEY_KP_F4 = MkKeySymbol #{const XKB_KEY_KP_F4} 
-- #define XKB_KEY_KP_Home                       0xff95
pattern XKB_KEY_KP_Home :: KeySymbol
pattern XKB_KEY_KP_Home = MkKeySymbol #{const XKB_KEY_KP_Home} 
-- #define XKB_KEY_KP_Left                       0xff96
pattern XKB_KEY_KP_Left :: KeySymbol
pattern XKB_KEY_KP_Left = MkKeySymbol #{const XKB_KEY_KP_Left} 
-- #define XKB_KEY_KP_Up                         0xff97
pattern XKB_KEY_KP_Up :: KeySymbol
pattern XKB_KEY_KP_Up = MkKeySymbol #{const XKB_KEY_KP_Up} 
-- #define XKB_KEY_KP_Right                      0xff98
pattern XKB_KEY_KP_Right :: KeySymbol
pattern XKB_KEY_KP_Right = MkKeySymbol #{const XKB_KEY_KP_Right} 
-- #define XKB_KEY_KP_Down                       0xff99
pattern XKB_KEY_KP_Down :: KeySymbol
pattern XKB_KEY_KP_Down = MkKeySymbol #{const XKB_KEY_KP_Down} 
-- #define XKB_KEY_KP_Prior                      0xff9a
pattern XKB_KEY_KP_Prior :: KeySymbol
pattern XKB_KEY_KP_Prior = MkKeySymbol #{const XKB_KEY_KP_Prior} 
-- #define XKB_KEY_KP_Page_Up                    0xff9a
pattern XKB_KEY_KP_Page_Up :: KeySymbol
pattern XKB_KEY_KP_Page_Up = MkKeySymbol #{const XKB_KEY_KP_Page_Up} 
-- #define XKB_KEY_KP_Next                       0xff9b
pattern XKB_KEY_KP_Next :: KeySymbol
pattern XKB_KEY_KP_Next = MkKeySymbol #{const XKB_KEY_KP_Next} 
-- #define XKB_KEY_KP_Page_Down                  0xff9b
pattern XKB_KEY_KP_Page_Down :: KeySymbol
pattern XKB_KEY_KP_Page_Down = MkKeySymbol #{const XKB_KEY_KP_Page_Down} 
-- #define XKB_KEY_KP_End                        0xff9c
pattern XKB_KEY_KP_End :: KeySymbol
pattern XKB_KEY_KP_End = MkKeySymbol #{const XKB_KEY_KP_End} 
-- #define XKB_KEY_KP_Begin                      0xff9d
pattern XKB_KEY_KP_Begin :: KeySymbol
pattern XKB_KEY_KP_Begin = MkKeySymbol #{const XKB_KEY_KP_Begin} 
-- #define XKB_KEY_KP_Insert                     0xff9e
pattern XKB_KEY_KP_Insert :: KeySymbol
pattern XKB_KEY_KP_Insert = MkKeySymbol #{const XKB_KEY_KP_Insert} 
-- #define XKB_KEY_KP_Delete                     0xff9f
pattern XKB_KEY_KP_Delete :: KeySymbol
pattern XKB_KEY_KP_Delete = MkKeySymbol #{const XKB_KEY_KP_Delete} 
-- #define XKB_KEY_KP_Equal                      0xffbd  /* Equals */
pattern XKB_KEY_KP_Equal :: KeySymbol
pattern XKB_KEY_KP_Equal = MkKeySymbol #{const XKB_KEY_KP_Equal} 
-- #define XKB_KEY_KP_Multiply                   0xffaa
pattern XKB_KEY_KP_Multiply :: KeySymbol
pattern XKB_KEY_KP_Multiply = MkKeySymbol #{const XKB_KEY_KP_Multiply} 
-- #define XKB_KEY_KP_Add                        0xffab
pattern XKB_KEY_KP_Add :: KeySymbol
pattern XKB_KEY_KP_Add = MkKeySymbol #{const XKB_KEY_KP_Add} 
-- #define XKB_KEY_KP_Separator                  0xffac  /* Separator, often comma */
pattern XKB_KEY_KP_Separator :: KeySymbol
pattern XKB_KEY_KP_Separator = MkKeySymbol #{const XKB_KEY_KP_Separator} 
-- #define XKB_KEY_KP_Subtract                   0xffad
pattern XKB_KEY_KP_Subtract :: KeySymbol
pattern XKB_KEY_KP_Subtract = MkKeySymbol #{const XKB_KEY_KP_Subtract} 
-- #define XKB_KEY_KP_Decimal                    0xffae
pattern XKB_KEY_KP_Decimal :: KeySymbol
pattern XKB_KEY_KP_Decimal = MkKeySymbol #{const XKB_KEY_KP_Decimal} 
-- #define XKB_KEY_KP_Divide                     0xffaf
pattern XKB_KEY_KP_Divide :: KeySymbol
pattern XKB_KEY_KP_Divide = MkKeySymbol #{const XKB_KEY_KP_Divide} 
-- #define XKB_KEY_KP_0                          0xffb0
pattern XKB_KEY_KP_0 :: KeySymbol
pattern XKB_KEY_KP_0 = MkKeySymbol #{const XKB_KEY_KP_0} 
-- #define XKB_KEY_KP_1                          0xffb1
pattern XKB_KEY_KP_1 :: KeySymbol
pattern XKB_KEY_KP_1 = MkKeySymbol #{const XKB_KEY_KP_1} 
-- #define XKB_KEY_KP_2                          0xffb2
pattern XKB_KEY_KP_2 :: KeySymbol
pattern XKB_KEY_KP_2 = MkKeySymbol #{const XKB_KEY_KP_2} 
-- #define XKB_KEY_KP_3                          0xffb3
pattern XKB_KEY_KP_3 :: KeySymbol
pattern XKB_KEY_KP_3 = MkKeySymbol #{const XKB_KEY_KP_3} 
-- #define XKB_KEY_KP_4                          0xffb4
pattern XKB_KEY_KP_4 :: KeySymbol
pattern XKB_KEY_KP_4 = MkKeySymbol #{const XKB_KEY_KP_4} 
-- #define XKB_KEY_KP_5                          0xffb5
pattern XKB_KEY_KP_5 :: KeySymbol
pattern XKB_KEY_KP_5 = MkKeySymbol #{const XKB_KEY_KP_5} 
-- #define XKB_KEY_KP_6                          0xffb6
pattern XKB_KEY_KP_6 :: KeySymbol
pattern XKB_KEY_KP_6 = MkKeySymbol #{const XKB_KEY_KP_6} 
-- #define XKB_KEY_KP_7                          0xffb7
pattern XKB_KEY_KP_7 :: KeySymbol
pattern XKB_KEY_KP_7 = MkKeySymbol #{const XKB_KEY_KP_7} 
-- #define XKB_KEY_KP_8                          0xffb8
pattern XKB_KEY_KP_8 :: KeySymbol
pattern XKB_KEY_KP_8 = MkKeySymbol #{const XKB_KEY_KP_8} 
-- #define XKB_KEY_KP_9                          0xffb9
pattern XKB_KEY_KP_9 :: KeySymbol
pattern XKB_KEY_KP_9 = MkKeySymbol #{const XKB_KEY_KP_9} 
-- #define XKB_KEY_F1                            0xffbe
pattern XKB_KEY_F1 :: KeySymbol
pattern XKB_KEY_F1 = MkKeySymbol #{const XKB_KEY_F1} 
-- #define XKB_KEY_F2                            0xffbf
pattern XKB_KEY_F2 :: KeySymbol
pattern XKB_KEY_F2 = MkKeySymbol #{const XKB_KEY_F2} 
-- #define XKB_KEY_F3                            0xffc0
pattern XKB_KEY_F3 :: KeySymbol
pattern XKB_KEY_F3 = MkKeySymbol #{const XKB_KEY_F3} 
-- #define XKB_KEY_F4                            0xffc1
pattern XKB_KEY_F4 :: KeySymbol
pattern XKB_KEY_F4 = MkKeySymbol #{const XKB_KEY_F4} 
-- #define XKB_KEY_F5                            0xffc2
pattern XKB_KEY_F5 :: KeySymbol
pattern XKB_KEY_F5 = MkKeySymbol #{const XKB_KEY_F5} 
-- #define XKB_KEY_F6                            0xffc3
pattern XKB_KEY_F6 :: KeySymbol
pattern XKB_KEY_F6 = MkKeySymbol #{const XKB_KEY_F6} 
-- #define XKB_KEY_F7                            0xffc4
pattern XKB_KEY_F7 :: KeySymbol
pattern XKB_KEY_F7 = MkKeySymbol #{const XKB_KEY_F7} 
-- #define XKB_KEY_F8                            0xffc5
pattern XKB_KEY_F8 :: KeySymbol
pattern XKB_KEY_F8 = MkKeySymbol #{const XKB_KEY_F8} 
-- #define XKB_KEY_F9                            0xffc6
pattern XKB_KEY_F9 :: KeySymbol
pattern XKB_KEY_F9 = MkKeySymbol #{const XKB_KEY_F9} 
-- #define XKB_KEY_F10                           0xffc7
pattern XKB_KEY_F10 :: KeySymbol
pattern XKB_KEY_F10 = MkKeySymbol #{const XKB_KEY_F10} 
-- #define XKB_KEY_F11                           0xffc8
pattern XKB_KEY_F11 :: KeySymbol
pattern XKB_KEY_F11 = MkKeySymbol #{const XKB_KEY_F11} 
-- #define XKB_KEY_L1                            0xffc8
pattern XKB_KEY_L1 :: KeySymbol
pattern XKB_KEY_L1 = MkKeySymbol #{const XKB_KEY_L1} 
-- #define XKB_KEY_F12                           0xffc9
pattern XKB_KEY_F12 :: KeySymbol
pattern XKB_KEY_F12 = MkKeySymbol #{const XKB_KEY_F12} 
-- #define XKB_KEY_L2                            0xffc9
pattern XKB_KEY_L2 :: KeySymbol
pattern XKB_KEY_L2 = MkKeySymbol #{const XKB_KEY_L2} 
-- #define XKB_KEY_F13                           0xffca
pattern XKB_KEY_F13 :: KeySymbol
pattern XKB_KEY_F13 = MkKeySymbol #{const XKB_KEY_F13} 
-- #define XKB_KEY_L3                            0xffca
pattern XKB_KEY_L3 :: KeySymbol
pattern XKB_KEY_L3 = MkKeySymbol #{const XKB_KEY_L3} 
-- #define XKB_KEY_F14                           0xffcb
pattern XKB_KEY_F14 :: KeySymbol
pattern XKB_KEY_F14 = MkKeySymbol #{const XKB_KEY_F14} 
-- #define XKB_KEY_L4                            0xffcb
pattern XKB_KEY_L4 :: KeySymbol
pattern XKB_KEY_L4 = MkKeySymbol #{const XKB_KEY_L4} 
-- #define XKB_KEY_F15                           0xffcc
pattern XKB_KEY_F15 :: KeySymbol
pattern XKB_KEY_F15 = MkKeySymbol #{const XKB_KEY_F15} 
-- #define XKB_KEY_L5                            0xffcc
pattern XKB_KEY_L5 :: KeySymbol
pattern XKB_KEY_L5 = MkKeySymbol #{const XKB_KEY_L5} 
-- #define XKB_KEY_F16                           0xffcd
pattern XKB_KEY_F16 :: KeySymbol
pattern XKB_KEY_F16 = MkKeySymbol #{const XKB_KEY_F16} 
-- #define XKB_KEY_L6                            0xffcd
pattern XKB_KEY_L6 :: KeySymbol
pattern XKB_KEY_L6 = MkKeySymbol #{const XKB_KEY_L6} 
-- #define XKB_KEY_F17                           0xffce
pattern XKB_KEY_F17 :: KeySymbol
pattern XKB_KEY_F17 = MkKeySymbol #{const XKB_KEY_F17} 
-- #define XKB_KEY_L7                            0xffce
pattern XKB_KEY_L7 :: KeySymbol
pattern XKB_KEY_L7 = MkKeySymbol #{const XKB_KEY_L7} 
-- #define XKB_KEY_F18                           0xffcf
pattern XKB_KEY_F18 :: KeySymbol
pattern XKB_KEY_F18 = MkKeySymbol #{const XKB_KEY_F18} 
-- #define XKB_KEY_L8                            0xffcf
pattern XKB_KEY_L8 :: KeySymbol
pattern XKB_KEY_L8 = MkKeySymbol #{const XKB_KEY_L8} 
-- #define XKB_KEY_F19                           0xffd0
pattern XKB_KEY_F19 :: KeySymbol
pattern XKB_KEY_F19 = MkKeySymbol #{const XKB_KEY_F19} 
-- #define XKB_KEY_L9                            0xffd0
pattern XKB_KEY_L9 :: KeySymbol
pattern XKB_KEY_L9 = MkKeySymbol #{const XKB_KEY_L9} 
-- #define XKB_KEY_F20                           0xffd1
pattern XKB_KEY_F20 :: KeySymbol
pattern XKB_KEY_F20 = MkKeySymbol #{const XKB_KEY_F20} 
-- #define XKB_KEY_L10                           0xffd1
pattern XKB_KEY_L10 :: KeySymbol
pattern XKB_KEY_L10 = MkKeySymbol #{const XKB_KEY_L10} 
-- #define XKB_KEY_F21                           0xffd2
pattern XKB_KEY_F21 :: KeySymbol
pattern XKB_KEY_F21 = MkKeySymbol #{const XKB_KEY_F21} 
-- #define XKB_KEY_R1                            0xffd2
pattern XKB_KEY_R1 :: KeySymbol
pattern XKB_KEY_R1 = MkKeySymbol #{const XKB_KEY_R1} 
-- #define XKB_KEY_F22                           0xffd3
pattern XKB_KEY_F22 :: KeySymbol
pattern XKB_KEY_F22 = MkKeySymbol #{const XKB_KEY_F22} 
-- #define XKB_KEY_R2                            0xffd3
pattern XKB_KEY_R2 :: KeySymbol
pattern XKB_KEY_R2 = MkKeySymbol #{const XKB_KEY_R2} 
-- #define XKB_KEY_F23                           0xffd4
pattern XKB_KEY_F23 :: KeySymbol
pattern XKB_KEY_F23 = MkKeySymbol #{const XKB_KEY_F23} 
-- #define XKB_KEY_R3                            0xffd4
pattern XKB_KEY_R3 :: KeySymbol
pattern XKB_KEY_R3 = MkKeySymbol #{const XKB_KEY_R3} 
-- #define XKB_KEY_F24                           0xffd5
pattern XKB_KEY_F24 :: KeySymbol
pattern XKB_KEY_F24 = MkKeySymbol #{const XKB_KEY_F24} 
-- #define XKB_KEY_R4                            0xffd5
pattern XKB_KEY_R4 :: KeySymbol
pattern XKB_KEY_R4 = MkKeySymbol #{const XKB_KEY_R4} 
-- #define XKB_KEY_F25                           0xffd6
pattern XKB_KEY_F25 :: KeySymbol
pattern XKB_KEY_F25 = MkKeySymbol #{const XKB_KEY_F25} 
-- #define XKB_KEY_R5                            0xffd6
pattern XKB_KEY_R5 :: KeySymbol
pattern XKB_KEY_R5 = MkKeySymbol #{const XKB_KEY_R5} 
-- #define XKB_KEY_F26                           0xffd7
pattern XKB_KEY_F26 :: KeySymbol
pattern XKB_KEY_F26 = MkKeySymbol #{const XKB_KEY_F26} 
-- #define XKB_KEY_R6                            0xffd7
pattern XKB_KEY_R6 :: KeySymbol
pattern XKB_KEY_R6 = MkKeySymbol #{const XKB_KEY_R6} 
-- #define XKB_KEY_F27                           0xffd8
pattern XKB_KEY_F27 :: KeySymbol
pattern XKB_KEY_F27 = MkKeySymbol #{const XKB_KEY_F27} 
-- #define XKB_KEY_R7                            0xffd8
pattern XKB_KEY_R7 :: KeySymbol
pattern XKB_KEY_R7 = MkKeySymbol #{const XKB_KEY_R7} 
-- #define XKB_KEY_F28                           0xffd9
pattern XKB_KEY_F28 :: KeySymbol
pattern XKB_KEY_F28 = MkKeySymbol #{const XKB_KEY_F28} 
-- #define XKB_KEY_R8                            0xffd9
pattern XKB_KEY_R8 :: KeySymbol
pattern XKB_KEY_R8 = MkKeySymbol #{const XKB_KEY_R8} 
-- #define XKB_KEY_F29                           0xffda
pattern XKB_KEY_F29 :: KeySymbol
pattern XKB_KEY_F29 = MkKeySymbol #{const XKB_KEY_F29} 
-- #define XKB_KEY_R9                            0xffda
pattern XKB_KEY_R9 :: KeySymbol
pattern XKB_KEY_R9 = MkKeySymbol #{const XKB_KEY_R9} 
-- #define XKB_KEY_F30                           0xffdb
pattern XKB_KEY_F30 :: KeySymbol
pattern XKB_KEY_F30 = MkKeySymbol #{const XKB_KEY_F30} 
-- #define XKB_KEY_R10                           0xffdb
pattern XKB_KEY_R10 :: KeySymbol
pattern XKB_KEY_R10 = MkKeySymbol #{const XKB_KEY_R10} 
-- #define XKB_KEY_F31                           0xffdc
pattern XKB_KEY_F31 :: KeySymbol
pattern XKB_KEY_F31 = MkKeySymbol #{const XKB_KEY_F31} 
-- #define XKB_KEY_R11                           0xffdc
pattern XKB_KEY_R11 :: KeySymbol
pattern XKB_KEY_R11 = MkKeySymbol #{const XKB_KEY_R11} 
-- #define XKB_KEY_F32                           0xffdd
pattern XKB_KEY_F32 :: KeySymbol
pattern XKB_KEY_F32 = MkKeySymbol #{const XKB_KEY_F32} 
-- #define XKB_KEY_R12                           0xffdd
pattern XKB_KEY_R12 :: KeySymbol
pattern XKB_KEY_R12 = MkKeySymbol #{const XKB_KEY_R12} 
-- #define XKB_KEY_F33                           0xffde
pattern XKB_KEY_F33 :: KeySymbol
pattern XKB_KEY_F33 = MkKeySymbol #{const XKB_KEY_F33} 
-- #define XKB_KEY_R13                           0xffde
pattern XKB_KEY_R13 :: KeySymbol
pattern XKB_KEY_R13 = MkKeySymbol #{const XKB_KEY_R13} 
-- #define XKB_KEY_F34                           0xffdf
pattern XKB_KEY_F34 :: KeySymbol
pattern XKB_KEY_F34 = MkKeySymbol #{const XKB_KEY_F34} 
-- #define XKB_KEY_R14                           0xffdf
pattern XKB_KEY_R14 :: KeySymbol
pattern XKB_KEY_R14 = MkKeySymbol #{const XKB_KEY_R14} 
-- #define XKB_KEY_F35                           0xffe0
pattern XKB_KEY_F35 :: KeySymbol
pattern XKB_KEY_F35 = MkKeySymbol #{const XKB_KEY_F35} 
-- #define XKB_KEY_R15                           0xffe0
pattern XKB_KEY_R15 :: KeySymbol
pattern XKB_KEY_R15 = MkKeySymbol #{const XKB_KEY_R15} 
-- #define XKB_KEY_Shift_L                       0xffe1  /* Left shift */
pattern XKB_KEY_Shift_L :: KeySymbol
pattern XKB_KEY_Shift_L = MkKeySymbol #{const XKB_KEY_Shift_L} 
-- #define XKB_KEY_Shift_R                       0xffe2  /* Right shift */
pattern XKB_KEY_Shift_R :: KeySymbol
pattern XKB_KEY_Shift_R = MkKeySymbol #{const XKB_KEY_Shift_R} 
-- #define XKB_KEY_Control_L                     0xffe3  /* Left control */
pattern XKB_KEY_Control_L :: KeySymbol
pattern XKB_KEY_Control_L = MkKeySymbol #{const XKB_KEY_Control_L} 
-- #define XKB_KEY_Control_R                     0xffe4  /* Right control */
pattern XKB_KEY_Control_R :: KeySymbol
pattern XKB_KEY_Control_R = MkKeySymbol #{const XKB_KEY_Control_R} 
-- #define XKB_KEY_Caps_Lock                     0xffe5  /* Caps lock */
pattern XKB_KEY_Caps_Lock :: KeySymbol
pattern XKB_KEY_Caps_Lock = MkKeySymbol #{const XKB_KEY_Caps_Lock} 
-- #define XKB_KEY_Shift_Lock                    0xffe6  /* Shift lock */
pattern XKB_KEY_Shift_Lock :: KeySymbol
pattern XKB_KEY_Shift_Lock = MkKeySymbol #{const XKB_KEY_Shift_Lock} 
-- #define XKB_KEY_Meta_L                        0xffe7  /* Left meta */
pattern XKB_KEY_Meta_L :: KeySymbol
pattern XKB_KEY_Meta_L = MkKeySymbol #{const XKB_KEY_Meta_L} 
-- #define XKB_KEY_Meta_R                        0xffe8  /* Right meta */
pattern XKB_KEY_Meta_R :: KeySymbol
pattern XKB_KEY_Meta_R = MkKeySymbol #{const XKB_KEY_Meta_R} 
-- #define XKB_KEY_Alt_L                         0xffe9  /* Left alt */
pattern XKB_KEY_Alt_L :: KeySymbol
pattern XKB_KEY_Alt_L = MkKeySymbol #{const XKB_KEY_Alt_L} 
-- #define XKB_KEY_Alt_R                         0xffea  /* Right alt */
pattern XKB_KEY_Alt_R :: KeySymbol
pattern XKB_KEY_Alt_R = MkKeySymbol #{const XKB_KEY_Alt_R} 
-- #define XKB_KEY_Super_L                       0xffeb  /* Left super */
pattern XKB_KEY_Super_L :: KeySymbol
pattern XKB_KEY_Super_L = MkKeySymbol #{const XKB_KEY_Super_L} 
-- #define XKB_KEY_Super_R                       0xffec  /* Right super */
pattern XKB_KEY_Super_R :: KeySymbol
pattern XKB_KEY_Super_R = MkKeySymbol #{const XKB_KEY_Super_R} 
-- #define XKB_KEY_Hyper_L                       0xffed  /* Left hyper */
pattern XKB_KEY_Hyper_L :: KeySymbol
pattern XKB_KEY_Hyper_L = MkKeySymbol #{const XKB_KEY_Hyper_L} 
-- #define XKB_KEY_Hyper_R                       0xffee  /* Right hyper */
pattern XKB_KEY_Hyper_R :: KeySymbol
pattern XKB_KEY_Hyper_R = MkKeySymbol #{const XKB_KEY_Hyper_R} 
-- #define XKB_KEY_ISO_Lock                      0xfe01
pattern XKB_KEY_ISO_Lock :: KeySymbol
pattern XKB_KEY_ISO_Lock = MkKeySymbol #{const XKB_KEY_ISO_Lock} 
-- #define XKB_KEY_ISO_Level2_Latch              0xfe02
pattern XKB_KEY_ISO_Level2_Latch :: KeySymbol
pattern XKB_KEY_ISO_Level2_Latch = MkKeySymbol #{const XKB_KEY_ISO_Level2_Latch} 
-- #define XKB_KEY_ISO_Level3_Shift              0xfe03
pattern XKB_KEY_ISO_Level3_Shift :: KeySymbol
pattern XKB_KEY_ISO_Level3_Shift = MkKeySymbol #{const XKB_KEY_ISO_Level3_Shift} 
-- #define XKB_KEY_ISO_Level3_Latch              0xfe04
pattern XKB_KEY_ISO_Level3_Latch :: KeySymbol
pattern XKB_KEY_ISO_Level3_Latch = MkKeySymbol #{const XKB_KEY_ISO_Level3_Latch} 
-- #define XKB_KEY_ISO_Level3_Lock               0xfe05
pattern XKB_KEY_ISO_Level3_Lock :: KeySymbol
pattern XKB_KEY_ISO_Level3_Lock = MkKeySymbol #{const XKB_KEY_ISO_Level3_Lock} 
-- #define XKB_KEY_ISO_Level5_Shift              0xfe11
pattern XKB_KEY_ISO_Level5_Shift :: KeySymbol
pattern XKB_KEY_ISO_Level5_Shift = MkKeySymbol #{const XKB_KEY_ISO_Level5_Shift} 
-- #define XKB_KEY_ISO_Level5_Latch              0xfe12
pattern XKB_KEY_ISO_Level5_Latch :: KeySymbol
pattern XKB_KEY_ISO_Level5_Latch = MkKeySymbol #{const XKB_KEY_ISO_Level5_Latch} 
-- #define XKB_KEY_ISO_Level5_Lock               0xfe13
pattern XKB_KEY_ISO_Level5_Lock :: KeySymbol
pattern XKB_KEY_ISO_Level5_Lock = MkKeySymbol #{const XKB_KEY_ISO_Level5_Lock} 
-- #define XKB_KEY_ISO_Group_Shift               0xff7e  /* Alias for mode_switch */
pattern XKB_KEY_ISO_Group_Shift :: KeySymbol
pattern XKB_KEY_ISO_Group_Shift = MkKeySymbol #{const XKB_KEY_ISO_Group_Shift} 
-- #define XKB_KEY_ISO_Group_Latch               0xfe06
pattern XKB_KEY_ISO_Group_Latch :: KeySymbol
pattern XKB_KEY_ISO_Group_Latch = MkKeySymbol #{const XKB_KEY_ISO_Group_Latch} 
-- #define XKB_KEY_ISO_Group_Lock                0xfe07
pattern XKB_KEY_ISO_Group_Lock :: KeySymbol
pattern XKB_KEY_ISO_Group_Lock = MkKeySymbol #{const XKB_KEY_ISO_Group_Lock} 
-- #define XKB_KEY_ISO_Next_Group                0xfe08
pattern XKB_KEY_ISO_Next_Group :: KeySymbol
pattern XKB_KEY_ISO_Next_Group = MkKeySymbol #{const XKB_KEY_ISO_Next_Group} 
-- #define XKB_KEY_ISO_Next_Group_Lock           0xfe09
pattern XKB_KEY_ISO_Next_Group_Lock :: KeySymbol
pattern XKB_KEY_ISO_Next_Group_Lock = MkKeySymbol #{const XKB_KEY_ISO_Next_Group_Lock} 
-- #define XKB_KEY_ISO_Prev_Group                0xfe0a
pattern XKB_KEY_ISO_Prev_Group :: KeySymbol
pattern XKB_KEY_ISO_Prev_Group = MkKeySymbol #{const XKB_KEY_ISO_Prev_Group} 
-- #define XKB_KEY_ISO_Prev_Group_Lock           0xfe0b
pattern XKB_KEY_ISO_Prev_Group_Lock :: KeySymbol
pattern XKB_KEY_ISO_Prev_Group_Lock = MkKeySymbol #{const XKB_KEY_ISO_Prev_Group_Lock} 
-- #define XKB_KEY_ISO_First_Group               0xfe0c
pattern XKB_KEY_ISO_First_Group :: KeySymbol
pattern XKB_KEY_ISO_First_Group = MkKeySymbol #{const XKB_KEY_ISO_First_Group} 
-- #define XKB_KEY_ISO_First_Group_Lock          0xfe0d
pattern XKB_KEY_ISO_First_Group_Lock :: KeySymbol
pattern XKB_KEY_ISO_First_Group_Lock = MkKeySymbol #{const XKB_KEY_ISO_First_Group_Lock} 
-- #define XKB_KEY_ISO_Last_Group                0xfe0e
pattern XKB_KEY_ISO_Last_Group :: KeySymbol
pattern XKB_KEY_ISO_Last_Group = MkKeySymbol #{const XKB_KEY_ISO_Last_Group} 
-- #define XKB_KEY_ISO_Last_Group_Lock           0xfe0f
pattern XKB_KEY_ISO_Last_Group_Lock :: KeySymbol
pattern XKB_KEY_ISO_Last_Group_Lock = MkKeySymbol #{const XKB_KEY_ISO_Last_Group_Lock} 
-- #define XKB_KEY_ISO_Left_Tab                  0xfe20
pattern XKB_KEY_ISO_Left_Tab :: KeySymbol
pattern XKB_KEY_ISO_Left_Tab = MkKeySymbol #{const XKB_KEY_ISO_Left_Tab} 
-- #define XKB_KEY_ISO_Move_Line_Up              0xfe21
pattern XKB_KEY_ISO_Move_Line_Up :: KeySymbol
pattern XKB_KEY_ISO_Move_Line_Up = MkKeySymbol #{const XKB_KEY_ISO_Move_Line_Up} 
-- #define XKB_KEY_ISO_Move_Line_Down            0xfe22
pattern XKB_KEY_ISO_Move_Line_Down :: KeySymbol
pattern XKB_KEY_ISO_Move_Line_Down = MkKeySymbol #{const XKB_KEY_ISO_Move_Line_Down} 
-- #define XKB_KEY_ISO_Partial_Line_Up           0xfe23
pattern XKB_KEY_ISO_Partial_Line_Up :: KeySymbol
pattern XKB_KEY_ISO_Partial_Line_Up = MkKeySymbol #{const XKB_KEY_ISO_Partial_Line_Up} 
-- #define XKB_KEY_ISO_Partial_Line_Down         0xfe24
pattern XKB_KEY_ISO_Partial_Line_Down :: KeySymbol
pattern XKB_KEY_ISO_Partial_Line_Down = MkKeySymbol #{const XKB_KEY_ISO_Partial_Line_Down} 
-- #define XKB_KEY_ISO_Partial_Space_Left        0xfe25
pattern XKB_KEY_ISO_Partial_Space_Left :: KeySymbol
pattern XKB_KEY_ISO_Partial_Space_Left = MkKeySymbol #{const XKB_KEY_ISO_Partial_Space_Left} 
-- #define XKB_KEY_ISO_Partial_Space_Right       0xfe26
pattern XKB_KEY_ISO_Partial_Space_Right :: KeySymbol
pattern XKB_KEY_ISO_Partial_Space_Right = MkKeySymbol #{const XKB_KEY_ISO_Partial_Space_Right} 
-- #define XKB_KEY_ISO_Set_Margin_Left           0xfe27
pattern XKB_KEY_ISO_Set_Margin_Left :: KeySymbol
pattern XKB_KEY_ISO_Set_Margin_Left = MkKeySymbol #{const XKB_KEY_ISO_Set_Margin_Left} 
-- #define XKB_KEY_ISO_Set_Margin_Right          0xfe28
pattern XKB_KEY_ISO_Set_Margin_Right :: KeySymbol
pattern XKB_KEY_ISO_Set_Margin_Right = MkKeySymbol #{const XKB_KEY_ISO_Set_Margin_Right} 
-- #define XKB_KEY_ISO_Release_Margin_Left       0xfe29
pattern XKB_KEY_ISO_Release_Margin_Left :: KeySymbol
pattern XKB_KEY_ISO_Release_Margin_Left = MkKeySymbol #{const XKB_KEY_ISO_Release_Margin_Left} 
-- #define XKB_KEY_ISO_Release_Margin_Right      0xfe2a
pattern XKB_KEY_ISO_Release_Margin_Right :: KeySymbol
pattern XKB_KEY_ISO_Release_Margin_Right = MkKeySymbol #{const XKB_KEY_ISO_Release_Margin_Right} 
-- #define XKB_KEY_ISO_Release_Both_Margins      0xfe2b
pattern XKB_KEY_ISO_Release_Both_Margins :: KeySymbol
pattern XKB_KEY_ISO_Release_Both_Margins = MkKeySymbol #{const XKB_KEY_ISO_Release_Both_Margins} 
-- #define XKB_KEY_ISO_Fast_Cursor_Left          0xfe2c
pattern XKB_KEY_ISO_Fast_Cursor_Left :: KeySymbol
pattern XKB_KEY_ISO_Fast_Cursor_Left = MkKeySymbol #{const XKB_KEY_ISO_Fast_Cursor_Left} 
-- #define XKB_KEY_ISO_Fast_Cursor_Right         0xfe2d
pattern XKB_KEY_ISO_Fast_Cursor_Right :: KeySymbol
pattern XKB_KEY_ISO_Fast_Cursor_Right = MkKeySymbol #{const XKB_KEY_ISO_Fast_Cursor_Right} 
-- #define XKB_KEY_ISO_Fast_Cursor_Up            0xfe2e
pattern XKB_KEY_ISO_Fast_Cursor_Up :: KeySymbol
pattern XKB_KEY_ISO_Fast_Cursor_Up = MkKeySymbol #{const XKB_KEY_ISO_Fast_Cursor_Up} 
-- #define XKB_KEY_ISO_Fast_Cursor_Down          0xfe2f
pattern XKB_KEY_ISO_Fast_Cursor_Down :: KeySymbol
pattern XKB_KEY_ISO_Fast_Cursor_Down = MkKeySymbol #{const XKB_KEY_ISO_Fast_Cursor_Down} 
-- #define XKB_KEY_ISO_Continuous_Underline      0xfe30
pattern XKB_KEY_ISO_Continuous_Underline :: KeySymbol
pattern XKB_KEY_ISO_Continuous_Underline = MkKeySymbol #{const XKB_KEY_ISO_Continuous_Underline} 
-- #define XKB_KEY_ISO_Discontinuous_Underline   0xfe31
pattern XKB_KEY_ISO_Discontinuous_Underline :: KeySymbol
pattern XKB_KEY_ISO_Discontinuous_Underline = MkKeySymbol #{const XKB_KEY_ISO_Discontinuous_Underline} 
-- #define XKB_KEY_ISO_Emphasize                 0xfe32
pattern XKB_KEY_ISO_Emphasize :: KeySymbol
pattern XKB_KEY_ISO_Emphasize = MkKeySymbol #{const XKB_KEY_ISO_Emphasize} 
-- #define XKB_KEY_ISO_Center_Object             0xfe33
pattern XKB_KEY_ISO_Center_Object :: KeySymbol
pattern XKB_KEY_ISO_Center_Object = MkKeySymbol #{const XKB_KEY_ISO_Center_Object} 
-- #define XKB_KEY_ISO_Enter                     0xfe34
pattern XKB_KEY_ISO_Enter :: KeySymbol
pattern XKB_KEY_ISO_Enter = MkKeySymbol #{const XKB_KEY_ISO_Enter} 
-- #define XKB_KEY_dead_grave                    0xfe50
pattern XKB_KEY_dead_grave :: KeySymbol
pattern XKB_KEY_dead_grave = MkKeySymbol #{const XKB_KEY_dead_grave} 
-- #define XKB_KEY_dead_acute                    0xfe51
pattern XKB_KEY_dead_acute :: KeySymbol
pattern XKB_KEY_dead_acute = MkKeySymbol #{const XKB_KEY_dead_acute} 
-- #define XKB_KEY_dead_circumflex               0xfe52
pattern XKB_KEY_dead_circumflex :: KeySymbol
pattern XKB_KEY_dead_circumflex = MkKeySymbol #{const XKB_KEY_dead_circumflex} 
-- #define XKB_KEY_dead_tilde                    0xfe53
pattern XKB_KEY_dead_tilde :: KeySymbol
pattern XKB_KEY_dead_tilde = MkKeySymbol #{const XKB_KEY_dead_tilde} 
-- #define XKB_KEY_dead_perispomeni              0xfe53  /* alias for dead_tilde */
pattern XKB_KEY_dead_perispomeni :: KeySymbol
pattern XKB_KEY_dead_perispomeni = MkKeySymbol #{const XKB_KEY_dead_perispomeni} 
-- #define XKB_KEY_dead_macron                   0xfe54
pattern XKB_KEY_dead_macron :: KeySymbol
pattern XKB_KEY_dead_macron = MkKeySymbol #{const XKB_KEY_dead_macron} 
-- #define XKB_KEY_dead_breve                    0xfe55
pattern XKB_KEY_dead_breve :: KeySymbol
pattern XKB_KEY_dead_breve = MkKeySymbol #{const XKB_KEY_dead_breve} 
-- #define XKB_KEY_dead_abovedot                 0xfe56
pattern XKB_KEY_dead_abovedot :: KeySymbol
pattern XKB_KEY_dead_abovedot = MkKeySymbol #{const XKB_KEY_dead_abovedot} 
-- #define XKB_KEY_dead_diaeresis                0xfe57
pattern XKB_KEY_dead_diaeresis :: KeySymbol
pattern XKB_KEY_dead_diaeresis = MkKeySymbol #{const XKB_KEY_dead_diaeresis} 
-- #define XKB_KEY_dead_abovering                0xfe58
pattern XKB_KEY_dead_abovering :: KeySymbol
pattern XKB_KEY_dead_abovering = MkKeySymbol #{const XKB_KEY_dead_abovering} 
-- #define XKB_KEY_dead_doubleacute              0xfe59
pattern XKB_KEY_dead_doubleacute :: KeySymbol
pattern XKB_KEY_dead_doubleacute = MkKeySymbol #{const XKB_KEY_dead_doubleacute} 
-- #define XKB_KEY_dead_caron                    0xfe5a
pattern XKB_KEY_dead_caron :: KeySymbol
pattern XKB_KEY_dead_caron = MkKeySymbol #{const XKB_KEY_dead_caron} 
-- #define XKB_KEY_dead_cedilla                  0xfe5b
pattern XKB_KEY_dead_cedilla :: KeySymbol
pattern XKB_KEY_dead_cedilla = MkKeySymbol #{const XKB_KEY_dead_cedilla} 
-- #define XKB_KEY_dead_ogonek                   0xfe5c
pattern XKB_KEY_dead_ogonek :: KeySymbol
pattern XKB_KEY_dead_ogonek = MkKeySymbol #{const XKB_KEY_dead_ogonek} 
-- #define XKB_KEY_dead_iota                     0xfe5d
pattern XKB_KEY_dead_iota :: KeySymbol
pattern XKB_KEY_dead_iota = MkKeySymbol #{const XKB_KEY_dead_iota} 
-- #define XKB_KEY_dead_voiced_sound             0xfe5e
pattern XKB_KEY_dead_voiced_sound :: KeySymbol
pattern XKB_KEY_dead_voiced_sound = MkKeySymbol #{const XKB_KEY_dead_voiced_sound} 
-- #define XKB_KEY_dead_semivoiced_sound         0xfe5f
pattern XKB_KEY_dead_semivoiced_sound :: KeySymbol
pattern XKB_KEY_dead_semivoiced_sound = MkKeySymbol #{const XKB_KEY_dead_semivoiced_sound} 
-- #define XKB_KEY_dead_belowdot                 0xfe60
pattern XKB_KEY_dead_belowdot :: KeySymbol
pattern XKB_KEY_dead_belowdot = MkKeySymbol #{const XKB_KEY_dead_belowdot} 
-- #define XKB_KEY_dead_hook                     0xfe61
pattern XKB_KEY_dead_hook :: KeySymbol
pattern XKB_KEY_dead_hook = MkKeySymbol #{const XKB_KEY_dead_hook} 
-- #define XKB_KEY_dead_horn                     0xfe62
pattern XKB_KEY_dead_horn :: KeySymbol
pattern XKB_KEY_dead_horn = MkKeySymbol #{const XKB_KEY_dead_horn} 
-- #define XKB_KEY_dead_stroke                   0xfe63
pattern XKB_KEY_dead_stroke :: KeySymbol
pattern XKB_KEY_dead_stroke = MkKeySymbol #{const XKB_KEY_dead_stroke} 
-- #define XKB_KEY_dead_abovecomma               0xfe64
pattern XKB_KEY_dead_abovecomma :: KeySymbol
pattern XKB_KEY_dead_abovecomma = MkKeySymbol #{const XKB_KEY_dead_abovecomma} 
-- #define XKB_KEY_dead_psili                    0xfe64  /* alias for dead_abovecomma */
pattern XKB_KEY_dead_psili :: KeySymbol
pattern XKB_KEY_dead_psili = MkKeySymbol #{const XKB_KEY_dead_psili} 
-- #define XKB_KEY_dead_abovereversedcomma       0xfe65
pattern XKB_KEY_dead_abovereversedcomma :: KeySymbol
pattern XKB_KEY_dead_abovereversedcomma = MkKeySymbol #{const XKB_KEY_dead_abovereversedcomma} 
-- #define XKB_KEY_dead_dasia                    0xfe65  /* alias for dead_abovereversedcomma */
pattern XKB_KEY_dead_dasia :: KeySymbol
pattern XKB_KEY_dead_dasia = MkKeySymbol #{const XKB_KEY_dead_dasia} 
-- #define XKB_KEY_dead_doublegrave              0xfe66
pattern XKB_KEY_dead_doublegrave :: KeySymbol
pattern XKB_KEY_dead_doublegrave = MkKeySymbol #{const XKB_KEY_dead_doublegrave} 
-- #define XKB_KEY_dead_belowring                0xfe67
pattern XKB_KEY_dead_belowring :: KeySymbol
pattern XKB_KEY_dead_belowring = MkKeySymbol #{const XKB_KEY_dead_belowring} 
-- #define XKB_KEY_dead_belowmacron              0xfe68
pattern XKB_KEY_dead_belowmacron :: KeySymbol
pattern XKB_KEY_dead_belowmacron = MkKeySymbol #{const XKB_KEY_dead_belowmacron} 
-- #define XKB_KEY_dead_belowcircumflex          0xfe69
pattern XKB_KEY_dead_belowcircumflex :: KeySymbol
pattern XKB_KEY_dead_belowcircumflex = MkKeySymbol #{const XKB_KEY_dead_belowcircumflex} 
-- #define XKB_KEY_dead_belowtilde               0xfe6a
pattern XKB_KEY_dead_belowtilde :: KeySymbol
pattern XKB_KEY_dead_belowtilde = MkKeySymbol #{const XKB_KEY_dead_belowtilde} 
-- #define XKB_KEY_dead_belowbreve               0xfe6b
pattern XKB_KEY_dead_belowbreve :: KeySymbol
pattern XKB_KEY_dead_belowbreve = MkKeySymbol #{const XKB_KEY_dead_belowbreve} 
-- #define XKB_KEY_dead_belowdiaeresis           0xfe6c
pattern XKB_KEY_dead_belowdiaeresis :: KeySymbol
pattern XKB_KEY_dead_belowdiaeresis = MkKeySymbol #{const XKB_KEY_dead_belowdiaeresis} 
-- #define XKB_KEY_dead_invertedbreve            0xfe6d
pattern XKB_KEY_dead_invertedbreve :: KeySymbol
pattern XKB_KEY_dead_invertedbreve = MkKeySymbol #{const XKB_KEY_dead_invertedbreve} 
-- #define XKB_KEY_dead_belowcomma               0xfe6e
pattern XKB_KEY_dead_belowcomma :: KeySymbol
pattern XKB_KEY_dead_belowcomma = MkKeySymbol #{const XKB_KEY_dead_belowcomma} 
-- #define XKB_KEY_dead_currency                 0xfe6f
pattern XKB_KEY_dead_currency :: KeySymbol
pattern XKB_KEY_dead_currency = MkKeySymbol #{const XKB_KEY_dead_currency} 
-- #define XKB_KEY_dead_lowline                  0xfe90
pattern XKB_KEY_dead_lowline :: KeySymbol
pattern XKB_KEY_dead_lowline = MkKeySymbol #{const XKB_KEY_dead_lowline} 
-- #define XKB_KEY_dead_aboveverticalline        0xfe91
pattern XKB_KEY_dead_aboveverticalline :: KeySymbol
pattern XKB_KEY_dead_aboveverticalline = MkKeySymbol #{const XKB_KEY_dead_aboveverticalline} 
-- #define XKB_KEY_dead_belowverticalline        0xfe92
pattern XKB_KEY_dead_belowverticalline :: KeySymbol
pattern XKB_KEY_dead_belowverticalline = MkKeySymbol #{const XKB_KEY_dead_belowverticalline} 
-- #define XKB_KEY_dead_longsolidusoverlay       0xfe93
pattern XKB_KEY_dead_longsolidusoverlay :: KeySymbol
pattern XKB_KEY_dead_longsolidusoverlay = MkKeySymbol #{const XKB_KEY_dead_longsolidusoverlay} 
-- #define XKB_KEY_dead_a                        0xfe80
pattern XKB_KEY_dead_a :: KeySymbol
pattern XKB_KEY_dead_a = MkKeySymbol #{const XKB_KEY_dead_a} 
-- #define XKB_KEY_dead_A                        0xfe81
pattern XKB_KEY_dead_A :: KeySymbol
pattern XKB_KEY_dead_A = MkKeySymbol #{const XKB_KEY_dead_A} 
-- #define XKB_KEY_dead_e                        0xfe82
pattern XKB_KEY_dead_e :: KeySymbol
pattern XKB_KEY_dead_e = MkKeySymbol #{const XKB_KEY_dead_e} 
-- #define XKB_KEY_dead_E                        0xfe83
pattern XKB_KEY_dead_E :: KeySymbol
pattern XKB_KEY_dead_E = MkKeySymbol #{const XKB_KEY_dead_E} 
-- #define XKB_KEY_dead_i                        0xfe84
pattern XKB_KEY_dead_i :: KeySymbol
pattern XKB_KEY_dead_i = MkKeySymbol #{const XKB_KEY_dead_i} 
-- #define XKB_KEY_dead_I                        0xfe85
pattern XKB_KEY_dead_I :: KeySymbol
pattern XKB_KEY_dead_I = MkKeySymbol #{const XKB_KEY_dead_I} 
-- #define XKB_KEY_dead_o                        0xfe86
pattern XKB_KEY_dead_o :: KeySymbol
pattern XKB_KEY_dead_o = MkKeySymbol #{const XKB_KEY_dead_o} 
-- #define XKB_KEY_dead_O                        0xfe87
pattern XKB_KEY_dead_O :: KeySymbol
pattern XKB_KEY_dead_O = MkKeySymbol #{const XKB_KEY_dead_O} 
-- #define XKB_KEY_dead_u                        0xfe88
pattern XKB_KEY_dead_u :: KeySymbol
pattern XKB_KEY_dead_u = MkKeySymbol #{const XKB_KEY_dead_u} 
-- #define XKB_KEY_dead_U                        0xfe89
pattern XKB_KEY_dead_U :: KeySymbol
pattern XKB_KEY_dead_U = MkKeySymbol #{const XKB_KEY_dead_U} 
-- #define XKB_KEY_dead_small_schwa              0xfe8a
pattern XKB_KEY_dead_small_schwa :: KeySymbol
pattern XKB_KEY_dead_small_schwa = MkKeySymbol #{const XKB_KEY_dead_small_schwa} 
-- #define XKB_KEY_dead_capital_schwa            0xfe8b
pattern XKB_KEY_dead_capital_schwa :: KeySymbol
pattern XKB_KEY_dead_capital_schwa = MkKeySymbol #{const XKB_KEY_dead_capital_schwa} 
-- #define XKB_KEY_dead_greek                    0xfe8c
pattern XKB_KEY_dead_greek :: KeySymbol
pattern XKB_KEY_dead_greek = MkKeySymbol #{const XKB_KEY_dead_greek} 
-- #define XKB_KEY_First_Virtual_Screen          0xfed0
pattern XKB_KEY_First_Virtual_Screen :: KeySymbol
pattern XKB_KEY_First_Virtual_Screen = MkKeySymbol #{const XKB_KEY_First_Virtual_Screen} 
-- #define XKB_KEY_Prev_Virtual_Screen           0xfed1
pattern XKB_KEY_Prev_Virtual_Screen :: KeySymbol
pattern XKB_KEY_Prev_Virtual_Screen = MkKeySymbol #{const XKB_KEY_Prev_Virtual_Screen} 
-- #define XKB_KEY_Next_Virtual_Screen           0xfed2
pattern XKB_KEY_Next_Virtual_Screen :: KeySymbol
pattern XKB_KEY_Next_Virtual_Screen = MkKeySymbol #{const XKB_KEY_Next_Virtual_Screen} 
-- #define XKB_KEY_Last_Virtual_Screen           0xfed4
pattern XKB_KEY_Last_Virtual_Screen :: KeySymbol
pattern XKB_KEY_Last_Virtual_Screen = MkKeySymbol #{const XKB_KEY_Last_Virtual_Screen} 
-- #define XKB_KEY_Terminate_Server              0xfed5
pattern XKB_KEY_Terminate_Server :: KeySymbol
pattern XKB_KEY_Terminate_Server = MkKeySymbol #{const XKB_KEY_Terminate_Server} 
-- #define XKB_KEY_AccessX_Enable                0xfe70
pattern XKB_KEY_AccessX_Enable :: KeySymbol
pattern XKB_KEY_AccessX_Enable = MkKeySymbol #{const XKB_KEY_AccessX_Enable} 
-- #define XKB_KEY_AccessX_Feedback_Enable       0xfe71
pattern XKB_KEY_AccessX_Feedback_Enable :: KeySymbol
pattern XKB_KEY_AccessX_Feedback_Enable = MkKeySymbol #{const XKB_KEY_AccessX_Feedback_Enable} 
-- #define XKB_KEY_RepeatKeys_Enable             0xfe72
pattern XKB_KEY_RepeatKeys_Enable :: KeySymbol
pattern XKB_KEY_RepeatKeys_Enable = MkKeySymbol #{const XKB_KEY_RepeatKeys_Enable} 
-- #define XKB_KEY_SlowKeys_Enable               0xfe73
pattern XKB_KEY_SlowKeys_Enable :: KeySymbol
pattern XKB_KEY_SlowKeys_Enable = MkKeySymbol #{const XKB_KEY_SlowKeys_Enable} 
-- #define XKB_KEY_BounceKeys_Enable             0xfe74
pattern XKB_KEY_BounceKeys_Enable :: KeySymbol
pattern XKB_KEY_BounceKeys_Enable = MkKeySymbol #{const XKB_KEY_BounceKeys_Enable} 
-- #define XKB_KEY_StickyKeys_Enable             0xfe75
pattern XKB_KEY_StickyKeys_Enable :: KeySymbol
pattern XKB_KEY_StickyKeys_Enable = MkKeySymbol #{const XKB_KEY_StickyKeys_Enable} 
-- #define XKB_KEY_MouseKeys_Enable              0xfe76
pattern XKB_KEY_MouseKeys_Enable :: KeySymbol
pattern XKB_KEY_MouseKeys_Enable = MkKeySymbol #{const XKB_KEY_MouseKeys_Enable} 
-- #define XKB_KEY_MouseKeys_Accel_Enable        0xfe77
pattern XKB_KEY_MouseKeys_Accel_Enable :: KeySymbol
pattern XKB_KEY_MouseKeys_Accel_Enable = MkKeySymbol #{const XKB_KEY_MouseKeys_Accel_Enable} 
-- #define XKB_KEY_Overlay1_Enable               0xfe78
pattern XKB_KEY_Overlay1_Enable :: KeySymbol
pattern XKB_KEY_Overlay1_Enable = MkKeySymbol #{const XKB_KEY_Overlay1_Enable} 
-- #define XKB_KEY_Overlay2_Enable               0xfe79
pattern XKB_KEY_Overlay2_Enable :: KeySymbol
pattern XKB_KEY_Overlay2_Enable = MkKeySymbol #{const XKB_KEY_Overlay2_Enable} 
-- #define XKB_KEY_AudibleBell_Enable            0xfe7a
pattern XKB_KEY_AudibleBell_Enable :: KeySymbol
pattern XKB_KEY_AudibleBell_Enable = MkKeySymbol #{const XKB_KEY_AudibleBell_Enable} 
-- #define XKB_KEY_Pointer_Left                  0xfee0
pattern XKB_KEY_Pointer_Left :: KeySymbol
pattern XKB_KEY_Pointer_Left = MkKeySymbol #{const XKB_KEY_Pointer_Left} 
-- #define XKB_KEY_Pointer_Right                 0xfee1
pattern XKB_KEY_Pointer_Right :: KeySymbol
pattern XKB_KEY_Pointer_Right = MkKeySymbol #{const XKB_KEY_Pointer_Right} 
-- #define XKB_KEY_Pointer_Up                    0xfee2
pattern XKB_KEY_Pointer_Up :: KeySymbol
pattern XKB_KEY_Pointer_Up = MkKeySymbol #{const XKB_KEY_Pointer_Up} 
-- #define XKB_KEY_Pointer_Down                  0xfee3
pattern XKB_KEY_Pointer_Down :: KeySymbol
pattern XKB_KEY_Pointer_Down = MkKeySymbol #{const XKB_KEY_Pointer_Down} 
-- #define XKB_KEY_Pointer_UpLeft                0xfee4
pattern XKB_KEY_Pointer_UpLeft :: KeySymbol
pattern XKB_KEY_Pointer_UpLeft = MkKeySymbol #{const XKB_KEY_Pointer_UpLeft} 
-- #define XKB_KEY_Pointer_UpRight               0xfee5
pattern XKB_KEY_Pointer_UpRight :: KeySymbol
pattern XKB_KEY_Pointer_UpRight = MkKeySymbol #{const XKB_KEY_Pointer_UpRight} 
-- #define XKB_KEY_Pointer_DownLeft              0xfee6
pattern XKB_KEY_Pointer_DownLeft :: KeySymbol
pattern XKB_KEY_Pointer_DownLeft = MkKeySymbol #{const XKB_KEY_Pointer_DownLeft} 
-- #define XKB_KEY_Pointer_DownRight             0xfee7
pattern XKB_KEY_Pointer_DownRight :: KeySymbol
pattern XKB_KEY_Pointer_DownRight = MkKeySymbol #{const XKB_KEY_Pointer_DownRight} 
-- #define XKB_KEY_Pointer_Button_Dflt           0xfee8
pattern XKB_KEY_Pointer_Button_Dflt :: KeySymbol
pattern XKB_KEY_Pointer_Button_Dflt = MkKeySymbol #{const XKB_KEY_Pointer_Button_Dflt} 
-- #define XKB_KEY_Pointer_Button1               0xfee9
pattern XKB_KEY_Pointer_Button1 :: KeySymbol
pattern XKB_KEY_Pointer_Button1 = MkKeySymbol #{const XKB_KEY_Pointer_Button1} 
-- #define XKB_KEY_Pointer_Button2               0xfeea
pattern XKB_KEY_Pointer_Button2 :: KeySymbol
pattern XKB_KEY_Pointer_Button2 = MkKeySymbol #{const XKB_KEY_Pointer_Button2} 
-- #define XKB_KEY_Pointer_Button3               0xfeeb
pattern XKB_KEY_Pointer_Button3 :: KeySymbol
pattern XKB_KEY_Pointer_Button3 = MkKeySymbol #{const XKB_KEY_Pointer_Button3} 
-- #define XKB_KEY_Pointer_Button4               0xfeec
pattern XKB_KEY_Pointer_Button4 :: KeySymbol
pattern XKB_KEY_Pointer_Button4 = MkKeySymbol #{const XKB_KEY_Pointer_Button4} 
-- #define XKB_KEY_Pointer_Button5               0xfeed
pattern XKB_KEY_Pointer_Button5 :: KeySymbol
pattern XKB_KEY_Pointer_Button5 = MkKeySymbol #{const XKB_KEY_Pointer_Button5} 
-- #define XKB_KEY_Pointer_DblClick_Dflt         0xfeee
pattern XKB_KEY_Pointer_DblClick_Dflt :: KeySymbol
pattern XKB_KEY_Pointer_DblClick_Dflt = MkKeySymbol #{const XKB_KEY_Pointer_DblClick_Dflt} 
-- #define XKB_KEY_Pointer_DblClick1             0xfeef
pattern XKB_KEY_Pointer_DblClick1 :: KeySymbol
pattern XKB_KEY_Pointer_DblClick1 = MkKeySymbol #{const XKB_KEY_Pointer_DblClick1} 
-- #define XKB_KEY_Pointer_DblClick2             0xfef0
pattern XKB_KEY_Pointer_DblClick2 :: KeySymbol
pattern XKB_KEY_Pointer_DblClick2 = MkKeySymbol #{const XKB_KEY_Pointer_DblClick2} 
-- #define XKB_KEY_Pointer_DblClick3             0xfef1
pattern XKB_KEY_Pointer_DblClick3 :: KeySymbol
pattern XKB_KEY_Pointer_DblClick3 = MkKeySymbol #{const XKB_KEY_Pointer_DblClick3} 
-- #define XKB_KEY_Pointer_DblClick4             0xfef2
pattern XKB_KEY_Pointer_DblClick4 :: KeySymbol
pattern XKB_KEY_Pointer_DblClick4 = MkKeySymbol #{const XKB_KEY_Pointer_DblClick4} 
-- #define XKB_KEY_Pointer_DblClick5             0xfef3
pattern XKB_KEY_Pointer_DblClick5 :: KeySymbol
pattern XKB_KEY_Pointer_DblClick5 = MkKeySymbol #{const XKB_KEY_Pointer_DblClick5} 
-- #define XKB_KEY_Pointer_Drag_Dflt             0xfef4
pattern XKB_KEY_Pointer_Drag_Dflt :: KeySymbol
pattern XKB_KEY_Pointer_Drag_Dflt = MkKeySymbol #{const XKB_KEY_Pointer_Drag_Dflt} 
-- #define XKB_KEY_Pointer_Drag1                 0xfef5
pattern XKB_KEY_Pointer_Drag1 :: KeySymbol
pattern XKB_KEY_Pointer_Drag1 = MkKeySymbol #{const XKB_KEY_Pointer_Drag1} 
-- #define XKB_KEY_Pointer_Drag2                 0xfef6
pattern XKB_KEY_Pointer_Drag2 :: KeySymbol
pattern XKB_KEY_Pointer_Drag2 = MkKeySymbol #{const XKB_KEY_Pointer_Drag2} 
-- #define XKB_KEY_Pointer_Drag3                 0xfef7
pattern XKB_KEY_Pointer_Drag3 :: KeySymbol
pattern XKB_KEY_Pointer_Drag3 = MkKeySymbol #{const XKB_KEY_Pointer_Drag3} 
-- #define XKB_KEY_Pointer_Drag4                 0xfef8
pattern XKB_KEY_Pointer_Drag4 :: KeySymbol
pattern XKB_KEY_Pointer_Drag4 = MkKeySymbol #{const XKB_KEY_Pointer_Drag4} 
-- #define XKB_KEY_Pointer_Drag5                 0xfefd
pattern XKB_KEY_Pointer_Drag5 :: KeySymbol
pattern XKB_KEY_Pointer_Drag5 = MkKeySymbol #{const XKB_KEY_Pointer_Drag5} 
-- #define XKB_KEY_Pointer_EnableKeys            0xfef9
pattern XKB_KEY_Pointer_EnableKeys :: KeySymbol
pattern XKB_KEY_Pointer_EnableKeys = MkKeySymbol #{const XKB_KEY_Pointer_EnableKeys} 
-- #define XKB_KEY_Pointer_Accelerate            0xfefa
pattern XKB_KEY_Pointer_Accelerate :: KeySymbol
pattern XKB_KEY_Pointer_Accelerate = MkKeySymbol #{const XKB_KEY_Pointer_Accelerate} 
-- #define XKB_KEY_Pointer_DfltBtnNext           0xfefb
pattern XKB_KEY_Pointer_DfltBtnNext :: KeySymbol
pattern XKB_KEY_Pointer_DfltBtnNext = MkKeySymbol #{const XKB_KEY_Pointer_DfltBtnNext} 
-- #define XKB_KEY_Pointer_DfltBtnPrev           0xfefc
pattern XKB_KEY_Pointer_DfltBtnPrev :: KeySymbol
pattern XKB_KEY_Pointer_DfltBtnPrev = MkKeySymbol #{const XKB_KEY_Pointer_DfltBtnPrev} 
-- #define XKB_KEY_ch                            0xfea0
pattern XKB_KEY_ch :: KeySymbol
pattern XKB_KEY_ch = MkKeySymbol #{const XKB_KEY_ch} 
-- #define XKB_KEY_Ch                            0xfea1
pattern XKB_KEY_Ch :: KeySymbol
pattern XKB_KEY_Ch = MkKeySymbol #{const XKB_KEY_Ch} 
-- #define XKB_KEY_CH                            0xfea2
pattern XKB_KEY_CH :: KeySymbol
pattern XKB_KEY_CH = MkKeySymbol #{const XKB_KEY_CH} 
-- #define XKB_KEY_c_h                           0xfea3
pattern XKB_KEY_c_h :: KeySymbol
pattern XKB_KEY_c_h = MkKeySymbol #{const XKB_KEY_c_h} 
-- #define XKB_KEY_C_h                           0xfea4
pattern XKB_KEY_C_h :: KeySymbol
pattern XKB_KEY_C_h = MkKeySymbol #{const XKB_KEY_C_h} 
-- #define XKB_KEY_C_H                           0xfea5
pattern XKB_KEY_C_H :: KeySymbol
pattern XKB_KEY_C_H = MkKeySymbol #{const XKB_KEY_C_H} 
-- #define XKB_KEY_3270_Duplicate                0xfd01
pattern XKB_KEY_3270_Duplicate :: KeySymbol
pattern XKB_KEY_3270_Duplicate = MkKeySymbol #{const XKB_KEY_3270_Duplicate} 
-- #define XKB_KEY_3270_FieldMark                0xfd02
pattern XKB_KEY_3270_FieldMark :: KeySymbol
pattern XKB_KEY_3270_FieldMark = MkKeySymbol #{const XKB_KEY_3270_FieldMark} 
-- #define XKB_KEY_3270_Right2                   0xfd03
pattern XKB_KEY_3270_Right2 :: KeySymbol
pattern XKB_KEY_3270_Right2 = MkKeySymbol #{const XKB_KEY_3270_Right2} 
-- #define XKB_KEY_3270_Left2                    0xfd04
pattern XKB_KEY_3270_Left2 :: KeySymbol
pattern XKB_KEY_3270_Left2 = MkKeySymbol #{const XKB_KEY_3270_Left2} 
-- #define XKB_KEY_3270_BackTab                  0xfd05
pattern XKB_KEY_3270_BackTab :: KeySymbol
pattern XKB_KEY_3270_BackTab = MkKeySymbol #{const XKB_KEY_3270_BackTab} 
-- #define XKB_KEY_3270_EraseEOF                 0xfd06
pattern XKB_KEY_3270_EraseEOF :: KeySymbol
pattern XKB_KEY_3270_EraseEOF = MkKeySymbol #{const XKB_KEY_3270_EraseEOF} 
-- #define XKB_KEY_3270_EraseInput               0xfd07
pattern XKB_KEY_3270_EraseInput :: KeySymbol
pattern XKB_KEY_3270_EraseInput = MkKeySymbol #{const XKB_KEY_3270_EraseInput} 
-- #define XKB_KEY_3270_Reset                    0xfd08
pattern XKB_KEY_3270_Reset :: KeySymbol
pattern XKB_KEY_3270_Reset = MkKeySymbol #{const XKB_KEY_3270_Reset} 
-- #define XKB_KEY_3270_Quit                     0xfd09
pattern XKB_KEY_3270_Quit :: KeySymbol
pattern XKB_KEY_3270_Quit = MkKeySymbol #{const XKB_KEY_3270_Quit} 
-- #define XKB_KEY_3270_PA1                      0xfd0a
pattern XKB_KEY_3270_PA1 :: KeySymbol
pattern XKB_KEY_3270_PA1 = MkKeySymbol #{const XKB_KEY_3270_PA1} 
-- #define XKB_KEY_3270_PA2                      0xfd0b
pattern XKB_KEY_3270_PA2 :: KeySymbol
pattern XKB_KEY_3270_PA2 = MkKeySymbol #{const XKB_KEY_3270_PA2} 
-- #define XKB_KEY_3270_PA3                      0xfd0c
pattern XKB_KEY_3270_PA3 :: KeySymbol
pattern XKB_KEY_3270_PA3 = MkKeySymbol #{const XKB_KEY_3270_PA3} 
-- #define XKB_KEY_3270_Test                     0xfd0d
pattern XKB_KEY_3270_Test :: KeySymbol
pattern XKB_KEY_3270_Test = MkKeySymbol #{const XKB_KEY_3270_Test} 
-- #define XKB_KEY_3270_Attn                     0xfd0e
pattern XKB_KEY_3270_Attn :: KeySymbol
pattern XKB_KEY_3270_Attn = MkKeySymbol #{const XKB_KEY_3270_Attn} 
-- #define XKB_KEY_3270_CursorBlink              0xfd0f
pattern XKB_KEY_3270_CursorBlink :: KeySymbol
pattern XKB_KEY_3270_CursorBlink = MkKeySymbol #{const XKB_KEY_3270_CursorBlink} 
-- #define XKB_KEY_3270_AltCursor                0xfd10
pattern XKB_KEY_3270_AltCursor :: KeySymbol
pattern XKB_KEY_3270_AltCursor = MkKeySymbol #{const XKB_KEY_3270_AltCursor} 
-- #define XKB_KEY_3270_KeyClick                 0xfd11
pattern XKB_KEY_3270_KeyClick :: KeySymbol
pattern XKB_KEY_3270_KeyClick = MkKeySymbol #{const XKB_KEY_3270_KeyClick} 
-- #define XKB_KEY_3270_Jump                     0xfd12
pattern XKB_KEY_3270_Jump :: KeySymbol
pattern XKB_KEY_3270_Jump = MkKeySymbol #{const XKB_KEY_3270_Jump} 
-- #define XKB_KEY_3270_Ident                    0xfd13
pattern XKB_KEY_3270_Ident :: KeySymbol
pattern XKB_KEY_3270_Ident = MkKeySymbol #{const XKB_KEY_3270_Ident} 
-- #define XKB_KEY_3270_Rule                     0xfd14
pattern XKB_KEY_3270_Rule :: KeySymbol
pattern XKB_KEY_3270_Rule = MkKeySymbol #{const XKB_KEY_3270_Rule} 
-- #define XKB_KEY_3270_Copy                     0xfd15
pattern XKB_KEY_3270_Copy :: KeySymbol
pattern XKB_KEY_3270_Copy = MkKeySymbol #{const XKB_KEY_3270_Copy} 
-- #define XKB_KEY_3270_Play                     0xfd16
pattern XKB_KEY_3270_Play :: KeySymbol
pattern XKB_KEY_3270_Play = MkKeySymbol #{const XKB_KEY_3270_Play} 
-- #define XKB_KEY_3270_Setup                    0xfd17
pattern XKB_KEY_3270_Setup :: KeySymbol
pattern XKB_KEY_3270_Setup = MkKeySymbol #{const XKB_KEY_3270_Setup} 
-- #define XKB_KEY_3270_Record                   0xfd18
pattern XKB_KEY_3270_Record :: KeySymbol
pattern XKB_KEY_3270_Record = MkKeySymbol #{const XKB_KEY_3270_Record} 
-- #define XKB_KEY_3270_ChangeScreen             0xfd19
pattern XKB_KEY_3270_ChangeScreen :: KeySymbol
pattern XKB_KEY_3270_ChangeScreen = MkKeySymbol #{const XKB_KEY_3270_ChangeScreen} 
-- #define XKB_KEY_3270_DeleteWord               0xfd1a
pattern XKB_KEY_3270_DeleteWord :: KeySymbol
pattern XKB_KEY_3270_DeleteWord = MkKeySymbol #{const XKB_KEY_3270_DeleteWord} 
-- #define XKB_KEY_3270_ExSelect                 0xfd1b
pattern XKB_KEY_3270_ExSelect :: KeySymbol
pattern XKB_KEY_3270_ExSelect = MkKeySymbol #{const XKB_KEY_3270_ExSelect} 
-- #define XKB_KEY_3270_CursorSelect             0xfd1c
pattern XKB_KEY_3270_CursorSelect :: KeySymbol
pattern XKB_KEY_3270_CursorSelect = MkKeySymbol #{const XKB_KEY_3270_CursorSelect} 
-- #define XKB_KEY_3270_PrintScreen              0xfd1d
pattern XKB_KEY_3270_PrintScreen :: KeySymbol
pattern XKB_KEY_3270_PrintScreen = MkKeySymbol #{const XKB_KEY_3270_PrintScreen} 
-- #define XKB_KEY_3270_Enter                    0xfd1e
pattern XKB_KEY_3270_Enter :: KeySymbol
pattern XKB_KEY_3270_Enter = MkKeySymbol #{const XKB_KEY_3270_Enter} 
-- #define XKB_KEY_space                         0x0020  /* U+0020 SPACE */
pattern XKB_KEY_space :: KeySymbol
pattern XKB_KEY_space = MkKeySymbol #{const XKB_KEY_space} 
-- #define XKB_KEY_exclam                        0x0021  /* U+0021 EXCLAMATION MARK */
pattern XKB_KEY_exclam :: KeySymbol
pattern XKB_KEY_exclam = MkKeySymbol #{const XKB_KEY_exclam} 
-- #define XKB_KEY_quotedbl                      0x0022  /* U+0022 QUOTATION MARK */
pattern XKB_KEY_quotedbl :: KeySymbol
pattern XKB_KEY_quotedbl = MkKeySymbol #{const XKB_KEY_quotedbl} 
-- #define XKB_KEY_numbersign                    0x0023  /* U+0023 NUMBER SIGN */
pattern XKB_KEY_numbersign :: KeySymbol
pattern XKB_KEY_numbersign = MkKeySymbol #{const XKB_KEY_numbersign} 
-- #define XKB_KEY_dollar                        0x0024  /* U+0024 DOLLAR SIGN */
pattern XKB_KEY_dollar :: KeySymbol
pattern XKB_KEY_dollar = MkKeySymbol #{const XKB_KEY_dollar} 
-- #define XKB_KEY_percent                       0x0025  /* U+0025 PERCENT SIGN */
pattern XKB_KEY_percent :: KeySymbol
pattern XKB_KEY_percent = MkKeySymbol #{const XKB_KEY_percent} 
-- #define XKB_KEY_ampersand                     0x0026  /* U+0026 AMPERSAND */
pattern XKB_KEY_ampersand :: KeySymbol
pattern XKB_KEY_ampersand = MkKeySymbol #{const XKB_KEY_ampersand} 
-- #define XKB_KEY_apostrophe                    0x0027  /* U+0027 APOSTROPHE */
pattern XKB_KEY_apostrophe :: KeySymbol
pattern XKB_KEY_apostrophe = MkKeySymbol #{const XKB_KEY_apostrophe} 
-- #define XKB_KEY_quoteright                    0x0027  /* deprecated */
pattern XKB_KEY_quoteright :: KeySymbol
pattern XKB_KEY_quoteright = MkKeySymbol #{const XKB_KEY_quoteright} 
-- #define XKB_KEY_parenleft                     0x0028  /* U+0028 LEFT PARENTHESIS */
pattern XKB_KEY_parenleft :: KeySymbol
pattern XKB_KEY_parenleft = MkKeySymbol #{const XKB_KEY_parenleft} 
-- #define XKB_KEY_parenright                    0x0029  /* U+0029 RIGHT PARENTHESIS */
pattern XKB_KEY_parenright :: KeySymbol
pattern XKB_KEY_parenright = MkKeySymbol #{const XKB_KEY_parenright} 
-- #define XKB_KEY_asterisk                      0x002a  /* U+002A ASTERISK */
pattern XKB_KEY_asterisk :: KeySymbol
pattern XKB_KEY_asterisk = MkKeySymbol #{const XKB_KEY_asterisk} 
-- #define XKB_KEY_plus                          0x002b  /* U+002B PLUS SIGN */
pattern XKB_KEY_plus :: KeySymbol
pattern XKB_KEY_plus = MkKeySymbol #{const XKB_KEY_plus} 
-- #define XKB_KEY_comma                         0x002c  /* U+002C COMMA */
pattern XKB_KEY_comma :: KeySymbol
pattern XKB_KEY_comma = MkKeySymbol #{const XKB_KEY_comma} 
-- #define XKB_KEY_minus                         0x002d  /* U+002D HYPHEN-MINUS */
pattern XKB_KEY_minus :: KeySymbol
pattern XKB_KEY_minus = MkKeySymbol #{const XKB_KEY_minus} 
-- #define XKB_KEY_period                        0x002e  /* U+002E FULL STOP */
pattern XKB_KEY_period :: KeySymbol
pattern XKB_KEY_period = MkKeySymbol #{const XKB_KEY_period} 
-- #define XKB_KEY_slash                         0x002f  /* U+002F SOLIDUS */
pattern XKB_KEY_slash :: KeySymbol
pattern XKB_KEY_slash = MkKeySymbol #{const XKB_KEY_slash} 
-- #define XKB_KEY_0                             0x0030  /* U+0030 DIGIT ZERO */
pattern XKB_KEY_0 :: KeySymbol
pattern XKB_KEY_0 = MkKeySymbol #{const XKB_KEY_0} 
-- #define XKB_KEY_1                             0x0031  /* U+0031 DIGIT ONE */
pattern XKB_KEY_1 :: KeySymbol
pattern XKB_KEY_1 = MkKeySymbol #{const XKB_KEY_1} 
-- #define XKB_KEY_2                             0x0032  /* U+0032 DIGIT TWO */
pattern XKB_KEY_2 :: KeySymbol
pattern XKB_KEY_2 = MkKeySymbol #{const XKB_KEY_2} 
-- #define XKB_KEY_3                             0x0033  /* U+0033 DIGIT THREE */
pattern XKB_KEY_3 :: KeySymbol
pattern XKB_KEY_3 = MkKeySymbol #{const XKB_KEY_3} 
-- #define XKB_KEY_4                             0x0034  /* U+0034 DIGIT FOUR */
pattern XKB_KEY_4 :: KeySymbol
pattern XKB_KEY_4 = MkKeySymbol #{const XKB_KEY_4} 
-- #define XKB_KEY_5                             0x0035  /* U+0035 DIGIT FIVE */
pattern XKB_KEY_5 :: KeySymbol
pattern XKB_KEY_5 = MkKeySymbol #{const XKB_KEY_5} 
-- #define XKB_KEY_6                             0x0036  /* U+0036 DIGIT SIX */
pattern XKB_KEY_6 :: KeySymbol
pattern XKB_KEY_6 = MkKeySymbol #{const XKB_KEY_6} 
-- #define XKB_KEY_7                             0x0037  /* U+0037 DIGIT SEVEN */
pattern XKB_KEY_7 :: KeySymbol
pattern XKB_KEY_7 = MkKeySymbol #{const XKB_KEY_7} 
-- #define XKB_KEY_8                             0x0038  /* U+0038 DIGIT EIGHT */
pattern XKB_KEY_8 :: KeySymbol
pattern XKB_KEY_8 = MkKeySymbol #{const XKB_KEY_8} 
-- #define XKB_KEY_9                             0x0039  /* U+0039 DIGIT NINE */
pattern XKB_KEY_9 :: KeySymbol
pattern XKB_KEY_9 = MkKeySymbol #{const XKB_KEY_9} 
-- #define XKB_KEY_colon                         0x003a  /* U+003A COLON */
pattern XKB_KEY_colon :: KeySymbol
pattern XKB_KEY_colon = MkKeySymbol #{const XKB_KEY_colon} 
-- #define XKB_KEY_semicolon                     0x003b  /* U+003B SEMICOLON */
pattern XKB_KEY_semicolon :: KeySymbol
pattern XKB_KEY_semicolon = MkKeySymbol #{const XKB_KEY_semicolon} 
-- #define XKB_KEY_less                          0x003c  /* U+003C LESS-THAN SIGN */
pattern XKB_KEY_less :: KeySymbol
pattern XKB_KEY_less = MkKeySymbol #{const XKB_KEY_less} 
-- #define XKB_KEY_equal                         0x003d  /* U+003D EQUALS SIGN */
pattern XKB_KEY_equal :: KeySymbol
pattern XKB_KEY_equal = MkKeySymbol #{const XKB_KEY_equal} 
-- #define XKB_KEY_greater                       0x003e  /* U+003E GREATER-THAN SIGN */
pattern XKB_KEY_greater :: KeySymbol
pattern XKB_KEY_greater = MkKeySymbol #{const XKB_KEY_greater} 
-- #define XKB_KEY_question                      0x003f  /* U+003F QUESTION MARK */
pattern XKB_KEY_question :: KeySymbol
pattern XKB_KEY_question = MkKeySymbol #{const XKB_KEY_question} 
-- #define XKB_KEY_at                            0x0040  /* U+0040 COMMERCIAL AT */
pattern XKB_KEY_at :: KeySymbol
pattern XKB_KEY_at = MkKeySymbol #{const XKB_KEY_at} 
-- #define XKB_KEY_A                             0x0041  /* U+0041 LATIN CAPITAL LETTER A */
pattern XKB_KEY_A :: KeySymbol
pattern XKB_KEY_A = MkKeySymbol #{const XKB_KEY_A} 
-- #define XKB_KEY_B                             0x0042  /* U+0042 LATIN CAPITAL LETTER B */
pattern XKB_KEY_B :: KeySymbol
pattern XKB_KEY_B = MkKeySymbol #{const XKB_KEY_B} 
-- #define XKB_KEY_C                             0x0043  /* U+0043 LATIN CAPITAL LETTER C */
pattern XKB_KEY_C :: KeySymbol
pattern XKB_KEY_C = MkKeySymbol #{const XKB_KEY_C} 
-- #define XKB_KEY_D                             0x0044  /* U+0044 LATIN CAPITAL LETTER D */
pattern XKB_KEY_D :: KeySymbol
pattern XKB_KEY_D = MkKeySymbol #{const XKB_KEY_D} 
-- #define XKB_KEY_E                             0x0045  /* U+0045 LATIN CAPITAL LETTER E */
pattern XKB_KEY_E :: KeySymbol
pattern XKB_KEY_E = MkKeySymbol #{const XKB_KEY_E} 
-- #define XKB_KEY_F                             0x0046  /* U+0046 LATIN CAPITAL LETTER F */
pattern XKB_KEY_F :: KeySymbol
pattern XKB_KEY_F = MkKeySymbol #{const XKB_KEY_F} 
-- #define XKB_KEY_G                             0x0047  /* U+0047 LATIN CAPITAL LETTER G */
pattern XKB_KEY_G :: KeySymbol
pattern XKB_KEY_G = MkKeySymbol #{const XKB_KEY_G} 
-- #define XKB_KEY_H                             0x0048  /* U+0048 LATIN CAPITAL LETTER H */
pattern XKB_KEY_H :: KeySymbol
pattern XKB_KEY_H = MkKeySymbol #{const XKB_KEY_H} 
-- #define XKB_KEY_I                             0x0049  /* U+0049 LATIN CAPITAL LETTER I */
pattern XKB_KEY_I :: KeySymbol
pattern XKB_KEY_I = MkKeySymbol #{const XKB_KEY_I} 
-- #define XKB_KEY_J                             0x004a  /* U+004A LATIN CAPITAL LETTER J */
pattern XKB_KEY_J :: KeySymbol
pattern XKB_KEY_J = MkKeySymbol #{const XKB_KEY_J} 
-- #define XKB_KEY_K                             0x004b  /* U+004B LATIN CAPITAL LETTER K */
pattern XKB_KEY_K :: KeySymbol
pattern XKB_KEY_K = MkKeySymbol #{const XKB_KEY_K} 
-- #define XKB_KEY_L                             0x004c  /* U+004C LATIN CAPITAL LETTER L */
pattern XKB_KEY_L :: KeySymbol
pattern XKB_KEY_L = MkKeySymbol #{const XKB_KEY_L} 
-- #define XKB_KEY_M                             0x004d  /* U+004D LATIN CAPITAL LETTER M */
pattern XKB_KEY_M :: KeySymbol
pattern XKB_KEY_M = MkKeySymbol #{const XKB_KEY_M} 
-- #define XKB_KEY_N                             0x004e  /* U+004E LATIN CAPITAL LETTER N */
pattern XKB_KEY_N :: KeySymbol
pattern XKB_KEY_N = MkKeySymbol #{const XKB_KEY_N} 
-- #define XKB_KEY_O                             0x004f  /* U+004F LATIN CAPITAL LETTER O */
pattern XKB_KEY_O :: KeySymbol
pattern XKB_KEY_O = MkKeySymbol #{const XKB_KEY_O} 
-- #define XKB_KEY_P                             0x0050  /* U+0050 LATIN CAPITAL LETTER P */
pattern XKB_KEY_P :: KeySymbol
pattern XKB_KEY_P = MkKeySymbol #{const XKB_KEY_P} 
-- #define XKB_KEY_Q                             0x0051  /* U+0051 LATIN CAPITAL LETTER Q */
pattern XKB_KEY_Q :: KeySymbol
pattern XKB_KEY_Q = MkKeySymbol #{const XKB_KEY_Q} 
-- #define XKB_KEY_R                             0x0052  /* U+0052 LATIN CAPITAL LETTER R */
pattern XKB_KEY_R :: KeySymbol
pattern XKB_KEY_R = MkKeySymbol #{const XKB_KEY_R} 
-- #define XKB_KEY_S                             0x0053  /* U+0053 LATIN CAPITAL LETTER S */
pattern XKB_KEY_S :: KeySymbol
pattern XKB_KEY_S = MkKeySymbol #{const XKB_KEY_S} 
-- #define XKB_KEY_T                             0x0054  /* U+0054 LATIN CAPITAL LETTER T */
pattern XKB_KEY_T :: KeySymbol
pattern XKB_KEY_T = MkKeySymbol #{const XKB_KEY_T} 
-- #define XKB_KEY_U                             0x0055  /* U+0055 LATIN CAPITAL LETTER U */
pattern XKB_KEY_U :: KeySymbol
pattern XKB_KEY_U = MkKeySymbol #{const XKB_KEY_U} 
-- #define XKB_KEY_V                             0x0056  /* U+0056 LATIN CAPITAL LETTER V */
pattern XKB_KEY_V :: KeySymbol
pattern XKB_KEY_V = MkKeySymbol #{const XKB_KEY_V} 
-- #define XKB_KEY_W                             0x0057  /* U+0057 LATIN CAPITAL LETTER W */
pattern XKB_KEY_W :: KeySymbol
pattern XKB_KEY_W = MkKeySymbol #{const XKB_KEY_W} 
-- #define XKB_KEY_X                             0x0058  /* U+0058 LATIN CAPITAL LETTER X */
pattern XKB_KEY_X :: KeySymbol
pattern XKB_KEY_X = MkKeySymbol #{const XKB_KEY_X} 
-- #define XKB_KEY_Y                             0x0059  /* U+0059 LATIN CAPITAL LETTER Y */
pattern XKB_KEY_Y :: KeySymbol
pattern XKB_KEY_Y = MkKeySymbol #{const XKB_KEY_Y} 
-- #define XKB_KEY_Z                             0x005a  /* U+005A LATIN CAPITAL LETTER Z */
pattern XKB_KEY_Z :: KeySymbol
pattern XKB_KEY_Z = MkKeySymbol #{const XKB_KEY_Z} 
-- #define XKB_KEY_bracketleft                   0x005b  /* U+005B LEFT SQUARE BRACKET */
pattern XKB_KEY_bracketleft :: KeySymbol
pattern XKB_KEY_bracketleft = MkKeySymbol #{const XKB_KEY_bracketleft} 
-- #define XKB_KEY_backslash                     0x005c  /* U+005C REVERSE SOLIDUS */
pattern XKB_KEY_backslash :: KeySymbol
pattern XKB_KEY_backslash = MkKeySymbol #{const XKB_KEY_backslash} 
-- #define XKB_KEY_bracketright                  0x005d  /* U+005D RIGHT SQUARE BRACKET */
pattern XKB_KEY_bracketright :: KeySymbol
pattern XKB_KEY_bracketright = MkKeySymbol #{const XKB_KEY_bracketright} 
-- #define XKB_KEY_asciicircum                   0x005e  /* U+005E CIRCUMFLEX ACCENT */
pattern XKB_KEY_asciicircum :: KeySymbol
pattern XKB_KEY_asciicircum = MkKeySymbol #{const XKB_KEY_asciicircum} 
-- #define XKB_KEY_underscore                    0x005f  /* U+005F LOW LINE */
pattern XKB_KEY_underscore :: KeySymbol
pattern XKB_KEY_underscore = MkKeySymbol #{const XKB_KEY_underscore} 
-- #define XKB_KEY_grave                         0x0060  /* U+0060 GRAVE ACCENT */
pattern XKB_KEY_grave :: KeySymbol
pattern XKB_KEY_grave = MkKeySymbol #{const XKB_KEY_grave} 
-- #define XKB_KEY_quoteleft                     0x0060  /* deprecated */
pattern XKB_KEY_quoteleft :: KeySymbol
pattern XKB_KEY_quoteleft = MkKeySymbol #{const XKB_KEY_quoteleft} 
-- #define XKB_KEY_a                             0x0061  /* U+0061 LATIN SMALL LETTER A */
pattern XKB_KEY_a :: KeySymbol
pattern XKB_KEY_a = MkKeySymbol #{const XKB_KEY_a} 
-- #define XKB_KEY_b                             0x0062  /* U+0062 LATIN SMALL LETTER B */
pattern XKB_KEY_b :: KeySymbol
pattern XKB_KEY_b = MkKeySymbol #{const XKB_KEY_b} 
-- #define XKB_KEY_c                             0x0063  /* U+0063 LATIN SMALL LETTER C */
pattern XKB_KEY_c :: KeySymbol
pattern XKB_KEY_c = MkKeySymbol #{const XKB_KEY_c} 
-- #define XKB_KEY_d                             0x0064  /* U+0064 LATIN SMALL LETTER D */
pattern XKB_KEY_d :: KeySymbol
pattern XKB_KEY_d = MkKeySymbol #{const XKB_KEY_d} 
-- #define XKB_KEY_e                             0x0065  /* U+0065 LATIN SMALL LETTER E */
pattern XKB_KEY_e :: KeySymbol
pattern XKB_KEY_e = MkKeySymbol #{const XKB_KEY_e} 
-- #define XKB_KEY_f                             0x0066  /* U+0066 LATIN SMALL LETTER F */
pattern XKB_KEY_f :: KeySymbol
pattern XKB_KEY_f = MkKeySymbol #{const XKB_KEY_f} 
-- #define XKB_KEY_g                             0x0067  /* U+0067 LATIN SMALL LETTER G */
pattern XKB_KEY_g :: KeySymbol
pattern XKB_KEY_g = MkKeySymbol #{const XKB_KEY_g} 
-- #define XKB_KEY_h                             0x0068  /* U+0068 LATIN SMALL LETTER H */
pattern XKB_KEY_h :: KeySymbol
pattern XKB_KEY_h = MkKeySymbol #{const XKB_KEY_h} 
-- #define XKB_KEY_i                             0x0069  /* U+0069 LATIN SMALL LETTER I */
pattern XKB_KEY_i :: KeySymbol
pattern XKB_KEY_i = MkKeySymbol #{const XKB_KEY_i} 
-- #define XKB_KEY_j                             0x006a  /* U+006A LATIN SMALL LETTER J */
pattern XKB_KEY_j :: KeySymbol
pattern XKB_KEY_j = MkKeySymbol #{const XKB_KEY_j} 
-- #define XKB_KEY_k                             0x006b  /* U+006B LATIN SMALL LETTER K */
pattern XKB_KEY_k :: KeySymbol
pattern XKB_KEY_k = MkKeySymbol #{const XKB_KEY_k} 
-- #define XKB_KEY_l                             0x006c  /* U+006C LATIN SMALL LETTER L */
pattern XKB_KEY_l :: KeySymbol
pattern XKB_KEY_l = MkKeySymbol #{const XKB_KEY_l} 
-- #define XKB_KEY_m                             0x006d  /* U+006D LATIN SMALL LETTER M */
pattern XKB_KEY_m :: KeySymbol
pattern XKB_KEY_m = MkKeySymbol #{const XKB_KEY_m} 
-- #define XKB_KEY_n                             0x006e  /* U+006E LATIN SMALL LETTER N */
pattern XKB_KEY_n :: KeySymbol
pattern XKB_KEY_n = MkKeySymbol #{const XKB_KEY_n} 
-- #define XKB_KEY_o                             0x006f  /* U+006F LATIN SMALL LETTER O */
pattern XKB_KEY_o :: KeySymbol
pattern XKB_KEY_o = MkKeySymbol #{const XKB_KEY_o} 
-- #define XKB_KEY_p                             0x0070  /* U+0070 LATIN SMALL LETTER P */
pattern XKB_KEY_p :: KeySymbol
pattern XKB_KEY_p = MkKeySymbol #{const XKB_KEY_p} 
-- #define XKB_KEY_q                             0x0071  /* U+0071 LATIN SMALL LETTER Q */
pattern XKB_KEY_q :: KeySymbol
pattern XKB_KEY_q = MkKeySymbol #{const XKB_KEY_q} 
-- #define XKB_KEY_r                             0x0072  /* U+0072 LATIN SMALL LETTER R */
pattern XKB_KEY_r :: KeySymbol
pattern XKB_KEY_r = MkKeySymbol #{const XKB_KEY_r} 
-- #define XKB_KEY_s                             0x0073  /* U+0073 LATIN SMALL LETTER S */
pattern XKB_KEY_s :: KeySymbol
pattern XKB_KEY_s = MkKeySymbol #{const XKB_KEY_s} 
-- #define XKB_KEY_t                             0x0074  /* U+0074 LATIN SMALL LETTER T */
pattern XKB_KEY_t :: KeySymbol
pattern XKB_KEY_t = MkKeySymbol #{const XKB_KEY_t} 
-- #define XKB_KEY_u                             0x0075  /* U+0075 LATIN SMALL LETTER U */
pattern XKB_KEY_u :: KeySymbol
pattern XKB_KEY_u = MkKeySymbol #{const XKB_KEY_u} 
-- #define XKB_KEY_v                             0x0076  /* U+0076 LATIN SMALL LETTER V */
pattern XKB_KEY_v :: KeySymbol
pattern XKB_KEY_v = MkKeySymbol #{const XKB_KEY_v} 
-- #define XKB_KEY_w                             0x0077  /* U+0077 LATIN SMALL LETTER W */
pattern XKB_KEY_w :: KeySymbol
pattern XKB_KEY_w = MkKeySymbol #{const XKB_KEY_w} 
-- #define XKB_KEY_x                             0x0078  /* U+0078 LATIN SMALL LETTER X */
pattern XKB_KEY_x :: KeySymbol
pattern XKB_KEY_x = MkKeySymbol #{const XKB_KEY_x} 
-- #define XKB_KEY_y                             0x0079  /* U+0079 LATIN SMALL LETTER Y */
pattern XKB_KEY_y :: KeySymbol
pattern XKB_KEY_y = MkKeySymbol #{const XKB_KEY_y} 
-- #define XKB_KEY_z                             0x007a  /* U+007A LATIN SMALL LETTER Z */
pattern XKB_KEY_z :: KeySymbol
pattern XKB_KEY_z = MkKeySymbol #{const XKB_KEY_z} 
-- #define XKB_KEY_braceleft                     0x007b  /* U+007B LEFT CURLY BRACKET */
pattern XKB_KEY_braceleft :: KeySymbol
pattern XKB_KEY_braceleft = MkKeySymbol #{const XKB_KEY_braceleft} 
-- #define XKB_KEY_bar                           0x007c  /* U+007C VERTICAL LINE */
pattern XKB_KEY_bar :: KeySymbol
pattern XKB_KEY_bar = MkKeySymbol #{const XKB_KEY_bar} 
-- #define XKB_KEY_braceright                    0x007d  /* U+007D RIGHT CURLY BRACKET */
pattern XKB_KEY_braceright :: KeySymbol
pattern XKB_KEY_braceright = MkKeySymbol #{const XKB_KEY_braceright} 
-- #define XKB_KEY_asciitilde                    0x007e  /* U+007E TILDE */
pattern XKB_KEY_asciitilde :: KeySymbol
pattern XKB_KEY_asciitilde = MkKeySymbol #{const XKB_KEY_asciitilde} 
-- #define XKB_KEY_nobreakspace                  0x00a0  /* U+00A0 NO-BREAK SPACE */
pattern XKB_KEY_nobreakspace :: KeySymbol
pattern XKB_KEY_nobreakspace = MkKeySymbol #{const XKB_KEY_nobreakspace} 
-- #define XKB_KEY_exclamdown                    0x00a1  /* U+00A1 INVERTED EXCLAMATION MARK */
pattern XKB_KEY_exclamdown :: KeySymbol
pattern XKB_KEY_exclamdown = MkKeySymbol #{const XKB_KEY_exclamdown} 
-- #define XKB_KEY_cent                          0x00a2  /* U+00A2 CENT SIGN */
pattern XKB_KEY_cent :: KeySymbol
pattern XKB_KEY_cent = MkKeySymbol #{const XKB_KEY_cent} 
-- #define XKB_KEY_sterling                      0x00a3  /* U+00A3 POUND SIGN */
pattern XKB_KEY_sterling :: KeySymbol
pattern XKB_KEY_sterling = MkKeySymbol #{const XKB_KEY_sterling} 
-- #define XKB_KEY_currency                      0x00a4  /* U+00A4 CURRENCY SIGN */
pattern XKB_KEY_currency :: KeySymbol
pattern XKB_KEY_currency = MkKeySymbol #{const XKB_KEY_currency} 
-- #define XKB_KEY_yen                           0x00a5  /* U+00A5 YEN SIGN */
pattern XKB_KEY_yen :: KeySymbol
pattern XKB_KEY_yen = MkKeySymbol #{const XKB_KEY_yen} 
-- #define XKB_KEY_brokenbar                     0x00a6  /* U+00A6 BROKEN BAR */
pattern XKB_KEY_brokenbar :: KeySymbol
pattern XKB_KEY_brokenbar = MkKeySymbol #{const XKB_KEY_brokenbar} 
-- #define XKB_KEY_section                       0x00a7  /* U+00A7 SECTION SIGN */
pattern XKB_KEY_section :: KeySymbol
pattern XKB_KEY_section = MkKeySymbol #{const XKB_KEY_section} 
-- #define XKB_KEY_diaeresis                     0x00a8  /* U+00A8 DIAERESIS */
pattern XKB_KEY_diaeresis :: KeySymbol
pattern XKB_KEY_diaeresis = MkKeySymbol #{const XKB_KEY_diaeresis} 
-- #define XKB_KEY_copyright                     0x00a9  /* U+00A9 COPYRIGHT SIGN */
pattern XKB_KEY_copyright :: KeySymbol
pattern XKB_KEY_copyright = MkKeySymbol #{const XKB_KEY_copyright} 
-- #define XKB_KEY_ordfeminine                   0x00aa  /* U+00AA FEMININE ORDINAL INDICATOR */
pattern XKB_KEY_ordfeminine :: KeySymbol
pattern XKB_KEY_ordfeminine = MkKeySymbol #{const XKB_KEY_ordfeminine} 
-- #define XKB_KEY_guillemotleft                 0x00ab  /* U+00AB LEFT-POINTING DOUBLE ANGLE QUOTATION MARK */
pattern XKB_KEY_guillemotleft :: KeySymbol
pattern XKB_KEY_guillemotleft = MkKeySymbol #{const XKB_KEY_guillemotleft} 
-- #define XKB_KEY_notsign                       0x00ac  /* U+00AC NOT SIGN */
pattern XKB_KEY_notsign :: KeySymbol
pattern XKB_KEY_notsign = MkKeySymbol #{const XKB_KEY_notsign} 
-- #define XKB_KEY_hyphen                        0x00ad  /* U+00AD SOFT HYPHEN */
pattern XKB_KEY_hyphen :: KeySymbol
pattern XKB_KEY_hyphen = MkKeySymbol #{const XKB_KEY_hyphen} 
-- #define XKB_KEY_registered                    0x00ae  /* U+00AE REGISTERED SIGN */
pattern XKB_KEY_registered :: KeySymbol
pattern XKB_KEY_registered = MkKeySymbol #{const XKB_KEY_registered} 
-- #define XKB_KEY_macron                        0x00af  /* U+00AF MACRON */
pattern XKB_KEY_macron :: KeySymbol
pattern XKB_KEY_macron = MkKeySymbol #{const XKB_KEY_macron} 
-- #define XKB_KEY_degree                        0x00b0  /* U+00B0 DEGREE SIGN */
pattern XKB_KEY_degree :: KeySymbol
pattern XKB_KEY_degree = MkKeySymbol #{const XKB_KEY_degree} 
-- #define XKB_KEY_plusminus                     0x00b1  /* U+00B1 PLUS-MINUS SIGN */
pattern XKB_KEY_plusminus :: KeySymbol
pattern XKB_KEY_plusminus = MkKeySymbol #{const XKB_KEY_plusminus} 
-- #define XKB_KEY_twosuperior                   0x00b2  /* U+00B2 SUPERSCRIPT TWO */
pattern XKB_KEY_twosuperior :: KeySymbol
pattern XKB_KEY_twosuperior = MkKeySymbol #{const XKB_KEY_twosuperior} 
-- #define XKB_KEY_threesuperior                 0x00b3  /* U+00B3 SUPERSCRIPT THREE */
pattern XKB_KEY_threesuperior :: KeySymbol
pattern XKB_KEY_threesuperior = MkKeySymbol #{const XKB_KEY_threesuperior} 
-- #define XKB_KEY_acute                         0x00b4  /* U+00B4 ACUTE ACCENT */
pattern XKB_KEY_acute :: KeySymbol
pattern XKB_KEY_acute = MkKeySymbol #{const XKB_KEY_acute} 
-- #define XKB_KEY_mu                            0x00b5  /* U+00B5 MICRO SIGN */
pattern XKB_KEY_mu :: KeySymbol
pattern XKB_KEY_mu = MkKeySymbol #{const XKB_KEY_mu} 
-- #define XKB_KEY_paragraph                     0x00b6  /* U+00B6 PILCROW SIGN */
pattern XKB_KEY_paragraph :: KeySymbol
pattern XKB_KEY_paragraph = MkKeySymbol #{const XKB_KEY_paragraph} 
-- #define XKB_KEY_periodcentered                0x00b7  /* U+00B7 MIDDLE DOT */
pattern XKB_KEY_periodcentered :: KeySymbol
pattern XKB_KEY_periodcentered = MkKeySymbol #{const XKB_KEY_periodcentered} 
-- #define XKB_KEY_cedilla                       0x00b8  /* U+00B8 CEDILLA */
pattern XKB_KEY_cedilla :: KeySymbol
pattern XKB_KEY_cedilla = MkKeySymbol #{const XKB_KEY_cedilla} 
-- #define XKB_KEY_onesuperior                   0x00b9  /* U+00B9 SUPERSCRIPT ONE */
pattern XKB_KEY_onesuperior :: KeySymbol
pattern XKB_KEY_onesuperior = MkKeySymbol #{const XKB_KEY_onesuperior} 
-- #define XKB_KEY_masculine                     0x00ba  /* U+00BA MASCULINE ORDINAL INDICATOR */
pattern XKB_KEY_masculine :: KeySymbol
pattern XKB_KEY_masculine = MkKeySymbol #{const XKB_KEY_masculine} 
-- #define XKB_KEY_guillemotright                0x00bb  /* U+00BB RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK */
pattern XKB_KEY_guillemotright :: KeySymbol
pattern XKB_KEY_guillemotright = MkKeySymbol #{const XKB_KEY_guillemotright} 
-- #define XKB_KEY_onequarter                    0x00bc  /* U+00BC VULGAR FRACTION ONE QUARTER */
pattern XKB_KEY_onequarter :: KeySymbol
pattern XKB_KEY_onequarter = MkKeySymbol #{const XKB_KEY_onequarter} 
-- #define XKB_KEY_onehalf                       0x00bd  /* U+00BD VULGAR FRACTION ONE HALF */
pattern XKB_KEY_onehalf :: KeySymbol
pattern XKB_KEY_onehalf = MkKeySymbol #{const XKB_KEY_onehalf} 
-- #define XKB_KEY_threequarters                 0x00be  /* U+00BE VULGAR FRACTION THREE QUARTERS */
pattern XKB_KEY_threequarters :: KeySymbol
pattern XKB_KEY_threequarters = MkKeySymbol #{const XKB_KEY_threequarters} 
-- #define XKB_KEY_questiondown                  0x00bf  /* U+00BF INVERTED QUESTION MARK */
pattern XKB_KEY_questiondown :: KeySymbol
pattern XKB_KEY_questiondown = MkKeySymbol #{const XKB_KEY_questiondown} 
-- #define XKB_KEY_Agrave                        0x00c0  /* U+00C0 LATIN CAPITAL LETTER A WITH GRAVE */
pattern XKB_KEY_Agrave :: KeySymbol
pattern XKB_KEY_Agrave = MkKeySymbol #{const XKB_KEY_Agrave} 
-- #define XKB_KEY_Aacute                        0x00c1  /* U+00C1 LATIN CAPITAL LETTER A WITH ACUTE */
pattern XKB_KEY_Aacute :: KeySymbol
pattern XKB_KEY_Aacute = MkKeySymbol #{const XKB_KEY_Aacute} 
-- #define XKB_KEY_Acircumflex                   0x00c2  /* U+00C2 LATIN CAPITAL LETTER A WITH CIRCUMFLEX */
pattern XKB_KEY_Acircumflex :: KeySymbol
pattern XKB_KEY_Acircumflex = MkKeySymbol #{const XKB_KEY_Acircumflex} 
-- #define XKB_KEY_Atilde                        0x00c3  /* U+00C3 LATIN CAPITAL LETTER A WITH TILDE */
pattern XKB_KEY_Atilde :: KeySymbol
pattern XKB_KEY_Atilde = MkKeySymbol #{const XKB_KEY_Atilde} 
-- #define XKB_KEY_Adiaeresis                    0x00c4  /* U+00C4 LATIN CAPITAL LETTER A WITH DIAERESIS */
pattern XKB_KEY_Adiaeresis :: KeySymbol
pattern XKB_KEY_Adiaeresis = MkKeySymbol #{const XKB_KEY_Adiaeresis} 
-- #define XKB_KEY_Aring                         0x00c5  /* U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE */
pattern XKB_KEY_Aring :: KeySymbol
pattern XKB_KEY_Aring = MkKeySymbol #{const XKB_KEY_Aring} 
-- #define XKB_KEY_AE                            0x00c6  /* U+00C6 LATIN CAPITAL LETTER AE */
pattern XKB_KEY_AE :: KeySymbol
pattern XKB_KEY_AE = MkKeySymbol #{const XKB_KEY_AE} 
-- #define XKB_KEY_Ccedilla                      0x00c7  /* U+00C7 LATIN CAPITAL LETTER C WITH CEDILLA */
pattern XKB_KEY_Ccedilla :: KeySymbol
pattern XKB_KEY_Ccedilla = MkKeySymbol #{const XKB_KEY_Ccedilla} 
-- #define XKB_KEY_Egrave                        0x00c8  /* U+00C8 LATIN CAPITAL LETTER E WITH GRAVE */
pattern XKB_KEY_Egrave :: KeySymbol
pattern XKB_KEY_Egrave = MkKeySymbol #{const XKB_KEY_Egrave} 
-- #define XKB_KEY_Eacute                        0x00c9  /* U+00C9 LATIN CAPITAL LETTER E WITH ACUTE */
pattern XKB_KEY_Eacute :: KeySymbol
pattern XKB_KEY_Eacute = MkKeySymbol #{const XKB_KEY_Eacute} 
-- #define XKB_KEY_Ecircumflex                   0x00ca  /* U+00CA LATIN CAPITAL LETTER E WITH CIRCUMFLEX */
pattern XKB_KEY_Ecircumflex :: KeySymbol
pattern XKB_KEY_Ecircumflex = MkKeySymbol #{const XKB_KEY_Ecircumflex} 
-- #define XKB_KEY_Ediaeresis                    0x00cb  /* U+00CB LATIN CAPITAL LETTER E WITH DIAERESIS */
pattern XKB_KEY_Ediaeresis :: KeySymbol
pattern XKB_KEY_Ediaeresis = MkKeySymbol #{const XKB_KEY_Ediaeresis} 
-- #define XKB_KEY_Igrave                        0x00cc  /* U+00CC LATIN CAPITAL LETTER I WITH GRAVE */
pattern XKB_KEY_Igrave :: KeySymbol
pattern XKB_KEY_Igrave = MkKeySymbol #{const XKB_KEY_Igrave} 
-- #define XKB_KEY_Iacute                        0x00cd  /* U+00CD LATIN CAPITAL LETTER I WITH ACUTE */
pattern XKB_KEY_Iacute :: KeySymbol
pattern XKB_KEY_Iacute = MkKeySymbol #{const XKB_KEY_Iacute} 
-- #define XKB_KEY_Icircumflex                   0x00ce  /* U+00CE LATIN CAPITAL LETTER I WITH CIRCUMFLEX */
pattern XKB_KEY_Icircumflex :: KeySymbol
pattern XKB_KEY_Icircumflex = MkKeySymbol #{const XKB_KEY_Icircumflex} 
-- #define XKB_KEY_Idiaeresis                    0x00cf  /* U+00CF LATIN CAPITAL LETTER I WITH DIAERESIS */
pattern XKB_KEY_Idiaeresis :: KeySymbol
pattern XKB_KEY_Idiaeresis = MkKeySymbol #{const XKB_KEY_Idiaeresis} 
-- #define XKB_KEY_ETH                           0x00d0  /* U+00D0 LATIN CAPITAL LETTER ETH */
pattern XKB_KEY_ETH :: KeySymbol
pattern XKB_KEY_ETH = MkKeySymbol #{const XKB_KEY_ETH} 
-- #define XKB_KEY_Eth                           0x00d0  /* deprecated */
pattern XKB_KEY_Eth :: KeySymbol
pattern XKB_KEY_Eth = MkKeySymbol #{const XKB_KEY_Eth} 
-- #define XKB_KEY_Ntilde                        0x00d1  /* U+00D1 LATIN CAPITAL LETTER N WITH TILDE */
pattern XKB_KEY_Ntilde :: KeySymbol
pattern XKB_KEY_Ntilde = MkKeySymbol #{const XKB_KEY_Ntilde} 
-- #define XKB_KEY_Ograve                        0x00d2  /* U+00D2 LATIN CAPITAL LETTER O WITH GRAVE */
pattern XKB_KEY_Ograve :: KeySymbol
pattern XKB_KEY_Ograve = MkKeySymbol #{const XKB_KEY_Ograve} 
-- #define XKB_KEY_Oacute                        0x00d3  /* U+00D3 LATIN CAPITAL LETTER O WITH ACUTE */
pattern XKB_KEY_Oacute :: KeySymbol
pattern XKB_KEY_Oacute = MkKeySymbol #{const XKB_KEY_Oacute} 
-- #define XKB_KEY_Ocircumflex                   0x00d4  /* U+00D4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX */
pattern XKB_KEY_Ocircumflex :: KeySymbol
pattern XKB_KEY_Ocircumflex = MkKeySymbol #{const XKB_KEY_Ocircumflex} 
-- #define XKB_KEY_Otilde                        0x00d5  /* U+00D5 LATIN CAPITAL LETTER O WITH TILDE */
pattern XKB_KEY_Otilde :: KeySymbol
pattern XKB_KEY_Otilde = MkKeySymbol #{const XKB_KEY_Otilde} 
-- #define XKB_KEY_Odiaeresis                    0x00d6  /* U+00D6 LATIN CAPITAL LETTER O WITH DIAERESIS */
pattern XKB_KEY_Odiaeresis :: KeySymbol
pattern XKB_KEY_Odiaeresis = MkKeySymbol #{const XKB_KEY_Odiaeresis} 
-- #define XKB_KEY_multiply                      0x00d7  /* U+00D7 MULTIPLICATION SIGN */
pattern XKB_KEY_multiply :: KeySymbol
pattern XKB_KEY_multiply = MkKeySymbol #{const XKB_KEY_multiply} 
-- #define XKB_KEY_Oslash                        0x00d8  /* U+00D8 LATIN CAPITAL LETTER O WITH STROKE */
pattern XKB_KEY_Oslash :: KeySymbol
pattern XKB_KEY_Oslash = MkKeySymbol #{const XKB_KEY_Oslash} 
-- #define XKB_KEY_Ooblique                      0x00d8  /* U+00D8 LATIN CAPITAL LETTER O WITH STROKE */
pattern XKB_KEY_Ooblique :: KeySymbol
pattern XKB_KEY_Ooblique = MkKeySymbol #{const XKB_KEY_Ooblique} 
-- #define XKB_KEY_Ugrave                        0x00d9  /* U+00D9 LATIN CAPITAL LETTER U WITH GRAVE */
pattern XKB_KEY_Ugrave :: KeySymbol
pattern XKB_KEY_Ugrave = MkKeySymbol #{const XKB_KEY_Ugrave} 
-- #define XKB_KEY_Uacute                        0x00da  /* U+00DA LATIN CAPITAL LETTER U WITH ACUTE */
pattern XKB_KEY_Uacute :: KeySymbol
pattern XKB_KEY_Uacute = MkKeySymbol #{const XKB_KEY_Uacute} 
-- #define XKB_KEY_Ucircumflex                   0x00db  /* U+00DB LATIN CAPITAL LETTER U WITH CIRCUMFLEX */
pattern XKB_KEY_Ucircumflex :: KeySymbol
pattern XKB_KEY_Ucircumflex = MkKeySymbol #{const XKB_KEY_Ucircumflex} 
-- #define XKB_KEY_Udiaeresis                    0x00dc  /* U+00DC LATIN CAPITAL LETTER U WITH DIAERESIS */
pattern XKB_KEY_Udiaeresis :: KeySymbol
pattern XKB_KEY_Udiaeresis = MkKeySymbol #{const XKB_KEY_Udiaeresis} 
-- #define XKB_KEY_Yacute                        0x00dd  /* U+00DD LATIN CAPITAL LETTER Y WITH ACUTE */
pattern XKB_KEY_Yacute :: KeySymbol
pattern XKB_KEY_Yacute = MkKeySymbol #{const XKB_KEY_Yacute} 
-- #define XKB_KEY_THORN                         0x00de  /* U+00DE LATIN CAPITAL LETTER THORN */
pattern XKB_KEY_THORN :: KeySymbol
pattern XKB_KEY_THORN = MkKeySymbol #{const XKB_KEY_THORN} 
-- #define XKB_KEY_Thorn                         0x00de  /* deprecated */
pattern XKB_KEY_Thorn :: KeySymbol
pattern XKB_KEY_Thorn = MkKeySymbol #{const XKB_KEY_Thorn} 
-- #define XKB_KEY_ssharp                        0x00df  /* U+00DF LATIN SMALL LETTER SHARP S */
pattern XKB_KEY_ssharp :: KeySymbol
pattern XKB_KEY_ssharp = MkKeySymbol #{const XKB_KEY_ssharp} 
-- #define XKB_KEY_agrave                        0x00e0  /* U+00E0 LATIN SMALL LETTER A WITH GRAVE */
pattern XKB_KEY_agrave :: KeySymbol
pattern XKB_KEY_agrave = MkKeySymbol #{const XKB_KEY_agrave} 
-- #define XKB_KEY_aacute                        0x00e1  /* U+00E1 LATIN SMALL LETTER A WITH ACUTE */
pattern XKB_KEY_aacute :: KeySymbol
pattern XKB_KEY_aacute = MkKeySymbol #{const XKB_KEY_aacute} 
-- #define XKB_KEY_acircumflex                   0x00e2  /* U+00E2 LATIN SMALL LETTER A WITH CIRCUMFLEX */
pattern XKB_KEY_acircumflex :: KeySymbol
pattern XKB_KEY_acircumflex = MkKeySymbol #{const XKB_KEY_acircumflex} 
-- #define XKB_KEY_atilde                        0x00e3  /* U+00E3 LATIN SMALL LETTER A WITH TILDE */
pattern XKB_KEY_atilde :: KeySymbol
pattern XKB_KEY_atilde = MkKeySymbol #{const XKB_KEY_atilde} 
-- #define XKB_KEY_adiaeresis                    0x00e4  /* U+00E4 LATIN SMALL LETTER A WITH DIAERESIS */
pattern XKB_KEY_adiaeresis :: KeySymbol
pattern XKB_KEY_adiaeresis = MkKeySymbol #{const XKB_KEY_adiaeresis} 
-- #define XKB_KEY_aring                         0x00e5  /* U+00E5 LATIN SMALL LETTER A WITH RING ABOVE */
pattern XKB_KEY_aring :: KeySymbol
pattern XKB_KEY_aring = MkKeySymbol #{const XKB_KEY_aring} 
-- #define XKB_KEY_ae                            0x00e6  /* U+00E6 LATIN SMALL LETTER AE */
pattern XKB_KEY_ae :: KeySymbol
pattern XKB_KEY_ae = MkKeySymbol #{const XKB_KEY_ae} 
-- #define XKB_KEY_ccedilla                      0x00e7  /* U+00E7 LATIN SMALL LETTER C WITH CEDILLA */
pattern XKB_KEY_ccedilla :: KeySymbol
pattern XKB_KEY_ccedilla = MkKeySymbol #{const XKB_KEY_ccedilla} 
-- #define XKB_KEY_egrave                        0x00e8  /* U+00E8 LATIN SMALL LETTER E WITH GRAVE */
pattern XKB_KEY_egrave :: KeySymbol
pattern XKB_KEY_egrave = MkKeySymbol #{const XKB_KEY_egrave} 
-- #define XKB_KEY_eacute                        0x00e9  /* U+00E9 LATIN SMALL LETTER E WITH ACUTE */
pattern XKB_KEY_eacute :: KeySymbol
pattern XKB_KEY_eacute = MkKeySymbol #{const XKB_KEY_eacute} 
-- #define XKB_KEY_ecircumflex                   0x00ea  /* U+00EA LATIN SMALL LETTER E WITH CIRCUMFLEX */
pattern XKB_KEY_ecircumflex :: KeySymbol
pattern XKB_KEY_ecircumflex = MkKeySymbol #{const XKB_KEY_ecircumflex} 
-- #define XKB_KEY_ediaeresis                    0x00eb  /* U+00EB LATIN SMALL LETTER E WITH DIAERESIS */
pattern XKB_KEY_ediaeresis :: KeySymbol
pattern XKB_KEY_ediaeresis = MkKeySymbol #{const XKB_KEY_ediaeresis} 
-- #define XKB_KEY_igrave                        0x00ec  /* U+00EC LATIN SMALL LETTER I WITH GRAVE */
pattern XKB_KEY_igrave :: KeySymbol
pattern XKB_KEY_igrave = MkKeySymbol #{const XKB_KEY_igrave} 
-- #define XKB_KEY_iacute                        0x00ed  /* U+00ED LATIN SMALL LETTER I WITH ACUTE */
pattern XKB_KEY_iacute :: KeySymbol
pattern XKB_KEY_iacute = MkKeySymbol #{const XKB_KEY_iacute} 
-- #define XKB_KEY_icircumflex                   0x00ee  /* U+00EE LATIN SMALL LETTER I WITH CIRCUMFLEX */
pattern XKB_KEY_icircumflex :: KeySymbol
pattern XKB_KEY_icircumflex = MkKeySymbol #{const XKB_KEY_icircumflex} 
-- #define XKB_KEY_idiaeresis                    0x00ef  /* U+00EF LATIN SMALL LETTER I WITH DIAERESIS */
pattern XKB_KEY_idiaeresis :: KeySymbol
pattern XKB_KEY_idiaeresis = MkKeySymbol #{const XKB_KEY_idiaeresis} 
-- #define XKB_KEY_eth                           0x00f0  /* U+00F0 LATIN SMALL LETTER ETH */
pattern XKB_KEY_eth :: KeySymbol
pattern XKB_KEY_eth = MkKeySymbol #{const XKB_KEY_eth} 
-- #define XKB_KEY_ntilde                        0x00f1  /* U+00F1 LATIN SMALL LETTER N WITH TILDE */
pattern XKB_KEY_ntilde :: KeySymbol
pattern XKB_KEY_ntilde = MkKeySymbol #{const XKB_KEY_ntilde} 
-- #define XKB_KEY_ograve                        0x00f2  /* U+00F2 LATIN SMALL LETTER O WITH GRAVE */
pattern XKB_KEY_ograve :: KeySymbol
pattern XKB_KEY_ograve = MkKeySymbol #{const XKB_KEY_ograve} 
-- #define XKB_KEY_oacute                        0x00f3  /* U+00F3 LATIN SMALL LETTER O WITH ACUTE */
pattern XKB_KEY_oacute :: KeySymbol
pattern XKB_KEY_oacute = MkKeySymbol #{const XKB_KEY_oacute} 
-- #define XKB_KEY_ocircumflex                   0x00f4  /* U+00F4 LATIN SMALL LETTER O WITH CIRCUMFLEX */
pattern XKB_KEY_ocircumflex :: KeySymbol
pattern XKB_KEY_ocircumflex = MkKeySymbol #{const XKB_KEY_ocircumflex} 
-- #define XKB_KEY_otilde                        0x00f5  /* U+00F5 LATIN SMALL LETTER O WITH TILDE */
pattern XKB_KEY_otilde :: KeySymbol
pattern XKB_KEY_otilde = MkKeySymbol #{const XKB_KEY_otilde} 
-- #define XKB_KEY_odiaeresis                    0x00f6  /* U+00F6 LATIN SMALL LETTER O WITH DIAERESIS */
pattern XKB_KEY_odiaeresis :: KeySymbol
pattern XKB_KEY_odiaeresis = MkKeySymbol #{const XKB_KEY_odiaeresis} 
-- #define XKB_KEY_division                      0x00f7  /* U+00F7 DIVISION SIGN */
pattern XKB_KEY_division :: KeySymbol
pattern XKB_KEY_division = MkKeySymbol #{const XKB_KEY_division} 
-- #define XKB_KEY_oslash                        0x00f8  /* U+00F8 LATIN SMALL LETTER O WITH STROKE */
pattern XKB_KEY_oslash :: KeySymbol
pattern XKB_KEY_oslash = MkKeySymbol #{const XKB_KEY_oslash} 
-- #define XKB_KEY_ooblique                      0x00f8  /* U+00F8 LATIN SMALL LETTER O WITH STROKE */
pattern XKB_KEY_ooblique :: KeySymbol
pattern XKB_KEY_ooblique = MkKeySymbol #{const XKB_KEY_ooblique} 
-- #define XKB_KEY_ugrave                        0x00f9  /* U+00F9 LATIN SMALL LETTER U WITH GRAVE */
pattern XKB_KEY_ugrave :: KeySymbol
pattern XKB_KEY_ugrave = MkKeySymbol #{const XKB_KEY_ugrave} 
-- #define XKB_KEY_uacute                        0x00fa  /* U+00FA LATIN SMALL LETTER U WITH ACUTE */
pattern XKB_KEY_uacute :: KeySymbol
pattern XKB_KEY_uacute = MkKeySymbol #{const XKB_KEY_uacute} 
-- #define XKB_KEY_ucircumflex                   0x00fb  /* U+00FB LATIN SMALL LETTER U WITH CIRCUMFLEX */
pattern XKB_KEY_ucircumflex :: KeySymbol
pattern XKB_KEY_ucircumflex = MkKeySymbol #{const XKB_KEY_ucircumflex} 
-- #define XKB_KEY_udiaeresis                    0x00fc  /* U+00FC LATIN SMALL LETTER U WITH DIAERESIS */
pattern XKB_KEY_udiaeresis :: KeySymbol
pattern XKB_KEY_udiaeresis = MkKeySymbol #{const XKB_KEY_udiaeresis} 
-- #define XKB_KEY_yacute                        0x00fd  /* U+00FD LATIN SMALL LETTER Y WITH ACUTE */
pattern XKB_KEY_yacute :: KeySymbol
pattern XKB_KEY_yacute = MkKeySymbol #{const XKB_KEY_yacute} 
-- #define XKB_KEY_thorn                         0x00fe  /* U+00FE LATIN SMALL LETTER THORN */
pattern XKB_KEY_thorn :: KeySymbol
pattern XKB_KEY_thorn = MkKeySymbol #{const XKB_KEY_thorn} 
-- #define XKB_KEY_ydiaeresis                    0x00ff  /* U+00FF LATIN SMALL LETTER Y WITH DIAERESIS */
pattern XKB_KEY_ydiaeresis :: KeySymbol
pattern XKB_KEY_ydiaeresis = MkKeySymbol #{const XKB_KEY_ydiaeresis} 
-- #define XKB_KEY_Aogonek                       0x01a1  /* U+0104 LATIN CAPITAL LETTER A WITH OGONEK */
pattern XKB_KEY_Aogonek :: KeySymbol
pattern XKB_KEY_Aogonek = MkKeySymbol #{const XKB_KEY_Aogonek} 
-- #define XKB_KEY_breve                         0x01a2  /* U+02D8 BREVE */
pattern XKB_KEY_breve :: KeySymbol
pattern XKB_KEY_breve = MkKeySymbol #{const XKB_KEY_breve} 
-- #define XKB_KEY_Lstroke                       0x01a3  /* U+0141 LATIN CAPITAL LETTER L WITH STROKE */
pattern XKB_KEY_Lstroke :: KeySymbol
pattern XKB_KEY_Lstroke = MkKeySymbol #{const XKB_KEY_Lstroke} 
-- #define XKB_KEY_Lcaron                        0x01a5  /* U+013D LATIN CAPITAL LETTER L WITH CARON */
pattern XKB_KEY_Lcaron :: KeySymbol
pattern XKB_KEY_Lcaron = MkKeySymbol #{const XKB_KEY_Lcaron} 
-- #define XKB_KEY_Sacute                        0x01a6  /* U+015A LATIN CAPITAL LETTER S WITH ACUTE */
pattern XKB_KEY_Sacute :: KeySymbol
pattern XKB_KEY_Sacute = MkKeySymbol #{const XKB_KEY_Sacute} 
-- #define XKB_KEY_Scaron                        0x01a9  /* U+0160 LATIN CAPITAL LETTER S WITH CARON */
pattern XKB_KEY_Scaron :: KeySymbol
pattern XKB_KEY_Scaron = MkKeySymbol #{const XKB_KEY_Scaron} 
-- #define XKB_KEY_Scedilla                      0x01aa  /* U+015E LATIN CAPITAL LETTER S WITH CEDILLA */
pattern XKB_KEY_Scedilla :: KeySymbol
pattern XKB_KEY_Scedilla = MkKeySymbol #{const XKB_KEY_Scedilla} 
-- #define XKB_KEY_Tcaron                        0x01ab  /* U+0164 LATIN CAPITAL LETTER T WITH CARON */
pattern XKB_KEY_Tcaron :: KeySymbol
pattern XKB_KEY_Tcaron = MkKeySymbol #{const XKB_KEY_Tcaron} 
-- #define XKB_KEY_Zacute                        0x01ac  /* U+0179 LATIN CAPITAL LETTER Z WITH ACUTE */
pattern XKB_KEY_Zacute :: KeySymbol
pattern XKB_KEY_Zacute = MkKeySymbol #{const XKB_KEY_Zacute} 
-- #define XKB_KEY_Zcaron                        0x01ae  /* U+017D LATIN CAPITAL LETTER Z WITH CARON */
pattern XKB_KEY_Zcaron :: KeySymbol
pattern XKB_KEY_Zcaron = MkKeySymbol #{const XKB_KEY_Zcaron} 
-- #define XKB_KEY_Zabovedot                     0x01af  /* U+017B LATIN CAPITAL LETTER Z WITH DOT ABOVE */
pattern XKB_KEY_Zabovedot :: KeySymbol
pattern XKB_KEY_Zabovedot = MkKeySymbol #{const XKB_KEY_Zabovedot} 
-- #define XKB_KEY_aogonek                       0x01b1  /* U+0105 LATIN SMALL LETTER A WITH OGONEK */
pattern XKB_KEY_aogonek :: KeySymbol
pattern XKB_KEY_aogonek = MkKeySymbol #{const XKB_KEY_aogonek} 
-- #define XKB_KEY_ogonek                        0x01b2  /* U+02DB OGONEK */
pattern XKB_KEY_ogonek :: KeySymbol
pattern XKB_KEY_ogonek = MkKeySymbol #{const XKB_KEY_ogonek} 
-- #define XKB_KEY_lstroke                       0x01b3  /* U+0142 LATIN SMALL LETTER L WITH STROKE */
pattern XKB_KEY_lstroke :: KeySymbol
pattern XKB_KEY_lstroke = MkKeySymbol #{const XKB_KEY_lstroke} 
-- #define XKB_KEY_lcaron                        0x01b5  /* U+013E LATIN SMALL LETTER L WITH CARON */
pattern XKB_KEY_lcaron :: KeySymbol
pattern XKB_KEY_lcaron = MkKeySymbol #{const XKB_KEY_lcaron} 
-- #define XKB_KEY_sacute                        0x01b6  /* U+015B LATIN SMALL LETTER S WITH ACUTE */
pattern XKB_KEY_sacute :: KeySymbol
pattern XKB_KEY_sacute = MkKeySymbol #{const XKB_KEY_sacute} 
-- #define XKB_KEY_caron                         0x01b7  /* U+02C7 CARON */
pattern XKB_KEY_caron :: KeySymbol
pattern XKB_KEY_caron = MkKeySymbol #{const XKB_KEY_caron} 
-- #define XKB_KEY_scaron                        0x01b9  /* U+0161 LATIN SMALL LETTER S WITH CARON */
pattern XKB_KEY_scaron :: KeySymbol
pattern XKB_KEY_scaron = MkKeySymbol #{const XKB_KEY_scaron} 
-- #define XKB_KEY_scedilla                      0x01ba  /* U+015F LATIN SMALL LETTER S WITH CEDILLA */
pattern XKB_KEY_scedilla :: KeySymbol
pattern XKB_KEY_scedilla = MkKeySymbol #{const XKB_KEY_scedilla} 
-- #define XKB_KEY_tcaron                        0x01bb  /* U+0165 LATIN SMALL LETTER T WITH CARON */
pattern XKB_KEY_tcaron :: KeySymbol
pattern XKB_KEY_tcaron = MkKeySymbol #{const XKB_KEY_tcaron} 
-- #define XKB_KEY_zacute                        0x01bc  /* U+017A LATIN SMALL LETTER Z WITH ACUTE */
pattern XKB_KEY_zacute :: KeySymbol
pattern XKB_KEY_zacute = MkKeySymbol #{const XKB_KEY_zacute} 
-- #define XKB_KEY_doubleacute                   0x01bd  /* U+02DD DOUBLE ACUTE ACCENT */
pattern XKB_KEY_doubleacute :: KeySymbol
pattern XKB_KEY_doubleacute = MkKeySymbol #{const XKB_KEY_doubleacute} 
-- #define XKB_KEY_zcaron                        0x01be  /* U+017E LATIN SMALL LETTER Z WITH CARON */
pattern XKB_KEY_zcaron :: KeySymbol
pattern XKB_KEY_zcaron = MkKeySymbol #{const XKB_KEY_zcaron} 
-- #define XKB_KEY_zabovedot                     0x01bf  /* U+017C LATIN SMALL LETTER Z WITH DOT ABOVE */
pattern XKB_KEY_zabovedot :: KeySymbol
pattern XKB_KEY_zabovedot = MkKeySymbol #{const XKB_KEY_zabovedot} 
-- #define XKB_KEY_Racute                        0x01c0  /* U+0154 LATIN CAPITAL LETTER R WITH ACUTE */
pattern XKB_KEY_Racute :: KeySymbol
pattern XKB_KEY_Racute = MkKeySymbol #{const XKB_KEY_Racute} 
-- #define XKB_KEY_Abreve                        0x01c3  /* U+0102 LATIN CAPITAL LETTER A WITH BREVE */
pattern XKB_KEY_Abreve :: KeySymbol
pattern XKB_KEY_Abreve = MkKeySymbol #{const XKB_KEY_Abreve} 
-- #define XKB_KEY_Lacute                        0x01c5  /* U+0139 LATIN CAPITAL LETTER L WITH ACUTE */
pattern XKB_KEY_Lacute :: KeySymbol
pattern XKB_KEY_Lacute = MkKeySymbol #{const XKB_KEY_Lacute} 
-- #define XKB_KEY_Cacute                        0x01c6  /* U+0106 LATIN CAPITAL LETTER C WITH ACUTE */
pattern XKB_KEY_Cacute :: KeySymbol
pattern XKB_KEY_Cacute = MkKeySymbol #{const XKB_KEY_Cacute} 
-- #define XKB_KEY_Ccaron                        0x01c8  /* U+010C LATIN CAPITAL LETTER C WITH CARON */
pattern XKB_KEY_Ccaron :: KeySymbol
pattern XKB_KEY_Ccaron = MkKeySymbol #{const XKB_KEY_Ccaron} 
-- #define XKB_KEY_Eogonek                       0x01ca  /* U+0118 LATIN CAPITAL LETTER E WITH OGONEK */
pattern XKB_KEY_Eogonek :: KeySymbol
pattern XKB_KEY_Eogonek = MkKeySymbol #{const XKB_KEY_Eogonek} 
-- #define XKB_KEY_Ecaron                        0x01cc  /* U+011A LATIN CAPITAL LETTER E WITH CARON */
pattern XKB_KEY_Ecaron :: KeySymbol
pattern XKB_KEY_Ecaron = MkKeySymbol #{const XKB_KEY_Ecaron} 
-- #define XKB_KEY_Dcaron                        0x01cf  /* U+010E LATIN CAPITAL LETTER D WITH CARON */
pattern XKB_KEY_Dcaron :: KeySymbol
pattern XKB_KEY_Dcaron = MkKeySymbol #{const XKB_KEY_Dcaron} 
-- #define XKB_KEY_Dstroke                       0x01d0  /* U+0110 LATIN CAPITAL LETTER D WITH STROKE */
pattern XKB_KEY_Dstroke :: KeySymbol
pattern XKB_KEY_Dstroke = MkKeySymbol #{const XKB_KEY_Dstroke} 
-- #define XKB_KEY_Nacute                        0x01d1  /* U+0143 LATIN CAPITAL LETTER N WITH ACUTE */
pattern XKB_KEY_Nacute :: KeySymbol
pattern XKB_KEY_Nacute = MkKeySymbol #{const XKB_KEY_Nacute} 
-- #define XKB_KEY_Ncaron                        0x01d2  /* U+0147 LATIN CAPITAL LETTER N WITH CARON */
pattern XKB_KEY_Ncaron :: KeySymbol
pattern XKB_KEY_Ncaron = MkKeySymbol #{const XKB_KEY_Ncaron} 
-- #define XKB_KEY_Odoubleacute                  0x01d5  /* U+0150 LATIN CAPITAL LETTER O WITH DOUBLE ACUTE */
pattern XKB_KEY_Odoubleacute :: KeySymbol
pattern XKB_KEY_Odoubleacute = MkKeySymbol #{const XKB_KEY_Odoubleacute} 
-- #define XKB_KEY_Rcaron                        0x01d8  /* U+0158 LATIN CAPITAL LETTER R WITH CARON */
pattern XKB_KEY_Rcaron :: KeySymbol
pattern XKB_KEY_Rcaron = MkKeySymbol #{const XKB_KEY_Rcaron} 
-- #define XKB_KEY_Uring                         0x01d9  /* U+016E LATIN CAPITAL LETTER U WITH RING ABOVE */
pattern XKB_KEY_Uring :: KeySymbol
pattern XKB_KEY_Uring = MkKeySymbol #{const XKB_KEY_Uring} 
-- #define XKB_KEY_Udoubleacute                  0x01db  /* U+0170 LATIN CAPITAL LETTER U WITH DOUBLE ACUTE */
pattern XKB_KEY_Udoubleacute :: KeySymbol
pattern XKB_KEY_Udoubleacute = MkKeySymbol #{const XKB_KEY_Udoubleacute} 
-- #define XKB_KEY_Tcedilla                      0x01de  /* U+0162 LATIN CAPITAL LETTER T WITH CEDILLA */
pattern XKB_KEY_Tcedilla :: KeySymbol
pattern XKB_KEY_Tcedilla = MkKeySymbol #{const XKB_KEY_Tcedilla} 
-- #define XKB_KEY_racute                        0x01e0  /* U+0155 LATIN SMALL LETTER R WITH ACUTE */
pattern XKB_KEY_racute :: KeySymbol
pattern XKB_KEY_racute = MkKeySymbol #{const XKB_KEY_racute} 
-- #define XKB_KEY_abreve                        0x01e3  /* U+0103 LATIN SMALL LETTER A WITH BREVE */
pattern XKB_KEY_abreve :: KeySymbol
pattern XKB_KEY_abreve = MkKeySymbol #{const XKB_KEY_abreve} 
-- #define XKB_KEY_lacute                        0x01e5  /* U+013A LATIN SMALL LETTER L WITH ACUTE */
pattern XKB_KEY_lacute :: KeySymbol
pattern XKB_KEY_lacute = MkKeySymbol #{const XKB_KEY_lacute} 
-- #define XKB_KEY_cacute                        0x01e6  /* U+0107 LATIN SMALL LETTER C WITH ACUTE */
pattern XKB_KEY_cacute :: KeySymbol
pattern XKB_KEY_cacute = MkKeySymbol #{const XKB_KEY_cacute} 
-- #define XKB_KEY_ccaron                        0x01e8  /* U+010D LATIN SMALL LETTER C WITH CARON */
pattern XKB_KEY_ccaron :: KeySymbol
pattern XKB_KEY_ccaron = MkKeySymbol #{const XKB_KEY_ccaron} 
-- #define XKB_KEY_eogonek                       0x01ea  /* U+0119 LATIN SMALL LETTER E WITH OGONEK */
pattern XKB_KEY_eogonek :: KeySymbol
pattern XKB_KEY_eogonek = MkKeySymbol #{const XKB_KEY_eogonek} 
-- #define XKB_KEY_ecaron                        0x01ec  /* U+011B LATIN SMALL LETTER E WITH CARON */
pattern XKB_KEY_ecaron :: KeySymbol
pattern XKB_KEY_ecaron = MkKeySymbol #{const XKB_KEY_ecaron} 
-- #define XKB_KEY_dcaron                        0x01ef  /* U+010F LATIN SMALL LETTER D WITH CARON */
pattern XKB_KEY_dcaron :: KeySymbol
pattern XKB_KEY_dcaron = MkKeySymbol #{const XKB_KEY_dcaron} 
-- #define XKB_KEY_dstroke                       0x01f0  /* U+0111 LATIN SMALL LETTER D WITH STROKE */
pattern XKB_KEY_dstroke :: KeySymbol
pattern XKB_KEY_dstroke = MkKeySymbol #{const XKB_KEY_dstroke} 
-- #define XKB_KEY_nacute                        0x01f1  /* U+0144 LATIN SMALL LETTER N WITH ACUTE */
pattern XKB_KEY_nacute :: KeySymbol
pattern XKB_KEY_nacute = MkKeySymbol #{const XKB_KEY_nacute} 
-- #define XKB_KEY_ncaron                        0x01f2  /* U+0148 LATIN SMALL LETTER N WITH CARON */
pattern XKB_KEY_ncaron :: KeySymbol
pattern XKB_KEY_ncaron = MkKeySymbol #{const XKB_KEY_ncaron} 
-- #define XKB_KEY_odoubleacute                  0x01f5  /* U+0151 LATIN SMALL LETTER O WITH DOUBLE ACUTE */
pattern XKB_KEY_odoubleacute :: KeySymbol
pattern XKB_KEY_odoubleacute = MkKeySymbol #{const XKB_KEY_odoubleacute} 
-- #define XKB_KEY_rcaron                        0x01f8  /* U+0159 LATIN SMALL LETTER R WITH CARON */
pattern XKB_KEY_rcaron :: KeySymbol
pattern XKB_KEY_rcaron = MkKeySymbol #{const XKB_KEY_rcaron} 
-- #define XKB_KEY_uring                         0x01f9  /* U+016F LATIN SMALL LETTER U WITH RING ABOVE */
pattern XKB_KEY_uring :: KeySymbol
pattern XKB_KEY_uring = MkKeySymbol #{const XKB_KEY_uring} 
-- #define XKB_KEY_udoubleacute                  0x01fb  /* U+0171 LATIN SMALL LETTER U WITH DOUBLE ACUTE */
pattern XKB_KEY_udoubleacute :: KeySymbol
pattern XKB_KEY_udoubleacute = MkKeySymbol #{const XKB_KEY_udoubleacute} 
-- #define XKB_KEY_tcedilla                      0x01fe  /* U+0163 LATIN SMALL LETTER T WITH CEDILLA */
pattern XKB_KEY_tcedilla :: KeySymbol
pattern XKB_KEY_tcedilla = MkKeySymbol #{const XKB_KEY_tcedilla} 
-- #define XKB_KEY_abovedot                      0x01ff  /* U+02D9 DOT ABOVE */
pattern XKB_KEY_abovedot :: KeySymbol
pattern XKB_KEY_abovedot = MkKeySymbol #{const XKB_KEY_abovedot} 
-- #define XKB_KEY_Hstroke                       0x02a1  /* U+0126 LATIN CAPITAL LETTER H WITH STROKE */
pattern XKB_KEY_Hstroke :: KeySymbol
pattern XKB_KEY_Hstroke = MkKeySymbol #{const XKB_KEY_Hstroke} 
-- #define XKB_KEY_Hcircumflex                   0x02a6  /* U+0124 LATIN CAPITAL LETTER H WITH CIRCUMFLEX */
pattern XKB_KEY_Hcircumflex :: KeySymbol
pattern XKB_KEY_Hcircumflex = MkKeySymbol #{const XKB_KEY_Hcircumflex} 
-- #define XKB_KEY_Iabovedot                     0x02a9  /* U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE */
pattern XKB_KEY_Iabovedot :: KeySymbol
pattern XKB_KEY_Iabovedot = MkKeySymbol #{const XKB_KEY_Iabovedot} 
-- #define XKB_KEY_Gbreve                        0x02ab  /* U+011E LATIN CAPITAL LETTER G WITH BREVE */
pattern XKB_KEY_Gbreve :: KeySymbol
pattern XKB_KEY_Gbreve = MkKeySymbol #{const XKB_KEY_Gbreve} 
-- #define XKB_KEY_Jcircumflex                   0x02ac  /* U+0134 LATIN CAPITAL LETTER J WITH CIRCUMFLEX */
pattern XKB_KEY_Jcircumflex :: KeySymbol
pattern XKB_KEY_Jcircumflex = MkKeySymbol #{const XKB_KEY_Jcircumflex} 
-- #define XKB_KEY_hstroke                       0x02b1  /* U+0127 LATIN SMALL LETTER H WITH STROKE */
pattern XKB_KEY_hstroke :: KeySymbol
pattern XKB_KEY_hstroke = MkKeySymbol #{const XKB_KEY_hstroke} 
-- #define XKB_KEY_hcircumflex                   0x02b6  /* U+0125 LATIN SMALL LETTER H WITH CIRCUMFLEX */
pattern XKB_KEY_hcircumflex :: KeySymbol
pattern XKB_KEY_hcircumflex = MkKeySymbol #{const XKB_KEY_hcircumflex} 
-- #define XKB_KEY_idotless                      0x02b9  /* U+0131 LATIN SMALL LETTER DOTLESS I */
pattern XKB_KEY_idotless :: KeySymbol
pattern XKB_KEY_idotless = MkKeySymbol #{const XKB_KEY_idotless} 
-- #define XKB_KEY_gbreve                        0x02bb  /* U+011F LATIN SMALL LETTER G WITH BREVE */
pattern XKB_KEY_gbreve :: KeySymbol
pattern XKB_KEY_gbreve = MkKeySymbol #{const XKB_KEY_gbreve} 
-- #define XKB_KEY_jcircumflex                   0x02bc  /* U+0135 LATIN SMALL LETTER J WITH CIRCUMFLEX */
pattern XKB_KEY_jcircumflex :: KeySymbol
pattern XKB_KEY_jcircumflex = MkKeySymbol #{const XKB_KEY_jcircumflex} 
-- #define XKB_KEY_Cabovedot                     0x02c5  /* U+010A LATIN CAPITAL LETTER C WITH DOT ABOVE */
pattern XKB_KEY_Cabovedot :: KeySymbol
pattern XKB_KEY_Cabovedot = MkKeySymbol #{const XKB_KEY_Cabovedot} 
-- #define XKB_KEY_Ccircumflex                   0x02c6  /* U+0108 LATIN CAPITAL LETTER C WITH CIRCUMFLEX */
pattern XKB_KEY_Ccircumflex :: KeySymbol
pattern XKB_KEY_Ccircumflex = MkKeySymbol #{const XKB_KEY_Ccircumflex} 
-- #define XKB_KEY_Gabovedot                     0x02d5  /* U+0120 LATIN CAPITAL LETTER G WITH DOT ABOVE */
pattern XKB_KEY_Gabovedot :: KeySymbol
pattern XKB_KEY_Gabovedot = MkKeySymbol #{const XKB_KEY_Gabovedot} 
-- #define XKB_KEY_Gcircumflex                   0x02d8  /* U+011C LATIN CAPITAL LETTER G WITH CIRCUMFLEX */
pattern XKB_KEY_Gcircumflex :: KeySymbol
pattern XKB_KEY_Gcircumflex = MkKeySymbol #{const XKB_KEY_Gcircumflex} 
-- #define XKB_KEY_Ubreve                        0x02dd  /* U+016C LATIN CAPITAL LETTER U WITH BREVE */
pattern XKB_KEY_Ubreve :: KeySymbol
pattern XKB_KEY_Ubreve = MkKeySymbol #{const XKB_KEY_Ubreve} 
-- #define XKB_KEY_Scircumflex                   0x02de  /* U+015C LATIN CAPITAL LETTER S WITH CIRCUMFLEX */
pattern XKB_KEY_Scircumflex :: KeySymbol
pattern XKB_KEY_Scircumflex = MkKeySymbol #{const XKB_KEY_Scircumflex} 
-- #define XKB_KEY_cabovedot                     0x02e5  /* U+010B LATIN SMALL LETTER C WITH DOT ABOVE */
pattern XKB_KEY_cabovedot :: KeySymbol
pattern XKB_KEY_cabovedot = MkKeySymbol #{const XKB_KEY_cabovedot} 
-- #define XKB_KEY_ccircumflex                   0x02e6  /* U+0109 LATIN SMALL LETTER C WITH CIRCUMFLEX */
pattern XKB_KEY_ccircumflex :: KeySymbol
pattern XKB_KEY_ccircumflex = MkKeySymbol #{const XKB_KEY_ccircumflex} 
-- #define XKB_KEY_gabovedot                     0x02f5  /* U+0121 LATIN SMALL LETTER G WITH DOT ABOVE */
pattern XKB_KEY_gabovedot :: KeySymbol
pattern XKB_KEY_gabovedot = MkKeySymbol #{const XKB_KEY_gabovedot} 
-- #define XKB_KEY_gcircumflex                   0x02f8  /* U+011D LATIN SMALL LETTER G WITH CIRCUMFLEX */
pattern XKB_KEY_gcircumflex :: KeySymbol
pattern XKB_KEY_gcircumflex = MkKeySymbol #{const XKB_KEY_gcircumflex} 
-- #define XKB_KEY_ubreve                        0x02fd  /* U+016D LATIN SMALL LETTER U WITH BREVE */
pattern XKB_KEY_ubreve :: KeySymbol
pattern XKB_KEY_ubreve = MkKeySymbol #{const XKB_KEY_ubreve} 
-- #define XKB_KEY_scircumflex                   0x02fe  /* U+015D LATIN SMALL LETTER S WITH CIRCUMFLEX */
pattern XKB_KEY_scircumflex :: KeySymbol
pattern XKB_KEY_scircumflex = MkKeySymbol #{const XKB_KEY_scircumflex} 
-- #define XKB_KEY_kra                           0x03a2  /* U+0138 LATIN SMALL LETTER KRA */
pattern XKB_KEY_kra :: KeySymbol
pattern XKB_KEY_kra = MkKeySymbol #{const XKB_KEY_kra} 
-- #define XKB_KEY_kappa                         0x03a2  /* deprecated */
pattern XKB_KEY_kappa :: KeySymbol
pattern XKB_KEY_kappa = MkKeySymbol #{const XKB_KEY_kappa} 
-- #define XKB_KEY_Rcedilla                      0x03a3  /* U+0156 LATIN CAPITAL LETTER R WITH CEDILLA */
pattern XKB_KEY_Rcedilla :: KeySymbol
pattern XKB_KEY_Rcedilla = MkKeySymbol #{const XKB_KEY_Rcedilla} 
-- #define XKB_KEY_Itilde                        0x03a5  /* U+0128 LATIN CAPITAL LETTER I WITH TILDE */
pattern XKB_KEY_Itilde :: KeySymbol
pattern XKB_KEY_Itilde = MkKeySymbol #{const XKB_KEY_Itilde} 
-- #define XKB_KEY_Lcedilla                      0x03a6  /* U+013B LATIN CAPITAL LETTER L WITH CEDILLA */
pattern XKB_KEY_Lcedilla :: KeySymbol
pattern XKB_KEY_Lcedilla = MkKeySymbol #{const XKB_KEY_Lcedilla} 
-- #define XKB_KEY_Emacron                       0x03aa  /* U+0112 LATIN CAPITAL LETTER E WITH MACRON */
pattern XKB_KEY_Emacron :: KeySymbol
pattern XKB_KEY_Emacron = MkKeySymbol #{const XKB_KEY_Emacron} 
-- #define XKB_KEY_Gcedilla                      0x03ab  /* U+0122 LATIN CAPITAL LETTER G WITH CEDILLA */
pattern XKB_KEY_Gcedilla :: KeySymbol
pattern XKB_KEY_Gcedilla = MkKeySymbol #{const XKB_KEY_Gcedilla} 
-- #define XKB_KEY_Tslash                        0x03ac  /* U+0166 LATIN CAPITAL LETTER T WITH STROKE */
pattern XKB_KEY_Tslash :: KeySymbol
pattern XKB_KEY_Tslash = MkKeySymbol #{const XKB_KEY_Tslash} 
-- #define XKB_KEY_rcedilla                      0x03b3  /* U+0157 LATIN SMALL LETTER R WITH CEDILLA */
pattern XKB_KEY_rcedilla :: KeySymbol
pattern XKB_KEY_rcedilla = MkKeySymbol #{const XKB_KEY_rcedilla} 
-- #define XKB_KEY_itilde                        0x03b5  /* U+0129 LATIN SMALL LETTER I WITH TILDE */
pattern XKB_KEY_itilde :: KeySymbol
pattern XKB_KEY_itilde = MkKeySymbol #{const XKB_KEY_itilde} 
-- #define XKB_KEY_lcedilla                      0x03b6  /* U+013C LATIN SMALL LETTER L WITH CEDILLA */
pattern XKB_KEY_lcedilla :: KeySymbol
pattern XKB_KEY_lcedilla = MkKeySymbol #{const XKB_KEY_lcedilla} 
-- #define XKB_KEY_emacron                       0x03ba  /* U+0113 LATIN SMALL LETTER E WITH MACRON */
pattern XKB_KEY_emacron :: KeySymbol
pattern XKB_KEY_emacron = MkKeySymbol #{const XKB_KEY_emacron} 
-- #define XKB_KEY_gcedilla                      0x03bb  /* U+0123 LATIN SMALL LETTER G WITH CEDILLA */
pattern XKB_KEY_gcedilla :: KeySymbol
pattern XKB_KEY_gcedilla = MkKeySymbol #{const XKB_KEY_gcedilla} 
-- #define XKB_KEY_tslash                        0x03bc  /* U+0167 LATIN SMALL LETTER T WITH STROKE */
pattern XKB_KEY_tslash :: KeySymbol
pattern XKB_KEY_tslash = MkKeySymbol #{const XKB_KEY_tslash} 
-- #define XKB_KEY_ENG                           0x03bd  /* U+014A LATIN CAPITAL LETTER ENG */
pattern XKB_KEY_ENG :: KeySymbol
pattern XKB_KEY_ENG = MkKeySymbol #{const XKB_KEY_ENG} 
-- #define XKB_KEY_eng                           0x03bf  /* U+014B LATIN SMALL LETTER ENG */
pattern XKB_KEY_eng :: KeySymbol
pattern XKB_KEY_eng = MkKeySymbol #{const XKB_KEY_eng} 
-- #define XKB_KEY_Amacron                       0x03c0  /* U+0100 LATIN CAPITAL LETTER A WITH MACRON */
pattern XKB_KEY_Amacron :: KeySymbol
pattern XKB_KEY_Amacron = MkKeySymbol #{const XKB_KEY_Amacron} 
-- #define XKB_KEY_Iogonek                       0x03c7  /* U+012E LATIN CAPITAL LETTER I WITH OGONEK */
pattern XKB_KEY_Iogonek :: KeySymbol
pattern XKB_KEY_Iogonek = MkKeySymbol #{const XKB_KEY_Iogonek} 
-- #define XKB_KEY_Eabovedot                     0x03cc  /* U+0116 LATIN CAPITAL LETTER E WITH DOT ABOVE */
pattern XKB_KEY_Eabovedot :: KeySymbol
pattern XKB_KEY_Eabovedot = MkKeySymbol #{const XKB_KEY_Eabovedot} 
-- #define XKB_KEY_Imacron                       0x03cf  /* U+012A LATIN CAPITAL LETTER I WITH MACRON */
pattern XKB_KEY_Imacron :: KeySymbol
pattern XKB_KEY_Imacron = MkKeySymbol #{const XKB_KEY_Imacron} 
-- #define XKB_KEY_Ncedilla                      0x03d1  /* U+0145 LATIN CAPITAL LETTER N WITH CEDILLA */
pattern XKB_KEY_Ncedilla :: KeySymbol
pattern XKB_KEY_Ncedilla = MkKeySymbol #{const XKB_KEY_Ncedilla} 
-- #define XKB_KEY_Omacron                       0x03d2  /* U+014C LATIN CAPITAL LETTER O WITH MACRON */
pattern XKB_KEY_Omacron :: KeySymbol
pattern XKB_KEY_Omacron = MkKeySymbol #{const XKB_KEY_Omacron} 
-- #define XKB_KEY_Kcedilla                      0x03d3  /* U+0136 LATIN CAPITAL LETTER K WITH CEDILLA */
pattern XKB_KEY_Kcedilla :: KeySymbol
pattern XKB_KEY_Kcedilla = MkKeySymbol #{const XKB_KEY_Kcedilla} 
-- #define XKB_KEY_Uogonek                       0x03d9  /* U+0172 LATIN CAPITAL LETTER U WITH OGONEK */
pattern XKB_KEY_Uogonek :: KeySymbol
pattern XKB_KEY_Uogonek = MkKeySymbol #{const XKB_KEY_Uogonek} 
-- #define XKB_KEY_Utilde                        0x03dd  /* U+0168 LATIN CAPITAL LETTER U WITH TILDE */
pattern XKB_KEY_Utilde :: KeySymbol
pattern XKB_KEY_Utilde = MkKeySymbol #{const XKB_KEY_Utilde} 
-- #define XKB_KEY_Umacron                       0x03de  /* U+016A LATIN CAPITAL LETTER U WITH MACRON */
pattern XKB_KEY_Umacron :: KeySymbol
pattern XKB_KEY_Umacron = MkKeySymbol #{const XKB_KEY_Umacron} 
-- #define XKB_KEY_amacron                       0x03e0  /* U+0101 LATIN SMALL LETTER A WITH MACRON */
pattern XKB_KEY_amacron :: KeySymbol
pattern XKB_KEY_amacron = MkKeySymbol #{const XKB_KEY_amacron} 
-- #define XKB_KEY_iogonek                       0x03e7  /* U+012F LATIN SMALL LETTER I WITH OGONEK */
pattern XKB_KEY_iogonek :: KeySymbol
pattern XKB_KEY_iogonek = MkKeySymbol #{const XKB_KEY_iogonek} 
-- #define XKB_KEY_eabovedot                     0x03ec  /* U+0117 LATIN SMALL LETTER E WITH DOT ABOVE */
pattern XKB_KEY_eabovedot :: KeySymbol
pattern XKB_KEY_eabovedot = MkKeySymbol #{const XKB_KEY_eabovedot} 
-- #define XKB_KEY_imacron                       0x03ef  /* U+012B LATIN SMALL LETTER I WITH MACRON */
pattern XKB_KEY_imacron :: KeySymbol
pattern XKB_KEY_imacron = MkKeySymbol #{const XKB_KEY_imacron} 
-- #define XKB_KEY_ncedilla                      0x03f1  /* U+0146 LATIN SMALL LETTER N WITH CEDILLA */
pattern XKB_KEY_ncedilla :: KeySymbol
pattern XKB_KEY_ncedilla = MkKeySymbol #{const XKB_KEY_ncedilla} 
-- #define XKB_KEY_omacron                       0x03f2  /* U+014D LATIN SMALL LETTER O WITH MACRON */
pattern XKB_KEY_omacron :: KeySymbol
pattern XKB_KEY_omacron = MkKeySymbol #{const XKB_KEY_omacron} 
-- #define XKB_KEY_kcedilla                      0x03f3  /* U+0137 LATIN SMALL LETTER K WITH CEDILLA */
pattern XKB_KEY_kcedilla :: KeySymbol
pattern XKB_KEY_kcedilla = MkKeySymbol #{const XKB_KEY_kcedilla} 
-- #define XKB_KEY_uogonek                       0x03f9  /* U+0173 LATIN SMALL LETTER U WITH OGONEK */
pattern XKB_KEY_uogonek :: KeySymbol
pattern XKB_KEY_uogonek = MkKeySymbol #{const XKB_KEY_uogonek} 
-- #define XKB_KEY_utilde                        0x03fd  /* U+0169 LATIN SMALL LETTER U WITH TILDE */
pattern XKB_KEY_utilde :: KeySymbol
pattern XKB_KEY_utilde = MkKeySymbol #{const XKB_KEY_utilde} 
-- #define XKB_KEY_umacron                       0x03fe  /* U+016B LATIN SMALL LETTER U WITH MACRON */
pattern XKB_KEY_umacron :: KeySymbol
pattern XKB_KEY_umacron = MkKeySymbol #{const XKB_KEY_umacron} 
-- #define XKB_KEY_Wcircumflex                0x1000174  /* U+0174 LATIN CAPITAL LETTER W WITH CIRCUMFLEX */
pattern XKB_KEY_Wcircumflex :: KeySymbol
pattern XKB_KEY_Wcircumflex = MkKeySymbol #{const XKB_KEY_Wcircumflex} 
-- #define XKB_KEY_wcircumflex                0x1000175  /* U+0175 LATIN SMALL LETTER W WITH CIRCUMFLEX */
pattern XKB_KEY_wcircumflex :: KeySymbol
pattern XKB_KEY_wcircumflex = MkKeySymbol #{const XKB_KEY_wcircumflex} 
-- #define XKB_KEY_Ycircumflex                0x1000176  /* U+0176 LATIN CAPITAL LETTER Y WITH CIRCUMFLEX */
pattern XKB_KEY_Ycircumflex :: KeySymbol
pattern XKB_KEY_Ycircumflex = MkKeySymbol #{const XKB_KEY_Ycircumflex} 
-- #define XKB_KEY_ycircumflex                0x1000177  /* U+0177 LATIN SMALL LETTER Y WITH CIRCUMFLEX */
pattern XKB_KEY_ycircumflex :: KeySymbol
pattern XKB_KEY_ycircumflex = MkKeySymbol #{const XKB_KEY_ycircumflex} 
-- #define XKB_KEY_Babovedot                  0x1001e02  /* U+1E02 LATIN CAPITAL LETTER B WITH DOT ABOVE */
pattern XKB_KEY_Babovedot :: KeySymbol
pattern XKB_KEY_Babovedot = MkKeySymbol #{const XKB_KEY_Babovedot} 
-- #define XKB_KEY_babovedot                  0x1001e03  /* U+1E03 LATIN SMALL LETTER B WITH DOT ABOVE */
pattern XKB_KEY_babovedot :: KeySymbol
pattern XKB_KEY_babovedot = MkKeySymbol #{const XKB_KEY_babovedot} 
-- #define XKB_KEY_Dabovedot                  0x1001e0a  /* U+1E0A LATIN CAPITAL LETTER D WITH DOT ABOVE */
pattern XKB_KEY_Dabovedot :: KeySymbol
pattern XKB_KEY_Dabovedot = MkKeySymbol #{const XKB_KEY_Dabovedot} 
-- #define XKB_KEY_dabovedot                  0x1001e0b  /* U+1E0B LATIN SMALL LETTER D WITH DOT ABOVE */
pattern XKB_KEY_dabovedot :: KeySymbol
pattern XKB_KEY_dabovedot = MkKeySymbol #{const XKB_KEY_dabovedot} 
-- #define XKB_KEY_Fabovedot                  0x1001e1e  /* U+1E1E LATIN CAPITAL LETTER F WITH DOT ABOVE */
pattern XKB_KEY_Fabovedot :: KeySymbol
pattern XKB_KEY_Fabovedot = MkKeySymbol #{const XKB_KEY_Fabovedot} 
-- #define XKB_KEY_fabovedot                  0x1001e1f  /* U+1E1F LATIN SMALL LETTER F WITH DOT ABOVE */
pattern XKB_KEY_fabovedot :: KeySymbol
pattern XKB_KEY_fabovedot = MkKeySymbol #{const XKB_KEY_fabovedot} 
-- #define XKB_KEY_Mabovedot                  0x1001e40  /* U+1E40 LATIN CAPITAL LETTER M WITH DOT ABOVE */
pattern XKB_KEY_Mabovedot :: KeySymbol
pattern XKB_KEY_Mabovedot = MkKeySymbol #{const XKB_KEY_Mabovedot} 
-- #define XKB_KEY_mabovedot                  0x1001e41  /* U+1E41 LATIN SMALL LETTER M WITH DOT ABOVE */
pattern XKB_KEY_mabovedot :: KeySymbol
pattern XKB_KEY_mabovedot = MkKeySymbol #{const XKB_KEY_mabovedot} 
-- #define XKB_KEY_Pabovedot                  0x1001e56  /* U+1E56 LATIN CAPITAL LETTER P WITH DOT ABOVE */
pattern XKB_KEY_Pabovedot :: KeySymbol
pattern XKB_KEY_Pabovedot = MkKeySymbol #{const XKB_KEY_Pabovedot} 
-- #define XKB_KEY_pabovedot                  0x1001e57  /* U+1E57 LATIN SMALL LETTER P WITH DOT ABOVE */
pattern XKB_KEY_pabovedot :: KeySymbol
pattern XKB_KEY_pabovedot = MkKeySymbol #{const XKB_KEY_pabovedot} 
-- #define XKB_KEY_Sabovedot                  0x1001e60  /* U+1E60 LATIN CAPITAL LETTER S WITH DOT ABOVE */
pattern XKB_KEY_Sabovedot :: KeySymbol
pattern XKB_KEY_Sabovedot = MkKeySymbol #{const XKB_KEY_Sabovedot} 
-- #define XKB_KEY_sabovedot                  0x1001e61  /* U+1E61 LATIN SMALL LETTER S WITH DOT ABOVE */
pattern XKB_KEY_sabovedot :: KeySymbol
pattern XKB_KEY_sabovedot = MkKeySymbol #{const XKB_KEY_sabovedot} 
-- #define XKB_KEY_Tabovedot                  0x1001e6a  /* U+1E6A LATIN CAPITAL LETTER T WITH DOT ABOVE */
pattern XKB_KEY_Tabovedot :: KeySymbol
pattern XKB_KEY_Tabovedot = MkKeySymbol #{const XKB_KEY_Tabovedot} 
-- #define XKB_KEY_tabovedot                  0x1001e6b  /* U+1E6B LATIN SMALL LETTER T WITH DOT ABOVE */
pattern XKB_KEY_tabovedot :: KeySymbol
pattern XKB_KEY_tabovedot = MkKeySymbol #{const XKB_KEY_tabovedot} 
-- #define XKB_KEY_Wgrave                     0x1001e80  /* U+1E80 LATIN CAPITAL LETTER W WITH GRAVE */
pattern XKB_KEY_Wgrave :: KeySymbol
pattern XKB_KEY_Wgrave = MkKeySymbol #{const XKB_KEY_Wgrave} 
-- #define XKB_KEY_wgrave                     0x1001e81  /* U+1E81 LATIN SMALL LETTER W WITH GRAVE */
pattern XKB_KEY_wgrave :: KeySymbol
pattern XKB_KEY_wgrave = MkKeySymbol #{const XKB_KEY_wgrave} 
-- #define XKB_KEY_Wacute                     0x1001e82  /* U+1E82 LATIN CAPITAL LETTER W WITH ACUTE */
pattern XKB_KEY_Wacute :: KeySymbol
pattern XKB_KEY_Wacute = MkKeySymbol #{const XKB_KEY_Wacute} 
-- #define XKB_KEY_wacute                     0x1001e83  /* U+1E83 LATIN SMALL LETTER W WITH ACUTE */
pattern XKB_KEY_wacute :: KeySymbol
pattern XKB_KEY_wacute = MkKeySymbol #{const XKB_KEY_wacute} 
-- #define XKB_KEY_Wdiaeresis                 0x1001e84  /* U+1E84 LATIN CAPITAL LETTER W WITH DIAERESIS */
pattern XKB_KEY_Wdiaeresis :: KeySymbol
pattern XKB_KEY_Wdiaeresis = MkKeySymbol #{const XKB_KEY_Wdiaeresis} 
-- #define XKB_KEY_wdiaeresis                 0x1001e85  /* U+1E85 LATIN SMALL LETTER W WITH DIAERESIS */
pattern XKB_KEY_wdiaeresis :: KeySymbol
pattern XKB_KEY_wdiaeresis = MkKeySymbol #{const XKB_KEY_wdiaeresis} 
-- #define XKB_KEY_Ygrave                     0x1001ef2  /* U+1EF2 LATIN CAPITAL LETTER Y WITH GRAVE */
pattern XKB_KEY_Ygrave :: KeySymbol
pattern XKB_KEY_Ygrave = MkKeySymbol #{const XKB_KEY_Ygrave} 
-- #define XKB_KEY_ygrave                     0x1001ef3  /* U+1EF3 LATIN SMALL LETTER Y WITH GRAVE */
pattern XKB_KEY_ygrave :: KeySymbol
pattern XKB_KEY_ygrave = MkKeySymbol #{const XKB_KEY_ygrave} 
-- #define XKB_KEY_OE                            0x13bc  /* U+0152 LATIN CAPITAL LIGATURE OE */
pattern XKB_KEY_OE :: KeySymbol
pattern XKB_KEY_OE = MkKeySymbol #{const XKB_KEY_OE} 
-- #define XKB_KEY_oe                            0x13bd  /* U+0153 LATIN SMALL LIGATURE OE */
pattern XKB_KEY_oe :: KeySymbol
pattern XKB_KEY_oe = MkKeySymbol #{const XKB_KEY_oe} 
-- #define XKB_KEY_Ydiaeresis                    0x13be  /* U+0178 LATIN CAPITAL LETTER Y WITH DIAERESIS */
pattern XKB_KEY_Ydiaeresis :: KeySymbol
pattern XKB_KEY_Ydiaeresis = MkKeySymbol #{const XKB_KEY_Ydiaeresis} 
-- #define XKB_KEY_overline                      0x047e  /* U+203E OVERLINE */
pattern XKB_KEY_overline :: KeySymbol
pattern XKB_KEY_overline = MkKeySymbol #{const XKB_KEY_overline} 
-- #define XKB_KEY_kana_fullstop                 0x04a1  /* U+3002 IDEOGRAPHIC FULL STOP */
pattern XKB_KEY_kana_fullstop :: KeySymbol
pattern XKB_KEY_kana_fullstop = MkKeySymbol #{const XKB_KEY_kana_fullstop} 
-- #define XKB_KEY_kana_openingbracket           0x04a2  /* U+300C LEFT CORNER BRACKET */
pattern XKB_KEY_kana_openingbracket :: KeySymbol
pattern XKB_KEY_kana_openingbracket = MkKeySymbol #{const XKB_KEY_kana_openingbracket} 
-- #define XKB_KEY_kana_closingbracket           0x04a3  /* U+300D RIGHT CORNER BRACKET */
pattern XKB_KEY_kana_closingbracket :: KeySymbol
pattern XKB_KEY_kana_closingbracket = MkKeySymbol #{const XKB_KEY_kana_closingbracket} 
-- #define XKB_KEY_kana_comma                    0x04a4  /* U+3001 IDEOGRAPHIC COMMA */
pattern XKB_KEY_kana_comma :: KeySymbol
pattern XKB_KEY_kana_comma = MkKeySymbol #{const XKB_KEY_kana_comma} 
-- #define XKB_KEY_kana_conjunctive              0x04a5  /* U+30FB KATAKANA MIDDLE DOT */
pattern XKB_KEY_kana_conjunctive :: KeySymbol
pattern XKB_KEY_kana_conjunctive = MkKeySymbol #{const XKB_KEY_kana_conjunctive} 
-- #define XKB_KEY_kana_middledot                0x04a5  /* deprecated */
pattern XKB_KEY_kana_middledot :: KeySymbol
pattern XKB_KEY_kana_middledot = MkKeySymbol #{const XKB_KEY_kana_middledot} 
-- #define XKB_KEY_kana_WO                       0x04a6  /* U+30F2 KATAKANA LETTER WO */
pattern XKB_KEY_kana_WO :: KeySymbol
pattern XKB_KEY_kana_WO = MkKeySymbol #{const XKB_KEY_kana_WO} 
-- #define XKB_KEY_kana_a                        0x04a7  /* U+30A1 KATAKANA LETTER SMALL A */
pattern XKB_KEY_kana_a :: KeySymbol
pattern XKB_KEY_kana_a = MkKeySymbol #{const XKB_KEY_kana_a} 
-- #define XKB_KEY_kana_i                        0x04a8  /* U+30A3 KATAKANA LETTER SMALL I */
pattern XKB_KEY_kana_i :: KeySymbol
pattern XKB_KEY_kana_i = MkKeySymbol #{const XKB_KEY_kana_i} 
-- #define XKB_KEY_kana_u                        0x04a9  /* U+30A5 KATAKANA LETTER SMALL U */
pattern XKB_KEY_kana_u :: KeySymbol
pattern XKB_KEY_kana_u = MkKeySymbol #{const XKB_KEY_kana_u} 
-- #define XKB_KEY_kana_e                        0x04aa  /* U+30A7 KATAKANA LETTER SMALL E */
pattern XKB_KEY_kana_e :: KeySymbol
pattern XKB_KEY_kana_e = MkKeySymbol #{const XKB_KEY_kana_e} 
-- #define XKB_KEY_kana_o                        0x04ab  /* U+30A9 KATAKANA LETTER SMALL O */
pattern XKB_KEY_kana_o :: KeySymbol
pattern XKB_KEY_kana_o = MkKeySymbol #{const XKB_KEY_kana_o} 
-- #define XKB_KEY_kana_ya                       0x04ac  /* U+30E3 KATAKANA LETTER SMALL YA */
pattern XKB_KEY_kana_ya :: KeySymbol
pattern XKB_KEY_kana_ya = MkKeySymbol #{const XKB_KEY_kana_ya} 
-- #define XKB_KEY_kana_yu                       0x04ad  /* U+30E5 KATAKANA LETTER SMALL YU */
pattern XKB_KEY_kana_yu :: KeySymbol
pattern XKB_KEY_kana_yu = MkKeySymbol #{const XKB_KEY_kana_yu} 
-- #define XKB_KEY_kana_yo                       0x04ae  /* U+30E7 KATAKANA LETTER SMALL YO */
pattern XKB_KEY_kana_yo :: KeySymbol
pattern XKB_KEY_kana_yo = MkKeySymbol #{const XKB_KEY_kana_yo} 
-- #define XKB_KEY_kana_tsu                      0x04af  /* U+30C3 KATAKANA LETTER SMALL TU */
pattern XKB_KEY_kana_tsu :: KeySymbol
pattern XKB_KEY_kana_tsu = MkKeySymbol #{const XKB_KEY_kana_tsu} 
-- #define XKB_KEY_kana_tu                       0x04af  /* deprecated */
pattern XKB_KEY_kana_tu :: KeySymbol
pattern XKB_KEY_kana_tu = MkKeySymbol #{const XKB_KEY_kana_tu} 
-- #define XKB_KEY_prolongedsound                0x04b0  /* U+30FC KATAKANA-HIRAGANA PROLONGED SOUND MARK */
pattern XKB_KEY_prolongedsound :: KeySymbol
pattern XKB_KEY_prolongedsound = MkKeySymbol #{const XKB_KEY_prolongedsound} 
-- #define XKB_KEY_kana_A                        0x04b1  /* U+30A2 KATAKANA LETTER A */
pattern XKB_KEY_kana_A :: KeySymbol
pattern XKB_KEY_kana_A = MkKeySymbol #{const XKB_KEY_kana_A} 
-- #define XKB_KEY_kana_I                        0x04b2  /* U+30A4 KATAKANA LETTER I */
pattern XKB_KEY_kana_I :: KeySymbol
pattern XKB_KEY_kana_I = MkKeySymbol #{const XKB_KEY_kana_I} 
-- #define XKB_KEY_kana_U                        0x04b3  /* U+30A6 KATAKANA LETTER U */
pattern XKB_KEY_kana_U :: KeySymbol
pattern XKB_KEY_kana_U = MkKeySymbol #{const XKB_KEY_kana_U} 
-- #define XKB_KEY_kana_E                        0x04b4  /* U+30A8 KATAKANA LETTER E */
pattern XKB_KEY_kana_E :: KeySymbol
pattern XKB_KEY_kana_E = MkKeySymbol #{const XKB_KEY_kana_E} 
-- #define XKB_KEY_kana_O                        0x04b5  /* U+30AA KATAKANA LETTER O */
pattern XKB_KEY_kana_O :: KeySymbol
pattern XKB_KEY_kana_O = MkKeySymbol #{const XKB_KEY_kana_O} 
-- #define XKB_KEY_kana_KA                       0x04b6  /* U+30AB KATAKANA LETTER KA */
pattern XKB_KEY_kana_KA :: KeySymbol
pattern XKB_KEY_kana_KA = MkKeySymbol #{const XKB_KEY_kana_KA} 
-- #define XKB_KEY_kana_KI                       0x04b7  /* U+30AD KATAKANA LETTER KI */
pattern XKB_KEY_kana_KI :: KeySymbol
pattern XKB_KEY_kana_KI = MkKeySymbol #{const XKB_KEY_kana_KI} 
-- #define XKB_KEY_kana_KU                       0x04b8  /* U+30AF KATAKANA LETTER KU */
pattern XKB_KEY_kana_KU :: KeySymbol
pattern XKB_KEY_kana_KU = MkKeySymbol #{const XKB_KEY_kana_KU} 
-- #define XKB_KEY_kana_KE                       0x04b9  /* U+30B1 KATAKANA LETTER KE */
pattern XKB_KEY_kana_KE :: KeySymbol
pattern XKB_KEY_kana_KE = MkKeySymbol #{const XKB_KEY_kana_KE} 
-- #define XKB_KEY_kana_KO                       0x04ba  /* U+30B3 KATAKANA LETTER KO */
pattern XKB_KEY_kana_KO :: KeySymbol
pattern XKB_KEY_kana_KO = MkKeySymbol #{const XKB_KEY_kana_KO} 
-- #define XKB_KEY_kana_SA                       0x04bb  /* U+30B5 KATAKANA LETTER SA */
pattern XKB_KEY_kana_SA :: KeySymbol
pattern XKB_KEY_kana_SA = MkKeySymbol #{const XKB_KEY_kana_SA} 
-- #define XKB_KEY_kana_SHI                      0x04bc  /* U+30B7 KATAKANA LETTER SI */
pattern XKB_KEY_kana_SHI :: KeySymbol
pattern XKB_KEY_kana_SHI = MkKeySymbol #{const XKB_KEY_kana_SHI} 
-- #define XKB_KEY_kana_SU                       0x04bd  /* U+30B9 KATAKANA LETTER SU */
pattern XKB_KEY_kana_SU :: KeySymbol
pattern XKB_KEY_kana_SU = MkKeySymbol #{const XKB_KEY_kana_SU} 
-- #define XKB_KEY_kana_SE                       0x04be  /* U+30BB KATAKANA LETTER SE */
pattern XKB_KEY_kana_SE :: KeySymbol
pattern XKB_KEY_kana_SE = MkKeySymbol #{const XKB_KEY_kana_SE} 
-- #define XKB_KEY_kana_SO                       0x04bf  /* U+30BD KATAKANA LETTER SO */
pattern XKB_KEY_kana_SO :: KeySymbol
pattern XKB_KEY_kana_SO = MkKeySymbol #{const XKB_KEY_kana_SO} 
-- #define XKB_KEY_kana_TA                       0x04c0  /* U+30BF KATAKANA LETTER TA */
pattern XKB_KEY_kana_TA :: KeySymbol
pattern XKB_KEY_kana_TA = MkKeySymbol #{const XKB_KEY_kana_TA} 
-- #define XKB_KEY_kana_CHI                      0x04c1  /* U+30C1 KATAKANA LETTER TI */
pattern XKB_KEY_kana_CHI :: KeySymbol
pattern XKB_KEY_kana_CHI = MkKeySymbol #{const XKB_KEY_kana_CHI} 
-- #define XKB_KEY_kana_TI                       0x04c1  /* deprecated */
pattern XKB_KEY_kana_TI :: KeySymbol
pattern XKB_KEY_kana_TI = MkKeySymbol #{const XKB_KEY_kana_TI} 
-- #define XKB_KEY_kana_TSU                      0x04c2  /* U+30C4 KATAKANA LETTER TU */
pattern XKB_KEY_kana_TSU :: KeySymbol
pattern XKB_KEY_kana_TSU = MkKeySymbol #{const XKB_KEY_kana_TSU} 
-- #define XKB_KEY_kana_TU                       0x04c2  /* deprecated */
pattern XKB_KEY_kana_TU :: KeySymbol
pattern XKB_KEY_kana_TU = MkKeySymbol #{const XKB_KEY_kana_TU} 
-- #define XKB_KEY_kana_TE                       0x04c3  /* U+30C6 KATAKANA LETTER TE */
pattern XKB_KEY_kana_TE :: KeySymbol
pattern XKB_KEY_kana_TE = MkKeySymbol #{const XKB_KEY_kana_TE} 
-- #define XKB_KEY_kana_TO                       0x04c4  /* U+30C8 KATAKANA LETTER TO */
pattern XKB_KEY_kana_TO :: KeySymbol
pattern XKB_KEY_kana_TO = MkKeySymbol #{const XKB_KEY_kana_TO} 
-- #define XKB_KEY_kana_NA                       0x04c5  /* U+30CA KATAKANA LETTER NA */
pattern XKB_KEY_kana_NA :: KeySymbol
pattern XKB_KEY_kana_NA = MkKeySymbol #{const XKB_KEY_kana_NA} 
-- #define XKB_KEY_kana_NI                       0x04c6  /* U+30CB KATAKANA LETTER NI */
pattern XKB_KEY_kana_NI :: KeySymbol
pattern XKB_KEY_kana_NI = MkKeySymbol #{const XKB_KEY_kana_NI} 
-- #define XKB_KEY_kana_NU                       0x04c7  /* U+30CC KATAKANA LETTER NU */
pattern XKB_KEY_kana_NU :: KeySymbol
pattern XKB_KEY_kana_NU = MkKeySymbol #{const XKB_KEY_kana_NU} 
-- #define XKB_KEY_kana_NE                       0x04c8  /* U+30CD KATAKANA LETTER NE */
pattern XKB_KEY_kana_NE :: KeySymbol
pattern XKB_KEY_kana_NE = MkKeySymbol #{const XKB_KEY_kana_NE} 
-- #define XKB_KEY_kana_NO                       0x04c9  /* U+30CE KATAKANA LETTER NO */
pattern XKB_KEY_kana_NO :: KeySymbol
pattern XKB_KEY_kana_NO = MkKeySymbol #{const XKB_KEY_kana_NO} 
-- #define XKB_KEY_kana_HA                       0x04ca  /* U+30CF KATAKANA LETTER HA */
pattern XKB_KEY_kana_HA :: KeySymbol
pattern XKB_KEY_kana_HA = MkKeySymbol #{const XKB_KEY_kana_HA} 
-- #define XKB_KEY_kana_HI                       0x04cb  /* U+30D2 KATAKANA LETTER HI */
pattern XKB_KEY_kana_HI :: KeySymbol
pattern XKB_KEY_kana_HI = MkKeySymbol #{const XKB_KEY_kana_HI} 
-- #define XKB_KEY_kana_FU                       0x04cc  /* U+30D5 KATAKANA LETTER HU */
pattern XKB_KEY_kana_FU :: KeySymbol
pattern XKB_KEY_kana_FU = MkKeySymbol #{const XKB_KEY_kana_FU} 
-- #define XKB_KEY_kana_HU                       0x04cc  /* deprecated */
pattern XKB_KEY_kana_HU :: KeySymbol
pattern XKB_KEY_kana_HU = MkKeySymbol #{const XKB_KEY_kana_HU} 
-- #define XKB_KEY_kana_HE                       0x04cd  /* U+30D8 KATAKANA LETTER HE */
pattern XKB_KEY_kana_HE :: KeySymbol
pattern XKB_KEY_kana_HE = MkKeySymbol #{const XKB_KEY_kana_HE} 
-- #define XKB_KEY_kana_HO                       0x04ce  /* U+30DB KATAKANA LETTER HO */
pattern XKB_KEY_kana_HO :: KeySymbol
pattern XKB_KEY_kana_HO = MkKeySymbol #{const XKB_KEY_kana_HO} 
-- #define XKB_KEY_kana_MA                       0x04cf  /* U+30DE KATAKANA LETTER MA */
pattern XKB_KEY_kana_MA :: KeySymbol
pattern XKB_KEY_kana_MA = MkKeySymbol #{const XKB_KEY_kana_MA} 
-- #define XKB_KEY_kana_MI                       0x04d0  /* U+30DF KATAKANA LETTER MI */
pattern XKB_KEY_kana_MI :: KeySymbol
pattern XKB_KEY_kana_MI = MkKeySymbol #{const XKB_KEY_kana_MI} 
-- #define XKB_KEY_kana_MU                       0x04d1  /* U+30E0 KATAKANA LETTER MU */
pattern XKB_KEY_kana_MU :: KeySymbol
pattern XKB_KEY_kana_MU = MkKeySymbol #{const XKB_KEY_kana_MU} 
-- #define XKB_KEY_kana_ME                       0x04d2  /* U+30E1 KATAKANA LETTER ME */
pattern XKB_KEY_kana_ME :: KeySymbol
pattern XKB_KEY_kana_ME = MkKeySymbol #{const XKB_KEY_kana_ME} 
-- #define XKB_KEY_kana_MO                       0x04d3  /* U+30E2 KATAKANA LETTER MO */
pattern XKB_KEY_kana_MO :: KeySymbol
pattern XKB_KEY_kana_MO = MkKeySymbol #{const XKB_KEY_kana_MO} 
-- #define XKB_KEY_kana_YA                       0x04d4  /* U+30E4 KATAKANA LETTER YA */
pattern XKB_KEY_kana_YA :: KeySymbol
pattern XKB_KEY_kana_YA = MkKeySymbol #{const XKB_KEY_kana_YA} 
-- #define XKB_KEY_kana_YU                       0x04d5  /* U+30E6 KATAKANA LETTER YU */
pattern XKB_KEY_kana_YU :: KeySymbol
pattern XKB_KEY_kana_YU = MkKeySymbol #{const XKB_KEY_kana_YU} 
-- #define XKB_KEY_kana_YO                       0x04d6  /* U+30E8 KATAKANA LETTER YO */
pattern XKB_KEY_kana_YO :: KeySymbol
pattern XKB_KEY_kana_YO = MkKeySymbol #{const XKB_KEY_kana_YO} 
-- #define XKB_KEY_kana_RA                       0x04d7  /* U+30E9 KATAKANA LETTER RA */
pattern XKB_KEY_kana_RA :: KeySymbol
pattern XKB_KEY_kana_RA = MkKeySymbol #{const XKB_KEY_kana_RA} 
-- #define XKB_KEY_kana_RI                       0x04d8  /* U+30EA KATAKANA LETTER RI */
pattern XKB_KEY_kana_RI :: KeySymbol
pattern XKB_KEY_kana_RI = MkKeySymbol #{const XKB_KEY_kana_RI} 
-- #define XKB_KEY_kana_RU                       0x04d9  /* U+30EB KATAKANA LETTER RU */
pattern XKB_KEY_kana_RU :: KeySymbol
pattern XKB_KEY_kana_RU = MkKeySymbol #{const XKB_KEY_kana_RU} 
-- #define XKB_KEY_kana_RE                       0x04da  /* U+30EC KATAKANA LETTER RE */
pattern XKB_KEY_kana_RE :: KeySymbol
pattern XKB_KEY_kana_RE = MkKeySymbol #{const XKB_KEY_kana_RE} 
-- #define XKB_KEY_kana_RO                       0x04db  /* U+30ED KATAKANA LETTER RO */
pattern XKB_KEY_kana_RO :: KeySymbol
pattern XKB_KEY_kana_RO = MkKeySymbol #{const XKB_KEY_kana_RO} 
-- #define XKB_KEY_kana_WA                       0x04dc  /* U+30EF KATAKANA LETTER WA */
pattern XKB_KEY_kana_WA :: KeySymbol
pattern XKB_KEY_kana_WA = MkKeySymbol #{const XKB_KEY_kana_WA} 
-- #define XKB_KEY_kana_N                        0x04dd  /* U+30F3 KATAKANA LETTER N */
pattern XKB_KEY_kana_N :: KeySymbol
pattern XKB_KEY_kana_N = MkKeySymbol #{const XKB_KEY_kana_N} 
-- #define XKB_KEY_voicedsound                   0x04de  /* U+309B KATAKANA-HIRAGANA VOICED SOUND MARK */
pattern XKB_KEY_voicedsound :: KeySymbol
pattern XKB_KEY_voicedsound = MkKeySymbol #{const XKB_KEY_voicedsound} 
-- #define XKB_KEY_semivoicedsound               0x04df  /* U+309C KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK */
pattern XKB_KEY_semivoicedsound :: KeySymbol
pattern XKB_KEY_semivoicedsound = MkKeySymbol #{const XKB_KEY_semivoicedsound} 
-- #define XKB_KEY_kana_switch                   0xff7e  /* Alias for mode_switch */
pattern XKB_KEY_kana_switch :: KeySymbol
pattern XKB_KEY_kana_switch = MkKeySymbol #{const XKB_KEY_kana_switch} 
-- #define XKB_KEY_Farsi_0                    0x10006f0  /* U+06F0 EXTENDED ARABIC-INDIC DIGIT ZERO */
pattern XKB_KEY_Farsi_0 :: KeySymbol
pattern XKB_KEY_Farsi_0 = MkKeySymbol #{const XKB_KEY_Farsi_0} 
-- #define XKB_KEY_Farsi_1                    0x10006f1  /* U+06F1 EXTENDED ARABIC-INDIC DIGIT ONE */
pattern XKB_KEY_Farsi_1 :: KeySymbol
pattern XKB_KEY_Farsi_1 = MkKeySymbol #{const XKB_KEY_Farsi_1} 
-- #define XKB_KEY_Farsi_2                    0x10006f2  /* U+06F2 EXTENDED ARABIC-INDIC DIGIT TWO */
pattern XKB_KEY_Farsi_2 :: KeySymbol
pattern XKB_KEY_Farsi_2 = MkKeySymbol #{const XKB_KEY_Farsi_2} 
-- #define XKB_KEY_Farsi_3                    0x10006f3  /* U+06F3 EXTENDED ARABIC-INDIC DIGIT THREE */
pattern XKB_KEY_Farsi_3 :: KeySymbol
pattern XKB_KEY_Farsi_3 = MkKeySymbol #{const XKB_KEY_Farsi_3} 
-- #define XKB_KEY_Farsi_4                    0x10006f4  /* U+06F4 EXTENDED ARABIC-INDIC DIGIT FOUR */
pattern XKB_KEY_Farsi_4 :: KeySymbol
pattern XKB_KEY_Farsi_4 = MkKeySymbol #{const XKB_KEY_Farsi_4} 
-- #define XKB_KEY_Farsi_5                    0x10006f5  /* U+06F5 EXTENDED ARABIC-INDIC DIGIT FIVE */
pattern XKB_KEY_Farsi_5 :: KeySymbol
pattern XKB_KEY_Farsi_5 = MkKeySymbol #{const XKB_KEY_Farsi_5} 
-- #define XKB_KEY_Farsi_6                    0x10006f6  /* U+06F6 EXTENDED ARABIC-INDIC DIGIT SIX */
pattern XKB_KEY_Farsi_6 :: KeySymbol
pattern XKB_KEY_Farsi_6 = MkKeySymbol #{const XKB_KEY_Farsi_6} 
-- #define XKB_KEY_Farsi_7                    0x10006f7  /* U+06F7 EXTENDED ARABIC-INDIC DIGIT SEVEN */
pattern XKB_KEY_Farsi_7 :: KeySymbol
pattern XKB_KEY_Farsi_7 = MkKeySymbol #{const XKB_KEY_Farsi_7} 
-- #define XKB_KEY_Farsi_8                    0x10006f8  /* U+06F8 EXTENDED ARABIC-INDIC DIGIT EIGHT */
pattern XKB_KEY_Farsi_8 :: KeySymbol
pattern XKB_KEY_Farsi_8 = MkKeySymbol #{const XKB_KEY_Farsi_8} 
-- #define XKB_KEY_Farsi_9                    0x10006f9  /* U+06F9 EXTENDED ARABIC-INDIC DIGIT NINE */
pattern XKB_KEY_Farsi_9 :: KeySymbol
pattern XKB_KEY_Farsi_9 = MkKeySymbol #{const XKB_KEY_Farsi_9} 
-- #define XKB_KEY_Arabic_percent             0x100066a  /* U+066A ARABIC PERCENT SIGN */
pattern XKB_KEY_Arabic_percent :: KeySymbol
pattern XKB_KEY_Arabic_percent = MkKeySymbol #{const XKB_KEY_Arabic_percent} 
-- #define XKB_KEY_Arabic_superscript_alef    0x1000670  /* U+0670 ARABIC LETTER SUPERSCRIPT ALEF */
pattern XKB_KEY_Arabic_superscript_alef :: KeySymbol
pattern XKB_KEY_Arabic_superscript_alef = MkKeySymbol #{const XKB_KEY_Arabic_superscript_alef} 
-- #define XKB_KEY_Arabic_tteh                0x1000679  /* U+0679 ARABIC LETTER TTEH */
pattern XKB_KEY_Arabic_tteh :: KeySymbol
pattern XKB_KEY_Arabic_tteh = MkKeySymbol #{const XKB_KEY_Arabic_tteh} 
-- #define XKB_KEY_Arabic_peh                 0x100067e  /* U+067E ARABIC LETTER PEH */
pattern XKB_KEY_Arabic_peh :: KeySymbol
pattern XKB_KEY_Arabic_peh = MkKeySymbol #{const XKB_KEY_Arabic_peh} 
-- #define XKB_KEY_Arabic_tcheh               0x1000686  /* U+0686 ARABIC LETTER TCHEH */
pattern XKB_KEY_Arabic_tcheh :: KeySymbol
pattern XKB_KEY_Arabic_tcheh = MkKeySymbol #{const XKB_KEY_Arabic_tcheh} 
-- #define XKB_KEY_Arabic_ddal                0x1000688  /* U+0688 ARABIC LETTER DDAL */
pattern XKB_KEY_Arabic_ddal :: KeySymbol
pattern XKB_KEY_Arabic_ddal = MkKeySymbol #{const XKB_KEY_Arabic_ddal} 
-- #define XKB_KEY_Arabic_rreh                0x1000691  /* U+0691 ARABIC LETTER RREH */
pattern XKB_KEY_Arabic_rreh :: KeySymbol
pattern XKB_KEY_Arabic_rreh = MkKeySymbol #{const XKB_KEY_Arabic_rreh} 
-- #define XKB_KEY_Arabic_comma                  0x05ac  /* U+060C ARABIC COMMA */
pattern XKB_KEY_Arabic_comma :: KeySymbol
pattern XKB_KEY_Arabic_comma = MkKeySymbol #{const XKB_KEY_Arabic_comma} 
-- #define XKB_KEY_Arabic_fullstop            0x10006d4  /* U+06D4 ARABIC FULL STOP */
pattern XKB_KEY_Arabic_fullstop :: KeySymbol
pattern XKB_KEY_Arabic_fullstop = MkKeySymbol #{const XKB_KEY_Arabic_fullstop} 
-- #define XKB_KEY_Arabic_0                   0x1000660  /* U+0660 ARABIC-INDIC DIGIT ZERO */
pattern XKB_KEY_Arabic_0 :: KeySymbol
pattern XKB_KEY_Arabic_0 = MkKeySymbol #{const XKB_KEY_Arabic_0} 
-- #define XKB_KEY_Arabic_1                   0x1000661  /* U+0661 ARABIC-INDIC DIGIT ONE */
pattern XKB_KEY_Arabic_1 :: KeySymbol
pattern XKB_KEY_Arabic_1 = MkKeySymbol #{const XKB_KEY_Arabic_1} 
-- #define XKB_KEY_Arabic_2                   0x1000662  /* U+0662 ARABIC-INDIC DIGIT TWO */
pattern XKB_KEY_Arabic_2 :: KeySymbol
pattern XKB_KEY_Arabic_2 = MkKeySymbol #{const XKB_KEY_Arabic_2} 
-- #define XKB_KEY_Arabic_3                   0x1000663  /* U+0663 ARABIC-INDIC DIGIT THREE */
pattern XKB_KEY_Arabic_3 :: KeySymbol
pattern XKB_KEY_Arabic_3 = MkKeySymbol #{const XKB_KEY_Arabic_3} 
-- #define XKB_KEY_Arabic_4                   0x1000664  /* U+0664 ARABIC-INDIC DIGIT FOUR */
pattern XKB_KEY_Arabic_4 :: KeySymbol
pattern XKB_KEY_Arabic_4 = MkKeySymbol #{const XKB_KEY_Arabic_4} 
-- #define XKB_KEY_Arabic_5                   0x1000665  /* U+0665 ARABIC-INDIC DIGIT FIVE */
pattern XKB_KEY_Arabic_5 :: KeySymbol
pattern XKB_KEY_Arabic_5 = MkKeySymbol #{const XKB_KEY_Arabic_5} 
-- #define XKB_KEY_Arabic_6                   0x1000666  /* U+0666 ARABIC-INDIC DIGIT SIX */
pattern XKB_KEY_Arabic_6 :: KeySymbol
pattern XKB_KEY_Arabic_6 = MkKeySymbol #{const XKB_KEY_Arabic_6} 
-- #define XKB_KEY_Arabic_7                   0x1000667  /* U+0667 ARABIC-INDIC DIGIT SEVEN */
pattern XKB_KEY_Arabic_7 :: KeySymbol
pattern XKB_KEY_Arabic_7 = MkKeySymbol #{const XKB_KEY_Arabic_7} 
-- #define XKB_KEY_Arabic_8                   0x1000668  /* U+0668 ARABIC-INDIC DIGIT EIGHT */
pattern XKB_KEY_Arabic_8 :: KeySymbol
pattern XKB_KEY_Arabic_8 = MkKeySymbol #{const XKB_KEY_Arabic_8} 
-- #define XKB_KEY_Arabic_9                   0x1000669  /* U+0669 ARABIC-INDIC DIGIT NINE */
pattern XKB_KEY_Arabic_9 :: KeySymbol
pattern XKB_KEY_Arabic_9 = MkKeySymbol #{const XKB_KEY_Arabic_9} 
-- #define XKB_KEY_Arabic_semicolon              0x05bb  /* U+061B ARABIC SEMICOLON */
pattern XKB_KEY_Arabic_semicolon :: KeySymbol
pattern XKB_KEY_Arabic_semicolon = MkKeySymbol #{const XKB_KEY_Arabic_semicolon} 
-- #define XKB_KEY_Arabic_question_mark          0x05bf  /* U+061F ARABIC QUESTION MARK */
pattern XKB_KEY_Arabic_question_mark :: KeySymbol
pattern XKB_KEY_Arabic_question_mark = MkKeySymbol #{const XKB_KEY_Arabic_question_mark} 
-- #define XKB_KEY_Arabic_hamza                  0x05c1  /* U+0621 ARABIC LETTER HAMZA */
pattern XKB_KEY_Arabic_hamza :: KeySymbol
pattern XKB_KEY_Arabic_hamza = MkKeySymbol #{const XKB_KEY_Arabic_hamza} 
-- #define XKB_KEY_Arabic_maddaonalef            0x05c2  /* U+0622 ARABIC LETTER ALEF WITH MADDA ABOVE */
pattern XKB_KEY_Arabic_maddaonalef :: KeySymbol
pattern XKB_KEY_Arabic_maddaonalef = MkKeySymbol #{const XKB_KEY_Arabic_maddaonalef} 
-- #define XKB_KEY_Arabic_hamzaonalef            0x05c3  /* U+0623 ARABIC LETTER ALEF WITH HAMZA ABOVE */
pattern XKB_KEY_Arabic_hamzaonalef :: KeySymbol
pattern XKB_KEY_Arabic_hamzaonalef = MkKeySymbol #{const XKB_KEY_Arabic_hamzaonalef} 
-- #define XKB_KEY_Arabic_hamzaonwaw             0x05c4  /* U+0624 ARABIC LETTER WAW WITH HAMZA ABOVE */
pattern XKB_KEY_Arabic_hamzaonwaw :: KeySymbol
pattern XKB_KEY_Arabic_hamzaonwaw = MkKeySymbol #{const XKB_KEY_Arabic_hamzaonwaw} 
-- #define XKB_KEY_Arabic_hamzaunderalef         0x05c5  /* U+0625 ARABIC LETTER ALEF WITH HAMZA BELOW */
pattern XKB_KEY_Arabic_hamzaunderalef :: KeySymbol
pattern XKB_KEY_Arabic_hamzaunderalef = MkKeySymbol #{const XKB_KEY_Arabic_hamzaunderalef} 
-- #define XKB_KEY_Arabic_hamzaonyeh             0x05c6  /* U+0626 ARABIC LETTER YEH WITH HAMZA ABOVE */
pattern XKB_KEY_Arabic_hamzaonyeh :: KeySymbol
pattern XKB_KEY_Arabic_hamzaonyeh = MkKeySymbol #{const XKB_KEY_Arabic_hamzaonyeh} 
-- #define XKB_KEY_Arabic_alef                   0x05c7  /* U+0627 ARABIC LETTER ALEF */
pattern XKB_KEY_Arabic_alef :: KeySymbol
pattern XKB_KEY_Arabic_alef = MkKeySymbol #{const XKB_KEY_Arabic_alef} 
-- #define XKB_KEY_Arabic_beh                    0x05c8  /* U+0628 ARABIC LETTER BEH */
pattern XKB_KEY_Arabic_beh :: KeySymbol
pattern XKB_KEY_Arabic_beh = MkKeySymbol #{const XKB_KEY_Arabic_beh} 
-- #define XKB_KEY_Arabic_tehmarbuta             0x05c9  /* U+0629 ARABIC LETTER TEH MARBUTA */
pattern XKB_KEY_Arabic_tehmarbuta :: KeySymbol
pattern XKB_KEY_Arabic_tehmarbuta = MkKeySymbol #{const XKB_KEY_Arabic_tehmarbuta} 
-- #define XKB_KEY_Arabic_teh                    0x05ca  /* U+062A ARABIC LETTER TEH */
pattern XKB_KEY_Arabic_teh :: KeySymbol
pattern XKB_KEY_Arabic_teh = MkKeySymbol #{const XKB_KEY_Arabic_teh} 
-- #define XKB_KEY_Arabic_theh                   0x05cb  /* U+062B ARABIC LETTER THEH */
pattern XKB_KEY_Arabic_theh :: KeySymbol
pattern XKB_KEY_Arabic_theh = MkKeySymbol #{const XKB_KEY_Arabic_theh} 
-- #define XKB_KEY_Arabic_jeem                   0x05cc  /* U+062C ARABIC LETTER JEEM */
pattern XKB_KEY_Arabic_jeem :: KeySymbol
pattern XKB_KEY_Arabic_jeem = MkKeySymbol #{const XKB_KEY_Arabic_jeem} 
-- #define XKB_KEY_Arabic_hah                    0x05cd  /* U+062D ARABIC LETTER HAH */
pattern XKB_KEY_Arabic_hah :: KeySymbol
pattern XKB_KEY_Arabic_hah = MkKeySymbol #{const XKB_KEY_Arabic_hah} 
-- #define XKB_KEY_Arabic_khah                   0x05ce  /* U+062E ARABIC LETTER KHAH */
pattern XKB_KEY_Arabic_khah :: KeySymbol
pattern XKB_KEY_Arabic_khah = MkKeySymbol #{const XKB_KEY_Arabic_khah} 
-- #define XKB_KEY_Arabic_dal                    0x05cf  /* U+062F ARABIC LETTER DAL */
pattern XKB_KEY_Arabic_dal :: KeySymbol
pattern XKB_KEY_Arabic_dal = MkKeySymbol #{const XKB_KEY_Arabic_dal} 
-- #define XKB_KEY_Arabic_thal                   0x05d0  /* U+0630 ARABIC LETTER THAL */
pattern XKB_KEY_Arabic_thal :: KeySymbol
pattern XKB_KEY_Arabic_thal = MkKeySymbol #{const XKB_KEY_Arabic_thal} 
-- #define XKB_KEY_Arabic_ra                     0x05d1  /* U+0631 ARABIC LETTER REH */
pattern XKB_KEY_Arabic_ra :: KeySymbol
pattern XKB_KEY_Arabic_ra = MkKeySymbol #{const XKB_KEY_Arabic_ra} 
-- #define XKB_KEY_Arabic_zain                   0x05d2  /* U+0632 ARABIC LETTER ZAIN */
pattern XKB_KEY_Arabic_zain :: KeySymbol
pattern XKB_KEY_Arabic_zain = MkKeySymbol #{const XKB_KEY_Arabic_zain} 
-- #define XKB_KEY_Arabic_seen                   0x05d3  /* U+0633 ARABIC LETTER SEEN */
pattern XKB_KEY_Arabic_seen :: KeySymbol
pattern XKB_KEY_Arabic_seen = MkKeySymbol #{const XKB_KEY_Arabic_seen} 
-- #define XKB_KEY_Arabic_sheen                  0x05d4  /* U+0634 ARABIC LETTER SHEEN */
pattern XKB_KEY_Arabic_sheen :: KeySymbol
pattern XKB_KEY_Arabic_sheen = MkKeySymbol #{const XKB_KEY_Arabic_sheen} 
-- #define XKB_KEY_Arabic_sad                    0x05d5  /* U+0635 ARABIC LETTER SAD */
pattern XKB_KEY_Arabic_sad :: KeySymbol
pattern XKB_KEY_Arabic_sad = MkKeySymbol #{const XKB_KEY_Arabic_sad} 
-- #define XKB_KEY_Arabic_dad                    0x05d6  /* U+0636 ARABIC LETTER DAD */
pattern XKB_KEY_Arabic_dad :: KeySymbol
pattern XKB_KEY_Arabic_dad = MkKeySymbol #{const XKB_KEY_Arabic_dad} 
-- #define XKB_KEY_Arabic_tah                    0x05d7  /* U+0637 ARABIC LETTER TAH */
pattern XKB_KEY_Arabic_tah :: KeySymbol
pattern XKB_KEY_Arabic_tah = MkKeySymbol #{const XKB_KEY_Arabic_tah} 
-- #define XKB_KEY_Arabic_zah                    0x05d8  /* U+0638 ARABIC LETTER ZAH */
pattern XKB_KEY_Arabic_zah :: KeySymbol
pattern XKB_KEY_Arabic_zah = MkKeySymbol #{const XKB_KEY_Arabic_zah} 
-- #define XKB_KEY_Arabic_ain                    0x05d9  /* U+0639 ARABIC LETTER AIN */
pattern XKB_KEY_Arabic_ain :: KeySymbol
pattern XKB_KEY_Arabic_ain = MkKeySymbol #{const XKB_KEY_Arabic_ain} 
-- #define XKB_KEY_Arabic_ghain                  0x05da  /* U+063A ARABIC LETTER GHAIN */
pattern XKB_KEY_Arabic_ghain :: KeySymbol
pattern XKB_KEY_Arabic_ghain = MkKeySymbol #{const XKB_KEY_Arabic_ghain} 
-- #define XKB_KEY_Arabic_tatweel                0x05e0  /* U+0640 ARABIC TATWEEL */
pattern XKB_KEY_Arabic_tatweel :: KeySymbol
pattern XKB_KEY_Arabic_tatweel = MkKeySymbol #{const XKB_KEY_Arabic_tatweel} 
-- #define XKB_KEY_Arabic_feh                    0x05e1  /* U+0641 ARABIC LETTER FEH */
pattern XKB_KEY_Arabic_feh :: KeySymbol
pattern XKB_KEY_Arabic_feh = MkKeySymbol #{const XKB_KEY_Arabic_feh} 
-- #define XKB_KEY_Arabic_qaf                    0x05e2  /* U+0642 ARABIC LETTER QAF */
pattern XKB_KEY_Arabic_qaf :: KeySymbol
pattern XKB_KEY_Arabic_qaf = MkKeySymbol #{const XKB_KEY_Arabic_qaf} 
-- #define XKB_KEY_Arabic_kaf                    0x05e3  /* U+0643 ARABIC LETTER KAF */
pattern XKB_KEY_Arabic_kaf :: KeySymbol
pattern XKB_KEY_Arabic_kaf = MkKeySymbol #{const XKB_KEY_Arabic_kaf} 
-- #define XKB_KEY_Arabic_lam                    0x05e4  /* U+0644 ARABIC LETTER LAM */
pattern XKB_KEY_Arabic_lam :: KeySymbol
pattern XKB_KEY_Arabic_lam = MkKeySymbol #{const XKB_KEY_Arabic_lam} 
-- #define XKB_KEY_Arabic_meem                   0x05e5  /* U+0645 ARABIC LETTER MEEM */
pattern XKB_KEY_Arabic_meem :: KeySymbol
pattern XKB_KEY_Arabic_meem = MkKeySymbol #{const XKB_KEY_Arabic_meem} 
-- #define XKB_KEY_Arabic_noon                   0x05e6  /* U+0646 ARABIC LETTER NOON */
pattern XKB_KEY_Arabic_noon :: KeySymbol
pattern XKB_KEY_Arabic_noon = MkKeySymbol #{const XKB_KEY_Arabic_noon} 
-- #define XKB_KEY_Arabic_ha                     0x05e7  /* U+0647 ARABIC LETTER HEH */
pattern XKB_KEY_Arabic_ha :: KeySymbol
pattern XKB_KEY_Arabic_ha = MkKeySymbol #{const XKB_KEY_Arabic_ha} 
-- #define XKB_KEY_Arabic_heh                    0x05e7  /* deprecated */
pattern XKB_KEY_Arabic_heh :: KeySymbol
pattern XKB_KEY_Arabic_heh = MkKeySymbol #{const XKB_KEY_Arabic_heh} 
-- #define XKB_KEY_Arabic_waw                    0x05e8  /* U+0648 ARABIC LETTER WAW */
pattern XKB_KEY_Arabic_waw :: KeySymbol
pattern XKB_KEY_Arabic_waw = MkKeySymbol #{const XKB_KEY_Arabic_waw} 
-- #define XKB_KEY_Arabic_alefmaksura            0x05e9  /* U+0649 ARABIC LETTER ALEF MAKSURA */
pattern XKB_KEY_Arabic_alefmaksura :: KeySymbol
pattern XKB_KEY_Arabic_alefmaksura = MkKeySymbol #{const XKB_KEY_Arabic_alefmaksura} 
-- #define XKB_KEY_Arabic_yeh                    0x05ea  /* U+064A ARABIC LETTER YEH */
pattern XKB_KEY_Arabic_yeh :: KeySymbol
pattern XKB_KEY_Arabic_yeh = MkKeySymbol #{const XKB_KEY_Arabic_yeh} 
-- #define XKB_KEY_Arabic_fathatan               0x05eb  /* U+064B ARABIC FATHATAN */
pattern XKB_KEY_Arabic_fathatan :: KeySymbol
pattern XKB_KEY_Arabic_fathatan = MkKeySymbol #{const XKB_KEY_Arabic_fathatan} 
-- #define XKB_KEY_Arabic_dammatan               0x05ec  /* U+064C ARABIC DAMMATAN */
pattern XKB_KEY_Arabic_dammatan :: KeySymbol
pattern XKB_KEY_Arabic_dammatan = MkKeySymbol #{const XKB_KEY_Arabic_dammatan} 
-- #define XKB_KEY_Arabic_kasratan               0x05ed  /* U+064D ARABIC KASRATAN */
pattern XKB_KEY_Arabic_kasratan :: KeySymbol
pattern XKB_KEY_Arabic_kasratan = MkKeySymbol #{const XKB_KEY_Arabic_kasratan} 
-- #define XKB_KEY_Arabic_fatha                  0x05ee  /* U+064E ARABIC FATHA */
pattern XKB_KEY_Arabic_fatha :: KeySymbol
pattern XKB_KEY_Arabic_fatha = MkKeySymbol #{const XKB_KEY_Arabic_fatha} 
-- #define XKB_KEY_Arabic_damma                  0x05ef  /* U+064F ARABIC DAMMA */
pattern XKB_KEY_Arabic_damma :: KeySymbol
pattern XKB_KEY_Arabic_damma = MkKeySymbol #{const XKB_KEY_Arabic_damma} 
-- #define XKB_KEY_Arabic_kasra                  0x05f0  /* U+0650 ARABIC KASRA */
pattern XKB_KEY_Arabic_kasra :: KeySymbol
pattern XKB_KEY_Arabic_kasra = MkKeySymbol #{const XKB_KEY_Arabic_kasra} 
-- #define XKB_KEY_Arabic_shadda                 0x05f1  /* U+0651 ARABIC SHADDA */
pattern XKB_KEY_Arabic_shadda :: KeySymbol
pattern XKB_KEY_Arabic_shadda = MkKeySymbol #{const XKB_KEY_Arabic_shadda} 
-- #define XKB_KEY_Arabic_sukun                  0x05f2  /* U+0652 ARABIC SUKUN */
pattern XKB_KEY_Arabic_sukun :: KeySymbol
pattern XKB_KEY_Arabic_sukun = MkKeySymbol #{const XKB_KEY_Arabic_sukun} 
-- #define XKB_KEY_Arabic_madda_above         0x1000653  /* U+0653 ARABIC MADDAH ABOVE */
pattern XKB_KEY_Arabic_madda_above :: KeySymbol
pattern XKB_KEY_Arabic_madda_above = MkKeySymbol #{const XKB_KEY_Arabic_madda_above} 
-- #define XKB_KEY_Arabic_hamza_above         0x1000654  /* U+0654 ARABIC HAMZA ABOVE */
pattern XKB_KEY_Arabic_hamza_above :: KeySymbol
pattern XKB_KEY_Arabic_hamza_above = MkKeySymbol #{const XKB_KEY_Arabic_hamza_above} 
-- #define XKB_KEY_Arabic_hamza_below         0x1000655  /* U+0655 ARABIC HAMZA BELOW */
pattern XKB_KEY_Arabic_hamza_below :: KeySymbol
pattern XKB_KEY_Arabic_hamza_below = MkKeySymbol #{const XKB_KEY_Arabic_hamza_below} 
-- #define XKB_KEY_Arabic_jeh                 0x1000698  /* U+0698 ARABIC LETTER JEH */
pattern XKB_KEY_Arabic_jeh :: KeySymbol
pattern XKB_KEY_Arabic_jeh = MkKeySymbol #{const XKB_KEY_Arabic_jeh} 
-- #define XKB_KEY_Arabic_veh                 0x10006a4  /* U+06A4 ARABIC LETTER VEH */
pattern XKB_KEY_Arabic_veh :: KeySymbol
pattern XKB_KEY_Arabic_veh = MkKeySymbol #{const XKB_KEY_Arabic_veh} 
-- #define XKB_KEY_Arabic_keheh               0x10006a9  /* U+06A9 ARABIC LETTER KEHEH */
pattern XKB_KEY_Arabic_keheh :: KeySymbol
pattern XKB_KEY_Arabic_keheh = MkKeySymbol #{const XKB_KEY_Arabic_keheh} 
-- #define XKB_KEY_Arabic_gaf                 0x10006af  /* U+06AF ARABIC LETTER GAF */
pattern XKB_KEY_Arabic_gaf :: KeySymbol
pattern XKB_KEY_Arabic_gaf = MkKeySymbol #{const XKB_KEY_Arabic_gaf} 
-- #define XKB_KEY_Arabic_noon_ghunna         0x10006ba  /* U+06BA ARABIC LETTER NOON GHUNNA */
pattern XKB_KEY_Arabic_noon_ghunna :: KeySymbol
pattern XKB_KEY_Arabic_noon_ghunna = MkKeySymbol #{const XKB_KEY_Arabic_noon_ghunna} 
-- #define XKB_KEY_Arabic_heh_doachashmee     0x10006be  /* U+06BE ARABIC LETTER HEH DOACHASHMEE */
pattern XKB_KEY_Arabic_heh_doachashmee :: KeySymbol
pattern XKB_KEY_Arabic_heh_doachashmee = MkKeySymbol #{const XKB_KEY_Arabic_heh_doachashmee} 
-- #define XKB_KEY_Farsi_yeh                  0x10006cc  /* U+06CC ARABIC LETTER FARSI YEH */
pattern XKB_KEY_Farsi_yeh :: KeySymbol
pattern XKB_KEY_Farsi_yeh = MkKeySymbol #{const XKB_KEY_Farsi_yeh} 
-- #define XKB_KEY_Arabic_farsi_yeh           0x10006cc  /* U+06CC ARABIC LETTER FARSI YEH */
pattern XKB_KEY_Arabic_farsi_yeh :: KeySymbol
pattern XKB_KEY_Arabic_farsi_yeh = MkKeySymbol #{const XKB_KEY_Arabic_farsi_yeh} 
-- #define XKB_KEY_Arabic_yeh_baree           0x10006d2  /* U+06D2 ARABIC LETTER YEH BARREE */
pattern XKB_KEY_Arabic_yeh_baree :: KeySymbol
pattern XKB_KEY_Arabic_yeh_baree = MkKeySymbol #{const XKB_KEY_Arabic_yeh_baree} 
-- #define XKB_KEY_Arabic_heh_goal            0x10006c1  /* U+06C1 ARABIC LETTER HEH GOAL */
pattern XKB_KEY_Arabic_heh_goal :: KeySymbol
pattern XKB_KEY_Arabic_heh_goal = MkKeySymbol #{const XKB_KEY_Arabic_heh_goal} 
-- #define XKB_KEY_Arabic_switch                 0xff7e  /* Alias for mode_switch */
pattern XKB_KEY_Arabic_switch :: KeySymbol
pattern XKB_KEY_Arabic_switch = MkKeySymbol #{const XKB_KEY_Arabic_switch} 
-- #define XKB_KEY_Cyrillic_GHE_bar           0x1000492  /* U+0492 CYRILLIC CAPITAL LETTER GHE WITH STROKE */
pattern XKB_KEY_Cyrillic_GHE_bar :: KeySymbol
pattern XKB_KEY_Cyrillic_GHE_bar = MkKeySymbol #{const XKB_KEY_Cyrillic_GHE_bar} 
-- #define XKB_KEY_Cyrillic_ghe_bar           0x1000493  /* U+0493 CYRILLIC SMALL LETTER GHE WITH STROKE */
pattern XKB_KEY_Cyrillic_ghe_bar :: KeySymbol
pattern XKB_KEY_Cyrillic_ghe_bar = MkKeySymbol #{const XKB_KEY_Cyrillic_ghe_bar} 
-- #define XKB_KEY_Cyrillic_ZHE_descender     0x1000496  /* U+0496 CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER */
pattern XKB_KEY_Cyrillic_ZHE_descender :: KeySymbol
pattern XKB_KEY_Cyrillic_ZHE_descender = MkKeySymbol #{const XKB_KEY_Cyrillic_ZHE_descender} 
-- #define XKB_KEY_Cyrillic_zhe_descender     0x1000497  /* U+0497 CYRILLIC SMALL LETTER ZHE WITH DESCENDER */
pattern XKB_KEY_Cyrillic_zhe_descender :: KeySymbol
pattern XKB_KEY_Cyrillic_zhe_descender = MkKeySymbol #{const XKB_KEY_Cyrillic_zhe_descender} 
-- #define XKB_KEY_Cyrillic_KA_descender      0x100049a  /* U+049A CYRILLIC CAPITAL LETTER KA WITH DESCENDER */
pattern XKB_KEY_Cyrillic_KA_descender :: KeySymbol
pattern XKB_KEY_Cyrillic_KA_descender = MkKeySymbol #{const XKB_KEY_Cyrillic_KA_descender} 
-- #define XKB_KEY_Cyrillic_ka_descender      0x100049b  /* U+049B CYRILLIC SMALL LETTER KA WITH DESCENDER */
pattern XKB_KEY_Cyrillic_ka_descender :: KeySymbol
pattern XKB_KEY_Cyrillic_ka_descender = MkKeySymbol #{const XKB_KEY_Cyrillic_ka_descender} 
-- #define XKB_KEY_Cyrillic_KA_vertstroke     0x100049c  /* U+049C CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE */
pattern XKB_KEY_Cyrillic_KA_vertstroke :: KeySymbol
pattern XKB_KEY_Cyrillic_KA_vertstroke = MkKeySymbol #{const XKB_KEY_Cyrillic_KA_vertstroke} 
-- #define XKB_KEY_Cyrillic_ka_vertstroke     0x100049d  /* U+049D CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE */
pattern XKB_KEY_Cyrillic_ka_vertstroke :: KeySymbol
pattern XKB_KEY_Cyrillic_ka_vertstroke = MkKeySymbol #{const XKB_KEY_Cyrillic_ka_vertstroke} 
-- #define XKB_KEY_Cyrillic_EN_descender      0x10004a2  /* U+04A2 CYRILLIC CAPITAL LETTER EN WITH DESCENDER */
pattern XKB_KEY_Cyrillic_EN_descender :: KeySymbol
pattern XKB_KEY_Cyrillic_EN_descender = MkKeySymbol #{const XKB_KEY_Cyrillic_EN_descender} 
-- #define XKB_KEY_Cyrillic_en_descender      0x10004a3  /* U+04A3 CYRILLIC SMALL LETTER EN WITH DESCENDER */
pattern XKB_KEY_Cyrillic_en_descender :: KeySymbol
pattern XKB_KEY_Cyrillic_en_descender = MkKeySymbol #{const XKB_KEY_Cyrillic_en_descender} 
-- #define XKB_KEY_Cyrillic_U_straight        0x10004ae  /* U+04AE CYRILLIC CAPITAL LETTER STRAIGHT U */
pattern XKB_KEY_Cyrillic_U_straight :: KeySymbol
pattern XKB_KEY_Cyrillic_U_straight = MkKeySymbol #{const XKB_KEY_Cyrillic_U_straight} 
-- #define XKB_KEY_Cyrillic_u_straight        0x10004af  /* U+04AF CYRILLIC SMALL LETTER STRAIGHT U */
pattern XKB_KEY_Cyrillic_u_straight :: KeySymbol
pattern XKB_KEY_Cyrillic_u_straight = MkKeySymbol #{const XKB_KEY_Cyrillic_u_straight} 
-- #define XKB_KEY_Cyrillic_U_straight_bar    0x10004b0  /* U+04B0 CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE */
pattern XKB_KEY_Cyrillic_U_straight_bar :: KeySymbol
pattern XKB_KEY_Cyrillic_U_straight_bar = MkKeySymbol #{const XKB_KEY_Cyrillic_U_straight_bar} 
-- #define XKB_KEY_Cyrillic_u_straight_bar    0x10004b1  /* U+04B1 CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE */
pattern XKB_KEY_Cyrillic_u_straight_bar :: KeySymbol
pattern XKB_KEY_Cyrillic_u_straight_bar = MkKeySymbol #{const XKB_KEY_Cyrillic_u_straight_bar} 
-- #define XKB_KEY_Cyrillic_HA_descender      0x10004b2  /* U+04B2 CYRILLIC CAPITAL LETTER HA WITH DESCENDER */
pattern XKB_KEY_Cyrillic_HA_descender :: KeySymbol
pattern XKB_KEY_Cyrillic_HA_descender = MkKeySymbol #{const XKB_KEY_Cyrillic_HA_descender} 
-- #define XKB_KEY_Cyrillic_ha_descender      0x10004b3  /* U+04B3 CYRILLIC SMALL LETTER HA WITH DESCENDER */
pattern XKB_KEY_Cyrillic_ha_descender :: KeySymbol
pattern XKB_KEY_Cyrillic_ha_descender = MkKeySymbol #{const XKB_KEY_Cyrillic_ha_descender} 
-- #define XKB_KEY_Cyrillic_CHE_descender     0x10004b6  /* U+04B6 CYRILLIC CAPITAL LETTER CHE WITH DESCENDER */
pattern XKB_KEY_Cyrillic_CHE_descender :: KeySymbol
pattern XKB_KEY_Cyrillic_CHE_descender = MkKeySymbol #{const XKB_KEY_Cyrillic_CHE_descender} 
-- #define XKB_KEY_Cyrillic_che_descender     0x10004b7  /* U+04B7 CYRILLIC SMALL LETTER CHE WITH DESCENDER */
pattern XKB_KEY_Cyrillic_che_descender :: KeySymbol
pattern XKB_KEY_Cyrillic_che_descender = MkKeySymbol #{const XKB_KEY_Cyrillic_che_descender} 
-- #define XKB_KEY_Cyrillic_CHE_vertstroke    0x10004b8  /* U+04B8 CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE */
pattern XKB_KEY_Cyrillic_CHE_vertstroke :: KeySymbol
pattern XKB_KEY_Cyrillic_CHE_vertstroke = MkKeySymbol #{const XKB_KEY_Cyrillic_CHE_vertstroke} 
-- #define XKB_KEY_Cyrillic_che_vertstroke    0x10004b9  /* U+04B9 CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE */
pattern XKB_KEY_Cyrillic_che_vertstroke :: KeySymbol
pattern XKB_KEY_Cyrillic_che_vertstroke = MkKeySymbol #{const XKB_KEY_Cyrillic_che_vertstroke} 
-- #define XKB_KEY_Cyrillic_SHHA              0x10004ba  /* U+04BA CYRILLIC CAPITAL LETTER SHHA */
pattern XKB_KEY_Cyrillic_SHHA :: KeySymbol
pattern XKB_KEY_Cyrillic_SHHA = MkKeySymbol #{const XKB_KEY_Cyrillic_SHHA} 
-- #define XKB_KEY_Cyrillic_shha              0x10004bb  /* U+04BB CYRILLIC SMALL LETTER SHHA */
pattern XKB_KEY_Cyrillic_shha :: KeySymbol
pattern XKB_KEY_Cyrillic_shha = MkKeySymbol #{const XKB_KEY_Cyrillic_shha} 
-- #define XKB_KEY_Cyrillic_SCHWA             0x10004d8  /* U+04D8 CYRILLIC CAPITAL LETTER SCHWA */
pattern XKB_KEY_Cyrillic_SCHWA :: KeySymbol
pattern XKB_KEY_Cyrillic_SCHWA = MkKeySymbol #{const XKB_KEY_Cyrillic_SCHWA} 
-- #define XKB_KEY_Cyrillic_schwa             0x10004d9  /* U+04D9 CYRILLIC SMALL LETTER SCHWA */
pattern XKB_KEY_Cyrillic_schwa :: KeySymbol
pattern XKB_KEY_Cyrillic_schwa = MkKeySymbol #{const XKB_KEY_Cyrillic_schwa} 
-- #define XKB_KEY_Cyrillic_I_macron          0x10004e2  /* U+04E2 CYRILLIC CAPITAL LETTER I WITH MACRON */
pattern XKB_KEY_Cyrillic_I_macron :: KeySymbol
pattern XKB_KEY_Cyrillic_I_macron = MkKeySymbol #{const XKB_KEY_Cyrillic_I_macron} 
-- #define XKB_KEY_Cyrillic_i_macron          0x10004e3  /* U+04E3 CYRILLIC SMALL LETTER I WITH MACRON */
pattern XKB_KEY_Cyrillic_i_macron :: KeySymbol
pattern XKB_KEY_Cyrillic_i_macron = MkKeySymbol #{const XKB_KEY_Cyrillic_i_macron} 
-- #define XKB_KEY_Cyrillic_O_bar             0x10004e8  /* U+04E8 CYRILLIC CAPITAL LETTER BARRED O */
pattern XKB_KEY_Cyrillic_O_bar :: KeySymbol
pattern XKB_KEY_Cyrillic_O_bar = MkKeySymbol #{const XKB_KEY_Cyrillic_O_bar} 
-- #define XKB_KEY_Cyrillic_o_bar             0x10004e9  /* U+04E9 CYRILLIC SMALL LETTER BARRED O */
pattern XKB_KEY_Cyrillic_o_bar :: KeySymbol
pattern XKB_KEY_Cyrillic_o_bar = MkKeySymbol #{const XKB_KEY_Cyrillic_o_bar} 
-- #define XKB_KEY_Cyrillic_U_macron          0x10004ee  /* U+04EE CYRILLIC CAPITAL LETTER U WITH MACRON */
pattern XKB_KEY_Cyrillic_U_macron :: KeySymbol
pattern XKB_KEY_Cyrillic_U_macron = MkKeySymbol #{const XKB_KEY_Cyrillic_U_macron} 
-- #define XKB_KEY_Cyrillic_u_macron          0x10004ef  /* U+04EF CYRILLIC SMALL LETTER U WITH MACRON */
pattern XKB_KEY_Cyrillic_u_macron :: KeySymbol
pattern XKB_KEY_Cyrillic_u_macron = MkKeySymbol #{const XKB_KEY_Cyrillic_u_macron} 
-- #define XKB_KEY_Serbian_dje                   0x06a1  /* U+0452 CYRILLIC SMALL LETTER DJE */
pattern XKB_KEY_Serbian_dje :: KeySymbol
pattern XKB_KEY_Serbian_dje = MkKeySymbol #{const XKB_KEY_Serbian_dje} 
-- #define XKB_KEY_Macedonia_gje                 0x06a2  /* U+0453 CYRILLIC SMALL LETTER GJE */
pattern XKB_KEY_Macedonia_gje :: KeySymbol
pattern XKB_KEY_Macedonia_gje = MkKeySymbol #{const XKB_KEY_Macedonia_gje} 
-- #define XKB_KEY_Cyrillic_io                   0x06a3  /* U+0451 CYRILLIC SMALL LETTER IO */
pattern XKB_KEY_Cyrillic_io :: KeySymbol
pattern XKB_KEY_Cyrillic_io = MkKeySymbol #{const XKB_KEY_Cyrillic_io} 
-- #define XKB_KEY_Ukrainian_ie                  0x06a4  /* U+0454 CYRILLIC SMALL LETTER UKRAINIAN IE */
pattern XKB_KEY_Ukrainian_ie :: KeySymbol
pattern XKB_KEY_Ukrainian_ie = MkKeySymbol #{const XKB_KEY_Ukrainian_ie} 
-- #define XKB_KEY_Ukranian_je                   0x06a4  /* deprecated */
pattern XKB_KEY_Ukranian_je :: KeySymbol
pattern XKB_KEY_Ukranian_je = MkKeySymbol #{const XKB_KEY_Ukranian_je} 
-- #define XKB_KEY_Macedonia_dse                 0x06a5  /* U+0455 CYRILLIC SMALL LETTER DZE */
pattern XKB_KEY_Macedonia_dse :: KeySymbol
pattern XKB_KEY_Macedonia_dse = MkKeySymbol #{const XKB_KEY_Macedonia_dse} 
-- #define XKB_KEY_Ukrainian_i                   0x06a6  /* U+0456 CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I */
pattern XKB_KEY_Ukrainian_i :: KeySymbol
pattern XKB_KEY_Ukrainian_i = MkKeySymbol #{const XKB_KEY_Ukrainian_i} 
-- #define XKB_KEY_Ukranian_i                    0x06a6  /* deprecated */
pattern XKB_KEY_Ukranian_i :: KeySymbol
pattern XKB_KEY_Ukranian_i = MkKeySymbol #{const XKB_KEY_Ukranian_i} 
-- #define XKB_KEY_Ukrainian_yi                  0x06a7  /* U+0457 CYRILLIC SMALL LETTER YI */
pattern XKB_KEY_Ukrainian_yi :: KeySymbol
pattern XKB_KEY_Ukrainian_yi = MkKeySymbol #{const XKB_KEY_Ukrainian_yi} 
-- #define XKB_KEY_Ukranian_yi                   0x06a7  /* deprecated */
pattern XKB_KEY_Ukranian_yi :: KeySymbol
pattern XKB_KEY_Ukranian_yi = MkKeySymbol #{const XKB_KEY_Ukranian_yi} 
-- #define XKB_KEY_Cyrillic_je                   0x06a8  /* U+0458 CYRILLIC SMALL LETTER JE */
pattern XKB_KEY_Cyrillic_je :: KeySymbol
pattern XKB_KEY_Cyrillic_je = MkKeySymbol #{const XKB_KEY_Cyrillic_je} 
-- #define XKB_KEY_Serbian_je                    0x06a8  /* deprecated */
pattern XKB_KEY_Serbian_je :: KeySymbol
pattern XKB_KEY_Serbian_je = MkKeySymbol #{const XKB_KEY_Serbian_je} 
-- #define XKB_KEY_Cyrillic_lje                  0x06a9  /* U+0459 CYRILLIC SMALL LETTER LJE */
pattern XKB_KEY_Cyrillic_lje :: KeySymbol
pattern XKB_KEY_Cyrillic_lje = MkKeySymbol #{const XKB_KEY_Cyrillic_lje} 
-- #define XKB_KEY_Serbian_lje                   0x06a9  /* deprecated */
pattern XKB_KEY_Serbian_lje :: KeySymbol
pattern XKB_KEY_Serbian_lje = MkKeySymbol #{const XKB_KEY_Serbian_lje} 
-- #define XKB_KEY_Cyrillic_nje                  0x06aa  /* U+045A CYRILLIC SMALL LETTER NJE */
pattern XKB_KEY_Cyrillic_nje :: KeySymbol
pattern XKB_KEY_Cyrillic_nje = MkKeySymbol #{const XKB_KEY_Cyrillic_nje} 
-- #define XKB_KEY_Serbian_nje                   0x06aa  /* deprecated */
pattern XKB_KEY_Serbian_nje :: KeySymbol
pattern XKB_KEY_Serbian_nje = MkKeySymbol #{const XKB_KEY_Serbian_nje} 
-- #define XKB_KEY_Serbian_tshe                  0x06ab  /* U+045B CYRILLIC SMALL LETTER TSHE */
pattern XKB_KEY_Serbian_tshe :: KeySymbol
pattern XKB_KEY_Serbian_tshe = MkKeySymbol #{const XKB_KEY_Serbian_tshe} 
-- #define XKB_KEY_Macedonia_kje                 0x06ac  /* U+045C CYRILLIC SMALL LETTER KJE */
pattern XKB_KEY_Macedonia_kje :: KeySymbol
pattern XKB_KEY_Macedonia_kje = MkKeySymbol #{const XKB_KEY_Macedonia_kje} 
-- #define XKB_KEY_Ukrainian_ghe_with_upturn     0x06ad  /* U+0491 CYRILLIC SMALL LETTER GHE WITH UPTURN */
pattern XKB_KEY_Ukrainian_ghe_with_upturn :: KeySymbol
pattern XKB_KEY_Ukrainian_ghe_with_upturn = MkKeySymbol #{const XKB_KEY_Ukrainian_ghe_with_upturn} 
-- #define XKB_KEY_Byelorussian_shortu           0x06ae  /* U+045E CYRILLIC SMALL LETTER SHORT U */
pattern XKB_KEY_Byelorussian_shortu :: KeySymbol
pattern XKB_KEY_Byelorussian_shortu = MkKeySymbol #{const XKB_KEY_Byelorussian_shortu} 
-- #define XKB_KEY_Cyrillic_dzhe                 0x06af  /* U+045F CYRILLIC SMALL LETTER DZHE */
pattern XKB_KEY_Cyrillic_dzhe :: KeySymbol
pattern XKB_KEY_Cyrillic_dzhe = MkKeySymbol #{const XKB_KEY_Cyrillic_dzhe} 
-- #define XKB_KEY_Serbian_dze                   0x06af  /* deprecated */
pattern XKB_KEY_Serbian_dze :: KeySymbol
pattern XKB_KEY_Serbian_dze = MkKeySymbol #{const XKB_KEY_Serbian_dze} 
-- #define XKB_KEY_numerosign                    0x06b0  /* U+2116 NUMERO SIGN */
pattern XKB_KEY_numerosign :: KeySymbol
pattern XKB_KEY_numerosign = MkKeySymbol #{const XKB_KEY_numerosign} 
-- #define XKB_KEY_Serbian_DJE                   0x06b1  /* U+0402 CYRILLIC CAPITAL LETTER DJE */
pattern XKB_KEY_Serbian_DJE :: KeySymbol
pattern XKB_KEY_Serbian_DJE = MkKeySymbol #{const XKB_KEY_Serbian_DJE} 
-- #define XKB_KEY_Macedonia_GJE                 0x06b2  /* U+0403 CYRILLIC CAPITAL LETTER GJE */
pattern XKB_KEY_Macedonia_GJE :: KeySymbol
pattern XKB_KEY_Macedonia_GJE = MkKeySymbol #{const XKB_KEY_Macedonia_GJE} 
-- #define XKB_KEY_Cyrillic_IO                   0x06b3  /* U+0401 CYRILLIC CAPITAL LETTER IO */
pattern XKB_KEY_Cyrillic_IO :: KeySymbol
pattern XKB_KEY_Cyrillic_IO = MkKeySymbol #{const XKB_KEY_Cyrillic_IO} 
-- #define XKB_KEY_Ukrainian_IE                  0x06b4  /* U+0404 CYRILLIC CAPITAL LETTER UKRAINIAN IE */
pattern XKB_KEY_Ukrainian_IE :: KeySymbol
pattern XKB_KEY_Ukrainian_IE = MkKeySymbol #{const XKB_KEY_Ukrainian_IE} 
-- #define XKB_KEY_Ukranian_JE                   0x06b4  /* deprecated */
pattern XKB_KEY_Ukranian_JE :: KeySymbol
pattern XKB_KEY_Ukranian_JE = MkKeySymbol #{const XKB_KEY_Ukranian_JE} 
-- #define XKB_KEY_Macedonia_DSE                 0x06b5  /* U+0405 CYRILLIC CAPITAL LETTER DZE */
pattern XKB_KEY_Macedonia_DSE :: KeySymbol
pattern XKB_KEY_Macedonia_DSE = MkKeySymbol #{const XKB_KEY_Macedonia_DSE} 
-- #define XKB_KEY_Ukrainian_I                   0x06b6  /* U+0406 CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I */
pattern XKB_KEY_Ukrainian_I :: KeySymbol
pattern XKB_KEY_Ukrainian_I = MkKeySymbol #{const XKB_KEY_Ukrainian_I} 
-- #define XKB_KEY_Ukranian_I                    0x06b6  /* deprecated */
pattern XKB_KEY_Ukranian_I :: KeySymbol
pattern XKB_KEY_Ukranian_I = MkKeySymbol #{const XKB_KEY_Ukranian_I} 
-- #define XKB_KEY_Ukrainian_YI                  0x06b7  /* U+0407 CYRILLIC CAPITAL LETTER YI */
pattern XKB_KEY_Ukrainian_YI :: KeySymbol
pattern XKB_KEY_Ukrainian_YI = MkKeySymbol #{const XKB_KEY_Ukrainian_YI} 
-- #define XKB_KEY_Ukranian_YI                   0x06b7  /* deprecated */
pattern XKB_KEY_Ukranian_YI :: KeySymbol
pattern XKB_KEY_Ukranian_YI = MkKeySymbol #{const XKB_KEY_Ukranian_YI} 
-- #define XKB_KEY_Cyrillic_JE                   0x06b8  /* U+0408 CYRILLIC CAPITAL LETTER JE */
pattern XKB_KEY_Cyrillic_JE :: KeySymbol
pattern XKB_KEY_Cyrillic_JE = MkKeySymbol #{const XKB_KEY_Cyrillic_JE} 
-- #define XKB_KEY_Serbian_JE                    0x06b8  /* deprecated */
pattern XKB_KEY_Serbian_JE :: KeySymbol
pattern XKB_KEY_Serbian_JE = MkKeySymbol #{const XKB_KEY_Serbian_JE} 
-- #define XKB_KEY_Cyrillic_LJE                  0x06b9  /* U+0409 CYRILLIC CAPITAL LETTER LJE */
pattern XKB_KEY_Cyrillic_LJE :: KeySymbol
pattern XKB_KEY_Cyrillic_LJE = MkKeySymbol #{const XKB_KEY_Cyrillic_LJE} 
-- #define XKB_KEY_Serbian_LJE                   0x06b9  /* deprecated */
pattern XKB_KEY_Serbian_LJE :: KeySymbol
pattern XKB_KEY_Serbian_LJE = MkKeySymbol #{const XKB_KEY_Serbian_LJE} 
-- #define XKB_KEY_Cyrillic_NJE                  0x06ba  /* U+040A CYRILLIC CAPITAL LETTER NJE */
pattern XKB_KEY_Cyrillic_NJE :: KeySymbol
pattern XKB_KEY_Cyrillic_NJE = MkKeySymbol #{const XKB_KEY_Cyrillic_NJE} 
-- #define XKB_KEY_Serbian_NJE                   0x06ba  /* deprecated */
pattern XKB_KEY_Serbian_NJE :: KeySymbol
pattern XKB_KEY_Serbian_NJE = MkKeySymbol #{const XKB_KEY_Serbian_NJE} 
-- #define XKB_KEY_Serbian_TSHE                  0x06bb  /* U+040B CYRILLIC CAPITAL LETTER TSHE */
pattern XKB_KEY_Serbian_TSHE :: KeySymbol
pattern XKB_KEY_Serbian_TSHE = MkKeySymbol #{const XKB_KEY_Serbian_TSHE} 
-- #define XKB_KEY_Macedonia_KJE                 0x06bc  /* U+040C CYRILLIC CAPITAL LETTER KJE */
pattern XKB_KEY_Macedonia_KJE :: KeySymbol
pattern XKB_KEY_Macedonia_KJE = MkKeySymbol #{const XKB_KEY_Macedonia_KJE} 
-- #define XKB_KEY_Ukrainian_GHE_WITH_UPTURN     0x06bd  /* U+0490 CYRILLIC CAPITAL LETTER GHE WITH UPTURN */
pattern XKB_KEY_Ukrainian_GHE_WITH_UPTURN :: KeySymbol
pattern XKB_KEY_Ukrainian_GHE_WITH_UPTURN = MkKeySymbol #{const XKB_KEY_Ukrainian_GHE_WITH_UPTURN} 
-- #define XKB_KEY_Byelorussian_SHORTU           0x06be  /* U+040E CYRILLIC CAPITAL LETTER SHORT U */
pattern XKB_KEY_Byelorussian_SHORTU :: KeySymbol
pattern XKB_KEY_Byelorussian_SHORTU = MkKeySymbol #{const XKB_KEY_Byelorussian_SHORTU} 
-- #define XKB_KEY_Cyrillic_DZHE                 0x06bf  /* U+040F CYRILLIC CAPITAL LETTER DZHE */
pattern XKB_KEY_Cyrillic_DZHE :: KeySymbol
pattern XKB_KEY_Cyrillic_DZHE = MkKeySymbol #{const XKB_KEY_Cyrillic_DZHE} 
-- #define XKB_KEY_Serbian_DZE                   0x06bf  /* deprecated */
pattern XKB_KEY_Serbian_DZE :: KeySymbol
pattern XKB_KEY_Serbian_DZE = MkKeySymbol #{const XKB_KEY_Serbian_DZE} 
-- #define XKB_KEY_Cyrillic_yu                   0x06c0  /* U+044E CYRILLIC SMALL LETTER YU */
pattern XKB_KEY_Cyrillic_yu :: KeySymbol
pattern XKB_KEY_Cyrillic_yu = MkKeySymbol #{const XKB_KEY_Cyrillic_yu} 
-- #define XKB_KEY_Cyrillic_a                    0x06c1  /* U+0430 CYRILLIC SMALL LETTER A */
pattern XKB_KEY_Cyrillic_a :: KeySymbol
pattern XKB_KEY_Cyrillic_a = MkKeySymbol #{const XKB_KEY_Cyrillic_a} 
-- #define XKB_KEY_Cyrillic_be                   0x06c2  /* U+0431 CYRILLIC SMALL LETTER BE */
pattern XKB_KEY_Cyrillic_be :: KeySymbol
pattern XKB_KEY_Cyrillic_be = MkKeySymbol #{const XKB_KEY_Cyrillic_be} 
-- #define XKB_KEY_Cyrillic_tse                  0x06c3  /* U+0446 CYRILLIC SMALL LETTER TSE */
pattern XKB_KEY_Cyrillic_tse :: KeySymbol
pattern XKB_KEY_Cyrillic_tse = MkKeySymbol #{const XKB_KEY_Cyrillic_tse} 
-- #define XKB_KEY_Cyrillic_de                   0x06c4  /* U+0434 CYRILLIC SMALL LETTER DE */
pattern XKB_KEY_Cyrillic_de :: KeySymbol
pattern XKB_KEY_Cyrillic_de = MkKeySymbol #{const XKB_KEY_Cyrillic_de} 
-- #define XKB_KEY_Cyrillic_ie                   0x06c5  /* U+0435 CYRILLIC SMALL LETTER IE */
pattern XKB_KEY_Cyrillic_ie :: KeySymbol
pattern XKB_KEY_Cyrillic_ie = MkKeySymbol #{const XKB_KEY_Cyrillic_ie} 
-- #define XKB_KEY_Cyrillic_ef                   0x06c6  /* U+0444 CYRILLIC SMALL LETTER EF */
pattern XKB_KEY_Cyrillic_ef :: KeySymbol
pattern XKB_KEY_Cyrillic_ef = MkKeySymbol #{const XKB_KEY_Cyrillic_ef} 
-- #define XKB_KEY_Cyrillic_ghe                  0x06c7  /* U+0433 CYRILLIC SMALL LETTER GHE */
pattern XKB_KEY_Cyrillic_ghe :: KeySymbol
pattern XKB_KEY_Cyrillic_ghe = MkKeySymbol #{const XKB_KEY_Cyrillic_ghe} 
-- #define XKB_KEY_Cyrillic_ha                   0x06c8  /* U+0445 CYRILLIC SMALL LETTER HA */
pattern XKB_KEY_Cyrillic_ha :: KeySymbol
pattern XKB_KEY_Cyrillic_ha = MkKeySymbol #{const XKB_KEY_Cyrillic_ha} 
-- #define XKB_KEY_Cyrillic_i                    0x06c9  /* U+0438 CYRILLIC SMALL LETTER I */
pattern XKB_KEY_Cyrillic_i :: KeySymbol
pattern XKB_KEY_Cyrillic_i = MkKeySymbol #{const XKB_KEY_Cyrillic_i} 
-- #define XKB_KEY_Cyrillic_shorti               0x06ca  /* U+0439 CYRILLIC SMALL LETTER SHORT I */
pattern XKB_KEY_Cyrillic_shorti :: KeySymbol
pattern XKB_KEY_Cyrillic_shorti = MkKeySymbol #{const XKB_KEY_Cyrillic_shorti} 
-- #define XKB_KEY_Cyrillic_ka                   0x06cb  /* U+043A CYRILLIC SMALL LETTER KA */
pattern XKB_KEY_Cyrillic_ka :: KeySymbol
pattern XKB_KEY_Cyrillic_ka = MkKeySymbol #{const XKB_KEY_Cyrillic_ka} 
-- #define XKB_KEY_Cyrillic_el                   0x06cc  /* U+043B CYRILLIC SMALL LETTER EL */
pattern XKB_KEY_Cyrillic_el :: KeySymbol
pattern XKB_KEY_Cyrillic_el = MkKeySymbol #{const XKB_KEY_Cyrillic_el} 
-- #define XKB_KEY_Cyrillic_em                   0x06cd  /* U+043C CYRILLIC SMALL LETTER EM */
pattern XKB_KEY_Cyrillic_em :: KeySymbol
pattern XKB_KEY_Cyrillic_em = MkKeySymbol #{const XKB_KEY_Cyrillic_em} 
-- #define XKB_KEY_Cyrillic_en                   0x06ce  /* U+043D CYRILLIC SMALL LETTER EN */
pattern XKB_KEY_Cyrillic_en :: KeySymbol
pattern XKB_KEY_Cyrillic_en = MkKeySymbol #{const XKB_KEY_Cyrillic_en} 
-- #define XKB_KEY_Cyrillic_o                    0x06cf  /* U+043E CYRILLIC SMALL LETTER O */
pattern XKB_KEY_Cyrillic_o :: KeySymbol
pattern XKB_KEY_Cyrillic_o = MkKeySymbol #{const XKB_KEY_Cyrillic_o} 
-- #define XKB_KEY_Cyrillic_pe                   0x06d0  /* U+043F CYRILLIC SMALL LETTER PE */
pattern XKB_KEY_Cyrillic_pe :: KeySymbol
pattern XKB_KEY_Cyrillic_pe = MkKeySymbol #{const XKB_KEY_Cyrillic_pe} 
-- #define XKB_KEY_Cyrillic_ya                   0x06d1  /* U+044F CYRILLIC SMALL LETTER YA */
pattern XKB_KEY_Cyrillic_ya :: KeySymbol
pattern XKB_KEY_Cyrillic_ya = MkKeySymbol #{const XKB_KEY_Cyrillic_ya} 
-- #define XKB_KEY_Cyrillic_er                   0x06d2  /* U+0440 CYRILLIC SMALL LETTER ER */
pattern XKB_KEY_Cyrillic_er :: KeySymbol
pattern XKB_KEY_Cyrillic_er = MkKeySymbol #{const XKB_KEY_Cyrillic_er} 
-- #define XKB_KEY_Cyrillic_es                   0x06d3  /* U+0441 CYRILLIC SMALL LETTER ES */
pattern XKB_KEY_Cyrillic_es :: KeySymbol
pattern XKB_KEY_Cyrillic_es = MkKeySymbol #{const XKB_KEY_Cyrillic_es} 
-- #define XKB_KEY_Cyrillic_te                   0x06d4  /* U+0442 CYRILLIC SMALL LETTER TE */
pattern XKB_KEY_Cyrillic_te :: KeySymbol
pattern XKB_KEY_Cyrillic_te = MkKeySymbol #{const XKB_KEY_Cyrillic_te} 
-- #define XKB_KEY_Cyrillic_u                    0x06d5  /* U+0443 CYRILLIC SMALL LETTER U */
pattern XKB_KEY_Cyrillic_u :: KeySymbol
pattern XKB_KEY_Cyrillic_u = MkKeySymbol #{const XKB_KEY_Cyrillic_u} 
-- #define XKB_KEY_Cyrillic_zhe                  0x06d6  /* U+0436 CYRILLIC SMALL LETTER ZHE */
pattern XKB_KEY_Cyrillic_zhe :: KeySymbol
pattern XKB_KEY_Cyrillic_zhe = MkKeySymbol #{const XKB_KEY_Cyrillic_zhe} 
-- #define XKB_KEY_Cyrillic_ve                   0x06d7  /* U+0432 CYRILLIC SMALL LETTER VE */
pattern XKB_KEY_Cyrillic_ve :: KeySymbol
pattern XKB_KEY_Cyrillic_ve = MkKeySymbol #{const XKB_KEY_Cyrillic_ve} 
-- #define XKB_KEY_Cyrillic_softsign             0x06d8  /* U+044C CYRILLIC SMALL LETTER SOFT SIGN */
pattern XKB_KEY_Cyrillic_softsign :: KeySymbol
pattern XKB_KEY_Cyrillic_softsign = MkKeySymbol #{const XKB_KEY_Cyrillic_softsign} 
-- #define XKB_KEY_Cyrillic_yeru                 0x06d9  /* U+044B CYRILLIC SMALL LETTER YERU */
pattern XKB_KEY_Cyrillic_yeru :: KeySymbol
pattern XKB_KEY_Cyrillic_yeru = MkKeySymbol #{const XKB_KEY_Cyrillic_yeru} 
-- #define XKB_KEY_Cyrillic_ze                   0x06da  /* U+0437 CYRILLIC SMALL LETTER ZE */
pattern XKB_KEY_Cyrillic_ze :: KeySymbol
pattern XKB_KEY_Cyrillic_ze = MkKeySymbol #{const XKB_KEY_Cyrillic_ze} 
-- #define XKB_KEY_Cyrillic_sha                  0x06db  /* U+0448 CYRILLIC SMALL LETTER SHA */
pattern XKB_KEY_Cyrillic_sha :: KeySymbol
pattern XKB_KEY_Cyrillic_sha = MkKeySymbol #{const XKB_KEY_Cyrillic_sha} 
-- #define XKB_KEY_Cyrillic_e                    0x06dc  /* U+044D CYRILLIC SMALL LETTER E */
pattern XKB_KEY_Cyrillic_e :: KeySymbol
pattern XKB_KEY_Cyrillic_e = MkKeySymbol #{const XKB_KEY_Cyrillic_e} 
-- #define XKB_KEY_Cyrillic_shcha                0x06dd  /* U+0449 CYRILLIC SMALL LETTER SHCHA */
pattern XKB_KEY_Cyrillic_shcha :: KeySymbol
pattern XKB_KEY_Cyrillic_shcha = MkKeySymbol #{const XKB_KEY_Cyrillic_shcha} 
-- #define XKB_KEY_Cyrillic_che                  0x06de  /* U+0447 CYRILLIC SMALL LETTER CHE */
pattern XKB_KEY_Cyrillic_che :: KeySymbol
pattern XKB_KEY_Cyrillic_che = MkKeySymbol #{const XKB_KEY_Cyrillic_che} 
-- #define XKB_KEY_Cyrillic_hardsign             0x06df  /* U+044A CYRILLIC SMALL LETTER HARD SIGN */
pattern XKB_KEY_Cyrillic_hardsign :: KeySymbol
pattern XKB_KEY_Cyrillic_hardsign = MkKeySymbol #{const XKB_KEY_Cyrillic_hardsign} 
-- #define XKB_KEY_Cyrillic_YU                   0x06e0  /* U+042E CYRILLIC CAPITAL LETTER YU */
pattern XKB_KEY_Cyrillic_YU :: KeySymbol
pattern XKB_KEY_Cyrillic_YU = MkKeySymbol #{const XKB_KEY_Cyrillic_YU} 
-- #define XKB_KEY_Cyrillic_A                    0x06e1  /* U+0410 CYRILLIC CAPITAL LETTER A */
pattern XKB_KEY_Cyrillic_A :: KeySymbol
pattern XKB_KEY_Cyrillic_A = MkKeySymbol #{const XKB_KEY_Cyrillic_A} 
-- #define XKB_KEY_Cyrillic_BE                   0x06e2  /* U+0411 CYRILLIC CAPITAL LETTER BE */
pattern XKB_KEY_Cyrillic_BE :: KeySymbol
pattern XKB_KEY_Cyrillic_BE = MkKeySymbol #{const XKB_KEY_Cyrillic_BE} 
-- #define XKB_KEY_Cyrillic_TSE                  0x06e3  /* U+0426 CYRILLIC CAPITAL LETTER TSE */
pattern XKB_KEY_Cyrillic_TSE :: KeySymbol
pattern XKB_KEY_Cyrillic_TSE = MkKeySymbol #{const XKB_KEY_Cyrillic_TSE} 
-- #define XKB_KEY_Cyrillic_DE                   0x06e4  /* U+0414 CYRILLIC CAPITAL LETTER DE */
pattern XKB_KEY_Cyrillic_DE :: KeySymbol
pattern XKB_KEY_Cyrillic_DE = MkKeySymbol #{const XKB_KEY_Cyrillic_DE} 
-- #define XKB_KEY_Cyrillic_IE                   0x06e5  /* U+0415 CYRILLIC CAPITAL LETTER IE */
pattern XKB_KEY_Cyrillic_IE :: KeySymbol
pattern XKB_KEY_Cyrillic_IE = MkKeySymbol #{const XKB_KEY_Cyrillic_IE} 
-- #define XKB_KEY_Cyrillic_EF                   0x06e6  /* U+0424 CYRILLIC CAPITAL LETTER EF */
pattern XKB_KEY_Cyrillic_EF :: KeySymbol
pattern XKB_KEY_Cyrillic_EF = MkKeySymbol #{const XKB_KEY_Cyrillic_EF} 
-- #define XKB_KEY_Cyrillic_GHE                  0x06e7  /* U+0413 CYRILLIC CAPITAL LETTER GHE */
pattern XKB_KEY_Cyrillic_GHE :: KeySymbol
pattern XKB_KEY_Cyrillic_GHE = MkKeySymbol #{const XKB_KEY_Cyrillic_GHE} 
-- #define XKB_KEY_Cyrillic_HA                   0x06e8  /* U+0425 CYRILLIC CAPITAL LETTER HA */
pattern XKB_KEY_Cyrillic_HA :: KeySymbol
pattern XKB_KEY_Cyrillic_HA = MkKeySymbol #{const XKB_KEY_Cyrillic_HA} 
-- #define XKB_KEY_Cyrillic_I                    0x06e9  /* U+0418 CYRILLIC CAPITAL LETTER I */
pattern XKB_KEY_Cyrillic_I :: KeySymbol
pattern XKB_KEY_Cyrillic_I = MkKeySymbol #{const XKB_KEY_Cyrillic_I} 
-- #define XKB_KEY_Cyrillic_SHORTI               0x06ea  /* U+0419 CYRILLIC CAPITAL LETTER SHORT I */
pattern XKB_KEY_Cyrillic_SHORTI :: KeySymbol
pattern XKB_KEY_Cyrillic_SHORTI = MkKeySymbol #{const XKB_KEY_Cyrillic_SHORTI} 
-- #define XKB_KEY_Cyrillic_KA                   0x06eb  /* U+041A CYRILLIC CAPITAL LETTER KA */
pattern XKB_KEY_Cyrillic_KA :: KeySymbol
pattern XKB_KEY_Cyrillic_KA = MkKeySymbol #{const XKB_KEY_Cyrillic_KA} 
-- #define XKB_KEY_Cyrillic_EL                   0x06ec  /* U+041B CYRILLIC CAPITAL LETTER EL */
pattern XKB_KEY_Cyrillic_EL :: KeySymbol
pattern XKB_KEY_Cyrillic_EL = MkKeySymbol #{const XKB_KEY_Cyrillic_EL} 
-- #define XKB_KEY_Cyrillic_EM                   0x06ed  /* U+041C CYRILLIC CAPITAL LETTER EM */
pattern XKB_KEY_Cyrillic_EM :: KeySymbol
pattern XKB_KEY_Cyrillic_EM = MkKeySymbol #{const XKB_KEY_Cyrillic_EM} 
-- #define XKB_KEY_Cyrillic_EN                   0x06ee  /* U+041D CYRILLIC CAPITAL LETTER EN */
pattern XKB_KEY_Cyrillic_EN :: KeySymbol
pattern XKB_KEY_Cyrillic_EN = MkKeySymbol #{const XKB_KEY_Cyrillic_EN} 
-- #define XKB_KEY_Cyrillic_O                    0x06ef  /* U+041E CYRILLIC CAPITAL LETTER O */
pattern XKB_KEY_Cyrillic_O :: KeySymbol
pattern XKB_KEY_Cyrillic_O = MkKeySymbol #{const XKB_KEY_Cyrillic_O} 
-- #define XKB_KEY_Cyrillic_PE                   0x06f0  /* U+041F CYRILLIC CAPITAL LETTER PE */
pattern XKB_KEY_Cyrillic_PE :: KeySymbol
pattern XKB_KEY_Cyrillic_PE = MkKeySymbol #{const XKB_KEY_Cyrillic_PE} 
-- #define XKB_KEY_Cyrillic_YA                   0x06f1  /* U+042F CYRILLIC CAPITAL LETTER YA */
pattern XKB_KEY_Cyrillic_YA :: KeySymbol
pattern XKB_KEY_Cyrillic_YA = MkKeySymbol #{const XKB_KEY_Cyrillic_YA} 
-- #define XKB_KEY_Cyrillic_ER                   0x06f2  /* U+0420 CYRILLIC CAPITAL LETTER ER */
pattern XKB_KEY_Cyrillic_ER :: KeySymbol
pattern XKB_KEY_Cyrillic_ER = MkKeySymbol #{const XKB_KEY_Cyrillic_ER} 
-- #define XKB_KEY_Cyrillic_ES                   0x06f3  /* U+0421 CYRILLIC CAPITAL LETTER ES */
pattern XKB_KEY_Cyrillic_ES :: KeySymbol
pattern XKB_KEY_Cyrillic_ES = MkKeySymbol #{const XKB_KEY_Cyrillic_ES} 
-- #define XKB_KEY_Cyrillic_TE                   0x06f4  /* U+0422 CYRILLIC CAPITAL LETTER TE */
pattern XKB_KEY_Cyrillic_TE :: KeySymbol
pattern XKB_KEY_Cyrillic_TE = MkKeySymbol #{const XKB_KEY_Cyrillic_TE} 
-- #define XKB_KEY_Cyrillic_U                    0x06f5  /* U+0423 CYRILLIC CAPITAL LETTER U */
pattern XKB_KEY_Cyrillic_U :: KeySymbol
pattern XKB_KEY_Cyrillic_U = MkKeySymbol #{const XKB_KEY_Cyrillic_U} 
-- #define XKB_KEY_Cyrillic_ZHE                  0x06f6  /* U+0416 CYRILLIC CAPITAL LETTER ZHE */
pattern XKB_KEY_Cyrillic_ZHE :: KeySymbol
pattern XKB_KEY_Cyrillic_ZHE = MkKeySymbol #{const XKB_KEY_Cyrillic_ZHE} 
-- #define XKB_KEY_Cyrillic_VE                   0x06f7  /* U+0412 CYRILLIC CAPITAL LETTER VE */
pattern XKB_KEY_Cyrillic_VE :: KeySymbol
pattern XKB_KEY_Cyrillic_VE = MkKeySymbol #{const XKB_KEY_Cyrillic_VE} 
-- #define XKB_KEY_Cyrillic_SOFTSIGN             0x06f8  /* U+042C CYRILLIC CAPITAL LETTER SOFT SIGN */
pattern XKB_KEY_Cyrillic_SOFTSIGN :: KeySymbol
pattern XKB_KEY_Cyrillic_SOFTSIGN = MkKeySymbol #{const XKB_KEY_Cyrillic_SOFTSIGN} 
-- #define XKB_KEY_Cyrillic_YERU                 0x06f9  /* U+042B CYRILLIC CAPITAL LETTER YERU */
pattern XKB_KEY_Cyrillic_YERU :: KeySymbol
pattern XKB_KEY_Cyrillic_YERU = MkKeySymbol #{const XKB_KEY_Cyrillic_YERU} 
-- #define XKB_KEY_Cyrillic_ZE                   0x06fa  /* U+0417 CYRILLIC CAPITAL LETTER ZE */
pattern XKB_KEY_Cyrillic_ZE :: KeySymbol
pattern XKB_KEY_Cyrillic_ZE = MkKeySymbol #{const XKB_KEY_Cyrillic_ZE} 
-- #define XKB_KEY_Cyrillic_SHA                  0x06fb  /* U+0428 CYRILLIC CAPITAL LETTER SHA */
pattern XKB_KEY_Cyrillic_SHA :: KeySymbol
pattern XKB_KEY_Cyrillic_SHA = MkKeySymbol #{const XKB_KEY_Cyrillic_SHA} 
-- #define XKB_KEY_Cyrillic_E                    0x06fc  /* U+042D CYRILLIC CAPITAL LETTER E */
pattern XKB_KEY_Cyrillic_E :: KeySymbol
pattern XKB_KEY_Cyrillic_E = MkKeySymbol #{const XKB_KEY_Cyrillic_E} 
-- #define XKB_KEY_Cyrillic_SHCHA                0x06fd  /* U+0429 CYRILLIC CAPITAL LETTER SHCHA */
pattern XKB_KEY_Cyrillic_SHCHA :: KeySymbol
pattern XKB_KEY_Cyrillic_SHCHA = MkKeySymbol #{const XKB_KEY_Cyrillic_SHCHA} 
-- #define XKB_KEY_Cyrillic_CHE                  0x06fe  /* U+0427 CYRILLIC CAPITAL LETTER CHE */
pattern XKB_KEY_Cyrillic_CHE :: KeySymbol
pattern XKB_KEY_Cyrillic_CHE = MkKeySymbol #{const XKB_KEY_Cyrillic_CHE} 
-- #define XKB_KEY_Cyrillic_HARDSIGN             0x06ff  /* U+042A CYRILLIC CAPITAL LETTER HARD SIGN */
pattern XKB_KEY_Cyrillic_HARDSIGN :: KeySymbol
pattern XKB_KEY_Cyrillic_HARDSIGN = MkKeySymbol #{const XKB_KEY_Cyrillic_HARDSIGN} 
-- #define XKB_KEY_Greek_ALPHAaccent             0x07a1  /* U+0386 GREEK CAPITAL LETTER ALPHA WITH TONOS */
pattern XKB_KEY_Greek_ALPHAaccent :: KeySymbol
pattern XKB_KEY_Greek_ALPHAaccent = MkKeySymbol #{const XKB_KEY_Greek_ALPHAaccent} 
-- #define XKB_KEY_Greek_EPSILONaccent           0x07a2  /* U+0388 GREEK CAPITAL LETTER EPSILON WITH TONOS */
pattern XKB_KEY_Greek_EPSILONaccent :: KeySymbol
pattern XKB_KEY_Greek_EPSILONaccent = MkKeySymbol #{const XKB_KEY_Greek_EPSILONaccent} 
-- #define XKB_KEY_Greek_ETAaccent               0x07a3  /* U+0389 GREEK CAPITAL LETTER ETA WITH TONOS */
pattern XKB_KEY_Greek_ETAaccent :: KeySymbol
pattern XKB_KEY_Greek_ETAaccent = MkKeySymbol #{const XKB_KEY_Greek_ETAaccent} 
-- #define XKB_KEY_Greek_IOTAaccent              0x07a4  /* U+038A GREEK CAPITAL LETTER IOTA WITH TONOS */
pattern XKB_KEY_Greek_IOTAaccent :: KeySymbol
pattern XKB_KEY_Greek_IOTAaccent = MkKeySymbol #{const XKB_KEY_Greek_IOTAaccent} 
-- #define XKB_KEY_Greek_IOTAdieresis            0x07a5  /* U+03AA GREEK CAPITAL LETTER IOTA WITH DIALYTIKA */
pattern XKB_KEY_Greek_IOTAdieresis :: KeySymbol
pattern XKB_KEY_Greek_IOTAdieresis = MkKeySymbol #{const XKB_KEY_Greek_IOTAdieresis} 
-- #define XKB_KEY_Greek_IOTAdiaeresis           0x07a5  /* old typo */
pattern XKB_KEY_Greek_IOTAdiaeresis :: KeySymbol
pattern XKB_KEY_Greek_IOTAdiaeresis = MkKeySymbol #{const XKB_KEY_Greek_IOTAdiaeresis} 
-- #define XKB_KEY_Greek_OMICRONaccent           0x07a7  /* U+038C GREEK CAPITAL LETTER OMICRON WITH TONOS */
pattern XKB_KEY_Greek_OMICRONaccent :: KeySymbol
pattern XKB_KEY_Greek_OMICRONaccent = MkKeySymbol #{const XKB_KEY_Greek_OMICRONaccent} 
-- #define XKB_KEY_Greek_UPSILONaccent           0x07a8  /* U+038E GREEK CAPITAL LETTER UPSILON WITH TONOS */
pattern XKB_KEY_Greek_UPSILONaccent :: KeySymbol
pattern XKB_KEY_Greek_UPSILONaccent = MkKeySymbol #{const XKB_KEY_Greek_UPSILONaccent} 
-- #define XKB_KEY_Greek_UPSILONdieresis         0x07a9  /* U+03AB GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA */
pattern XKB_KEY_Greek_UPSILONdieresis :: KeySymbol
pattern XKB_KEY_Greek_UPSILONdieresis = MkKeySymbol #{const XKB_KEY_Greek_UPSILONdieresis} 
-- #define XKB_KEY_Greek_OMEGAaccent             0x07ab  /* U+038F GREEK CAPITAL LETTER OMEGA WITH TONOS */
pattern XKB_KEY_Greek_OMEGAaccent :: KeySymbol
pattern XKB_KEY_Greek_OMEGAaccent = MkKeySymbol #{const XKB_KEY_Greek_OMEGAaccent} 
-- #define XKB_KEY_Greek_accentdieresis          0x07ae  /* U+0385 GREEK DIALYTIKA TONOS */
pattern XKB_KEY_Greek_accentdieresis :: KeySymbol
pattern XKB_KEY_Greek_accentdieresis = MkKeySymbol #{const XKB_KEY_Greek_accentdieresis} 
-- #define XKB_KEY_Greek_horizbar                0x07af  /* U+2015 HORIZONTAL BAR */
pattern XKB_KEY_Greek_horizbar :: KeySymbol
pattern XKB_KEY_Greek_horizbar = MkKeySymbol #{const XKB_KEY_Greek_horizbar} 
-- #define XKB_KEY_Greek_alphaaccent             0x07b1  /* U+03AC GREEK SMALL LETTER ALPHA WITH TONOS */
pattern XKB_KEY_Greek_alphaaccent :: KeySymbol
pattern XKB_KEY_Greek_alphaaccent = MkKeySymbol #{const XKB_KEY_Greek_alphaaccent} 
-- #define XKB_KEY_Greek_epsilonaccent           0x07b2  /* U+03AD GREEK SMALL LETTER EPSILON WITH TONOS */
pattern XKB_KEY_Greek_epsilonaccent :: KeySymbol
pattern XKB_KEY_Greek_epsilonaccent = MkKeySymbol #{const XKB_KEY_Greek_epsilonaccent} 
-- #define XKB_KEY_Greek_etaaccent               0x07b3  /* U+03AE GREEK SMALL LETTER ETA WITH TONOS */
pattern XKB_KEY_Greek_etaaccent :: KeySymbol
pattern XKB_KEY_Greek_etaaccent = MkKeySymbol #{const XKB_KEY_Greek_etaaccent} 
-- #define XKB_KEY_Greek_iotaaccent              0x07b4  /* U+03AF GREEK SMALL LETTER IOTA WITH TONOS */
pattern XKB_KEY_Greek_iotaaccent :: KeySymbol
pattern XKB_KEY_Greek_iotaaccent = MkKeySymbol #{const XKB_KEY_Greek_iotaaccent} 
-- #define XKB_KEY_Greek_iotadieresis            0x07b5  /* U+03CA GREEK SMALL LETTER IOTA WITH DIALYTIKA */
pattern XKB_KEY_Greek_iotadieresis :: KeySymbol
pattern XKB_KEY_Greek_iotadieresis = MkKeySymbol #{const XKB_KEY_Greek_iotadieresis} 
-- #define XKB_KEY_Greek_iotaaccentdieresis      0x07b6  /* U+0390 GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS */
pattern XKB_KEY_Greek_iotaaccentdieresis :: KeySymbol
pattern XKB_KEY_Greek_iotaaccentdieresis = MkKeySymbol #{const XKB_KEY_Greek_iotaaccentdieresis} 
-- #define XKB_KEY_Greek_omicronaccent           0x07b7  /* U+03CC GREEK SMALL LETTER OMICRON WITH TONOS */
pattern XKB_KEY_Greek_omicronaccent :: KeySymbol
pattern XKB_KEY_Greek_omicronaccent = MkKeySymbol #{const XKB_KEY_Greek_omicronaccent} 
-- #define XKB_KEY_Greek_upsilonaccent           0x07b8  /* U+03CD GREEK SMALL LETTER UPSILON WITH TONOS */
pattern XKB_KEY_Greek_upsilonaccent :: KeySymbol
pattern XKB_KEY_Greek_upsilonaccent = MkKeySymbol #{const XKB_KEY_Greek_upsilonaccent} 
-- #define XKB_KEY_Greek_upsilondieresis         0x07b9  /* U+03CB GREEK SMALL LETTER UPSILON WITH DIALYTIKA */
pattern XKB_KEY_Greek_upsilondieresis :: KeySymbol
pattern XKB_KEY_Greek_upsilondieresis = MkKeySymbol #{const XKB_KEY_Greek_upsilondieresis} 
-- #define XKB_KEY_Greek_upsilonaccentdieresis   0x07ba  /* U+03B0 GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS */
pattern XKB_KEY_Greek_upsilonaccentdieresis :: KeySymbol
pattern XKB_KEY_Greek_upsilonaccentdieresis = MkKeySymbol #{const XKB_KEY_Greek_upsilonaccentdieresis} 
-- #define XKB_KEY_Greek_omegaaccent             0x07bb  /* U+03CE GREEK SMALL LETTER OMEGA WITH TONOS */
pattern XKB_KEY_Greek_omegaaccent :: KeySymbol
pattern XKB_KEY_Greek_omegaaccent = MkKeySymbol #{const XKB_KEY_Greek_omegaaccent} 
-- #define XKB_KEY_Greek_ALPHA                   0x07c1  /* U+0391 GREEK CAPITAL LETTER ALPHA */
pattern XKB_KEY_Greek_ALPHA :: KeySymbol
pattern XKB_KEY_Greek_ALPHA = MkKeySymbol #{const XKB_KEY_Greek_ALPHA} 
-- #define XKB_KEY_Greek_BETA                    0x07c2  /* U+0392 GREEK CAPITAL LETTER BETA */
pattern XKB_KEY_Greek_BETA :: KeySymbol
pattern XKB_KEY_Greek_BETA = MkKeySymbol #{const XKB_KEY_Greek_BETA} 
-- #define XKB_KEY_Greek_GAMMA                   0x07c3  /* U+0393 GREEK CAPITAL LETTER GAMMA */
pattern XKB_KEY_Greek_GAMMA :: KeySymbol
pattern XKB_KEY_Greek_GAMMA = MkKeySymbol #{const XKB_KEY_Greek_GAMMA} 
-- #define XKB_KEY_Greek_DELTA                   0x07c4  /* U+0394 GREEK CAPITAL LETTER DELTA */
pattern XKB_KEY_Greek_DELTA :: KeySymbol
pattern XKB_KEY_Greek_DELTA = MkKeySymbol #{const XKB_KEY_Greek_DELTA} 
-- #define XKB_KEY_Greek_EPSILON                 0x07c5  /* U+0395 GREEK CAPITAL LETTER EPSILON */
pattern XKB_KEY_Greek_EPSILON :: KeySymbol
pattern XKB_KEY_Greek_EPSILON = MkKeySymbol #{const XKB_KEY_Greek_EPSILON} 
-- #define XKB_KEY_Greek_ZETA                    0x07c6  /* U+0396 GREEK CAPITAL LETTER ZETA */
pattern XKB_KEY_Greek_ZETA :: KeySymbol
pattern XKB_KEY_Greek_ZETA = MkKeySymbol #{const XKB_KEY_Greek_ZETA} 
-- #define XKB_KEY_Greek_ETA                     0x07c7  /* U+0397 GREEK CAPITAL LETTER ETA */
pattern XKB_KEY_Greek_ETA :: KeySymbol
pattern XKB_KEY_Greek_ETA = MkKeySymbol #{const XKB_KEY_Greek_ETA} 
-- #define XKB_KEY_Greek_THETA                   0x07c8  /* U+0398 GREEK CAPITAL LETTER THETA */
pattern XKB_KEY_Greek_THETA :: KeySymbol
pattern XKB_KEY_Greek_THETA = MkKeySymbol #{const XKB_KEY_Greek_THETA} 
-- #define XKB_KEY_Greek_IOTA                    0x07c9  /* U+0399 GREEK CAPITAL LETTER IOTA */
pattern XKB_KEY_Greek_IOTA :: KeySymbol
pattern XKB_KEY_Greek_IOTA = MkKeySymbol #{const XKB_KEY_Greek_IOTA} 
-- #define XKB_KEY_Greek_KAPPA                   0x07ca  /* U+039A GREEK CAPITAL LETTER KAPPA */
pattern XKB_KEY_Greek_KAPPA :: KeySymbol
pattern XKB_KEY_Greek_KAPPA = MkKeySymbol #{const XKB_KEY_Greek_KAPPA} 
-- #define XKB_KEY_Greek_LAMDA                   0x07cb  /* U+039B GREEK CAPITAL LETTER LAMDA */
pattern XKB_KEY_Greek_LAMDA :: KeySymbol
pattern XKB_KEY_Greek_LAMDA = MkKeySymbol #{const XKB_KEY_Greek_LAMDA} 
-- #define XKB_KEY_Greek_LAMBDA                  0x07cb  /* U+039B GREEK CAPITAL LETTER LAMDA */
pattern XKB_KEY_Greek_LAMBDA :: KeySymbol
pattern XKB_KEY_Greek_LAMBDA = MkKeySymbol #{const XKB_KEY_Greek_LAMBDA} 
-- #define XKB_KEY_Greek_MU                      0x07cc  /* U+039C GREEK CAPITAL LETTER MU */
pattern XKB_KEY_Greek_MU :: KeySymbol
pattern XKB_KEY_Greek_MU = MkKeySymbol #{const XKB_KEY_Greek_MU} 
-- #define XKB_KEY_Greek_NU                      0x07cd  /* U+039D GREEK CAPITAL LETTER NU */
pattern XKB_KEY_Greek_NU :: KeySymbol
pattern XKB_KEY_Greek_NU = MkKeySymbol #{const XKB_KEY_Greek_NU} 
-- #define XKB_KEY_Greek_XI                      0x07ce  /* U+039E GREEK CAPITAL LETTER XI */
pattern XKB_KEY_Greek_XI :: KeySymbol
pattern XKB_KEY_Greek_XI = MkKeySymbol #{const XKB_KEY_Greek_XI} 
-- #define XKB_KEY_Greek_OMICRON                 0x07cf  /* U+039F GREEK CAPITAL LETTER OMICRON */
pattern XKB_KEY_Greek_OMICRON :: KeySymbol
pattern XKB_KEY_Greek_OMICRON = MkKeySymbol #{const XKB_KEY_Greek_OMICRON} 
-- #define XKB_KEY_Greek_PI                      0x07d0  /* U+03A0 GREEK CAPITAL LETTER PI */
pattern XKB_KEY_Greek_PI :: KeySymbol
pattern XKB_KEY_Greek_PI = MkKeySymbol #{const XKB_KEY_Greek_PI} 
-- #define XKB_KEY_Greek_RHO                     0x07d1  /* U+03A1 GREEK CAPITAL LETTER RHO */
pattern XKB_KEY_Greek_RHO :: KeySymbol
pattern XKB_KEY_Greek_RHO = MkKeySymbol #{const XKB_KEY_Greek_RHO} 
-- #define XKB_KEY_Greek_SIGMA                   0x07d2  /* U+03A3 GREEK CAPITAL LETTER SIGMA */
pattern XKB_KEY_Greek_SIGMA :: KeySymbol
pattern XKB_KEY_Greek_SIGMA = MkKeySymbol #{const XKB_KEY_Greek_SIGMA} 
-- #define XKB_KEY_Greek_TAU                     0x07d4  /* U+03A4 GREEK CAPITAL LETTER TAU */
pattern XKB_KEY_Greek_TAU :: KeySymbol
pattern XKB_KEY_Greek_TAU = MkKeySymbol #{const XKB_KEY_Greek_TAU} 
-- #define XKB_KEY_Greek_UPSILON                 0x07d5  /* U+03A5 GREEK CAPITAL LETTER UPSILON */
pattern XKB_KEY_Greek_UPSILON :: KeySymbol
pattern XKB_KEY_Greek_UPSILON = MkKeySymbol #{const XKB_KEY_Greek_UPSILON} 
-- #define XKB_KEY_Greek_PHI                     0x07d6  /* U+03A6 GREEK CAPITAL LETTER PHI */
pattern XKB_KEY_Greek_PHI :: KeySymbol
pattern XKB_KEY_Greek_PHI = MkKeySymbol #{const XKB_KEY_Greek_PHI} 
-- #define XKB_KEY_Greek_CHI                     0x07d7  /* U+03A7 GREEK CAPITAL LETTER CHI */
pattern XKB_KEY_Greek_CHI :: KeySymbol
pattern XKB_KEY_Greek_CHI = MkKeySymbol #{const XKB_KEY_Greek_CHI} 
-- #define XKB_KEY_Greek_PSI                     0x07d8  /* U+03A8 GREEK CAPITAL LETTER PSI */
pattern XKB_KEY_Greek_PSI :: KeySymbol
pattern XKB_KEY_Greek_PSI = MkKeySymbol #{const XKB_KEY_Greek_PSI} 
-- #define XKB_KEY_Greek_OMEGA                   0x07d9  /* U+03A9 GREEK CAPITAL LETTER OMEGA */
pattern XKB_KEY_Greek_OMEGA :: KeySymbol
pattern XKB_KEY_Greek_OMEGA = MkKeySymbol #{const XKB_KEY_Greek_OMEGA} 
-- #define XKB_KEY_Greek_alpha                   0x07e1  /* U+03B1 GREEK SMALL LETTER ALPHA */
pattern XKB_KEY_Greek_alpha :: KeySymbol
pattern XKB_KEY_Greek_alpha = MkKeySymbol #{const XKB_KEY_Greek_alpha} 
-- #define XKB_KEY_Greek_beta                    0x07e2  /* U+03B2 GREEK SMALL LETTER BETA */
pattern XKB_KEY_Greek_beta :: KeySymbol
pattern XKB_KEY_Greek_beta = MkKeySymbol #{const XKB_KEY_Greek_beta} 
-- #define XKB_KEY_Greek_gamma                   0x07e3  /* U+03B3 GREEK SMALL LETTER GAMMA */
pattern XKB_KEY_Greek_gamma :: KeySymbol
pattern XKB_KEY_Greek_gamma = MkKeySymbol #{const XKB_KEY_Greek_gamma} 
-- #define XKB_KEY_Greek_delta                   0x07e4  /* U+03B4 GREEK SMALL LETTER DELTA */
pattern XKB_KEY_Greek_delta :: KeySymbol
pattern XKB_KEY_Greek_delta = MkKeySymbol #{const XKB_KEY_Greek_delta} 
-- #define XKB_KEY_Greek_epsilon                 0x07e5  /* U+03B5 GREEK SMALL LETTER EPSILON */
pattern XKB_KEY_Greek_epsilon :: KeySymbol
pattern XKB_KEY_Greek_epsilon = MkKeySymbol #{const XKB_KEY_Greek_epsilon} 
-- #define XKB_KEY_Greek_zeta                    0x07e6  /* U+03B6 GREEK SMALL LETTER ZETA */
pattern XKB_KEY_Greek_zeta :: KeySymbol
pattern XKB_KEY_Greek_zeta = MkKeySymbol #{const XKB_KEY_Greek_zeta} 
-- #define XKB_KEY_Greek_eta                     0x07e7  /* U+03B7 GREEK SMALL LETTER ETA */
pattern XKB_KEY_Greek_eta :: KeySymbol
pattern XKB_KEY_Greek_eta = MkKeySymbol #{const XKB_KEY_Greek_eta} 
-- #define XKB_KEY_Greek_theta                   0x07e8  /* U+03B8 GREEK SMALL LETTER THETA */
pattern XKB_KEY_Greek_theta :: KeySymbol
pattern XKB_KEY_Greek_theta = MkKeySymbol #{const XKB_KEY_Greek_theta} 
-- #define XKB_KEY_Greek_iota                    0x07e9  /* U+03B9 GREEK SMALL LETTER IOTA */
pattern XKB_KEY_Greek_iota :: KeySymbol
pattern XKB_KEY_Greek_iota = MkKeySymbol #{const XKB_KEY_Greek_iota} 
-- #define XKB_KEY_Greek_kappa                   0x07ea  /* U+03BA GREEK SMALL LETTER KAPPA */
pattern XKB_KEY_Greek_kappa :: KeySymbol
pattern XKB_KEY_Greek_kappa = MkKeySymbol #{const XKB_KEY_Greek_kappa} 
-- #define XKB_KEY_Greek_lamda                   0x07eb  /* U+03BB GREEK SMALL LETTER LAMDA */
pattern XKB_KEY_Greek_lamda :: KeySymbol
pattern XKB_KEY_Greek_lamda = MkKeySymbol #{const XKB_KEY_Greek_lamda} 
-- #define XKB_KEY_Greek_lambda                  0x07eb  /* U+03BB GREEK SMALL LETTER LAMDA */
pattern XKB_KEY_Greek_lambda :: KeySymbol
pattern XKB_KEY_Greek_lambda = MkKeySymbol #{const XKB_KEY_Greek_lambda} 
-- #define XKB_KEY_Greek_mu                      0x07ec  /* U+03BC GREEK SMALL LETTER MU */
pattern XKB_KEY_Greek_mu :: KeySymbol
pattern XKB_KEY_Greek_mu = MkKeySymbol #{const XKB_KEY_Greek_mu} 
-- #define XKB_KEY_Greek_nu                      0x07ed  /* U+03BD GREEK SMALL LETTER NU */
pattern XKB_KEY_Greek_nu :: KeySymbol
pattern XKB_KEY_Greek_nu = MkKeySymbol #{const XKB_KEY_Greek_nu} 
-- #define XKB_KEY_Greek_xi                      0x07ee  /* U+03BE GREEK SMALL LETTER XI */
pattern XKB_KEY_Greek_xi :: KeySymbol
pattern XKB_KEY_Greek_xi = MkKeySymbol #{const XKB_KEY_Greek_xi} 
-- #define XKB_KEY_Greek_omicron                 0x07ef  /* U+03BF GREEK SMALL LETTER OMICRON */
pattern XKB_KEY_Greek_omicron :: KeySymbol
pattern XKB_KEY_Greek_omicron = MkKeySymbol #{const XKB_KEY_Greek_omicron} 
-- #define XKB_KEY_Greek_pi                      0x07f0  /* U+03C0 GREEK SMALL LETTER PI */
pattern XKB_KEY_Greek_pi :: KeySymbol
pattern XKB_KEY_Greek_pi = MkKeySymbol #{const XKB_KEY_Greek_pi} 
-- #define XKB_KEY_Greek_rho                     0x07f1  /* U+03C1 GREEK SMALL LETTER RHO */
pattern XKB_KEY_Greek_rho :: KeySymbol
pattern XKB_KEY_Greek_rho = MkKeySymbol #{const XKB_KEY_Greek_rho} 
-- #define XKB_KEY_Greek_sigma                   0x07f2  /* U+03C3 GREEK SMALL LETTER SIGMA */
pattern XKB_KEY_Greek_sigma :: KeySymbol
pattern XKB_KEY_Greek_sigma = MkKeySymbol #{const XKB_KEY_Greek_sigma} 
-- #define XKB_KEY_Greek_finalsmallsigma         0x07f3  /* U+03C2 GREEK SMALL LETTER FINAL SIGMA */
pattern XKB_KEY_Greek_finalsmallsigma :: KeySymbol
pattern XKB_KEY_Greek_finalsmallsigma = MkKeySymbol #{const XKB_KEY_Greek_finalsmallsigma} 
-- #define XKB_KEY_Greek_tau                     0x07f4  /* U+03C4 GREEK SMALL LETTER TAU */
pattern XKB_KEY_Greek_tau :: KeySymbol
pattern XKB_KEY_Greek_tau = MkKeySymbol #{const XKB_KEY_Greek_tau} 
-- #define XKB_KEY_Greek_upsilon                 0x07f5  /* U+03C5 GREEK SMALL LETTER UPSILON */
pattern XKB_KEY_Greek_upsilon :: KeySymbol
pattern XKB_KEY_Greek_upsilon = MkKeySymbol #{const XKB_KEY_Greek_upsilon} 
-- #define XKB_KEY_Greek_phi                     0x07f6  /* U+03C6 GREEK SMALL LETTER PHI */
pattern XKB_KEY_Greek_phi :: KeySymbol
pattern XKB_KEY_Greek_phi = MkKeySymbol #{const XKB_KEY_Greek_phi} 
-- #define XKB_KEY_Greek_chi                     0x07f7  /* U+03C7 GREEK SMALL LETTER CHI */
pattern XKB_KEY_Greek_chi :: KeySymbol
pattern XKB_KEY_Greek_chi = MkKeySymbol #{const XKB_KEY_Greek_chi} 
-- #define XKB_KEY_Greek_psi                     0x07f8  /* U+03C8 GREEK SMALL LETTER PSI */
pattern XKB_KEY_Greek_psi :: KeySymbol
pattern XKB_KEY_Greek_psi = MkKeySymbol #{const XKB_KEY_Greek_psi} 
-- #define XKB_KEY_Greek_omega                   0x07f9  /* U+03C9 GREEK SMALL LETTER OMEGA */
pattern XKB_KEY_Greek_omega :: KeySymbol
pattern XKB_KEY_Greek_omega = MkKeySymbol #{const XKB_KEY_Greek_omega} 
-- #define XKB_KEY_Greek_switch                  0xff7e  /* Alias for mode_switch */
pattern XKB_KEY_Greek_switch :: KeySymbol
pattern XKB_KEY_Greek_switch = MkKeySymbol #{const XKB_KEY_Greek_switch} 
-- #define XKB_KEY_leftradical                   0x08a1  /* U+23B7 RADICAL SYMBOL BOTTOM */
pattern XKB_KEY_leftradical :: KeySymbol
pattern XKB_KEY_leftradical = MkKeySymbol #{const XKB_KEY_leftradical} 
-- #define XKB_KEY_topleftradical                0x08a2  /*(U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT)*/
pattern XKB_KEY_topleftradical :: KeySymbol
pattern XKB_KEY_topleftradical = MkKeySymbol #{const XKB_KEY_topleftradical} 
-- #define XKB_KEY_horizconnector                0x08a3  /*(U+2500 BOX DRAWINGS LIGHT HORIZONTAL)*/
pattern XKB_KEY_horizconnector :: KeySymbol
pattern XKB_KEY_horizconnector = MkKeySymbol #{const XKB_KEY_horizconnector} 
-- #define XKB_KEY_topintegral                   0x08a4  /* U+2320 TOP HALF INTEGRAL */
pattern XKB_KEY_topintegral :: KeySymbol
pattern XKB_KEY_topintegral = MkKeySymbol #{const XKB_KEY_topintegral} 
-- #define XKB_KEY_botintegral                   0x08a5  /* U+2321 BOTTOM HALF INTEGRAL */
pattern XKB_KEY_botintegral :: KeySymbol
pattern XKB_KEY_botintegral = MkKeySymbol #{const XKB_KEY_botintegral} 
-- #define XKB_KEY_vertconnector                 0x08a6  /*(U+2502 BOX DRAWINGS LIGHT VERTICAL)*/
pattern XKB_KEY_vertconnector :: KeySymbol
pattern XKB_KEY_vertconnector = MkKeySymbol #{const XKB_KEY_vertconnector} 
-- #define XKB_KEY_topleftsqbracket              0x08a7  /* U+23A1 LEFT SQUARE BRACKET UPPER CORNER */
pattern XKB_KEY_topleftsqbracket :: KeySymbol
pattern XKB_KEY_topleftsqbracket = MkKeySymbol #{const XKB_KEY_topleftsqbracket} 
-- #define XKB_KEY_botleftsqbracket              0x08a8  /* U+23A3 LEFT SQUARE BRACKET LOWER CORNER */
pattern XKB_KEY_botleftsqbracket :: KeySymbol
pattern XKB_KEY_botleftsqbracket = MkKeySymbol #{const XKB_KEY_botleftsqbracket} 
-- #define XKB_KEY_toprightsqbracket             0x08a9  /* U+23A4 RIGHT SQUARE BRACKET UPPER CORNER */
pattern XKB_KEY_toprightsqbracket :: KeySymbol
pattern XKB_KEY_toprightsqbracket = MkKeySymbol #{const XKB_KEY_toprightsqbracket} 
-- #define XKB_KEY_botrightsqbracket             0x08aa  /* U+23A6 RIGHT SQUARE BRACKET LOWER CORNER */
pattern XKB_KEY_botrightsqbracket :: KeySymbol
pattern XKB_KEY_botrightsqbracket = MkKeySymbol #{const XKB_KEY_botrightsqbracket} 
-- #define XKB_KEY_topleftparens                 0x08ab  /* U+239B LEFT PARENTHESIS UPPER HOOK */
pattern XKB_KEY_topleftparens :: KeySymbol
pattern XKB_KEY_topleftparens = MkKeySymbol #{const XKB_KEY_topleftparens} 
-- #define XKB_KEY_botleftparens                 0x08ac  /* U+239D LEFT PARENTHESIS LOWER HOOK */
pattern XKB_KEY_botleftparens :: KeySymbol
pattern XKB_KEY_botleftparens = MkKeySymbol #{const XKB_KEY_botleftparens} 
-- #define XKB_KEY_toprightparens                0x08ad  /* U+239E RIGHT PARENTHESIS UPPER HOOK */
pattern XKB_KEY_toprightparens :: KeySymbol
pattern XKB_KEY_toprightparens = MkKeySymbol #{const XKB_KEY_toprightparens} 
-- #define XKB_KEY_botrightparens                0x08ae  /* U+23A0 RIGHT PARENTHESIS LOWER HOOK */
pattern XKB_KEY_botrightparens :: KeySymbol
pattern XKB_KEY_botrightparens = MkKeySymbol #{const XKB_KEY_botrightparens} 
-- #define XKB_KEY_leftmiddlecurlybrace          0x08af  /* U+23A8 LEFT CURLY BRACKET MIDDLE PIECE */
pattern XKB_KEY_leftmiddlecurlybrace :: KeySymbol
pattern XKB_KEY_leftmiddlecurlybrace = MkKeySymbol #{const XKB_KEY_leftmiddlecurlybrace} 
-- #define XKB_KEY_rightmiddlecurlybrace         0x08b0  /* U+23AC RIGHT CURLY BRACKET MIDDLE PIECE */
pattern XKB_KEY_rightmiddlecurlybrace :: KeySymbol
pattern XKB_KEY_rightmiddlecurlybrace = MkKeySymbol #{const XKB_KEY_rightmiddlecurlybrace} 
-- #define XKB_KEY_topleftsummation              0x08b1
pattern XKB_KEY_topleftsummation :: KeySymbol
pattern XKB_KEY_topleftsummation = MkKeySymbol #{const XKB_KEY_topleftsummation} 
-- #define XKB_KEY_botleftsummation              0x08b2
pattern XKB_KEY_botleftsummation :: KeySymbol
pattern XKB_KEY_botleftsummation = MkKeySymbol #{const XKB_KEY_botleftsummation} 
-- #define XKB_KEY_topvertsummationconnector     0x08b3
pattern XKB_KEY_topvertsummationconnector :: KeySymbol
pattern XKB_KEY_topvertsummationconnector = MkKeySymbol #{const XKB_KEY_topvertsummationconnector} 
-- #define XKB_KEY_botvertsummationconnector     0x08b4
pattern XKB_KEY_botvertsummationconnector :: KeySymbol
pattern XKB_KEY_botvertsummationconnector = MkKeySymbol #{const XKB_KEY_botvertsummationconnector} 
-- #define XKB_KEY_toprightsummation             0x08b5
pattern XKB_KEY_toprightsummation :: KeySymbol
pattern XKB_KEY_toprightsummation = MkKeySymbol #{const XKB_KEY_toprightsummation} 
-- #define XKB_KEY_botrightsummation             0x08b6
pattern XKB_KEY_botrightsummation :: KeySymbol
pattern XKB_KEY_botrightsummation = MkKeySymbol #{const XKB_KEY_botrightsummation} 
-- #define XKB_KEY_rightmiddlesummation          0x08b7
pattern XKB_KEY_rightmiddlesummation :: KeySymbol
pattern XKB_KEY_rightmiddlesummation = MkKeySymbol #{const XKB_KEY_rightmiddlesummation} 
-- #define XKB_KEY_lessthanequal                 0x08bc  /* U+2264 LESS-THAN OR EQUAL TO */
pattern XKB_KEY_lessthanequal :: KeySymbol
pattern XKB_KEY_lessthanequal = MkKeySymbol #{const XKB_KEY_lessthanequal} 
-- #define XKB_KEY_notequal                      0x08bd  /* U+2260 NOT EQUAL TO */
pattern XKB_KEY_notequal :: KeySymbol
pattern XKB_KEY_notequal = MkKeySymbol #{const XKB_KEY_notequal} 
-- #define XKB_KEY_greaterthanequal              0x08be  /* U+2265 GREATER-THAN OR EQUAL TO */
pattern XKB_KEY_greaterthanequal :: KeySymbol
pattern XKB_KEY_greaterthanequal = MkKeySymbol #{const XKB_KEY_greaterthanequal} 
-- #define XKB_KEY_integral                      0x08bf  /* U+222B INTEGRAL */
pattern XKB_KEY_integral :: KeySymbol
pattern XKB_KEY_integral = MkKeySymbol #{const XKB_KEY_integral} 
-- #define XKB_KEY_therefore                     0x08c0  /* U+2234 THEREFORE */
pattern XKB_KEY_therefore :: KeySymbol
pattern XKB_KEY_therefore = MkKeySymbol #{const XKB_KEY_therefore} 
-- #define XKB_KEY_variation                     0x08c1  /* U+221D PROPORTIONAL TO */
pattern XKB_KEY_variation :: KeySymbol
pattern XKB_KEY_variation = MkKeySymbol #{const XKB_KEY_variation} 
-- #define XKB_KEY_infinity                      0x08c2  /* U+221E INFINITY */
pattern XKB_KEY_infinity :: KeySymbol
pattern XKB_KEY_infinity = MkKeySymbol #{const XKB_KEY_infinity} 
-- #define XKB_KEY_nabla                         0x08c5  /* U+2207 NABLA */
pattern XKB_KEY_nabla :: KeySymbol
pattern XKB_KEY_nabla = MkKeySymbol #{const XKB_KEY_nabla} 
-- #define XKB_KEY_approximate                   0x08c8  /* U+223C TILDE OPERATOR */
pattern XKB_KEY_approximate :: KeySymbol
pattern XKB_KEY_approximate = MkKeySymbol #{const XKB_KEY_approximate} 
-- #define XKB_KEY_similarequal                  0x08c9  /* U+2243 ASYMPTOTICALLY EQUAL TO */
pattern XKB_KEY_similarequal :: KeySymbol
pattern XKB_KEY_similarequal = MkKeySymbol #{const XKB_KEY_similarequal} 
-- #define XKB_KEY_ifonlyif                      0x08cd  /* U+21D4 LEFT RIGHT DOUBLE ARROW */
pattern XKB_KEY_ifonlyif :: KeySymbol
pattern XKB_KEY_ifonlyif = MkKeySymbol #{const XKB_KEY_ifonlyif} 
-- #define XKB_KEY_implies                       0x08ce  /* U+21D2 RIGHTWARDS DOUBLE ARROW */
pattern XKB_KEY_implies :: KeySymbol
pattern XKB_KEY_implies = MkKeySymbol #{const XKB_KEY_implies} 
-- #define XKB_KEY_identical                     0x08cf  /* U+2261 IDENTICAL TO */
pattern XKB_KEY_identical :: KeySymbol
pattern XKB_KEY_identical = MkKeySymbol #{const XKB_KEY_identical} 
-- #define XKB_KEY_radical                       0x08d6  /* U+221A SQUARE ROOT */
pattern XKB_KEY_radical :: KeySymbol
pattern XKB_KEY_radical = MkKeySymbol #{const XKB_KEY_radical} 
-- #define XKB_KEY_includedin                    0x08da  /* U+2282 SUBSET OF */
pattern XKB_KEY_includedin :: KeySymbol
pattern XKB_KEY_includedin = MkKeySymbol #{const XKB_KEY_includedin} 
-- #define XKB_KEY_includes                      0x08db  /* U+2283 SUPERSET OF */
pattern XKB_KEY_includes :: KeySymbol
pattern XKB_KEY_includes = MkKeySymbol #{const XKB_KEY_includes} 
-- #define XKB_KEY_intersection                  0x08dc  /* U+2229 INTERSECTION */
pattern XKB_KEY_intersection :: KeySymbol
pattern XKB_KEY_intersection = MkKeySymbol #{const XKB_KEY_intersection} 
-- #define XKB_KEY_union                         0x08dd  /* U+222A UNION */
pattern XKB_KEY_union :: KeySymbol
pattern XKB_KEY_union = MkKeySymbol #{const XKB_KEY_union} 
-- #define XKB_KEY_logicaland                    0x08de  /* U+2227 LOGICAL AND */
pattern XKB_KEY_logicaland :: KeySymbol
pattern XKB_KEY_logicaland = MkKeySymbol #{const XKB_KEY_logicaland} 
-- #define XKB_KEY_logicalor                     0x08df  /* U+2228 LOGICAL OR */
pattern XKB_KEY_logicalor :: KeySymbol
pattern XKB_KEY_logicalor = MkKeySymbol #{const XKB_KEY_logicalor} 
-- #define XKB_KEY_partialderivative             0x08ef  /* U+2202 PARTIAL DIFFERENTIAL */
pattern XKB_KEY_partialderivative :: KeySymbol
pattern XKB_KEY_partialderivative = MkKeySymbol #{const XKB_KEY_partialderivative} 
-- #define XKB_KEY_function                      0x08f6  /* U+0192 LATIN SMALL LETTER F WITH HOOK */
pattern XKB_KEY_function :: KeySymbol
pattern XKB_KEY_function = MkKeySymbol #{const XKB_KEY_function} 
-- #define XKB_KEY_leftarrow                     0x08fb  /* U+2190 LEFTWARDS ARROW */
pattern XKB_KEY_leftarrow :: KeySymbol
pattern XKB_KEY_leftarrow = MkKeySymbol #{const XKB_KEY_leftarrow} 
-- #define XKB_KEY_uparrow                       0x08fc  /* U+2191 UPWARDS ARROW */
pattern XKB_KEY_uparrow :: KeySymbol
pattern XKB_KEY_uparrow = MkKeySymbol #{const XKB_KEY_uparrow} 
-- #define XKB_KEY_rightarrow                    0x08fd  /* U+2192 RIGHTWARDS ARROW */
pattern XKB_KEY_rightarrow :: KeySymbol
pattern XKB_KEY_rightarrow = MkKeySymbol #{const XKB_KEY_rightarrow} 
-- #define XKB_KEY_downarrow                     0x08fe  /* U+2193 DOWNWARDS ARROW */
pattern XKB_KEY_downarrow :: KeySymbol
pattern XKB_KEY_downarrow = MkKeySymbol #{const XKB_KEY_downarrow} 
-- #define XKB_KEY_blank                         0x09df
pattern XKB_KEY_blank :: KeySymbol
pattern XKB_KEY_blank = MkKeySymbol #{const XKB_KEY_blank} 
-- #define XKB_KEY_soliddiamond                  0x09e0  /* U+25C6 BLACK DIAMOND */
pattern XKB_KEY_soliddiamond :: KeySymbol
pattern XKB_KEY_soliddiamond = MkKeySymbol #{const XKB_KEY_soliddiamond} 
-- #define XKB_KEY_checkerboard                  0x09e1  /* U+2592 MEDIUM SHADE */
pattern XKB_KEY_checkerboard :: KeySymbol
pattern XKB_KEY_checkerboard = MkKeySymbol #{const XKB_KEY_checkerboard} 
-- #define XKB_KEY_ht                            0x09e2  /* U+2409 SYMBOL FOR HORIZONTAL TABULATION */
pattern XKB_KEY_ht :: KeySymbol
pattern XKB_KEY_ht = MkKeySymbol #{const XKB_KEY_ht} 
-- #define XKB_KEY_ff                            0x09e3  /* U+240C SYMBOL FOR FORM FEED */
pattern XKB_KEY_ff :: KeySymbol
pattern XKB_KEY_ff = MkKeySymbol #{const XKB_KEY_ff} 
-- #define XKB_KEY_cr                            0x09e4  /* U+240D SYMBOL FOR CARRIAGE RETURN */
pattern XKB_KEY_cr :: KeySymbol
pattern XKB_KEY_cr = MkKeySymbol #{const XKB_KEY_cr} 
-- #define XKB_KEY_lf                            0x09e5  /* U+240A SYMBOL FOR LINE FEED */
pattern XKB_KEY_lf :: KeySymbol
pattern XKB_KEY_lf = MkKeySymbol #{const XKB_KEY_lf} 
-- #define XKB_KEY_nl                            0x09e8  /* U+2424 SYMBOL FOR NEWLINE */
pattern XKB_KEY_nl :: KeySymbol
pattern XKB_KEY_nl = MkKeySymbol #{const XKB_KEY_nl} 
-- #define XKB_KEY_vt                            0x09e9  /* U+240B SYMBOL FOR VERTICAL TABULATION */
pattern XKB_KEY_vt :: KeySymbol
pattern XKB_KEY_vt = MkKeySymbol #{const XKB_KEY_vt} 
-- #define XKB_KEY_lowrightcorner                0x09ea  /* U+2518 BOX DRAWINGS LIGHT UP AND LEFT */
pattern XKB_KEY_lowrightcorner :: KeySymbol
pattern XKB_KEY_lowrightcorner = MkKeySymbol #{const XKB_KEY_lowrightcorner} 
-- #define XKB_KEY_uprightcorner                 0x09eb  /* U+2510 BOX DRAWINGS LIGHT DOWN AND LEFT */
pattern XKB_KEY_uprightcorner :: KeySymbol
pattern XKB_KEY_uprightcorner = MkKeySymbol #{const XKB_KEY_uprightcorner} 
-- #define XKB_KEY_upleftcorner                  0x09ec  /* U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT */
pattern XKB_KEY_upleftcorner :: KeySymbol
pattern XKB_KEY_upleftcorner = MkKeySymbol #{const XKB_KEY_upleftcorner} 
-- #define XKB_KEY_lowleftcorner                 0x09ed  /* U+2514 BOX DRAWINGS LIGHT UP AND RIGHT */
pattern XKB_KEY_lowleftcorner :: KeySymbol
pattern XKB_KEY_lowleftcorner = MkKeySymbol #{const XKB_KEY_lowleftcorner} 
-- #define XKB_KEY_crossinglines                 0x09ee  /* U+253C BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL */
pattern XKB_KEY_crossinglines :: KeySymbol
pattern XKB_KEY_crossinglines = MkKeySymbol #{const XKB_KEY_crossinglines} 
-- #define XKB_KEY_horizlinescan1                0x09ef  /* U+23BA HORIZONTAL SCAN LINE-1 */
pattern XKB_KEY_horizlinescan1 :: KeySymbol
pattern XKB_KEY_horizlinescan1 = MkKeySymbol #{const XKB_KEY_horizlinescan1} 
-- #define XKB_KEY_horizlinescan3                0x09f0  /* U+23BB HORIZONTAL SCAN LINE-3 */
pattern XKB_KEY_horizlinescan3 :: KeySymbol
pattern XKB_KEY_horizlinescan3 = MkKeySymbol #{const XKB_KEY_horizlinescan3} 
-- #define XKB_KEY_horizlinescan5                0x09f1  /* U+2500 BOX DRAWINGS LIGHT HORIZONTAL */
pattern XKB_KEY_horizlinescan5 :: KeySymbol
pattern XKB_KEY_horizlinescan5 = MkKeySymbol #{const XKB_KEY_horizlinescan5} 
-- #define XKB_KEY_horizlinescan7                0x09f2  /* U+23BC HORIZONTAL SCAN LINE-7 */
pattern XKB_KEY_horizlinescan7 :: KeySymbol
pattern XKB_KEY_horizlinescan7 = MkKeySymbol #{const XKB_KEY_horizlinescan7} 
-- #define XKB_KEY_horizlinescan9                0x09f3  /* U+23BD HORIZONTAL SCAN LINE-9 */
pattern XKB_KEY_horizlinescan9 :: KeySymbol
pattern XKB_KEY_horizlinescan9 = MkKeySymbol #{const XKB_KEY_horizlinescan9} 
-- #define XKB_KEY_leftt                         0x09f4  /* U+251C BOX DRAWINGS LIGHT VERTICAL AND RIGHT */
pattern XKB_KEY_leftt :: KeySymbol
pattern XKB_KEY_leftt = MkKeySymbol #{const XKB_KEY_leftt} 
-- #define XKB_KEY_rightt                        0x09f5  /* U+2524 BOX DRAWINGS LIGHT VERTICAL AND LEFT */
pattern XKB_KEY_rightt :: KeySymbol
pattern XKB_KEY_rightt = MkKeySymbol #{const XKB_KEY_rightt} 
-- #define XKB_KEY_bott                          0x09f6  /* U+2534 BOX DRAWINGS LIGHT UP AND HORIZONTAL */
pattern XKB_KEY_bott :: KeySymbol
pattern XKB_KEY_bott = MkKeySymbol #{const XKB_KEY_bott} 
-- #define XKB_KEY_topt                          0x09f7  /* U+252C BOX DRAWINGS LIGHT DOWN AND HORIZONTAL */
pattern XKB_KEY_topt :: KeySymbol
pattern XKB_KEY_topt = MkKeySymbol #{const XKB_KEY_topt} 
-- #define XKB_KEY_vertbar                       0x09f8  /* U+2502 BOX DRAWINGS LIGHT VERTICAL */
pattern XKB_KEY_vertbar :: KeySymbol
pattern XKB_KEY_vertbar = MkKeySymbol #{const XKB_KEY_vertbar} 
-- #define XKB_KEY_emspace                       0x0aa1  /* U+2003 EM SPACE */
pattern XKB_KEY_emspace :: KeySymbol
pattern XKB_KEY_emspace = MkKeySymbol #{const XKB_KEY_emspace} 
-- #define XKB_KEY_enspace                       0x0aa2  /* U+2002 EN SPACE */
pattern XKB_KEY_enspace :: KeySymbol
pattern XKB_KEY_enspace = MkKeySymbol #{const XKB_KEY_enspace} 
-- #define XKB_KEY_em3space                      0x0aa3  /* U+2004 THREE-PER-EM SPACE */
pattern XKB_KEY_em3space :: KeySymbol
pattern XKB_KEY_em3space = MkKeySymbol #{const XKB_KEY_em3space} 
-- #define XKB_KEY_em4space                      0x0aa4  /* U+2005 FOUR-PER-EM SPACE */
pattern XKB_KEY_em4space :: KeySymbol
pattern XKB_KEY_em4space = MkKeySymbol #{const XKB_KEY_em4space} 
-- #define XKB_KEY_digitspace                    0x0aa5  /* U+2007 FIGURE SPACE */
pattern XKB_KEY_digitspace :: KeySymbol
pattern XKB_KEY_digitspace = MkKeySymbol #{const XKB_KEY_digitspace} 
-- #define XKB_KEY_punctspace                    0x0aa6  /* U+2008 PUNCTUATION SPACE */
pattern XKB_KEY_punctspace :: KeySymbol
pattern XKB_KEY_punctspace = MkKeySymbol #{const XKB_KEY_punctspace} 
-- #define XKB_KEY_thinspace                     0x0aa7  /* U+2009 THIN SPACE */
pattern XKB_KEY_thinspace :: KeySymbol
pattern XKB_KEY_thinspace = MkKeySymbol #{const XKB_KEY_thinspace} 
-- #define XKB_KEY_hairspace                     0x0aa8  /* U+200A HAIR SPACE */
pattern XKB_KEY_hairspace :: KeySymbol
pattern XKB_KEY_hairspace = MkKeySymbol #{const XKB_KEY_hairspace} 
-- #define XKB_KEY_emdash                        0x0aa9  /* U+2014 EM DASH */
pattern XKB_KEY_emdash :: KeySymbol
pattern XKB_KEY_emdash = MkKeySymbol #{const XKB_KEY_emdash} 
-- #define XKB_KEY_endash                        0x0aaa  /* U+2013 EN DASH */
pattern XKB_KEY_endash :: KeySymbol
pattern XKB_KEY_endash = MkKeySymbol #{const XKB_KEY_endash} 
-- #define XKB_KEY_signifblank                   0x0aac  /*(U+2423 OPEN BOX)*/
pattern XKB_KEY_signifblank :: KeySymbol
pattern XKB_KEY_signifblank = MkKeySymbol #{const XKB_KEY_signifblank} 
-- #define XKB_KEY_ellipsis                      0x0aae  /* U+2026 HORIZONTAL ELLIPSIS */
pattern XKB_KEY_ellipsis :: KeySymbol
pattern XKB_KEY_ellipsis = MkKeySymbol #{const XKB_KEY_ellipsis} 
-- #define XKB_KEY_doubbaselinedot               0x0aaf  /* U+2025 TWO DOT LEADER */
pattern XKB_KEY_doubbaselinedot :: KeySymbol
pattern XKB_KEY_doubbaselinedot = MkKeySymbol #{const XKB_KEY_doubbaselinedot} 
-- #define XKB_KEY_onethird                      0x0ab0  /* U+2153 VULGAR FRACTION ONE THIRD */
pattern XKB_KEY_onethird :: KeySymbol
pattern XKB_KEY_onethird = MkKeySymbol #{const XKB_KEY_onethird} 
-- #define XKB_KEY_twothirds                     0x0ab1  /* U+2154 VULGAR FRACTION TWO THIRDS */
pattern XKB_KEY_twothirds :: KeySymbol
pattern XKB_KEY_twothirds = MkKeySymbol #{const XKB_KEY_twothirds} 
-- #define XKB_KEY_onefifth                      0x0ab2  /* U+2155 VULGAR FRACTION ONE FIFTH */
pattern XKB_KEY_onefifth :: KeySymbol
pattern XKB_KEY_onefifth = MkKeySymbol #{const XKB_KEY_onefifth} 
-- #define XKB_KEY_twofifths                     0x0ab3  /* U+2156 VULGAR FRACTION TWO FIFTHS */
pattern XKB_KEY_twofifths :: KeySymbol
pattern XKB_KEY_twofifths = MkKeySymbol #{const XKB_KEY_twofifths} 
-- #define XKB_KEY_threefifths                   0x0ab4  /* U+2157 VULGAR FRACTION THREE FIFTHS */
pattern XKB_KEY_threefifths :: KeySymbol
pattern XKB_KEY_threefifths = MkKeySymbol #{const XKB_KEY_threefifths} 
-- #define XKB_KEY_fourfifths                    0x0ab5  /* U+2158 VULGAR FRACTION FOUR FIFTHS */
pattern XKB_KEY_fourfifths :: KeySymbol
pattern XKB_KEY_fourfifths = MkKeySymbol #{const XKB_KEY_fourfifths} 
-- #define XKB_KEY_onesixth                      0x0ab6  /* U+2159 VULGAR FRACTION ONE SIXTH */
pattern XKB_KEY_onesixth :: KeySymbol
pattern XKB_KEY_onesixth = MkKeySymbol #{const XKB_KEY_onesixth} 
-- #define XKB_KEY_fivesixths                    0x0ab7  /* U+215A VULGAR FRACTION FIVE SIXTHS */
pattern XKB_KEY_fivesixths :: KeySymbol
pattern XKB_KEY_fivesixths = MkKeySymbol #{const XKB_KEY_fivesixths} 
-- #define XKB_KEY_careof                        0x0ab8  /* U+2105 CARE OF */
pattern XKB_KEY_careof :: KeySymbol
pattern XKB_KEY_careof = MkKeySymbol #{const XKB_KEY_careof} 
-- #define XKB_KEY_figdash                       0x0abb  /* U+2012 FIGURE DASH */
pattern XKB_KEY_figdash :: KeySymbol
pattern XKB_KEY_figdash = MkKeySymbol #{const XKB_KEY_figdash} 
-- #define XKB_KEY_leftanglebracket              0x0abc  /*(U+27E8 MATHEMATICAL LEFT ANGLE BRACKET)*/
pattern XKB_KEY_leftanglebracket :: KeySymbol
pattern XKB_KEY_leftanglebracket = MkKeySymbol #{const XKB_KEY_leftanglebracket} 
-- #define XKB_KEY_decimalpoint                  0x0abd  /*(U+002E FULL STOP)*/
pattern XKB_KEY_decimalpoint :: KeySymbol
pattern XKB_KEY_decimalpoint = MkKeySymbol #{const XKB_KEY_decimalpoint} 
-- #define XKB_KEY_rightanglebracket             0x0abe  /*(U+27E9 MATHEMATICAL RIGHT ANGLE BRACKET)*/
pattern XKB_KEY_rightanglebracket :: KeySymbol
pattern XKB_KEY_rightanglebracket = MkKeySymbol #{const XKB_KEY_rightanglebracket} 
-- #define XKB_KEY_marker                        0x0abf
pattern XKB_KEY_marker :: KeySymbol
pattern XKB_KEY_marker = MkKeySymbol #{const XKB_KEY_marker} 
-- #define XKB_KEY_oneeighth                     0x0ac3  /* U+215B VULGAR FRACTION ONE EIGHTH */
pattern XKB_KEY_oneeighth :: KeySymbol
pattern XKB_KEY_oneeighth = MkKeySymbol #{const XKB_KEY_oneeighth} 
-- #define XKB_KEY_threeeighths                  0x0ac4  /* U+215C VULGAR FRACTION THREE EIGHTHS */
pattern XKB_KEY_threeeighths :: KeySymbol
pattern XKB_KEY_threeeighths = MkKeySymbol #{const XKB_KEY_threeeighths} 
-- #define XKB_KEY_fiveeighths                   0x0ac5  /* U+215D VULGAR FRACTION FIVE EIGHTHS */
pattern XKB_KEY_fiveeighths :: KeySymbol
pattern XKB_KEY_fiveeighths = MkKeySymbol #{const XKB_KEY_fiveeighths} 
-- #define XKB_KEY_seveneighths                  0x0ac6  /* U+215E VULGAR FRACTION SEVEN EIGHTHS */
pattern XKB_KEY_seveneighths :: KeySymbol
pattern XKB_KEY_seveneighths = MkKeySymbol #{const XKB_KEY_seveneighths} 
-- #define XKB_KEY_trademark                     0x0ac9  /* U+2122 TRADE MARK SIGN */
pattern XKB_KEY_trademark :: KeySymbol
pattern XKB_KEY_trademark = MkKeySymbol #{const XKB_KEY_trademark} 
-- #define XKB_KEY_signaturemark                 0x0aca  /*(U+2613 SALTIRE)*/
pattern XKB_KEY_signaturemark :: KeySymbol
pattern XKB_KEY_signaturemark = MkKeySymbol #{const XKB_KEY_signaturemark} 
-- #define XKB_KEY_trademarkincircle             0x0acb
pattern XKB_KEY_trademarkincircle :: KeySymbol
pattern XKB_KEY_trademarkincircle = MkKeySymbol #{const XKB_KEY_trademarkincircle} 
-- #define XKB_KEY_leftopentriangle              0x0acc  /*(U+25C1 WHITE LEFT-POINTING TRIANGLE)*/
pattern XKB_KEY_leftopentriangle :: KeySymbol
pattern XKB_KEY_leftopentriangle = MkKeySymbol #{const XKB_KEY_leftopentriangle} 
-- #define XKB_KEY_rightopentriangle             0x0acd  /*(U+25B7 WHITE RIGHT-POINTING TRIANGLE)*/
pattern XKB_KEY_rightopentriangle :: KeySymbol
pattern XKB_KEY_rightopentriangle = MkKeySymbol #{const XKB_KEY_rightopentriangle} 
-- #define XKB_KEY_emopencircle                  0x0ace  /*(U+25CB WHITE CIRCLE)*/
pattern XKB_KEY_emopencircle :: KeySymbol
pattern XKB_KEY_emopencircle = MkKeySymbol #{const XKB_KEY_emopencircle} 
-- #define XKB_KEY_emopenrectangle               0x0acf  /*(U+25AF WHITE VERTICAL RECTANGLE)*/
pattern XKB_KEY_emopenrectangle :: KeySymbol
pattern XKB_KEY_emopenrectangle = MkKeySymbol #{const XKB_KEY_emopenrectangle} 
-- #define XKB_KEY_leftsinglequotemark           0x0ad0  /* U+2018 LEFT SINGLE QUOTATION MARK */
pattern XKB_KEY_leftsinglequotemark :: KeySymbol
pattern XKB_KEY_leftsinglequotemark = MkKeySymbol #{const XKB_KEY_leftsinglequotemark} 
-- #define XKB_KEY_rightsinglequotemark          0x0ad1  /* U+2019 RIGHT SINGLE QUOTATION MARK */
pattern XKB_KEY_rightsinglequotemark :: KeySymbol
pattern XKB_KEY_rightsinglequotemark = MkKeySymbol #{const XKB_KEY_rightsinglequotemark} 
-- #define XKB_KEY_leftdoublequotemark           0x0ad2  /* U+201C LEFT DOUBLE QUOTATION MARK */
pattern XKB_KEY_leftdoublequotemark :: KeySymbol
pattern XKB_KEY_leftdoublequotemark = MkKeySymbol #{const XKB_KEY_leftdoublequotemark} 
-- #define XKB_KEY_rightdoublequotemark          0x0ad3  /* U+201D RIGHT DOUBLE QUOTATION MARK */
pattern XKB_KEY_rightdoublequotemark :: KeySymbol
pattern XKB_KEY_rightdoublequotemark = MkKeySymbol #{const XKB_KEY_rightdoublequotemark} 
-- #define XKB_KEY_prescription                  0x0ad4  /* U+211E PRESCRIPTION TAKE */
pattern XKB_KEY_prescription :: KeySymbol
pattern XKB_KEY_prescription = MkKeySymbol #{const XKB_KEY_prescription} 
-- #define XKB_KEY_permille                      0x0ad5  /* U+2030 PER MILLE SIGN */
pattern XKB_KEY_permille :: KeySymbol
pattern XKB_KEY_permille = MkKeySymbol #{const XKB_KEY_permille} 
-- #define XKB_KEY_minutes                       0x0ad6  /* U+2032 PRIME */
pattern XKB_KEY_minutes :: KeySymbol
pattern XKB_KEY_minutes = MkKeySymbol #{const XKB_KEY_minutes} 
-- #define XKB_KEY_seconds                       0x0ad7  /* U+2033 DOUBLE PRIME */
pattern XKB_KEY_seconds :: KeySymbol
pattern XKB_KEY_seconds = MkKeySymbol #{const XKB_KEY_seconds} 
-- #define XKB_KEY_latincross                    0x0ad9  /* U+271D LATIN CROSS */
pattern XKB_KEY_latincross :: KeySymbol
pattern XKB_KEY_latincross = MkKeySymbol #{const XKB_KEY_latincross} 
-- #define XKB_KEY_hexagram                      0x0ada
pattern XKB_KEY_hexagram :: KeySymbol
pattern XKB_KEY_hexagram = MkKeySymbol #{const XKB_KEY_hexagram} 
-- #define XKB_KEY_filledrectbullet              0x0adb  /*(U+25AC BLACK RECTANGLE)*/
pattern XKB_KEY_filledrectbullet :: KeySymbol
pattern XKB_KEY_filledrectbullet = MkKeySymbol #{const XKB_KEY_filledrectbullet} 
-- #define XKB_KEY_filledlefttribullet           0x0adc  /*(U+25C0 BLACK LEFT-POINTING TRIANGLE)*/
pattern XKB_KEY_filledlefttribullet :: KeySymbol
pattern XKB_KEY_filledlefttribullet = MkKeySymbol #{const XKB_KEY_filledlefttribullet} 
-- #define XKB_KEY_filledrighttribullet          0x0add  /*(U+25B6 BLACK RIGHT-POINTING TRIANGLE)*/
pattern XKB_KEY_filledrighttribullet :: KeySymbol
pattern XKB_KEY_filledrighttribullet = MkKeySymbol #{const XKB_KEY_filledrighttribullet} 
-- #define XKB_KEY_emfilledcircle                0x0ade  /*(U+25CF BLACK CIRCLE)*/
pattern XKB_KEY_emfilledcircle :: KeySymbol
pattern XKB_KEY_emfilledcircle = MkKeySymbol #{const XKB_KEY_emfilledcircle} 
-- #define XKB_KEY_emfilledrect                  0x0adf  /*(U+25AE BLACK VERTICAL RECTANGLE)*/
pattern XKB_KEY_emfilledrect :: KeySymbol
pattern XKB_KEY_emfilledrect = MkKeySymbol #{const XKB_KEY_emfilledrect} 
-- #define XKB_KEY_enopencircbullet              0x0ae0  /*(U+25E6 WHITE BULLET)*/
pattern XKB_KEY_enopencircbullet :: KeySymbol
pattern XKB_KEY_enopencircbullet = MkKeySymbol #{const XKB_KEY_enopencircbullet} 
-- #define XKB_KEY_enopensquarebullet            0x0ae1  /*(U+25AB WHITE SMALL SQUARE)*/
pattern XKB_KEY_enopensquarebullet :: KeySymbol
pattern XKB_KEY_enopensquarebullet = MkKeySymbol #{const XKB_KEY_enopensquarebullet} 
-- #define XKB_KEY_openrectbullet                0x0ae2  /*(U+25AD WHITE RECTANGLE)*/
pattern XKB_KEY_openrectbullet :: KeySymbol
pattern XKB_KEY_openrectbullet = MkKeySymbol #{const XKB_KEY_openrectbullet} 
-- #define XKB_KEY_opentribulletup               0x0ae3  /*(U+25B3 WHITE UP-POINTING TRIANGLE)*/
pattern XKB_KEY_opentribulletup :: KeySymbol
pattern XKB_KEY_opentribulletup = MkKeySymbol #{const XKB_KEY_opentribulletup} 
-- #define XKB_KEY_opentribulletdown             0x0ae4  /*(U+25BD WHITE DOWN-POINTING TRIANGLE)*/
pattern XKB_KEY_opentribulletdown :: KeySymbol
pattern XKB_KEY_opentribulletdown = MkKeySymbol #{const XKB_KEY_opentribulletdown} 
-- #define XKB_KEY_openstar                      0x0ae5  /*(U+2606 WHITE STAR)*/
pattern XKB_KEY_openstar :: KeySymbol
pattern XKB_KEY_openstar = MkKeySymbol #{const XKB_KEY_openstar} 
-- #define XKB_KEY_enfilledcircbullet            0x0ae6  /*(U+2022 BULLET)*/
pattern XKB_KEY_enfilledcircbullet :: KeySymbol
pattern XKB_KEY_enfilledcircbullet = MkKeySymbol #{const XKB_KEY_enfilledcircbullet} 
-- #define XKB_KEY_enfilledsqbullet              0x0ae7  /*(U+25AA BLACK SMALL SQUARE)*/
pattern XKB_KEY_enfilledsqbullet :: KeySymbol
pattern XKB_KEY_enfilledsqbullet = MkKeySymbol #{const XKB_KEY_enfilledsqbullet} 
-- #define XKB_KEY_filledtribulletup             0x0ae8  /*(U+25B2 BLACK UP-POINTING TRIANGLE)*/
pattern XKB_KEY_filledtribulletup :: KeySymbol
pattern XKB_KEY_filledtribulletup = MkKeySymbol #{const XKB_KEY_filledtribulletup} 
-- #define XKB_KEY_filledtribulletdown           0x0ae9  /*(U+25BC BLACK DOWN-POINTING TRIANGLE)*/
pattern XKB_KEY_filledtribulletdown :: KeySymbol
pattern XKB_KEY_filledtribulletdown = MkKeySymbol #{const XKB_KEY_filledtribulletdown} 
-- #define XKB_KEY_leftpointer                   0x0aea  /*(U+261C WHITE LEFT POINTING INDEX)*/
pattern XKB_KEY_leftpointer :: KeySymbol
pattern XKB_KEY_leftpointer = MkKeySymbol #{const XKB_KEY_leftpointer} 
-- #define XKB_KEY_rightpointer                  0x0aeb  /*(U+261E WHITE RIGHT POINTING INDEX)*/
pattern XKB_KEY_rightpointer :: KeySymbol
pattern XKB_KEY_rightpointer = MkKeySymbol #{const XKB_KEY_rightpointer} 
-- #define XKB_KEY_club                          0x0aec  /* U+2663 BLACK CLUB SUIT */
pattern XKB_KEY_club :: KeySymbol
pattern XKB_KEY_club = MkKeySymbol #{const XKB_KEY_club} 
-- #define XKB_KEY_diamond                       0x0aed  /* U+2666 BLACK DIAMOND SUIT */
pattern XKB_KEY_diamond :: KeySymbol
pattern XKB_KEY_diamond = MkKeySymbol #{const XKB_KEY_diamond} 
-- #define XKB_KEY_heart                         0x0aee  /* U+2665 BLACK HEART SUIT */
pattern XKB_KEY_heart :: KeySymbol
pattern XKB_KEY_heart = MkKeySymbol #{const XKB_KEY_heart} 
-- #define XKB_KEY_maltesecross                  0x0af0  /* U+2720 MALTESE CROSS */
pattern XKB_KEY_maltesecross :: KeySymbol
pattern XKB_KEY_maltesecross = MkKeySymbol #{const XKB_KEY_maltesecross} 
-- #define XKB_KEY_dagger                        0x0af1  /* U+2020 DAGGER */
pattern XKB_KEY_dagger :: KeySymbol
pattern XKB_KEY_dagger = MkKeySymbol #{const XKB_KEY_dagger} 
-- #define XKB_KEY_doubledagger                  0x0af2  /* U+2021 DOUBLE DAGGER */
pattern XKB_KEY_doubledagger :: KeySymbol
pattern XKB_KEY_doubledagger = MkKeySymbol #{const XKB_KEY_doubledagger} 
-- #define XKB_KEY_checkmark                     0x0af3  /* U+2713 CHECK MARK */
pattern XKB_KEY_checkmark :: KeySymbol
pattern XKB_KEY_checkmark = MkKeySymbol #{const XKB_KEY_checkmark} 
-- #define XKB_KEY_ballotcross                   0x0af4  /* U+2717 BALLOT X */
pattern XKB_KEY_ballotcross :: KeySymbol
pattern XKB_KEY_ballotcross = MkKeySymbol #{const XKB_KEY_ballotcross} 
-- #define XKB_KEY_musicalsharp                  0x0af5  /* U+266F MUSIC SHARP SIGN */
pattern XKB_KEY_musicalsharp :: KeySymbol
pattern XKB_KEY_musicalsharp = MkKeySymbol #{const XKB_KEY_musicalsharp} 
-- #define XKB_KEY_musicalflat                   0x0af6  /* U+266D MUSIC FLAT SIGN */
pattern XKB_KEY_musicalflat :: KeySymbol
pattern XKB_KEY_musicalflat = MkKeySymbol #{const XKB_KEY_musicalflat} 
-- #define XKB_KEY_malesymbol                    0x0af7  /* U+2642 MALE SIGN */
pattern XKB_KEY_malesymbol :: KeySymbol
pattern XKB_KEY_malesymbol = MkKeySymbol #{const XKB_KEY_malesymbol} 
-- #define XKB_KEY_femalesymbol                  0x0af8  /* U+2640 FEMALE SIGN */
pattern XKB_KEY_femalesymbol :: KeySymbol
pattern XKB_KEY_femalesymbol = MkKeySymbol #{const XKB_KEY_femalesymbol} 
-- #define XKB_KEY_telephone                     0x0af9  /* U+260E BLACK TELEPHONE */
pattern XKB_KEY_telephone :: KeySymbol
pattern XKB_KEY_telephone = MkKeySymbol #{const XKB_KEY_telephone} 
-- #define XKB_KEY_telephonerecorder             0x0afa  /* U+2315 TELEPHONE RECORDER */
pattern XKB_KEY_telephonerecorder :: KeySymbol
pattern XKB_KEY_telephonerecorder = MkKeySymbol #{const XKB_KEY_telephonerecorder} 
-- #define XKB_KEY_phonographcopyright           0x0afb  /* U+2117 SOUND RECORDING COPYRIGHT */
pattern XKB_KEY_phonographcopyright :: KeySymbol
pattern XKB_KEY_phonographcopyright = MkKeySymbol #{const XKB_KEY_phonographcopyright} 
-- #define XKB_KEY_caret                         0x0afc  /* U+2038 CARET */
pattern XKB_KEY_caret :: KeySymbol
pattern XKB_KEY_caret = MkKeySymbol #{const XKB_KEY_caret} 
-- #define XKB_KEY_singlelowquotemark            0x0afd  /* U+201A SINGLE LOW-9 QUOTATION MARK */
pattern XKB_KEY_singlelowquotemark :: KeySymbol
pattern XKB_KEY_singlelowquotemark = MkKeySymbol #{const XKB_KEY_singlelowquotemark} 
-- #define XKB_KEY_doublelowquotemark            0x0afe  /* U+201E DOUBLE LOW-9 QUOTATION MARK */
pattern XKB_KEY_doublelowquotemark :: KeySymbol
pattern XKB_KEY_doublelowquotemark = MkKeySymbol #{const XKB_KEY_doublelowquotemark} 
-- #define XKB_KEY_cursor                        0x0aff
pattern XKB_KEY_cursor :: KeySymbol
pattern XKB_KEY_cursor = MkKeySymbol #{const XKB_KEY_cursor} 
-- #define XKB_KEY_leftcaret                     0x0ba3  /*(U+003C LESS-THAN SIGN)*/
pattern XKB_KEY_leftcaret :: KeySymbol
pattern XKB_KEY_leftcaret = MkKeySymbol #{const XKB_KEY_leftcaret} 
-- #define XKB_KEY_rightcaret                    0x0ba6  /*(U+003E GREATER-THAN SIGN)*/
pattern XKB_KEY_rightcaret :: KeySymbol
pattern XKB_KEY_rightcaret = MkKeySymbol #{const XKB_KEY_rightcaret} 
-- #define XKB_KEY_downcaret                     0x0ba8  /*(U+2228 LOGICAL OR)*/
pattern XKB_KEY_downcaret :: KeySymbol
pattern XKB_KEY_downcaret = MkKeySymbol #{const XKB_KEY_downcaret} 
-- #define XKB_KEY_upcaret                       0x0ba9  /*(U+2227 LOGICAL AND)*/
pattern XKB_KEY_upcaret :: KeySymbol
pattern XKB_KEY_upcaret = MkKeySymbol #{const XKB_KEY_upcaret} 
-- #define XKB_KEY_overbar                       0x0bc0  /*(U+00AF MACRON)*/
pattern XKB_KEY_overbar :: KeySymbol
pattern XKB_KEY_overbar = MkKeySymbol #{const XKB_KEY_overbar} 
-- #define XKB_KEY_downtack                      0x0bc2  /* U+22A4 DOWN TACK */
pattern XKB_KEY_downtack :: KeySymbol
pattern XKB_KEY_downtack = MkKeySymbol #{const XKB_KEY_downtack} 
-- #define XKB_KEY_upshoe                        0x0bc3  /*(U+2229 INTERSECTION)*/
pattern XKB_KEY_upshoe :: KeySymbol
pattern XKB_KEY_upshoe = MkKeySymbol #{const XKB_KEY_upshoe} 
-- #define XKB_KEY_downstile                     0x0bc4  /* U+230A LEFT FLOOR */
pattern XKB_KEY_downstile :: KeySymbol
pattern XKB_KEY_downstile = MkKeySymbol #{const XKB_KEY_downstile} 
-- #define XKB_KEY_underbar                      0x0bc6  /*(U+005F LOW LINE)*/
pattern XKB_KEY_underbar :: KeySymbol
pattern XKB_KEY_underbar = MkKeySymbol #{const XKB_KEY_underbar} 
-- #define XKB_KEY_jot                           0x0bca  /* U+2218 RING OPERATOR */
pattern XKB_KEY_jot :: KeySymbol
pattern XKB_KEY_jot = MkKeySymbol #{const XKB_KEY_jot} 
-- #define XKB_KEY_quad                          0x0bcc  /* U+2395 APL FUNCTIONAL SYMBOL QUAD */
pattern XKB_KEY_quad :: KeySymbol
pattern XKB_KEY_quad = MkKeySymbol #{const XKB_KEY_quad} 
-- #define XKB_KEY_uptack                        0x0bce  /* U+22A5 UP TACK */
pattern XKB_KEY_uptack :: KeySymbol
pattern XKB_KEY_uptack = MkKeySymbol #{const XKB_KEY_uptack} 
-- #define XKB_KEY_circle                        0x0bcf  /* U+25CB WHITE CIRCLE */
pattern XKB_KEY_circle :: KeySymbol
pattern XKB_KEY_circle = MkKeySymbol #{const XKB_KEY_circle} 
-- #define XKB_KEY_upstile                       0x0bd3  /* U+2308 LEFT CEILING */
pattern XKB_KEY_upstile :: KeySymbol
pattern XKB_KEY_upstile = MkKeySymbol #{const XKB_KEY_upstile} 
-- #define XKB_KEY_downshoe                      0x0bd6  /*(U+222A UNION)*/
pattern XKB_KEY_downshoe :: KeySymbol
pattern XKB_KEY_downshoe = MkKeySymbol #{const XKB_KEY_downshoe} 
-- #define XKB_KEY_rightshoe                     0x0bd8  /*(U+2283 SUPERSET OF)*/
pattern XKB_KEY_rightshoe :: KeySymbol
pattern XKB_KEY_rightshoe = MkKeySymbol #{const XKB_KEY_rightshoe} 
-- #define XKB_KEY_leftshoe                      0x0bda  /*(U+2282 SUBSET OF)*/
pattern XKB_KEY_leftshoe :: KeySymbol
pattern XKB_KEY_leftshoe = MkKeySymbol #{const XKB_KEY_leftshoe} 
-- #define XKB_KEY_lefttack                      0x0bdc  /* U+22A3 LEFT TACK */
pattern XKB_KEY_lefttack :: KeySymbol
pattern XKB_KEY_lefttack = MkKeySymbol #{const XKB_KEY_lefttack} 
-- #define XKB_KEY_righttack                     0x0bfc  /* U+22A2 RIGHT TACK */
pattern XKB_KEY_righttack :: KeySymbol
pattern XKB_KEY_righttack = MkKeySymbol #{const XKB_KEY_righttack} 
-- #define XKB_KEY_hebrew_doublelowline          0x0cdf  /* U+2017 DOUBLE LOW LINE */
pattern XKB_KEY_hebrew_doublelowline :: KeySymbol
pattern XKB_KEY_hebrew_doublelowline = MkKeySymbol #{const XKB_KEY_hebrew_doublelowline} 
-- #define XKB_KEY_hebrew_aleph                  0x0ce0  /* U+05D0 HEBREW LETTER ALEF */
pattern XKB_KEY_hebrew_aleph :: KeySymbol
pattern XKB_KEY_hebrew_aleph = MkKeySymbol #{const XKB_KEY_hebrew_aleph} 
-- #define XKB_KEY_hebrew_bet                    0x0ce1  /* U+05D1 HEBREW LETTER BET */
pattern XKB_KEY_hebrew_bet :: KeySymbol
pattern XKB_KEY_hebrew_bet = MkKeySymbol #{const XKB_KEY_hebrew_bet} 
-- #define XKB_KEY_hebrew_beth                   0x0ce1  /* deprecated */
pattern XKB_KEY_hebrew_beth :: KeySymbol
pattern XKB_KEY_hebrew_beth = MkKeySymbol #{const XKB_KEY_hebrew_beth} 
-- #define XKB_KEY_hebrew_gimel                  0x0ce2  /* U+05D2 HEBREW LETTER GIMEL */
pattern XKB_KEY_hebrew_gimel :: KeySymbol
pattern XKB_KEY_hebrew_gimel = MkKeySymbol #{const XKB_KEY_hebrew_gimel} 
-- #define XKB_KEY_hebrew_gimmel                 0x0ce2  /* deprecated */
pattern XKB_KEY_hebrew_gimmel :: KeySymbol
pattern XKB_KEY_hebrew_gimmel = MkKeySymbol #{const XKB_KEY_hebrew_gimmel} 
-- #define XKB_KEY_hebrew_dalet                  0x0ce3  /* U+05D3 HEBREW LETTER DALET */
pattern XKB_KEY_hebrew_dalet :: KeySymbol
pattern XKB_KEY_hebrew_dalet = MkKeySymbol #{const XKB_KEY_hebrew_dalet} 
-- #define XKB_KEY_hebrew_daleth                 0x0ce3  /* deprecated */
pattern XKB_KEY_hebrew_daleth :: KeySymbol
pattern XKB_KEY_hebrew_daleth = MkKeySymbol #{const XKB_KEY_hebrew_daleth} 
-- #define XKB_KEY_hebrew_he                     0x0ce4  /* U+05D4 HEBREW LETTER HE */
pattern XKB_KEY_hebrew_he :: KeySymbol
pattern XKB_KEY_hebrew_he = MkKeySymbol #{const XKB_KEY_hebrew_he} 
-- #define XKB_KEY_hebrew_waw                    0x0ce5  /* U+05D5 HEBREW LETTER VAV */
pattern XKB_KEY_hebrew_waw :: KeySymbol
pattern XKB_KEY_hebrew_waw = MkKeySymbol #{const XKB_KEY_hebrew_waw} 
-- #define XKB_KEY_hebrew_zain                   0x0ce6  /* U+05D6 HEBREW LETTER ZAYIN */
pattern XKB_KEY_hebrew_zain :: KeySymbol
pattern XKB_KEY_hebrew_zain = MkKeySymbol #{const XKB_KEY_hebrew_zain} 
-- #define XKB_KEY_hebrew_zayin                  0x0ce6  /* deprecated */
pattern XKB_KEY_hebrew_zayin :: KeySymbol
pattern XKB_KEY_hebrew_zayin = MkKeySymbol #{const XKB_KEY_hebrew_zayin} 
-- #define XKB_KEY_hebrew_chet                   0x0ce7  /* U+05D7 HEBREW LETTER HET */
pattern XKB_KEY_hebrew_chet :: KeySymbol
pattern XKB_KEY_hebrew_chet = MkKeySymbol #{const XKB_KEY_hebrew_chet} 
-- #define XKB_KEY_hebrew_het                    0x0ce7  /* deprecated */
pattern XKB_KEY_hebrew_het :: KeySymbol
pattern XKB_KEY_hebrew_het = MkKeySymbol #{const XKB_KEY_hebrew_het} 
-- #define XKB_KEY_hebrew_tet                    0x0ce8  /* U+05D8 HEBREW LETTER TET */
pattern XKB_KEY_hebrew_tet :: KeySymbol
pattern XKB_KEY_hebrew_tet = MkKeySymbol #{const XKB_KEY_hebrew_tet} 
-- #define XKB_KEY_hebrew_teth                   0x0ce8  /* deprecated */
pattern XKB_KEY_hebrew_teth :: KeySymbol
pattern XKB_KEY_hebrew_teth = MkKeySymbol #{const XKB_KEY_hebrew_teth} 
-- #define XKB_KEY_hebrew_yod                    0x0ce9  /* U+05D9 HEBREW LETTER YOD */
pattern XKB_KEY_hebrew_yod :: KeySymbol
pattern XKB_KEY_hebrew_yod = MkKeySymbol #{const XKB_KEY_hebrew_yod} 
-- #define XKB_KEY_hebrew_finalkaph              0x0cea  /* U+05DA HEBREW LETTER FINAL KAF */
pattern XKB_KEY_hebrew_finalkaph :: KeySymbol
pattern XKB_KEY_hebrew_finalkaph = MkKeySymbol #{const XKB_KEY_hebrew_finalkaph} 
-- #define XKB_KEY_hebrew_kaph                   0x0ceb  /* U+05DB HEBREW LETTER KAF */
pattern XKB_KEY_hebrew_kaph :: KeySymbol
pattern XKB_KEY_hebrew_kaph = MkKeySymbol #{const XKB_KEY_hebrew_kaph} 
-- #define XKB_KEY_hebrew_lamed                  0x0cec  /* U+05DC HEBREW LETTER LAMED */
pattern XKB_KEY_hebrew_lamed :: KeySymbol
pattern XKB_KEY_hebrew_lamed = MkKeySymbol #{const XKB_KEY_hebrew_lamed} 
-- #define XKB_KEY_hebrew_finalmem               0x0ced  /* U+05DD HEBREW LETTER FINAL MEM */
pattern XKB_KEY_hebrew_finalmem :: KeySymbol
pattern XKB_KEY_hebrew_finalmem = MkKeySymbol #{const XKB_KEY_hebrew_finalmem} 
-- #define XKB_KEY_hebrew_mem                    0x0cee  /* U+05DE HEBREW LETTER MEM */
pattern XKB_KEY_hebrew_mem :: KeySymbol
pattern XKB_KEY_hebrew_mem = MkKeySymbol #{const XKB_KEY_hebrew_mem} 
-- #define XKB_KEY_hebrew_finalnun               0x0cef  /* U+05DF HEBREW LETTER FINAL NUN */
pattern XKB_KEY_hebrew_finalnun :: KeySymbol
pattern XKB_KEY_hebrew_finalnun = MkKeySymbol #{const XKB_KEY_hebrew_finalnun} 
-- #define XKB_KEY_hebrew_nun                    0x0cf0  /* U+05E0 HEBREW LETTER NUN */
pattern XKB_KEY_hebrew_nun :: KeySymbol
pattern XKB_KEY_hebrew_nun = MkKeySymbol #{const XKB_KEY_hebrew_nun} 
-- #define XKB_KEY_hebrew_samech                 0x0cf1  /* U+05E1 HEBREW LETTER SAMEKH */
pattern XKB_KEY_hebrew_samech :: KeySymbol
pattern XKB_KEY_hebrew_samech = MkKeySymbol #{const XKB_KEY_hebrew_samech} 
-- #define XKB_KEY_hebrew_samekh                 0x0cf1  /* deprecated */
pattern XKB_KEY_hebrew_samekh :: KeySymbol
pattern XKB_KEY_hebrew_samekh = MkKeySymbol #{const XKB_KEY_hebrew_samekh} 
-- #define XKB_KEY_hebrew_ayin                   0x0cf2  /* U+05E2 HEBREW LETTER AYIN */
pattern XKB_KEY_hebrew_ayin :: KeySymbol
pattern XKB_KEY_hebrew_ayin = MkKeySymbol #{const XKB_KEY_hebrew_ayin} 
-- #define XKB_KEY_hebrew_finalpe                0x0cf3  /* U+05E3 HEBREW LETTER FINAL PE */
pattern XKB_KEY_hebrew_finalpe :: KeySymbol
pattern XKB_KEY_hebrew_finalpe = MkKeySymbol #{const XKB_KEY_hebrew_finalpe} 
-- #define XKB_KEY_hebrew_pe                     0x0cf4  /* U+05E4 HEBREW LETTER PE */
pattern XKB_KEY_hebrew_pe :: KeySymbol
pattern XKB_KEY_hebrew_pe = MkKeySymbol #{const XKB_KEY_hebrew_pe} 
-- #define XKB_KEY_hebrew_finalzade              0x0cf5  /* U+05E5 HEBREW LETTER FINAL TSADI */
pattern XKB_KEY_hebrew_finalzade :: KeySymbol
pattern XKB_KEY_hebrew_finalzade = MkKeySymbol #{const XKB_KEY_hebrew_finalzade} 
-- #define XKB_KEY_hebrew_finalzadi              0x0cf5  /* deprecated */
pattern XKB_KEY_hebrew_finalzadi :: KeySymbol
pattern XKB_KEY_hebrew_finalzadi = MkKeySymbol #{const XKB_KEY_hebrew_finalzadi} 
-- #define XKB_KEY_hebrew_zade                   0x0cf6  /* U+05E6 HEBREW LETTER TSADI */
pattern XKB_KEY_hebrew_zade :: KeySymbol
pattern XKB_KEY_hebrew_zade = MkKeySymbol #{const XKB_KEY_hebrew_zade} 
-- #define XKB_KEY_hebrew_zadi                   0x0cf6  /* deprecated */
pattern XKB_KEY_hebrew_zadi :: KeySymbol
pattern XKB_KEY_hebrew_zadi = MkKeySymbol #{const XKB_KEY_hebrew_zadi} 
-- #define XKB_KEY_hebrew_qoph                   0x0cf7  /* U+05E7 HEBREW LETTER QOF */
pattern XKB_KEY_hebrew_qoph :: KeySymbol
pattern XKB_KEY_hebrew_qoph = MkKeySymbol #{const XKB_KEY_hebrew_qoph} 
-- #define XKB_KEY_hebrew_kuf                    0x0cf7  /* deprecated */
pattern XKB_KEY_hebrew_kuf :: KeySymbol
pattern XKB_KEY_hebrew_kuf = MkKeySymbol #{const XKB_KEY_hebrew_kuf} 
-- #define XKB_KEY_hebrew_resh                   0x0cf8  /* U+05E8 HEBREW LETTER RESH */
pattern XKB_KEY_hebrew_resh :: KeySymbol
pattern XKB_KEY_hebrew_resh = MkKeySymbol #{const XKB_KEY_hebrew_resh} 
-- #define XKB_KEY_hebrew_shin                   0x0cf9  /* U+05E9 HEBREW LETTER SHIN */
pattern XKB_KEY_hebrew_shin :: KeySymbol
pattern XKB_KEY_hebrew_shin = MkKeySymbol #{const XKB_KEY_hebrew_shin} 
-- #define XKB_KEY_hebrew_taw                    0x0cfa  /* U+05EA HEBREW LETTER TAV */
pattern XKB_KEY_hebrew_taw :: KeySymbol
pattern XKB_KEY_hebrew_taw = MkKeySymbol #{const XKB_KEY_hebrew_taw} 
-- #define XKB_KEY_hebrew_taf                    0x0cfa  /* deprecated */
pattern XKB_KEY_hebrew_taf :: KeySymbol
pattern XKB_KEY_hebrew_taf = MkKeySymbol #{const XKB_KEY_hebrew_taf} 
-- #define XKB_KEY_Hebrew_switch                 0xff7e  /* Alias for mode_switch */
pattern XKB_KEY_Hebrew_switch :: KeySymbol
pattern XKB_KEY_Hebrew_switch = MkKeySymbol #{const XKB_KEY_Hebrew_switch} 
-- #define XKB_KEY_Thai_kokai                    0x0da1  /* U+0E01 THAI CHARACTER KO KAI */
pattern XKB_KEY_Thai_kokai :: KeySymbol
pattern XKB_KEY_Thai_kokai = MkKeySymbol #{const XKB_KEY_Thai_kokai} 
-- #define XKB_KEY_Thai_khokhai                  0x0da2  /* U+0E02 THAI CHARACTER KHO KHAI */
pattern XKB_KEY_Thai_khokhai :: KeySymbol
pattern XKB_KEY_Thai_khokhai = MkKeySymbol #{const XKB_KEY_Thai_khokhai} 
-- #define XKB_KEY_Thai_khokhuat                 0x0da3  /* U+0E03 THAI CHARACTER KHO KHUAT */
pattern XKB_KEY_Thai_khokhuat :: KeySymbol
pattern XKB_KEY_Thai_khokhuat = MkKeySymbol #{const XKB_KEY_Thai_khokhuat} 
-- #define XKB_KEY_Thai_khokhwai                 0x0da4  /* U+0E04 THAI CHARACTER KHO KHWAI */
pattern XKB_KEY_Thai_khokhwai :: KeySymbol
pattern XKB_KEY_Thai_khokhwai = MkKeySymbol #{const XKB_KEY_Thai_khokhwai} 
-- #define XKB_KEY_Thai_khokhon                  0x0da5  /* U+0E05 THAI CHARACTER KHO KHON */
pattern XKB_KEY_Thai_khokhon :: KeySymbol
pattern XKB_KEY_Thai_khokhon = MkKeySymbol #{const XKB_KEY_Thai_khokhon} 
-- #define XKB_KEY_Thai_khorakhang               0x0da6  /* U+0E06 THAI CHARACTER KHO RAKHANG */
pattern XKB_KEY_Thai_khorakhang :: KeySymbol
pattern XKB_KEY_Thai_khorakhang = MkKeySymbol #{const XKB_KEY_Thai_khorakhang} 
-- #define XKB_KEY_Thai_ngongu                   0x0da7  /* U+0E07 THAI CHARACTER NGO NGU */
pattern XKB_KEY_Thai_ngongu :: KeySymbol
pattern XKB_KEY_Thai_ngongu = MkKeySymbol #{const XKB_KEY_Thai_ngongu} 
-- #define XKB_KEY_Thai_chochan                  0x0da8  /* U+0E08 THAI CHARACTER CHO CHAN */
pattern XKB_KEY_Thai_chochan :: KeySymbol
pattern XKB_KEY_Thai_chochan = MkKeySymbol #{const XKB_KEY_Thai_chochan} 
-- #define XKB_KEY_Thai_choching                 0x0da9  /* U+0E09 THAI CHARACTER CHO CHING */
pattern XKB_KEY_Thai_choching :: KeySymbol
pattern XKB_KEY_Thai_choching = MkKeySymbol #{const XKB_KEY_Thai_choching} 
-- #define XKB_KEY_Thai_chochang                 0x0daa  /* U+0E0A THAI CHARACTER CHO CHANG */
pattern XKB_KEY_Thai_chochang :: KeySymbol
pattern XKB_KEY_Thai_chochang = MkKeySymbol #{const XKB_KEY_Thai_chochang} 
-- #define XKB_KEY_Thai_soso                     0x0dab  /* U+0E0B THAI CHARACTER SO SO */
pattern XKB_KEY_Thai_soso :: KeySymbol
pattern XKB_KEY_Thai_soso = MkKeySymbol #{const XKB_KEY_Thai_soso} 
-- #define XKB_KEY_Thai_chochoe                  0x0dac  /* U+0E0C THAI CHARACTER CHO CHOE */
pattern XKB_KEY_Thai_chochoe :: KeySymbol
pattern XKB_KEY_Thai_chochoe = MkKeySymbol #{const XKB_KEY_Thai_chochoe} 
-- #define XKB_KEY_Thai_yoying                   0x0dad  /* U+0E0D THAI CHARACTER YO YING */
pattern XKB_KEY_Thai_yoying :: KeySymbol
pattern XKB_KEY_Thai_yoying = MkKeySymbol #{const XKB_KEY_Thai_yoying} 
-- #define XKB_KEY_Thai_dochada                  0x0dae  /* U+0E0E THAI CHARACTER DO CHADA */
pattern XKB_KEY_Thai_dochada :: KeySymbol
pattern XKB_KEY_Thai_dochada = MkKeySymbol #{const XKB_KEY_Thai_dochada} 
-- #define XKB_KEY_Thai_topatak                  0x0daf  /* U+0E0F THAI CHARACTER TO PATAK */
pattern XKB_KEY_Thai_topatak :: KeySymbol
pattern XKB_KEY_Thai_topatak = MkKeySymbol #{const XKB_KEY_Thai_topatak} 
-- #define XKB_KEY_Thai_thothan                  0x0db0  /* U+0E10 THAI CHARACTER THO THAN */
pattern XKB_KEY_Thai_thothan :: KeySymbol
pattern XKB_KEY_Thai_thothan = MkKeySymbol #{const XKB_KEY_Thai_thothan} 
-- #define XKB_KEY_Thai_thonangmontho            0x0db1  /* U+0E11 THAI CHARACTER THO NANGMONTHO */
pattern XKB_KEY_Thai_thonangmontho :: KeySymbol
pattern XKB_KEY_Thai_thonangmontho = MkKeySymbol #{const XKB_KEY_Thai_thonangmontho} 
-- #define XKB_KEY_Thai_thophuthao               0x0db2  /* U+0E12 THAI CHARACTER THO PHUTHAO */
pattern XKB_KEY_Thai_thophuthao :: KeySymbol
pattern XKB_KEY_Thai_thophuthao = MkKeySymbol #{const XKB_KEY_Thai_thophuthao} 
-- #define XKB_KEY_Thai_nonen                    0x0db3  /* U+0E13 THAI CHARACTER NO NEN */
pattern XKB_KEY_Thai_nonen :: KeySymbol
pattern XKB_KEY_Thai_nonen = MkKeySymbol #{const XKB_KEY_Thai_nonen} 
-- #define XKB_KEY_Thai_dodek                    0x0db4  /* U+0E14 THAI CHARACTER DO DEK */
pattern XKB_KEY_Thai_dodek :: KeySymbol
pattern XKB_KEY_Thai_dodek = MkKeySymbol #{const XKB_KEY_Thai_dodek} 
-- #define XKB_KEY_Thai_totao                    0x0db5  /* U+0E15 THAI CHARACTER TO TAO */
pattern XKB_KEY_Thai_totao :: KeySymbol
pattern XKB_KEY_Thai_totao = MkKeySymbol #{const XKB_KEY_Thai_totao} 
-- #define XKB_KEY_Thai_thothung                 0x0db6  /* U+0E16 THAI CHARACTER THO THUNG */
pattern XKB_KEY_Thai_thothung :: KeySymbol
pattern XKB_KEY_Thai_thothung = MkKeySymbol #{const XKB_KEY_Thai_thothung} 
-- #define XKB_KEY_Thai_thothahan                0x0db7  /* U+0E17 THAI CHARACTER THO THAHAN */
pattern XKB_KEY_Thai_thothahan :: KeySymbol
pattern XKB_KEY_Thai_thothahan = MkKeySymbol #{const XKB_KEY_Thai_thothahan} 
-- #define XKB_KEY_Thai_thothong                 0x0db8  /* U+0E18 THAI CHARACTER THO THONG */
pattern XKB_KEY_Thai_thothong :: KeySymbol
pattern XKB_KEY_Thai_thothong = MkKeySymbol #{const XKB_KEY_Thai_thothong} 
-- #define XKB_KEY_Thai_nonu                     0x0db9  /* U+0E19 THAI CHARACTER NO NU */
pattern XKB_KEY_Thai_nonu :: KeySymbol
pattern XKB_KEY_Thai_nonu = MkKeySymbol #{const XKB_KEY_Thai_nonu} 
-- #define XKB_KEY_Thai_bobaimai                 0x0dba  /* U+0E1A THAI CHARACTER BO BAIMAI */
pattern XKB_KEY_Thai_bobaimai :: KeySymbol
pattern XKB_KEY_Thai_bobaimai = MkKeySymbol #{const XKB_KEY_Thai_bobaimai} 
-- #define XKB_KEY_Thai_popla                    0x0dbb  /* U+0E1B THAI CHARACTER PO PLA */
pattern XKB_KEY_Thai_popla :: KeySymbol
pattern XKB_KEY_Thai_popla = MkKeySymbol #{const XKB_KEY_Thai_popla} 
-- #define XKB_KEY_Thai_phophung                 0x0dbc  /* U+0E1C THAI CHARACTER PHO PHUNG */
pattern XKB_KEY_Thai_phophung :: KeySymbol
pattern XKB_KEY_Thai_phophung = MkKeySymbol #{const XKB_KEY_Thai_phophung} 
-- #define XKB_KEY_Thai_fofa                     0x0dbd  /* U+0E1D THAI CHARACTER FO FA */
pattern XKB_KEY_Thai_fofa :: KeySymbol
pattern XKB_KEY_Thai_fofa = MkKeySymbol #{const XKB_KEY_Thai_fofa} 
-- #define XKB_KEY_Thai_phophan                  0x0dbe  /* U+0E1E THAI CHARACTER PHO PHAN */
pattern XKB_KEY_Thai_phophan :: KeySymbol
pattern XKB_KEY_Thai_phophan = MkKeySymbol #{const XKB_KEY_Thai_phophan} 
-- #define XKB_KEY_Thai_fofan                    0x0dbf  /* U+0E1F THAI CHARACTER FO FAN */
pattern XKB_KEY_Thai_fofan :: KeySymbol
pattern XKB_KEY_Thai_fofan = MkKeySymbol #{const XKB_KEY_Thai_fofan} 
-- #define XKB_KEY_Thai_phosamphao               0x0dc0  /* U+0E20 THAI CHARACTER PHO SAMPHAO */
pattern XKB_KEY_Thai_phosamphao :: KeySymbol
pattern XKB_KEY_Thai_phosamphao = MkKeySymbol #{const XKB_KEY_Thai_phosamphao} 
-- #define XKB_KEY_Thai_moma                     0x0dc1  /* U+0E21 THAI CHARACTER MO MA */
pattern XKB_KEY_Thai_moma :: KeySymbol
pattern XKB_KEY_Thai_moma = MkKeySymbol #{const XKB_KEY_Thai_moma} 
-- #define XKB_KEY_Thai_yoyak                    0x0dc2  /* U+0E22 THAI CHARACTER YO YAK */
pattern XKB_KEY_Thai_yoyak :: KeySymbol
pattern XKB_KEY_Thai_yoyak = MkKeySymbol #{const XKB_KEY_Thai_yoyak} 
-- #define XKB_KEY_Thai_rorua                    0x0dc3  /* U+0E23 THAI CHARACTER RO RUA */
pattern XKB_KEY_Thai_rorua :: KeySymbol
pattern XKB_KEY_Thai_rorua = MkKeySymbol #{const XKB_KEY_Thai_rorua} 
-- #define XKB_KEY_Thai_ru                       0x0dc4  /* U+0E24 THAI CHARACTER RU */
pattern XKB_KEY_Thai_ru :: KeySymbol
pattern XKB_KEY_Thai_ru = MkKeySymbol #{const XKB_KEY_Thai_ru} 
-- #define XKB_KEY_Thai_loling                   0x0dc5  /* U+0E25 THAI CHARACTER LO LING */
pattern XKB_KEY_Thai_loling :: KeySymbol
pattern XKB_KEY_Thai_loling = MkKeySymbol #{const XKB_KEY_Thai_loling} 
-- #define XKB_KEY_Thai_lu                       0x0dc6  /* U+0E26 THAI CHARACTER LU */
pattern XKB_KEY_Thai_lu :: KeySymbol
pattern XKB_KEY_Thai_lu = MkKeySymbol #{const XKB_KEY_Thai_lu} 
-- #define XKB_KEY_Thai_wowaen                   0x0dc7  /* U+0E27 THAI CHARACTER WO WAEN */
pattern XKB_KEY_Thai_wowaen :: KeySymbol
pattern XKB_KEY_Thai_wowaen = MkKeySymbol #{const XKB_KEY_Thai_wowaen} 
-- #define XKB_KEY_Thai_sosala                   0x0dc8  /* U+0E28 THAI CHARACTER SO SALA */
pattern XKB_KEY_Thai_sosala :: KeySymbol
pattern XKB_KEY_Thai_sosala = MkKeySymbol #{const XKB_KEY_Thai_sosala} 
-- #define XKB_KEY_Thai_sorusi                   0x0dc9  /* U+0E29 THAI CHARACTER SO RUSI */
pattern XKB_KEY_Thai_sorusi :: KeySymbol
pattern XKB_KEY_Thai_sorusi = MkKeySymbol #{const XKB_KEY_Thai_sorusi} 
-- #define XKB_KEY_Thai_sosua                    0x0dca  /* U+0E2A THAI CHARACTER SO SUA */
pattern XKB_KEY_Thai_sosua :: KeySymbol
pattern XKB_KEY_Thai_sosua = MkKeySymbol #{const XKB_KEY_Thai_sosua} 
-- #define XKB_KEY_Thai_hohip                    0x0dcb  /* U+0E2B THAI CHARACTER HO HIP */
pattern XKB_KEY_Thai_hohip :: KeySymbol
pattern XKB_KEY_Thai_hohip = MkKeySymbol #{const XKB_KEY_Thai_hohip} 
-- #define XKB_KEY_Thai_lochula                  0x0dcc  /* U+0E2C THAI CHARACTER LO CHULA */
pattern XKB_KEY_Thai_lochula :: KeySymbol
pattern XKB_KEY_Thai_lochula = MkKeySymbol #{const XKB_KEY_Thai_lochula} 
-- #define XKB_KEY_Thai_oang                     0x0dcd  /* U+0E2D THAI CHARACTER O ANG */
pattern XKB_KEY_Thai_oang :: KeySymbol
pattern XKB_KEY_Thai_oang = MkKeySymbol #{const XKB_KEY_Thai_oang} 
-- #define XKB_KEY_Thai_honokhuk                 0x0dce  /* U+0E2E THAI CHARACTER HO NOKHUK */
pattern XKB_KEY_Thai_honokhuk :: KeySymbol
pattern XKB_KEY_Thai_honokhuk = MkKeySymbol #{const XKB_KEY_Thai_honokhuk} 
-- #define XKB_KEY_Thai_paiyannoi                0x0dcf  /* U+0E2F THAI CHARACTER PAIYANNOI */
pattern XKB_KEY_Thai_paiyannoi :: KeySymbol
pattern XKB_KEY_Thai_paiyannoi = MkKeySymbol #{const XKB_KEY_Thai_paiyannoi} 
-- #define XKB_KEY_Thai_saraa                    0x0dd0  /* U+0E30 THAI CHARACTER SARA A */
pattern XKB_KEY_Thai_saraa :: KeySymbol
pattern XKB_KEY_Thai_saraa = MkKeySymbol #{const XKB_KEY_Thai_saraa} 
-- #define XKB_KEY_Thai_maihanakat               0x0dd1  /* U+0E31 THAI CHARACTER MAI HAN-AKAT */
pattern XKB_KEY_Thai_maihanakat :: KeySymbol
pattern XKB_KEY_Thai_maihanakat = MkKeySymbol #{const XKB_KEY_Thai_maihanakat} 
-- #define XKB_KEY_Thai_saraaa                   0x0dd2  /* U+0E32 THAI CHARACTER SARA AA */
pattern XKB_KEY_Thai_saraaa :: KeySymbol
pattern XKB_KEY_Thai_saraaa = MkKeySymbol #{const XKB_KEY_Thai_saraaa} 
-- #define XKB_KEY_Thai_saraam                   0x0dd3  /* U+0E33 THAI CHARACTER SARA AM */
pattern XKB_KEY_Thai_saraam :: KeySymbol
pattern XKB_KEY_Thai_saraam = MkKeySymbol #{const XKB_KEY_Thai_saraam} 
-- #define XKB_KEY_Thai_sarai                    0x0dd4  /* U+0E34 THAI CHARACTER SARA I */
pattern XKB_KEY_Thai_sarai :: KeySymbol
pattern XKB_KEY_Thai_sarai = MkKeySymbol #{const XKB_KEY_Thai_sarai} 
-- #define XKB_KEY_Thai_saraii                   0x0dd5  /* U+0E35 THAI CHARACTER SARA II */
pattern XKB_KEY_Thai_saraii :: KeySymbol
pattern XKB_KEY_Thai_saraii = MkKeySymbol #{const XKB_KEY_Thai_saraii} 
-- #define XKB_KEY_Thai_saraue                   0x0dd6  /* U+0E36 THAI CHARACTER SARA UE */
pattern XKB_KEY_Thai_saraue :: KeySymbol
pattern XKB_KEY_Thai_saraue = MkKeySymbol #{const XKB_KEY_Thai_saraue} 
-- #define XKB_KEY_Thai_sarauee                  0x0dd7  /* U+0E37 THAI CHARACTER SARA UEE */
pattern XKB_KEY_Thai_sarauee :: KeySymbol
pattern XKB_KEY_Thai_sarauee = MkKeySymbol #{const XKB_KEY_Thai_sarauee} 
-- #define XKB_KEY_Thai_sarau                    0x0dd8  /* U+0E38 THAI CHARACTER SARA U */
pattern XKB_KEY_Thai_sarau :: KeySymbol
pattern XKB_KEY_Thai_sarau = MkKeySymbol #{const XKB_KEY_Thai_sarau} 
-- #define XKB_KEY_Thai_sarauu                   0x0dd9  /* U+0E39 THAI CHARACTER SARA UU */
pattern XKB_KEY_Thai_sarauu :: KeySymbol
pattern XKB_KEY_Thai_sarauu = MkKeySymbol #{const XKB_KEY_Thai_sarauu} 
-- #define XKB_KEY_Thai_phinthu                  0x0dda  /* U+0E3A THAI CHARACTER PHINTHU */
pattern XKB_KEY_Thai_phinthu :: KeySymbol
pattern XKB_KEY_Thai_phinthu = MkKeySymbol #{const XKB_KEY_Thai_phinthu} 
-- #define XKB_KEY_Thai_maihanakat_maitho        0x0dde
pattern XKB_KEY_Thai_maihanakat_maitho :: KeySymbol
pattern XKB_KEY_Thai_maihanakat_maitho = MkKeySymbol #{const XKB_KEY_Thai_maihanakat_maitho} 
-- #define XKB_KEY_Thai_baht                     0x0ddf  /* U+0E3F THAI CURRENCY SYMBOL BAHT */
pattern XKB_KEY_Thai_baht :: KeySymbol
pattern XKB_KEY_Thai_baht = MkKeySymbol #{const XKB_KEY_Thai_baht} 
-- #define XKB_KEY_Thai_sarae                    0x0de0  /* U+0E40 THAI CHARACTER SARA E */
pattern XKB_KEY_Thai_sarae :: KeySymbol
pattern XKB_KEY_Thai_sarae = MkKeySymbol #{const XKB_KEY_Thai_sarae} 
-- #define XKB_KEY_Thai_saraae                   0x0de1  /* U+0E41 THAI CHARACTER SARA AE */
pattern XKB_KEY_Thai_saraae :: KeySymbol
pattern XKB_KEY_Thai_saraae = MkKeySymbol #{const XKB_KEY_Thai_saraae} 
-- #define XKB_KEY_Thai_sarao                    0x0de2  /* U+0E42 THAI CHARACTER SARA O */
pattern XKB_KEY_Thai_sarao :: KeySymbol
pattern XKB_KEY_Thai_sarao = MkKeySymbol #{const XKB_KEY_Thai_sarao} 
-- #define XKB_KEY_Thai_saraaimaimuan            0x0de3  /* U+0E43 THAI CHARACTER SARA AI MAIMUAN */
pattern XKB_KEY_Thai_saraaimaimuan :: KeySymbol
pattern XKB_KEY_Thai_saraaimaimuan = MkKeySymbol #{const XKB_KEY_Thai_saraaimaimuan} 
-- #define XKB_KEY_Thai_saraaimaimalai           0x0de4  /* U+0E44 THAI CHARACTER SARA AI MAIMALAI */
pattern XKB_KEY_Thai_saraaimaimalai :: KeySymbol
pattern XKB_KEY_Thai_saraaimaimalai = MkKeySymbol #{const XKB_KEY_Thai_saraaimaimalai} 
-- #define XKB_KEY_Thai_lakkhangyao              0x0de5  /* U+0E45 THAI CHARACTER LAKKHANGYAO */
pattern XKB_KEY_Thai_lakkhangyao :: KeySymbol
pattern XKB_KEY_Thai_lakkhangyao = MkKeySymbol #{const XKB_KEY_Thai_lakkhangyao} 
-- #define XKB_KEY_Thai_maiyamok                 0x0de6  /* U+0E46 THAI CHARACTER MAIYAMOK */
pattern XKB_KEY_Thai_maiyamok :: KeySymbol
pattern XKB_KEY_Thai_maiyamok = MkKeySymbol #{const XKB_KEY_Thai_maiyamok} 
-- #define XKB_KEY_Thai_maitaikhu                0x0de7  /* U+0E47 THAI CHARACTER MAITAIKHU */
pattern XKB_KEY_Thai_maitaikhu :: KeySymbol
pattern XKB_KEY_Thai_maitaikhu = MkKeySymbol #{const XKB_KEY_Thai_maitaikhu} 
-- #define XKB_KEY_Thai_maiek                    0x0de8  /* U+0E48 THAI CHARACTER MAI EK */
pattern XKB_KEY_Thai_maiek :: KeySymbol
pattern XKB_KEY_Thai_maiek = MkKeySymbol #{const XKB_KEY_Thai_maiek} 
-- #define XKB_KEY_Thai_maitho                   0x0de9  /* U+0E49 THAI CHARACTER MAI THO */
pattern XKB_KEY_Thai_maitho :: KeySymbol
pattern XKB_KEY_Thai_maitho = MkKeySymbol #{const XKB_KEY_Thai_maitho} 
-- #define XKB_KEY_Thai_maitri                   0x0dea  /* U+0E4A THAI CHARACTER MAI TRI */
pattern XKB_KEY_Thai_maitri :: KeySymbol
pattern XKB_KEY_Thai_maitri = MkKeySymbol #{const XKB_KEY_Thai_maitri} 
-- #define XKB_KEY_Thai_maichattawa              0x0deb  /* U+0E4B THAI CHARACTER MAI CHATTAWA */
pattern XKB_KEY_Thai_maichattawa :: KeySymbol
pattern XKB_KEY_Thai_maichattawa = MkKeySymbol #{const XKB_KEY_Thai_maichattawa} 
-- #define XKB_KEY_Thai_thanthakhat              0x0dec  /* U+0E4C THAI CHARACTER THANTHAKHAT */
pattern XKB_KEY_Thai_thanthakhat :: KeySymbol
pattern XKB_KEY_Thai_thanthakhat = MkKeySymbol #{const XKB_KEY_Thai_thanthakhat} 
-- #define XKB_KEY_Thai_nikhahit                 0x0ded  /* U+0E4D THAI CHARACTER NIKHAHIT */
pattern XKB_KEY_Thai_nikhahit :: KeySymbol
pattern XKB_KEY_Thai_nikhahit = MkKeySymbol #{const XKB_KEY_Thai_nikhahit} 
-- #define XKB_KEY_Thai_leksun                   0x0df0  /* U+0E50 THAI DIGIT ZERO */
pattern XKB_KEY_Thai_leksun :: KeySymbol
pattern XKB_KEY_Thai_leksun = MkKeySymbol #{const XKB_KEY_Thai_leksun} 
-- #define XKB_KEY_Thai_leknung                  0x0df1  /* U+0E51 THAI DIGIT ONE */
pattern XKB_KEY_Thai_leknung :: KeySymbol
pattern XKB_KEY_Thai_leknung = MkKeySymbol #{const XKB_KEY_Thai_leknung} 
-- #define XKB_KEY_Thai_leksong                  0x0df2  /* U+0E52 THAI DIGIT TWO */
pattern XKB_KEY_Thai_leksong :: KeySymbol
pattern XKB_KEY_Thai_leksong = MkKeySymbol #{const XKB_KEY_Thai_leksong} 
-- #define XKB_KEY_Thai_leksam                   0x0df3  /* U+0E53 THAI DIGIT THREE */
pattern XKB_KEY_Thai_leksam :: KeySymbol
pattern XKB_KEY_Thai_leksam = MkKeySymbol #{const XKB_KEY_Thai_leksam} 
-- #define XKB_KEY_Thai_leksi                    0x0df4  /* U+0E54 THAI DIGIT FOUR */
pattern XKB_KEY_Thai_leksi :: KeySymbol
pattern XKB_KEY_Thai_leksi = MkKeySymbol #{const XKB_KEY_Thai_leksi} 
-- #define XKB_KEY_Thai_lekha                    0x0df5  /* U+0E55 THAI DIGIT FIVE */
pattern XKB_KEY_Thai_lekha :: KeySymbol
pattern XKB_KEY_Thai_lekha = MkKeySymbol #{const XKB_KEY_Thai_lekha} 
-- #define XKB_KEY_Thai_lekhok                   0x0df6  /* U+0E56 THAI DIGIT SIX */
pattern XKB_KEY_Thai_lekhok :: KeySymbol
pattern XKB_KEY_Thai_lekhok = MkKeySymbol #{const XKB_KEY_Thai_lekhok} 
-- #define XKB_KEY_Thai_lekchet                  0x0df7  /* U+0E57 THAI DIGIT SEVEN */
pattern XKB_KEY_Thai_lekchet :: KeySymbol
pattern XKB_KEY_Thai_lekchet = MkKeySymbol #{const XKB_KEY_Thai_lekchet} 
-- #define XKB_KEY_Thai_lekpaet                  0x0df8  /* U+0E58 THAI DIGIT EIGHT */
pattern XKB_KEY_Thai_lekpaet :: KeySymbol
pattern XKB_KEY_Thai_lekpaet = MkKeySymbol #{const XKB_KEY_Thai_lekpaet} 
-- #define XKB_KEY_Thai_lekkao                   0x0df9  /* U+0E59 THAI DIGIT NINE */
pattern XKB_KEY_Thai_lekkao :: KeySymbol
pattern XKB_KEY_Thai_lekkao = MkKeySymbol #{const XKB_KEY_Thai_lekkao} 
-- #define XKB_KEY_Hangul                        0xff31  /* Hangul start/stop(toggle) */
pattern XKB_KEY_Hangul :: KeySymbol
pattern XKB_KEY_Hangul = MkKeySymbol #{const XKB_KEY_Hangul} 
-- #define XKB_KEY_Hangul_Start                  0xff32  /* Hangul start */
pattern XKB_KEY_Hangul_Start :: KeySymbol
pattern XKB_KEY_Hangul_Start = MkKeySymbol #{const XKB_KEY_Hangul_Start} 
-- #define XKB_KEY_Hangul_End                    0xff33  /* Hangul end, English start */
pattern XKB_KEY_Hangul_End :: KeySymbol
pattern XKB_KEY_Hangul_End = MkKeySymbol #{const XKB_KEY_Hangul_End} 
-- #define XKB_KEY_Hangul_Hanja                  0xff34  /* Start Hangul->Hanja Conversion */
pattern XKB_KEY_Hangul_Hanja :: KeySymbol
pattern XKB_KEY_Hangul_Hanja = MkKeySymbol #{const XKB_KEY_Hangul_Hanja} 
-- #define XKB_KEY_Hangul_Jamo                   0xff35  /* Hangul Jamo mode */
pattern XKB_KEY_Hangul_Jamo :: KeySymbol
pattern XKB_KEY_Hangul_Jamo = MkKeySymbol #{const XKB_KEY_Hangul_Jamo} 
-- #define XKB_KEY_Hangul_Romaja                 0xff36  /* Hangul Romaja mode */
pattern XKB_KEY_Hangul_Romaja :: KeySymbol
pattern XKB_KEY_Hangul_Romaja = MkKeySymbol #{const XKB_KEY_Hangul_Romaja} 
-- #define XKB_KEY_Hangul_Codeinput              0xff37  /* Hangul code input mode */
pattern XKB_KEY_Hangul_Codeinput :: KeySymbol
pattern XKB_KEY_Hangul_Codeinput = MkKeySymbol #{const XKB_KEY_Hangul_Codeinput} 
-- #define XKB_KEY_Hangul_Jeonja                 0xff38  /* Jeonja mode */
pattern XKB_KEY_Hangul_Jeonja :: KeySymbol
pattern XKB_KEY_Hangul_Jeonja = MkKeySymbol #{const XKB_KEY_Hangul_Jeonja} 
-- #define XKB_KEY_Hangul_Banja                  0xff39  /* Banja mode */
pattern XKB_KEY_Hangul_Banja :: KeySymbol
pattern XKB_KEY_Hangul_Banja = MkKeySymbol #{const XKB_KEY_Hangul_Banja} 
-- #define XKB_KEY_Hangul_PreHanja               0xff3a  /* Pre Hanja conversion */
pattern XKB_KEY_Hangul_PreHanja :: KeySymbol
pattern XKB_KEY_Hangul_PreHanja = MkKeySymbol #{const XKB_KEY_Hangul_PreHanja} 
-- #define XKB_KEY_Hangul_PostHanja              0xff3b  /* Post Hanja conversion */
pattern XKB_KEY_Hangul_PostHanja :: KeySymbol
pattern XKB_KEY_Hangul_PostHanja = MkKeySymbol #{const XKB_KEY_Hangul_PostHanja} 
-- #define XKB_KEY_Hangul_SingleCandidate        0xff3c  /* Single candidate */
pattern XKB_KEY_Hangul_SingleCandidate :: KeySymbol
pattern XKB_KEY_Hangul_SingleCandidate = MkKeySymbol #{const XKB_KEY_Hangul_SingleCandidate} 
-- #define XKB_KEY_Hangul_MultipleCandidate      0xff3d  /* Multiple candidate */
pattern XKB_KEY_Hangul_MultipleCandidate :: KeySymbol
pattern XKB_KEY_Hangul_MultipleCandidate = MkKeySymbol #{const XKB_KEY_Hangul_MultipleCandidate} 
-- #define XKB_KEY_Hangul_PreviousCandidate      0xff3e  /* Previous candidate */
pattern XKB_KEY_Hangul_PreviousCandidate :: KeySymbol
pattern XKB_KEY_Hangul_PreviousCandidate = MkKeySymbol #{const XKB_KEY_Hangul_PreviousCandidate} 
-- #define XKB_KEY_Hangul_Special                0xff3f  /* Special symbols */
pattern XKB_KEY_Hangul_Special :: KeySymbol
pattern XKB_KEY_Hangul_Special = MkKeySymbol #{const XKB_KEY_Hangul_Special} 
-- #define XKB_KEY_Hangul_switch                 0xff7e  /* Alias for mode_switch */
pattern XKB_KEY_Hangul_switch :: KeySymbol
pattern XKB_KEY_Hangul_switch = MkKeySymbol #{const XKB_KEY_Hangul_switch} 
-- #define XKB_KEY_Hangul_Kiyeog                 0x0ea1
pattern XKB_KEY_Hangul_Kiyeog :: KeySymbol
pattern XKB_KEY_Hangul_Kiyeog = MkKeySymbol #{const XKB_KEY_Hangul_Kiyeog} 
-- #define XKB_KEY_Hangul_SsangKiyeog            0x0ea2
pattern XKB_KEY_Hangul_SsangKiyeog :: KeySymbol
pattern XKB_KEY_Hangul_SsangKiyeog = MkKeySymbol #{const XKB_KEY_Hangul_SsangKiyeog} 
-- #define XKB_KEY_Hangul_KiyeogSios             0x0ea3
pattern XKB_KEY_Hangul_KiyeogSios :: KeySymbol
pattern XKB_KEY_Hangul_KiyeogSios = MkKeySymbol #{const XKB_KEY_Hangul_KiyeogSios} 
-- #define XKB_KEY_Hangul_Nieun                  0x0ea4
pattern XKB_KEY_Hangul_Nieun :: KeySymbol
pattern XKB_KEY_Hangul_Nieun = MkKeySymbol #{const XKB_KEY_Hangul_Nieun} 
-- #define XKB_KEY_Hangul_NieunJieuj             0x0ea5
pattern XKB_KEY_Hangul_NieunJieuj :: KeySymbol
pattern XKB_KEY_Hangul_NieunJieuj = MkKeySymbol #{const XKB_KEY_Hangul_NieunJieuj} 
-- #define XKB_KEY_Hangul_NieunHieuh             0x0ea6
pattern XKB_KEY_Hangul_NieunHieuh :: KeySymbol
pattern XKB_KEY_Hangul_NieunHieuh = MkKeySymbol #{const XKB_KEY_Hangul_NieunHieuh} 
-- #define XKB_KEY_Hangul_Dikeud                 0x0ea7
pattern XKB_KEY_Hangul_Dikeud :: KeySymbol
pattern XKB_KEY_Hangul_Dikeud = MkKeySymbol #{const XKB_KEY_Hangul_Dikeud} 
-- #define XKB_KEY_Hangul_SsangDikeud            0x0ea8
pattern XKB_KEY_Hangul_SsangDikeud :: KeySymbol
pattern XKB_KEY_Hangul_SsangDikeud = MkKeySymbol #{const XKB_KEY_Hangul_SsangDikeud} 
-- #define XKB_KEY_Hangul_Rieul                  0x0ea9
pattern XKB_KEY_Hangul_Rieul :: KeySymbol
pattern XKB_KEY_Hangul_Rieul = MkKeySymbol #{const XKB_KEY_Hangul_Rieul} 
-- #define XKB_KEY_Hangul_RieulKiyeog            0x0eaa
pattern XKB_KEY_Hangul_RieulKiyeog :: KeySymbol
pattern XKB_KEY_Hangul_RieulKiyeog = MkKeySymbol #{const XKB_KEY_Hangul_RieulKiyeog} 
-- #define XKB_KEY_Hangul_RieulMieum             0x0eab
pattern XKB_KEY_Hangul_RieulMieum :: KeySymbol
pattern XKB_KEY_Hangul_RieulMieum = MkKeySymbol #{const XKB_KEY_Hangul_RieulMieum} 
-- #define XKB_KEY_Hangul_RieulPieub             0x0eac
pattern XKB_KEY_Hangul_RieulPieub :: KeySymbol
pattern XKB_KEY_Hangul_RieulPieub = MkKeySymbol #{const XKB_KEY_Hangul_RieulPieub} 
-- #define XKB_KEY_Hangul_RieulSios              0x0ead
pattern XKB_KEY_Hangul_RieulSios :: KeySymbol
pattern XKB_KEY_Hangul_RieulSios = MkKeySymbol #{const XKB_KEY_Hangul_RieulSios} 
-- #define XKB_KEY_Hangul_RieulTieut             0x0eae
pattern XKB_KEY_Hangul_RieulTieut :: KeySymbol
pattern XKB_KEY_Hangul_RieulTieut = MkKeySymbol #{const XKB_KEY_Hangul_RieulTieut} 
-- #define XKB_KEY_Hangul_RieulPhieuf            0x0eaf
pattern XKB_KEY_Hangul_RieulPhieuf :: KeySymbol
pattern XKB_KEY_Hangul_RieulPhieuf = MkKeySymbol #{const XKB_KEY_Hangul_RieulPhieuf} 
-- #define XKB_KEY_Hangul_RieulHieuh             0x0eb0
pattern XKB_KEY_Hangul_RieulHieuh :: KeySymbol
pattern XKB_KEY_Hangul_RieulHieuh = MkKeySymbol #{const XKB_KEY_Hangul_RieulHieuh} 
-- #define XKB_KEY_Hangul_Mieum                  0x0eb1
pattern XKB_KEY_Hangul_Mieum :: KeySymbol
pattern XKB_KEY_Hangul_Mieum = MkKeySymbol #{const XKB_KEY_Hangul_Mieum} 
-- #define XKB_KEY_Hangul_Pieub                  0x0eb2
pattern XKB_KEY_Hangul_Pieub :: KeySymbol
pattern XKB_KEY_Hangul_Pieub = MkKeySymbol #{const XKB_KEY_Hangul_Pieub} 
-- #define XKB_KEY_Hangul_SsangPieub             0x0eb3
pattern XKB_KEY_Hangul_SsangPieub :: KeySymbol
pattern XKB_KEY_Hangul_SsangPieub = MkKeySymbol #{const XKB_KEY_Hangul_SsangPieub} 
-- #define XKB_KEY_Hangul_PieubSios              0x0eb4
pattern XKB_KEY_Hangul_PieubSios :: KeySymbol
pattern XKB_KEY_Hangul_PieubSios = MkKeySymbol #{const XKB_KEY_Hangul_PieubSios} 
-- #define XKB_KEY_Hangul_Sios                   0x0eb5
pattern XKB_KEY_Hangul_Sios :: KeySymbol
pattern XKB_KEY_Hangul_Sios = MkKeySymbol #{const XKB_KEY_Hangul_Sios} 
-- #define XKB_KEY_Hangul_SsangSios              0x0eb6
pattern XKB_KEY_Hangul_SsangSios :: KeySymbol
pattern XKB_KEY_Hangul_SsangSios = MkKeySymbol #{const XKB_KEY_Hangul_SsangSios} 
-- #define XKB_KEY_Hangul_Ieung                  0x0eb7
pattern XKB_KEY_Hangul_Ieung :: KeySymbol
pattern XKB_KEY_Hangul_Ieung = MkKeySymbol #{const XKB_KEY_Hangul_Ieung} 
-- #define XKB_KEY_Hangul_Jieuj                  0x0eb8
pattern XKB_KEY_Hangul_Jieuj :: KeySymbol
pattern XKB_KEY_Hangul_Jieuj = MkKeySymbol #{const XKB_KEY_Hangul_Jieuj} 
-- #define XKB_KEY_Hangul_SsangJieuj             0x0eb9
pattern XKB_KEY_Hangul_SsangJieuj :: KeySymbol
pattern XKB_KEY_Hangul_SsangJieuj = MkKeySymbol #{const XKB_KEY_Hangul_SsangJieuj} 
-- #define XKB_KEY_Hangul_Cieuc                  0x0eba
pattern XKB_KEY_Hangul_Cieuc :: KeySymbol
pattern XKB_KEY_Hangul_Cieuc = MkKeySymbol #{const XKB_KEY_Hangul_Cieuc} 
-- #define XKB_KEY_Hangul_Khieuq                 0x0ebb
pattern XKB_KEY_Hangul_Khieuq :: KeySymbol
pattern XKB_KEY_Hangul_Khieuq = MkKeySymbol #{const XKB_KEY_Hangul_Khieuq} 
-- #define XKB_KEY_Hangul_Tieut                  0x0ebc
pattern XKB_KEY_Hangul_Tieut :: KeySymbol
pattern XKB_KEY_Hangul_Tieut = MkKeySymbol #{const XKB_KEY_Hangul_Tieut} 
-- #define XKB_KEY_Hangul_Phieuf                 0x0ebd
pattern XKB_KEY_Hangul_Phieuf :: KeySymbol
pattern XKB_KEY_Hangul_Phieuf = MkKeySymbol #{const XKB_KEY_Hangul_Phieuf} 
-- #define XKB_KEY_Hangul_Hieuh                  0x0ebe
pattern XKB_KEY_Hangul_Hieuh :: KeySymbol
pattern XKB_KEY_Hangul_Hieuh = MkKeySymbol #{const XKB_KEY_Hangul_Hieuh} 
-- #define XKB_KEY_Hangul_A                      0x0ebf
pattern XKB_KEY_Hangul_A :: KeySymbol
pattern XKB_KEY_Hangul_A = MkKeySymbol #{const XKB_KEY_Hangul_A} 
-- #define XKB_KEY_Hangul_AE                     0x0ec0
pattern XKB_KEY_Hangul_AE :: KeySymbol
pattern XKB_KEY_Hangul_AE = MkKeySymbol #{const XKB_KEY_Hangul_AE} 
-- #define XKB_KEY_Hangul_YA                     0x0ec1
pattern XKB_KEY_Hangul_YA :: KeySymbol
pattern XKB_KEY_Hangul_YA = MkKeySymbol #{const XKB_KEY_Hangul_YA} 
-- #define XKB_KEY_Hangul_YAE                    0x0ec2
pattern XKB_KEY_Hangul_YAE :: KeySymbol
pattern XKB_KEY_Hangul_YAE = MkKeySymbol #{const XKB_KEY_Hangul_YAE} 
-- #define XKB_KEY_Hangul_EO                     0x0ec3
pattern XKB_KEY_Hangul_EO :: KeySymbol
pattern XKB_KEY_Hangul_EO = MkKeySymbol #{const XKB_KEY_Hangul_EO} 
-- #define XKB_KEY_Hangul_E                      0x0ec4
pattern XKB_KEY_Hangul_E :: KeySymbol
pattern XKB_KEY_Hangul_E = MkKeySymbol #{const XKB_KEY_Hangul_E} 
-- #define XKB_KEY_Hangul_YEO                    0x0ec5
pattern XKB_KEY_Hangul_YEO :: KeySymbol
pattern XKB_KEY_Hangul_YEO = MkKeySymbol #{const XKB_KEY_Hangul_YEO} 
-- #define XKB_KEY_Hangul_YE                     0x0ec6
pattern XKB_KEY_Hangul_YE :: KeySymbol
pattern XKB_KEY_Hangul_YE = MkKeySymbol #{const XKB_KEY_Hangul_YE} 
-- #define XKB_KEY_Hangul_O                      0x0ec7
pattern XKB_KEY_Hangul_O :: KeySymbol
pattern XKB_KEY_Hangul_O = MkKeySymbol #{const XKB_KEY_Hangul_O} 
-- #define XKB_KEY_Hangul_WA                     0x0ec8
pattern XKB_KEY_Hangul_WA :: KeySymbol
pattern XKB_KEY_Hangul_WA = MkKeySymbol #{const XKB_KEY_Hangul_WA} 
-- #define XKB_KEY_Hangul_WAE                    0x0ec9
pattern XKB_KEY_Hangul_WAE :: KeySymbol
pattern XKB_KEY_Hangul_WAE = MkKeySymbol #{const XKB_KEY_Hangul_WAE} 
-- #define XKB_KEY_Hangul_OE                     0x0eca
pattern XKB_KEY_Hangul_OE :: KeySymbol
pattern XKB_KEY_Hangul_OE = MkKeySymbol #{const XKB_KEY_Hangul_OE} 
-- #define XKB_KEY_Hangul_YO                     0x0ecb
pattern XKB_KEY_Hangul_YO :: KeySymbol
pattern XKB_KEY_Hangul_YO = MkKeySymbol #{const XKB_KEY_Hangul_YO} 
-- #define XKB_KEY_Hangul_U                      0x0ecc
pattern XKB_KEY_Hangul_U :: KeySymbol
pattern XKB_KEY_Hangul_U = MkKeySymbol #{const XKB_KEY_Hangul_U} 
-- #define XKB_KEY_Hangul_WEO                    0x0ecd
pattern XKB_KEY_Hangul_WEO :: KeySymbol
pattern XKB_KEY_Hangul_WEO = MkKeySymbol #{const XKB_KEY_Hangul_WEO} 
-- #define XKB_KEY_Hangul_WE                     0x0ece
pattern XKB_KEY_Hangul_WE :: KeySymbol
pattern XKB_KEY_Hangul_WE = MkKeySymbol #{const XKB_KEY_Hangul_WE} 
-- #define XKB_KEY_Hangul_WI                     0x0ecf
pattern XKB_KEY_Hangul_WI :: KeySymbol
pattern XKB_KEY_Hangul_WI = MkKeySymbol #{const XKB_KEY_Hangul_WI} 
-- #define XKB_KEY_Hangul_YU                     0x0ed0
pattern XKB_KEY_Hangul_YU :: KeySymbol
pattern XKB_KEY_Hangul_YU = MkKeySymbol #{const XKB_KEY_Hangul_YU} 
-- #define XKB_KEY_Hangul_EU                     0x0ed1
pattern XKB_KEY_Hangul_EU :: KeySymbol
pattern XKB_KEY_Hangul_EU = MkKeySymbol #{const XKB_KEY_Hangul_EU} 
-- #define XKB_KEY_Hangul_YI                     0x0ed2
pattern XKB_KEY_Hangul_YI :: KeySymbol
pattern XKB_KEY_Hangul_YI = MkKeySymbol #{const XKB_KEY_Hangul_YI} 
-- #define XKB_KEY_Hangul_I                      0x0ed3
pattern XKB_KEY_Hangul_I :: KeySymbol
pattern XKB_KEY_Hangul_I = MkKeySymbol #{const XKB_KEY_Hangul_I} 
-- #define XKB_KEY_Hangul_J_Kiyeog               0x0ed4
pattern XKB_KEY_Hangul_J_Kiyeog :: KeySymbol
pattern XKB_KEY_Hangul_J_Kiyeog = MkKeySymbol #{const XKB_KEY_Hangul_J_Kiyeog} 
-- #define XKB_KEY_Hangul_J_SsangKiyeog          0x0ed5
pattern XKB_KEY_Hangul_J_SsangKiyeog :: KeySymbol
pattern XKB_KEY_Hangul_J_SsangKiyeog = MkKeySymbol #{const XKB_KEY_Hangul_J_SsangKiyeog} 
-- #define XKB_KEY_Hangul_J_KiyeogSios           0x0ed6
pattern XKB_KEY_Hangul_J_KiyeogSios :: KeySymbol
pattern XKB_KEY_Hangul_J_KiyeogSios = MkKeySymbol #{const XKB_KEY_Hangul_J_KiyeogSios} 
-- #define XKB_KEY_Hangul_J_Nieun                0x0ed7
pattern XKB_KEY_Hangul_J_Nieun :: KeySymbol
pattern XKB_KEY_Hangul_J_Nieun = MkKeySymbol #{const XKB_KEY_Hangul_J_Nieun} 
-- #define XKB_KEY_Hangul_J_NieunJieuj           0x0ed8
pattern XKB_KEY_Hangul_J_NieunJieuj :: KeySymbol
pattern XKB_KEY_Hangul_J_NieunJieuj = MkKeySymbol #{const XKB_KEY_Hangul_J_NieunJieuj} 
-- #define XKB_KEY_Hangul_J_NieunHieuh           0x0ed9
pattern XKB_KEY_Hangul_J_NieunHieuh :: KeySymbol
pattern XKB_KEY_Hangul_J_NieunHieuh = MkKeySymbol #{const XKB_KEY_Hangul_J_NieunHieuh} 
-- #define XKB_KEY_Hangul_J_Dikeud               0x0eda
pattern XKB_KEY_Hangul_J_Dikeud :: KeySymbol
pattern XKB_KEY_Hangul_J_Dikeud = MkKeySymbol #{const XKB_KEY_Hangul_J_Dikeud} 
-- #define XKB_KEY_Hangul_J_Rieul                0x0edb
pattern XKB_KEY_Hangul_J_Rieul :: KeySymbol
pattern XKB_KEY_Hangul_J_Rieul = MkKeySymbol #{const XKB_KEY_Hangul_J_Rieul} 
-- #define XKB_KEY_Hangul_J_RieulKiyeog          0x0edc
pattern XKB_KEY_Hangul_J_RieulKiyeog :: KeySymbol
pattern XKB_KEY_Hangul_J_RieulKiyeog = MkKeySymbol #{const XKB_KEY_Hangul_J_RieulKiyeog} 
-- #define XKB_KEY_Hangul_J_RieulMieum           0x0edd
pattern XKB_KEY_Hangul_J_RieulMieum :: KeySymbol
pattern XKB_KEY_Hangul_J_RieulMieum = MkKeySymbol #{const XKB_KEY_Hangul_J_RieulMieum} 
-- #define XKB_KEY_Hangul_J_RieulPieub           0x0ede
pattern XKB_KEY_Hangul_J_RieulPieub :: KeySymbol
pattern XKB_KEY_Hangul_J_RieulPieub = MkKeySymbol #{const XKB_KEY_Hangul_J_RieulPieub} 
-- #define XKB_KEY_Hangul_J_RieulSios            0x0edf
pattern XKB_KEY_Hangul_J_RieulSios :: KeySymbol
pattern XKB_KEY_Hangul_J_RieulSios = MkKeySymbol #{const XKB_KEY_Hangul_J_RieulSios} 
-- #define XKB_KEY_Hangul_J_RieulTieut           0x0ee0
pattern XKB_KEY_Hangul_J_RieulTieut :: KeySymbol
pattern XKB_KEY_Hangul_J_RieulTieut = MkKeySymbol #{const XKB_KEY_Hangul_J_RieulTieut} 
-- #define XKB_KEY_Hangul_J_RieulPhieuf          0x0ee1
pattern XKB_KEY_Hangul_J_RieulPhieuf :: KeySymbol
pattern XKB_KEY_Hangul_J_RieulPhieuf = MkKeySymbol #{const XKB_KEY_Hangul_J_RieulPhieuf} 
-- #define XKB_KEY_Hangul_J_RieulHieuh           0x0ee2
pattern XKB_KEY_Hangul_J_RieulHieuh :: KeySymbol
pattern XKB_KEY_Hangul_J_RieulHieuh = MkKeySymbol #{const XKB_KEY_Hangul_J_RieulHieuh} 
-- #define XKB_KEY_Hangul_J_Mieum                0x0ee3
pattern XKB_KEY_Hangul_J_Mieum :: KeySymbol
pattern XKB_KEY_Hangul_J_Mieum = MkKeySymbol #{const XKB_KEY_Hangul_J_Mieum} 
-- #define XKB_KEY_Hangul_J_Pieub                0x0ee4
pattern XKB_KEY_Hangul_J_Pieub :: KeySymbol
pattern XKB_KEY_Hangul_J_Pieub = MkKeySymbol #{const XKB_KEY_Hangul_J_Pieub} 
-- #define XKB_KEY_Hangul_J_PieubSios            0x0ee5
pattern XKB_KEY_Hangul_J_PieubSios :: KeySymbol
pattern XKB_KEY_Hangul_J_PieubSios = MkKeySymbol #{const XKB_KEY_Hangul_J_PieubSios} 
-- #define XKB_KEY_Hangul_J_Sios                 0x0ee6
pattern XKB_KEY_Hangul_J_Sios :: KeySymbol
pattern XKB_KEY_Hangul_J_Sios = MkKeySymbol #{const XKB_KEY_Hangul_J_Sios} 
-- #define XKB_KEY_Hangul_J_SsangSios            0x0ee7
pattern XKB_KEY_Hangul_J_SsangSios :: KeySymbol
pattern XKB_KEY_Hangul_J_SsangSios = MkKeySymbol #{const XKB_KEY_Hangul_J_SsangSios} 
-- #define XKB_KEY_Hangul_J_Ieung                0x0ee8
pattern XKB_KEY_Hangul_J_Ieung :: KeySymbol
pattern XKB_KEY_Hangul_J_Ieung = MkKeySymbol #{const XKB_KEY_Hangul_J_Ieung} 
-- #define XKB_KEY_Hangul_J_Jieuj                0x0ee9
pattern XKB_KEY_Hangul_J_Jieuj :: KeySymbol
pattern XKB_KEY_Hangul_J_Jieuj = MkKeySymbol #{const XKB_KEY_Hangul_J_Jieuj} 
-- #define XKB_KEY_Hangul_J_Cieuc                0x0eea
pattern XKB_KEY_Hangul_J_Cieuc :: KeySymbol
pattern XKB_KEY_Hangul_J_Cieuc = MkKeySymbol #{const XKB_KEY_Hangul_J_Cieuc} 
-- #define XKB_KEY_Hangul_J_Khieuq               0x0eeb
pattern XKB_KEY_Hangul_J_Khieuq :: KeySymbol
pattern XKB_KEY_Hangul_J_Khieuq = MkKeySymbol #{const XKB_KEY_Hangul_J_Khieuq} 
-- #define XKB_KEY_Hangul_J_Tieut                0x0eec
pattern XKB_KEY_Hangul_J_Tieut :: KeySymbol
pattern XKB_KEY_Hangul_J_Tieut = MkKeySymbol #{const XKB_KEY_Hangul_J_Tieut} 
-- #define XKB_KEY_Hangul_J_Phieuf               0x0eed
pattern XKB_KEY_Hangul_J_Phieuf :: KeySymbol
pattern XKB_KEY_Hangul_J_Phieuf = MkKeySymbol #{const XKB_KEY_Hangul_J_Phieuf} 
-- #define XKB_KEY_Hangul_J_Hieuh                0x0eee
pattern XKB_KEY_Hangul_J_Hieuh :: KeySymbol
pattern XKB_KEY_Hangul_J_Hieuh = MkKeySymbol #{const XKB_KEY_Hangul_J_Hieuh} 
-- #define XKB_KEY_Hangul_RieulYeorinHieuh       0x0eef
pattern XKB_KEY_Hangul_RieulYeorinHieuh :: KeySymbol
pattern XKB_KEY_Hangul_RieulYeorinHieuh = MkKeySymbol #{const XKB_KEY_Hangul_RieulYeorinHieuh} 
-- #define XKB_KEY_Hangul_SunkyeongeumMieum      0x0ef0
pattern XKB_KEY_Hangul_SunkyeongeumMieum :: KeySymbol
pattern XKB_KEY_Hangul_SunkyeongeumMieum = MkKeySymbol #{const XKB_KEY_Hangul_SunkyeongeumMieum} 
-- #define XKB_KEY_Hangul_SunkyeongeumPieub      0x0ef1
pattern XKB_KEY_Hangul_SunkyeongeumPieub :: KeySymbol
pattern XKB_KEY_Hangul_SunkyeongeumPieub = MkKeySymbol #{const XKB_KEY_Hangul_SunkyeongeumPieub} 
-- #define XKB_KEY_Hangul_PanSios                0x0ef2
pattern XKB_KEY_Hangul_PanSios :: KeySymbol
pattern XKB_KEY_Hangul_PanSios = MkKeySymbol #{const XKB_KEY_Hangul_PanSios} 
-- #define XKB_KEY_Hangul_KkogjiDalrinIeung      0x0ef3
pattern XKB_KEY_Hangul_KkogjiDalrinIeung :: KeySymbol
pattern XKB_KEY_Hangul_KkogjiDalrinIeung = MkKeySymbol #{const XKB_KEY_Hangul_KkogjiDalrinIeung} 
-- #define XKB_KEY_Hangul_SunkyeongeumPhieuf     0x0ef4
pattern XKB_KEY_Hangul_SunkyeongeumPhieuf :: KeySymbol
pattern XKB_KEY_Hangul_SunkyeongeumPhieuf = MkKeySymbol #{const XKB_KEY_Hangul_SunkyeongeumPhieuf} 
-- #define XKB_KEY_Hangul_YeorinHieuh            0x0ef5
pattern XKB_KEY_Hangul_YeorinHieuh :: KeySymbol
pattern XKB_KEY_Hangul_YeorinHieuh = MkKeySymbol #{const XKB_KEY_Hangul_YeorinHieuh} 
-- #define XKB_KEY_Hangul_AraeA                  0x0ef6
pattern XKB_KEY_Hangul_AraeA :: KeySymbol
pattern XKB_KEY_Hangul_AraeA = MkKeySymbol #{const XKB_KEY_Hangul_AraeA} 
-- #define XKB_KEY_Hangul_AraeAE                 0x0ef7
pattern XKB_KEY_Hangul_AraeAE :: KeySymbol
pattern XKB_KEY_Hangul_AraeAE = MkKeySymbol #{const XKB_KEY_Hangul_AraeAE} 
-- #define XKB_KEY_Hangul_J_PanSios              0x0ef8
pattern XKB_KEY_Hangul_J_PanSios :: KeySymbol
pattern XKB_KEY_Hangul_J_PanSios = MkKeySymbol #{const XKB_KEY_Hangul_J_PanSios} 
-- #define XKB_KEY_Hangul_J_KkogjiDalrinIeung    0x0ef9
pattern XKB_KEY_Hangul_J_KkogjiDalrinIeung :: KeySymbol
pattern XKB_KEY_Hangul_J_KkogjiDalrinIeung = MkKeySymbol #{const XKB_KEY_Hangul_J_KkogjiDalrinIeung} 
-- #define XKB_KEY_Hangul_J_YeorinHieuh          0x0efa
pattern XKB_KEY_Hangul_J_YeorinHieuh :: KeySymbol
pattern XKB_KEY_Hangul_J_YeorinHieuh = MkKeySymbol #{const XKB_KEY_Hangul_J_YeorinHieuh} 
-- #define XKB_KEY_Korean_Won                    0x0eff  /*(U+20A9 WON SIGN)*/
pattern XKB_KEY_Korean_Won :: KeySymbol
pattern XKB_KEY_Korean_Won = MkKeySymbol #{const XKB_KEY_Korean_Won} 
-- #define XKB_KEY_Armenian_ligature_ew       0x1000587  /* U+0587 ARMENIAN SMALL LIGATURE ECH YIWN */
pattern XKB_KEY_Armenian_ligature_ew :: KeySymbol
pattern XKB_KEY_Armenian_ligature_ew = MkKeySymbol #{const XKB_KEY_Armenian_ligature_ew} 
-- #define XKB_KEY_Armenian_full_stop         0x1000589  /* U+0589 ARMENIAN FULL STOP */
pattern XKB_KEY_Armenian_full_stop :: KeySymbol
pattern XKB_KEY_Armenian_full_stop = MkKeySymbol #{const XKB_KEY_Armenian_full_stop} 
-- #define XKB_KEY_Armenian_verjaket          0x1000589  /* U+0589 ARMENIAN FULL STOP */
pattern XKB_KEY_Armenian_verjaket :: KeySymbol
pattern XKB_KEY_Armenian_verjaket = MkKeySymbol #{const XKB_KEY_Armenian_verjaket} 
-- #define XKB_KEY_Armenian_separation_mark   0x100055d  /* U+055D ARMENIAN COMMA */
pattern XKB_KEY_Armenian_separation_mark :: KeySymbol
pattern XKB_KEY_Armenian_separation_mark = MkKeySymbol #{const XKB_KEY_Armenian_separation_mark} 
-- #define XKB_KEY_Armenian_but               0x100055d  /* U+055D ARMENIAN COMMA */
pattern XKB_KEY_Armenian_but :: KeySymbol
pattern XKB_KEY_Armenian_but = MkKeySymbol #{const XKB_KEY_Armenian_but} 
-- #define XKB_KEY_Armenian_hyphen            0x100058a  /* U+058A ARMENIAN HYPHEN */
pattern XKB_KEY_Armenian_hyphen :: KeySymbol
pattern XKB_KEY_Armenian_hyphen = MkKeySymbol #{const XKB_KEY_Armenian_hyphen} 
-- #define XKB_KEY_Armenian_yentamna          0x100058a  /* U+058A ARMENIAN HYPHEN */
pattern XKB_KEY_Armenian_yentamna :: KeySymbol
pattern XKB_KEY_Armenian_yentamna = MkKeySymbol #{const XKB_KEY_Armenian_yentamna} 
-- #define XKB_KEY_Armenian_exclam            0x100055c  /* U+055C ARMENIAN EXCLAMATION MARK */
pattern XKB_KEY_Armenian_exclam :: KeySymbol
pattern XKB_KEY_Armenian_exclam = MkKeySymbol #{const XKB_KEY_Armenian_exclam} 
-- #define XKB_KEY_Armenian_amanak            0x100055c  /* U+055C ARMENIAN EXCLAMATION MARK */
pattern XKB_KEY_Armenian_amanak :: KeySymbol
pattern XKB_KEY_Armenian_amanak = MkKeySymbol #{const XKB_KEY_Armenian_amanak} 
-- #define XKB_KEY_Armenian_accent            0x100055b  /* U+055B ARMENIAN EMPHASIS MARK */
pattern XKB_KEY_Armenian_accent :: KeySymbol
pattern XKB_KEY_Armenian_accent = MkKeySymbol #{const XKB_KEY_Armenian_accent} 
-- #define XKB_KEY_Armenian_shesht            0x100055b  /* U+055B ARMENIAN EMPHASIS MARK */
pattern XKB_KEY_Armenian_shesht :: KeySymbol
pattern XKB_KEY_Armenian_shesht = MkKeySymbol #{const XKB_KEY_Armenian_shesht} 
-- #define XKB_KEY_Armenian_question          0x100055e  /* U+055E ARMENIAN QUESTION MARK */
pattern XKB_KEY_Armenian_question :: KeySymbol
pattern XKB_KEY_Armenian_question = MkKeySymbol #{const XKB_KEY_Armenian_question} 
-- #define XKB_KEY_Armenian_paruyk            0x100055e  /* U+055E ARMENIAN QUESTION MARK */
pattern XKB_KEY_Armenian_paruyk :: KeySymbol
pattern XKB_KEY_Armenian_paruyk = MkKeySymbol #{const XKB_KEY_Armenian_paruyk} 
-- #define XKB_KEY_Armenian_AYB               0x1000531  /* U+0531 ARMENIAN CAPITAL LETTER AYB */
pattern XKB_KEY_Armenian_AYB :: KeySymbol
pattern XKB_KEY_Armenian_AYB = MkKeySymbol #{const XKB_KEY_Armenian_AYB} 
-- #define XKB_KEY_Armenian_ayb               0x1000561  /* U+0561 ARMENIAN SMALL LETTER AYB */
pattern XKB_KEY_Armenian_ayb :: KeySymbol
pattern XKB_KEY_Armenian_ayb = MkKeySymbol #{const XKB_KEY_Armenian_ayb} 
-- #define XKB_KEY_Armenian_BEN               0x1000532  /* U+0532 ARMENIAN CAPITAL LETTER BEN */
pattern XKB_KEY_Armenian_BEN :: KeySymbol
pattern XKB_KEY_Armenian_BEN = MkKeySymbol #{const XKB_KEY_Armenian_BEN} 
-- #define XKB_KEY_Armenian_ben               0x1000562  /* U+0562 ARMENIAN SMALL LETTER BEN */
pattern XKB_KEY_Armenian_ben :: KeySymbol
pattern XKB_KEY_Armenian_ben = MkKeySymbol #{const XKB_KEY_Armenian_ben} 
-- #define XKB_KEY_Armenian_GIM               0x1000533  /* U+0533 ARMENIAN CAPITAL LETTER GIM */
pattern XKB_KEY_Armenian_GIM :: KeySymbol
pattern XKB_KEY_Armenian_GIM = MkKeySymbol #{const XKB_KEY_Armenian_GIM} 
-- #define XKB_KEY_Armenian_gim               0x1000563  /* U+0563 ARMENIAN SMALL LETTER GIM */
pattern XKB_KEY_Armenian_gim :: KeySymbol
pattern XKB_KEY_Armenian_gim = MkKeySymbol #{const XKB_KEY_Armenian_gim} 
-- #define XKB_KEY_Armenian_DA                0x1000534  /* U+0534 ARMENIAN CAPITAL LETTER DA */
pattern XKB_KEY_Armenian_DA :: KeySymbol
pattern XKB_KEY_Armenian_DA = MkKeySymbol #{const XKB_KEY_Armenian_DA} 
-- #define XKB_KEY_Armenian_da                0x1000564  /* U+0564 ARMENIAN SMALL LETTER DA */
pattern XKB_KEY_Armenian_da :: KeySymbol
pattern XKB_KEY_Armenian_da = MkKeySymbol #{const XKB_KEY_Armenian_da} 
-- #define XKB_KEY_Armenian_YECH              0x1000535  /* U+0535 ARMENIAN CAPITAL LETTER ECH */
pattern XKB_KEY_Armenian_YECH :: KeySymbol
pattern XKB_KEY_Armenian_YECH = MkKeySymbol #{const XKB_KEY_Armenian_YECH} 
-- #define XKB_KEY_Armenian_yech              0x1000565  /* U+0565 ARMENIAN SMALL LETTER ECH */
pattern XKB_KEY_Armenian_yech :: KeySymbol
pattern XKB_KEY_Armenian_yech = MkKeySymbol #{const XKB_KEY_Armenian_yech} 
-- #define XKB_KEY_Armenian_ZA                0x1000536  /* U+0536 ARMENIAN CAPITAL LETTER ZA */
pattern XKB_KEY_Armenian_ZA :: KeySymbol
pattern XKB_KEY_Armenian_ZA = MkKeySymbol #{const XKB_KEY_Armenian_ZA} 
-- #define XKB_KEY_Armenian_za                0x1000566  /* U+0566 ARMENIAN SMALL LETTER ZA */
pattern XKB_KEY_Armenian_za :: KeySymbol
pattern XKB_KEY_Armenian_za = MkKeySymbol #{const XKB_KEY_Armenian_za} 
-- #define XKB_KEY_Armenian_E                 0x1000537  /* U+0537 ARMENIAN CAPITAL LETTER EH */
pattern XKB_KEY_Armenian_E :: KeySymbol
pattern XKB_KEY_Armenian_E = MkKeySymbol #{const XKB_KEY_Armenian_E} 
-- #define XKB_KEY_Armenian_e                 0x1000567  /* U+0567 ARMENIAN SMALL LETTER EH */
pattern XKB_KEY_Armenian_e :: KeySymbol
pattern XKB_KEY_Armenian_e = MkKeySymbol #{const XKB_KEY_Armenian_e} 
-- #define XKB_KEY_Armenian_AT                0x1000538  /* U+0538 ARMENIAN CAPITAL LETTER ET */
pattern XKB_KEY_Armenian_AT :: KeySymbol
pattern XKB_KEY_Armenian_AT = MkKeySymbol #{const XKB_KEY_Armenian_AT} 
-- #define XKB_KEY_Armenian_at                0x1000568  /* U+0568 ARMENIAN SMALL LETTER ET */
pattern XKB_KEY_Armenian_at :: KeySymbol
pattern XKB_KEY_Armenian_at = MkKeySymbol #{const XKB_KEY_Armenian_at} 
-- #define XKB_KEY_Armenian_TO                0x1000539  /* U+0539 ARMENIAN CAPITAL LETTER TO */
pattern XKB_KEY_Armenian_TO :: KeySymbol
pattern XKB_KEY_Armenian_TO = MkKeySymbol #{const XKB_KEY_Armenian_TO} 
-- #define XKB_KEY_Armenian_to                0x1000569  /* U+0569 ARMENIAN SMALL LETTER TO */
pattern XKB_KEY_Armenian_to :: KeySymbol
pattern XKB_KEY_Armenian_to = MkKeySymbol #{const XKB_KEY_Armenian_to} 
-- #define XKB_KEY_Armenian_ZHE               0x100053a  /* U+053A ARMENIAN CAPITAL LETTER ZHE */
pattern XKB_KEY_Armenian_ZHE :: KeySymbol
pattern XKB_KEY_Armenian_ZHE = MkKeySymbol #{const XKB_KEY_Armenian_ZHE} 
-- #define XKB_KEY_Armenian_zhe               0x100056a  /* U+056A ARMENIAN SMALL LETTER ZHE */
pattern XKB_KEY_Armenian_zhe :: KeySymbol
pattern XKB_KEY_Armenian_zhe = MkKeySymbol #{const XKB_KEY_Armenian_zhe} 
-- #define XKB_KEY_Armenian_INI               0x100053b  /* U+053B ARMENIAN CAPITAL LETTER INI */
pattern XKB_KEY_Armenian_INI :: KeySymbol
pattern XKB_KEY_Armenian_INI = MkKeySymbol #{const XKB_KEY_Armenian_INI} 
-- #define XKB_KEY_Armenian_ini               0x100056b  /* U+056B ARMENIAN SMALL LETTER INI */
pattern XKB_KEY_Armenian_ini :: KeySymbol
pattern XKB_KEY_Armenian_ini = MkKeySymbol #{const XKB_KEY_Armenian_ini} 
-- #define XKB_KEY_Armenian_LYUN              0x100053c  /* U+053C ARMENIAN CAPITAL LETTER LIWN */
pattern XKB_KEY_Armenian_LYUN :: KeySymbol
pattern XKB_KEY_Armenian_LYUN = MkKeySymbol #{const XKB_KEY_Armenian_LYUN} 
-- #define XKB_KEY_Armenian_lyun              0x100056c  /* U+056C ARMENIAN SMALL LETTER LIWN */
pattern XKB_KEY_Armenian_lyun :: KeySymbol
pattern XKB_KEY_Armenian_lyun = MkKeySymbol #{const XKB_KEY_Armenian_lyun} 
-- #define XKB_KEY_Armenian_KHE               0x100053d  /* U+053D ARMENIAN CAPITAL LETTER XEH */
pattern XKB_KEY_Armenian_KHE :: KeySymbol
pattern XKB_KEY_Armenian_KHE = MkKeySymbol #{const XKB_KEY_Armenian_KHE} 
-- #define XKB_KEY_Armenian_khe               0x100056d  /* U+056D ARMENIAN SMALL LETTER XEH */
pattern XKB_KEY_Armenian_khe :: KeySymbol
pattern XKB_KEY_Armenian_khe = MkKeySymbol #{const XKB_KEY_Armenian_khe} 
-- #define XKB_KEY_Armenian_TSA               0x100053e  /* U+053E ARMENIAN CAPITAL LETTER CA */
pattern XKB_KEY_Armenian_TSA :: KeySymbol
pattern XKB_KEY_Armenian_TSA = MkKeySymbol #{const XKB_KEY_Armenian_TSA} 
-- #define XKB_KEY_Armenian_tsa               0x100056e  /* U+056E ARMENIAN SMALL LETTER CA */
pattern XKB_KEY_Armenian_tsa :: KeySymbol
pattern XKB_KEY_Armenian_tsa = MkKeySymbol #{const XKB_KEY_Armenian_tsa} 
-- #define XKB_KEY_Armenian_KEN               0x100053f  /* U+053F ARMENIAN CAPITAL LETTER KEN */
pattern XKB_KEY_Armenian_KEN :: KeySymbol
pattern XKB_KEY_Armenian_KEN = MkKeySymbol #{const XKB_KEY_Armenian_KEN} 
-- #define XKB_KEY_Armenian_ken               0x100056f  /* U+056F ARMENIAN SMALL LETTER KEN */
pattern XKB_KEY_Armenian_ken :: KeySymbol
pattern XKB_KEY_Armenian_ken = MkKeySymbol #{const XKB_KEY_Armenian_ken} 
-- #define XKB_KEY_Armenian_HO                0x1000540  /* U+0540 ARMENIAN CAPITAL LETTER HO */
pattern XKB_KEY_Armenian_HO :: KeySymbol
pattern XKB_KEY_Armenian_HO = MkKeySymbol #{const XKB_KEY_Armenian_HO} 
-- #define XKB_KEY_Armenian_ho                0x1000570  /* U+0570 ARMENIAN SMALL LETTER HO */
pattern XKB_KEY_Armenian_ho :: KeySymbol
pattern XKB_KEY_Armenian_ho = MkKeySymbol #{const XKB_KEY_Armenian_ho} 
-- #define XKB_KEY_Armenian_DZA               0x1000541  /* U+0541 ARMENIAN CAPITAL LETTER JA */
pattern XKB_KEY_Armenian_DZA :: KeySymbol
pattern XKB_KEY_Armenian_DZA = MkKeySymbol #{const XKB_KEY_Armenian_DZA} 
-- #define XKB_KEY_Armenian_dza               0x1000571  /* U+0571 ARMENIAN SMALL LETTER JA */
pattern XKB_KEY_Armenian_dza :: KeySymbol
pattern XKB_KEY_Armenian_dza = MkKeySymbol #{const XKB_KEY_Armenian_dza} 
-- #define XKB_KEY_Armenian_GHAT              0x1000542  /* U+0542 ARMENIAN CAPITAL LETTER GHAD */
pattern XKB_KEY_Armenian_GHAT :: KeySymbol
pattern XKB_KEY_Armenian_GHAT = MkKeySymbol #{const XKB_KEY_Armenian_GHAT} 
-- #define XKB_KEY_Armenian_ghat              0x1000572  /* U+0572 ARMENIAN SMALL LETTER GHAD */
pattern XKB_KEY_Armenian_ghat :: KeySymbol
pattern XKB_KEY_Armenian_ghat = MkKeySymbol #{const XKB_KEY_Armenian_ghat} 
-- #define XKB_KEY_Armenian_TCHE              0x1000543  /* U+0543 ARMENIAN CAPITAL LETTER CHEH */
pattern XKB_KEY_Armenian_TCHE :: KeySymbol
pattern XKB_KEY_Armenian_TCHE = MkKeySymbol #{const XKB_KEY_Armenian_TCHE} 
-- #define XKB_KEY_Armenian_tche              0x1000573  /* U+0573 ARMENIAN SMALL LETTER CHEH */
pattern XKB_KEY_Armenian_tche :: KeySymbol
pattern XKB_KEY_Armenian_tche = MkKeySymbol #{const XKB_KEY_Armenian_tche} 
-- #define XKB_KEY_Armenian_MEN               0x1000544  /* U+0544 ARMENIAN CAPITAL LETTER MEN */
pattern XKB_KEY_Armenian_MEN :: KeySymbol
pattern XKB_KEY_Armenian_MEN = MkKeySymbol #{const XKB_KEY_Armenian_MEN} 
-- #define XKB_KEY_Armenian_men               0x1000574  /* U+0574 ARMENIAN SMALL LETTER MEN */
pattern XKB_KEY_Armenian_men :: KeySymbol
pattern XKB_KEY_Armenian_men = MkKeySymbol #{const XKB_KEY_Armenian_men} 
-- #define XKB_KEY_Armenian_HI                0x1000545  /* U+0545 ARMENIAN CAPITAL LETTER YI */
pattern XKB_KEY_Armenian_HI :: KeySymbol
pattern XKB_KEY_Armenian_HI = MkKeySymbol #{const XKB_KEY_Armenian_HI} 
-- #define XKB_KEY_Armenian_hi                0x1000575  /* U+0575 ARMENIAN SMALL LETTER YI */
pattern XKB_KEY_Armenian_hi :: KeySymbol
pattern XKB_KEY_Armenian_hi = MkKeySymbol #{const XKB_KEY_Armenian_hi} 
-- #define XKB_KEY_Armenian_NU                0x1000546  /* U+0546 ARMENIAN CAPITAL LETTER NOW */
pattern XKB_KEY_Armenian_NU :: KeySymbol
pattern XKB_KEY_Armenian_NU = MkKeySymbol #{const XKB_KEY_Armenian_NU} 
-- #define XKB_KEY_Armenian_nu                0x1000576  /* U+0576 ARMENIAN SMALL LETTER NOW */
pattern XKB_KEY_Armenian_nu :: KeySymbol
pattern XKB_KEY_Armenian_nu = MkKeySymbol #{const XKB_KEY_Armenian_nu} 
-- #define XKB_KEY_Armenian_SHA               0x1000547  /* U+0547 ARMENIAN CAPITAL LETTER SHA */
pattern XKB_KEY_Armenian_SHA :: KeySymbol
pattern XKB_KEY_Armenian_SHA = MkKeySymbol #{const XKB_KEY_Armenian_SHA} 
-- #define XKB_KEY_Armenian_sha               0x1000577  /* U+0577 ARMENIAN SMALL LETTER SHA */
pattern XKB_KEY_Armenian_sha :: KeySymbol
pattern XKB_KEY_Armenian_sha = MkKeySymbol #{const XKB_KEY_Armenian_sha} 
-- #define XKB_KEY_Armenian_VO                0x1000548  /* U+0548 ARMENIAN CAPITAL LETTER VO */
pattern XKB_KEY_Armenian_VO :: KeySymbol
pattern XKB_KEY_Armenian_VO = MkKeySymbol #{const XKB_KEY_Armenian_VO} 
-- #define XKB_KEY_Armenian_vo                0x1000578  /* U+0578 ARMENIAN SMALL LETTER VO */
pattern XKB_KEY_Armenian_vo :: KeySymbol
pattern XKB_KEY_Armenian_vo = MkKeySymbol #{const XKB_KEY_Armenian_vo} 
-- #define XKB_KEY_Armenian_CHA               0x1000549  /* U+0549 ARMENIAN CAPITAL LETTER CHA */
pattern XKB_KEY_Armenian_CHA :: KeySymbol
pattern XKB_KEY_Armenian_CHA = MkKeySymbol #{const XKB_KEY_Armenian_CHA} 
-- #define XKB_KEY_Armenian_cha               0x1000579  /* U+0579 ARMENIAN SMALL LETTER CHA */
pattern XKB_KEY_Armenian_cha :: KeySymbol
pattern XKB_KEY_Armenian_cha = MkKeySymbol #{const XKB_KEY_Armenian_cha} 
-- #define XKB_KEY_Armenian_PE                0x100054a  /* U+054A ARMENIAN CAPITAL LETTER PEH */
pattern XKB_KEY_Armenian_PE :: KeySymbol
pattern XKB_KEY_Armenian_PE = MkKeySymbol #{const XKB_KEY_Armenian_PE} 
-- #define XKB_KEY_Armenian_pe                0x100057a  /* U+057A ARMENIAN SMALL LETTER PEH */
pattern XKB_KEY_Armenian_pe :: KeySymbol
pattern XKB_KEY_Armenian_pe = MkKeySymbol #{const XKB_KEY_Armenian_pe} 
-- #define XKB_KEY_Armenian_JE                0x100054b  /* U+054B ARMENIAN CAPITAL LETTER JHEH */
pattern XKB_KEY_Armenian_JE :: KeySymbol
pattern XKB_KEY_Armenian_JE = MkKeySymbol #{const XKB_KEY_Armenian_JE} 
-- #define XKB_KEY_Armenian_je                0x100057b  /* U+057B ARMENIAN SMALL LETTER JHEH */
pattern XKB_KEY_Armenian_je :: KeySymbol
pattern XKB_KEY_Armenian_je = MkKeySymbol #{const XKB_KEY_Armenian_je} 
-- #define XKB_KEY_Armenian_RA                0x100054c  /* U+054C ARMENIAN CAPITAL LETTER RA */
pattern XKB_KEY_Armenian_RA :: KeySymbol
pattern XKB_KEY_Armenian_RA = MkKeySymbol #{const XKB_KEY_Armenian_RA} 
-- #define XKB_KEY_Armenian_ra                0x100057c  /* U+057C ARMENIAN SMALL LETTER RA */
pattern XKB_KEY_Armenian_ra :: KeySymbol
pattern XKB_KEY_Armenian_ra = MkKeySymbol #{const XKB_KEY_Armenian_ra} 
-- #define XKB_KEY_Armenian_SE                0x100054d  /* U+054D ARMENIAN CAPITAL LETTER SEH */
pattern XKB_KEY_Armenian_SE :: KeySymbol
pattern XKB_KEY_Armenian_SE = MkKeySymbol #{const XKB_KEY_Armenian_SE} 
-- #define XKB_KEY_Armenian_se                0x100057d  /* U+057D ARMENIAN SMALL LETTER SEH */
pattern XKB_KEY_Armenian_se :: KeySymbol
pattern XKB_KEY_Armenian_se = MkKeySymbol #{const XKB_KEY_Armenian_se} 
-- #define XKB_KEY_Armenian_VEV               0x100054e  /* U+054E ARMENIAN CAPITAL LETTER VEW */
pattern XKB_KEY_Armenian_VEV :: KeySymbol
pattern XKB_KEY_Armenian_VEV = MkKeySymbol #{const XKB_KEY_Armenian_VEV} 
-- #define XKB_KEY_Armenian_vev               0x100057e  /* U+057E ARMENIAN SMALL LETTER VEW */
pattern XKB_KEY_Armenian_vev :: KeySymbol
pattern XKB_KEY_Armenian_vev = MkKeySymbol #{const XKB_KEY_Armenian_vev} 
-- #define XKB_KEY_Armenian_TYUN              0x100054f  /* U+054F ARMENIAN CAPITAL LETTER TIWN */
pattern XKB_KEY_Armenian_TYUN :: KeySymbol
pattern XKB_KEY_Armenian_TYUN = MkKeySymbol #{const XKB_KEY_Armenian_TYUN} 
-- #define XKB_KEY_Armenian_tyun              0x100057f  /* U+057F ARMENIAN SMALL LETTER TIWN */
pattern XKB_KEY_Armenian_tyun :: KeySymbol
pattern XKB_KEY_Armenian_tyun = MkKeySymbol #{const XKB_KEY_Armenian_tyun} 
-- #define XKB_KEY_Armenian_RE                0x1000550  /* U+0550 ARMENIAN CAPITAL LETTER REH */
pattern XKB_KEY_Armenian_RE :: KeySymbol
pattern XKB_KEY_Armenian_RE = MkKeySymbol #{const XKB_KEY_Armenian_RE} 
-- #define XKB_KEY_Armenian_re                0x1000580  /* U+0580 ARMENIAN SMALL LETTER REH */
pattern XKB_KEY_Armenian_re :: KeySymbol
pattern XKB_KEY_Armenian_re = MkKeySymbol #{const XKB_KEY_Armenian_re} 
-- #define XKB_KEY_Armenian_TSO               0x1000551  /* U+0551 ARMENIAN CAPITAL LETTER CO */
pattern XKB_KEY_Armenian_TSO :: KeySymbol
pattern XKB_KEY_Armenian_TSO = MkKeySymbol #{const XKB_KEY_Armenian_TSO} 
-- #define XKB_KEY_Armenian_tso               0x1000581  /* U+0581 ARMENIAN SMALL LETTER CO */
pattern XKB_KEY_Armenian_tso :: KeySymbol
pattern XKB_KEY_Armenian_tso = MkKeySymbol #{const XKB_KEY_Armenian_tso} 
-- #define XKB_KEY_Armenian_VYUN              0x1000552  /* U+0552 ARMENIAN CAPITAL LETTER YIWN */
pattern XKB_KEY_Armenian_VYUN :: KeySymbol
pattern XKB_KEY_Armenian_VYUN = MkKeySymbol #{const XKB_KEY_Armenian_VYUN} 
-- #define XKB_KEY_Armenian_vyun              0x1000582  /* U+0582 ARMENIAN SMALL LETTER YIWN */
pattern XKB_KEY_Armenian_vyun :: KeySymbol
pattern XKB_KEY_Armenian_vyun = MkKeySymbol #{const XKB_KEY_Armenian_vyun} 
-- #define XKB_KEY_Armenian_PYUR              0x1000553  /* U+0553 ARMENIAN CAPITAL LETTER PIWR */
pattern XKB_KEY_Armenian_PYUR :: KeySymbol
pattern XKB_KEY_Armenian_PYUR = MkKeySymbol #{const XKB_KEY_Armenian_PYUR} 
-- #define XKB_KEY_Armenian_pyur              0x1000583  /* U+0583 ARMENIAN SMALL LETTER PIWR */
pattern XKB_KEY_Armenian_pyur :: KeySymbol
pattern XKB_KEY_Armenian_pyur = MkKeySymbol #{const XKB_KEY_Armenian_pyur} 
-- #define XKB_KEY_Armenian_KE                0x1000554  /* U+0554 ARMENIAN CAPITAL LETTER KEH */
pattern XKB_KEY_Armenian_KE :: KeySymbol
pattern XKB_KEY_Armenian_KE = MkKeySymbol #{const XKB_KEY_Armenian_KE} 
-- #define XKB_KEY_Armenian_ke                0x1000584  /* U+0584 ARMENIAN SMALL LETTER KEH */
pattern XKB_KEY_Armenian_ke :: KeySymbol
pattern XKB_KEY_Armenian_ke = MkKeySymbol #{const XKB_KEY_Armenian_ke} 
-- #define XKB_KEY_Armenian_O                 0x1000555  /* U+0555 ARMENIAN CAPITAL LETTER OH */
pattern XKB_KEY_Armenian_O :: KeySymbol
pattern XKB_KEY_Armenian_O = MkKeySymbol #{const XKB_KEY_Armenian_O} 
-- #define XKB_KEY_Armenian_o                 0x1000585  /* U+0585 ARMENIAN SMALL LETTER OH */
pattern XKB_KEY_Armenian_o :: KeySymbol
pattern XKB_KEY_Armenian_o = MkKeySymbol #{const XKB_KEY_Armenian_o} 
-- #define XKB_KEY_Armenian_FE                0x1000556  /* U+0556 ARMENIAN CAPITAL LETTER FEH */
pattern XKB_KEY_Armenian_FE :: KeySymbol
pattern XKB_KEY_Armenian_FE = MkKeySymbol #{const XKB_KEY_Armenian_FE} 
-- #define XKB_KEY_Armenian_fe                0x1000586  /* U+0586 ARMENIAN SMALL LETTER FEH */
pattern XKB_KEY_Armenian_fe :: KeySymbol
pattern XKB_KEY_Armenian_fe = MkKeySymbol #{const XKB_KEY_Armenian_fe} 
-- #define XKB_KEY_Armenian_apostrophe        0x100055a  /* U+055A ARMENIAN APOSTROPHE */
pattern XKB_KEY_Armenian_apostrophe :: KeySymbol
pattern XKB_KEY_Armenian_apostrophe = MkKeySymbol #{const XKB_KEY_Armenian_apostrophe} 
-- #define XKB_KEY_Georgian_an                0x10010d0  /* U+10D0 GEORGIAN LETTER AN */
pattern XKB_KEY_Georgian_an :: KeySymbol
pattern XKB_KEY_Georgian_an = MkKeySymbol #{const XKB_KEY_Georgian_an} 
-- #define XKB_KEY_Georgian_ban               0x10010d1  /* U+10D1 GEORGIAN LETTER BAN */
pattern XKB_KEY_Georgian_ban :: KeySymbol
pattern XKB_KEY_Georgian_ban = MkKeySymbol #{const XKB_KEY_Georgian_ban} 
-- #define XKB_KEY_Georgian_gan               0x10010d2  /* U+10D2 GEORGIAN LETTER GAN */
pattern XKB_KEY_Georgian_gan :: KeySymbol
pattern XKB_KEY_Georgian_gan = MkKeySymbol #{const XKB_KEY_Georgian_gan} 
-- #define XKB_KEY_Georgian_don               0x10010d3  /* U+10D3 GEORGIAN LETTER DON */
pattern XKB_KEY_Georgian_don :: KeySymbol
pattern XKB_KEY_Georgian_don = MkKeySymbol #{const XKB_KEY_Georgian_don} 
-- #define XKB_KEY_Georgian_en                0x10010d4  /* U+10D4 GEORGIAN LETTER EN */
pattern XKB_KEY_Georgian_en :: KeySymbol
pattern XKB_KEY_Georgian_en = MkKeySymbol #{const XKB_KEY_Georgian_en} 
-- #define XKB_KEY_Georgian_vin               0x10010d5  /* U+10D5 GEORGIAN LETTER VIN */
pattern XKB_KEY_Georgian_vin :: KeySymbol
pattern XKB_KEY_Georgian_vin = MkKeySymbol #{const XKB_KEY_Georgian_vin} 
-- #define XKB_KEY_Georgian_zen               0x10010d6  /* U+10D6 GEORGIAN LETTER ZEN */
pattern XKB_KEY_Georgian_zen :: KeySymbol
pattern XKB_KEY_Georgian_zen = MkKeySymbol #{const XKB_KEY_Georgian_zen} 
-- #define XKB_KEY_Georgian_tan               0x10010d7  /* U+10D7 GEORGIAN LETTER TAN */
pattern XKB_KEY_Georgian_tan :: KeySymbol
pattern XKB_KEY_Georgian_tan = MkKeySymbol #{const XKB_KEY_Georgian_tan} 
-- #define XKB_KEY_Georgian_in                0x10010d8  /* U+10D8 GEORGIAN LETTER IN */
pattern XKB_KEY_Georgian_in :: KeySymbol
pattern XKB_KEY_Georgian_in = MkKeySymbol #{const XKB_KEY_Georgian_in} 
-- #define XKB_KEY_Georgian_kan               0x10010d9  /* U+10D9 GEORGIAN LETTER KAN */
pattern XKB_KEY_Georgian_kan :: KeySymbol
pattern XKB_KEY_Georgian_kan = MkKeySymbol #{const XKB_KEY_Georgian_kan} 
-- #define XKB_KEY_Georgian_las               0x10010da  /* U+10DA GEORGIAN LETTER LAS */
pattern XKB_KEY_Georgian_las :: KeySymbol
pattern XKB_KEY_Georgian_las = MkKeySymbol #{const XKB_KEY_Georgian_las} 
-- #define XKB_KEY_Georgian_man               0x10010db  /* U+10DB GEORGIAN LETTER MAN */
pattern XKB_KEY_Georgian_man :: KeySymbol
pattern XKB_KEY_Georgian_man = MkKeySymbol #{const XKB_KEY_Georgian_man} 
-- #define XKB_KEY_Georgian_nar               0x10010dc  /* U+10DC GEORGIAN LETTER NAR */
pattern XKB_KEY_Georgian_nar :: KeySymbol
pattern XKB_KEY_Georgian_nar = MkKeySymbol #{const XKB_KEY_Georgian_nar} 
-- #define XKB_KEY_Georgian_on                0x10010dd  /* U+10DD GEORGIAN LETTER ON */
pattern XKB_KEY_Georgian_on :: KeySymbol
pattern XKB_KEY_Georgian_on = MkKeySymbol #{const XKB_KEY_Georgian_on} 
-- #define XKB_KEY_Georgian_par               0x10010de  /* U+10DE GEORGIAN LETTER PAR */
pattern XKB_KEY_Georgian_par :: KeySymbol
pattern XKB_KEY_Georgian_par = MkKeySymbol #{const XKB_KEY_Georgian_par} 
-- #define XKB_KEY_Georgian_zhar              0x10010df  /* U+10DF GEORGIAN LETTER ZHAR */
pattern XKB_KEY_Georgian_zhar :: KeySymbol
pattern XKB_KEY_Georgian_zhar = MkKeySymbol #{const XKB_KEY_Georgian_zhar} 
-- #define XKB_KEY_Georgian_rae               0x10010e0  /* U+10E0 GEORGIAN LETTER RAE */
pattern XKB_KEY_Georgian_rae :: KeySymbol
pattern XKB_KEY_Georgian_rae = MkKeySymbol #{const XKB_KEY_Georgian_rae} 
-- #define XKB_KEY_Georgian_san               0x10010e1  /* U+10E1 GEORGIAN LETTER SAN */
pattern XKB_KEY_Georgian_san :: KeySymbol
pattern XKB_KEY_Georgian_san = MkKeySymbol #{const XKB_KEY_Georgian_san} 
-- #define XKB_KEY_Georgian_tar               0x10010e2  /* U+10E2 GEORGIAN LETTER TAR */
pattern XKB_KEY_Georgian_tar :: KeySymbol
pattern XKB_KEY_Georgian_tar = MkKeySymbol #{const XKB_KEY_Georgian_tar} 
-- #define XKB_KEY_Georgian_un                0x10010e3  /* U+10E3 GEORGIAN LETTER UN */
pattern XKB_KEY_Georgian_un :: KeySymbol
pattern XKB_KEY_Georgian_un = MkKeySymbol #{const XKB_KEY_Georgian_un} 
-- #define XKB_KEY_Georgian_phar              0x10010e4  /* U+10E4 GEORGIAN LETTER PHAR */
pattern XKB_KEY_Georgian_phar :: KeySymbol
pattern XKB_KEY_Georgian_phar = MkKeySymbol #{const XKB_KEY_Georgian_phar} 
-- #define XKB_KEY_Georgian_khar              0x10010e5  /* U+10E5 GEORGIAN LETTER KHAR */
pattern XKB_KEY_Georgian_khar :: KeySymbol
pattern XKB_KEY_Georgian_khar = MkKeySymbol #{const XKB_KEY_Georgian_khar} 
-- #define XKB_KEY_Georgian_ghan              0x10010e6  /* U+10E6 GEORGIAN LETTER GHAN */
pattern XKB_KEY_Georgian_ghan :: KeySymbol
pattern XKB_KEY_Georgian_ghan = MkKeySymbol #{const XKB_KEY_Georgian_ghan} 
-- #define XKB_KEY_Georgian_qar               0x10010e7  /* U+10E7 GEORGIAN LETTER QAR */
pattern XKB_KEY_Georgian_qar :: KeySymbol
pattern XKB_KEY_Georgian_qar = MkKeySymbol #{const XKB_KEY_Georgian_qar} 
-- #define XKB_KEY_Georgian_shin              0x10010e8  /* U+10E8 GEORGIAN LETTER SHIN */
pattern XKB_KEY_Georgian_shin :: KeySymbol
pattern XKB_KEY_Georgian_shin = MkKeySymbol #{const XKB_KEY_Georgian_shin} 
-- #define XKB_KEY_Georgian_chin              0x10010e9  /* U+10E9 GEORGIAN LETTER CHIN */
pattern XKB_KEY_Georgian_chin :: KeySymbol
pattern XKB_KEY_Georgian_chin = MkKeySymbol #{const XKB_KEY_Georgian_chin} 
-- #define XKB_KEY_Georgian_can               0x10010ea  /* U+10EA GEORGIAN LETTER CAN */
pattern XKB_KEY_Georgian_can :: KeySymbol
pattern XKB_KEY_Georgian_can = MkKeySymbol #{const XKB_KEY_Georgian_can} 
-- #define XKB_KEY_Georgian_jil               0x10010eb  /* U+10EB GEORGIAN LETTER JIL */
pattern XKB_KEY_Georgian_jil :: KeySymbol
pattern XKB_KEY_Georgian_jil = MkKeySymbol #{const XKB_KEY_Georgian_jil} 
-- #define XKB_KEY_Georgian_cil               0x10010ec  /* U+10EC GEORGIAN LETTER CIL */
pattern XKB_KEY_Georgian_cil :: KeySymbol
pattern XKB_KEY_Georgian_cil = MkKeySymbol #{const XKB_KEY_Georgian_cil} 
-- #define XKB_KEY_Georgian_char              0x10010ed  /* U+10ED GEORGIAN LETTER CHAR */
pattern XKB_KEY_Georgian_char :: KeySymbol
pattern XKB_KEY_Georgian_char = MkKeySymbol #{const XKB_KEY_Georgian_char} 
-- #define XKB_KEY_Georgian_xan               0x10010ee  /* U+10EE GEORGIAN LETTER XAN */
pattern XKB_KEY_Georgian_xan :: KeySymbol
pattern XKB_KEY_Georgian_xan = MkKeySymbol #{const XKB_KEY_Georgian_xan} 
-- #define XKB_KEY_Georgian_jhan              0x10010ef  /* U+10EF GEORGIAN LETTER JHAN */
pattern XKB_KEY_Georgian_jhan :: KeySymbol
pattern XKB_KEY_Georgian_jhan = MkKeySymbol #{const XKB_KEY_Georgian_jhan} 
-- #define XKB_KEY_Georgian_hae               0x10010f0  /* U+10F0 GEORGIAN LETTER HAE */
pattern XKB_KEY_Georgian_hae :: KeySymbol
pattern XKB_KEY_Georgian_hae = MkKeySymbol #{const XKB_KEY_Georgian_hae} 
-- #define XKB_KEY_Georgian_he                0x10010f1  /* U+10F1 GEORGIAN LETTER HE */
pattern XKB_KEY_Georgian_he :: KeySymbol
pattern XKB_KEY_Georgian_he = MkKeySymbol #{const XKB_KEY_Georgian_he} 
-- #define XKB_KEY_Georgian_hie               0x10010f2  /* U+10F2 GEORGIAN LETTER HIE */
pattern XKB_KEY_Georgian_hie :: KeySymbol
pattern XKB_KEY_Georgian_hie = MkKeySymbol #{const XKB_KEY_Georgian_hie} 
-- #define XKB_KEY_Georgian_we                0x10010f3  /* U+10F3 GEORGIAN LETTER WE */
pattern XKB_KEY_Georgian_we :: KeySymbol
pattern XKB_KEY_Georgian_we = MkKeySymbol #{const XKB_KEY_Georgian_we} 
-- #define XKB_KEY_Georgian_har               0x10010f4  /* U+10F4 GEORGIAN LETTER HAR */
pattern XKB_KEY_Georgian_har :: KeySymbol
pattern XKB_KEY_Georgian_har = MkKeySymbol #{const XKB_KEY_Georgian_har} 
-- #define XKB_KEY_Georgian_hoe               0x10010f5  /* U+10F5 GEORGIAN LETTER HOE */
pattern XKB_KEY_Georgian_hoe :: KeySymbol
pattern XKB_KEY_Georgian_hoe = MkKeySymbol #{const XKB_KEY_Georgian_hoe} 
-- #define XKB_KEY_Georgian_fi                0x10010f6  /* U+10F6 GEORGIAN LETTER FI */
pattern XKB_KEY_Georgian_fi :: KeySymbol
pattern XKB_KEY_Georgian_fi = MkKeySymbol #{const XKB_KEY_Georgian_fi} 
-- #define XKB_KEY_Xabovedot                  0x1001e8a  /* U+1E8A LATIN CAPITAL LETTER X WITH DOT ABOVE */
pattern XKB_KEY_Xabovedot :: KeySymbol
pattern XKB_KEY_Xabovedot = MkKeySymbol #{const XKB_KEY_Xabovedot} 
-- #define XKB_KEY_Ibreve                     0x100012c  /* U+012C LATIN CAPITAL LETTER I WITH BREVE */
pattern XKB_KEY_Ibreve :: KeySymbol
pattern XKB_KEY_Ibreve = MkKeySymbol #{const XKB_KEY_Ibreve} 
-- #define XKB_KEY_Zstroke                    0x10001b5  /* U+01B5 LATIN CAPITAL LETTER Z WITH STROKE */
pattern XKB_KEY_Zstroke :: KeySymbol
pattern XKB_KEY_Zstroke = MkKeySymbol #{const XKB_KEY_Zstroke} 
-- #define XKB_KEY_Gcaron                     0x10001e6  /* U+01E6 LATIN CAPITAL LETTER G WITH CARON */
pattern XKB_KEY_Gcaron :: KeySymbol
pattern XKB_KEY_Gcaron = MkKeySymbol #{const XKB_KEY_Gcaron} 
-- #define XKB_KEY_Ocaron                     0x10001d1  /* U+01D2 LATIN CAPITAL LETTER O WITH CARON */
pattern XKB_KEY_Ocaron :: KeySymbol
pattern XKB_KEY_Ocaron = MkKeySymbol #{const XKB_KEY_Ocaron} 
-- #define XKB_KEY_Obarred                    0x100019f  /* U+019F LATIN CAPITAL LETTER O WITH MIDDLE TILDE */
pattern XKB_KEY_Obarred :: KeySymbol
pattern XKB_KEY_Obarred = MkKeySymbol #{const XKB_KEY_Obarred} 
-- #define XKB_KEY_xabovedot                  0x1001e8b  /* U+1E8B LATIN SMALL LETTER X WITH DOT ABOVE */
pattern XKB_KEY_xabovedot :: KeySymbol
pattern XKB_KEY_xabovedot = MkKeySymbol #{const XKB_KEY_xabovedot} 
-- #define XKB_KEY_ibreve                     0x100012d  /* U+012D LATIN SMALL LETTER I WITH BREVE */
pattern XKB_KEY_ibreve :: KeySymbol
pattern XKB_KEY_ibreve = MkKeySymbol #{const XKB_KEY_ibreve} 
-- #define XKB_KEY_zstroke                    0x10001b6  /* U+01B6 LATIN SMALL LETTER Z WITH STROKE */
pattern XKB_KEY_zstroke :: KeySymbol
pattern XKB_KEY_zstroke = MkKeySymbol #{const XKB_KEY_zstroke} 
-- #define XKB_KEY_gcaron                     0x10001e7  /* U+01E7 LATIN SMALL LETTER G WITH CARON */
pattern XKB_KEY_gcaron :: KeySymbol
pattern XKB_KEY_gcaron = MkKeySymbol #{const XKB_KEY_gcaron} 
-- #define XKB_KEY_ocaron                     0x10001d2  /* U+01D2 LATIN SMALL LETTER O WITH CARON */
pattern XKB_KEY_ocaron :: KeySymbol
pattern XKB_KEY_ocaron = MkKeySymbol #{const XKB_KEY_ocaron} 
-- #define XKB_KEY_obarred                    0x1000275  /* U+0275 LATIN SMALL LETTER BARRED O */
pattern XKB_KEY_obarred :: KeySymbol
pattern XKB_KEY_obarred = MkKeySymbol #{const XKB_KEY_obarred} 
-- #define XKB_KEY_SCHWA                      0x100018f  /* U+018F LATIN CAPITAL LETTER SCHWA */
pattern XKB_KEY_SCHWA :: KeySymbol
pattern XKB_KEY_SCHWA = MkKeySymbol #{const XKB_KEY_SCHWA} 
-- #define XKB_KEY_schwa                      0x1000259  /* U+0259 LATIN SMALL LETTER SCHWA */
pattern XKB_KEY_schwa :: KeySymbol
pattern XKB_KEY_schwa = MkKeySymbol #{const XKB_KEY_schwa} 
-- #define XKB_KEY_EZH                        0x10001b7  /* U+01B7 LATIN CAPITAL LETTER EZH */
pattern XKB_KEY_EZH :: KeySymbol
pattern XKB_KEY_EZH = MkKeySymbol #{const XKB_KEY_EZH} 
-- #define XKB_KEY_ezh                        0x1000292  /* U+0292 LATIN SMALL LETTER EZH */
pattern XKB_KEY_ezh :: KeySymbol
pattern XKB_KEY_ezh = MkKeySymbol #{const XKB_KEY_ezh} 
-- #define XKB_KEY_Lbelowdot                  0x1001e36  /* U+1E36 LATIN CAPITAL LETTER L WITH DOT BELOW */
pattern XKB_KEY_Lbelowdot :: KeySymbol
pattern XKB_KEY_Lbelowdot = MkKeySymbol #{const XKB_KEY_Lbelowdot} 
-- #define XKB_KEY_lbelowdot                  0x1001e37  /* U+1E37 LATIN SMALL LETTER L WITH DOT BELOW */
pattern XKB_KEY_lbelowdot :: KeySymbol
pattern XKB_KEY_lbelowdot = MkKeySymbol #{const XKB_KEY_lbelowdot} 
-- #define XKB_KEY_Abelowdot                  0x1001ea0  /* U+1EA0 LATIN CAPITAL LETTER A WITH DOT BELOW */
pattern XKB_KEY_Abelowdot :: KeySymbol
pattern XKB_KEY_Abelowdot = MkKeySymbol #{const XKB_KEY_Abelowdot} 
-- #define XKB_KEY_abelowdot                  0x1001ea1  /* U+1EA1 LATIN SMALL LETTER A WITH DOT BELOW */
pattern XKB_KEY_abelowdot :: KeySymbol
pattern XKB_KEY_abelowdot = MkKeySymbol #{const XKB_KEY_abelowdot} 
-- #define XKB_KEY_Ahook                      0x1001ea2  /* U+1EA2 LATIN CAPITAL LETTER A WITH HOOK ABOVE */
pattern XKB_KEY_Ahook :: KeySymbol
pattern XKB_KEY_Ahook = MkKeySymbol #{const XKB_KEY_Ahook} 
-- #define XKB_KEY_ahook                      0x1001ea3  /* U+1EA3 LATIN SMALL LETTER A WITH HOOK ABOVE */
pattern XKB_KEY_ahook :: KeySymbol
pattern XKB_KEY_ahook = MkKeySymbol #{const XKB_KEY_ahook} 
-- #define XKB_KEY_Acircumflexacute           0x1001ea4  /* U+1EA4 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE */
pattern XKB_KEY_Acircumflexacute :: KeySymbol
pattern XKB_KEY_Acircumflexacute = MkKeySymbol #{const XKB_KEY_Acircumflexacute} 
-- #define XKB_KEY_acircumflexacute           0x1001ea5  /* U+1EA5 LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE */
pattern XKB_KEY_acircumflexacute :: KeySymbol
pattern XKB_KEY_acircumflexacute = MkKeySymbol #{const XKB_KEY_acircumflexacute} 
-- #define XKB_KEY_Acircumflexgrave           0x1001ea6  /* U+1EA6 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE */
pattern XKB_KEY_Acircumflexgrave :: KeySymbol
pattern XKB_KEY_Acircumflexgrave = MkKeySymbol #{const XKB_KEY_Acircumflexgrave} 
-- #define XKB_KEY_acircumflexgrave           0x1001ea7  /* U+1EA7 LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE */
pattern XKB_KEY_acircumflexgrave :: KeySymbol
pattern XKB_KEY_acircumflexgrave = MkKeySymbol #{const XKB_KEY_acircumflexgrave} 
-- #define XKB_KEY_Acircumflexhook            0x1001ea8  /* U+1EA8 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE */
pattern XKB_KEY_Acircumflexhook :: KeySymbol
pattern XKB_KEY_Acircumflexhook = MkKeySymbol #{const XKB_KEY_Acircumflexhook} 
-- #define XKB_KEY_acircumflexhook            0x1001ea9  /* U+1EA9 LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE */
pattern XKB_KEY_acircumflexhook :: KeySymbol
pattern XKB_KEY_acircumflexhook = MkKeySymbol #{const XKB_KEY_acircumflexhook} 
-- #define XKB_KEY_Acircumflextilde           0x1001eaa  /* U+1EAA LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE */
pattern XKB_KEY_Acircumflextilde :: KeySymbol
pattern XKB_KEY_Acircumflextilde = MkKeySymbol #{const XKB_KEY_Acircumflextilde} 
-- #define XKB_KEY_acircumflextilde           0x1001eab  /* U+1EAB LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE */
pattern XKB_KEY_acircumflextilde :: KeySymbol
pattern XKB_KEY_acircumflextilde = MkKeySymbol #{const XKB_KEY_acircumflextilde} 
-- #define XKB_KEY_Acircumflexbelowdot        0x1001eac  /* U+1EAC LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW */
pattern XKB_KEY_Acircumflexbelowdot :: KeySymbol
pattern XKB_KEY_Acircumflexbelowdot = MkKeySymbol #{const XKB_KEY_Acircumflexbelowdot} 
-- #define XKB_KEY_acircumflexbelowdot        0x1001ead  /* U+1EAD LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW */
pattern XKB_KEY_acircumflexbelowdot :: KeySymbol
pattern XKB_KEY_acircumflexbelowdot = MkKeySymbol #{const XKB_KEY_acircumflexbelowdot} 
-- #define XKB_KEY_Abreveacute                0x1001eae  /* U+1EAE LATIN CAPITAL LETTER A WITH BREVE AND ACUTE */
pattern XKB_KEY_Abreveacute :: KeySymbol
pattern XKB_KEY_Abreveacute = MkKeySymbol #{const XKB_KEY_Abreveacute} 
-- #define XKB_KEY_abreveacute                0x1001eaf  /* U+1EAF LATIN SMALL LETTER A WITH BREVE AND ACUTE */
pattern XKB_KEY_abreveacute :: KeySymbol
pattern XKB_KEY_abreveacute = MkKeySymbol #{const XKB_KEY_abreveacute} 
-- #define XKB_KEY_Abrevegrave                0x1001eb0  /* U+1EB0 LATIN CAPITAL LETTER A WITH BREVE AND GRAVE */
pattern XKB_KEY_Abrevegrave :: KeySymbol
pattern XKB_KEY_Abrevegrave = MkKeySymbol #{const XKB_KEY_Abrevegrave} 
-- #define XKB_KEY_abrevegrave                0x1001eb1  /* U+1EB1 LATIN SMALL LETTER A WITH BREVE AND GRAVE */
pattern XKB_KEY_abrevegrave :: KeySymbol
pattern XKB_KEY_abrevegrave = MkKeySymbol #{const XKB_KEY_abrevegrave} 
-- #define XKB_KEY_Abrevehook                 0x1001eb2  /* U+1EB2 LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE */
pattern XKB_KEY_Abrevehook :: KeySymbol
pattern XKB_KEY_Abrevehook = MkKeySymbol #{const XKB_KEY_Abrevehook} 
-- #define XKB_KEY_abrevehook                 0x1001eb3  /* U+1EB3 LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE */
pattern XKB_KEY_abrevehook :: KeySymbol
pattern XKB_KEY_abrevehook = MkKeySymbol #{const XKB_KEY_abrevehook} 
-- #define XKB_KEY_Abrevetilde                0x1001eb4  /* U+1EB4 LATIN CAPITAL LETTER A WITH BREVE AND TILDE */
pattern XKB_KEY_Abrevetilde :: KeySymbol
pattern XKB_KEY_Abrevetilde = MkKeySymbol #{const XKB_KEY_Abrevetilde} 
-- #define XKB_KEY_abrevetilde                0x1001eb5  /* U+1EB5 LATIN SMALL LETTER A WITH BREVE AND TILDE */
pattern XKB_KEY_abrevetilde :: KeySymbol
pattern XKB_KEY_abrevetilde = MkKeySymbol #{const XKB_KEY_abrevetilde} 
-- #define XKB_KEY_Abrevebelowdot             0x1001eb6  /* U+1EB6 LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW */
pattern XKB_KEY_Abrevebelowdot :: KeySymbol
pattern XKB_KEY_Abrevebelowdot = MkKeySymbol #{const XKB_KEY_Abrevebelowdot} 
-- #define XKB_KEY_abrevebelowdot             0x1001eb7  /* U+1EB7 LATIN SMALL LETTER A WITH BREVE AND DOT BELOW */
pattern XKB_KEY_abrevebelowdot :: KeySymbol
pattern XKB_KEY_abrevebelowdot = MkKeySymbol #{const XKB_KEY_abrevebelowdot} 
-- #define XKB_KEY_Ebelowdot                  0x1001eb8  /* U+1EB8 LATIN CAPITAL LETTER E WITH DOT BELOW */
pattern XKB_KEY_Ebelowdot :: KeySymbol
pattern XKB_KEY_Ebelowdot = MkKeySymbol #{const XKB_KEY_Ebelowdot} 
-- #define XKB_KEY_ebelowdot                  0x1001eb9  /* U+1EB9 LATIN SMALL LETTER E WITH DOT BELOW */
pattern XKB_KEY_ebelowdot :: KeySymbol
pattern XKB_KEY_ebelowdot = MkKeySymbol #{const XKB_KEY_ebelowdot} 
-- #define XKB_KEY_Ehook                      0x1001eba  /* U+1EBA LATIN CAPITAL LETTER E WITH HOOK ABOVE */
pattern XKB_KEY_Ehook :: KeySymbol
pattern XKB_KEY_Ehook = MkKeySymbol #{const XKB_KEY_Ehook} 
-- #define XKB_KEY_ehook                      0x1001ebb  /* U+1EBB LATIN SMALL LETTER E WITH HOOK ABOVE */
pattern XKB_KEY_ehook :: KeySymbol
pattern XKB_KEY_ehook = MkKeySymbol #{const XKB_KEY_ehook} 
-- #define XKB_KEY_Etilde                     0x1001ebc  /* U+1EBC LATIN CAPITAL LETTER E WITH TILDE */
pattern XKB_KEY_Etilde :: KeySymbol
pattern XKB_KEY_Etilde = MkKeySymbol #{const XKB_KEY_Etilde} 
-- #define XKB_KEY_etilde                     0x1001ebd  /* U+1EBD LATIN SMALL LETTER E WITH TILDE */
pattern XKB_KEY_etilde :: KeySymbol
pattern XKB_KEY_etilde = MkKeySymbol #{const XKB_KEY_etilde} 
-- #define XKB_KEY_Ecircumflexacute           0x1001ebe  /* U+1EBE LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE */
pattern XKB_KEY_Ecircumflexacute :: KeySymbol
pattern XKB_KEY_Ecircumflexacute = MkKeySymbol #{const XKB_KEY_Ecircumflexacute} 
-- #define XKB_KEY_ecircumflexacute           0x1001ebf  /* U+1EBF LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE */
pattern XKB_KEY_ecircumflexacute :: KeySymbol
pattern XKB_KEY_ecircumflexacute = MkKeySymbol #{const XKB_KEY_ecircumflexacute} 
-- #define XKB_KEY_Ecircumflexgrave           0x1001ec0  /* U+1EC0 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE */
pattern XKB_KEY_Ecircumflexgrave :: KeySymbol
pattern XKB_KEY_Ecircumflexgrave = MkKeySymbol #{const XKB_KEY_Ecircumflexgrave} 
-- #define XKB_KEY_ecircumflexgrave           0x1001ec1  /* U+1EC1 LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE */
pattern XKB_KEY_ecircumflexgrave :: KeySymbol
pattern XKB_KEY_ecircumflexgrave = MkKeySymbol #{const XKB_KEY_ecircumflexgrave} 
-- #define XKB_KEY_Ecircumflexhook            0x1001ec2  /* U+1EC2 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE */
pattern XKB_KEY_Ecircumflexhook :: KeySymbol
pattern XKB_KEY_Ecircumflexhook = MkKeySymbol #{const XKB_KEY_Ecircumflexhook} 
-- #define XKB_KEY_ecircumflexhook            0x1001ec3  /* U+1EC3 LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE */
pattern XKB_KEY_ecircumflexhook :: KeySymbol
pattern XKB_KEY_ecircumflexhook = MkKeySymbol #{const XKB_KEY_ecircumflexhook} 
-- #define XKB_KEY_Ecircumflextilde           0x1001ec4  /* U+1EC4 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE */
pattern XKB_KEY_Ecircumflextilde :: KeySymbol
pattern XKB_KEY_Ecircumflextilde = MkKeySymbol #{const XKB_KEY_Ecircumflextilde} 
-- #define XKB_KEY_ecircumflextilde           0x1001ec5  /* U+1EC5 LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE */
pattern XKB_KEY_ecircumflextilde :: KeySymbol
pattern XKB_KEY_ecircumflextilde = MkKeySymbol #{const XKB_KEY_ecircumflextilde} 
-- #define XKB_KEY_Ecircumflexbelowdot        0x1001ec6  /* U+1EC6 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW */
pattern XKB_KEY_Ecircumflexbelowdot :: KeySymbol
pattern XKB_KEY_Ecircumflexbelowdot = MkKeySymbol #{const XKB_KEY_Ecircumflexbelowdot} 
-- #define XKB_KEY_ecircumflexbelowdot        0x1001ec7  /* U+1EC7 LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW */
pattern XKB_KEY_ecircumflexbelowdot :: KeySymbol
pattern XKB_KEY_ecircumflexbelowdot = MkKeySymbol #{const XKB_KEY_ecircumflexbelowdot} 
-- #define XKB_KEY_Ihook                      0x1001ec8  /* U+1EC8 LATIN CAPITAL LETTER I WITH HOOK ABOVE */
pattern XKB_KEY_Ihook :: KeySymbol
pattern XKB_KEY_Ihook = MkKeySymbol #{const XKB_KEY_Ihook} 
-- #define XKB_KEY_ihook                      0x1001ec9  /* U+1EC9 LATIN SMALL LETTER I WITH HOOK ABOVE */
pattern XKB_KEY_ihook :: KeySymbol
pattern XKB_KEY_ihook = MkKeySymbol #{const XKB_KEY_ihook} 
-- #define XKB_KEY_Ibelowdot                  0x1001eca  /* U+1ECA LATIN CAPITAL LETTER I WITH DOT BELOW */
pattern XKB_KEY_Ibelowdot :: KeySymbol
pattern XKB_KEY_Ibelowdot = MkKeySymbol #{const XKB_KEY_Ibelowdot} 
-- #define XKB_KEY_ibelowdot                  0x1001ecb  /* U+1ECB LATIN SMALL LETTER I WITH DOT BELOW */
pattern XKB_KEY_ibelowdot :: KeySymbol
pattern XKB_KEY_ibelowdot = MkKeySymbol #{const XKB_KEY_ibelowdot} 
-- #define XKB_KEY_Obelowdot                  0x1001ecc  /* U+1ECC LATIN CAPITAL LETTER O WITH DOT BELOW */
pattern XKB_KEY_Obelowdot :: KeySymbol
pattern XKB_KEY_Obelowdot = MkKeySymbol #{const XKB_KEY_Obelowdot} 
-- #define XKB_KEY_obelowdot                  0x1001ecd  /* U+1ECD LATIN SMALL LETTER O WITH DOT BELOW */
pattern XKB_KEY_obelowdot :: KeySymbol
pattern XKB_KEY_obelowdot = MkKeySymbol #{const XKB_KEY_obelowdot} 
-- #define XKB_KEY_Ohook                      0x1001ece  /* U+1ECE LATIN CAPITAL LETTER O WITH HOOK ABOVE */
pattern XKB_KEY_Ohook :: KeySymbol
pattern XKB_KEY_Ohook = MkKeySymbol #{const XKB_KEY_Ohook} 
-- #define XKB_KEY_ohook                      0x1001ecf  /* U+1ECF LATIN SMALL LETTER O WITH HOOK ABOVE */
pattern XKB_KEY_ohook :: KeySymbol
pattern XKB_KEY_ohook = MkKeySymbol #{const XKB_KEY_ohook} 
-- #define XKB_KEY_Ocircumflexacute           0x1001ed0  /* U+1ED0 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE */
pattern XKB_KEY_Ocircumflexacute :: KeySymbol
pattern XKB_KEY_Ocircumflexacute = MkKeySymbol #{const XKB_KEY_Ocircumflexacute} 
-- #define XKB_KEY_ocircumflexacute           0x1001ed1  /* U+1ED1 LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE */
pattern XKB_KEY_ocircumflexacute :: KeySymbol
pattern XKB_KEY_ocircumflexacute = MkKeySymbol #{const XKB_KEY_ocircumflexacute} 
-- #define XKB_KEY_Ocircumflexgrave           0x1001ed2  /* U+1ED2 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE */
pattern XKB_KEY_Ocircumflexgrave :: KeySymbol
pattern XKB_KEY_Ocircumflexgrave = MkKeySymbol #{const XKB_KEY_Ocircumflexgrave} 
-- #define XKB_KEY_ocircumflexgrave           0x1001ed3  /* U+1ED3 LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE */
pattern XKB_KEY_ocircumflexgrave :: KeySymbol
pattern XKB_KEY_ocircumflexgrave = MkKeySymbol #{const XKB_KEY_ocircumflexgrave} 
-- #define XKB_KEY_Ocircumflexhook            0x1001ed4  /* U+1ED4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE */
pattern XKB_KEY_Ocircumflexhook :: KeySymbol
pattern XKB_KEY_Ocircumflexhook = MkKeySymbol #{const XKB_KEY_Ocircumflexhook} 
-- #define XKB_KEY_ocircumflexhook            0x1001ed5  /* U+1ED5 LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE */
pattern XKB_KEY_ocircumflexhook :: KeySymbol
pattern XKB_KEY_ocircumflexhook = MkKeySymbol #{const XKB_KEY_ocircumflexhook} 
-- #define XKB_KEY_Ocircumflextilde           0x1001ed6  /* U+1ED6 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE */
pattern XKB_KEY_Ocircumflextilde :: KeySymbol
pattern XKB_KEY_Ocircumflextilde = MkKeySymbol #{const XKB_KEY_Ocircumflextilde} 
-- #define XKB_KEY_ocircumflextilde           0x1001ed7  /* U+1ED7 LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE */
pattern XKB_KEY_ocircumflextilde :: KeySymbol
pattern XKB_KEY_ocircumflextilde = MkKeySymbol #{const XKB_KEY_ocircumflextilde} 
-- #define XKB_KEY_Ocircumflexbelowdot        0x1001ed8  /* U+1ED8 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW */
pattern XKB_KEY_Ocircumflexbelowdot :: KeySymbol
pattern XKB_KEY_Ocircumflexbelowdot = MkKeySymbol #{const XKB_KEY_Ocircumflexbelowdot} 
-- #define XKB_KEY_ocircumflexbelowdot        0x1001ed9  /* U+1ED9 LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW */
pattern XKB_KEY_ocircumflexbelowdot :: KeySymbol
pattern XKB_KEY_ocircumflexbelowdot = MkKeySymbol #{const XKB_KEY_ocircumflexbelowdot} 
-- #define XKB_KEY_Ohornacute                 0x1001eda  /* U+1EDA LATIN CAPITAL LETTER O WITH HORN AND ACUTE */
pattern XKB_KEY_Ohornacute :: KeySymbol
pattern XKB_KEY_Ohornacute = MkKeySymbol #{const XKB_KEY_Ohornacute} 
-- #define XKB_KEY_ohornacute                 0x1001edb  /* U+1EDB LATIN SMALL LETTER O WITH HORN AND ACUTE */
pattern XKB_KEY_ohornacute :: KeySymbol
pattern XKB_KEY_ohornacute = MkKeySymbol #{const XKB_KEY_ohornacute} 
-- #define XKB_KEY_Ohorngrave                 0x1001edc  /* U+1EDC LATIN CAPITAL LETTER O WITH HORN AND GRAVE */
pattern XKB_KEY_Ohorngrave :: KeySymbol
pattern XKB_KEY_Ohorngrave = MkKeySymbol #{const XKB_KEY_Ohorngrave} 
-- #define XKB_KEY_ohorngrave                 0x1001edd  /* U+1EDD LATIN SMALL LETTER O WITH HORN AND GRAVE */
pattern XKB_KEY_ohorngrave :: KeySymbol
pattern XKB_KEY_ohorngrave = MkKeySymbol #{const XKB_KEY_ohorngrave} 
-- #define XKB_KEY_Ohornhook                  0x1001ede  /* U+1EDE LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE */
pattern XKB_KEY_Ohornhook :: KeySymbol
pattern XKB_KEY_Ohornhook = MkKeySymbol #{const XKB_KEY_Ohornhook} 
-- #define XKB_KEY_ohornhook                  0x1001edf  /* U+1EDF LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE */
pattern XKB_KEY_ohornhook :: KeySymbol
pattern XKB_KEY_ohornhook = MkKeySymbol #{const XKB_KEY_ohornhook} 
-- #define XKB_KEY_Ohorntilde                 0x1001ee0  /* U+1EE0 LATIN CAPITAL LETTER O WITH HORN AND TILDE */
pattern XKB_KEY_Ohorntilde :: KeySymbol
pattern XKB_KEY_Ohorntilde = MkKeySymbol #{const XKB_KEY_Ohorntilde} 
-- #define XKB_KEY_ohorntilde                 0x1001ee1  /* U+1EE1 LATIN SMALL LETTER O WITH HORN AND TILDE */
pattern XKB_KEY_ohorntilde :: KeySymbol
pattern XKB_KEY_ohorntilde = MkKeySymbol #{const XKB_KEY_ohorntilde} 
-- #define XKB_KEY_Ohornbelowdot              0x1001ee2  /* U+1EE2 LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW */
pattern XKB_KEY_Ohornbelowdot :: KeySymbol
pattern XKB_KEY_Ohornbelowdot = MkKeySymbol #{const XKB_KEY_Ohornbelowdot} 
-- #define XKB_KEY_ohornbelowdot              0x1001ee3  /* U+1EE3 LATIN SMALL LETTER O WITH HORN AND DOT BELOW */
pattern XKB_KEY_ohornbelowdot :: KeySymbol
pattern XKB_KEY_ohornbelowdot = MkKeySymbol #{const XKB_KEY_ohornbelowdot} 
-- #define XKB_KEY_Ubelowdot                  0x1001ee4  /* U+1EE4 LATIN CAPITAL LETTER U WITH DOT BELOW */
pattern XKB_KEY_Ubelowdot :: KeySymbol
pattern XKB_KEY_Ubelowdot = MkKeySymbol #{const XKB_KEY_Ubelowdot} 
-- #define XKB_KEY_ubelowdot                  0x1001ee5  /* U+1EE5 LATIN SMALL LETTER U WITH DOT BELOW */
pattern XKB_KEY_ubelowdot :: KeySymbol
pattern XKB_KEY_ubelowdot = MkKeySymbol #{const XKB_KEY_ubelowdot} 
-- #define XKB_KEY_Uhook                      0x1001ee6  /* U+1EE6 LATIN CAPITAL LETTER U WITH HOOK ABOVE */
pattern XKB_KEY_Uhook :: KeySymbol
pattern XKB_KEY_Uhook = MkKeySymbol #{const XKB_KEY_Uhook} 
-- #define XKB_KEY_uhook                      0x1001ee7  /* U+1EE7 LATIN SMALL LETTER U WITH HOOK ABOVE */
pattern XKB_KEY_uhook :: KeySymbol
pattern XKB_KEY_uhook = MkKeySymbol #{const XKB_KEY_uhook} 
-- #define XKB_KEY_Uhornacute                 0x1001ee8  /* U+1EE8 LATIN CAPITAL LETTER U WITH HORN AND ACUTE */
pattern XKB_KEY_Uhornacute :: KeySymbol
pattern XKB_KEY_Uhornacute = MkKeySymbol #{const XKB_KEY_Uhornacute} 
-- #define XKB_KEY_uhornacute                 0x1001ee9  /* U+1EE9 LATIN SMALL LETTER U WITH HORN AND ACUTE */
pattern XKB_KEY_uhornacute :: KeySymbol
pattern XKB_KEY_uhornacute = MkKeySymbol #{const XKB_KEY_uhornacute} 
-- #define XKB_KEY_Uhorngrave                 0x1001eea  /* U+1EEA LATIN CAPITAL LETTER U WITH HORN AND GRAVE */
pattern XKB_KEY_Uhorngrave :: KeySymbol
pattern XKB_KEY_Uhorngrave = MkKeySymbol #{const XKB_KEY_Uhorngrave} 
-- #define XKB_KEY_uhorngrave                 0x1001eeb  /* U+1EEB LATIN SMALL LETTER U WITH HORN AND GRAVE */
pattern XKB_KEY_uhorngrave :: KeySymbol
pattern XKB_KEY_uhorngrave = MkKeySymbol #{const XKB_KEY_uhorngrave} 
-- #define XKB_KEY_Uhornhook                  0x1001eec  /* U+1EEC LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE */
pattern XKB_KEY_Uhornhook :: KeySymbol
pattern XKB_KEY_Uhornhook = MkKeySymbol #{const XKB_KEY_Uhornhook} 
-- #define XKB_KEY_uhornhook                  0x1001eed  /* U+1EED LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE */
pattern XKB_KEY_uhornhook :: KeySymbol
pattern XKB_KEY_uhornhook = MkKeySymbol #{const XKB_KEY_uhornhook} 
-- #define XKB_KEY_Uhorntilde                 0x1001eee  /* U+1EEE LATIN CAPITAL LETTER U WITH HORN AND TILDE */
pattern XKB_KEY_Uhorntilde :: KeySymbol
pattern XKB_KEY_Uhorntilde = MkKeySymbol #{const XKB_KEY_Uhorntilde} 
-- #define XKB_KEY_uhorntilde                 0x1001eef  /* U+1EEF LATIN SMALL LETTER U WITH HORN AND TILDE */
pattern XKB_KEY_uhorntilde :: KeySymbol
pattern XKB_KEY_uhorntilde = MkKeySymbol #{const XKB_KEY_uhorntilde} 
-- #define XKB_KEY_Uhornbelowdot              0x1001ef0  /* U+1EF0 LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW */
pattern XKB_KEY_Uhornbelowdot :: KeySymbol
pattern XKB_KEY_Uhornbelowdot = MkKeySymbol #{const XKB_KEY_Uhornbelowdot} 
-- #define XKB_KEY_uhornbelowdot              0x1001ef1  /* U+1EF1 LATIN SMALL LETTER U WITH HORN AND DOT BELOW */
pattern XKB_KEY_uhornbelowdot :: KeySymbol
pattern XKB_KEY_uhornbelowdot = MkKeySymbol #{const XKB_KEY_uhornbelowdot} 
-- #define XKB_KEY_Ybelowdot                  0x1001ef4  /* U+1EF4 LATIN CAPITAL LETTER Y WITH DOT BELOW */
pattern XKB_KEY_Ybelowdot :: KeySymbol
pattern XKB_KEY_Ybelowdot = MkKeySymbol #{const XKB_KEY_Ybelowdot} 
-- #define XKB_KEY_ybelowdot                  0x1001ef5  /* U+1EF5 LATIN SMALL LETTER Y WITH DOT BELOW */
pattern XKB_KEY_ybelowdot :: KeySymbol
pattern XKB_KEY_ybelowdot = MkKeySymbol #{const XKB_KEY_ybelowdot} 
-- #define XKB_KEY_Yhook                      0x1001ef6  /* U+1EF6 LATIN CAPITAL LETTER Y WITH HOOK ABOVE */
pattern XKB_KEY_Yhook :: KeySymbol
pattern XKB_KEY_Yhook = MkKeySymbol #{const XKB_KEY_Yhook} 
-- #define XKB_KEY_yhook                      0x1001ef7  /* U+1EF7 LATIN SMALL LETTER Y WITH HOOK ABOVE */
pattern XKB_KEY_yhook :: KeySymbol
pattern XKB_KEY_yhook = MkKeySymbol #{const XKB_KEY_yhook} 
-- #define XKB_KEY_Ytilde                     0x1001ef8  /* U+1EF8 LATIN CAPITAL LETTER Y WITH TILDE */
pattern XKB_KEY_Ytilde :: KeySymbol
pattern XKB_KEY_Ytilde = MkKeySymbol #{const XKB_KEY_Ytilde} 
-- #define XKB_KEY_ytilde                     0x1001ef9  /* U+1EF9 LATIN SMALL LETTER Y WITH TILDE */
pattern XKB_KEY_ytilde :: KeySymbol
pattern XKB_KEY_ytilde = MkKeySymbol #{const XKB_KEY_ytilde} 
-- #define XKB_KEY_Ohorn                      0x10001a0  /* U+01A0 LATIN CAPITAL LETTER O WITH HORN */
pattern XKB_KEY_Ohorn :: KeySymbol
pattern XKB_KEY_Ohorn = MkKeySymbol #{const XKB_KEY_Ohorn} 
-- #define XKB_KEY_ohorn                      0x10001a1  /* U+01A1 LATIN SMALL LETTER O WITH HORN */
pattern XKB_KEY_ohorn :: KeySymbol
pattern XKB_KEY_ohorn = MkKeySymbol #{const XKB_KEY_ohorn} 
-- #define XKB_KEY_Uhorn                      0x10001af  /* U+01AF LATIN CAPITAL LETTER U WITH HORN */
pattern XKB_KEY_Uhorn :: KeySymbol
pattern XKB_KEY_Uhorn = MkKeySymbol #{const XKB_KEY_Uhorn} 
-- #define XKB_KEY_uhorn                      0x10001b0  /* U+01B0 LATIN SMALL LETTER U WITH HORN */
pattern XKB_KEY_uhorn :: KeySymbol
pattern XKB_KEY_uhorn = MkKeySymbol #{const XKB_KEY_uhorn} 
-- #define XKB_KEY_EcuSign                    0x10020a0  /* U+20A0 EURO-CURRENCY SIGN */
pattern XKB_KEY_EcuSign :: KeySymbol
pattern XKB_KEY_EcuSign = MkKeySymbol #{const XKB_KEY_EcuSign} 
-- #define XKB_KEY_ColonSign                  0x10020a1  /* U+20A1 COLON SIGN */
pattern XKB_KEY_ColonSign :: KeySymbol
pattern XKB_KEY_ColonSign = MkKeySymbol #{const XKB_KEY_ColonSign} 
-- #define XKB_KEY_CruzeiroSign               0x10020a2  /* U+20A2 CRUZEIRO SIGN */
pattern XKB_KEY_CruzeiroSign :: KeySymbol
pattern XKB_KEY_CruzeiroSign = MkKeySymbol #{const XKB_KEY_CruzeiroSign} 
-- #define XKB_KEY_FFrancSign                 0x10020a3  /* U+20A3 FRENCH FRANC SIGN */
pattern XKB_KEY_FFrancSign :: KeySymbol
pattern XKB_KEY_FFrancSign = MkKeySymbol #{const XKB_KEY_FFrancSign} 
-- #define XKB_KEY_LiraSign                   0x10020a4  /* U+20A4 LIRA SIGN */
pattern XKB_KEY_LiraSign :: KeySymbol
pattern XKB_KEY_LiraSign = MkKeySymbol #{const XKB_KEY_LiraSign} 
-- #define XKB_KEY_MillSign                   0x10020a5  /* U+20A5 MILL SIGN */
pattern XKB_KEY_MillSign :: KeySymbol
pattern XKB_KEY_MillSign = MkKeySymbol #{const XKB_KEY_MillSign} 
-- #define XKB_KEY_NairaSign                  0x10020a6  /* U+20A6 NAIRA SIGN */
pattern XKB_KEY_NairaSign :: KeySymbol
pattern XKB_KEY_NairaSign = MkKeySymbol #{const XKB_KEY_NairaSign} 
-- #define XKB_KEY_PesetaSign                 0x10020a7  /* U+20A7 PESETA SIGN */
pattern XKB_KEY_PesetaSign :: KeySymbol
pattern XKB_KEY_PesetaSign = MkKeySymbol #{const XKB_KEY_PesetaSign} 
-- #define XKB_KEY_RupeeSign                  0x10020a8  /* U+20A8 RUPEE SIGN */
pattern XKB_KEY_RupeeSign :: KeySymbol
pattern XKB_KEY_RupeeSign = MkKeySymbol #{const XKB_KEY_RupeeSign} 
-- #define XKB_KEY_WonSign                    0x10020a9  /* U+20A9 WON SIGN */
pattern XKB_KEY_WonSign :: KeySymbol
pattern XKB_KEY_WonSign = MkKeySymbol #{const XKB_KEY_WonSign} 
-- #define XKB_KEY_NewSheqelSign              0x10020aa  /* U+20AA NEW SHEQEL SIGN */
pattern XKB_KEY_NewSheqelSign :: KeySymbol
pattern XKB_KEY_NewSheqelSign = MkKeySymbol #{const XKB_KEY_NewSheqelSign} 
-- #define XKB_KEY_DongSign                   0x10020ab  /* U+20AB DONG SIGN */
pattern XKB_KEY_DongSign :: KeySymbol
pattern XKB_KEY_DongSign = MkKeySymbol #{const XKB_KEY_DongSign} 
-- #define XKB_KEY_EuroSign                      0x20ac  /* U+20AC EURO SIGN */
pattern XKB_KEY_EuroSign :: KeySymbol
pattern XKB_KEY_EuroSign = MkKeySymbol #{const XKB_KEY_EuroSign} 
-- #define XKB_KEY_zerosuperior               0x1002070  /* U+2070 SUPERSCRIPT ZERO */
pattern XKB_KEY_zerosuperior :: KeySymbol
pattern XKB_KEY_zerosuperior = MkKeySymbol #{const XKB_KEY_zerosuperior} 
-- #define XKB_KEY_foursuperior               0x1002074  /* U+2074 SUPERSCRIPT FOUR */
pattern XKB_KEY_foursuperior :: KeySymbol
pattern XKB_KEY_foursuperior = MkKeySymbol #{const XKB_KEY_foursuperior} 
-- #define XKB_KEY_fivesuperior               0x1002075  /* U+2075 SUPERSCRIPT FIVE */
pattern XKB_KEY_fivesuperior :: KeySymbol
pattern XKB_KEY_fivesuperior = MkKeySymbol #{const XKB_KEY_fivesuperior} 
-- #define XKB_KEY_sixsuperior                0x1002076  /* U+2076 SUPERSCRIPT SIX */
pattern XKB_KEY_sixsuperior :: KeySymbol
pattern XKB_KEY_sixsuperior = MkKeySymbol #{const XKB_KEY_sixsuperior} 
-- #define XKB_KEY_sevensuperior              0x1002077  /* U+2077 SUPERSCRIPT SEVEN */
pattern XKB_KEY_sevensuperior :: KeySymbol
pattern XKB_KEY_sevensuperior = MkKeySymbol #{const XKB_KEY_sevensuperior} 
-- #define XKB_KEY_eightsuperior              0x1002078  /* U+2078 SUPERSCRIPT EIGHT */
pattern XKB_KEY_eightsuperior :: KeySymbol
pattern XKB_KEY_eightsuperior = MkKeySymbol #{const XKB_KEY_eightsuperior} 
-- #define XKB_KEY_ninesuperior               0x1002079  /* U+2079 SUPERSCRIPT NINE */
pattern XKB_KEY_ninesuperior :: KeySymbol
pattern XKB_KEY_ninesuperior = MkKeySymbol #{const XKB_KEY_ninesuperior} 
-- #define XKB_KEY_zerosubscript              0x1002080  /* U+2080 SUBSCRIPT ZERO */
pattern XKB_KEY_zerosubscript :: KeySymbol
pattern XKB_KEY_zerosubscript = MkKeySymbol #{const XKB_KEY_zerosubscript} 
-- #define XKB_KEY_onesubscript               0x1002081  /* U+2081 SUBSCRIPT ONE */
pattern XKB_KEY_onesubscript :: KeySymbol
pattern XKB_KEY_onesubscript = MkKeySymbol #{const XKB_KEY_onesubscript} 
-- #define XKB_KEY_twosubscript               0x1002082  /* U+2082 SUBSCRIPT TWO */
pattern XKB_KEY_twosubscript :: KeySymbol
pattern XKB_KEY_twosubscript = MkKeySymbol #{const XKB_KEY_twosubscript} 
-- #define XKB_KEY_threesubscript             0x1002083  /* U+2083 SUBSCRIPT THREE */
pattern XKB_KEY_threesubscript :: KeySymbol
pattern XKB_KEY_threesubscript = MkKeySymbol #{const XKB_KEY_threesubscript} 
-- #define XKB_KEY_foursubscript              0x1002084  /* U+2084 SUBSCRIPT FOUR */
pattern XKB_KEY_foursubscript :: KeySymbol
pattern XKB_KEY_foursubscript = MkKeySymbol #{const XKB_KEY_foursubscript} 
-- #define XKB_KEY_fivesubscript              0x1002085  /* U+2085 SUBSCRIPT FIVE */
pattern XKB_KEY_fivesubscript :: KeySymbol
pattern XKB_KEY_fivesubscript = MkKeySymbol #{const XKB_KEY_fivesubscript} 
-- #define XKB_KEY_sixsubscript               0x1002086  /* U+2086 SUBSCRIPT SIX */
pattern XKB_KEY_sixsubscript :: KeySymbol
pattern XKB_KEY_sixsubscript = MkKeySymbol #{const XKB_KEY_sixsubscript} 
-- #define XKB_KEY_sevensubscript             0x1002087  /* U+2087 SUBSCRIPT SEVEN */
pattern XKB_KEY_sevensubscript :: KeySymbol
pattern XKB_KEY_sevensubscript = MkKeySymbol #{const XKB_KEY_sevensubscript} 
-- #define XKB_KEY_eightsubscript             0x1002088  /* U+2088 SUBSCRIPT EIGHT */
pattern XKB_KEY_eightsubscript :: KeySymbol
pattern XKB_KEY_eightsubscript = MkKeySymbol #{const XKB_KEY_eightsubscript} 
-- #define XKB_KEY_ninesubscript              0x1002089  /* U+2089 SUBSCRIPT NINE */
pattern XKB_KEY_ninesubscript :: KeySymbol
pattern XKB_KEY_ninesubscript = MkKeySymbol #{const XKB_KEY_ninesubscript} 
-- #define XKB_KEY_partdifferential           0x1002202  /* U+2202 PARTIAL DIFFERENTIAL */
pattern XKB_KEY_partdifferential :: KeySymbol
pattern XKB_KEY_partdifferential = MkKeySymbol #{const XKB_KEY_partdifferential} 
-- #define XKB_KEY_emptyset                   0x1002205  /* U+2205 NULL SET */
pattern XKB_KEY_emptyset :: KeySymbol
pattern XKB_KEY_emptyset = MkKeySymbol #{const XKB_KEY_emptyset} 
-- #define XKB_KEY_elementof                  0x1002208  /* U+2208 ELEMENT OF */
pattern XKB_KEY_elementof :: KeySymbol
pattern XKB_KEY_elementof = MkKeySymbol #{const XKB_KEY_elementof} 
-- #define XKB_KEY_notelementof               0x1002209  /* U+2209 NOT AN ELEMENT OF */
pattern XKB_KEY_notelementof :: KeySymbol
pattern XKB_KEY_notelementof = MkKeySymbol #{const XKB_KEY_notelementof} 
-- #define XKB_KEY_containsas                 0x100220B  /* U+220B CONTAINS AS MEMBER */
pattern XKB_KEY_containsas :: KeySymbol
pattern XKB_KEY_containsas = MkKeySymbol #{const XKB_KEY_containsas} 
-- #define XKB_KEY_squareroot                 0x100221A  /* U+221A SQUARE ROOT */
pattern XKB_KEY_squareroot :: KeySymbol
pattern XKB_KEY_squareroot = MkKeySymbol #{const XKB_KEY_squareroot} 
-- #define XKB_KEY_cuberoot                   0x100221B  /* U+221B CUBE ROOT */
pattern XKB_KEY_cuberoot :: KeySymbol
pattern XKB_KEY_cuberoot = MkKeySymbol #{const XKB_KEY_cuberoot} 
-- #define XKB_KEY_fourthroot                 0x100221C  /* U+221C FOURTH ROOT */
pattern XKB_KEY_fourthroot :: KeySymbol
pattern XKB_KEY_fourthroot = MkKeySymbol #{const XKB_KEY_fourthroot} 
-- #define XKB_KEY_dintegral                  0x100222C  /* U+222C DOUBLE INTEGRAL */
pattern XKB_KEY_dintegral :: KeySymbol
pattern XKB_KEY_dintegral = MkKeySymbol #{const XKB_KEY_dintegral} 
-- #define XKB_KEY_tintegral                  0x100222D  /* U+222D TRIPLE INTEGRAL */
pattern XKB_KEY_tintegral :: KeySymbol
pattern XKB_KEY_tintegral = MkKeySymbol #{const XKB_KEY_tintegral} 
-- #define XKB_KEY_because                    0x1002235  /* U+2235 BECAUSE */
pattern XKB_KEY_because :: KeySymbol
pattern XKB_KEY_because = MkKeySymbol #{const XKB_KEY_because} 
-- #define XKB_KEY_approxeq                   0x1002248  /* U+2245 ALMOST EQUAL TO */
pattern XKB_KEY_approxeq :: KeySymbol
pattern XKB_KEY_approxeq = MkKeySymbol #{const XKB_KEY_approxeq} 
-- #define XKB_KEY_notapproxeq                0x1002247  /* U+2247 NOT ALMOST EQUAL TO */
pattern XKB_KEY_notapproxeq :: KeySymbol
pattern XKB_KEY_notapproxeq = MkKeySymbol #{const XKB_KEY_notapproxeq} 
-- #define XKB_KEY_notidentical               0x1002262  /* U+2262 NOT IDENTICAL TO */
pattern XKB_KEY_notidentical :: KeySymbol
pattern XKB_KEY_notidentical = MkKeySymbol #{const XKB_KEY_notidentical} 
-- #define XKB_KEY_stricteq                   0x1002263  /* U+2263 STRICTLY EQUIVALENT TO */          
pattern XKB_KEY_stricteq :: KeySymbol
pattern XKB_KEY_stricteq = MkKeySymbol #{const XKB_KEY_stricteq} 
-- #define XKB_KEY_braille_dot_1                 0xfff1
pattern XKB_KEY_braille_dot_1 :: KeySymbol
pattern XKB_KEY_braille_dot_1 = MkKeySymbol #{const XKB_KEY_braille_dot_1} 
-- #define XKB_KEY_braille_dot_2                 0xfff2
pattern XKB_KEY_braille_dot_2 :: KeySymbol
pattern XKB_KEY_braille_dot_2 = MkKeySymbol #{const XKB_KEY_braille_dot_2} 
-- #define XKB_KEY_braille_dot_3                 0xfff3
pattern XKB_KEY_braille_dot_3 :: KeySymbol
pattern XKB_KEY_braille_dot_3 = MkKeySymbol #{const XKB_KEY_braille_dot_3} 
-- #define XKB_KEY_braille_dot_4                 0xfff4
pattern XKB_KEY_braille_dot_4 :: KeySymbol
pattern XKB_KEY_braille_dot_4 = MkKeySymbol #{const XKB_KEY_braille_dot_4} 
-- #define XKB_KEY_braille_dot_5                 0xfff5
pattern XKB_KEY_braille_dot_5 :: KeySymbol
pattern XKB_KEY_braille_dot_5 = MkKeySymbol #{const XKB_KEY_braille_dot_5} 
-- #define XKB_KEY_braille_dot_6                 0xfff6
pattern XKB_KEY_braille_dot_6 :: KeySymbol
pattern XKB_KEY_braille_dot_6 = MkKeySymbol #{const XKB_KEY_braille_dot_6} 
-- #define XKB_KEY_braille_dot_7                 0xfff7
pattern XKB_KEY_braille_dot_7 :: KeySymbol
pattern XKB_KEY_braille_dot_7 = MkKeySymbol #{const XKB_KEY_braille_dot_7} 
-- #define XKB_KEY_braille_dot_8                 0xfff8
pattern XKB_KEY_braille_dot_8 :: KeySymbol
pattern XKB_KEY_braille_dot_8 = MkKeySymbol #{const XKB_KEY_braille_dot_8} 
-- #define XKB_KEY_braille_dot_9                 0xfff9
pattern XKB_KEY_braille_dot_9 :: KeySymbol
pattern XKB_KEY_braille_dot_9 = MkKeySymbol #{const XKB_KEY_braille_dot_9} 
-- #define XKB_KEY_braille_dot_10                0xfffa
pattern XKB_KEY_braille_dot_10 :: KeySymbol
pattern XKB_KEY_braille_dot_10 = MkKeySymbol #{const XKB_KEY_braille_dot_10} 
-- #define XKB_KEY_braille_blank              0x1002800  /* U+2800 BRAILLE PATTERN BLANK */
pattern XKB_KEY_braille_blank :: KeySymbol
pattern XKB_KEY_braille_blank = MkKeySymbol #{const XKB_KEY_braille_blank} 
-- #define XKB_KEY_braille_dots_1             0x1002801  /* U+2801 BRAILLE PATTERN DOTS-1 */
pattern XKB_KEY_braille_dots_1 :: KeySymbol
pattern XKB_KEY_braille_dots_1 = MkKeySymbol #{const XKB_KEY_braille_dots_1} 
-- #define XKB_KEY_braille_dots_2             0x1002802  /* U+2802 BRAILLE PATTERN DOTS-2 */
pattern XKB_KEY_braille_dots_2 :: KeySymbol
pattern XKB_KEY_braille_dots_2 = MkKeySymbol #{const XKB_KEY_braille_dots_2} 
-- #define XKB_KEY_braille_dots_12            0x1002803  /* U+2803 BRAILLE PATTERN DOTS-12 */
pattern XKB_KEY_braille_dots_12 :: KeySymbol
pattern XKB_KEY_braille_dots_12 = MkKeySymbol #{const XKB_KEY_braille_dots_12} 
-- #define XKB_KEY_braille_dots_3             0x1002804  /* U+2804 BRAILLE PATTERN DOTS-3 */
pattern XKB_KEY_braille_dots_3 :: KeySymbol
pattern XKB_KEY_braille_dots_3 = MkKeySymbol #{const XKB_KEY_braille_dots_3} 
-- #define XKB_KEY_braille_dots_13            0x1002805  /* U+2805 BRAILLE PATTERN DOTS-13 */
pattern XKB_KEY_braille_dots_13 :: KeySymbol
pattern XKB_KEY_braille_dots_13 = MkKeySymbol #{const XKB_KEY_braille_dots_13} 
-- #define XKB_KEY_braille_dots_23            0x1002806  /* U+2806 BRAILLE PATTERN DOTS-23 */
pattern XKB_KEY_braille_dots_23 :: KeySymbol
pattern XKB_KEY_braille_dots_23 = MkKeySymbol #{const XKB_KEY_braille_dots_23} 
-- #define XKB_KEY_braille_dots_123           0x1002807  /* U+2807 BRAILLE PATTERN DOTS-123 */
pattern XKB_KEY_braille_dots_123 :: KeySymbol
pattern XKB_KEY_braille_dots_123 = MkKeySymbol #{const XKB_KEY_braille_dots_123} 
-- #define XKB_KEY_braille_dots_4             0x1002808  /* U+2808 BRAILLE PATTERN DOTS-4 */
pattern XKB_KEY_braille_dots_4 :: KeySymbol
pattern XKB_KEY_braille_dots_4 = MkKeySymbol #{const XKB_KEY_braille_dots_4} 
-- #define XKB_KEY_braille_dots_14            0x1002809  /* U+2809 BRAILLE PATTERN DOTS-14 */
pattern XKB_KEY_braille_dots_14 :: KeySymbol
pattern XKB_KEY_braille_dots_14 = MkKeySymbol #{const XKB_KEY_braille_dots_14} 
-- #define XKB_KEY_braille_dots_24            0x100280a  /* U+280a BRAILLE PATTERN DOTS-24 */
pattern XKB_KEY_braille_dots_24 :: KeySymbol
pattern XKB_KEY_braille_dots_24 = MkKeySymbol #{const XKB_KEY_braille_dots_24} 
-- #define XKB_KEY_braille_dots_124           0x100280b  /* U+280b BRAILLE PATTERN DOTS-124 */
pattern XKB_KEY_braille_dots_124 :: KeySymbol
pattern XKB_KEY_braille_dots_124 = MkKeySymbol #{const XKB_KEY_braille_dots_124} 
-- #define XKB_KEY_braille_dots_34            0x100280c  /* U+280c BRAILLE PATTERN DOTS-34 */
pattern XKB_KEY_braille_dots_34 :: KeySymbol
pattern XKB_KEY_braille_dots_34 = MkKeySymbol #{const XKB_KEY_braille_dots_34} 
-- #define XKB_KEY_braille_dots_134           0x100280d  /* U+280d BRAILLE PATTERN DOTS-134 */
pattern XKB_KEY_braille_dots_134 :: KeySymbol
pattern XKB_KEY_braille_dots_134 = MkKeySymbol #{const XKB_KEY_braille_dots_134} 
-- #define XKB_KEY_braille_dots_234           0x100280e  /* U+280e BRAILLE PATTERN DOTS-234 */
pattern XKB_KEY_braille_dots_234 :: KeySymbol
pattern XKB_KEY_braille_dots_234 = MkKeySymbol #{const XKB_KEY_braille_dots_234} 
-- #define XKB_KEY_braille_dots_1234          0x100280f  /* U+280f BRAILLE PATTERN DOTS-1234 */
pattern XKB_KEY_braille_dots_1234 :: KeySymbol
pattern XKB_KEY_braille_dots_1234 = MkKeySymbol #{const XKB_KEY_braille_dots_1234} 
-- #define XKB_KEY_braille_dots_5             0x1002810  /* U+2810 BRAILLE PATTERN DOTS-5 */
pattern XKB_KEY_braille_dots_5 :: KeySymbol
pattern XKB_KEY_braille_dots_5 = MkKeySymbol #{const XKB_KEY_braille_dots_5} 
-- #define XKB_KEY_braille_dots_15            0x1002811  /* U+2811 BRAILLE PATTERN DOTS-15 */
pattern XKB_KEY_braille_dots_15 :: KeySymbol
pattern XKB_KEY_braille_dots_15 = MkKeySymbol #{const XKB_KEY_braille_dots_15} 
-- #define XKB_KEY_braille_dots_25            0x1002812  /* U+2812 BRAILLE PATTERN DOTS-25 */
pattern XKB_KEY_braille_dots_25 :: KeySymbol
pattern XKB_KEY_braille_dots_25 = MkKeySymbol #{const XKB_KEY_braille_dots_25} 
-- #define XKB_KEY_braille_dots_125           0x1002813  /* U+2813 BRAILLE PATTERN DOTS-125 */
pattern XKB_KEY_braille_dots_125 :: KeySymbol
pattern XKB_KEY_braille_dots_125 = MkKeySymbol #{const XKB_KEY_braille_dots_125} 
-- #define XKB_KEY_braille_dots_35            0x1002814  /* U+2814 BRAILLE PATTERN DOTS-35 */
pattern XKB_KEY_braille_dots_35 :: KeySymbol
pattern XKB_KEY_braille_dots_35 = MkKeySymbol #{const XKB_KEY_braille_dots_35} 
-- #define XKB_KEY_braille_dots_135           0x1002815  /* U+2815 BRAILLE PATTERN DOTS-135 */
pattern XKB_KEY_braille_dots_135 :: KeySymbol
pattern XKB_KEY_braille_dots_135 = MkKeySymbol #{const XKB_KEY_braille_dots_135} 
-- #define XKB_KEY_braille_dots_235           0x1002816  /* U+2816 BRAILLE PATTERN DOTS-235 */
pattern XKB_KEY_braille_dots_235 :: KeySymbol
pattern XKB_KEY_braille_dots_235 = MkKeySymbol #{const XKB_KEY_braille_dots_235} 
-- #define XKB_KEY_braille_dots_1235          0x1002817  /* U+2817 BRAILLE PATTERN DOTS-1235 */
pattern XKB_KEY_braille_dots_1235 :: KeySymbol
pattern XKB_KEY_braille_dots_1235 = MkKeySymbol #{const XKB_KEY_braille_dots_1235} 
-- #define XKB_KEY_braille_dots_45            0x1002818  /* U+2818 BRAILLE PATTERN DOTS-45 */
pattern XKB_KEY_braille_dots_45 :: KeySymbol
pattern XKB_KEY_braille_dots_45 = MkKeySymbol #{const XKB_KEY_braille_dots_45} 
-- #define XKB_KEY_braille_dots_145           0x1002819  /* U+2819 BRAILLE PATTERN DOTS-145 */
pattern XKB_KEY_braille_dots_145 :: KeySymbol
pattern XKB_KEY_braille_dots_145 = MkKeySymbol #{const XKB_KEY_braille_dots_145} 
-- #define XKB_KEY_braille_dots_245           0x100281a  /* U+281a BRAILLE PATTERN DOTS-245 */
pattern XKB_KEY_braille_dots_245 :: KeySymbol
pattern XKB_KEY_braille_dots_245 = MkKeySymbol #{const XKB_KEY_braille_dots_245} 
-- #define XKB_KEY_braille_dots_1245          0x100281b  /* U+281b BRAILLE PATTERN DOTS-1245 */
pattern XKB_KEY_braille_dots_1245 :: KeySymbol
pattern XKB_KEY_braille_dots_1245 = MkKeySymbol #{const XKB_KEY_braille_dots_1245} 
-- #define XKB_KEY_braille_dots_345           0x100281c  /* U+281c BRAILLE PATTERN DOTS-345 */
pattern XKB_KEY_braille_dots_345 :: KeySymbol
pattern XKB_KEY_braille_dots_345 = MkKeySymbol #{const XKB_KEY_braille_dots_345} 
-- #define XKB_KEY_braille_dots_1345          0x100281d  /* U+281d BRAILLE PATTERN DOTS-1345 */
pattern XKB_KEY_braille_dots_1345 :: KeySymbol
pattern XKB_KEY_braille_dots_1345 = MkKeySymbol #{const XKB_KEY_braille_dots_1345} 
-- #define XKB_KEY_braille_dots_2345          0x100281e  /* U+281e BRAILLE PATTERN DOTS-2345 */
pattern XKB_KEY_braille_dots_2345 :: KeySymbol
pattern XKB_KEY_braille_dots_2345 = MkKeySymbol #{const XKB_KEY_braille_dots_2345} 
-- #define XKB_KEY_braille_dots_12345         0x100281f  /* U+281f BRAILLE PATTERN DOTS-12345 */
pattern XKB_KEY_braille_dots_12345 :: KeySymbol
pattern XKB_KEY_braille_dots_12345 = MkKeySymbol #{const XKB_KEY_braille_dots_12345} 
-- #define XKB_KEY_braille_dots_6             0x1002820  /* U+2820 BRAILLE PATTERN DOTS-6 */
pattern XKB_KEY_braille_dots_6 :: KeySymbol
pattern XKB_KEY_braille_dots_6 = MkKeySymbol #{const XKB_KEY_braille_dots_6} 
-- #define XKB_KEY_braille_dots_16            0x1002821  /* U+2821 BRAILLE PATTERN DOTS-16 */
pattern XKB_KEY_braille_dots_16 :: KeySymbol
pattern XKB_KEY_braille_dots_16 = MkKeySymbol #{const XKB_KEY_braille_dots_16} 
-- #define XKB_KEY_braille_dots_26            0x1002822  /* U+2822 BRAILLE PATTERN DOTS-26 */
pattern XKB_KEY_braille_dots_26 :: KeySymbol
pattern XKB_KEY_braille_dots_26 = MkKeySymbol #{const XKB_KEY_braille_dots_26} 
-- #define XKB_KEY_braille_dots_126           0x1002823  /* U+2823 BRAILLE PATTERN DOTS-126 */
pattern XKB_KEY_braille_dots_126 :: KeySymbol
pattern XKB_KEY_braille_dots_126 = MkKeySymbol #{const XKB_KEY_braille_dots_126} 
-- #define XKB_KEY_braille_dots_36            0x1002824  /* U+2824 BRAILLE PATTERN DOTS-36 */
pattern XKB_KEY_braille_dots_36 :: KeySymbol
pattern XKB_KEY_braille_dots_36 = MkKeySymbol #{const XKB_KEY_braille_dots_36} 
-- #define XKB_KEY_braille_dots_136           0x1002825  /* U+2825 BRAILLE PATTERN DOTS-136 */
pattern XKB_KEY_braille_dots_136 :: KeySymbol
pattern XKB_KEY_braille_dots_136 = MkKeySymbol #{const XKB_KEY_braille_dots_136} 
-- #define XKB_KEY_braille_dots_236           0x1002826  /* U+2826 BRAILLE PATTERN DOTS-236 */
pattern XKB_KEY_braille_dots_236 :: KeySymbol
pattern XKB_KEY_braille_dots_236 = MkKeySymbol #{const XKB_KEY_braille_dots_236} 
-- #define XKB_KEY_braille_dots_1236          0x1002827  /* U+2827 BRAILLE PATTERN DOTS-1236 */
pattern XKB_KEY_braille_dots_1236 :: KeySymbol
pattern XKB_KEY_braille_dots_1236 = MkKeySymbol #{const XKB_KEY_braille_dots_1236} 
-- #define XKB_KEY_braille_dots_46            0x1002828  /* U+2828 BRAILLE PATTERN DOTS-46 */
pattern XKB_KEY_braille_dots_46 :: KeySymbol
pattern XKB_KEY_braille_dots_46 = MkKeySymbol #{const XKB_KEY_braille_dots_46} 
-- #define XKB_KEY_braille_dots_146           0x1002829  /* U+2829 BRAILLE PATTERN DOTS-146 */
pattern XKB_KEY_braille_dots_146 :: KeySymbol
pattern XKB_KEY_braille_dots_146 = MkKeySymbol #{const XKB_KEY_braille_dots_146} 
-- #define XKB_KEY_braille_dots_246           0x100282a  /* U+282a BRAILLE PATTERN DOTS-246 */
pattern XKB_KEY_braille_dots_246 :: KeySymbol
pattern XKB_KEY_braille_dots_246 = MkKeySymbol #{const XKB_KEY_braille_dots_246} 
-- #define XKB_KEY_braille_dots_1246          0x100282b  /* U+282b BRAILLE PATTERN DOTS-1246 */
pattern XKB_KEY_braille_dots_1246 :: KeySymbol
pattern XKB_KEY_braille_dots_1246 = MkKeySymbol #{const XKB_KEY_braille_dots_1246} 
-- #define XKB_KEY_braille_dots_346           0x100282c  /* U+282c BRAILLE PATTERN DOTS-346 */
pattern XKB_KEY_braille_dots_346 :: KeySymbol
pattern XKB_KEY_braille_dots_346 = MkKeySymbol #{const XKB_KEY_braille_dots_346} 
-- #define XKB_KEY_braille_dots_1346          0x100282d  /* U+282d BRAILLE PATTERN DOTS-1346 */
pattern XKB_KEY_braille_dots_1346 :: KeySymbol
pattern XKB_KEY_braille_dots_1346 = MkKeySymbol #{const XKB_KEY_braille_dots_1346} 
-- #define XKB_KEY_braille_dots_2346          0x100282e  /* U+282e BRAILLE PATTERN DOTS-2346 */
pattern XKB_KEY_braille_dots_2346 :: KeySymbol
pattern XKB_KEY_braille_dots_2346 = MkKeySymbol #{const XKB_KEY_braille_dots_2346} 
-- #define XKB_KEY_braille_dots_12346         0x100282f  /* U+282f BRAILLE PATTERN DOTS-12346 */
pattern XKB_KEY_braille_dots_12346 :: KeySymbol
pattern XKB_KEY_braille_dots_12346 = MkKeySymbol #{const XKB_KEY_braille_dots_12346} 
-- #define XKB_KEY_braille_dots_56            0x1002830  /* U+2830 BRAILLE PATTERN DOTS-56 */
pattern XKB_KEY_braille_dots_56 :: KeySymbol
pattern XKB_KEY_braille_dots_56 = MkKeySymbol #{const XKB_KEY_braille_dots_56} 
-- #define XKB_KEY_braille_dots_156           0x1002831  /* U+2831 BRAILLE PATTERN DOTS-156 */
pattern XKB_KEY_braille_dots_156 :: KeySymbol
pattern XKB_KEY_braille_dots_156 = MkKeySymbol #{const XKB_KEY_braille_dots_156} 
-- #define XKB_KEY_braille_dots_256           0x1002832  /* U+2832 BRAILLE PATTERN DOTS-256 */
pattern XKB_KEY_braille_dots_256 :: KeySymbol
pattern XKB_KEY_braille_dots_256 = MkKeySymbol #{const XKB_KEY_braille_dots_256} 
-- #define XKB_KEY_braille_dots_1256          0x1002833  /* U+2833 BRAILLE PATTERN DOTS-1256 */
pattern XKB_KEY_braille_dots_1256 :: KeySymbol
pattern XKB_KEY_braille_dots_1256 = MkKeySymbol #{const XKB_KEY_braille_dots_1256} 
-- #define XKB_KEY_braille_dots_356           0x1002834  /* U+2834 BRAILLE PATTERN DOTS-356 */
pattern XKB_KEY_braille_dots_356 :: KeySymbol
pattern XKB_KEY_braille_dots_356 = MkKeySymbol #{const XKB_KEY_braille_dots_356} 
-- #define XKB_KEY_braille_dots_1356          0x1002835  /* U+2835 BRAILLE PATTERN DOTS-1356 */
pattern XKB_KEY_braille_dots_1356 :: KeySymbol
pattern XKB_KEY_braille_dots_1356 = MkKeySymbol #{const XKB_KEY_braille_dots_1356} 
-- #define XKB_KEY_braille_dots_2356          0x1002836  /* U+2836 BRAILLE PATTERN DOTS-2356 */
pattern XKB_KEY_braille_dots_2356 :: KeySymbol
pattern XKB_KEY_braille_dots_2356 = MkKeySymbol #{const XKB_KEY_braille_dots_2356} 
-- #define XKB_KEY_braille_dots_12356         0x1002837  /* U+2837 BRAILLE PATTERN DOTS-12356 */
pattern XKB_KEY_braille_dots_12356 :: KeySymbol
pattern XKB_KEY_braille_dots_12356 = MkKeySymbol #{const XKB_KEY_braille_dots_12356} 
-- #define XKB_KEY_braille_dots_456           0x1002838  /* U+2838 BRAILLE PATTERN DOTS-456 */
pattern XKB_KEY_braille_dots_456 :: KeySymbol
pattern XKB_KEY_braille_dots_456 = MkKeySymbol #{const XKB_KEY_braille_dots_456} 
-- #define XKB_KEY_braille_dots_1456          0x1002839  /* U+2839 BRAILLE PATTERN DOTS-1456 */
pattern XKB_KEY_braille_dots_1456 :: KeySymbol
pattern XKB_KEY_braille_dots_1456 = MkKeySymbol #{const XKB_KEY_braille_dots_1456} 
-- #define XKB_KEY_braille_dots_2456          0x100283a  /* U+283a BRAILLE PATTERN DOTS-2456 */
pattern XKB_KEY_braille_dots_2456 :: KeySymbol
pattern XKB_KEY_braille_dots_2456 = MkKeySymbol #{const XKB_KEY_braille_dots_2456} 
-- #define XKB_KEY_braille_dots_12456         0x100283b  /* U+283b BRAILLE PATTERN DOTS-12456 */
pattern XKB_KEY_braille_dots_12456 :: KeySymbol
pattern XKB_KEY_braille_dots_12456 = MkKeySymbol #{const XKB_KEY_braille_dots_12456} 
-- #define XKB_KEY_braille_dots_3456          0x100283c  /* U+283c BRAILLE PATTERN DOTS-3456 */
pattern XKB_KEY_braille_dots_3456 :: KeySymbol
pattern XKB_KEY_braille_dots_3456 = MkKeySymbol #{const XKB_KEY_braille_dots_3456} 
-- #define XKB_KEY_braille_dots_13456         0x100283d  /* U+283d BRAILLE PATTERN DOTS-13456 */
pattern XKB_KEY_braille_dots_13456 :: KeySymbol
pattern XKB_KEY_braille_dots_13456 = MkKeySymbol #{const XKB_KEY_braille_dots_13456} 
-- #define XKB_KEY_braille_dots_23456         0x100283e  /* U+283e BRAILLE PATTERN DOTS-23456 */
pattern XKB_KEY_braille_dots_23456 :: KeySymbol
pattern XKB_KEY_braille_dots_23456 = MkKeySymbol #{const XKB_KEY_braille_dots_23456} 
-- #define XKB_KEY_braille_dots_123456        0x100283f  /* U+283f BRAILLE PATTERN DOTS-123456 */
pattern XKB_KEY_braille_dots_123456 :: KeySymbol
pattern XKB_KEY_braille_dots_123456 = MkKeySymbol #{const XKB_KEY_braille_dots_123456} 
-- #define XKB_KEY_braille_dots_7             0x1002840  /* U+2840 BRAILLE PATTERN DOTS-7 */
pattern XKB_KEY_braille_dots_7 :: KeySymbol
pattern XKB_KEY_braille_dots_7 = MkKeySymbol #{const XKB_KEY_braille_dots_7} 
-- #define XKB_KEY_braille_dots_17            0x1002841  /* U+2841 BRAILLE PATTERN DOTS-17 */
pattern XKB_KEY_braille_dots_17 :: KeySymbol
pattern XKB_KEY_braille_dots_17 = MkKeySymbol #{const XKB_KEY_braille_dots_17} 
-- #define XKB_KEY_braille_dots_27            0x1002842  /* U+2842 BRAILLE PATTERN DOTS-27 */
pattern XKB_KEY_braille_dots_27 :: KeySymbol
pattern XKB_KEY_braille_dots_27 = MkKeySymbol #{const XKB_KEY_braille_dots_27} 
-- #define XKB_KEY_braille_dots_127           0x1002843  /* U+2843 BRAILLE PATTERN DOTS-127 */
pattern XKB_KEY_braille_dots_127 :: KeySymbol
pattern XKB_KEY_braille_dots_127 = MkKeySymbol #{const XKB_KEY_braille_dots_127} 
-- #define XKB_KEY_braille_dots_37            0x1002844  /* U+2844 BRAILLE PATTERN DOTS-37 */
pattern XKB_KEY_braille_dots_37 :: KeySymbol
pattern XKB_KEY_braille_dots_37 = MkKeySymbol #{const XKB_KEY_braille_dots_37} 
-- #define XKB_KEY_braille_dots_137           0x1002845  /* U+2845 BRAILLE PATTERN DOTS-137 */
pattern XKB_KEY_braille_dots_137 :: KeySymbol
pattern XKB_KEY_braille_dots_137 = MkKeySymbol #{const XKB_KEY_braille_dots_137} 
-- #define XKB_KEY_braille_dots_237           0x1002846  /* U+2846 BRAILLE PATTERN DOTS-237 */
pattern XKB_KEY_braille_dots_237 :: KeySymbol
pattern XKB_KEY_braille_dots_237 = MkKeySymbol #{const XKB_KEY_braille_dots_237} 
-- #define XKB_KEY_braille_dots_1237          0x1002847  /* U+2847 BRAILLE PATTERN DOTS-1237 */
pattern XKB_KEY_braille_dots_1237 :: KeySymbol
pattern XKB_KEY_braille_dots_1237 = MkKeySymbol #{const XKB_KEY_braille_dots_1237} 
-- #define XKB_KEY_braille_dots_47            0x1002848  /* U+2848 BRAILLE PATTERN DOTS-47 */
pattern XKB_KEY_braille_dots_47 :: KeySymbol
pattern XKB_KEY_braille_dots_47 = MkKeySymbol #{const XKB_KEY_braille_dots_47} 
-- #define XKB_KEY_braille_dots_147           0x1002849  /* U+2849 BRAILLE PATTERN DOTS-147 */
pattern XKB_KEY_braille_dots_147 :: KeySymbol
pattern XKB_KEY_braille_dots_147 = MkKeySymbol #{const XKB_KEY_braille_dots_147} 
-- #define XKB_KEY_braille_dots_247           0x100284a  /* U+284a BRAILLE PATTERN DOTS-247 */
pattern XKB_KEY_braille_dots_247 :: KeySymbol
pattern XKB_KEY_braille_dots_247 = MkKeySymbol #{const XKB_KEY_braille_dots_247} 
-- #define XKB_KEY_braille_dots_1247          0x100284b  /* U+284b BRAILLE PATTERN DOTS-1247 */
pattern XKB_KEY_braille_dots_1247 :: KeySymbol
pattern XKB_KEY_braille_dots_1247 = MkKeySymbol #{const XKB_KEY_braille_dots_1247} 
-- #define XKB_KEY_braille_dots_347           0x100284c  /* U+284c BRAILLE PATTERN DOTS-347 */
pattern XKB_KEY_braille_dots_347 :: KeySymbol
pattern XKB_KEY_braille_dots_347 = MkKeySymbol #{const XKB_KEY_braille_dots_347} 
-- #define XKB_KEY_braille_dots_1347          0x100284d  /* U+284d BRAILLE PATTERN DOTS-1347 */
pattern XKB_KEY_braille_dots_1347 :: KeySymbol
pattern XKB_KEY_braille_dots_1347 = MkKeySymbol #{const XKB_KEY_braille_dots_1347} 
-- #define XKB_KEY_braille_dots_2347          0x100284e  /* U+284e BRAILLE PATTERN DOTS-2347 */
pattern XKB_KEY_braille_dots_2347 :: KeySymbol
pattern XKB_KEY_braille_dots_2347 = MkKeySymbol #{const XKB_KEY_braille_dots_2347} 
-- #define XKB_KEY_braille_dots_12347         0x100284f  /* U+284f BRAILLE PATTERN DOTS-12347 */
pattern XKB_KEY_braille_dots_12347 :: KeySymbol
pattern XKB_KEY_braille_dots_12347 = MkKeySymbol #{const XKB_KEY_braille_dots_12347} 
-- #define XKB_KEY_braille_dots_57            0x1002850  /* U+2850 BRAILLE PATTERN DOTS-57 */
pattern XKB_KEY_braille_dots_57 :: KeySymbol
pattern XKB_KEY_braille_dots_57 = MkKeySymbol #{const XKB_KEY_braille_dots_57} 
-- #define XKB_KEY_braille_dots_157           0x1002851  /* U+2851 BRAILLE PATTERN DOTS-157 */
pattern XKB_KEY_braille_dots_157 :: KeySymbol
pattern XKB_KEY_braille_dots_157 = MkKeySymbol #{const XKB_KEY_braille_dots_157} 
-- #define XKB_KEY_braille_dots_257           0x1002852  /* U+2852 BRAILLE PATTERN DOTS-257 */
pattern XKB_KEY_braille_dots_257 :: KeySymbol
pattern XKB_KEY_braille_dots_257 = MkKeySymbol #{const XKB_KEY_braille_dots_257} 
-- #define XKB_KEY_braille_dots_1257          0x1002853  /* U+2853 BRAILLE PATTERN DOTS-1257 */
pattern XKB_KEY_braille_dots_1257 :: KeySymbol
pattern XKB_KEY_braille_dots_1257 = MkKeySymbol #{const XKB_KEY_braille_dots_1257} 
-- #define XKB_KEY_braille_dots_357           0x1002854  /* U+2854 BRAILLE PATTERN DOTS-357 */
pattern XKB_KEY_braille_dots_357 :: KeySymbol
pattern XKB_KEY_braille_dots_357 = MkKeySymbol #{const XKB_KEY_braille_dots_357} 
-- #define XKB_KEY_braille_dots_1357          0x1002855  /* U+2855 BRAILLE PATTERN DOTS-1357 */
pattern XKB_KEY_braille_dots_1357 :: KeySymbol
pattern XKB_KEY_braille_dots_1357 = MkKeySymbol #{const XKB_KEY_braille_dots_1357} 
-- #define XKB_KEY_braille_dots_2357          0x1002856  /* U+2856 BRAILLE PATTERN DOTS-2357 */
pattern XKB_KEY_braille_dots_2357 :: KeySymbol
pattern XKB_KEY_braille_dots_2357 = MkKeySymbol #{const XKB_KEY_braille_dots_2357} 
-- #define XKB_KEY_braille_dots_12357         0x1002857  /* U+2857 BRAILLE PATTERN DOTS-12357 */
pattern XKB_KEY_braille_dots_12357 :: KeySymbol
pattern XKB_KEY_braille_dots_12357 = MkKeySymbol #{const XKB_KEY_braille_dots_12357} 
-- #define XKB_KEY_braille_dots_457           0x1002858  /* U+2858 BRAILLE PATTERN DOTS-457 */
pattern XKB_KEY_braille_dots_457 :: KeySymbol
pattern XKB_KEY_braille_dots_457 = MkKeySymbol #{const XKB_KEY_braille_dots_457} 
-- #define XKB_KEY_braille_dots_1457          0x1002859  /* U+2859 BRAILLE PATTERN DOTS-1457 */
pattern XKB_KEY_braille_dots_1457 :: KeySymbol
pattern XKB_KEY_braille_dots_1457 = MkKeySymbol #{const XKB_KEY_braille_dots_1457} 
-- #define XKB_KEY_braille_dots_2457          0x100285a  /* U+285a BRAILLE PATTERN DOTS-2457 */
pattern XKB_KEY_braille_dots_2457 :: KeySymbol
pattern XKB_KEY_braille_dots_2457 = MkKeySymbol #{const XKB_KEY_braille_dots_2457} 
-- #define XKB_KEY_braille_dots_12457         0x100285b  /* U+285b BRAILLE PATTERN DOTS-12457 */
pattern XKB_KEY_braille_dots_12457 :: KeySymbol
pattern XKB_KEY_braille_dots_12457 = MkKeySymbol #{const XKB_KEY_braille_dots_12457} 
-- #define XKB_KEY_braille_dots_3457          0x100285c  /* U+285c BRAILLE PATTERN DOTS-3457 */
pattern XKB_KEY_braille_dots_3457 :: KeySymbol
pattern XKB_KEY_braille_dots_3457 = MkKeySymbol #{const XKB_KEY_braille_dots_3457} 
-- #define XKB_KEY_braille_dots_13457         0x100285d  /* U+285d BRAILLE PATTERN DOTS-13457 */
pattern XKB_KEY_braille_dots_13457 :: KeySymbol
pattern XKB_KEY_braille_dots_13457 = MkKeySymbol #{const XKB_KEY_braille_dots_13457} 
-- #define XKB_KEY_braille_dots_23457         0x100285e  /* U+285e BRAILLE PATTERN DOTS-23457 */
pattern XKB_KEY_braille_dots_23457 :: KeySymbol
pattern XKB_KEY_braille_dots_23457 = MkKeySymbol #{const XKB_KEY_braille_dots_23457} 
-- #define XKB_KEY_braille_dots_123457        0x100285f  /* U+285f BRAILLE PATTERN DOTS-123457 */
pattern XKB_KEY_braille_dots_123457 :: KeySymbol
pattern XKB_KEY_braille_dots_123457 = MkKeySymbol #{const XKB_KEY_braille_dots_123457} 
-- #define XKB_KEY_braille_dots_67            0x1002860  /* U+2860 BRAILLE PATTERN DOTS-67 */
pattern XKB_KEY_braille_dots_67 :: KeySymbol
pattern XKB_KEY_braille_dots_67 = MkKeySymbol #{const XKB_KEY_braille_dots_67} 
-- #define XKB_KEY_braille_dots_167           0x1002861  /* U+2861 BRAILLE PATTERN DOTS-167 */
pattern XKB_KEY_braille_dots_167 :: KeySymbol
pattern XKB_KEY_braille_dots_167 = MkKeySymbol #{const XKB_KEY_braille_dots_167} 
-- #define XKB_KEY_braille_dots_267           0x1002862  /* U+2862 BRAILLE PATTERN DOTS-267 */
pattern XKB_KEY_braille_dots_267 :: KeySymbol
pattern XKB_KEY_braille_dots_267 = MkKeySymbol #{const XKB_KEY_braille_dots_267} 
-- #define XKB_KEY_braille_dots_1267          0x1002863  /* U+2863 BRAILLE PATTERN DOTS-1267 */
pattern XKB_KEY_braille_dots_1267 :: KeySymbol
pattern XKB_KEY_braille_dots_1267 = MkKeySymbol #{const XKB_KEY_braille_dots_1267} 
-- #define XKB_KEY_braille_dots_367           0x1002864  /* U+2864 BRAILLE PATTERN DOTS-367 */
pattern XKB_KEY_braille_dots_367 :: KeySymbol
pattern XKB_KEY_braille_dots_367 = MkKeySymbol #{const XKB_KEY_braille_dots_367} 
-- #define XKB_KEY_braille_dots_1367          0x1002865  /* U+2865 BRAILLE PATTERN DOTS-1367 */
pattern XKB_KEY_braille_dots_1367 :: KeySymbol
pattern XKB_KEY_braille_dots_1367 = MkKeySymbol #{const XKB_KEY_braille_dots_1367} 
-- #define XKB_KEY_braille_dots_2367          0x1002866  /* U+2866 BRAILLE PATTERN DOTS-2367 */
pattern XKB_KEY_braille_dots_2367 :: KeySymbol
pattern XKB_KEY_braille_dots_2367 = MkKeySymbol #{const XKB_KEY_braille_dots_2367} 
-- #define XKB_KEY_braille_dots_12367         0x1002867  /* U+2867 BRAILLE PATTERN DOTS-12367 */
pattern XKB_KEY_braille_dots_12367 :: KeySymbol
pattern XKB_KEY_braille_dots_12367 = MkKeySymbol #{const XKB_KEY_braille_dots_12367} 
-- #define XKB_KEY_braille_dots_467           0x1002868  /* U+2868 BRAILLE PATTERN DOTS-467 */
pattern XKB_KEY_braille_dots_467 :: KeySymbol
pattern XKB_KEY_braille_dots_467 = MkKeySymbol #{const XKB_KEY_braille_dots_467} 
-- #define XKB_KEY_braille_dots_1467          0x1002869  /* U+2869 BRAILLE PATTERN DOTS-1467 */
pattern XKB_KEY_braille_dots_1467 :: KeySymbol
pattern XKB_KEY_braille_dots_1467 = MkKeySymbol #{const XKB_KEY_braille_dots_1467} 
-- #define XKB_KEY_braille_dots_2467          0x100286a  /* U+286a BRAILLE PATTERN DOTS-2467 */
pattern XKB_KEY_braille_dots_2467 :: KeySymbol
pattern XKB_KEY_braille_dots_2467 = MkKeySymbol #{const XKB_KEY_braille_dots_2467} 
-- #define XKB_KEY_braille_dots_12467         0x100286b  /* U+286b BRAILLE PATTERN DOTS-12467 */
pattern XKB_KEY_braille_dots_12467 :: KeySymbol
pattern XKB_KEY_braille_dots_12467 = MkKeySymbol #{const XKB_KEY_braille_dots_12467} 
-- #define XKB_KEY_braille_dots_3467          0x100286c  /* U+286c BRAILLE PATTERN DOTS-3467 */
pattern XKB_KEY_braille_dots_3467 :: KeySymbol
pattern XKB_KEY_braille_dots_3467 = MkKeySymbol #{const XKB_KEY_braille_dots_3467} 
-- #define XKB_KEY_braille_dots_13467         0x100286d  /* U+286d BRAILLE PATTERN DOTS-13467 */
pattern XKB_KEY_braille_dots_13467 :: KeySymbol
pattern XKB_KEY_braille_dots_13467 = MkKeySymbol #{const XKB_KEY_braille_dots_13467} 
-- #define XKB_KEY_braille_dots_23467         0x100286e  /* U+286e BRAILLE PATTERN DOTS-23467 */
pattern XKB_KEY_braille_dots_23467 :: KeySymbol
pattern XKB_KEY_braille_dots_23467 = MkKeySymbol #{const XKB_KEY_braille_dots_23467} 
-- #define XKB_KEY_braille_dots_123467        0x100286f  /* U+286f BRAILLE PATTERN DOTS-123467 */
pattern XKB_KEY_braille_dots_123467 :: KeySymbol
pattern XKB_KEY_braille_dots_123467 = MkKeySymbol #{const XKB_KEY_braille_dots_123467} 
-- #define XKB_KEY_braille_dots_567           0x1002870  /* U+2870 BRAILLE PATTERN DOTS-567 */
pattern XKB_KEY_braille_dots_567 :: KeySymbol
pattern XKB_KEY_braille_dots_567 = MkKeySymbol #{const XKB_KEY_braille_dots_567} 
-- #define XKB_KEY_braille_dots_1567          0x1002871  /* U+2871 BRAILLE PATTERN DOTS-1567 */
pattern XKB_KEY_braille_dots_1567 :: KeySymbol
pattern XKB_KEY_braille_dots_1567 = MkKeySymbol #{const XKB_KEY_braille_dots_1567} 
-- #define XKB_KEY_braille_dots_2567          0x1002872  /* U+2872 BRAILLE PATTERN DOTS-2567 */
pattern XKB_KEY_braille_dots_2567 :: KeySymbol
pattern XKB_KEY_braille_dots_2567 = MkKeySymbol #{const XKB_KEY_braille_dots_2567} 
-- #define XKB_KEY_braille_dots_12567         0x1002873  /* U+2873 BRAILLE PATTERN DOTS-12567 */
pattern XKB_KEY_braille_dots_12567 :: KeySymbol
pattern XKB_KEY_braille_dots_12567 = MkKeySymbol #{const XKB_KEY_braille_dots_12567} 
-- #define XKB_KEY_braille_dots_3567          0x1002874  /* U+2874 BRAILLE PATTERN DOTS-3567 */
pattern XKB_KEY_braille_dots_3567 :: KeySymbol
pattern XKB_KEY_braille_dots_3567 = MkKeySymbol #{const XKB_KEY_braille_dots_3567} 
-- #define XKB_KEY_braille_dots_13567         0x1002875  /* U+2875 BRAILLE PATTERN DOTS-13567 */
pattern XKB_KEY_braille_dots_13567 :: KeySymbol
pattern XKB_KEY_braille_dots_13567 = MkKeySymbol #{const XKB_KEY_braille_dots_13567} 
-- #define XKB_KEY_braille_dots_23567         0x1002876  /* U+2876 BRAILLE PATTERN DOTS-23567 */
pattern XKB_KEY_braille_dots_23567 :: KeySymbol
pattern XKB_KEY_braille_dots_23567 = MkKeySymbol #{const XKB_KEY_braille_dots_23567} 
-- #define XKB_KEY_braille_dots_123567        0x1002877  /* U+2877 BRAILLE PATTERN DOTS-123567 */
pattern XKB_KEY_braille_dots_123567 :: KeySymbol
pattern XKB_KEY_braille_dots_123567 = MkKeySymbol #{const XKB_KEY_braille_dots_123567} 
-- #define XKB_KEY_braille_dots_4567          0x1002878  /* U+2878 BRAILLE PATTERN DOTS-4567 */
pattern XKB_KEY_braille_dots_4567 :: KeySymbol
pattern XKB_KEY_braille_dots_4567 = MkKeySymbol #{const XKB_KEY_braille_dots_4567} 
-- #define XKB_KEY_braille_dots_14567         0x1002879  /* U+2879 BRAILLE PATTERN DOTS-14567 */
pattern XKB_KEY_braille_dots_14567 :: KeySymbol
pattern XKB_KEY_braille_dots_14567 = MkKeySymbol #{const XKB_KEY_braille_dots_14567} 
-- #define XKB_KEY_braille_dots_24567         0x100287a  /* U+287a BRAILLE PATTERN DOTS-24567 */
pattern XKB_KEY_braille_dots_24567 :: KeySymbol
pattern XKB_KEY_braille_dots_24567 = MkKeySymbol #{const XKB_KEY_braille_dots_24567} 
-- #define XKB_KEY_braille_dots_124567        0x100287b  /* U+287b BRAILLE PATTERN DOTS-124567 */
pattern XKB_KEY_braille_dots_124567 :: KeySymbol
pattern XKB_KEY_braille_dots_124567 = MkKeySymbol #{const XKB_KEY_braille_dots_124567} 
-- #define XKB_KEY_braille_dots_34567         0x100287c  /* U+287c BRAILLE PATTERN DOTS-34567 */
pattern XKB_KEY_braille_dots_34567 :: KeySymbol
pattern XKB_KEY_braille_dots_34567 = MkKeySymbol #{const XKB_KEY_braille_dots_34567} 
-- #define XKB_KEY_braille_dots_134567        0x100287d  /* U+287d BRAILLE PATTERN DOTS-134567 */
pattern XKB_KEY_braille_dots_134567 :: KeySymbol
pattern XKB_KEY_braille_dots_134567 = MkKeySymbol #{const XKB_KEY_braille_dots_134567} 
-- #define XKB_KEY_braille_dots_234567        0x100287e  /* U+287e BRAILLE PATTERN DOTS-234567 */
pattern XKB_KEY_braille_dots_234567 :: KeySymbol
pattern XKB_KEY_braille_dots_234567 = MkKeySymbol #{const XKB_KEY_braille_dots_234567} 
-- #define XKB_KEY_braille_dots_1234567       0x100287f  /* U+287f BRAILLE PATTERN DOTS-1234567 */
pattern XKB_KEY_braille_dots_1234567 :: KeySymbol
pattern XKB_KEY_braille_dots_1234567 = MkKeySymbol #{const XKB_KEY_braille_dots_1234567} 
-- #define XKB_KEY_braille_dots_8             0x1002880  /* U+2880 BRAILLE PATTERN DOTS-8 */
pattern XKB_KEY_braille_dots_8 :: KeySymbol
pattern XKB_KEY_braille_dots_8 = MkKeySymbol #{const XKB_KEY_braille_dots_8} 
-- #define XKB_KEY_braille_dots_18            0x1002881  /* U+2881 BRAILLE PATTERN DOTS-18 */
pattern XKB_KEY_braille_dots_18 :: KeySymbol
pattern XKB_KEY_braille_dots_18 = MkKeySymbol #{const XKB_KEY_braille_dots_18} 
-- #define XKB_KEY_braille_dots_28            0x1002882  /* U+2882 BRAILLE PATTERN DOTS-28 */
pattern XKB_KEY_braille_dots_28 :: KeySymbol
pattern XKB_KEY_braille_dots_28 = MkKeySymbol #{const XKB_KEY_braille_dots_28} 
-- #define XKB_KEY_braille_dots_128           0x1002883  /* U+2883 BRAILLE PATTERN DOTS-128 */
pattern XKB_KEY_braille_dots_128 :: KeySymbol
pattern XKB_KEY_braille_dots_128 = MkKeySymbol #{const XKB_KEY_braille_dots_128} 
-- #define XKB_KEY_braille_dots_38            0x1002884  /* U+2884 BRAILLE PATTERN DOTS-38 */
pattern XKB_KEY_braille_dots_38 :: KeySymbol
pattern XKB_KEY_braille_dots_38 = MkKeySymbol #{const XKB_KEY_braille_dots_38} 
-- #define XKB_KEY_braille_dots_138           0x1002885  /* U+2885 BRAILLE PATTERN DOTS-138 */
pattern XKB_KEY_braille_dots_138 :: KeySymbol
pattern XKB_KEY_braille_dots_138 = MkKeySymbol #{const XKB_KEY_braille_dots_138} 
-- #define XKB_KEY_braille_dots_238           0x1002886  /* U+2886 BRAILLE PATTERN DOTS-238 */
pattern XKB_KEY_braille_dots_238 :: KeySymbol
pattern XKB_KEY_braille_dots_238 = MkKeySymbol #{const XKB_KEY_braille_dots_238} 
-- #define XKB_KEY_braille_dots_1238          0x1002887  /* U+2887 BRAILLE PATTERN DOTS-1238 */
pattern XKB_KEY_braille_dots_1238 :: KeySymbol
pattern XKB_KEY_braille_dots_1238 = MkKeySymbol #{const XKB_KEY_braille_dots_1238} 
-- #define XKB_KEY_braille_dots_48            0x1002888  /* U+2888 BRAILLE PATTERN DOTS-48 */
pattern XKB_KEY_braille_dots_48 :: KeySymbol
pattern XKB_KEY_braille_dots_48 = MkKeySymbol #{const XKB_KEY_braille_dots_48} 
-- #define XKB_KEY_braille_dots_148           0x1002889  /* U+2889 BRAILLE PATTERN DOTS-148 */
pattern XKB_KEY_braille_dots_148 :: KeySymbol
pattern XKB_KEY_braille_dots_148 = MkKeySymbol #{const XKB_KEY_braille_dots_148} 
-- #define XKB_KEY_braille_dots_248           0x100288a  /* U+288a BRAILLE PATTERN DOTS-248 */
pattern XKB_KEY_braille_dots_248 :: KeySymbol
pattern XKB_KEY_braille_dots_248 = MkKeySymbol #{const XKB_KEY_braille_dots_248} 
-- #define XKB_KEY_braille_dots_1248          0x100288b  /* U+288b BRAILLE PATTERN DOTS-1248 */
pattern XKB_KEY_braille_dots_1248 :: KeySymbol
pattern XKB_KEY_braille_dots_1248 = MkKeySymbol #{const XKB_KEY_braille_dots_1248} 
-- #define XKB_KEY_braille_dots_348           0x100288c  /* U+288c BRAILLE PATTERN DOTS-348 */
pattern XKB_KEY_braille_dots_348 :: KeySymbol
pattern XKB_KEY_braille_dots_348 = MkKeySymbol #{const XKB_KEY_braille_dots_348} 
-- #define XKB_KEY_braille_dots_1348          0x100288d  /* U+288d BRAILLE PATTERN DOTS-1348 */
pattern XKB_KEY_braille_dots_1348 :: KeySymbol
pattern XKB_KEY_braille_dots_1348 = MkKeySymbol #{const XKB_KEY_braille_dots_1348} 
-- #define XKB_KEY_braille_dots_2348          0x100288e  /* U+288e BRAILLE PATTERN DOTS-2348 */
pattern XKB_KEY_braille_dots_2348 :: KeySymbol
pattern XKB_KEY_braille_dots_2348 = MkKeySymbol #{const XKB_KEY_braille_dots_2348} 
-- #define XKB_KEY_braille_dots_12348         0x100288f  /* U+288f BRAILLE PATTERN DOTS-12348 */
pattern XKB_KEY_braille_dots_12348 :: KeySymbol
pattern XKB_KEY_braille_dots_12348 = MkKeySymbol #{const XKB_KEY_braille_dots_12348} 
-- #define XKB_KEY_braille_dots_58            0x1002890  /* U+2890 BRAILLE PATTERN DOTS-58 */
pattern XKB_KEY_braille_dots_58 :: KeySymbol
pattern XKB_KEY_braille_dots_58 = MkKeySymbol #{const XKB_KEY_braille_dots_58} 
-- #define XKB_KEY_braille_dots_158           0x1002891  /* U+2891 BRAILLE PATTERN DOTS-158 */
pattern XKB_KEY_braille_dots_158 :: KeySymbol
pattern XKB_KEY_braille_dots_158 = MkKeySymbol #{const XKB_KEY_braille_dots_158} 
-- #define XKB_KEY_braille_dots_258           0x1002892  /* U+2892 BRAILLE PATTERN DOTS-258 */
pattern XKB_KEY_braille_dots_258 :: KeySymbol
pattern XKB_KEY_braille_dots_258 = MkKeySymbol #{const XKB_KEY_braille_dots_258} 
-- #define XKB_KEY_braille_dots_1258          0x1002893  /* U+2893 BRAILLE PATTERN DOTS-1258 */
pattern XKB_KEY_braille_dots_1258 :: KeySymbol
pattern XKB_KEY_braille_dots_1258 = MkKeySymbol #{const XKB_KEY_braille_dots_1258} 
-- #define XKB_KEY_braille_dots_358           0x1002894  /* U+2894 BRAILLE PATTERN DOTS-358 */
pattern XKB_KEY_braille_dots_358 :: KeySymbol
pattern XKB_KEY_braille_dots_358 = MkKeySymbol #{const XKB_KEY_braille_dots_358} 
-- #define XKB_KEY_braille_dots_1358          0x1002895  /* U+2895 BRAILLE PATTERN DOTS-1358 */
pattern XKB_KEY_braille_dots_1358 :: KeySymbol
pattern XKB_KEY_braille_dots_1358 = MkKeySymbol #{const XKB_KEY_braille_dots_1358} 
-- #define XKB_KEY_braille_dots_2358          0x1002896  /* U+2896 BRAILLE PATTERN DOTS-2358 */
pattern XKB_KEY_braille_dots_2358 :: KeySymbol
pattern XKB_KEY_braille_dots_2358 = MkKeySymbol #{const XKB_KEY_braille_dots_2358} 
-- #define XKB_KEY_braille_dots_12358         0x1002897  /* U+2897 BRAILLE PATTERN DOTS-12358 */
pattern XKB_KEY_braille_dots_12358 :: KeySymbol
pattern XKB_KEY_braille_dots_12358 = MkKeySymbol #{const XKB_KEY_braille_dots_12358} 
-- #define XKB_KEY_braille_dots_458           0x1002898  /* U+2898 BRAILLE PATTERN DOTS-458 */
pattern XKB_KEY_braille_dots_458 :: KeySymbol
pattern XKB_KEY_braille_dots_458 = MkKeySymbol #{const XKB_KEY_braille_dots_458} 
-- #define XKB_KEY_braille_dots_1458          0x1002899  /* U+2899 BRAILLE PATTERN DOTS-1458 */
pattern XKB_KEY_braille_dots_1458 :: KeySymbol
pattern XKB_KEY_braille_dots_1458 = MkKeySymbol #{const XKB_KEY_braille_dots_1458} 
-- #define XKB_KEY_braille_dots_2458          0x100289a  /* U+289a BRAILLE PATTERN DOTS-2458 */
pattern XKB_KEY_braille_dots_2458 :: KeySymbol
pattern XKB_KEY_braille_dots_2458 = MkKeySymbol #{const XKB_KEY_braille_dots_2458} 
-- #define XKB_KEY_braille_dots_12458         0x100289b  /* U+289b BRAILLE PATTERN DOTS-12458 */
pattern XKB_KEY_braille_dots_12458 :: KeySymbol
pattern XKB_KEY_braille_dots_12458 = MkKeySymbol #{const XKB_KEY_braille_dots_12458} 
-- #define XKB_KEY_braille_dots_3458          0x100289c  /* U+289c BRAILLE PATTERN DOTS-3458 */
pattern XKB_KEY_braille_dots_3458 :: KeySymbol
pattern XKB_KEY_braille_dots_3458 = MkKeySymbol #{const XKB_KEY_braille_dots_3458} 
-- #define XKB_KEY_braille_dots_13458         0x100289d  /* U+289d BRAILLE PATTERN DOTS-13458 */
pattern XKB_KEY_braille_dots_13458 :: KeySymbol
pattern XKB_KEY_braille_dots_13458 = MkKeySymbol #{const XKB_KEY_braille_dots_13458} 
-- #define XKB_KEY_braille_dots_23458         0x100289e  /* U+289e BRAILLE PATTERN DOTS-23458 */
pattern XKB_KEY_braille_dots_23458 :: KeySymbol
pattern XKB_KEY_braille_dots_23458 = MkKeySymbol #{const XKB_KEY_braille_dots_23458} 
-- #define XKB_KEY_braille_dots_123458        0x100289f  /* U+289f BRAILLE PATTERN DOTS-123458 */
pattern XKB_KEY_braille_dots_123458 :: KeySymbol
pattern XKB_KEY_braille_dots_123458 = MkKeySymbol #{const XKB_KEY_braille_dots_123458} 
-- #define XKB_KEY_braille_dots_68            0x10028a0  /* U+28a0 BRAILLE PATTERN DOTS-68 */
pattern XKB_KEY_braille_dots_68 :: KeySymbol
pattern XKB_KEY_braille_dots_68 = MkKeySymbol #{const XKB_KEY_braille_dots_68} 
-- #define XKB_KEY_braille_dots_168           0x10028a1  /* U+28a1 BRAILLE PATTERN DOTS-168 */
pattern XKB_KEY_braille_dots_168 :: KeySymbol
pattern XKB_KEY_braille_dots_168 = MkKeySymbol #{const XKB_KEY_braille_dots_168} 
-- #define XKB_KEY_braille_dots_268           0x10028a2  /* U+28a2 BRAILLE PATTERN DOTS-268 */
pattern XKB_KEY_braille_dots_268 :: KeySymbol
pattern XKB_KEY_braille_dots_268 = MkKeySymbol #{const XKB_KEY_braille_dots_268} 
-- #define XKB_KEY_braille_dots_1268          0x10028a3  /* U+28a3 BRAILLE PATTERN DOTS-1268 */
pattern XKB_KEY_braille_dots_1268 :: KeySymbol
pattern XKB_KEY_braille_dots_1268 = MkKeySymbol #{const XKB_KEY_braille_dots_1268} 
-- #define XKB_KEY_braille_dots_368           0x10028a4  /* U+28a4 BRAILLE PATTERN DOTS-368 */
pattern XKB_KEY_braille_dots_368 :: KeySymbol
pattern XKB_KEY_braille_dots_368 = MkKeySymbol #{const XKB_KEY_braille_dots_368} 
-- #define XKB_KEY_braille_dots_1368          0x10028a5  /* U+28a5 BRAILLE PATTERN DOTS-1368 */
pattern XKB_KEY_braille_dots_1368 :: KeySymbol
pattern XKB_KEY_braille_dots_1368 = MkKeySymbol #{const XKB_KEY_braille_dots_1368} 
-- #define XKB_KEY_braille_dots_2368          0x10028a6  /* U+28a6 BRAILLE PATTERN DOTS-2368 */
pattern XKB_KEY_braille_dots_2368 :: KeySymbol
pattern XKB_KEY_braille_dots_2368 = MkKeySymbol #{const XKB_KEY_braille_dots_2368} 
-- #define XKB_KEY_braille_dots_12368         0x10028a7  /* U+28a7 BRAILLE PATTERN DOTS-12368 */
pattern XKB_KEY_braille_dots_12368 :: KeySymbol
pattern XKB_KEY_braille_dots_12368 = MkKeySymbol #{const XKB_KEY_braille_dots_12368} 
-- #define XKB_KEY_braille_dots_468           0x10028a8  /* U+28a8 BRAILLE PATTERN DOTS-468 */
pattern XKB_KEY_braille_dots_468 :: KeySymbol
pattern XKB_KEY_braille_dots_468 = MkKeySymbol #{const XKB_KEY_braille_dots_468} 
-- #define XKB_KEY_braille_dots_1468          0x10028a9  /* U+28a9 BRAILLE PATTERN DOTS-1468 */
pattern XKB_KEY_braille_dots_1468 :: KeySymbol
pattern XKB_KEY_braille_dots_1468 = MkKeySymbol #{const XKB_KEY_braille_dots_1468} 
-- #define XKB_KEY_braille_dots_2468          0x10028aa  /* U+28aa BRAILLE PATTERN DOTS-2468 */
pattern XKB_KEY_braille_dots_2468 :: KeySymbol
pattern XKB_KEY_braille_dots_2468 = MkKeySymbol #{const XKB_KEY_braille_dots_2468} 
-- #define XKB_KEY_braille_dots_12468         0x10028ab  /* U+28ab BRAILLE PATTERN DOTS-12468 */
pattern XKB_KEY_braille_dots_12468 :: KeySymbol
pattern XKB_KEY_braille_dots_12468 = MkKeySymbol #{const XKB_KEY_braille_dots_12468} 
-- #define XKB_KEY_braille_dots_3468          0x10028ac  /* U+28ac BRAILLE PATTERN DOTS-3468 */
pattern XKB_KEY_braille_dots_3468 :: KeySymbol
pattern XKB_KEY_braille_dots_3468 = MkKeySymbol #{const XKB_KEY_braille_dots_3468} 
-- #define XKB_KEY_braille_dots_13468         0x10028ad  /* U+28ad BRAILLE PATTERN DOTS-13468 */
pattern XKB_KEY_braille_dots_13468 :: KeySymbol
pattern XKB_KEY_braille_dots_13468 = MkKeySymbol #{const XKB_KEY_braille_dots_13468} 
-- #define XKB_KEY_braille_dots_23468         0x10028ae  /* U+28ae BRAILLE PATTERN DOTS-23468 */
pattern XKB_KEY_braille_dots_23468 :: KeySymbol
pattern XKB_KEY_braille_dots_23468 = MkKeySymbol #{const XKB_KEY_braille_dots_23468} 
-- #define XKB_KEY_braille_dots_123468        0x10028af  /* U+28af BRAILLE PATTERN DOTS-123468 */
pattern XKB_KEY_braille_dots_123468 :: KeySymbol
pattern XKB_KEY_braille_dots_123468 = MkKeySymbol #{const XKB_KEY_braille_dots_123468} 
-- #define XKB_KEY_braille_dots_568           0x10028b0  /* U+28b0 BRAILLE PATTERN DOTS-568 */
pattern XKB_KEY_braille_dots_568 :: KeySymbol
pattern XKB_KEY_braille_dots_568 = MkKeySymbol #{const XKB_KEY_braille_dots_568} 
-- #define XKB_KEY_braille_dots_1568          0x10028b1  /* U+28b1 BRAILLE PATTERN DOTS-1568 */
pattern XKB_KEY_braille_dots_1568 :: KeySymbol
pattern XKB_KEY_braille_dots_1568 = MkKeySymbol #{const XKB_KEY_braille_dots_1568} 
-- #define XKB_KEY_braille_dots_2568          0x10028b2  /* U+28b2 BRAILLE PATTERN DOTS-2568 */
pattern XKB_KEY_braille_dots_2568 :: KeySymbol
pattern XKB_KEY_braille_dots_2568 = MkKeySymbol #{const XKB_KEY_braille_dots_2568} 
-- #define XKB_KEY_braille_dots_12568         0x10028b3  /* U+28b3 BRAILLE PATTERN DOTS-12568 */
pattern XKB_KEY_braille_dots_12568 :: KeySymbol
pattern XKB_KEY_braille_dots_12568 = MkKeySymbol #{const XKB_KEY_braille_dots_12568} 
-- #define XKB_KEY_braille_dots_3568          0x10028b4  /* U+28b4 BRAILLE PATTERN DOTS-3568 */
pattern XKB_KEY_braille_dots_3568 :: KeySymbol
pattern XKB_KEY_braille_dots_3568 = MkKeySymbol #{const XKB_KEY_braille_dots_3568} 
-- #define XKB_KEY_braille_dots_13568         0x10028b5  /* U+28b5 BRAILLE PATTERN DOTS-13568 */
pattern XKB_KEY_braille_dots_13568 :: KeySymbol
pattern XKB_KEY_braille_dots_13568 = MkKeySymbol #{const XKB_KEY_braille_dots_13568} 
-- #define XKB_KEY_braille_dots_23568         0x10028b6  /* U+28b6 BRAILLE PATTERN DOTS-23568 */
pattern XKB_KEY_braille_dots_23568 :: KeySymbol
pattern XKB_KEY_braille_dots_23568 = MkKeySymbol #{const XKB_KEY_braille_dots_23568} 
-- #define XKB_KEY_braille_dots_123568        0x10028b7  /* U+28b7 BRAILLE PATTERN DOTS-123568 */
pattern XKB_KEY_braille_dots_123568 :: KeySymbol
pattern XKB_KEY_braille_dots_123568 = MkKeySymbol #{const XKB_KEY_braille_dots_123568} 
-- #define XKB_KEY_braille_dots_4568          0x10028b8  /* U+28b8 BRAILLE PATTERN DOTS-4568 */
pattern XKB_KEY_braille_dots_4568 :: KeySymbol
pattern XKB_KEY_braille_dots_4568 = MkKeySymbol #{const XKB_KEY_braille_dots_4568} 
-- #define XKB_KEY_braille_dots_14568         0x10028b9  /* U+28b9 BRAILLE PATTERN DOTS-14568 */
pattern XKB_KEY_braille_dots_14568 :: KeySymbol
pattern XKB_KEY_braille_dots_14568 = MkKeySymbol #{const XKB_KEY_braille_dots_14568} 
-- #define XKB_KEY_braille_dots_24568         0x10028ba  /* U+28ba BRAILLE PATTERN DOTS-24568 */
pattern XKB_KEY_braille_dots_24568 :: KeySymbol
pattern XKB_KEY_braille_dots_24568 = MkKeySymbol #{const XKB_KEY_braille_dots_24568} 
-- #define XKB_KEY_braille_dots_124568        0x10028bb  /* U+28bb BRAILLE PATTERN DOTS-124568 */
pattern XKB_KEY_braille_dots_124568 :: KeySymbol
pattern XKB_KEY_braille_dots_124568 = MkKeySymbol #{const XKB_KEY_braille_dots_124568} 
-- #define XKB_KEY_braille_dots_34568         0x10028bc  /* U+28bc BRAILLE PATTERN DOTS-34568 */
pattern XKB_KEY_braille_dots_34568 :: KeySymbol
pattern XKB_KEY_braille_dots_34568 = MkKeySymbol #{const XKB_KEY_braille_dots_34568} 
-- #define XKB_KEY_braille_dots_134568        0x10028bd  /* U+28bd BRAILLE PATTERN DOTS-134568 */
pattern XKB_KEY_braille_dots_134568 :: KeySymbol
pattern XKB_KEY_braille_dots_134568 = MkKeySymbol #{const XKB_KEY_braille_dots_134568} 
-- #define XKB_KEY_braille_dots_234568        0x10028be  /* U+28be BRAILLE PATTERN DOTS-234568 */
pattern XKB_KEY_braille_dots_234568 :: KeySymbol
pattern XKB_KEY_braille_dots_234568 = MkKeySymbol #{const XKB_KEY_braille_dots_234568} 
-- #define XKB_KEY_braille_dots_1234568       0x10028bf  /* U+28bf BRAILLE PATTERN DOTS-1234568 */
pattern XKB_KEY_braille_dots_1234568 :: KeySymbol
pattern XKB_KEY_braille_dots_1234568 = MkKeySymbol #{const XKB_KEY_braille_dots_1234568} 
-- #define XKB_KEY_braille_dots_78            0x10028c0  /* U+28c0 BRAILLE PATTERN DOTS-78 */
pattern XKB_KEY_braille_dots_78 :: KeySymbol
pattern XKB_KEY_braille_dots_78 = MkKeySymbol #{const XKB_KEY_braille_dots_78} 
-- #define XKB_KEY_braille_dots_178           0x10028c1  /* U+28c1 BRAILLE PATTERN DOTS-178 */
pattern XKB_KEY_braille_dots_178 :: KeySymbol
pattern XKB_KEY_braille_dots_178 = MkKeySymbol #{const XKB_KEY_braille_dots_178} 
-- #define XKB_KEY_braille_dots_278           0x10028c2  /* U+28c2 BRAILLE PATTERN DOTS-278 */
pattern XKB_KEY_braille_dots_278 :: KeySymbol
pattern XKB_KEY_braille_dots_278 = MkKeySymbol #{const XKB_KEY_braille_dots_278} 
-- #define XKB_KEY_braille_dots_1278          0x10028c3  /* U+28c3 BRAILLE PATTERN DOTS-1278 */
pattern XKB_KEY_braille_dots_1278 :: KeySymbol
pattern XKB_KEY_braille_dots_1278 = MkKeySymbol #{const XKB_KEY_braille_dots_1278} 
-- #define XKB_KEY_braille_dots_378           0x10028c4  /* U+28c4 BRAILLE PATTERN DOTS-378 */
pattern XKB_KEY_braille_dots_378 :: KeySymbol
pattern XKB_KEY_braille_dots_378 = MkKeySymbol #{const XKB_KEY_braille_dots_378} 
-- #define XKB_KEY_braille_dots_1378          0x10028c5  /* U+28c5 BRAILLE PATTERN DOTS-1378 */
pattern XKB_KEY_braille_dots_1378 :: KeySymbol
pattern XKB_KEY_braille_dots_1378 = MkKeySymbol #{const XKB_KEY_braille_dots_1378} 
-- #define XKB_KEY_braille_dots_2378          0x10028c6  /* U+28c6 BRAILLE PATTERN DOTS-2378 */
pattern XKB_KEY_braille_dots_2378 :: KeySymbol
pattern XKB_KEY_braille_dots_2378 = MkKeySymbol #{const XKB_KEY_braille_dots_2378} 
-- #define XKB_KEY_braille_dots_12378         0x10028c7  /* U+28c7 BRAILLE PATTERN DOTS-12378 */
pattern XKB_KEY_braille_dots_12378 :: KeySymbol
pattern XKB_KEY_braille_dots_12378 = MkKeySymbol #{const XKB_KEY_braille_dots_12378} 
-- #define XKB_KEY_braille_dots_478           0x10028c8  /* U+28c8 BRAILLE PATTERN DOTS-478 */
pattern XKB_KEY_braille_dots_478 :: KeySymbol
pattern XKB_KEY_braille_dots_478 = MkKeySymbol #{const XKB_KEY_braille_dots_478} 
-- #define XKB_KEY_braille_dots_1478          0x10028c9  /* U+28c9 BRAILLE PATTERN DOTS-1478 */
pattern XKB_KEY_braille_dots_1478 :: KeySymbol
pattern XKB_KEY_braille_dots_1478 = MkKeySymbol #{const XKB_KEY_braille_dots_1478} 
-- #define XKB_KEY_braille_dots_2478          0x10028ca  /* U+28ca BRAILLE PATTERN DOTS-2478 */
pattern XKB_KEY_braille_dots_2478 :: KeySymbol
pattern XKB_KEY_braille_dots_2478 = MkKeySymbol #{const XKB_KEY_braille_dots_2478} 
-- #define XKB_KEY_braille_dots_12478         0x10028cb  /* U+28cb BRAILLE PATTERN DOTS-12478 */
pattern XKB_KEY_braille_dots_12478 :: KeySymbol
pattern XKB_KEY_braille_dots_12478 = MkKeySymbol #{const XKB_KEY_braille_dots_12478} 
-- #define XKB_KEY_braille_dots_3478          0x10028cc  /* U+28cc BRAILLE PATTERN DOTS-3478 */
pattern XKB_KEY_braille_dots_3478 :: KeySymbol
pattern XKB_KEY_braille_dots_3478 = MkKeySymbol #{const XKB_KEY_braille_dots_3478} 
-- #define XKB_KEY_braille_dots_13478         0x10028cd  /* U+28cd BRAILLE PATTERN DOTS-13478 */
pattern XKB_KEY_braille_dots_13478 :: KeySymbol
pattern XKB_KEY_braille_dots_13478 = MkKeySymbol #{const XKB_KEY_braille_dots_13478} 
-- #define XKB_KEY_braille_dots_23478         0x10028ce  /* U+28ce BRAILLE PATTERN DOTS-23478 */
pattern XKB_KEY_braille_dots_23478 :: KeySymbol
pattern XKB_KEY_braille_dots_23478 = MkKeySymbol #{const XKB_KEY_braille_dots_23478} 
-- #define XKB_KEY_braille_dots_123478        0x10028cf  /* U+28cf BRAILLE PATTERN DOTS-123478 */
pattern XKB_KEY_braille_dots_123478 :: KeySymbol
pattern XKB_KEY_braille_dots_123478 = MkKeySymbol #{const XKB_KEY_braille_dots_123478} 
-- #define XKB_KEY_braille_dots_578           0x10028d0  /* U+28d0 BRAILLE PATTERN DOTS-578 */
pattern XKB_KEY_braille_dots_578 :: KeySymbol
pattern XKB_KEY_braille_dots_578 = MkKeySymbol #{const XKB_KEY_braille_dots_578} 
-- #define XKB_KEY_braille_dots_1578          0x10028d1  /* U+28d1 BRAILLE PATTERN DOTS-1578 */
pattern XKB_KEY_braille_dots_1578 :: KeySymbol
pattern XKB_KEY_braille_dots_1578 = MkKeySymbol #{const XKB_KEY_braille_dots_1578} 
-- #define XKB_KEY_braille_dots_2578          0x10028d2  /* U+28d2 BRAILLE PATTERN DOTS-2578 */
pattern XKB_KEY_braille_dots_2578 :: KeySymbol
pattern XKB_KEY_braille_dots_2578 = MkKeySymbol #{const XKB_KEY_braille_dots_2578} 
-- #define XKB_KEY_braille_dots_12578         0x10028d3  /* U+28d3 BRAILLE PATTERN DOTS-12578 */
pattern XKB_KEY_braille_dots_12578 :: KeySymbol
pattern XKB_KEY_braille_dots_12578 = MkKeySymbol #{const XKB_KEY_braille_dots_12578} 
-- #define XKB_KEY_braille_dots_3578          0x10028d4  /* U+28d4 BRAILLE PATTERN DOTS-3578 */
pattern XKB_KEY_braille_dots_3578 :: KeySymbol
pattern XKB_KEY_braille_dots_3578 = MkKeySymbol #{const XKB_KEY_braille_dots_3578} 
-- #define XKB_KEY_braille_dots_13578         0x10028d5  /* U+28d5 BRAILLE PATTERN DOTS-13578 */
pattern XKB_KEY_braille_dots_13578 :: KeySymbol
pattern XKB_KEY_braille_dots_13578 = MkKeySymbol #{const XKB_KEY_braille_dots_13578} 
-- #define XKB_KEY_braille_dots_23578         0x10028d6  /* U+28d6 BRAILLE PATTERN DOTS-23578 */
pattern XKB_KEY_braille_dots_23578 :: KeySymbol
pattern XKB_KEY_braille_dots_23578 = MkKeySymbol #{const XKB_KEY_braille_dots_23578} 
-- #define XKB_KEY_braille_dots_123578        0x10028d7  /* U+28d7 BRAILLE PATTERN DOTS-123578 */
pattern XKB_KEY_braille_dots_123578 :: KeySymbol
pattern XKB_KEY_braille_dots_123578 = MkKeySymbol #{const XKB_KEY_braille_dots_123578} 
-- #define XKB_KEY_braille_dots_4578          0x10028d8  /* U+28d8 BRAILLE PATTERN DOTS-4578 */
pattern XKB_KEY_braille_dots_4578 :: KeySymbol
pattern XKB_KEY_braille_dots_4578 = MkKeySymbol #{const XKB_KEY_braille_dots_4578} 
-- #define XKB_KEY_braille_dots_14578         0x10028d9  /* U+28d9 BRAILLE PATTERN DOTS-14578 */
pattern XKB_KEY_braille_dots_14578 :: KeySymbol
pattern XKB_KEY_braille_dots_14578 = MkKeySymbol #{const XKB_KEY_braille_dots_14578} 
-- #define XKB_KEY_braille_dots_24578         0x10028da  /* U+28da BRAILLE PATTERN DOTS-24578 */
pattern XKB_KEY_braille_dots_24578 :: KeySymbol
pattern XKB_KEY_braille_dots_24578 = MkKeySymbol #{const XKB_KEY_braille_dots_24578} 
-- #define XKB_KEY_braille_dots_124578        0x10028db  /* U+28db BRAILLE PATTERN DOTS-124578 */
pattern XKB_KEY_braille_dots_124578 :: KeySymbol
pattern XKB_KEY_braille_dots_124578 = MkKeySymbol #{const XKB_KEY_braille_dots_124578} 
-- #define XKB_KEY_braille_dots_34578         0x10028dc  /* U+28dc BRAILLE PATTERN DOTS-34578 */
pattern XKB_KEY_braille_dots_34578 :: KeySymbol
pattern XKB_KEY_braille_dots_34578 = MkKeySymbol #{const XKB_KEY_braille_dots_34578} 
-- #define XKB_KEY_braille_dots_134578        0x10028dd  /* U+28dd BRAILLE PATTERN DOTS-134578 */
pattern XKB_KEY_braille_dots_134578 :: KeySymbol
pattern XKB_KEY_braille_dots_134578 = MkKeySymbol #{const XKB_KEY_braille_dots_134578} 
-- #define XKB_KEY_braille_dots_234578        0x10028de  /* U+28de BRAILLE PATTERN DOTS-234578 */
pattern XKB_KEY_braille_dots_234578 :: KeySymbol
pattern XKB_KEY_braille_dots_234578 = MkKeySymbol #{const XKB_KEY_braille_dots_234578} 
-- #define XKB_KEY_braille_dots_1234578       0x10028df  /* U+28df BRAILLE PATTERN DOTS-1234578 */
pattern XKB_KEY_braille_dots_1234578 :: KeySymbol
pattern XKB_KEY_braille_dots_1234578 = MkKeySymbol #{const XKB_KEY_braille_dots_1234578} 
-- #define XKB_KEY_braille_dots_678           0x10028e0  /* U+28e0 BRAILLE PATTERN DOTS-678 */
pattern XKB_KEY_braille_dots_678 :: KeySymbol
pattern XKB_KEY_braille_dots_678 = MkKeySymbol #{const XKB_KEY_braille_dots_678} 
-- #define XKB_KEY_braille_dots_1678          0x10028e1  /* U+28e1 BRAILLE PATTERN DOTS-1678 */
pattern XKB_KEY_braille_dots_1678 :: KeySymbol
pattern XKB_KEY_braille_dots_1678 = MkKeySymbol #{const XKB_KEY_braille_dots_1678} 
-- #define XKB_KEY_braille_dots_2678          0x10028e2  /* U+28e2 BRAILLE PATTERN DOTS-2678 */
pattern XKB_KEY_braille_dots_2678 :: KeySymbol
pattern XKB_KEY_braille_dots_2678 = MkKeySymbol #{const XKB_KEY_braille_dots_2678} 
-- #define XKB_KEY_braille_dots_12678         0x10028e3  /* U+28e3 BRAILLE PATTERN DOTS-12678 */
pattern XKB_KEY_braille_dots_12678 :: KeySymbol
pattern XKB_KEY_braille_dots_12678 = MkKeySymbol #{const XKB_KEY_braille_dots_12678} 
-- #define XKB_KEY_braille_dots_3678          0x10028e4  /* U+28e4 BRAILLE PATTERN DOTS-3678 */
pattern XKB_KEY_braille_dots_3678 :: KeySymbol
pattern XKB_KEY_braille_dots_3678 = MkKeySymbol #{const XKB_KEY_braille_dots_3678} 
-- #define XKB_KEY_braille_dots_13678         0x10028e5  /* U+28e5 BRAILLE PATTERN DOTS-13678 */
pattern XKB_KEY_braille_dots_13678 :: KeySymbol
pattern XKB_KEY_braille_dots_13678 = MkKeySymbol #{const XKB_KEY_braille_dots_13678} 
-- #define XKB_KEY_braille_dots_23678         0x10028e6  /* U+28e6 BRAILLE PATTERN DOTS-23678 */
pattern XKB_KEY_braille_dots_23678 :: KeySymbol
pattern XKB_KEY_braille_dots_23678 = MkKeySymbol #{const XKB_KEY_braille_dots_23678} 
-- #define XKB_KEY_braille_dots_123678        0x10028e7  /* U+28e7 BRAILLE PATTERN DOTS-123678 */
pattern XKB_KEY_braille_dots_123678 :: KeySymbol
pattern XKB_KEY_braille_dots_123678 = MkKeySymbol #{const XKB_KEY_braille_dots_123678} 
-- #define XKB_KEY_braille_dots_4678          0x10028e8  /* U+28e8 BRAILLE PATTERN DOTS-4678 */
pattern XKB_KEY_braille_dots_4678 :: KeySymbol
pattern XKB_KEY_braille_dots_4678 = MkKeySymbol #{const XKB_KEY_braille_dots_4678} 
-- #define XKB_KEY_braille_dots_14678         0x10028e9  /* U+28e9 BRAILLE PATTERN DOTS-14678 */
pattern XKB_KEY_braille_dots_14678 :: KeySymbol
pattern XKB_KEY_braille_dots_14678 = MkKeySymbol #{const XKB_KEY_braille_dots_14678} 
-- #define XKB_KEY_braille_dots_24678         0x10028ea  /* U+28ea BRAILLE PATTERN DOTS-24678 */
pattern XKB_KEY_braille_dots_24678 :: KeySymbol
pattern XKB_KEY_braille_dots_24678 = MkKeySymbol #{const XKB_KEY_braille_dots_24678} 
-- #define XKB_KEY_braille_dots_124678        0x10028eb  /* U+28eb BRAILLE PATTERN DOTS-124678 */
pattern XKB_KEY_braille_dots_124678 :: KeySymbol
pattern XKB_KEY_braille_dots_124678 = MkKeySymbol #{const XKB_KEY_braille_dots_124678} 
-- #define XKB_KEY_braille_dots_34678         0x10028ec  /* U+28ec BRAILLE PATTERN DOTS-34678 */
pattern XKB_KEY_braille_dots_34678 :: KeySymbol
pattern XKB_KEY_braille_dots_34678 = MkKeySymbol #{const XKB_KEY_braille_dots_34678} 
-- #define XKB_KEY_braille_dots_134678        0x10028ed  /* U+28ed BRAILLE PATTERN DOTS-134678 */
pattern XKB_KEY_braille_dots_134678 :: KeySymbol
pattern XKB_KEY_braille_dots_134678 = MkKeySymbol #{const XKB_KEY_braille_dots_134678} 
-- #define XKB_KEY_braille_dots_234678        0x10028ee  /* U+28ee BRAILLE PATTERN DOTS-234678 */
pattern XKB_KEY_braille_dots_234678 :: KeySymbol
pattern XKB_KEY_braille_dots_234678 = MkKeySymbol #{const XKB_KEY_braille_dots_234678} 
-- #define XKB_KEY_braille_dots_1234678       0x10028ef  /* U+28ef BRAILLE PATTERN DOTS-1234678 */
pattern XKB_KEY_braille_dots_1234678 :: KeySymbol
pattern XKB_KEY_braille_dots_1234678 = MkKeySymbol #{const XKB_KEY_braille_dots_1234678} 
-- #define XKB_KEY_braille_dots_5678          0x10028f0  /* U+28f0 BRAILLE PATTERN DOTS-5678 */
pattern XKB_KEY_braille_dots_5678 :: KeySymbol
pattern XKB_KEY_braille_dots_5678 = MkKeySymbol #{const XKB_KEY_braille_dots_5678} 
-- #define XKB_KEY_braille_dots_15678         0x10028f1  /* U+28f1 BRAILLE PATTERN DOTS-15678 */
pattern XKB_KEY_braille_dots_15678 :: KeySymbol
pattern XKB_KEY_braille_dots_15678 = MkKeySymbol #{const XKB_KEY_braille_dots_15678} 
-- #define XKB_KEY_braille_dots_25678         0x10028f2  /* U+28f2 BRAILLE PATTERN DOTS-25678 */
pattern XKB_KEY_braille_dots_25678 :: KeySymbol
pattern XKB_KEY_braille_dots_25678 = MkKeySymbol #{const XKB_KEY_braille_dots_25678} 
-- #define XKB_KEY_braille_dots_125678        0x10028f3  /* U+28f3 BRAILLE PATTERN DOTS-125678 */
pattern XKB_KEY_braille_dots_125678 :: KeySymbol
pattern XKB_KEY_braille_dots_125678 = MkKeySymbol #{const XKB_KEY_braille_dots_125678} 
-- #define XKB_KEY_braille_dots_35678         0x10028f4  /* U+28f4 BRAILLE PATTERN DOTS-35678 */
pattern XKB_KEY_braille_dots_35678 :: KeySymbol
pattern XKB_KEY_braille_dots_35678 = MkKeySymbol #{const XKB_KEY_braille_dots_35678} 
-- #define XKB_KEY_braille_dots_135678        0x10028f5  /* U+28f5 BRAILLE PATTERN DOTS-135678 */
pattern XKB_KEY_braille_dots_135678 :: KeySymbol
pattern XKB_KEY_braille_dots_135678 = MkKeySymbol #{const XKB_KEY_braille_dots_135678} 
-- #define XKB_KEY_braille_dots_235678        0x10028f6  /* U+28f6 BRAILLE PATTERN DOTS-235678 */
pattern XKB_KEY_braille_dots_235678 :: KeySymbol
pattern XKB_KEY_braille_dots_235678 = MkKeySymbol #{const XKB_KEY_braille_dots_235678} 
-- #define XKB_KEY_braille_dots_1235678       0x10028f7  /* U+28f7 BRAILLE PATTERN DOTS-1235678 */
pattern XKB_KEY_braille_dots_1235678 :: KeySymbol
pattern XKB_KEY_braille_dots_1235678 = MkKeySymbol #{const XKB_KEY_braille_dots_1235678} 
-- #define XKB_KEY_braille_dots_45678         0x10028f8  /* U+28f8 BRAILLE PATTERN DOTS-45678 */
pattern XKB_KEY_braille_dots_45678 :: KeySymbol
pattern XKB_KEY_braille_dots_45678 = MkKeySymbol #{const XKB_KEY_braille_dots_45678} 
-- #define XKB_KEY_braille_dots_145678        0x10028f9  /* U+28f9 BRAILLE PATTERN DOTS-145678 */
pattern XKB_KEY_braille_dots_145678 :: KeySymbol
pattern XKB_KEY_braille_dots_145678 = MkKeySymbol #{const XKB_KEY_braille_dots_145678} 
-- #define XKB_KEY_braille_dots_245678        0x10028fa  /* U+28fa BRAILLE PATTERN DOTS-245678 */
pattern XKB_KEY_braille_dots_245678 :: KeySymbol
pattern XKB_KEY_braille_dots_245678 = MkKeySymbol #{const XKB_KEY_braille_dots_245678} 
-- #define XKB_KEY_braille_dots_1245678       0x10028fb  /* U+28fb BRAILLE PATTERN DOTS-1245678 */
pattern XKB_KEY_braille_dots_1245678 :: KeySymbol
pattern XKB_KEY_braille_dots_1245678 = MkKeySymbol #{const XKB_KEY_braille_dots_1245678} 
-- #define XKB_KEY_braille_dots_345678        0x10028fc  /* U+28fc BRAILLE PATTERN DOTS-345678 */
pattern XKB_KEY_braille_dots_345678 :: KeySymbol
pattern XKB_KEY_braille_dots_345678 = MkKeySymbol #{const XKB_KEY_braille_dots_345678} 
-- #define XKB_KEY_braille_dots_1345678       0x10028fd  /* U+28fd BRAILLE PATTERN DOTS-1345678 */
pattern XKB_KEY_braille_dots_1345678 :: KeySymbol
pattern XKB_KEY_braille_dots_1345678 = MkKeySymbol #{const XKB_KEY_braille_dots_1345678} 
-- #define XKB_KEY_braille_dots_2345678       0x10028fe  /* U+28fe BRAILLE PATTERN DOTS-2345678 */
pattern XKB_KEY_braille_dots_2345678 :: KeySymbol
pattern XKB_KEY_braille_dots_2345678 = MkKeySymbol #{const XKB_KEY_braille_dots_2345678} 
-- #define XKB_KEY_braille_dots_12345678      0x10028ff  /* U+28ff BRAILLE PATTERN DOTS-12345678 */
pattern XKB_KEY_braille_dots_12345678 :: KeySymbol
pattern XKB_KEY_braille_dots_12345678 = MkKeySymbol #{const XKB_KEY_braille_dots_12345678} 
-- #define XKB_KEY_Sinh_ng            0x1000d82  /* U+0D82 SINHALA ANUSVARAYA */
pattern XKB_KEY_Sinh_ng :: KeySymbol
pattern XKB_KEY_Sinh_ng = MkKeySymbol #{const XKB_KEY_Sinh_ng} 
-- #define XKB_KEY_Sinh_h2            0x1000d83  /* U+0D83 SINHALA VISARGAYA */
pattern XKB_KEY_Sinh_h2 :: KeySymbol
pattern XKB_KEY_Sinh_h2 = MkKeySymbol #{const XKB_KEY_Sinh_h2} 
-- #define XKB_KEY_Sinh_a             0x1000d85  /* U+0D85 SINHALA AYANNA */
pattern XKB_KEY_Sinh_a :: KeySymbol
pattern XKB_KEY_Sinh_a = MkKeySymbol #{const XKB_KEY_Sinh_a} 
-- #define XKB_KEY_Sinh_aa            0x1000d86  /* U+0D86 SINHALA AAYANNA */
pattern XKB_KEY_Sinh_aa :: KeySymbol
pattern XKB_KEY_Sinh_aa = MkKeySymbol #{const XKB_KEY_Sinh_aa} 
-- #define XKB_KEY_Sinh_ae            0x1000d87  /* U+0D87 SINHALA AEYANNA */
pattern XKB_KEY_Sinh_ae :: KeySymbol
pattern XKB_KEY_Sinh_ae = MkKeySymbol #{const XKB_KEY_Sinh_ae} 
-- #define XKB_KEY_Sinh_aee           0x1000d88  /* U+0D88 SINHALA AEEYANNA */
pattern XKB_KEY_Sinh_aee :: KeySymbol
pattern XKB_KEY_Sinh_aee = MkKeySymbol #{const XKB_KEY_Sinh_aee} 
-- #define XKB_KEY_Sinh_i             0x1000d89  /* U+0D89 SINHALA IYANNA */
pattern XKB_KEY_Sinh_i :: KeySymbol
pattern XKB_KEY_Sinh_i = MkKeySymbol #{const XKB_KEY_Sinh_i} 
-- #define XKB_KEY_Sinh_ii            0x1000d8a  /* U+0D8A SINHALA IIYANNA */
pattern XKB_KEY_Sinh_ii :: KeySymbol
pattern XKB_KEY_Sinh_ii = MkKeySymbol #{const XKB_KEY_Sinh_ii} 
-- #define XKB_KEY_Sinh_u             0x1000d8b  /* U+0D8B SINHALA UYANNA */
pattern XKB_KEY_Sinh_u :: KeySymbol
pattern XKB_KEY_Sinh_u = MkKeySymbol #{const XKB_KEY_Sinh_u} 
-- #define XKB_KEY_Sinh_uu            0x1000d8c  /* U+0D8C SINHALA UUYANNA */
pattern XKB_KEY_Sinh_uu :: KeySymbol
pattern XKB_KEY_Sinh_uu = MkKeySymbol #{const XKB_KEY_Sinh_uu} 
-- #define XKB_KEY_Sinh_ri            0x1000d8d  /* U+0D8D SINHALA IRUYANNA */
pattern XKB_KEY_Sinh_ri :: KeySymbol
pattern XKB_KEY_Sinh_ri = MkKeySymbol #{const XKB_KEY_Sinh_ri} 
-- #define XKB_KEY_Sinh_rii           0x1000d8e  /* U+0D8E SINHALA IRUUYANNA */
pattern XKB_KEY_Sinh_rii :: KeySymbol
pattern XKB_KEY_Sinh_rii = MkKeySymbol #{const XKB_KEY_Sinh_rii} 
-- #define XKB_KEY_Sinh_lu            0x1000d8f  /* U+0D8F SINHALA ILUYANNA */
pattern XKB_KEY_Sinh_lu :: KeySymbol
pattern XKB_KEY_Sinh_lu = MkKeySymbol #{const XKB_KEY_Sinh_lu} 
-- #define XKB_KEY_Sinh_luu           0x1000d90  /* U+0D90 SINHALA ILUUYANNA */
pattern XKB_KEY_Sinh_luu :: KeySymbol
pattern XKB_KEY_Sinh_luu = MkKeySymbol #{const XKB_KEY_Sinh_luu} 
-- #define XKB_KEY_Sinh_e             0x1000d91  /* U+0D91 SINHALA EYANNA */
pattern XKB_KEY_Sinh_e :: KeySymbol
pattern XKB_KEY_Sinh_e = MkKeySymbol #{const XKB_KEY_Sinh_e} 
-- #define XKB_KEY_Sinh_ee            0x1000d92  /* U+0D92 SINHALA EEYANNA */
pattern XKB_KEY_Sinh_ee :: KeySymbol
pattern XKB_KEY_Sinh_ee = MkKeySymbol #{const XKB_KEY_Sinh_ee} 
-- #define XKB_KEY_Sinh_ai            0x1000d93  /* U+0D93 SINHALA AIYANNA */
pattern XKB_KEY_Sinh_ai :: KeySymbol
pattern XKB_KEY_Sinh_ai = MkKeySymbol #{const XKB_KEY_Sinh_ai} 
-- #define XKB_KEY_Sinh_o             0x1000d94  /* U+0D94 SINHALA OYANNA */
pattern XKB_KEY_Sinh_o :: KeySymbol
pattern XKB_KEY_Sinh_o = MkKeySymbol #{const XKB_KEY_Sinh_o} 
-- #define XKB_KEY_Sinh_oo            0x1000d95  /* U+0D95 SINHALA OOYANNA */
pattern XKB_KEY_Sinh_oo :: KeySymbol
pattern XKB_KEY_Sinh_oo = MkKeySymbol #{const XKB_KEY_Sinh_oo} 
-- #define XKB_KEY_Sinh_au            0x1000d96  /* U+0D96 SINHALA AUYANNA */
pattern XKB_KEY_Sinh_au :: KeySymbol
pattern XKB_KEY_Sinh_au = MkKeySymbol #{const XKB_KEY_Sinh_au} 
-- #define XKB_KEY_Sinh_ka            0x1000d9a  /* U+0D9A SINHALA KAYANNA */
pattern XKB_KEY_Sinh_ka :: KeySymbol
pattern XKB_KEY_Sinh_ka = MkKeySymbol #{const XKB_KEY_Sinh_ka} 
-- #define XKB_KEY_Sinh_kha           0x1000d9b  /* U+0D9B SINHALA MAHA. KAYANNA */
pattern XKB_KEY_Sinh_kha :: KeySymbol
pattern XKB_KEY_Sinh_kha = MkKeySymbol #{const XKB_KEY_Sinh_kha} 
-- #define XKB_KEY_Sinh_ga            0x1000d9c  /* U+0D9C SINHALA GAYANNA */
pattern XKB_KEY_Sinh_ga :: KeySymbol
pattern XKB_KEY_Sinh_ga = MkKeySymbol #{const XKB_KEY_Sinh_ga} 
-- #define XKB_KEY_Sinh_gha           0x1000d9d  /* U+0D9D SINHALA MAHA. GAYANNA */
pattern XKB_KEY_Sinh_gha :: KeySymbol
pattern XKB_KEY_Sinh_gha = MkKeySymbol #{const XKB_KEY_Sinh_gha} 
-- #define XKB_KEY_Sinh_ng2           0x1000d9e  /* U+0D9E SINHALA KANTAJA NAASIKYAYA */
pattern XKB_KEY_Sinh_ng2 :: KeySymbol
pattern XKB_KEY_Sinh_ng2 = MkKeySymbol #{const XKB_KEY_Sinh_ng2} 
-- #define XKB_KEY_Sinh_nga           0x1000d9f  /* U+0D9F SINHALA SANYAKA GAYANNA */
pattern XKB_KEY_Sinh_nga :: KeySymbol
pattern XKB_KEY_Sinh_nga = MkKeySymbol #{const XKB_KEY_Sinh_nga} 
-- #define XKB_KEY_Sinh_ca            0x1000da0  /* U+0DA0 SINHALA CAYANNA */
pattern XKB_KEY_Sinh_ca :: KeySymbol
pattern XKB_KEY_Sinh_ca = MkKeySymbol #{const XKB_KEY_Sinh_ca} 
-- #define XKB_KEY_Sinh_cha           0x1000da1  /* U+0DA1 SINHALA MAHA. CAYANNA */
pattern XKB_KEY_Sinh_cha :: KeySymbol
pattern XKB_KEY_Sinh_cha = MkKeySymbol #{const XKB_KEY_Sinh_cha} 
-- #define XKB_KEY_Sinh_ja            0x1000da2  /* U+0DA2 SINHALA JAYANNA */
pattern XKB_KEY_Sinh_ja :: KeySymbol
pattern XKB_KEY_Sinh_ja = MkKeySymbol #{const XKB_KEY_Sinh_ja} 
-- #define XKB_KEY_Sinh_jha           0x1000da3  /* U+0DA3 SINHALA MAHA. JAYANNA */
pattern XKB_KEY_Sinh_jha :: KeySymbol
pattern XKB_KEY_Sinh_jha = MkKeySymbol #{const XKB_KEY_Sinh_jha} 
-- #define XKB_KEY_Sinh_nya           0x1000da4  /* U+0DA4 SINHALA TAALUJA NAASIKYAYA */
pattern XKB_KEY_Sinh_nya :: KeySymbol
pattern XKB_KEY_Sinh_nya = MkKeySymbol #{const XKB_KEY_Sinh_nya} 
-- #define XKB_KEY_Sinh_jnya          0x1000da5  /* U+0DA5 SINHALA TAALUJA SANYOOGA NAASIKYAYA */
pattern XKB_KEY_Sinh_jnya :: KeySymbol
pattern XKB_KEY_Sinh_jnya = MkKeySymbol #{const XKB_KEY_Sinh_jnya} 
-- #define XKB_KEY_Sinh_nja           0x1000da6  /* U+0DA6 SINHALA SANYAKA JAYANNA */
pattern XKB_KEY_Sinh_nja :: KeySymbol
pattern XKB_KEY_Sinh_nja = MkKeySymbol #{const XKB_KEY_Sinh_nja} 
-- #define XKB_KEY_Sinh_tta           0x1000da7  /* U+0DA7 SINHALA TTAYANNA */
pattern XKB_KEY_Sinh_tta :: KeySymbol
pattern XKB_KEY_Sinh_tta = MkKeySymbol #{const XKB_KEY_Sinh_tta} 
-- #define XKB_KEY_Sinh_ttha          0x1000da8  /* U+0DA8 SINHALA MAHA. TTAYANNA */
pattern XKB_KEY_Sinh_ttha :: KeySymbol
pattern XKB_KEY_Sinh_ttha = MkKeySymbol #{const XKB_KEY_Sinh_ttha} 
-- #define XKB_KEY_Sinh_dda           0x1000da9  /* U+0DA9 SINHALA DDAYANNA */
pattern XKB_KEY_Sinh_dda :: KeySymbol
pattern XKB_KEY_Sinh_dda = MkKeySymbol #{const XKB_KEY_Sinh_dda} 
-- #define XKB_KEY_Sinh_ddha          0x1000daa  /* U+0DAA SINHALA MAHA. DDAYANNA */
pattern XKB_KEY_Sinh_ddha :: KeySymbol
pattern XKB_KEY_Sinh_ddha = MkKeySymbol #{const XKB_KEY_Sinh_ddha} 
-- #define XKB_KEY_Sinh_nna           0x1000dab  /* U+0DAB SINHALA MUURDHAJA NAYANNA */
pattern XKB_KEY_Sinh_nna :: KeySymbol
pattern XKB_KEY_Sinh_nna = MkKeySymbol #{const XKB_KEY_Sinh_nna} 
-- #define XKB_KEY_Sinh_ndda          0x1000dac  /* U+0DAC SINHALA SANYAKA DDAYANNA */
pattern XKB_KEY_Sinh_ndda :: KeySymbol
pattern XKB_KEY_Sinh_ndda = MkKeySymbol #{const XKB_KEY_Sinh_ndda} 
-- #define XKB_KEY_Sinh_tha           0x1000dad  /* U+0DAD SINHALA TAYANNA */
pattern XKB_KEY_Sinh_tha :: KeySymbol
pattern XKB_KEY_Sinh_tha = MkKeySymbol #{const XKB_KEY_Sinh_tha} 
-- #define XKB_KEY_Sinh_thha          0x1000dae  /* U+0DAE SINHALA MAHA. TAYANNA */
pattern XKB_KEY_Sinh_thha :: KeySymbol
pattern XKB_KEY_Sinh_thha = MkKeySymbol #{const XKB_KEY_Sinh_thha} 
-- #define XKB_KEY_Sinh_dha           0x1000daf  /* U+0DAF SINHALA DAYANNA */
pattern XKB_KEY_Sinh_dha :: KeySymbol
pattern XKB_KEY_Sinh_dha = MkKeySymbol #{const XKB_KEY_Sinh_dha} 
-- #define XKB_KEY_Sinh_dhha          0x1000db0  /* U+0DB0 SINHALA MAHA. DAYANNA */
pattern XKB_KEY_Sinh_dhha :: KeySymbol
pattern XKB_KEY_Sinh_dhha = MkKeySymbol #{const XKB_KEY_Sinh_dhha} 
-- #define XKB_KEY_Sinh_na            0x1000db1  /* U+0DB1 SINHALA DANTAJA NAYANNA */
pattern XKB_KEY_Sinh_na :: KeySymbol
pattern XKB_KEY_Sinh_na = MkKeySymbol #{const XKB_KEY_Sinh_na} 
-- #define XKB_KEY_Sinh_ndha          0x1000db3  /* U+0DB3 SINHALA SANYAKA DAYANNA */
pattern XKB_KEY_Sinh_ndha :: KeySymbol
pattern XKB_KEY_Sinh_ndha = MkKeySymbol #{const XKB_KEY_Sinh_ndha} 
-- #define XKB_KEY_Sinh_pa            0x1000db4  /* U+0DB4 SINHALA PAYANNA */
pattern XKB_KEY_Sinh_pa :: KeySymbol
pattern XKB_KEY_Sinh_pa = MkKeySymbol #{const XKB_KEY_Sinh_pa} 
-- #define XKB_KEY_Sinh_pha           0x1000db5  /* U+0DB5 SINHALA MAHA. PAYANNA */
pattern XKB_KEY_Sinh_pha :: KeySymbol
pattern XKB_KEY_Sinh_pha = MkKeySymbol #{const XKB_KEY_Sinh_pha} 
-- #define XKB_KEY_Sinh_ba            0x1000db6  /* U+0DB6 SINHALA BAYANNA */
pattern XKB_KEY_Sinh_ba :: KeySymbol
pattern XKB_KEY_Sinh_ba = MkKeySymbol #{const XKB_KEY_Sinh_ba} 
-- #define XKB_KEY_Sinh_bha           0x1000db7  /* U+0DB7 SINHALA MAHA. BAYANNA */
pattern XKB_KEY_Sinh_bha :: KeySymbol
pattern XKB_KEY_Sinh_bha = MkKeySymbol #{const XKB_KEY_Sinh_bha} 
-- #define XKB_KEY_Sinh_ma            0x1000db8  /* U+0DB8 SINHALA MAYANNA */
pattern XKB_KEY_Sinh_ma :: KeySymbol
pattern XKB_KEY_Sinh_ma = MkKeySymbol #{const XKB_KEY_Sinh_ma} 
-- #define XKB_KEY_Sinh_mba           0x1000db9  /* U+0DB9 SINHALA AMBA BAYANNA */
pattern XKB_KEY_Sinh_mba :: KeySymbol
pattern XKB_KEY_Sinh_mba = MkKeySymbol #{const XKB_KEY_Sinh_mba} 
-- #define XKB_KEY_Sinh_ya            0x1000dba  /* U+0DBA SINHALA YAYANNA */
pattern XKB_KEY_Sinh_ya :: KeySymbol
pattern XKB_KEY_Sinh_ya = MkKeySymbol #{const XKB_KEY_Sinh_ya} 
-- #define XKB_KEY_Sinh_ra            0x1000dbb  /* U+0DBB SINHALA RAYANNA */
pattern XKB_KEY_Sinh_ra :: KeySymbol
pattern XKB_KEY_Sinh_ra = MkKeySymbol #{const XKB_KEY_Sinh_ra} 
-- #define XKB_KEY_Sinh_la            0x1000dbd  /* U+0DBD SINHALA DANTAJA LAYANNA */
pattern XKB_KEY_Sinh_la :: KeySymbol
pattern XKB_KEY_Sinh_la = MkKeySymbol #{const XKB_KEY_Sinh_la} 
-- #define XKB_KEY_Sinh_va            0x1000dc0  /* U+0DC0 SINHALA VAYANNA */
pattern XKB_KEY_Sinh_va :: KeySymbol
pattern XKB_KEY_Sinh_va = MkKeySymbol #{const XKB_KEY_Sinh_va} 
-- #define XKB_KEY_Sinh_sha           0x1000dc1  /* U+0DC1 SINHALA TAALUJA SAYANNA */
pattern XKB_KEY_Sinh_sha :: KeySymbol
pattern XKB_KEY_Sinh_sha = MkKeySymbol #{const XKB_KEY_Sinh_sha} 
-- #define XKB_KEY_Sinh_ssha          0x1000dc2  /* U+0DC2 SINHALA MUURDHAJA SAYANNA */
pattern XKB_KEY_Sinh_ssha :: KeySymbol
pattern XKB_KEY_Sinh_ssha = MkKeySymbol #{const XKB_KEY_Sinh_ssha} 
-- #define XKB_KEY_Sinh_sa            0x1000dc3  /* U+0DC3 SINHALA DANTAJA SAYANNA */
pattern XKB_KEY_Sinh_sa :: KeySymbol
pattern XKB_KEY_Sinh_sa = MkKeySymbol #{const XKB_KEY_Sinh_sa} 
-- #define XKB_KEY_Sinh_ha            0x1000dc4  /* U+0DC4 SINHALA HAYANNA */
pattern XKB_KEY_Sinh_ha :: KeySymbol
pattern XKB_KEY_Sinh_ha = MkKeySymbol #{const XKB_KEY_Sinh_ha} 
-- #define XKB_KEY_Sinh_lla           0x1000dc5  /* U+0DC5 SINHALA MUURDHAJA LAYANNA */
pattern XKB_KEY_Sinh_lla :: KeySymbol
pattern XKB_KEY_Sinh_lla = MkKeySymbol #{const XKB_KEY_Sinh_lla} 
-- #define XKB_KEY_Sinh_fa            0x1000dc6  /* U+0DC6 SINHALA FAYANNA */
pattern XKB_KEY_Sinh_fa :: KeySymbol
pattern XKB_KEY_Sinh_fa = MkKeySymbol #{const XKB_KEY_Sinh_fa} 
-- #define XKB_KEY_Sinh_al            0x1000dca  /* U+0DCA SINHALA AL-LAKUNA */
pattern XKB_KEY_Sinh_al :: KeySymbol
pattern XKB_KEY_Sinh_al = MkKeySymbol #{const XKB_KEY_Sinh_al} 
-- #define XKB_KEY_Sinh_aa2           0x1000dcf  /* U+0DCF SINHALA AELA-PILLA */
pattern XKB_KEY_Sinh_aa2 :: KeySymbol
pattern XKB_KEY_Sinh_aa2 = MkKeySymbol #{const XKB_KEY_Sinh_aa2} 
-- #define XKB_KEY_Sinh_ae2           0x1000dd0  /* U+0DD0 SINHALA AEDA-PILLA */
pattern XKB_KEY_Sinh_ae2 :: KeySymbol
pattern XKB_KEY_Sinh_ae2 = MkKeySymbol #{const XKB_KEY_Sinh_ae2} 
-- #define XKB_KEY_Sinh_aee2          0x1000dd1  /* U+0DD1 SINHALA DIGA AEDA-PILLA */
pattern XKB_KEY_Sinh_aee2 :: KeySymbol
pattern XKB_KEY_Sinh_aee2 = MkKeySymbol #{const XKB_KEY_Sinh_aee2} 
-- #define XKB_KEY_Sinh_i2            0x1000dd2  /* U+0DD2 SINHALA IS-PILLA */
pattern XKB_KEY_Sinh_i2 :: KeySymbol
pattern XKB_KEY_Sinh_i2 = MkKeySymbol #{const XKB_KEY_Sinh_i2} 
-- #define XKB_KEY_Sinh_ii2           0x1000dd3  /* U+0DD3 SINHALA DIGA IS-PILLA */
pattern XKB_KEY_Sinh_ii2 :: KeySymbol
pattern XKB_KEY_Sinh_ii2 = MkKeySymbol #{const XKB_KEY_Sinh_ii2} 
-- #define XKB_KEY_Sinh_u2            0x1000dd4  /* U+0DD4 SINHALA PAA-PILLA */
pattern XKB_KEY_Sinh_u2 :: KeySymbol
pattern XKB_KEY_Sinh_u2 = MkKeySymbol #{const XKB_KEY_Sinh_u2} 
-- #define XKB_KEY_Sinh_uu2           0x1000dd6  /* U+0DD6 SINHALA DIGA PAA-PILLA */
pattern XKB_KEY_Sinh_uu2 :: KeySymbol
pattern XKB_KEY_Sinh_uu2 = MkKeySymbol #{const XKB_KEY_Sinh_uu2} 
-- #define XKB_KEY_Sinh_ru2           0x1000dd8  /* U+0DD8 SINHALA GAETTA-PILLA */
pattern XKB_KEY_Sinh_ru2 :: KeySymbol
pattern XKB_KEY_Sinh_ru2 = MkKeySymbol #{const XKB_KEY_Sinh_ru2} 
-- #define XKB_KEY_Sinh_e2            0x1000dd9  /* U+0DD9 SINHALA KOMBUVA */
pattern XKB_KEY_Sinh_e2 :: KeySymbol
pattern XKB_KEY_Sinh_e2 = MkKeySymbol #{const XKB_KEY_Sinh_e2} 
-- #define XKB_KEY_Sinh_ee2           0x1000dda  /* U+0DDA SINHALA DIGA KOMBUVA */
pattern XKB_KEY_Sinh_ee2 :: KeySymbol
pattern XKB_KEY_Sinh_ee2 = MkKeySymbol #{const XKB_KEY_Sinh_ee2} 
-- #define XKB_KEY_Sinh_ai2           0x1000ddb  /* U+0DDB SINHALA KOMBU DEKA */
pattern XKB_KEY_Sinh_ai2 :: KeySymbol
pattern XKB_KEY_Sinh_ai2 = MkKeySymbol #{const XKB_KEY_Sinh_ai2} 
-- #define XKB_KEY_Sinh_o2            0x1000ddc  /* U+0DDC SINHALA KOMBUVA HAA AELA-PILLA*/
pattern XKB_KEY_Sinh_o2 :: KeySymbol
pattern XKB_KEY_Sinh_o2 = MkKeySymbol #{const XKB_KEY_Sinh_o2} 
-- #define XKB_KEY_Sinh_oo2           0x1000ddd  /* U+0DDD SINHALA KOMBUVA HAA DIGA AELA-PILLA*/
pattern XKB_KEY_Sinh_oo2 :: KeySymbol
pattern XKB_KEY_Sinh_oo2 = MkKeySymbol #{const XKB_KEY_Sinh_oo2} 
-- #define XKB_KEY_Sinh_au2           0x1000dde  /* U+0DDE SINHALA KOMBUVA HAA GAYANUKITTA */
pattern XKB_KEY_Sinh_au2 :: KeySymbol
pattern XKB_KEY_Sinh_au2 = MkKeySymbol #{const XKB_KEY_Sinh_au2} 
-- #define XKB_KEY_Sinh_lu2           0x1000ddf  /* U+0DDF SINHALA GAYANUKITTA */
pattern XKB_KEY_Sinh_lu2 :: KeySymbol
pattern XKB_KEY_Sinh_lu2 = MkKeySymbol #{const XKB_KEY_Sinh_lu2} 
-- #define XKB_KEY_Sinh_ruu2          0x1000df2  /* U+0DF2 SINHALA DIGA GAETTA-PILLA */
pattern XKB_KEY_Sinh_ruu2 :: KeySymbol
pattern XKB_KEY_Sinh_ruu2 = MkKeySymbol #{const XKB_KEY_Sinh_ruu2} 
-- #define XKB_KEY_Sinh_luu2          0x1000df3  /* U+0DF3 SINHALA DIGA GAYANUKITTA */
pattern XKB_KEY_Sinh_luu2 :: KeySymbol
pattern XKB_KEY_Sinh_luu2 = MkKeySymbol #{const XKB_KEY_Sinh_luu2} 
-- #define XKB_KEY_Sinh_kunddaliya    0x1000df4  /* U+0DF4 SINHALA KUNDDALIYA */
pattern XKB_KEY_Sinh_kunddaliya :: KeySymbol
pattern XKB_KEY_Sinh_kunddaliya = MkKeySymbol #{const XKB_KEY_Sinh_kunddaliya} 
-- #define XKB_KEY_XF86ModeLock        0x1008FF01  /* Mode Switch Lock */
pattern XKB_KEY_XF86ModeLock :: KeySymbol
pattern XKB_KEY_XF86ModeLock = MkKeySymbol #{const XKB_KEY_XF86ModeLock} 
-- #define XKB_KEY_XF86MonBrightnessUp   0x1008FF02  /* Monitor/panel brightness */
pattern XKB_KEY_XF86MonBrightnessUp :: KeySymbol
pattern XKB_KEY_XF86MonBrightnessUp = MkKeySymbol #{const XKB_KEY_XF86MonBrightnessUp} 
-- #define XKB_KEY_XF86MonBrightnessDown 0x1008FF03  /* Monitor/panel brightness */
pattern XKB_KEY_XF86MonBrightnessDown :: KeySymbol
pattern XKB_KEY_XF86MonBrightnessDown = MkKeySymbol #{const XKB_KEY_XF86MonBrightnessDown} 
-- #define XKB_KEY_XF86KbdLightOnOff     0x1008FF04  /* Keyboards may be lit     */
pattern XKB_KEY_XF86KbdLightOnOff :: KeySymbol
pattern XKB_KEY_XF86KbdLightOnOff = MkKeySymbol #{const XKB_KEY_XF86KbdLightOnOff} 
-- #define XKB_KEY_XF86KbdBrightnessUp   0x1008FF05  /* Keyboards may be lit     */
pattern XKB_KEY_XF86KbdBrightnessUp :: KeySymbol
pattern XKB_KEY_XF86KbdBrightnessUp = MkKeySymbol #{const XKB_KEY_XF86KbdBrightnessUp} 
-- #define XKB_KEY_XF86KbdBrightnessDown 0x1008FF06  /* Keyboards may be lit     */
pattern XKB_KEY_XF86KbdBrightnessDown :: KeySymbol
pattern XKB_KEY_XF86KbdBrightnessDown = MkKeySymbol #{const XKB_KEY_XF86KbdBrightnessDown} 
-- #define XKB_KEY_XF86Standby     0x1008FF10   /* System into standby mode   */
pattern XKB_KEY_XF86Standby :: KeySymbol
pattern XKB_KEY_XF86Standby = MkKeySymbol #{const XKB_KEY_XF86Standby} 
-- #define XKB_KEY_XF86AudioLowerVolume    0x1008FF11   /* Volume control down        */
pattern XKB_KEY_XF86AudioLowerVolume :: KeySymbol
pattern XKB_KEY_XF86AudioLowerVolume = MkKeySymbol #{const XKB_KEY_XF86AudioLowerVolume} 
-- #define XKB_KEY_XF86AudioMute   0x1008FF12   /* Mute sound from the system */
pattern XKB_KEY_XF86AudioMute :: KeySymbol
pattern XKB_KEY_XF86AudioMute = MkKeySymbol #{const XKB_KEY_XF86AudioMute} 
-- #define XKB_KEY_XF86AudioRaiseVolume    0x1008FF13   /* Volume control up          */
pattern XKB_KEY_XF86AudioRaiseVolume :: KeySymbol
pattern XKB_KEY_XF86AudioRaiseVolume = MkKeySymbol #{const XKB_KEY_XF86AudioRaiseVolume} 
-- #define XKB_KEY_XF86AudioPlay   0x1008FF14   /* Start playing of audio >   */
pattern XKB_KEY_XF86AudioPlay :: KeySymbol
pattern XKB_KEY_XF86AudioPlay = MkKeySymbol #{const XKB_KEY_XF86AudioPlay} 
-- #define XKB_KEY_XF86AudioStop   0x1008FF15   /* Stop playing audio         */
pattern XKB_KEY_XF86AudioStop :: KeySymbol
pattern XKB_KEY_XF86AudioStop = MkKeySymbol #{const XKB_KEY_XF86AudioStop} 
-- #define XKB_KEY_XF86AudioPrev   0x1008FF16   /* Previous track             */
pattern XKB_KEY_XF86AudioPrev :: KeySymbol
pattern XKB_KEY_XF86AudioPrev = MkKeySymbol #{const XKB_KEY_XF86AudioPrev} 
-- #define XKB_KEY_XF86AudioNext   0x1008FF17   /* Next track                 */
pattern XKB_KEY_XF86AudioNext :: KeySymbol
pattern XKB_KEY_XF86AudioNext = MkKeySymbol #{const XKB_KEY_XF86AudioNext} 
-- #define XKB_KEY_XF86HomePage        0x1008FF18   /* Display user's home page   */
pattern XKB_KEY_XF86HomePage :: KeySymbol
pattern XKB_KEY_XF86HomePage = MkKeySymbol #{const XKB_KEY_XF86HomePage} 
-- #define XKB_KEY_XF86Mail        0x1008FF19   /* Invoke user's mail program */
pattern XKB_KEY_XF86Mail :: KeySymbol
pattern XKB_KEY_XF86Mail = MkKeySymbol #{const XKB_KEY_XF86Mail} 
-- #define XKB_KEY_XF86Start       0x1008FF1A   /* Start application          */
pattern XKB_KEY_XF86Start :: KeySymbol
pattern XKB_KEY_XF86Start = MkKeySymbol #{const XKB_KEY_XF86Start} 
-- #define XKB_KEY_XF86Search      0x1008FF1B   /* Search                     */
pattern XKB_KEY_XF86Search :: KeySymbol
pattern XKB_KEY_XF86Search = MkKeySymbol #{const XKB_KEY_XF86Search} 
-- #define XKB_KEY_XF86AudioRecord 0x1008FF1C   /* Record audio application   */
pattern XKB_KEY_XF86AudioRecord :: KeySymbol
pattern XKB_KEY_XF86AudioRecord = MkKeySymbol #{const XKB_KEY_XF86AudioRecord} 
-- #define XKB_KEY_XF86Calculator  0x1008FF1D   /* Invoke calculator program  */
pattern XKB_KEY_XF86Calculator :: KeySymbol
pattern XKB_KEY_XF86Calculator = MkKeySymbol #{const XKB_KEY_XF86Calculator} 
-- #define XKB_KEY_XF86Memo        0x1008FF1E   /* Invoke Memo taking program */
pattern XKB_KEY_XF86Memo :: KeySymbol
pattern XKB_KEY_XF86Memo = MkKeySymbol #{const XKB_KEY_XF86Memo} 
-- #define XKB_KEY_XF86ToDoList        0x1008FF1F   /* Invoke To Do List program  */
pattern XKB_KEY_XF86ToDoList :: KeySymbol
pattern XKB_KEY_XF86ToDoList = MkKeySymbol #{const XKB_KEY_XF86ToDoList} 
-- #define XKB_KEY_XF86Calendar        0x1008FF20   /* Invoke Calendar program    */
pattern XKB_KEY_XF86Calendar :: KeySymbol
pattern XKB_KEY_XF86Calendar = MkKeySymbol #{const XKB_KEY_XF86Calendar} 
-- #define XKB_KEY_XF86PowerDown   0x1008FF21   /* Deep sleep the system      */
pattern XKB_KEY_XF86PowerDown :: KeySymbol
pattern XKB_KEY_XF86PowerDown = MkKeySymbol #{const XKB_KEY_XF86PowerDown} 
-- #define XKB_KEY_XF86ContrastAdjust  0x1008FF22   /* Adjust screen contrast     */
pattern XKB_KEY_XF86ContrastAdjust :: KeySymbol
pattern XKB_KEY_XF86ContrastAdjust = MkKeySymbol #{const XKB_KEY_XF86ContrastAdjust} 
-- #define XKB_KEY_XF86RockerUp        0x1008FF23   /* Rocker switches exist up   */
pattern XKB_KEY_XF86RockerUp :: KeySymbol
pattern XKB_KEY_XF86RockerUp = MkKeySymbol #{const XKB_KEY_XF86RockerUp} 
-- #define XKB_KEY_XF86RockerDown  0x1008FF24   /* and down                   */
pattern XKB_KEY_XF86RockerDown :: KeySymbol
pattern XKB_KEY_XF86RockerDown = MkKeySymbol #{const XKB_KEY_XF86RockerDown} 
-- #define XKB_KEY_XF86RockerEnter 0x1008FF25   /* and let you press them     */
pattern XKB_KEY_XF86RockerEnter :: KeySymbol
pattern XKB_KEY_XF86RockerEnter = MkKeySymbol #{const XKB_KEY_XF86RockerEnter} 
-- #define XKB_KEY_XF86Back        0x1008FF26   /* Like back on a browser     */
pattern XKB_KEY_XF86Back :: KeySymbol
pattern XKB_KEY_XF86Back = MkKeySymbol #{const XKB_KEY_XF86Back} 
-- #define XKB_KEY_XF86Forward     0x1008FF27   /* Like forward on a browser  */
pattern XKB_KEY_XF86Forward :: KeySymbol
pattern XKB_KEY_XF86Forward = MkKeySymbol #{const XKB_KEY_XF86Forward} 
-- #define XKB_KEY_XF86Stop        0x1008FF28   /* Stop current operation     */
pattern XKB_KEY_XF86Stop :: KeySymbol
pattern XKB_KEY_XF86Stop = MkKeySymbol #{const XKB_KEY_XF86Stop} 
-- #define XKB_KEY_XF86Refresh     0x1008FF29   /* Refresh the page           */
pattern XKB_KEY_XF86Refresh :: KeySymbol
pattern XKB_KEY_XF86Refresh = MkKeySymbol #{const XKB_KEY_XF86Refresh} 
-- #define XKB_KEY_XF86PowerOff        0x1008FF2A   /* Power off system entirely  */
pattern XKB_KEY_XF86PowerOff :: KeySymbol
pattern XKB_KEY_XF86PowerOff = MkKeySymbol #{const XKB_KEY_XF86PowerOff} 
-- #define XKB_KEY_XF86WakeUp      0x1008FF2B   /* Wake up system from sleep  */
pattern XKB_KEY_XF86WakeUp :: KeySymbol
pattern XKB_KEY_XF86WakeUp = MkKeySymbol #{const XKB_KEY_XF86WakeUp} 
-- #define XKB_KEY_XF86Eject            0x1008FF2C   /* Eject device (e.g. DVD)    */
pattern XKB_KEY_XF86Eject :: KeySymbol
pattern XKB_KEY_XF86Eject = MkKeySymbol #{const XKB_KEY_XF86Eject} 
-- #define XKB_KEY_XF86ScreenSaver      0x1008FF2D   /* Invoke screensaver         */
pattern XKB_KEY_XF86ScreenSaver :: KeySymbol
pattern XKB_KEY_XF86ScreenSaver = MkKeySymbol #{const XKB_KEY_XF86ScreenSaver} 
-- #define XKB_KEY_XF86WWW              0x1008FF2E   /* Invoke web browser         */
pattern XKB_KEY_XF86WWW :: KeySymbol
pattern XKB_KEY_XF86WWW = MkKeySymbol #{const XKB_KEY_XF86WWW} 
-- #define XKB_KEY_XF86Sleep            0x1008FF2F   /* Put system to sleep        */
pattern XKB_KEY_XF86Sleep :: KeySymbol
pattern XKB_KEY_XF86Sleep = MkKeySymbol #{const XKB_KEY_XF86Sleep} 
-- #define XKB_KEY_XF86Favorites   0x1008FF30   /* Show favorite locations    */
pattern XKB_KEY_XF86Favorites :: KeySymbol
pattern XKB_KEY_XF86Favorites = MkKeySymbol #{const XKB_KEY_XF86Favorites} 
-- #define XKB_KEY_XF86AudioPause  0x1008FF31   /* Pause audio playing        */
pattern XKB_KEY_XF86AudioPause :: KeySymbol
pattern XKB_KEY_XF86AudioPause = MkKeySymbol #{const XKB_KEY_XF86AudioPause} 
-- #define XKB_KEY_XF86AudioMedia  0x1008FF32   /* Launch media collection app */
pattern XKB_KEY_XF86AudioMedia :: KeySymbol
pattern XKB_KEY_XF86AudioMedia = MkKeySymbol #{const XKB_KEY_XF86AudioMedia} 
-- #define XKB_KEY_XF86MyComputer  0x1008FF33   /* Display "My Computer" window */
pattern XKB_KEY_XF86MyComputer :: KeySymbol
pattern XKB_KEY_XF86MyComputer = MkKeySymbol #{const XKB_KEY_XF86MyComputer} 
-- #define XKB_KEY_XF86VendorHome  0x1008FF34   /* Display vendor home web site */
pattern XKB_KEY_XF86VendorHome :: KeySymbol
pattern XKB_KEY_XF86VendorHome = MkKeySymbol #{const XKB_KEY_XF86VendorHome} 
-- #define XKB_KEY_XF86LightBulb   0x1008FF35   /* Light bulb keys exist       */
pattern XKB_KEY_XF86LightBulb :: KeySymbol
pattern XKB_KEY_XF86LightBulb = MkKeySymbol #{const XKB_KEY_XF86LightBulb} 
-- #define XKB_KEY_XF86Shop        0x1008FF36   /* Display shopping web site   */
pattern XKB_KEY_XF86Shop :: KeySymbol
pattern XKB_KEY_XF86Shop = MkKeySymbol #{const XKB_KEY_XF86Shop} 
-- #define XKB_KEY_XF86History     0x1008FF37   /* Show history of web surfing */
pattern XKB_KEY_XF86History :: KeySymbol
pattern XKB_KEY_XF86History = MkKeySymbol #{const XKB_KEY_XF86History} 
-- #define XKB_KEY_XF86OpenURL     0x1008FF38   /* Open selected URL           */
pattern XKB_KEY_XF86OpenURL :: KeySymbol
pattern XKB_KEY_XF86OpenURL = MkKeySymbol #{const XKB_KEY_XF86OpenURL} 
-- #define XKB_KEY_XF86AddFavorite 0x1008FF39   /* Add URL to favorites list   */
pattern XKB_KEY_XF86AddFavorite :: KeySymbol
pattern XKB_KEY_XF86AddFavorite = MkKeySymbol #{const XKB_KEY_XF86AddFavorite} 
-- #define XKB_KEY_XF86HotLinks        0x1008FF3A   /* Show "hot" links            */
pattern XKB_KEY_XF86HotLinks :: KeySymbol
pattern XKB_KEY_XF86HotLinks = MkKeySymbol #{const XKB_KEY_XF86HotLinks} 
-- #define XKB_KEY_XF86BrightnessAdjust    0x1008FF3B   /* Invoke brightness adj. UI   */
pattern XKB_KEY_XF86BrightnessAdjust :: KeySymbol
pattern XKB_KEY_XF86BrightnessAdjust = MkKeySymbol #{const XKB_KEY_XF86BrightnessAdjust} 
-- #define XKB_KEY_XF86Finance     0x1008FF3C   /* Display financial site      */
pattern XKB_KEY_XF86Finance :: KeySymbol
pattern XKB_KEY_XF86Finance = MkKeySymbol #{const XKB_KEY_XF86Finance} 
-- #define XKB_KEY_XF86Community   0x1008FF3D   /* Display user's community    */
pattern XKB_KEY_XF86Community :: KeySymbol
pattern XKB_KEY_XF86Community = MkKeySymbol #{const XKB_KEY_XF86Community} 
-- #define XKB_KEY_XF86AudioRewind 0x1008FF3E   /* "rewind" audio track        */
pattern XKB_KEY_XF86AudioRewind :: KeySymbol
pattern XKB_KEY_XF86AudioRewind = MkKeySymbol #{const XKB_KEY_XF86AudioRewind} 
-- #define XKB_KEY_XF86BackForward 0x1008FF3F   /* ??? */
pattern XKB_KEY_XF86BackForward :: KeySymbol
pattern XKB_KEY_XF86BackForward = MkKeySymbol #{const XKB_KEY_XF86BackForward} 
-- #define XKB_KEY_XF86Launch0     0x1008FF40   /* Launch Application          */
pattern XKB_KEY_XF86Launch0 :: KeySymbol
pattern XKB_KEY_XF86Launch0 = MkKeySymbol #{const XKB_KEY_XF86Launch0} 
-- #define XKB_KEY_XF86Launch1     0x1008FF41   /* Launch Application          */
pattern XKB_KEY_XF86Launch1 :: KeySymbol
pattern XKB_KEY_XF86Launch1 = MkKeySymbol #{const XKB_KEY_XF86Launch1} 
-- #define XKB_KEY_XF86Launch2     0x1008FF42   /* Launch Application          */
pattern XKB_KEY_XF86Launch2 :: KeySymbol
pattern XKB_KEY_XF86Launch2 = MkKeySymbol #{const XKB_KEY_XF86Launch2} 
-- #define XKB_KEY_XF86Launch3     0x1008FF43   /* Launch Application          */
pattern XKB_KEY_XF86Launch3 :: KeySymbol
pattern XKB_KEY_XF86Launch3 = MkKeySymbol #{const XKB_KEY_XF86Launch3} 
-- #define XKB_KEY_XF86Launch4     0x1008FF44   /* Launch Application          */
pattern XKB_KEY_XF86Launch4 :: KeySymbol
pattern XKB_KEY_XF86Launch4 = MkKeySymbol #{const XKB_KEY_XF86Launch4} 
-- #define XKB_KEY_XF86Launch5     0x1008FF45   /* Launch Application          */
pattern XKB_KEY_XF86Launch5 :: KeySymbol
pattern XKB_KEY_XF86Launch5 = MkKeySymbol #{const XKB_KEY_XF86Launch5} 
-- #define XKB_KEY_XF86Launch6     0x1008FF46   /* Launch Application          */
pattern XKB_KEY_XF86Launch6 :: KeySymbol
pattern XKB_KEY_XF86Launch6 = MkKeySymbol #{const XKB_KEY_XF86Launch6} 
-- #define XKB_KEY_XF86Launch7     0x1008FF47   /* Launch Application          */
pattern XKB_KEY_XF86Launch7 :: KeySymbol
pattern XKB_KEY_XF86Launch7 = MkKeySymbol #{const XKB_KEY_XF86Launch7} 
-- #define XKB_KEY_XF86Launch8     0x1008FF48   /* Launch Application          */
pattern XKB_KEY_XF86Launch8 :: KeySymbol
pattern XKB_KEY_XF86Launch8 = MkKeySymbol #{const XKB_KEY_XF86Launch8} 
-- #define XKB_KEY_XF86Launch9     0x1008FF49   /* Launch Application          */
pattern XKB_KEY_XF86Launch9 :: KeySymbol
pattern XKB_KEY_XF86Launch9 = MkKeySymbol #{const XKB_KEY_XF86Launch9} 
-- #define XKB_KEY_XF86LaunchA     0x1008FF4A   /* Launch Application          */
pattern XKB_KEY_XF86LaunchA :: KeySymbol
pattern XKB_KEY_XF86LaunchA = MkKeySymbol #{const XKB_KEY_XF86LaunchA} 
-- #define XKB_KEY_XF86LaunchB     0x1008FF4B   /* Launch Application          */
pattern XKB_KEY_XF86LaunchB :: KeySymbol
pattern XKB_KEY_XF86LaunchB = MkKeySymbol #{const XKB_KEY_XF86LaunchB} 
-- #define XKB_KEY_XF86LaunchC     0x1008FF4C   /* Launch Application          */
pattern XKB_KEY_XF86LaunchC :: KeySymbol
pattern XKB_KEY_XF86LaunchC = MkKeySymbol #{const XKB_KEY_XF86LaunchC} 
-- #define XKB_KEY_XF86LaunchD     0x1008FF4D   /* Launch Application          */
pattern XKB_KEY_XF86LaunchD :: KeySymbol
pattern XKB_KEY_XF86LaunchD = MkKeySymbol #{const XKB_KEY_XF86LaunchD} 
-- #define XKB_KEY_XF86LaunchE     0x1008FF4E   /* Launch Application          */
pattern XKB_KEY_XF86LaunchE :: KeySymbol
pattern XKB_KEY_XF86LaunchE = MkKeySymbol #{const XKB_KEY_XF86LaunchE} 
-- #define XKB_KEY_XF86LaunchF     0x1008FF4F   /* Launch Application          */
pattern XKB_KEY_XF86LaunchF :: KeySymbol
pattern XKB_KEY_XF86LaunchF = MkKeySymbol #{const XKB_KEY_XF86LaunchF} 
-- #define XKB_KEY_XF86ApplicationLeft 0x1008FF50   /* switch to application, left */
pattern XKB_KEY_XF86ApplicationLeft :: KeySymbol
pattern XKB_KEY_XF86ApplicationLeft = MkKeySymbol #{const XKB_KEY_XF86ApplicationLeft} 
-- #define XKB_KEY_XF86ApplicationRight    0x1008FF51   /* switch to application, right*/
pattern XKB_KEY_XF86ApplicationRight :: KeySymbol
pattern XKB_KEY_XF86ApplicationRight = MkKeySymbol #{const XKB_KEY_XF86ApplicationRight} 
-- #define XKB_KEY_XF86Book        0x1008FF52   /* Launch bookreader           */
pattern XKB_KEY_XF86Book :: KeySymbol
pattern XKB_KEY_XF86Book = MkKeySymbol #{const XKB_KEY_XF86Book} 
-- #define XKB_KEY_XF86CD      0x1008FF53   /* Launch CD/DVD player        */
pattern XKB_KEY_XF86CD :: KeySymbol
pattern XKB_KEY_XF86CD = MkKeySymbol #{const XKB_KEY_XF86CD} 
-- #define XKB_KEY_XF86Calculater  0x1008FF54   /* Launch Calculater           */
pattern XKB_KEY_XF86Calculater :: KeySymbol
pattern XKB_KEY_XF86Calculater = MkKeySymbol #{const XKB_KEY_XF86Calculater} 
-- #define XKB_KEY_XF86Clear       0x1008FF55   /* Clear window, screen        */
pattern XKB_KEY_XF86Clear :: KeySymbol
pattern XKB_KEY_XF86Clear = MkKeySymbol #{const XKB_KEY_XF86Clear} 
-- #define XKB_KEY_XF86Close       0x1008FF56   /* Close window                */
pattern XKB_KEY_XF86Close :: KeySymbol
pattern XKB_KEY_XF86Close = MkKeySymbol #{const XKB_KEY_XF86Close} 
-- #define XKB_KEY_XF86Copy        0x1008FF57   /* Copy selection              */
pattern XKB_KEY_XF86Copy :: KeySymbol
pattern XKB_KEY_XF86Copy = MkKeySymbol #{const XKB_KEY_XF86Copy} 
-- #define XKB_KEY_XF86Cut     0x1008FF58   /* Cut selection               */
pattern XKB_KEY_XF86Cut :: KeySymbol
pattern XKB_KEY_XF86Cut = MkKeySymbol #{const XKB_KEY_XF86Cut} 
-- #define XKB_KEY_XF86Display     0x1008FF59   /* Output switch key           */
pattern XKB_KEY_XF86Display :: KeySymbol
pattern XKB_KEY_XF86Display = MkKeySymbol #{const XKB_KEY_XF86Display} 
-- #define XKB_KEY_XF86DOS     0x1008FF5A   /* Launch DOS (emulation)      */
pattern XKB_KEY_XF86DOS :: KeySymbol
pattern XKB_KEY_XF86DOS = MkKeySymbol #{const XKB_KEY_XF86DOS} 
-- #define XKB_KEY_XF86Documents   0x1008FF5B   /* Open documents window       */
pattern XKB_KEY_XF86Documents :: KeySymbol
pattern XKB_KEY_XF86Documents = MkKeySymbol #{const XKB_KEY_XF86Documents} 
-- #define XKB_KEY_XF86Excel       0x1008FF5C   /* Launch spread sheet         */
pattern XKB_KEY_XF86Excel :: KeySymbol
pattern XKB_KEY_XF86Excel = MkKeySymbol #{const XKB_KEY_XF86Excel} 
-- #define XKB_KEY_XF86Explorer        0x1008FF5D   /* Launch file explorer        */
pattern XKB_KEY_XF86Explorer :: KeySymbol
pattern XKB_KEY_XF86Explorer = MkKeySymbol #{const XKB_KEY_XF86Explorer} 
-- #define XKB_KEY_XF86Game        0x1008FF5E   /* Launch game                 */
pattern XKB_KEY_XF86Game :: KeySymbol
pattern XKB_KEY_XF86Game = MkKeySymbol #{const XKB_KEY_XF86Game} 
-- #define XKB_KEY_XF86Go      0x1008FF5F   /* Go to URL                   */
pattern XKB_KEY_XF86Go :: KeySymbol
pattern XKB_KEY_XF86Go = MkKeySymbol #{const XKB_KEY_XF86Go} 
-- #define XKB_KEY_XF86iTouch      0x1008FF60   /* Logitch iTouch- don't use   */
pattern XKB_KEY_XF86iTouch :: KeySymbol
pattern XKB_KEY_XF86iTouch = MkKeySymbol #{const XKB_KEY_XF86iTouch} 
-- #define XKB_KEY_XF86LogOff      0x1008FF61   /* Log off system              */
pattern XKB_KEY_XF86LogOff :: KeySymbol
pattern XKB_KEY_XF86LogOff = MkKeySymbol #{const XKB_KEY_XF86LogOff} 
-- #define XKB_KEY_XF86Market      0x1008FF62   /* ??                          */
pattern XKB_KEY_XF86Market :: KeySymbol
pattern XKB_KEY_XF86Market = MkKeySymbol #{const XKB_KEY_XF86Market} 
-- #define XKB_KEY_XF86Meeting     0x1008FF63   /* enter meeting in calendar   */
pattern XKB_KEY_XF86Meeting :: KeySymbol
pattern XKB_KEY_XF86Meeting = MkKeySymbol #{const XKB_KEY_XF86Meeting} 
-- #define XKB_KEY_XF86MenuKB      0x1008FF65   /* distingush keyboard from PB */
pattern XKB_KEY_XF86MenuKB :: KeySymbol
pattern XKB_KEY_XF86MenuKB = MkKeySymbol #{const XKB_KEY_XF86MenuKB} 
-- #define XKB_KEY_XF86MenuPB      0x1008FF66   /* distinuish PB from keyboard */
pattern XKB_KEY_XF86MenuPB :: KeySymbol
pattern XKB_KEY_XF86MenuPB = MkKeySymbol #{const XKB_KEY_XF86MenuPB} 
-- #define XKB_KEY_XF86MySites     0x1008FF67   /* Favourites                  */
pattern XKB_KEY_XF86MySites :: KeySymbol
pattern XKB_KEY_XF86MySites = MkKeySymbol #{const XKB_KEY_XF86MySites} 
-- #define XKB_KEY_XF86New     0x1008FF68   /* New (folder, document...    */
pattern XKB_KEY_XF86New :: KeySymbol
pattern XKB_KEY_XF86New = MkKeySymbol #{const XKB_KEY_XF86New} 
-- #define XKB_KEY_XF86News        0x1008FF69   /* News                        */
pattern XKB_KEY_XF86News :: KeySymbol
pattern XKB_KEY_XF86News = MkKeySymbol #{const XKB_KEY_XF86News} 
-- #define XKB_KEY_XF86OfficeHome  0x1008FF6A   /* Office home (old Staroffice)*/
pattern XKB_KEY_XF86OfficeHome :: KeySymbol
pattern XKB_KEY_XF86OfficeHome = MkKeySymbol #{const XKB_KEY_XF86OfficeHome} 
-- #define XKB_KEY_XF86Open        0x1008FF6B   /* Open                        */
pattern XKB_KEY_XF86Open :: KeySymbol
pattern XKB_KEY_XF86Open = MkKeySymbol #{const XKB_KEY_XF86Open} 
-- #define XKB_KEY_XF86Option      0x1008FF6C   /* ?? */
pattern XKB_KEY_XF86Option :: KeySymbol
pattern XKB_KEY_XF86Option = MkKeySymbol #{const XKB_KEY_XF86Option} 
-- #define XKB_KEY_XF86Paste       0x1008FF6D   /* Paste                       */
pattern XKB_KEY_XF86Paste :: KeySymbol
pattern XKB_KEY_XF86Paste = MkKeySymbol #{const XKB_KEY_XF86Paste} 
-- #define XKB_KEY_XF86Phone       0x1008FF6E   /* Launch phone; dial number   */
pattern XKB_KEY_XF86Phone :: KeySymbol
pattern XKB_KEY_XF86Phone = MkKeySymbol #{const XKB_KEY_XF86Phone} 
-- #define XKB_KEY_XF86Q       0x1008FF70   /* Compaq's Q - don't use      */
pattern XKB_KEY_XF86Q :: KeySymbol
pattern XKB_KEY_XF86Q = MkKeySymbol #{const XKB_KEY_XF86Q} 
-- #define XKB_KEY_XF86Reply       0x1008FF72   /* Reply e.g., mail            */
pattern XKB_KEY_XF86Reply :: KeySymbol
pattern XKB_KEY_XF86Reply = MkKeySymbol #{const XKB_KEY_XF86Reply} 
-- #define XKB_KEY_XF86Reload      0x1008FF73   /* Reload web page, file, etc. */
pattern XKB_KEY_XF86Reload :: KeySymbol
pattern XKB_KEY_XF86Reload = MkKeySymbol #{const XKB_KEY_XF86Reload} 
-- #define XKB_KEY_XF86RotateWindows   0x1008FF74   /* Rotate windows e.g. xrandr  */
pattern XKB_KEY_XF86RotateWindows :: KeySymbol
pattern XKB_KEY_XF86RotateWindows = MkKeySymbol #{const XKB_KEY_XF86RotateWindows} 
-- #define XKB_KEY_XF86RotationPB  0x1008FF75   /* don't use                   */
pattern XKB_KEY_XF86RotationPB :: KeySymbol
pattern XKB_KEY_XF86RotationPB = MkKeySymbol #{const XKB_KEY_XF86RotationPB} 
-- #define XKB_KEY_XF86RotationKB  0x1008FF76   /* don't use                   */
pattern XKB_KEY_XF86RotationKB :: KeySymbol
pattern XKB_KEY_XF86RotationKB = MkKeySymbol #{const XKB_KEY_XF86RotationKB} 
-- #define XKB_KEY_XF86Save        0x1008FF77   /* Save (file, document, state */
pattern XKB_KEY_XF86Save :: KeySymbol
pattern XKB_KEY_XF86Save = MkKeySymbol #{const XKB_KEY_XF86Save} 
-- #define XKB_KEY_XF86ScrollUp        0x1008FF78   /* Scroll window/contents up   */
pattern XKB_KEY_XF86ScrollUp :: KeySymbol
pattern XKB_KEY_XF86ScrollUp = MkKeySymbol #{const XKB_KEY_XF86ScrollUp} 
-- #define XKB_KEY_XF86ScrollDown  0x1008FF79   /* Scrool window/contentd down */
pattern XKB_KEY_XF86ScrollDown :: KeySymbol
pattern XKB_KEY_XF86ScrollDown = MkKeySymbol #{const XKB_KEY_XF86ScrollDown} 
-- #define XKB_KEY_XF86ScrollClick 0x1008FF7A   /* Use XKB mousekeys instead   */
pattern XKB_KEY_XF86ScrollClick :: KeySymbol
pattern XKB_KEY_XF86ScrollClick = MkKeySymbol #{const XKB_KEY_XF86ScrollClick} 
-- #define XKB_KEY_XF86Send        0x1008FF7B   /* Send mail, file, object     */
pattern XKB_KEY_XF86Send :: KeySymbol
pattern XKB_KEY_XF86Send = MkKeySymbol #{const XKB_KEY_XF86Send} 
-- #define XKB_KEY_XF86Spell       0x1008FF7C   /* Spell checker               */
pattern XKB_KEY_XF86Spell :: KeySymbol
pattern XKB_KEY_XF86Spell = MkKeySymbol #{const XKB_KEY_XF86Spell} 
-- #define XKB_KEY_XF86SplitScreen 0x1008FF7D   /* Split window or screen      */
pattern XKB_KEY_XF86SplitScreen :: KeySymbol
pattern XKB_KEY_XF86SplitScreen = MkKeySymbol #{const XKB_KEY_XF86SplitScreen} 
-- #define XKB_KEY_XF86Support     0x1008FF7E   /* Get support (??)            */
pattern XKB_KEY_XF86Support :: KeySymbol
pattern XKB_KEY_XF86Support = MkKeySymbol #{const XKB_KEY_XF86Support} 
-- #define XKB_KEY_XF86TaskPane        0x1008FF7F   /* Show tasks */
pattern XKB_KEY_XF86TaskPane :: KeySymbol
pattern XKB_KEY_XF86TaskPane = MkKeySymbol #{const XKB_KEY_XF86TaskPane} 
-- #define XKB_KEY_XF86Terminal        0x1008FF80   /* Launch terminal emulator    */
pattern XKB_KEY_XF86Terminal :: KeySymbol
pattern XKB_KEY_XF86Terminal = MkKeySymbol #{const XKB_KEY_XF86Terminal} 
-- #define XKB_KEY_XF86Tools       0x1008FF81   /* toolbox of desktop/app.     */
pattern XKB_KEY_XF86Tools :: KeySymbol
pattern XKB_KEY_XF86Tools = MkKeySymbol #{const XKB_KEY_XF86Tools} 
-- #define XKB_KEY_XF86Travel      0x1008FF82   /* ?? */
pattern XKB_KEY_XF86Travel :: KeySymbol
pattern XKB_KEY_XF86Travel = MkKeySymbol #{const XKB_KEY_XF86Travel} 
-- #define XKB_KEY_XF86UserPB      0x1008FF84   /* ?? */
pattern XKB_KEY_XF86UserPB :: KeySymbol
pattern XKB_KEY_XF86UserPB = MkKeySymbol #{const XKB_KEY_XF86UserPB} 
-- #define XKB_KEY_XF86User1KB     0x1008FF85   /* ?? */
pattern XKB_KEY_XF86User1KB :: KeySymbol
pattern XKB_KEY_XF86User1KB = MkKeySymbol #{const XKB_KEY_XF86User1KB} 
-- #define XKB_KEY_XF86User2KB     0x1008FF86   /* ?? */
pattern XKB_KEY_XF86User2KB :: KeySymbol
pattern XKB_KEY_XF86User2KB = MkKeySymbol #{const XKB_KEY_XF86User2KB} 
-- #define XKB_KEY_XF86Video       0x1008FF87   /* Launch video player       */
pattern XKB_KEY_XF86Video :: KeySymbol
pattern XKB_KEY_XF86Video = MkKeySymbol #{const XKB_KEY_XF86Video} 
-- #define XKB_KEY_XF86WheelButton 0x1008FF88   /* button from a mouse wheel */
pattern XKB_KEY_XF86WheelButton :: KeySymbol
pattern XKB_KEY_XF86WheelButton = MkKeySymbol #{const XKB_KEY_XF86WheelButton} 
-- #define XKB_KEY_XF86Word        0x1008FF89   /* Launch word processor     */
pattern XKB_KEY_XF86Word :: KeySymbol
pattern XKB_KEY_XF86Word = MkKeySymbol #{const XKB_KEY_XF86Word} 
-- #define XKB_KEY_XF86Xfer        0x1008FF8A
pattern XKB_KEY_XF86Xfer :: KeySymbol
pattern XKB_KEY_XF86Xfer = MkKeySymbol #{const XKB_KEY_XF86Xfer} 
-- #define XKB_KEY_XF86ZoomIn      0x1008FF8B   /* zoom in view, map, etc.   */
pattern XKB_KEY_XF86ZoomIn :: KeySymbol
pattern XKB_KEY_XF86ZoomIn = MkKeySymbol #{const XKB_KEY_XF86ZoomIn} 
-- #define XKB_KEY_XF86ZoomOut     0x1008FF8C   /* zoom out view, map, etc.  */
pattern XKB_KEY_XF86ZoomOut :: KeySymbol
pattern XKB_KEY_XF86ZoomOut = MkKeySymbol #{const XKB_KEY_XF86ZoomOut} 
-- #define XKB_KEY_XF86Away        0x1008FF8D   /* mark yourself as away     */
pattern XKB_KEY_XF86Away :: KeySymbol
pattern XKB_KEY_XF86Away = MkKeySymbol #{const XKB_KEY_XF86Away} 
-- #define XKB_KEY_XF86Messenger   0x1008FF8E   /* as in instant messaging   */
pattern XKB_KEY_XF86Messenger :: KeySymbol
pattern XKB_KEY_XF86Messenger = MkKeySymbol #{const XKB_KEY_XF86Messenger} 
-- #define XKB_KEY_XF86WebCam      0x1008FF8F   /* Launch web camera app.    */
pattern XKB_KEY_XF86WebCam :: KeySymbol
pattern XKB_KEY_XF86WebCam = MkKeySymbol #{const XKB_KEY_XF86WebCam} 
-- #define XKB_KEY_XF86MailForward 0x1008FF90   /* Forward in mail           */
pattern XKB_KEY_XF86MailForward :: KeySymbol
pattern XKB_KEY_XF86MailForward = MkKeySymbol #{const XKB_KEY_XF86MailForward} 
-- #define XKB_KEY_XF86Pictures        0x1008FF91   /* Show pictures             */
pattern XKB_KEY_XF86Pictures :: KeySymbol
pattern XKB_KEY_XF86Pictures = MkKeySymbol #{const XKB_KEY_XF86Pictures} 
-- #define XKB_KEY_XF86Music       0x1008FF92   /* Launch music application  */
pattern XKB_KEY_XF86Music :: KeySymbol
pattern XKB_KEY_XF86Music = MkKeySymbol #{const XKB_KEY_XF86Music} 
-- #define XKB_KEY_XF86Battery     0x1008FF93   /* Display battery information */
pattern XKB_KEY_XF86Battery :: KeySymbol
pattern XKB_KEY_XF86Battery = MkKeySymbol #{const XKB_KEY_XF86Battery} 
-- #define XKB_KEY_XF86Bluetooth   0x1008FF94   /* Enable/disable Bluetooth    */
pattern XKB_KEY_XF86Bluetooth :: KeySymbol
pattern XKB_KEY_XF86Bluetooth = MkKeySymbol #{const XKB_KEY_XF86Bluetooth} 
-- #define XKB_KEY_XF86WLAN        0x1008FF95   /* Enable/disable WLAN         */
pattern XKB_KEY_XF86WLAN :: KeySymbol
pattern XKB_KEY_XF86WLAN = MkKeySymbol #{const XKB_KEY_XF86WLAN} 
-- #define XKB_KEY_XF86UWB     0x1008FF96   /* Enable/disable UWB      */
pattern XKB_KEY_XF86UWB :: KeySymbol
pattern XKB_KEY_XF86UWB = MkKeySymbol #{const XKB_KEY_XF86UWB} 
-- #define XKB_KEY_XF86AudioForward    0x1008FF97   /* fast-forward audio track    */
pattern XKB_KEY_XF86AudioForward :: KeySymbol
pattern XKB_KEY_XF86AudioForward = MkKeySymbol #{const XKB_KEY_XF86AudioForward} 
-- #define XKB_KEY_XF86AudioRepeat 0x1008FF98   /* toggle repeat mode          */
pattern XKB_KEY_XF86AudioRepeat :: KeySymbol
pattern XKB_KEY_XF86AudioRepeat = MkKeySymbol #{const XKB_KEY_XF86AudioRepeat} 
-- #define XKB_KEY_XF86AudioRandomPlay 0x1008FF99   /* toggle shuffle mode         */
pattern XKB_KEY_XF86AudioRandomPlay :: KeySymbol
pattern XKB_KEY_XF86AudioRandomPlay = MkKeySymbol #{const XKB_KEY_XF86AudioRandomPlay} 
-- #define XKB_KEY_XF86Subtitle        0x1008FF9A   /* cycle through subtitle      */
pattern XKB_KEY_XF86Subtitle :: KeySymbol
pattern XKB_KEY_XF86Subtitle = MkKeySymbol #{const XKB_KEY_XF86Subtitle} 
-- #define XKB_KEY_XF86AudioCycleTrack 0x1008FF9B   /* cycle through audio tracks  */
pattern XKB_KEY_XF86AudioCycleTrack :: KeySymbol
pattern XKB_KEY_XF86AudioCycleTrack = MkKeySymbol #{const XKB_KEY_XF86AudioCycleTrack} 
-- #define XKB_KEY_XF86CycleAngle  0x1008FF9C   /* cycle through angles        */
pattern XKB_KEY_XF86CycleAngle :: KeySymbol
pattern XKB_KEY_XF86CycleAngle = MkKeySymbol #{const XKB_KEY_XF86CycleAngle} 
-- #define XKB_KEY_XF86FrameBack   0x1008FF9D   /* video: go one frame back    */
pattern XKB_KEY_XF86FrameBack :: KeySymbol
pattern XKB_KEY_XF86FrameBack = MkKeySymbol #{const XKB_KEY_XF86FrameBack} 
-- #define XKB_KEY_XF86FrameForward    0x1008FF9E   /* video: go one frame forward */
pattern XKB_KEY_XF86FrameForward :: KeySymbol
pattern XKB_KEY_XF86FrameForward = MkKeySymbol #{const XKB_KEY_XF86FrameForward} 
-- #define XKB_KEY_XF86Time        0x1008FF9F   /* display, or shows an entry for time seeking */
pattern XKB_KEY_XF86Time :: KeySymbol
pattern XKB_KEY_XF86Time = MkKeySymbol #{const XKB_KEY_XF86Time} 
-- #define XKB_KEY_XF86Select      0x1008FFA0   /* Select button on joypads and remotes */
pattern XKB_KEY_XF86Select :: KeySymbol
pattern XKB_KEY_XF86Select = MkKeySymbol #{const XKB_KEY_XF86Select} 
-- #define XKB_KEY_XF86View        0x1008FFA1   /* Show a view options/properties */
pattern XKB_KEY_XF86View :: KeySymbol
pattern XKB_KEY_XF86View = MkKeySymbol #{const XKB_KEY_XF86View} 
-- #define XKB_KEY_XF86TopMenu     0x1008FFA2   /* Go to a top-level menu in a video */
pattern XKB_KEY_XF86TopMenu :: KeySymbol
pattern XKB_KEY_XF86TopMenu = MkKeySymbol #{const XKB_KEY_XF86TopMenu} 
-- #define XKB_KEY_XF86Red     0x1008FFA3   /* Red button                  */
pattern XKB_KEY_XF86Red :: KeySymbol
pattern XKB_KEY_XF86Red = MkKeySymbol #{const XKB_KEY_XF86Red} 
-- #define XKB_KEY_XF86Green       0x1008FFA4   /* Green button                */
pattern XKB_KEY_XF86Green :: KeySymbol
pattern XKB_KEY_XF86Green = MkKeySymbol #{const XKB_KEY_XF86Green} 
-- #define XKB_KEY_XF86Yellow      0x1008FFA5   /* Yellow button               */
pattern XKB_KEY_XF86Yellow :: KeySymbol
pattern XKB_KEY_XF86Yellow = MkKeySymbol #{const XKB_KEY_XF86Yellow} 
-- #define XKB_KEY_XF86Blue             0x1008FFA6   /* Blue button                 */
pattern XKB_KEY_XF86Blue :: KeySymbol
pattern XKB_KEY_XF86Blue = MkKeySymbol #{const XKB_KEY_XF86Blue} 
-- #define XKB_KEY_XF86Suspend     0x1008FFA7   /* Sleep to RAM                */
pattern XKB_KEY_XF86Suspend :: KeySymbol
pattern XKB_KEY_XF86Suspend = MkKeySymbol #{const XKB_KEY_XF86Suspend} 
-- #define XKB_KEY_XF86Hibernate   0x1008FFA8   /* Sleep to disk               */
pattern XKB_KEY_XF86Hibernate :: KeySymbol
pattern XKB_KEY_XF86Hibernate = MkKeySymbol #{const XKB_KEY_XF86Hibernate} 
-- #define XKB_KEY_XF86TouchpadToggle  0x1008FFA9   /* Toggle between touchpad/trackstick */
pattern XKB_KEY_XF86TouchpadToggle :: KeySymbol
pattern XKB_KEY_XF86TouchpadToggle = MkKeySymbol #{const XKB_KEY_XF86TouchpadToggle} 
-- #define XKB_KEY_XF86TouchpadOn  0x1008FFB0   /* The touchpad got switched on */
pattern XKB_KEY_XF86TouchpadOn :: KeySymbol
pattern XKB_KEY_XF86TouchpadOn = MkKeySymbol #{const XKB_KEY_XF86TouchpadOn} 
-- #define XKB_KEY_XF86TouchpadOff 0x1008FFB1   /* The touchpad got switched off */
pattern XKB_KEY_XF86TouchpadOff :: KeySymbol
pattern XKB_KEY_XF86TouchpadOff = MkKeySymbol #{const XKB_KEY_XF86TouchpadOff} 
-- #define XKB_KEY_XF86AudioMicMute    0x1008FFB2   /* Mute the Mic from the system */
pattern XKB_KEY_XF86AudioMicMute :: KeySymbol
pattern XKB_KEY_XF86AudioMicMute = MkKeySymbol #{const XKB_KEY_XF86AudioMicMute} 
-- #define XKB_KEY_XF86Switch_VT_1 0x1008FE01
pattern XKB_KEY_XF86Switch_VT_1 :: KeySymbol
pattern XKB_KEY_XF86Switch_VT_1 = MkKeySymbol #{const XKB_KEY_XF86Switch_VT_1} 
-- #define XKB_KEY_XF86Switch_VT_2 0x1008FE02
pattern XKB_KEY_XF86Switch_VT_2 :: KeySymbol
pattern XKB_KEY_XF86Switch_VT_2 = MkKeySymbol #{const XKB_KEY_XF86Switch_VT_2} 
-- #define XKB_KEY_XF86Switch_VT_3 0x1008FE03
pattern XKB_KEY_XF86Switch_VT_3 :: KeySymbol
pattern XKB_KEY_XF86Switch_VT_3 = MkKeySymbol #{const XKB_KEY_XF86Switch_VT_3} 
-- #define XKB_KEY_XF86Switch_VT_4 0x1008FE04
pattern XKB_KEY_XF86Switch_VT_4 :: KeySymbol
pattern XKB_KEY_XF86Switch_VT_4 = MkKeySymbol #{const XKB_KEY_XF86Switch_VT_4} 
-- #define XKB_KEY_XF86Switch_VT_5 0x1008FE05
pattern XKB_KEY_XF86Switch_VT_5 :: KeySymbol
pattern XKB_KEY_XF86Switch_VT_5 = MkKeySymbol #{const XKB_KEY_XF86Switch_VT_5} 
-- #define XKB_KEY_XF86Switch_VT_6 0x1008FE06
pattern XKB_KEY_XF86Switch_VT_6 :: KeySymbol
pattern XKB_KEY_XF86Switch_VT_6 = MkKeySymbol #{const XKB_KEY_XF86Switch_VT_6} 
-- #define XKB_KEY_XF86Switch_VT_7 0x1008FE07
pattern XKB_KEY_XF86Switch_VT_7 :: KeySymbol
pattern XKB_KEY_XF86Switch_VT_7 = MkKeySymbol #{const XKB_KEY_XF86Switch_VT_7} 
-- #define XKB_KEY_XF86Switch_VT_8 0x1008FE08
pattern XKB_KEY_XF86Switch_VT_8 :: KeySymbol
pattern XKB_KEY_XF86Switch_VT_8 = MkKeySymbol #{const XKB_KEY_XF86Switch_VT_8} 
-- #define XKB_KEY_XF86Switch_VT_9 0x1008FE09
pattern XKB_KEY_XF86Switch_VT_9 :: KeySymbol
pattern XKB_KEY_XF86Switch_VT_9 = MkKeySymbol #{const XKB_KEY_XF86Switch_VT_9} 
-- #define XKB_KEY_XF86Switch_VT_10    0x1008FE0A
pattern XKB_KEY_XF86Switch_VT_10 :: KeySymbol
pattern XKB_KEY_XF86Switch_VT_10 = MkKeySymbol #{const XKB_KEY_XF86Switch_VT_10} 
-- #define XKB_KEY_XF86Switch_VT_11    0x1008FE0B
pattern XKB_KEY_XF86Switch_VT_11 :: KeySymbol
pattern XKB_KEY_XF86Switch_VT_11 = MkKeySymbol #{const XKB_KEY_XF86Switch_VT_11} 
-- #define XKB_KEY_XF86Switch_VT_12    0x1008FE0C
pattern XKB_KEY_XF86Switch_VT_12 :: KeySymbol
pattern XKB_KEY_XF86Switch_VT_12 = MkKeySymbol #{const XKB_KEY_XF86Switch_VT_12} 
-- #define XKB_KEY_XF86Ungrab      0x1008FE20   /* force ungrab               */
pattern XKB_KEY_XF86Ungrab :: KeySymbol
pattern XKB_KEY_XF86Ungrab = MkKeySymbol #{const XKB_KEY_XF86Ungrab} 
-- #define XKB_KEY_XF86ClearGrab   0x1008FE21   /* kill application with grab */
pattern XKB_KEY_XF86ClearGrab :: KeySymbol
pattern XKB_KEY_XF86ClearGrab = MkKeySymbol #{const XKB_KEY_XF86ClearGrab} 
-- #define XKB_KEY_XF86Next_VMode  0x1008FE22   /* next video mode available  */
pattern XKB_KEY_XF86Next_VMode :: KeySymbol
pattern XKB_KEY_XF86Next_VMode = MkKeySymbol #{const XKB_KEY_XF86Next_VMode} 
-- #define XKB_KEY_XF86Prev_VMode  0x1008FE23   /* prev. video mode available */
pattern XKB_KEY_XF86Prev_VMode :: KeySymbol
pattern XKB_KEY_XF86Prev_VMode = MkKeySymbol #{const XKB_KEY_XF86Prev_VMode} 
-- #define XKB_KEY_XF86LogWindowTree   0x1008FE24   /* print window tree to log   */
pattern XKB_KEY_XF86LogWindowTree :: KeySymbol
pattern XKB_KEY_XF86LogWindowTree = MkKeySymbol #{const XKB_KEY_XF86LogWindowTree} 
-- #define XKB_KEY_XF86LogGrabInfo 0x1008FE25   /* print all active grabs to log */
pattern XKB_KEY_XF86LogGrabInfo :: KeySymbol
pattern XKB_KEY_XF86LogGrabInfo = MkKeySymbol #{const XKB_KEY_XF86LogGrabInfo} 
-- #define XKB_KEY_SunFA_Grave     0x1005FF00
pattern XKB_KEY_SunFA_Grave :: KeySymbol
pattern XKB_KEY_SunFA_Grave = MkKeySymbol #{const XKB_KEY_SunFA_Grave} 
-- #define XKB_KEY_SunFA_Circum        0x1005FF01
pattern XKB_KEY_SunFA_Circum :: KeySymbol
pattern XKB_KEY_SunFA_Circum = MkKeySymbol #{const XKB_KEY_SunFA_Circum} 
-- #define XKB_KEY_SunFA_Tilde     0x1005FF02
pattern XKB_KEY_SunFA_Tilde :: KeySymbol
pattern XKB_KEY_SunFA_Tilde = MkKeySymbol #{const XKB_KEY_SunFA_Tilde} 
-- #define XKB_KEY_SunFA_Acute     0x1005FF03
pattern XKB_KEY_SunFA_Acute :: KeySymbol
pattern XKB_KEY_SunFA_Acute = MkKeySymbol #{const XKB_KEY_SunFA_Acute} 
-- #define XKB_KEY_SunFA_Diaeresis 0x1005FF04
pattern XKB_KEY_SunFA_Diaeresis :: KeySymbol
pattern XKB_KEY_SunFA_Diaeresis = MkKeySymbol #{const XKB_KEY_SunFA_Diaeresis} 
-- #define XKB_KEY_SunFA_Cedilla   0x1005FF05
pattern XKB_KEY_SunFA_Cedilla :: KeySymbol
pattern XKB_KEY_SunFA_Cedilla = MkKeySymbol #{const XKB_KEY_SunFA_Cedilla} 
-- #define XKB_KEY_SunF36      0x1005FF10  /* Labeled F11 */
pattern XKB_KEY_SunF36 :: KeySymbol
pattern XKB_KEY_SunF36 = MkKeySymbol #{const XKB_KEY_SunF36} 
-- #define XKB_KEY_SunF37      0x1005FF11  /* Labeled F12 */
pattern XKB_KEY_SunF37 :: KeySymbol
pattern XKB_KEY_SunF37 = MkKeySymbol #{const XKB_KEY_SunF37} 
-- #define XKB_KEY_SunSys_Req      0x1005FF60
pattern XKB_KEY_SunSys_Req :: KeySymbol
pattern XKB_KEY_SunSys_Req = MkKeySymbol #{const XKB_KEY_SunSys_Req} 
-- #define XKB_KEY_SunPrint_Screen 0x0000FF61  /* Same as XK_Print */
pattern XKB_KEY_SunPrint_Screen :: KeySymbol
pattern XKB_KEY_SunPrint_Screen = MkKeySymbol #{const XKB_KEY_SunPrint_Screen} 
-- #define XKB_KEY_SunCompose      0x0000FF20  /* Same as XK_Multi_key */
pattern XKB_KEY_SunCompose :: KeySymbol
pattern XKB_KEY_SunCompose = MkKeySymbol #{const XKB_KEY_SunCompose} 
-- #define XKB_KEY_SunAltGraph     0x0000FF7E  /* Same as XK_Mode_switch */
pattern XKB_KEY_SunAltGraph :: KeySymbol
pattern XKB_KEY_SunAltGraph = MkKeySymbol #{const XKB_KEY_SunAltGraph} 
-- #define XKB_KEY_SunPageUp       0x0000FF55  /* Same as XK_Prior */
pattern XKB_KEY_SunPageUp :: KeySymbol
pattern XKB_KEY_SunPageUp = MkKeySymbol #{const XKB_KEY_SunPageUp} 
-- #define XKB_KEY_SunPageDown     0x0000FF56  /* Same as XK_Next */
pattern XKB_KEY_SunPageDown :: KeySymbol
pattern XKB_KEY_SunPageDown = MkKeySymbol #{const XKB_KEY_SunPageDown} 
-- #define XKB_KEY_SunUndo     0x0000FF65  /* Same as XK_Undo */
pattern XKB_KEY_SunUndo :: KeySymbol
pattern XKB_KEY_SunUndo = MkKeySymbol #{const XKB_KEY_SunUndo} 
-- #define XKB_KEY_SunAgain        0x0000FF66  /* Same as XK_Redo */
pattern XKB_KEY_SunAgain :: KeySymbol
pattern XKB_KEY_SunAgain = MkKeySymbol #{const XKB_KEY_SunAgain} 
-- #define XKB_KEY_SunFind     0x0000FF68  /* Same as XK_Find */
pattern XKB_KEY_SunFind :: KeySymbol
pattern XKB_KEY_SunFind = MkKeySymbol #{const XKB_KEY_SunFind} 
-- #define XKB_KEY_SunStop     0x0000FF69  /* Same as XK_Cancel */
pattern XKB_KEY_SunStop :: KeySymbol
pattern XKB_KEY_SunStop = MkKeySymbol #{const XKB_KEY_SunStop} 
-- #define XKB_KEY_SunProps        0x1005FF70
pattern XKB_KEY_SunProps :: KeySymbol
pattern XKB_KEY_SunProps = MkKeySymbol #{const XKB_KEY_SunProps} 
-- #define XKB_KEY_SunFront        0x1005FF71
pattern XKB_KEY_SunFront :: KeySymbol
pattern XKB_KEY_SunFront = MkKeySymbol #{const XKB_KEY_SunFront} 
-- #define XKB_KEY_SunCopy     0x1005FF72
pattern XKB_KEY_SunCopy :: KeySymbol
pattern XKB_KEY_SunCopy = MkKeySymbol #{const XKB_KEY_SunCopy} 
-- #define XKB_KEY_SunOpen     0x1005FF73
pattern XKB_KEY_SunOpen :: KeySymbol
pattern XKB_KEY_SunOpen = MkKeySymbol #{const XKB_KEY_SunOpen} 
-- #define XKB_KEY_SunPaste        0x1005FF74
pattern XKB_KEY_SunPaste :: KeySymbol
pattern XKB_KEY_SunPaste = MkKeySymbol #{const XKB_KEY_SunPaste} 
-- #define XKB_KEY_SunCut      0x1005FF75
pattern XKB_KEY_SunCut :: KeySymbol
pattern XKB_KEY_SunCut = MkKeySymbol #{const XKB_KEY_SunCut} 
-- #define XKB_KEY_SunPowerSwitch      0x1005FF76
pattern XKB_KEY_SunPowerSwitch :: KeySymbol
pattern XKB_KEY_SunPowerSwitch = MkKeySymbol #{const XKB_KEY_SunPowerSwitch} 
-- #define XKB_KEY_SunAudioLowerVolume     0x1005FF77
pattern XKB_KEY_SunAudioLowerVolume :: KeySymbol
pattern XKB_KEY_SunAudioLowerVolume = MkKeySymbol #{const XKB_KEY_SunAudioLowerVolume} 
-- #define XKB_KEY_SunAudioMute            0x1005FF78
pattern XKB_KEY_SunAudioMute :: KeySymbol
pattern XKB_KEY_SunAudioMute = MkKeySymbol #{const XKB_KEY_SunAudioMute} 
-- #define XKB_KEY_SunAudioRaiseVolume     0x1005FF79
pattern XKB_KEY_SunAudioRaiseVolume :: KeySymbol
pattern XKB_KEY_SunAudioRaiseVolume = MkKeySymbol #{const XKB_KEY_SunAudioRaiseVolume} 
-- #define XKB_KEY_SunVideoDegauss     0x1005FF7A
pattern XKB_KEY_SunVideoDegauss :: KeySymbol
pattern XKB_KEY_SunVideoDegauss = MkKeySymbol #{const XKB_KEY_SunVideoDegauss} 
-- #define XKB_KEY_SunVideoLowerBrightness 0x1005FF7B
pattern XKB_KEY_SunVideoLowerBrightness :: KeySymbol
pattern XKB_KEY_SunVideoLowerBrightness = MkKeySymbol #{const XKB_KEY_SunVideoLowerBrightness} 
-- #define XKB_KEY_SunVideoRaiseBrightness 0x1005FF7C
pattern XKB_KEY_SunVideoRaiseBrightness :: KeySymbol
pattern XKB_KEY_SunVideoRaiseBrightness = MkKeySymbol #{const XKB_KEY_SunVideoRaiseBrightness} 
-- #define XKB_KEY_SunPowerSwitchShift     0x1005FF7D
pattern XKB_KEY_SunPowerSwitchShift :: KeySymbol
pattern XKB_KEY_SunPowerSwitchShift = MkKeySymbol #{const XKB_KEY_SunPowerSwitchShift} 
-- #define XKB_KEY_Dring_accent         0x1000FEB0
pattern XKB_KEY_Dring_accent :: KeySymbol
pattern XKB_KEY_Dring_accent = MkKeySymbol #{const XKB_KEY_Dring_accent} 
-- #define XKB_KEY_Dcircumflex_accent   0x1000FE5E
pattern XKB_KEY_Dcircumflex_accent :: KeySymbol
pattern XKB_KEY_Dcircumflex_accent = MkKeySymbol #{const XKB_KEY_Dcircumflex_accent} 
-- #define XKB_KEY_Dcedilla_accent      0x1000FE2C
pattern XKB_KEY_Dcedilla_accent :: KeySymbol
pattern XKB_KEY_Dcedilla_accent = MkKeySymbol #{const XKB_KEY_Dcedilla_accent} 
-- #define XKB_KEY_Dacute_accent        0x1000FE27
pattern XKB_KEY_Dacute_accent :: KeySymbol
pattern XKB_KEY_Dacute_accent = MkKeySymbol #{const XKB_KEY_Dacute_accent} 
-- #define XKB_KEY_Dgrave_accent        0x1000FE60
pattern XKB_KEY_Dgrave_accent :: KeySymbol
pattern XKB_KEY_Dgrave_accent = MkKeySymbol #{const XKB_KEY_Dgrave_accent} 
-- #define XKB_KEY_Dtilde               0x1000FE7E
pattern XKB_KEY_Dtilde :: KeySymbol
pattern XKB_KEY_Dtilde = MkKeySymbol #{const XKB_KEY_Dtilde} 
-- #define XKB_KEY_Ddiaeresis           0x1000FE22
pattern XKB_KEY_Ddiaeresis :: KeySymbol
pattern XKB_KEY_Ddiaeresis = MkKeySymbol #{const XKB_KEY_Ddiaeresis} 
-- #define XKB_KEY_DRemove 0x1000FF00   /* Remove */
pattern XKB_KEY_DRemove :: KeySymbol
pattern XKB_KEY_DRemove = MkKeySymbol #{const XKB_KEY_DRemove} 
-- #define XKB_KEY_hpClearLine     0x1000FF6F
pattern XKB_KEY_hpClearLine :: KeySymbol
pattern XKB_KEY_hpClearLine = MkKeySymbol #{const XKB_KEY_hpClearLine} 
-- #define XKB_KEY_hpInsertLine        0x1000FF70
pattern XKB_KEY_hpInsertLine :: KeySymbol
pattern XKB_KEY_hpInsertLine = MkKeySymbol #{const XKB_KEY_hpInsertLine} 
-- #define XKB_KEY_hpDeleteLine        0x1000FF71
pattern XKB_KEY_hpDeleteLine :: KeySymbol
pattern XKB_KEY_hpDeleteLine = MkKeySymbol #{const XKB_KEY_hpDeleteLine} 
-- #define XKB_KEY_hpInsertChar        0x1000FF72
pattern XKB_KEY_hpInsertChar :: KeySymbol
pattern XKB_KEY_hpInsertChar = MkKeySymbol #{const XKB_KEY_hpInsertChar} 
-- #define XKB_KEY_hpDeleteChar        0x1000FF73
pattern XKB_KEY_hpDeleteChar :: KeySymbol
pattern XKB_KEY_hpDeleteChar = MkKeySymbol #{const XKB_KEY_hpDeleteChar} 
-- #define XKB_KEY_hpBackTab       0x1000FF74
pattern XKB_KEY_hpBackTab :: KeySymbol
pattern XKB_KEY_hpBackTab = MkKeySymbol #{const XKB_KEY_hpBackTab} 
-- #define XKB_KEY_hpKP_BackTab        0x1000FF75
pattern XKB_KEY_hpKP_BackTab :: KeySymbol
pattern XKB_KEY_hpKP_BackTab = MkKeySymbol #{const XKB_KEY_hpKP_BackTab} 
-- #define XKB_KEY_hpModelock1     0x1000FF48
pattern XKB_KEY_hpModelock1 :: KeySymbol
pattern XKB_KEY_hpModelock1 = MkKeySymbol #{const XKB_KEY_hpModelock1} 
-- #define XKB_KEY_hpModelock2     0x1000FF49
pattern XKB_KEY_hpModelock2 :: KeySymbol
pattern XKB_KEY_hpModelock2 = MkKeySymbol #{const XKB_KEY_hpModelock2} 
-- #define XKB_KEY_hpReset     0x1000FF6C
pattern XKB_KEY_hpReset :: KeySymbol
pattern XKB_KEY_hpReset = MkKeySymbol #{const XKB_KEY_hpReset} 
-- #define XKB_KEY_hpSystem        0x1000FF6D
pattern XKB_KEY_hpSystem :: KeySymbol
pattern XKB_KEY_hpSystem = MkKeySymbol #{const XKB_KEY_hpSystem} 
-- #define XKB_KEY_hpUser      0x1000FF6E
pattern XKB_KEY_hpUser :: KeySymbol
pattern XKB_KEY_hpUser = MkKeySymbol #{const XKB_KEY_hpUser} 
-- #define XKB_KEY_hpmute_acute        0x100000A8
pattern XKB_KEY_hpmute_acute :: KeySymbol
pattern XKB_KEY_hpmute_acute = MkKeySymbol #{const XKB_KEY_hpmute_acute} 
-- #define XKB_KEY_hpmute_grave        0x100000A9
pattern XKB_KEY_hpmute_grave :: KeySymbol
pattern XKB_KEY_hpmute_grave = MkKeySymbol #{const XKB_KEY_hpmute_grave} 
-- #define XKB_KEY_hpmute_asciicircum  0x100000AA
pattern XKB_KEY_hpmute_asciicircum :: KeySymbol
pattern XKB_KEY_hpmute_asciicircum = MkKeySymbol #{const XKB_KEY_hpmute_asciicircum} 
-- #define XKB_KEY_hpmute_diaeresis    0x100000AB
pattern XKB_KEY_hpmute_diaeresis :: KeySymbol
pattern XKB_KEY_hpmute_diaeresis = MkKeySymbol #{const XKB_KEY_hpmute_diaeresis} 
-- #define XKB_KEY_hpmute_asciitilde   0x100000AC
pattern XKB_KEY_hpmute_asciitilde :: KeySymbol
pattern XKB_KEY_hpmute_asciitilde = MkKeySymbol #{const XKB_KEY_hpmute_asciitilde} 
-- #define XKB_KEY_hplira      0x100000AF
pattern XKB_KEY_hplira :: KeySymbol
pattern XKB_KEY_hplira = MkKeySymbol #{const XKB_KEY_hplira} 
-- #define XKB_KEY_hpguilder       0x100000BE
pattern XKB_KEY_hpguilder :: KeySymbol
pattern XKB_KEY_hpguilder = MkKeySymbol #{const XKB_KEY_hpguilder} 
-- #define XKB_KEY_hpYdiaeresis        0x100000EE
pattern XKB_KEY_hpYdiaeresis :: KeySymbol
pattern XKB_KEY_hpYdiaeresis = MkKeySymbol #{const XKB_KEY_hpYdiaeresis} 
-- #define XKB_KEY_hpIO            0x100000EE
pattern XKB_KEY_hpIO :: KeySymbol
pattern XKB_KEY_hpIO = MkKeySymbol #{const XKB_KEY_hpIO} 
-- #define XKB_KEY_hplongminus     0x100000F6
pattern XKB_KEY_hplongminus :: KeySymbol
pattern XKB_KEY_hplongminus = MkKeySymbol #{const XKB_KEY_hplongminus} 
-- #define XKB_KEY_hpblock     0x100000FC
pattern XKB_KEY_hpblock :: KeySymbol
pattern XKB_KEY_hpblock = MkKeySymbol #{const XKB_KEY_hpblock} 
-- #define XKB_KEY_osfCopy     0x1004FF02
pattern XKB_KEY_osfCopy :: KeySymbol
pattern XKB_KEY_osfCopy = MkKeySymbol #{const XKB_KEY_osfCopy} 
-- #define XKB_KEY_osfCut      0x1004FF03
pattern XKB_KEY_osfCut :: KeySymbol
pattern XKB_KEY_osfCut = MkKeySymbol #{const XKB_KEY_osfCut} 
-- #define XKB_KEY_osfPaste        0x1004FF04
pattern XKB_KEY_osfPaste :: KeySymbol
pattern XKB_KEY_osfPaste = MkKeySymbol #{const XKB_KEY_osfPaste} 
-- #define XKB_KEY_osfBackTab      0x1004FF07
pattern XKB_KEY_osfBackTab :: KeySymbol
pattern XKB_KEY_osfBackTab = MkKeySymbol #{const XKB_KEY_osfBackTab} 
-- #define XKB_KEY_osfBackSpace        0x1004FF08
pattern XKB_KEY_osfBackSpace :: KeySymbol
pattern XKB_KEY_osfBackSpace = MkKeySymbol #{const XKB_KEY_osfBackSpace} 
-- #define XKB_KEY_osfClear        0x1004FF0B
pattern XKB_KEY_osfClear :: KeySymbol
pattern XKB_KEY_osfClear = MkKeySymbol #{const XKB_KEY_osfClear} 
-- #define XKB_KEY_osfEscape       0x1004FF1B
pattern XKB_KEY_osfEscape :: KeySymbol
pattern XKB_KEY_osfEscape = MkKeySymbol #{const XKB_KEY_osfEscape} 
-- #define XKB_KEY_osfAddMode      0x1004FF31
pattern XKB_KEY_osfAddMode :: KeySymbol
pattern XKB_KEY_osfAddMode = MkKeySymbol #{const XKB_KEY_osfAddMode} 
-- #define XKB_KEY_osfPrimaryPaste 0x1004FF32
pattern XKB_KEY_osfPrimaryPaste :: KeySymbol
pattern XKB_KEY_osfPrimaryPaste = MkKeySymbol #{const XKB_KEY_osfPrimaryPaste} 
-- #define XKB_KEY_osfQuickPaste   0x1004FF33
pattern XKB_KEY_osfQuickPaste :: KeySymbol
pattern XKB_KEY_osfQuickPaste = MkKeySymbol #{const XKB_KEY_osfQuickPaste} 
-- #define XKB_KEY_osfPageLeft     0x1004FF40
pattern XKB_KEY_osfPageLeft :: KeySymbol
pattern XKB_KEY_osfPageLeft = MkKeySymbol #{const XKB_KEY_osfPageLeft} 
-- #define XKB_KEY_osfPageUp       0x1004FF41
pattern XKB_KEY_osfPageUp :: KeySymbol
pattern XKB_KEY_osfPageUp = MkKeySymbol #{const XKB_KEY_osfPageUp} 
-- #define XKB_KEY_osfPageDown     0x1004FF42
pattern XKB_KEY_osfPageDown :: KeySymbol
pattern XKB_KEY_osfPageDown = MkKeySymbol #{const XKB_KEY_osfPageDown} 
-- #define XKB_KEY_osfPageRight        0x1004FF43
pattern XKB_KEY_osfPageRight :: KeySymbol
pattern XKB_KEY_osfPageRight = MkKeySymbol #{const XKB_KEY_osfPageRight} 
-- #define XKB_KEY_osfActivate     0x1004FF44
pattern XKB_KEY_osfActivate :: KeySymbol
pattern XKB_KEY_osfActivate = MkKeySymbol #{const XKB_KEY_osfActivate} 
-- #define XKB_KEY_osfMenuBar      0x1004FF45
pattern XKB_KEY_osfMenuBar :: KeySymbol
pattern XKB_KEY_osfMenuBar = MkKeySymbol #{const XKB_KEY_osfMenuBar} 
-- #define XKB_KEY_osfLeft     0x1004FF51
pattern XKB_KEY_osfLeft :: KeySymbol
pattern XKB_KEY_osfLeft = MkKeySymbol #{const XKB_KEY_osfLeft} 
-- #define XKB_KEY_osfUp       0x1004FF52
pattern XKB_KEY_osfUp :: KeySymbol
pattern XKB_KEY_osfUp = MkKeySymbol #{const XKB_KEY_osfUp} 
-- #define XKB_KEY_osfRight        0x1004FF53
pattern XKB_KEY_osfRight :: KeySymbol
pattern XKB_KEY_osfRight = MkKeySymbol #{const XKB_KEY_osfRight} 
-- #define XKB_KEY_osfDown     0x1004FF54
pattern XKB_KEY_osfDown :: KeySymbol
pattern XKB_KEY_osfDown = MkKeySymbol #{const XKB_KEY_osfDown} 
-- #define XKB_KEY_osfEndLine      0x1004FF57
pattern XKB_KEY_osfEndLine :: KeySymbol
pattern XKB_KEY_osfEndLine = MkKeySymbol #{const XKB_KEY_osfEndLine} 
-- #define XKB_KEY_osfBeginLine        0x1004FF58
pattern XKB_KEY_osfBeginLine :: KeySymbol
pattern XKB_KEY_osfBeginLine = MkKeySymbol #{const XKB_KEY_osfBeginLine} 
-- #define XKB_KEY_osfEndData      0x1004FF59
pattern XKB_KEY_osfEndData :: KeySymbol
pattern XKB_KEY_osfEndData = MkKeySymbol #{const XKB_KEY_osfEndData} 
-- #define XKB_KEY_osfBeginData        0x1004FF5A
pattern XKB_KEY_osfBeginData :: KeySymbol
pattern XKB_KEY_osfBeginData = MkKeySymbol #{const XKB_KEY_osfBeginData} 
-- #define XKB_KEY_osfPrevMenu     0x1004FF5B
pattern XKB_KEY_osfPrevMenu :: KeySymbol
pattern XKB_KEY_osfPrevMenu = MkKeySymbol #{const XKB_KEY_osfPrevMenu} 
-- #define XKB_KEY_osfNextMenu     0x1004FF5C
pattern XKB_KEY_osfNextMenu :: KeySymbol
pattern XKB_KEY_osfNextMenu = MkKeySymbol #{const XKB_KEY_osfNextMenu} 
-- #define XKB_KEY_osfPrevField        0x1004FF5D
pattern XKB_KEY_osfPrevField :: KeySymbol
pattern XKB_KEY_osfPrevField = MkKeySymbol #{const XKB_KEY_osfPrevField} 
-- #define XKB_KEY_osfNextField        0x1004FF5E
pattern XKB_KEY_osfNextField :: KeySymbol
pattern XKB_KEY_osfNextField = MkKeySymbol #{const XKB_KEY_osfNextField} 
-- #define XKB_KEY_osfSelect       0x1004FF60
pattern XKB_KEY_osfSelect :: KeySymbol
pattern XKB_KEY_osfSelect = MkKeySymbol #{const XKB_KEY_osfSelect} 
-- #define XKB_KEY_osfInsert       0x1004FF63
pattern XKB_KEY_osfInsert :: KeySymbol
pattern XKB_KEY_osfInsert = MkKeySymbol #{const XKB_KEY_osfInsert} 
-- #define XKB_KEY_osfUndo     0x1004FF65
pattern XKB_KEY_osfUndo :: KeySymbol
pattern XKB_KEY_osfUndo = MkKeySymbol #{const XKB_KEY_osfUndo} 
-- #define XKB_KEY_osfMenu     0x1004FF67
pattern XKB_KEY_osfMenu :: KeySymbol
pattern XKB_KEY_osfMenu = MkKeySymbol #{const XKB_KEY_osfMenu} 
-- #define XKB_KEY_osfCancel       0x1004FF69
pattern XKB_KEY_osfCancel :: KeySymbol
pattern XKB_KEY_osfCancel = MkKeySymbol #{const XKB_KEY_osfCancel} 
-- #define XKB_KEY_osfHelp     0x1004FF6A
pattern XKB_KEY_osfHelp :: KeySymbol
pattern XKB_KEY_osfHelp = MkKeySymbol #{const XKB_KEY_osfHelp} 
-- #define XKB_KEY_osfSelectAll        0x1004FF71
pattern XKB_KEY_osfSelectAll :: KeySymbol
pattern XKB_KEY_osfSelectAll = MkKeySymbol #{const XKB_KEY_osfSelectAll} 
-- #define XKB_KEY_osfDeselectAll  0x1004FF72
pattern XKB_KEY_osfDeselectAll :: KeySymbol
pattern XKB_KEY_osfDeselectAll = MkKeySymbol #{const XKB_KEY_osfDeselectAll} 
-- #define XKB_KEY_osfReselect     0x1004FF73
pattern XKB_KEY_osfReselect :: KeySymbol
pattern XKB_KEY_osfReselect = MkKeySymbol #{const XKB_KEY_osfReselect} 
-- #define XKB_KEY_osfExtend       0x1004FF74
pattern XKB_KEY_osfExtend :: KeySymbol
pattern XKB_KEY_osfExtend = MkKeySymbol #{const XKB_KEY_osfExtend} 
-- #define XKB_KEY_osfRestore      0x1004FF78
pattern XKB_KEY_osfRestore :: KeySymbol
pattern XKB_KEY_osfRestore = MkKeySymbol #{const XKB_KEY_osfRestore} 
-- #define XKB_KEY_osfDelete       0x1004FFFF
pattern XKB_KEY_osfDelete :: KeySymbol
pattern XKB_KEY_osfDelete = MkKeySymbol #{const XKB_KEY_osfDelete} 
-- #define XKB_KEY_Reset                0x1000FF6C
pattern XKB_KEY_Reset :: KeySymbol
pattern XKB_KEY_Reset = MkKeySymbol #{const XKB_KEY_Reset} 
-- #define XKB_KEY_System               0x1000FF6D
pattern XKB_KEY_System :: KeySymbol
pattern XKB_KEY_System = MkKeySymbol #{const XKB_KEY_System} 
-- #define XKB_KEY_User                 0x1000FF6E
pattern XKB_KEY_User :: KeySymbol
pattern XKB_KEY_User = MkKeySymbol #{const XKB_KEY_User} 
-- #define XKB_KEY_ClearLine            0x1000FF6F
pattern XKB_KEY_ClearLine :: KeySymbol
pattern XKB_KEY_ClearLine = MkKeySymbol #{const XKB_KEY_ClearLine} 
-- #define XKB_KEY_InsertLine           0x1000FF70
pattern XKB_KEY_InsertLine :: KeySymbol
pattern XKB_KEY_InsertLine = MkKeySymbol #{const XKB_KEY_InsertLine} 
-- #define XKB_KEY_DeleteLine           0x1000FF71
pattern XKB_KEY_DeleteLine :: KeySymbol
pattern XKB_KEY_DeleteLine = MkKeySymbol #{const XKB_KEY_DeleteLine} 
-- #define XKB_KEY_InsertChar           0x1000FF72
pattern XKB_KEY_InsertChar :: KeySymbol
pattern XKB_KEY_InsertChar = MkKeySymbol #{const XKB_KEY_InsertChar} 
-- #define XKB_KEY_DeleteChar           0x1000FF73
pattern XKB_KEY_DeleteChar :: KeySymbol
pattern XKB_KEY_DeleteChar = MkKeySymbol #{const XKB_KEY_DeleteChar} 
-- #define XKB_KEY_BackTab              0x1000FF74
pattern XKB_KEY_BackTab :: KeySymbol
pattern XKB_KEY_BackTab = MkKeySymbol #{const XKB_KEY_BackTab} 
-- #define XKB_KEY_KP_BackTab           0x1000FF75
pattern XKB_KEY_KP_BackTab :: KeySymbol
pattern XKB_KEY_KP_BackTab = MkKeySymbol #{const XKB_KEY_KP_BackTab} 
-- #define XKB_KEY_Ext16bit_L           0x1000FF76
pattern XKB_KEY_Ext16bit_L :: KeySymbol
pattern XKB_KEY_Ext16bit_L = MkKeySymbol #{const XKB_KEY_Ext16bit_L} 
-- #define XKB_KEY_Ext16bit_R           0x1000FF77
pattern XKB_KEY_Ext16bit_R :: KeySymbol
pattern XKB_KEY_Ext16bit_R = MkKeySymbol #{const XKB_KEY_Ext16bit_R} 
-- #define XKB_KEY_mute_acute           0x100000a8
pattern XKB_KEY_mute_acute :: KeySymbol
pattern XKB_KEY_mute_acute = MkKeySymbol #{const XKB_KEY_mute_acute} 
-- #define XKB_KEY_mute_grave           0x100000a9
pattern XKB_KEY_mute_grave :: KeySymbol
pattern XKB_KEY_mute_grave = MkKeySymbol #{const XKB_KEY_mute_grave} 
-- #define XKB_KEY_mute_asciicircum     0x100000aa
pattern XKB_KEY_mute_asciicircum :: KeySymbol
pattern XKB_KEY_mute_asciicircum = MkKeySymbol #{const XKB_KEY_mute_asciicircum} 
-- #define XKB_KEY_mute_diaeresis       0x100000ab
pattern XKB_KEY_mute_diaeresis :: KeySymbol
pattern XKB_KEY_mute_diaeresis = MkKeySymbol #{const XKB_KEY_mute_diaeresis} 
-- #define XKB_KEY_mute_asciitilde      0x100000ac
pattern XKB_KEY_mute_asciitilde :: KeySymbol
pattern XKB_KEY_mute_asciitilde = MkKeySymbol #{const XKB_KEY_mute_asciitilde} 
-- #define XKB_KEY_lira                 0x100000af
pattern XKB_KEY_lira :: KeySymbol
pattern XKB_KEY_lira = MkKeySymbol #{const XKB_KEY_lira} 
-- #define XKB_KEY_guilder              0x100000be
pattern XKB_KEY_guilder :: KeySymbol
pattern XKB_KEY_guilder = MkKeySymbol #{const XKB_KEY_guilder} 
-- #define XKB_KEY_IO                   0x100000ee
pattern XKB_KEY_IO :: KeySymbol
pattern XKB_KEY_IO = MkKeySymbol #{const XKB_KEY_IO} 
-- #define XKB_KEY_longminus            0x100000f6
pattern XKB_KEY_longminus :: KeySymbol
pattern XKB_KEY_longminus = MkKeySymbol #{const XKB_KEY_longminus} 
-- #define XKB_KEY_block                0x100000fc
pattern XKB_KEY_block :: KeySymbol
pattern XKB_KEY_block = MkKeySymbol #{const XKB_KEY_block} 

showKeySymbol :: KeySymbol -> T.Text
-- #define XKB_KEY_NoSymbol                    0x000000  /* Special KeySym */
showKeySymbol XKB_KEY_NoSymbol = "NoSymbol"
-- #define XKB_KEY_VoidSymbol                  0xffffff  /* Void symbol */
showKeySymbol XKB_KEY_VoidSymbol = "VoidSymbol"
-- #define XKB_KEY_BackSpace                     0xff08  /* Back space, back char */
showKeySymbol XKB_KEY_BackSpace = "BackSpace"
-- #define XKB_KEY_Tab                           0xff09
showKeySymbol XKB_KEY_Tab = "Tab"
-- #define XKB_KEY_Linefeed                      0xff0a  /* Linefeed, LF */
showKeySymbol XKB_KEY_Linefeed = "Linefeed"
-- #define XKB_KEY_Clear                         0xff0b
showKeySymbol XKB_KEY_Clear = "Clear"
-- #define XKB_KEY_Return                        0xff0d  /* Return, enter */
showKeySymbol XKB_KEY_Return = "Return"
-- #define XKB_KEY_Pause                         0xff13  /* Pause, hold */
showKeySymbol XKB_KEY_Pause = "Pause"
-- #define XKB_KEY_Scroll_Lock                   0xff14
showKeySymbol XKB_KEY_Scroll_Lock = "Scroll_Lock"
-- #define XKB_KEY_Sys_Req                       0xff15
showKeySymbol XKB_KEY_Sys_Req = "Sys_Req"
-- #define XKB_KEY_Escape                        0xff1b
showKeySymbol XKB_KEY_Escape = "Escape"
-- #define XKB_KEY_Delete                        0xffff  /* Delete, rubout */
showKeySymbol XKB_KEY_Delete = "Delete"
-- #define XKB_KEY_Multi_key                     0xff20  /* Multi-key character compose */
showKeySymbol XKB_KEY_Multi_key = "Multi_key"
-- #define XKB_KEY_Codeinput                     0xff37
showKeySymbol XKB_KEY_Codeinput = "Codeinput"
-- #define XKB_KEY_SingleCandidate               0xff3c
showKeySymbol XKB_KEY_SingleCandidate = "SingleCandidate"
-- #define XKB_KEY_MultipleCandidate             0xff3d
showKeySymbol XKB_KEY_MultipleCandidate = "MultipleCandidate"
-- #define XKB_KEY_PreviousCandidate             0xff3e
showKeySymbol XKB_KEY_PreviousCandidate = "PreviousCandidate"
-- #define XKB_KEY_Kanji                         0xff21  /* Kanji, Kanji convert */
showKeySymbol XKB_KEY_Kanji = "Kanji"
-- #define XKB_KEY_Muhenkan                      0xff22  /* Cancel Conversion */
showKeySymbol XKB_KEY_Muhenkan = "Muhenkan"
-- #define XKB_KEY_Henkan_Mode                   0xff23  /* Start/Stop Conversion */
showKeySymbol XKB_KEY_Henkan_Mode = "Henkan_Mode"
-- #define XKB_KEY_Henkan                        0xff23  /* Alias for Henkan_Mode */
showKeySymbol XKB_KEY_Henkan = "Henkan"
-- #define XKB_KEY_Romaji                        0xff24  /* to Romaji */
showKeySymbol XKB_KEY_Romaji = "Romaji"
-- #define XKB_KEY_Hiragana                      0xff25  /* to Hiragana */
showKeySymbol XKB_KEY_Hiragana = "Hiragana"
-- #define XKB_KEY_Katakana                      0xff26  /* to Katakana */
showKeySymbol XKB_KEY_Katakana = "Katakana"
-- #define XKB_KEY_Hiragana_Katakana             0xff27  /* Hiragana/Katakana toggle */
showKeySymbol XKB_KEY_Hiragana_Katakana = "Hiragana_Katakana"
-- #define XKB_KEY_Zenkaku                       0xff28  /* to Zenkaku */
showKeySymbol XKB_KEY_Zenkaku = "Zenkaku"
-- #define XKB_KEY_Hankaku                       0xff29  /* to Hankaku */
showKeySymbol XKB_KEY_Hankaku = "Hankaku"
-- #define XKB_KEY_Zenkaku_Hankaku               0xff2a  /* Zenkaku/Hankaku toggle */
showKeySymbol XKB_KEY_Zenkaku_Hankaku = "Zenkaku_Hankaku"
-- #define XKB_KEY_Touroku                       0xff2b  /* Add to Dictionary */
showKeySymbol XKB_KEY_Touroku = "Touroku"
-- #define XKB_KEY_Massyo                        0xff2c  /* Delete from Dictionary */
showKeySymbol XKB_KEY_Massyo = "Massyo"
-- #define XKB_KEY_Kana_Lock                     0xff2d  /* Kana Lock */
showKeySymbol XKB_KEY_Kana_Lock = "Kana_Lock"
-- #define XKB_KEY_Kana_Shift                    0xff2e  /* Kana Shift */
showKeySymbol XKB_KEY_Kana_Shift = "Kana_Shift"
-- #define XKB_KEY_Eisu_Shift                    0xff2f  /* Alphanumeric Shift */
showKeySymbol XKB_KEY_Eisu_Shift = "Eisu_Shift"
-- #define XKB_KEY_Eisu_toggle                   0xff30  /* Alphanumeric toggle */
showKeySymbol XKB_KEY_Eisu_toggle = "Eisu_toggle"
-- #define XKB_KEY_Kanji_Bangou                  0xff37  /* Codeinput */
showKeySymbol XKB_KEY_Kanji_Bangou = "Kanji_Bangou"
-- #define XKB_KEY_Zen_Koho                      0xff3d  /* Multiple/All Candidate(s) */
showKeySymbol XKB_KEY_Zen_Koho = "Zen_Koho"
-- #define XKB_KEY_Mae_Koho                      0xff3e  /* Previous Candidate */
showKeySymbol XKB_KEY_Mae_Koho = "Mae_Koho"
-- #define XKB_KEY_Home                          0xff50
showKeySymbol XKB_KEY_Home = "Home"
-- #define XKB_KEY_Left                          0xff51  /* Move left, left arrow */
showKeySymbol XKB_KEY_Left = "Left"
-- #define XKB_KEY_Up                            0xff52  /* Move up, up arrow */
showKeySymbol XKB_KEY_Up = "Up"
-- #define XKB_KEY_Right                         0xff53  /* Move right, right arrow */
showKeySymbol XKB_KEY_Right = "Right"
-- #define XKB_KEY_Down                          0xff54  /* Move down, down arrow */
showKeySymbol XKB_KEY_Down = "Down"
-- #define XKB_KEY_Prior                         0xff55  /* Prior, previous */
showKeySymbol XKB_KEY_Prior = "Prior"
-- #define XKB_KEY_Page_Up                       0xff55
showKeySymbol XKB_KEY_Page_Up = "Page_Up"
-- #define XKB_KEY_Next                          0xff56  /* Next */
showKeySymbol XKB_KEY_Next = "Next"
-- #define XKB_KEY_Page_Down                     0xff56
showKeySymbol XKB_KEY_Page_Down = "Page_Down"
-- #define XKB_KEY_End                           0xff57  /* EOL */
showKeySymbol XKB_KEY_End = "End"
-- #define XKB_KEY_Begin                         0xff58  /* BOL */
showKeySymbol XKB_KEY_Begin = "Begin"
-- #define XKB_KEY_Select                        0xff60  /* Select, mark */
showKeySymbol XKB_KEY_Select = "Select"
-- #define XKB_KEY_Print                         0xff61
showKeySymbol XKB_KEY_Print = "Print"
-- #define XKB_KEY_Execute                       0xff62  /* Execute, run, do */
showKeySymbol XKB_KEY_Execute = "Execute"
-- #define XKB_KEY_Insert                        0xff63  /* Insert, insert here */
showKeySymbol XKB_KEY_Insert = "Insert"
-- #define XKB_KEY_Undo                          0xff65
showKeySymbol XKB_KEY_Undo = "Undo"
-- #define XKB_KEY_Redo                          0xff66  /* Redo, again */
showKeySymbol XKB_KEY_Redo = "Redo"
-- #define XKB_KEY_Menu                          0xff67
showKeySymbol XKB_KEY_Menu = "Menu"
-- #define XKB_KEY_Find                          0xff68  /* Find, search */
showKeySymbol XKB_KEY_Find = "Find"
-- #define XKB_KEY_Cancel                        0xff69  /* Cancel, stop, abort, exit */
showKeySymbol XKB_KEY_Cancel = "Cancel"
-- #define XKB_KEY_Help                          0xff6a  /* Help */
showKeySymbol XKB_KEY_Help = "Help"
-- #define XKB_KEY_Break                         0xff6b
showKeySymbol XKB_KEY_Break = "Break"
-- #define XKB_KEY_Mode_switch                   0xff7e  /* Character set switch */
showKeySymbol XKB_KEY_Mode_switch = "Mode_switch"
-- #define XKB_KEY_script_switch                 0xff7e  /* Alias for mode_switch */
showKeySymbol XKB_KEY_script_switch = "script_switch"
-- #define XKB_KEY_Num_Lock                      0xff7f
showKeySymbol XKB_KEY_Num_Lock = "Num_Lock"
-- #define XKB_KEY_KP_Space                      0xff80  /* Space */
showKeySymbol XKB_KEY_KP_Space = "KP_Space"
-- #define XKB_KEY_KP_Tab                        0xff89
showKeySymbol XKB_KEY_KP_Tab = "KP_Tab"
-- #define XKB_KEY_KP_Enter                      0xff8d  /* Enter */
showKeySymbol XKB_KEY_KP_Enter = "KP_Enter"
-- #define XKB_KEY_KP_F1                         0xff91  /* PF1, KP_A, ... */
showKeySymbol XKB_KEY_KP_F1 = "KP_F1"
-- #define XKB_KEY_KP_F2                         0xff92
showKeySymbol XKB_KEY_KP_F2 = "KP_F2"
-- #define XKB_KEY_KP_F3                         0xff93
showKeySymbol XKB_KEY_KP_F3 = "KP_F3"
-- #define XKB_KEY_KP_F4                         0xff94
showKeySymbol XKB_KEY_KP_F4 = "KP_F4"
-- #define XKB_KEY_KP_Home                       0xff95
showKeySymbol XKB_KEY_KP_Home = "KP_Home"
-- #define XKB_KEY_KP_Left                       0xff96
showKeySymbol XKB_KEY_KP_Left = "KP_Left"
-- #define XKB_KEY_KP_Up                         0xff97
showKeySymbol XKB_KEY_KP_Up = "KP_Up"
-- #define XKB_KEY_KP_Right                      0xff98
showKeySymbol XKB_KEY_KP_Right = "KP_Right"
-- #define XKB_KEY_KP_Down                       0xff99
showKeySymbol XKB_KEY_KP_Down = "KP_Down"
-- #define XKB_KEY_KP_Prior                      0xff9a
showKeySymbol XKB_KEY_KP_Prior = "KP_Prior"
-- #define XKB_KEY_KP_Page_Up                    0xff9a
showKeySymbol XKB_KEY_KP_Page_Up = "KP_Page_Up"
-- #define XKB_KEY_KP_Next                       0xff9b
showKeySymbol XKB_KEY_KP_Next = "KP_Next"
-- #define XKB_KEY_KP_Page_Down                  0xff9b
showKeySymbol XKB_KEY_KP_Page_Down = "KP_Page_Down"
-- #define XKB_KEY_KP_End                        0xff9c
showKeySymbol XKB_KEY_KP_End = "KP_End"
-- #define XKB_KEY_KP_Begin                      0xff9d
showKeySymbol XKB_KEY_KP_Begin = "KP_Begin"
-- #define XKB_KEY_KP_Insert                     0xff9e
showKeySymbol XKB_KEY_KP_Insert = "KP_Insert"
-- #define XKB_KEY_KP_Delete                     0xff9f
showKeySymbol XKB_KEY_KP_Delete = "KP_Delete"
-- #define XKB_KEY_KP_Equal                      0xffbd  /* Equals */
showKeySymbol XKB_KEY_KP_Equal = "KP_Equal"
-- #define XKB_KEY_KP_Multiply                   0xffaa
showKeySymbol XKB_KEY_KP_Multiply = "KP_Multiply"
-- #define XKB_KEY_KP_Add                        0xffab
showKeySymbol XKB_KEY_KP_Add = "KP_Add"
-- #define XKB_KEY_KP_Separator                  0xffac  /* Separator, often comma */
showKeySymbol XKB_KEY_KP_Separator = "KP_Separator"
-- #define XKB_KEY_KP_Subtract                   0xffad
showKeySymbol XKB_KEY_KP_Subtract = "KP_Subtract"
-- #define XKB_KEY_KP_Decimal                    0xffae
showKeySymbol XKB_KEY_KP_Decimal = "KP_Decimal"
-- #define XKB_KEY_KP_Divide                     0xffaf
showKeySymbol XKB_KEY_KP_Divide = "KP_Divide"
-- #define XKB_KEY_KP_0                          0xffb0
showKeySymbol XKB_KEY_KP_0 = "KP_0"
-- #define XKB_KEY_KP_1                          0xffb1
showKeySymbol XKB_KEY_KP_1 = "KP_1"
-- #define XKB_KEY_KP_2                          0xffb2
showKeySymbol XKB_KEY_KP_2 = "KP_2"
-- #define XKB_KEY_KP_3                          0xffb3
showKeySymbol XKB_KEY_KP_3 = "KP_3"
-- #define XKB_KEY_KP_4                          0xffb4
showKeySymbol XKB_KEY_KP_4 = "KP_4"
-- #define XKB_KEY_KP_5                          0xffb5
showKeySymbol XKB_KEY_KP_5 = "KP_5"
-- #define XKB_KEY_KP_6                          0xffb6
showKeySymbol XKB_KEY_KP_6 = "KP_6"
-- #define XKB_KEY_KP_7                          0xffb7
showKeySymbol XKB_KEY_KP_7 = "KP_7"
-- #define XKB_KEY_KP_8                          0xffb8
showKeySymbol XKB_KEY_KP_8 = "KP_8"
-- #define XKB_KEY_KP_9                          0xffb9
showKeySymbol XKB_KEY_KP_9 = "KP_9"
-- #define XKB_KEY_F1                            0xffbe
showKeySymbol XKB_KEY_F1 = "F1"
-- #define XKB_KEY_F2                            0xffbf
showKeySymbol XKB_KEY_F2 = "F2"
-- #define XKB_KEY_F3                            0xffc0
showKeySymbol XKB_KEY_F3 = "F3"
-- #define XKB_KEY_F4                            0xffc1
showKeySymbol XKB_KEY_F4 = "F4"
-- #define XKB_KEY_F5                            0xffc2
showKeySymbol XKB_KEY_F5 = "F5"
-- #define XKB_KEY_F6                            0xffc3
showKeySymbol XKB_KEY_F6 = "F6"
-- #define XKB_KEY_F7                            0xffc4
showKeySymbol XKB_KEY_F7 = "F7"
-- #define XKB_KEY_F8                            0xffc5
showKeySymbol XKB_KEY_F8 = "F8"
-- #define XKB_KEY_F9                            0xffc6
showKeySymbol XKB_KEY_F9 = "F9"
-- #define XKB_KEY_F10                           0xffc7
showKeySymbol XKB_KEY_F10 = "F10"
-- #define XKB_KEY_F11                           0xffc8
showKeySymbol XKB_KEY_F11 = "F11"
-- #define XKB_KEY_L1                            0xffc8
showKeySymbol XKB_KEY_L1 = "L1"
-- #define XKB_KEY_F12                           0xffc9
showKeySymbol XKB_KEY_F12 = "F12"
-- #define XKB_KEY_L2                            0xffc9
showKeySymbol XKB_KEY_L2 = "L2"
-- #define XKB_KEY_F13                           0xffca
showKeySymbol XKB_KEY_F13 = "F13"
-- #define XKB_KEY_L3                            0xffca
showKeySymbol XKB_KEY_L3 = "L3"
-- #define XKB_KEY_F14                           0xffcb
showKeySymbol XKB_KEY_F14 = "F14"
-- #define XKB_KEY_L4                            0xffcb
showKeySymbol XKB_KEY_L4 = "L4"
-- #define XKB_KEY_F15                           0xffcc
showKeySymbol XKB_KEY_F15 = "F15"
-- #define XKB_KEY_L5                            0xffcc
showKeySymbol XKB_KEY_L5 = "L5"
-- #define XKB_KEY_F16                           0xffcd
showKeySymbol XKB_KEY_F16 = "F16"
-- #define XKB_KEY_L6                            0xffcd
showKeySymbol XKB_KEY_L6 = "L6"
-- #define XKB_KEY_F17                           0xffce
showKeySymbol XKB_KEY_F17 = "F17"
-- #define XKB_KEY_L7                            0xffce
showKeySymbol XKB_KEY_L7 = "L7"
-- #define XKB_KEY_F18                           0xffcf
showKeySymbol XKB_KEY_F18 = "F18"
-- #define XKB_KEY_L8                            0xffcf
showKeySymbol XKB_KEY_L8 = "L8"
-- #define XKB_KEY_F19                           0xffd0
showKeySymbol XKB_KEY_F19 = "F19"
-- #define XKB_KEY_L9                            0xffd0
showKeySymbol XKB_KEY_L9 = "L9"
-- #define XKB_KEY_F20                           0xffd1
showKeySymbol XKB_KEY_F20 = "F20"
-- #define XKB_KEY_L10                           0xffd1
showKeySymbol XKB_KEY_L10 = "L10"
-- #define XKB_KEY_F21                           0xffd2
showKeySymbol XKB_KEY_F21 = "F21"
-- #define XKB_KEY_R1                            0xffd2
showKeySymbol XKB_KEY_R1 = "R1"
-- #define XKB_KEY_F22                           0xffd3
showKeySymbol XKB_KEY_F22 = "F22"
-- #define XKB_KEY_R2                            0xffd3
showKeySymbol XKB_KEY_R2 = "R2"
-- #define XKB_KEY_F23                           0xffd4
showKeySymbol XKB_KEY_F23 = "F23"
-- #define XKB_KEY_R3                            0xffd4
showKeySymbol XKB_KEY_R3 = "R3"
-- #define XKB_KEY_F24                           0xffd5
showKeySymbol XKB_KEY_F24 = "F24"
-- #define XKB_KEY_R4                            0xffd5
showKeySymbol XKB_KEY_R4 = "R4"
-- #define XKB_KEY_F25                           0xffd6
showKeySymbol XKB_KEY_F25 = "F25"
-- #define XKB_KEY_R5                            0xffd6
showKeySymbol XKB_KEY_R5 = "R5"
-- #define XKB_KEY_F26                           0xffd7
showKeySymbol XKB_KEY_F26 = "F26"
-- #define XKB_KEY_R6                            0xffd7
showKeySymbol XKB_KEY_R6 = "R6"
-- #define XKB_KEY_F27                           0xffd8
showKeySymbol XKB_KEY_F27 = "F27"
-- #define XKB_KEY_R7                            0xffd8
showKeySymbol XKB_KEY_R7 = "R7"
-- #define XKB_KEY_F28                           0xffd9
showKeySymbol XKB_KEY_F28 = "F28"
-- #define XKB_KEY_R8                            0xffd9
showKeySymbol XKB_KEY_R8 = "R8"
-- #define XKB_KEY_F29                           0xffda
showKeySymbol XKB_KEY_F29 = "F29"
-- #define XKB_KEY_R9                            0xffda
showKeySymbol XKB_KEY_R9 = "R9"
-- #define XKB_KEY_F30                           0xffdb
showKeySymbol XKB_KEY_F30 = "F30"
-- #define XKB_KEY_R10                           0xffdb
showKeySymbol XKB_KEY_R10 = "R10"
-- #define XKB_KEY_F31                           0xffdc
showKeySymbol XKB_KEY_F31 = "F31"
-- #define XKB_KEY_R11                           0xffdc
showKeySymbol XKB_KEY_R11 = "R11"
-- #define XKB_KEY_F32                           0xffdd
showKeySymbol XKB_KEY_F32 = "F32"
-- #define XKB_KEY_R12                           0xffdd
showKeySymbol XKB_KEY_R12 = "R12"
-- #define XKB_KEY_F33                           0xffde
showKeySymbol XKB_KEY_F33 = "F33"
-- #define XKB_KEY_R13                           0xffde
showKeySymbol XKB_KEY_R13 = "R13"
-- #define XKB_KEY_F34                           0xffdf
showKeySymbol XKB_KEY_F34 = "F34"
-- #define XKB_KEY_R14                           0xffdf
showKeySymbol XKB_KEY_R14 = "R14"
-- #define XKB_KEY_F35                           0xffe0
showKeySymbol XKB_KEY_F35 = "F35"
-- #define XKB_KEY_R15                           0xffe0
showKeySymbol XKB_KEY_R15 = "R15"
-- #define XKB_KEY_Shift_L                       0xffe1  /* Left shift */
showKeySymbol XKB_KEY_Shift_L = "Shift_L"
-- #define XKB_KEY_Shift_R                       0xffe2  /* Right shift */
showKeySymbol XKB_KEY_Shift_R = "Shift_R"
-- #define XKB_KEY_Control_L                     0xffe3  /* Left control */
showKeySymbol XKB_KEY_Control_L = "Control_L"
-- #define XKB_KEY_Control_R                     0xffe4  /* Right control */
showKeySymbol XKB_KEY_Control_R = "Control_R"
-- #define XKB_KEY_Caps_Lock                     0xffe5  /* Caps lock */
showKeySymbol XKB_KEY_Caps_Lock = "Caps_Lock"
-- #define XKB_KEY_Shift_Lock                    0xffe6  /* Shift lock */
showKeySymbol XKB_KEY_Shift_Lock = "Shift_Lock"
-- #define XKB_KEY_Meta_L                        0xffe7  /* Left meta */
showKeySymbol XKB_KEY_Meta_L = "Meta_L"
-- #define XKB_KEY_Meta_R                        0xffe8  /* Right meta */
showKeySymbol XKB_KEY_Meta_R = "Meta_R"
-- #define XKB_KEY_Alt_L                         0xffe9  /* Left alt */
showKeySymbol XKB_KEY_Alt_L = "Alt_L"
-- #define XKB_KEY_Alt_R                         0xffea  /* Right alt */
showKeySymbol XKB_KEY_Alt_R = "Alt_R"
-- #define XKB_KEY_Super_L                       0xffeb  /* Left super */
showKeySymbol XKB_KEY_Super_L = "Super_L"
-- #define XKB_KEY_Super_R                       0xffec  /* Right super */
showKeySymbol XKB_KEY_Super_R = "Super_R"
-- #define XKB_KEY_Hyper_L                       0xffed  /* Left hyper */
showKeySymbol XKB_KEY_Hyper_L = "Hyper_L"
-- #define XKB_KEY_Hyper_R                       0xffee  /* Right hyper */
showKeySymbol XKB_KEY_Hyper_R = "Hyper_R"
-- #define XKB_KEY_ISO_Lock                      0xfe01
showKeySymbol XKB_KEY_ISO_Lock = "ISO_Lock"
-- #define XKB_KEY_ISO_Level2_Latch              0xfe02
showKeySymbol XKB_KEY_ISO_Level2_Latch = "ISO_Level2_Latch"
-- #define XKB_KEY_ISO_Level3_Shift              0xfe03
showKeySymbol XKB_KEY_ISO_Level3_Shift = "ISO_Level3_Shift"
-- #define XKB_KEY_ISO_Level3_Latch              0xfe04
showKeySymbol XKB_KEY_ISO_Level3_Latch = "ISO_Level3_Latch"
-- #define XKB_KEY_ISO_Level3_Lock               0xfe05
showKeySymbol XKB_KEY_ISO_Level3_Lock = "ISO_Level3_Lock"
-- #define XKB_KEY_ISO_Level5_Shift              0xfe11
showKeySymbol XKB_KEY_ISO_Level5_Shift = "ISO_Level5_Shift"
-- #define XKB_KEY_ISO_Level5_Latch              0xfe12
showKeySymbol XKB_KEY_ISO_Level5_Latch = "ISO_Level5_Latch"
-- #define XKB_KEY_ISO_Level5_Lock               0xfe13
showKeySymbol XKB_KEY_ISO_Level5_Lock = "ISO_Level5_Lock"
-- #define XKB_KEY_ISO_Group_Shift               0xff7e  /* Alias for mode_switch */
showKeySymbol XKB_KEY_ISO_Group_Shift = "ISO_Group_Shift"
-- #define XKB_KEY_ISO_Group_Latch               0xfe06
showKeySymbol XKB_KEY_ISO_Group_Latch = "ISO_Group_Latch"
-- #define XKB_KEY_ISO_Group_Lock                0xfe07
showKeySymbol XKB_KEY_ISO_Group_Lock = "ISO_Group_Lock"
-- #define XKB_KEY_ISO_Next_Group                0xfe08
showKeySymbol XKB_KEY_ISO_Next_Group = "ISO_Next_Group"
-- #define XKB_KEY_ISO_Next_Group_Lock           0xfe09
showKeySymbol XKB_KEY_ISO_Next_Group_Lock = "ISO_Next_Group_Lock"
-- #define XKB_KEY_ISO_Prev_Group                0xfe0a
showKeySymbol XKB_KEY_ISO_Prev_Group = "ISO_Prev_Group"
-- #define XKB_KEY_ISO_Prev_Group_Lock           0xfe0b
showKeySymbol XKB_KEY_ISO_Prev_Group_Lock = "ISO_Prev_Group_Lock"
-- #define XKB_KEY_ISO_First_Group               0xfe0c
showKeySymbol XKB_KEY_ISO_First_Group = "ISO_First_Group"
-- #define XKB_KEY_ISO_First_Group_Lock          0xfe0d
showKeySymbol XKB_KEY_ISO_First_Group_Lock = "ISO_First_Group_Lock"
-- #define XKB_KEY_ISO_Last_Group                0xfe0e
showKeySymbol XKB_KEY_ISO_Last_Group = "ISO_Last_Group"
-- #define XKB_KEY_ISO_Last_Group_Lock           0xfe0f
showKeySymbol XKB_KEY_ISO_Last_Group_Lock = "ISO_Last_Group_Lock"
-- #define XKB_KEY_ISO_Left_Tab                  0xfe20
showKeySymbol XKB_KEY_ISO_Left_Tab = "ISO_Left_Tab"
-- #define XKB_KEY_ISO_Move_Line_Up              0xfe21
showKeySymbol XKB_KEY_ISO_Move_Line_Up = "ISO_Move_Line_Up"
-- #define XKB_KEY_ISO_Move_Line_Down            0xfe22
showKeySymbol XKB_KEY_ISO_Move_Line_Down = "ISO_Move_Line_Down"
-- #define XKB_KEY_ISO_Partial_Line_Up           0xfe23
showKeySymbol XKB_KEY_ISO_Partial_Line_Up = "ISO_Partial_Line_Up"
-- #define XKB_KEY_ISO_Partial_Line_Down         0xfe24
showKeySymbol XKB_KEY_ISO_Partial_Line_Down = "ISO_Partial_Line_Down"
-- #define XKB_KEY_ISO_Partial_Space_Left        0xfe25
showKeySymbol XKB_KEY_ISO_Partial_Space_Left = "ISO_Partial_Space_Left"
-- #define XKB_KEY_ISO_Partial_Space_Right       0xfe26
showKeySymbol XKB_KEY_ISO_Partial_Space_Right = "ISO_Partial_Space_Right"
-- #define XKB_KEY_ISO_Set_Margin_Left           0xfe27
showKeySymbol XKB_KEY_ISO_Set_Margin_Left = "ISO_Set_Margin_Left"
-- #define XKB_KEY_ISO_Set_Margin_Right          0xfe28
showKeySymbol XKB_KEY_ISO_Set_Margin_Right = "ISO_Set_Margin_Right"
-- #define XKB_KEY_ISO_Release_Margin_Left       0xfe29
showKeySymbol XKB_KEY_ISO_Release_Margin_Left = "ISO_Release_Margin_Left"
-- #define XKB_KEY_ISO_Release_Margin_Right      0xfe2a
showKeySymbol XKB_KEY_ISO_Release_Margin_Right = "ISO_Release_Margin_Right"
-- #define XKB_KEY_ISO_Release_Both_Margins      0xfe2b
showKeySymbol XKB_KEY_ISO_Release_Both_Margins = "ISO_Release_Both_Margins"
-- #define XKB_KEY_ISO_Fast_Cursor_Left          0xfe2c
showKeySymbol XKB_KEY_ISO_Fast_Cursor_Left = "ISO_Fast_Cursor_Left"
-- #define XKB_KEY_ISO_Fast_Cursor_Right         0xfe2d
showKeySymbol XKB_KEY_ISO_Fast_Cursor_Right = "ISO_Fast_Cursor_Right"
-- #define XKB_KEY_ISO_Fast_Cursor_Up            0xfe2e
showKeySymbol XKB_KEY_ISO_Fast_Cursor_Up = "ISO_Fast_Cursor_Up"
-- #define XKB_KEY_ISO_Fast_Cursor_Down          0xfe2f
showKeySymbol XKB_KEY_ISO_Fast_Cursor_Down = "ISO_Fast_Cursor_Down"
-- #define XKB_KEY_ISO_Continuous_Underline      0xfe30
showKeySymbol XKB_KEY_ISO_Continuous_Underline = "ISO_Continuous_Underline"
-- #define XKB_KEY_ISO_Discontinuous_Underline   0xfe31
showKeySymbol XKB_KEY_ISO_Discontinuous_Underline = "ISO_Discontinuous_Underline"
-- #define XKB_KEY_ISO_Emphasize                 0xfe32
showKeySymbol XKB_KEY_ISO_Emphasize = "ISO_Emphasize"
-- #define XKB_KEY_ISO_Center_Object             0xfe33
showKeySymbol XKB_KEY_ISO_Center_Object = "ISO_Center_Object"
-- #define XKB_KEY_ISO_Enter                     0xfe34
showKeySymbol XKB_KEY_ISO_Enter = "ISO_Enter"
-- #define XKB_KEY_dead_grave                    0xfe50
showKeySymbol XKB_KEY_dead_grave = "dead_grave"
-- #define XKB_KEY_dead_acute                    0xfe51
showKeySymbol XKB_KEY_dead_acute = "dead_acute"
-- #define XKB_KEY_dead_circumflex               0xfe52
showKeySymbol XKB_KEY_dead_circumflex = "dead_circumflex"
-- #define XKB_KEY_dead_tilde                    0xfe53
showKeySymbol XKB_KEY_dead_tilde = "dead_tilde"
-- #define XKB_KEY_dead_perispomeni              0xfe53  /* alias for dead_tilde */
showKeySymbol XKB_KEY_dead_perispomeni = "dead_perispomeni"
-- #define XKB_KEY_dead_macron                   0xfe54
showKeySymbol XKB_KEY_dead_macron = "dead_macron"
-- #define XKB_KEY_dead_breve                    0xfe55
showKeySymbol XKB_KEY_dead_breve = "dead_breve"
-- #define XKB_KEY_dead_abovedot                 0xfe56
showKeySymbol XKB_KEY_dead_abovedot = "dead_abovedot"
-- #define XKB_KEY_dead_diaeresis                0xfe57
showKeySymbol XKB_KEY_dead_diaeresis = "dead_diaeresis"
-- #define XKB_KEY_dead_abovering                0xfe58
showKeySymbol XKB_KEY_dead_abovering = "dead_abovering"
-- #define XKB_KEY_dead_doubleacute              0xfe59
showKeySymbol XKB_KEY_dead_doubleacute = "dead_doubleacute"
-- #define XKB_KEY_dead_caron                    0xfe5a
showKeySymbol XKB_KEY_dead_caron = "dead_caron"
-- #define XKB_KEY_dead_cedilla                  0xfe5b
showKeySymbol XKB_KEY_dead_cedilla = "dead_cedilla"
-- #define XKB_KEY_dead_ogonek                   0xfe5c
showKeySymbol XKB_KEY_dead_ogonek = "dead_ogonek"
-- #define XKB_KEY_dead_iota                     0xfe5d
showKeySymbol XKB_KEY_dead_iota = "dead_iota"
-- #define XKB_KEY_dead_voiced_sound             0xfe5e
showKeySymbol XKB_KEY_dead_voiced_sound = "dead_voiced_sound"
-- #define XKB_KEY_dead_semivoiced_sound         0xfe5f
showKeySymbol XKB_KEY_dead_semivoiced_sound = "dead_semivoiced_sound"
-- #define XKB_KEY_dead_belowdot                 0xfe60
showKeySymbol XKB_KEY_dead_belowdot = "dead_belowdot"
-- #define XKB_KEY_dead_hook                     0xfe61
showKeySymbol XKB_KEY_dead_hook = "dead_hook"
-- #define XKB_KEY_dead_horn                     0xfe62
showKeySymbol XKB_KEY_dead_horn = "dead_horn"
-- #define XKB_KEY_dead_stroke                   0xfe63
showKeySymbol XKB_KEY_dead_stroke = "dead_stroke"
-- #define XKB_KEY_dead_abovecomma               0xfe64
showKeySymbol XKB_KEY_dead_abovecomma = "dead_abovecomma"
-- #define XKB_KEY_dead_psili                    0xfe64  /* alias for dead_abovecomma */
showKeySymbol XKB_KEY_dead_psili = "dead_psili"
-- #define XKB_KEY_dead_abovereversedcomma       0xfe65
showKeySymbol XKB_KEY_dead_abovereversedcomma = "dead_abovereversedcomma"
-- #define XKB_KEY_dead_dasia                    0xfe65  /* alias for dead_abovereversedcomma */
showKeySymbol XKB_KEY_dead_dasia = "dead_dasia"
-- #define XKB_KEY_dead_doublegrave              0xfe66
showKeySymbol XKB_KEY_dead_doublegrave = "dead_doublegrave"
-- #define XKB_KEY_dead_belowring                0xfe67
showKeySymbol XKB_KEY_dead_belowring = "dead_belowring"
-- #define XKB_KEY_dead_belowmacron              0xfe68
showKeySymbol XKB_KEY_dead_belowmacron = "dead_belowmacron"
-- #define XKB_KEY_dead_belowcircumflex          0xfe69
showKeySymbol XKB_KEY_dead_belowcircumflex = "dead_belowcircumflex"
-- #define XKB_KEY_dead_belowtilde               0xfe6a
showKeySymbol XKB_KEY_dead_belowtilde = "dead_belowtilde"
-- #define XKB_KEY_dead_belowbreve               0xfe6b
showKeySymbol XKB_KEY_dead_belowbreve = "dead_belowbreve"
-- #define XKB_KEY_dead_belowdiaeresis           0xfe6c
showKeySymbol XKB_KEY_dead_belowdiaeresis = "dead_belowdiaeresis"
-- #define XKB_KEY_dead_invertedbreve            0xfe6d
showKeySymbol XKB_KEY_dead_invertedbreve = "dead_invertedbreve"
-- #define XKB_KEY_dead_belowcomma               0xfe6e
showKeySymbol XKB_KEY_dead_belowcomma = "dead_belowcomma"
-- #define XKB_KEY_dead_currency                 0xfe6f
showKeySymbol XKB_KEY_dead_currency = "dead_currency"
-- #define XKB_KEY_dead_lowline                  0xfe90
showKeySymbol XKB_KEY_dead_lowline = "dead_lowline"
-- #define XKB_KEY_dead_aboveverticalline        0xfe91
showKeySymbol XKB_KEY_dead_aboveverticalline = "dead_aboveverticalline"
-- #define XKB_KEY_dead_belowverticalline        0xfe92
showKeySymbol XKB_KEY_dead_belowverticalline = "dead_belowverticalline"
-- #define XKB_KEY_dead_longsolidusoverlay       0xfe93
showKeySymbol XKB_KEY_dead_longsolidusoverlay = "dead_longsolidusoverlay"
-- #define XKB_KEY_dead_a                        0xfe80
showKeySymbol XKB_KEY_dead_a = "dead_a"
-- #define XKB_KEY_dead_A                        0xfe81
showKeySymbol XKB_KEY_dead_A = "dead_A"
-- #define XKB_KEY_dead_e                        0xfe82
showKeySymbol XKB_KEY_dead_e = "dead_e"
-- #define XKB_KEY_dead_E                        0xfe83
showKeySymbol XKB_KEY_dead_E = "dead_E"
-- #define XKB_KEY_dead_i                        0xfe84
showKeySymbol XKB_KEY_dead_i = "dead_i"
-- #define XKB_KEY_dead_I                        0xfe85
showKeySymbol XKB_KEY_dead_I = "dead_I"
-- #define XKB_KEY_dead_o                        0xfe86
showKeySymbol XKB_KEY_dead_o = "dead_o"
-- #define XKB_KEY_dead_O                        0xfe87
showKeySymbol XKB_KEY_dead_O = "dead_O"
-- #define XKB_KEY_dead_u                        0xfe88
showKeySymbol XKB_KEY_dead_u = "dead_u"
-- #define XKB_KEY_dead_U                        0xfe89
showKeySymbol XKB_KEY_dead_U = "dead_U"
-- #define XKB_KEY_dead_small_schwa              0xfe8a
showKeySymbol XKB_KEY_dead_small_schwa = "dead_small_schwa"
-- #define XKB_KEY_dead_capital_schwa            0xfe8b
showKeySymbol XKB_KEY_dead_capital_schwa = "dead_capital_schwa"
-- #define XKB_KEY_dead_greek                    0xfe8c
showKeySymbol XKB_KEY_dead_greek = "dead_greek"
-- #define XKB_KEY_First_Virtual_Screen          0xfed0
showKeySymbol XKB_KEY_First_Virtual_Screen = "First_Virtual_Screen"
-- #define XKB_KEY_Prev_Virtual_Screen           0xfed1
showKeySymbol XKB_KEY_Prev_Virtual_Screen = "Prev_Virtual_Screen"
-- #define XKB_KEY_Next_Virtual_Screen           0xfed2
showKeySymbol XKB_KEY_Next_Virtual_Screen = "Next_Virtual_Screen"
-- #define XKB_KEY_Last_Virtual_Screen           0xfed4
showKeySymbol XKB_KEY_Last_Virtual_Screen = "Last_Virtual_Screen"
-- #define XKB_KEY_Terminate_Server              0xfed5
showKeySymbol XKB_KEY_Terminate_Server = "Terminate_Server"
-- #define XKB_KEY_AccessX_Enable                0xfe70
showKeySymbol XKB_KEY_AccessX_Enable = "AccessX_Enable"
-- #define XKB_KEY_AccessX_Feedback_Enable       0xfe71
showKeySymbol XKB_KEY_AccessX_Feedback_Enable = "AccessX_Feedback_Enable"
-- #define XKB_KEY_RepeatKeys_Enable             0xfe72
showKeySymbol XKB_KEY_RepeatKeys_Enable = "RepeatKeys_Enable"
-- #define XKB_KEY_SlowKeys_Enable               0xfe73
showKeySymbol XKB_KEY_SlowKeys_Enable = "SlowKeys_Enable"
-- #define XKB_KEY_BounceKeys_Enable             0xfe74
showKeySymbol XKB_KEY_BounceKeys_Enable = "BounceKeys_Enable"
-- #define XKB_KEY_StickyKeys_Enable             0xfe75
showKeySymbol XKB_KEY_StickyKeys_Enable = "StickyKeys_Enable"
-- #define XKB_KEY_MouseKeys_Enable              0xfe76
showKeySymbol XKB_KEY_MouseKeys_Enable = "MouseKeys_Enable"
-- #define XKB_KEY_MouseKeys_Accel_Enable        0xfe77
showKeySymbol XKB_KEY_MouseKeys_Accel_Enable = "MouseKeys_Accel_Enable"
-- #define XKB_KEY_Overlay1_Enable               0xfe78
showKeySymbol XKB_KEY_Overlay1_Enable = "Overlay1_Enable"
-- #define XKB_KEY_Overlay2_Enable               0xfe79
showKeySymbol XKB_KEY_Overlay2_Enable = "Overlay2_Enable"
-- #define XKB_KEY_AudibleBell_Enable            0xfe7a
showKeySymbol XKB_KEY_AudibleBell_Enable = "AudibleBell_Enable"
-- #define XKB_KEY_Pointer_Left                  0xfee0
showKeySymbol XKB_KEY_Pointer_Left = "Pointer_Left"
-- #define XKB_KEY_Pointer_Right                 0xfee1
showKeySymbol XKB_KEY_Pointer_Right = "Pointer_Right"
-- #define XKB_KEY_Pointer_Up                    0xfee2
showKeySymbol XKB_KEY_Pointer_Up = "Pointer_Up"
-- #define XKB_KEY_Pointer_Down                  0xfee3
showKeySymbol XKB_KEY_Pointer_Down = "Pointer_Down"
-- #define XKB_KEY_Pointer_UpLeft                0xfee4
showKeySymbol XKB_KEY_Pointer_UpLeft = "Pointer_UpLeft"
-- #define XKB_KEY_Pointer_UpRight               0xfee5
showKeySymbol XKB_KEY_Pointer_UpRight = "Pointer_UpRight"
-- #define XKB_KEY_Pointer_DownLeft              0xfee6
showKeySymbol XKB_KEY_Pointer_DownLeft = "Pointer_DownLeft"
-- #define XKB_KEY_Pointer_DownRight             0xfee7
showKeySymbol XKB_KEY_Pointer_DownRight = "Pointer_DownRight"
-- #define XKB_KEY_Pointer_Button_Dflt           0xfee8
showKeySymbol XKB_KEY_Pointer_Button_Dflt = "Pointer_Button_Dflt"
-- #define XKB_KEY_Pointer_Button1               0xfee9
showKeySymbol XKB_KEY_Pointer_Button1 = "Pointer_Button1"
-- #define XKB_KEY_Pointer_Button2               0xfeea
showKeySymbol XKB_KEY_Pointer_Button2 = "Pointer_Button2"
-- #define XKB_KEY_Pointer_Button3               0xfeeb
showKeySymbol XKB_KEY_Pointer_Button3 = "Pointer_Button3"
-- #define XKB_KEY_Pointer_Button4               0xfeec
showKeySymbol XKB_KEY_Pointer_Button4 = "Pointer_Button4"
-- #define XKB_KEY_Pointer_Button5               0xfeed
showKeySymbol XKB_KEY_Pointer_Button5 = "Pointer_Button5"
-- #define XKB_KEY_Pointer_DblClick_Dflt         0xfeee
showKeySymbol XKB_KEY_Pointer_DblClick_Dflt = "Pointer_DblClick_Dflt"
-- #define XKB_KEY_Pointer_DblClick1             0xfeef
showKeySymbol XKB_KEY_Pointer_DblClick1 = "Pointer_DblClick1"
-- #define XKB_KEY_Pointer_DblClick2             0xfef0
showKeySymbol XKB_KEY_Pointer_DblClick2 = "Pointer_DblClick2"
-- #define XKB_KEY_Pointer_DblClick3             0xfef1
showKeySymbol XKB_KEY_Pointer_DblClick3 = "Pointer_DblClick3"
-- #define XKB_KEY_Pointer_DblClick4             0xfef2
showKeySymbol XKB_KEY_Pointer_DblClick4 = "Pointer_DblClick4"
-- #define XKB_KEY_Pointer_DblClick5             0xfef3
showKeySymbol XKB_KEY_Pointer_DblClick5 = "Pointer_DblClick5"
-- #define XKB_KEY_Pointer_Drag_Dflt             0xfef4
showKeySymbol XKB_KEY_Pointer_Drag_Dflt = "Pointer_Drag_Dflt"
-- #define XKB_KEY_Pointer_Drag1                 0xfef5
showKeySymbol XKB_KEY_Pointer_Drag1 = "Pointer_Drag1"
-- #define XKB_KEY_Pointer_Drag2                 0xfef6
showKeySymbol XKB_KEY_Pointer_Drag2 = "Pointer_Drag2"
-- #define XKB_KEY_Pointer_Drag3                 0xfef7
showKeySymbol XKB_KEY_Pointer_Drag3 = "Pointer_Drag3"
-- #define XKB_KEY_Pointer_Drag4                 0xfef8
showKeySymbol XKB_KEY_Pointer_Drag4 = "Pointer_Drag4"
-- #define XKB_KEY_Pointer_Drag5                 0xfefd
showKeySymbol XKB_KEY_Pointer_Drag5 = "Pointer_Drag5"
-- #define XKB_KEY_Pointer_EnableKeys            0xfef9
showKeySymbol XKB_KEY_Pointer_EnableKeys = "Pointer_EnableKeys"
-- #define XKB_KEY_Pointer_Accelerate            0xfefa
showKeySymbol XKB_KEY_Pointer_Accelerate = "Pointer_Accelerate"
-- #define XKB_KEY_Pointer_DfltBtnNext           0xfefb
showKeySymbol XKB_KEY_Pointer_DfltBtnNext = "Pointer_DfltBtnNext"
-- #define XKB_KEY_Pointer_DfltBtnPrev           0xfefc
showKeySymbol XKB_KEY_Pointer_DfltBtnPrev = "Pointer_DfltBtnPrev"
-- #define XKB_KEY_ch                            0xfea0
showKeySymbol XKB_KEY_ch = "ch"
-- #define XKB_KEY_Ch                            0xfea1
showKeySymbol XKB_KEY_Ch = "Ch"
-- #define XKB_KEY_CH                            0xfea2
showKeySymbol XKB_KEY_CH = "CH"
-- #define XKB_KEY_c_h                           0xfea3
showKeySymbol XKB_KEY_c_h = "c_h"
-- #define XKB_KEY_C_h                           0xfea4
showKeySymbol XKB_KEY_C_h = "C_h"
-- #define XKB_KEY_C_H                           0xfea5
showKeySymbol XKB_KEY_C_H = "C_H"
-- #define XKB_KEY_3270_Duplicate                0xfd01
showKeySymbol XKB_KEY_3270_Duplicate = "3270_Duplicate"
-- #define XKB_KEY_3270_FieldMark                0xfd02
showKeySymbol XKB_KEY_3270_FieldMark = "3270_FieldMark"
-- #define XKB_KEY_3270_Right2                   0xfd03
showKeySymbol XKB_KEY_3270_Right2 = "3270_Right2"
-- #define XKB_KEY_3270_Left2                    0xfd04
showKeySymbol XKB_KEY_3270_Left2 = "3270_Left2"
-- #define XKB_KEY_3270_BackTab                  0xfd05
showKeySymbol XKB_KEY_3270_BackTab = "3270_BackTab"
-- #define XKB_KEY_3270_EraseEOF                 0xfd06
showKeySymbol XKB_KEY_3270_EraseEOF = "3270_EraseEOF"
-- #define XKB_KEY_3270_EraseInput               0xfd07
showKeySymbol XKB_KEY_3270_EraseInput = "3270_EraseInput"
-- #define XKB_KEY_3270_Reset                    0xfd08
showKeySymbol XKB_KEY_3270_Reset = "3270_Reset"
-- #define XKB_KEY_3270_Quit                     0xfd09
showKeySymbol XKB_KEY_3270_Quit = "3270_Quit"
-- #define XKB_KEY_3270_PA1                      0xfd0a
showKeySymbol XKB_KEY_3270_PA1 = "3270_PA1"
-- #define XKB_KEY_3270_PA2                      0xfd0b
showKeySymbol XKB_KEY_3270_PA2 = "3270_PA2"
-- #define XKB_KEY_3270_PA3                      0xfd0c
showKeySymbol XKB_KEY_3270_PA3 = "3270_PA3"
-- #define XKB_KEY_3270_Test                     0xfd0d
showKeySymbol XKB_KEY_3270_Test = "3270_Test"
-- #define XKB_KEY_3270_Attn                     0xfd0e
showKeySymbol XKB_KEY_3270_Attn = "3270_Attn"
-- #define XKB_KEY_3270_CursorBlink              0xfd0f
showKeySymbol XKB_KEY_3270_CursorBlink = "3270_CursorBlink"
-- #define XKB_KEY_3270_AltCursor                0xfd10
showKeySymbol XKB_KEY_3270_AltCursor = "3270_AltCursor"
-- #define XKB_KEY_3270_KeyClick                 0xfd11
showKeySymbol XKB_KEY_3270_KeyClick = "3270_KeyClick"
-- #define XKB_KEY_3270_Jump                     0xfd12
showKeySymbol XKB_KEY_3270_Jump = "3270_Jump"
-- #define XKB_KEY_3270_Ident                    0xfd13
showKeySymbol XKB_KEY_3270_Ident = "3270_Ident"
-- #define XKB_KEY_3270_Rule                     0xfd14
showKeySymbol XKB_KEY_3270_Rule = "3270_Rule"
-- #define XKB_KEY_3270_Copy                     0xfd15
showKeySymbol XKB_KEY_3270_Copy = "3270_Copy"
-- #define XKB_KEY_3270_Play                     0xfd16
showKeySymbol XKB_KEY_3270_Play = "3270_Play"
-- #define XKB_KEY_3270_Setup                    0xfd17
showKeySymbol XKB_KEY_3270_Setup = "3270_Setup"
-- #define XKB_KEY_3270_Record                   0xfd18
showKeySymbol XKB_KEY_3270_Record = "3270_Record"
-- #define XKB_KEY_3270_ChangeScreen             0xfd19
showKeySymbol XKB_KEY_3270_ChangeScreen = "3270_ChangeScreen"
-- #define XKB_KEY_3270_DeleteWord               0xfd1a
showKeySymbol XKB_KEY_3270_DeleteWord = "3270_DeleteWord"
-- #define XKB_KEY_3270_ExSelect                 0xfd1b
showKeySymbol XKB_KEY_3270_ExSelect = "3270_ExSelect"
-- #define XKB_KEY_3270_CursorSelect             0xfd1c
showKeySymbol XKB_KEY_3270_CursorSelect = "3270_CursorSelect"
-- #define XKB_KEY_3270_PrintScreen              0xfd1d
showKeySymbol XKB_KEY_3270_PrintScreen = "3270_PrintScreen"
-- #define XKB_KEY_3270_Enter                    0xfd1e
showKeySymbol XKB_KEY_3270_Enter = "3270_Enter"
-- #define XKB_KEY_space                         0x0020  /* U+0020 SPACE */
showKeySymbol XKB_KEY_space = "space"
-- #define XKB_KEY_exclam                        0x0021  /* U+0021 EXCLAMATION MARK */
showKeySymbol XKB_KEY_exclam = "exclam"
-- #define XKB_KEY_quotedbl                      0x0022  /* U+0022 QUOTATION MARK */
showKeySymbol XKB_KEY_quotedbl = "quotedbl"
-- #define XKB_KEY_numbersign                    0x0023  /* U+0023 NUMBER SIGN */
showKeySymbol XKB_KEY_numbersign = "numbersign"
-- #define XKB_KEY_dollar                        0x0024  /* U+0024 DOLLAR SIGN */
showKeySymbol XKB_KEY_dollar = "dollar"
-- #define XKB_KEY_percent                       0x0025  /* U+0025 PERCENT SIGN */
showKeySymbol XKB_KEY_percent = "percent"
-- #define XKB_KEY_ampersand                     0x0026  /* U+0026 AMPERSAND */
showKeySymbol XKB_KEY_ampersand = "ampersand"
-- #define XKB_KEY_apostrophe                    0x0027  /* U+0027 APOSTROPHE */
showKeySymbol XKB_KEY_apostrophe = "apostrophe"
-- #define XKB_KEY_quoteright                    0x0027  /* deprecated */
showKeySymbol XKB_KEY_quoteright = "quoteright"
-- #define XKB_KEY_parenleft                     0x0028  /* U+0028 LEFT PARENTHESIS */
showKeySymbol XKB_KEY_parenleft = "parenleft"
-- #define XKB_KEY_parenright                    0x0029  /* U+0029 RIGHT PARENTHESIS */
showKeySymbol XKB_KEY_parenright = "parenright"
-- #define XKB_KEY_asterisk                      0x002a  /* U+002A ASTERISK */
showKeySymbol XKB_KEY_asterisk = "asterisk"
-- #define XKB_KEY_plus                          0x002b  /* U+002B PLUS SIGN */
showKeySymbol XKB_KEY_plus = "plus"
-- #define XKB_KEY_comma                         0x002c  /* U+002C COMMA */
showKeySymbol XKB_KEY_comma = "comma"
-- #define XKB_KEY_minus                         0x002d  /* U+002D HYPHEN-MINUS */
showKeySymbol XKB_KEY_minus = "minus"
-- #define XKB_KEY_period                        0x002e  /* U+002E FULL STOP */
showKeySymbol XKB_KEY_period = "period"
-- #define XKB_KEY_slash                         0x002f  /* U+002F SOLIDUS */
showKeySymbol XKB_KEY_slash = "slash"
-- #define XKB_KEY_0                             0x0030  /* U+0030 DIGIT ZERO */
showKeySymbol XKB_KEY_0 = "0"
-- #define XKB_KEY_1                             0x0031  /* U+0031 DIGIT ONE */
showKeySymbol XKB_KEY_1 = "1"
-- #define XKB_KEY_2                             0x0032  /* U+0032 DIGIT TWO */
showKeySymbol XKB_KEY_2 = "2"
-- #define XKB_KEY_3                             0x0033  /* U+0033 DIGIT THREE */
showKeySymbol XKB_KEY_3 = "3"
-- #define XKB_KEY_4                             0x0034  /* U+0034 DIGIT FOUR */
showKeySymbol XKB_KEY_4 = "4"
-- #define XKB_KEY_5                             0x0035  /* U+0035 DIGIT FIVE */
showKeySymbol XKB_KEY_5 = "5"
-- #define XKB_KEY_6                             0x0036  /* U+0036 DIGIT SIX */
showKeySymbol XKB_KEY_6 = "6"
-- #define XKB_KEY_7                             0x0037  /* U+0037 DIGIT SEVEN */
showKeySymbol XKB_KEY_7 = "7"
-- #define XKB_KEY_8                             0x0038  /* U+0038 DIGIT EIGHT */
showKeySymbol XKB_KEY_8 = "8"
-- #define XKB_KEY_9                             0x0039  /* U+0039 DIGIT NINE */
showKeySymbol XKB_KEY_9 = "9"
-- #define XKB_KEY_colon                         0x003a  /* U+003A COLON */
showKeySymbol XKB_KEY_colon = "colon"
-- #define XKB_KEY_semicolon                     0x003b  /* U+003B SEMICOLON */
showKeySymbol XKB_KEY_semicolon = "semicolon"
-- #define XKB_KEY_less                          0x003c  /* U+003C LESS-THAN SIGN */
showKeySymbol XKB_KEY_less = "less"
-- #define XKB_KEY_equal                         0x003d  /* U+003D EQUALS SIGN */
showKeySymbol XKB_KEY_equal = "equal"
-- #define XKB_KEY_greater                       0x003e  /* U+003E GREATER-THAN SIGN */
showKeySymbol XKB_KEY_greater = "greater"
-- #define XKB_KEY_question                      0x003f  /* U+003F QUESTION MARK */
showKeySymbol XKB_KEY_question = "question"
-- #define XKB_KEY_at                            0x0040  /* U+0040 COMMERCIAL AT */
showKeySymbol XKB_KEY_at = "at"
-- #define XKB_KEY_A                             0x0041  /* U+0041 LATIN CAPITAL LETTER A */
showKeySymbol XKB_KEY_A = "A"
-- #define XKB_KEY_B                             0x0042  /* U+0042 LATIN CAPITAL LETTER B */
showKeySymbol XKB_KEY_B = "B"
-- #define XKB_KEY_C                             0x0043  /* U+0043 LATIN CAPITAL LETTER C */
showKeySymbol XKB_KEY_C = "C"
-- #define XKB_KEY_D                             0x0044  /* U+0044 LATIN CAPITAL LETTER D */
showKeySymbol XKB_KEY_D = "D"
-- #define XKB_KEY_E                             0x0045  /* U+0045 LATIN CAPITAL LETTER E */
showKeySymbol XKB_KEY_E = "E"
-- #define XKB_KEY_F                             0x0046  /* U+0046 LATIN CAPITAL LETTER F */
showKeySymbol XKB_KEY_F = "F"
-- #define XKB_KEY_G                             0x0047  /* U+0047 LATIN CAPITAL LETTER G */
showKeySymbol XKB_KEY_G = "G"
-- #define XKB_KEY_H                             0x0048  /* U+0048 LATIN CAPITAL LETTER H */
showKeySymbol XKB_KEY_H = "H"
-- #define XKB_KEY_I                             0x0049  /* U+0049 LATIN CAPITAL LETTER I */
showKeySymbol XKB_KEY_I = "I"
-- #define XKB_KEY_J                             0x004a  /* U+004A LATIN CAPITAL LETTER J */
showKeySymbol XKB_KEY_J = "J"
-- #define XKB_KEY_K                             0x004b  /* U+004B LATIN CAPITAL LETTER K */
showKeySymbol XKB_KEY_K = "K"
-- #define XKB_KEY_L                             0x004c  /* U+004C LATIN CAPITAL LETTER L */
showKeySymbol XKB_KEY_L = "L"
-- #define XKB_KEY_M                             0x004d  /* U+004D LATIN CAPITAL LETTER M */
showKeySymbol XKB_KEY_M = "M"
-- #define XKB_KEY_N                             0x004e  /* U+004E LATIN CAPITAL LETTER N */
showKeySymbol XKB_KEY_N = "N"
-- #define XKB_KEY_O                             0x004f  /* U+004F LATIN CAPITAL LETTER O */
showKeySymbol XKB_KEY_O = "O"
-- #define XKB_KEY_P                             0x0050  /* U+0050 LATIN CAPITAL LETTER P */
showKeySymbol XKB_KEY_P = "P"
-- #define XKB_KEY_Q                             0x0051  /* U+0051 LATIN CAPITAL LETTER Q */
showKeySymbol XKB_KEY_Q = "Q"
-- #define XKB_KEY_R                             0x0052  /* U+0052 LATIN CAPITAL LETTER R */
showKeySymbol XKB_KEY_R = "R"
-- #define XKB_KEY_S                             0x0053  /* U+0053 LATIN CAPITAL LETTER S */
showKeySymbol XKB_KEY_S = "S"
-- #define XKB_KEY_T                             0x0054  /* U+0054 LATIN CAPITAL LETTER T */
showKeySymbol XKB_KEY_T = "T"
-- #define XKB_KEY_U                             0x0055  /* U+0055 LATIN CAPITAL LETTER U */
showKeySymbol XKB_KEY_U = "U"
-- #define XKB_KEY_V                             0x0056  /* U+0056 LATIN CAPITAL LETTER V */
showKeySymbol XKB_KEY_V = "V"
-- #define XKB_KEY_W                             0x0057  /* U+0057 LATIN CAPITAL LETTER W */
showKeySymbol XKB_KEY_W = "W"
-- #define XKB_KEY_X                             0x0058  /* U+0058 LATIN CAPITAL LETTER X */
showKeySymbol XKB_KEY_X = "X"
-- #define XKB_KEY_Y                             0x0059  /* U+0059 LATIN CAPITAL LETTER Y */
showKeySymbol XKB_KEY_Y = "Y"
-- #define XKB_KEY_Z                             0x005a  /* U+005A LATIN CAPITAL LETTER Z */
showKeySymbol XKB_KEY_Z = "Z"
-- #define XKB_KEY_bracketleft                   0x005b  /* U+005B LEFT SQUARE BRACKET */
showKeySymbol XKB_KEY_bracketleft = "bracketleft"
-- #define XKB_KEY_backslash                     0x005c  /* U+005C REVERSE SOLIDUS */
showKeySymbol XKB_KEY_backslash = "backslash"
-- #define XKB_KEY_bracketright                  0x005d  /* U+005D RIGHT SQUARE BRACKET */
showKeySymbol XKB_KEY_bracketright = "bracketright"
-- #define XKB_KEY_asciicircum                   0x005e  /* U+005E CIRCUMFLEX ACCENT */
showKeySymbol XKB_KEY_asciicircum = "asciicircum"
-- #define XKB_KEY_underscore                    0x005f  /* U+005F LOW LINE */
showKeySymbol XKB_KEY_underscore = "underscore"
-- #define XKB_KEY_grave                         0x0060  /* U+0060 GRAVE ACCENT */
showKeySymbol XKB_KEY_grave = "grave"
-- #define XKB_KEY_quoteleft                     0x0060  /* deprecated */
showKeySymbol XKB_KEY_quoteleft = "quoteleft"
-- #define XKB_KEY_a                             0x0061  /* U+0061 LATIN SMALL LETTER A */
showKeySymbol XKB_KEY_a = "a"
-- #define XKB_KEY_b                             0x0062  /* U+0062 LATIN SMALL LETTER B */
showKeySymbol XKB_KEY_b = "b"
-- #define XKB_KEY_c                             0x0063  /* U+0063 LATIN SMALL LETTER C */
showKeySymbol XKB_KEY_c = "c"
-- #define XKB_KEY_d                             0x0064  /* U+0064 LATIN SMALL LETTER D */
showKeySymbol XKB_KEY_d = "d"
-- #define XKB_KEY_e                             0x0065  /* U+0065 LATIN SMALL LETTER E */
showKeySymbol XKB_KEY_e = "e"
-- #define XKB_KEY_f                             0x0066  /* U+0066 LATIN SMALL LETTER F */
showKeySymbol XKB_KEY_f = "f"
-- #define XKB_KEY_g                             0x0067  /* U+0067 LATIN SMALL LETTER G */
showKeySymbol XKB_KEY_g = "g"
-- #define XKB_KEY_h                             0x0068  /* U+0068 LATIN SMALL LETTER H */
showKeySymbol XKB_KEY_h = "h"
-- #define XKB_KEY_i                             0x0069  /* U+0069 LATIN SMALL LETTER I */
showKeySymbol XKB_KEY_i = "i"
-- #define XKB_KEY_j                             0x006a  /* U+006A LATIN SMALL LETTER J */
showKeySymbol XKB_KEY_j = "j"
-- #define XKB_KEY_k                             0x006b  /* U+006B LATIN SMALL LETTER K */
showKeySymbol XKB_KEY_k = "k"
-- #define XKB_KEY_l                             0x006c  /* U+006C LATIN SMALL LETTER L */
showKeySymbol XKB_KEY_l = "l"
-- #define XKB_KEY_m                             0x006d  /* U+006D LATIN SMALL LETTER M */
showKeySymbol XKB_KEY_m = "m"
-- #define XKB_KEY_n                             0x006e  /* U+006E LATIN SMALL LETTER N */
showKeySymbol XKB_KEY_n = "n"
-- #define XKB_KEY_o                             0x006f  /* U+006F LATIN SMALL LETTER O */
showKeySymbol XKB_KEY_o = "o"
-- #define XKB_KEY_p                             0x0070  /* U+0070 LATIN SMALL LETTER P */
showKeySymbol XKB_KEY_p = "p"
-- #define XKB_KEY_q                             0x0071  /* U+0071 LATIN SMALL LETTER Q */
showKeySymbol XKB_KEY_q = "q"
-- #define XKB_KEY_r                             0x0072  /* U+0072 LATIN SMALL LETTER R */
showKeySymbol XKB_KEY_r = "r"
-- #define XKB_KEY_s                             0x0073  /* U+0073 LATIN SMALL LETTER S */
showKeySymbol XKB_KEY_s = "s"
-- #define XKB_KEY_t                             0x0074  /* U+0074 LATIN SMALL LETTER T */
showKeySymbol XKB_KEY_t = "t"
-- #define XKB_KEY_u                             0x0075  /* U+0075 LATIN SMALL LETTER U */
showKeySymbol XKB_KEY_u = "u"
-- #define XKB_KEY_v                             0x0076  /* U+0076 LATIN SMALL LETTER V */
showKeySymbol XKB_KEY_v = "v"
-- #define XKB_KEY_w                             0x0077  /* U+0077 LATIN SMALL LETTER W */
showKeySymbol XKB_KEY_w = "w"
-- #define XKB_KEY_x                             0x0078  /* U+0078 LATIN SMALL LETTER X */
showKeySymbol XKB_KEY_x = "x"
-- #define XKB_KEY_y                             0x0079  /* U+0079 LATIN SMALL LETTER Y */
showKeySymbol XKB_KEY_y = "y"
-- #define XKB_KEY_z                             0x007a  /* U+007A LATIN SMALL LETTER Z */
showKeySymbol XKB_KEY_z = "z"
-- #define XKB_KEY_braceleft                     0x007b  /* U+007B LEFT CURLY BRACKET */
showKeySymbol XKB_KEY_braceleft = "braceleft"
-- #define XKB_KEY_bar                           0x007c  /* U+007C VERTICAL LINE */
showKeySymbol XKB_KEY_bar = "bar"
-- #define XKB_KEY_braceright                    0x007d  /* U+007D RIGHT CURLY BRACKET */
showKeySymbol XKB_KEY_braceright = "braceright"
-- #define XKB_KEY_asciitilde                    0x007e  /* U+007E TILDE */
showKeySymbol XKB_KEY_asciitilde = "asciitilde"
-- #define XKB_KEY_nobreakspace                  0x00a0  /* U+00A0 NO-BREAK SPACE */
showKeySymbol XKB_KEY_nobreakspace = "nobreakspace"
-- #define XKB_KEY_exclamdown                    0x00a1  /* U+00A1 INVERTED EXCLAMATION MARK */
showKeySymbol XKB_KEY_exclamdown = "exclamdown"
-- #define XKB_KEY_cent                          0x00a2  /* U+00A2 CENT SIGN */
showKeySymbol XKB_KEY_cent = "cent"
-- #define XKB_KEY_sterling                      0x00a3  /* U+00A3 POUND SIGN */
showKeySymbol XKB_KEY_sterling = "sterling"
-- #define XKB_KEY_currency                      0x00a4  /* U+00A4 CURRENCY SIGN */
showKeySymbol XKB_KEY_currency = "currency"
-- #define XKB_KEY_yen                           0x00a5  /* U+00A5 YEN SIGN */
showKeySymbol XKB_KEY_yen = "yen"
-- #define XKB_KEY_brokenbar                     0x00a6  /* U+00A6 BROKEN BAR */
showKeySymbol XKB_KEY_brokenbar = "brokenbar"
-- #define XKB_KEY_section                       0x00a7  /* U+00A7 SECTION SIGN */
showKeySymbol XKB_KEY_section = "section"
-- #define XKB_KEY_diaeresis                     0x00a8  /* U+00A8 DIAERESIS */
showKeySymbol XKB_KEY_diaeresis = "diaeresis"
-- #define XKB_KEY_copyright                     0x00a9  /* U+00A9 COPYRIGHT SIGN */
showKeySymbol XKB_KEY_copyright = "copyright"
-- #define XKB_KEY_ordfeminine                   0x00aa  /* U+00AA FEMININE ORDINAL INDICATOR */
showKeySymbol XKB_KEY_ordfeminine = "ordfeminine"
-- #define XKB_KEY_guillemotleft                 0x00ab  /* U+00AB LEFT-POINTING DOUBLE ANGLE QUOTATION MARK */
showKeySymbol XKB_KEY_guillemotleft = "guillemotleft"
-- #define XKB_KEY_notsign                       0x00ac  /* U+00AC NOT SIGN */
showKeySymbol XKB_KEY_notsign = "notsign"
-- #define XKB_KEY_hyphen                        0x00ad  /* U+00AD SOFT HYPHEN */
showKeySymbol XKB_KEY_hyphen = "hyphen"
-- #define XKB_KEY_registered                    0x00ae  /* U+00AE REGISTERED SIGN */
showKeySymbol XKB_KEY_registered = "registered"
-- #define XKB_KEY_macron                        0x00af  /* U+00AF MACRON */
showKeySymbol XKB_KEY_macron = "macron"
-- #define XKB_KEY_degree                        0x00b0  /* U+00B0 DEGREE SIGN */
showKeySymbol XKB_KEY_degree = "degree"
-- #define XKB_KEY_plusminus                     0x00b1  /* U+00B1 PLUS-MINUS SIGN */
showKeySymbol XKB_KEY_plusminus = "plusminus"
-- #define XKB_KEY_twosuperior                   0x00b2  /* U+00B2 SUPERSCRIPT TWO */
showKeySymbol XKB_KEY_twosuperior = "twosuperior"
-- #define XKB_KEY_threesuperior                 0x00b3  /* U+00B3 SUPERSCRIPT THREE */
showKeySymbol XKB_KEY_threesuperior = "threesuperior"
-- #define XKB_KEY_acute                         0x00b4  /* U+00B4 ACUTE ACCENT */
showKeySymbol XKB_KEY_acute = "acute"
-- #define XKB_KEY_mu                            0x00b5  /* U+00B5 MICRO SIGN */
showKeySymbol XKB_KEY_mu = "mu"
-- #define XKB_KEY_paragraph                     0x00b6  /* U+00B6 PILCROW SIGN */
showKeySymbol XKB_KEY_paragraph = "paragraph"
-- #define XKB_KEY_periodcentered                0x00b7  /* U+00B7 MIDDLE DOT */
showKeySymbol XKB_KEY_periodcentered = "periodcentered"
-- #define XKB_KEY_cedilla                       0x00b8  /* U+00B8 CEDILLA */
showKeySymbol XKB_KEY_cedilla = "cedilla"
-- #define XKB_KEY_onesuperior                   0x00b9  /* U+00B9 SUPERSCRIPT ONE */
showKeySymbol XKB_KEY_onesuperior = "onesuperior"
-- #define XKB_KEY_masculine                     0x00ba  /* U+00BA MASCULINE ORDINAL INDICATOR */
showKeySymbol XKB_KEY_masculine = "masculine"
-- #define XKB_KEY_guillemotright                0x00bb  /* U+00BB RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK */
showKeySymbol XKB_KEY_guillemotright = "guillemotright"
-- #define XKB_KEY_onequarter                    0x00bc  /* U+00BC VULGAR FRACTION ONE QUARTER */
showKeySymbol XKB_KEY_onequarter = "onequarter"
-- #define XKB_KEY_onehalf                       0x00bd  /* U+00BD VULGAR FRACTION ONE HALF */
showKeySymbol XKB_KEY_onehalf = "onehalf"
-- #define XKB_KEY_threequarters                 0x00be  /* U+00BE VULGAR FRACTION THREE QUARTERS */
showKeySymbol XKB_KEY_threequarters = "threequarters"
-- #define XKB_KEY_questiondown                  0x00bf  /* U+00BF INVERTED QUESTION MARK */
showKeySymbol XKB_KEY_questiondown = "questiondown"
-- #define XKB_KEY_Agrave                        0x00c0  /* U+00C0 LATIN CAPITAL LETTER A WITH GRAVE */
showKeySymbol XKB_KEY_Agrave = "Agrave"
-- #define XKB_KEY_Aacute                        0x00c1  /* U+00C1 LATIN CAPITAL LETTER A WITH ACUTE */
showKeySymbol XKB_KEY_Aacute = "Aacute"
-- #define XKB_KEY_Acircumflex                   0x00c2  /* U+00C2 LATIN CAPITAL LETTER A WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_Acircumflex = "Acircumflex"
-- #define XKB_KEY_Atilde                        0x00c3  /* U+00C3 LATIN CAPITAL LETTER A WITH TILDE */
showKeySymbol XKB_KEY_Atilde = "Atilde"
-- #define XKB_KEY_Adiaeresis                    0x00c4  /* U+00C4 LATIN CAPITAL LETTER A WITH DIAERESIS */
showKeySymbol XKB_KEY_Adiaeresis = "Adiaeresis"
-- #define XKB_KEY_Aring                         0x00c5  /* U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE */
showKeySymbol XKB_KEY_Aring = "Aring"
-- #define XKB_KEY_AE                            0x00c6  /* U+00C6 LATIN CAPITAL LETTER AE */
showKeySymbol XKB_KEY_AE = "AE"
-- #define XKB_KEY_Ccedilla                      0x00c7  /* U+00C7 LATIN CAPITAL LETTER C WITH CEDILLA */
showKeySymbol XKB_KEY_Ccedilla = "Ccedilla"
-- #define XKB_KEY_Egrave                        0x00c8  /* U+00C8 LATIN CAPITAL LETTER E WITH GRAVE */
showKeySymbol XKB_KEY_Egrave = "Egrave"
-- #define XKB_KEY_Eacute                        0x00c9  /* U+00C9 LATIN CAPITAL LETTER E WITH ACUTE */
showKeySymbol XKB_KEY_Eacute = "Eacute"
-- #define XKB_KEY_Ecircumflex                   0x00ca  /* U+00CA LATIN CAPITAL LETTER E WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_Ecircumflex = "Ecircumflex"
-- #define XKB_KEY_Ediaeresis                    0x00cb  /* U+00CB LATIN CAPITAL LETTER E WITH DIAERESIS */
showKeySymbol XKB_KEY_Ediaeresis = "Ediaeresis"
-- #define XKB_KEY_Igrave                        0x00cc  /* U+00CC LATIN CAPITAL LETTER I WITH GRAVE */
showKeySymbol XKB_KEY_Igrave = "Igrave"
-- #define XKB_KEY_Iacute                        0x00cd  /* U+00CD LATIN CAPITAL LETTER I WITH ACUTE */
showKeySymbol XKB_KEY_Iacute = "Iacute"
-- #define XKB_KEY_Icircumflex                   0x00ce  /* U+00CE LATIN CAPITAL LETTER I WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_Icircumflex = "Icircumflex"
-- #define XKB_KEY_Idiaeresis                    0x00cf  /* U+00CF LATIN CAPITAL LETTER I WITH DIAERESIS */
showKeySymbol XKB_KEY_Idiaeresis = "Idiaeresis"
-- #define XKB_KEY_ETH                           0x00d0  /* U+00D0 LATIN CAPITAL LETTER ETH */
showKeySymbol XKB_KEY_ETH = "ETH"
-- #define XKB_KEY_Eth                           0x00d0  /* deprecated */
showKeySymbol XKB_KEY_Eth = "Eth"
-- #define XKB_KEY_Ntilde                        0x00d1  /* U+00D1 LATIN CAPITAL LETTER N WITH TILDE */
showKeySymbol XKB_KEY_Ntilde = "Ntilde"
-- #define XKB_KEY_Ograve                        0x00d2  /* U+00D2 LATIN CAPITAL LETTER O WITH GRAVE */
showKeySymbol XKB_KEY_Ograve = "Ograve"
-- #define XKB_KEY_Oacute                        0x00d3  /* U+00D3 LATIN CAPITAL LETTER O WITH ACUTE */
showKeySymbol XKB_KEY_Oacute = "Oacute"
-- #define XKB_KEY_Ocircumflex                   0x00d4  /* U+00D4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_Ocircumflex = "Ocircumflex"
-- #define XKB_KEY_Otilde                        0x00d5  /* U+00D5 LATIN CAPITAL LETTER O WITH TILDE */
showKeySymbol XKB_KEY_Otilde = "Otilde"
-- #define XKB_KEY_Odiaeresis                    0x00d6  /* U+00D6 LATIN CAPITAL LETTER O WITH DIAERESIS */
showKeySymbol XKB_KEY_Odiaeresis = "Odiaeresis"
-- #define XKB_KEY_multiply                      0x00d7  /* U+00D7 MULTIPLICATION SIGN */
showKeySymbol XKB_KEY_multiply = "multiply"
-- #define XKB_KEY_Oslash                        0x00d8  /* U+00D8 LATIN CAPITAL LETTER O WITH STROKE */
showKeySymbol XKB_KEY_Oslash = "Oslash"
-- #define XKB_KEY_Ooblique                      0x00d8  /* U+00D8 LATIN CAPITAL LETTER O WITH STROKE */
showKeySymbol XKB_KEY_Ooblique = "Ooblique"
-- #define XKB_KEY_Ugrave                        0x00d9  /* U+00D9 LATIN CAPITAL LETTER U WITH GRAVE */
showKeySymbol XKB_KEY_Ugrave = "Ugrave"
-- #define XKB_KEY_Uacute                        0x00da  /* U+00DA LATIN CAPITAL LETTER U WITH ACUTE */
showKeySymbol XKB_KEY_Uacute = "Uacute"
-- #define XKB_KEY_Ucircumflex                   0x00db  /* U+00DB LATIN CAPITAL LETTER U WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_Ucircumflex = "Ucircumflex"
-- #define XKB_KEY_Udiaeresis                    0x00dc  /* U+00DC LATIN CAPITAL LETTER U WITH DIAERESIS */
showKeySymbol XKB_KEY_Udiaeresis = "Udiaeresis"
-- #define XKB_KEY_Yacute                        0x00dd  /* U+00DD LATIN CAPITAL LETTER Y WITH ACUTE */
showKeySymbol XKB_KEY_Yacute = "Yacute"
-- #define XKB_KEY_THORN                         0x00de  /* U+00DE LATIN CAPITAL LETTER THORN */
showKeySymbol XKB_KEY_THORN = "THORN"
-- #define XKB_KEY_Thorn                         0x00de  /* deprecated */
showKeySymbol XKB_KEY_Thorn = "Thorn"
-- #define XKB_KEY_ssharp                        0x00df  /* U+00DF LATIN SMALL LETTER SHARP S */
showKeySymbol XKB_KEY_ssharp = "ssharp"
-- #define XKB_KEY_agrave                        0x00e0  /* U+00E0 LATIN SMALL LETTER A WITH GRAVE */
showKeySymbol XKB_KEY_agrave = "agrave"
-- #define XKB_KEY_aacute                        0x00e1  /* U+00E1 LATIN SMALL LETTER A WITH ACUTE */
showKeySymbol XKB_KEY_aacute = "aacute"
-- #define XKB_KEY_acircumflex                   0x00e2  /* U+00E2 LATIN SMALL LETTER A WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_acircumflex = "acircumflex"
-- #define XKB_KEY_atilde                        0x00e3  /* U+00E3 LATIN SMALL LETTER A WITH TILDE */
showKeySymbol XKB_KEY_atilde = "atilde"
-- #define XKB_KEY_adiaeresis                    0x00e4  /* U+00E4 LATIN SMALL LETTER A WITH DIAERESIS */
showKeySymbol XKB_KEY_adiaeresis = "adiaeresis"
-- #define XKB_KEY_aring                         0x00e5  /* U+00E5 LATIN SMALL LETTER A WITH RING ABOVE */
showKeySymbol XKB_KEY_aring = "aring"
-- #define XKB_KEY_ae                            0x00e6  /* U+00E6 LATIN SMALL LETTER AE */
showKeySymbol XKB_KEY_ae = "ae"
-- #define XKB_KEY_ccedilla                      0x00e7  /* U+00E7 LATIN SMALL LETTER C WITH CEDILLA */
showKeySymbol XKB_KEY_ccedilla = "ccedilla"
-- #define XKB_KEY_egrave                        0x00e8  /* U+00E8 LATIN SMALL LETTER E WITH GRAVE */
showKeySymbol XKB_KEY_egrave = "egrave"
-- #define XKB_KEY_eacute                        0x00e9  /* U+00E9 LATIN SMALL LETTER E WITH ACUTE */
showKeySymbol XKB_KEY_eacute = "eacute"
-- #define XKB_KEY_ecircumflex                   0x00ea  /* U+00EA LATIN SMALL LETTER E WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_ecircumflex = "ecircumflex"
-- #define XKB_KEY_ediaeresis                    0x00eb  /* U+00EB LATIN SMALL LETTER E WITH DIAERESIS */
showKeySymbol XKB_KEY_ediaeresis = "ediaeresis"
-- #define XKB_KEY_igrave                        0x00ec  /* U+00EC LATIN SMALL LETTER I WITH GRAVE */
showKeySymbol XKB_KEY_igrave = "igrave"
-- #define XKB_KEY_iacute                        0x00ed  /* U+00ED LATIN SMALL LETTER I WITH ACUTE */
showKeySymbol XKB_KEY_iacute = "iacute"
-- #define XKB_KEY_icircumflex                   0x00ee  /* U+00EE LATIN SMALL LETTER I WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_icircumflex = "icircumflex"
-- #define XKB_KEY_idiaeresis                    0x00ef  /* U+00EF LATIN SMALL LETTER I WITH DIAERESIS */
showKeySymbol XKB_KEY_idiaeresis = "idiaeresis"
-- #define XKB_KEY_eth                           0x00f0  /* U+00F0 LATIN SMALL LETTER ETH */
showKeySymbol XKB_KEY_eth = "eth"
-- #define XKB_KEY_ntilde                        0x00f1  /* U+00F1 LATIN SMALL LETTER N WITH TILDE */
showKeySymbol XKB_KEY_ntilde = "ntilde"
-- #define XKB_KEY_ograve                        0x00f2  /* U+00F2 LATIN SMALL LETTER O WITH GRAVE */
showKeySymbol XKB_KEY_ograve = "ograve"
-- #define XKB_KEY_oacute                        0x00f3  /* U+00F3 LATIN SMALL LETTER O WITH ACUTE */
showKeySymbol XKB_KEY_oacute = "oacute"
-- #define XKB_KEY_ocircumflex                   0x00f4  /* U+00F4 LATIN SMALL LETTER O WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_ocircumflex = "ocircumflex"
-- #define XKB_KEY_otilde                        0x00f5  /* U+00F5 LATIN SMALL LETTER O WITH TILDE */
showKeySymbol XKB_KEY_otilde = "otilde"
-- #define XKB_KEY_odiaeresis                    0x00f6  /* U+00F6 LATIN SMALL LETTER O WITH DIAERESIS */
showKeySymbol XKB_KEY_odiaeresis = "odiaeresis"
-- #define XKB_KEY_division                      0x00f7  /* U+00F7 DIVISION SIGN */
showKeySymbol XKB_KEY_division = "division"
-- #define XKB_KEY_oslash                        0x00f8  /* U+00F8 LATIN SMALL LETTER O WITH STROKE */
showKeySymbol XKB_KEY_oslash = "oslash"
-- #define XKB_KEY_ooblique                      0x00f8  /* U+00F8 LATIN SMALL LETTER O WITH STROKE */
showKeySymbol XKB_KEY_ooblique = "ooblique"
-- #define XKB_KEY_ugrave                        0x00f9  /* U+00F9 LATIN SMALL LETTER U WITH GRAVE */
showKeySymbol XKB_KEY_ugrave = "ugrave"
-- #define XKB_KEY_uacute                        0x00fa  /* U+00FA LATIN SMALL LETTER U WITH ACUTE */
showKeySymbol XKB_KEY_uacute = "uacute"
-- #define XKB_KEY_ucircumflex                   0x00fb  /* U+00FB LATIN SMALL LETTER U WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_ucircumflex = "ucircumflex"
-- #define XKB_KEY_udiaeresis                    0x00fc  /* U+00FC LATIN SMALL LETTER U WITH DIAERESIS */
showKeySymbol XKB_KEY_udiaeresis = "udiaeresis"
-- #define XKB_KEY_yacute                        0x00fd  /* U+00FD LATIN SMALL LETTER Y WITH ACUTE */
showKeySymbol XKB_KEY_yacute = "yacute"
-- #define XKB_KEY_thorn                         0x00fe  /* U+00FE LATIN SMALL LETTER THORN */
showKeySymbol XKB_KEY_thorn = "thorn"
-- #define XKB_KEY_ydiaeresis                    0x00ff  /* U+00FF LATIN SMALL LETTER Y WITH DIAERESIS */
showKeySymbol XKB_KEY_ydiaeresis = "ydiaeresis"
-- #define XKB_KEY_Aogonek                       0x01a1  /* U+0104 LATIN CAPITAL LETTER A WITH OGONEK */
showKeySymbol XKB_KEY_Aogonek = "Aogonek"
-- #define XKB_KEY_breve                         0x01a2  /* U+02D8 BREVE */
showKeySymbol XKB_KEY_breve = "breve"
-- #define XKB_KEY_Lstroke                       0x01a3  /* U+0141 LATIN CAPITAL LETTER L WITH STROKE */
showKeySymbol XKB_KEY_Lstroke = "Lstroke"
-- #define XKB_KEY_Lcaron                        0x01a5  /* U+013D LATIN CAPITAL LETTER L WITH CARON */
showKeySymbol XKB_KEY_Lcaron = "Lcaron"
-- #define XKB_KEY_Sacute                        0x01a6  /* U+015A LATIN CAPITAL LETTER S WITH ACUTE */
showKeySymbol XKB_KEY_Sacute = "Sacute"
-- #define XKB_KEY_Scaron                        0x01a9  /* U+0160 LATIN CAPITAL LETTER S WITH CARON */
showKeySymbol XKB_KEY_Scaron = "Scaron"
-- #define XKB_KEY_Scedilla                      0x01aa  /* U+015E LATIN CAPITAL LETTER S WITH CEDILLA */
showKeySymbol XKB_KEY_Scedilla = "Scedilla"
-- #define XKB_KEY_Tcaron                        0x01ab  /* U+0164 LATIN CAPITAL LETTER T WITH CARON */
showKeySymbol XKB_KEY_Tcaron = "Tcaron"
-- #define XKB_KEY_Zacute                        0x01ac  /* U+0179 LATIN CAPITAL LETTER Z WITH ACUTE */
showKeySymbol XKB_KEY_Zacute = "Zacute"
-- #define XKB_KEY_Zcaron                        0x01ae  /* U+017D LATIN CAPITAL LETTER Z WITH CARON */
showKeySymbol XKB_KEY_Zcaron = "Zcaron"
-- #define XKB_KEY_Zabovedot                     0x01af  /* U+017B LATIN CAPITAL LETTER Z WITH DOT ABOVE */
showKeySymbol XKB_KEY_Zabovedot = "Zabovedot"
-- #define XKB_KEY_aogonek                       0x01b1  /* U+0105 LATIN SMALL LETTER A WITH OGONEK */
showKeySymbol XKB_KEY_aogonek = "aogonek"
-- #define XKB_KEY_ogonek                        0x01b2  /* U+02DB OGONEK */
showKeySymbol XKB_KEY_ogonek = "ogonek"
-- #define XKB_KEY_lstroke                       0x01b3  /* U+0142 LATIN SMALL LETTER L WITH STROKE */
showKeySymbol XKB_KEY_lstroke = "lstroke"
-- #define XKB_KEY_lcaron                        0x01b5  /* U+013E LATIN SMALL LETTER L WITH CARON */
showKeySymbol XKB_KEY_lcaron = "lcaron"
-- #define XKB_KEY_sacute                        0x01b6  /* U+015B LATIN SMALL LETTER S WITH ACUTE */
showKeySymbol XKB_KEY_sacute = "sacute"
-- #define XKB_KEY_caron                         0x01b7  /* U+02C7 CARON */
showKeySymbol XKB_KEY_caron = "caron"
-- #define XKB_KEY_scaron                        0x01b9  /* U+0161 LATIN SMALL LETTER S WITH CARON */
showKeySymbol XKB_KEY_scaron = "scaron"
-- #define XKB_KEY_scedilla                      0x01ba  /* U+015F LATIN SMALL LETTER S WITH CEDILLA */
showKeySymbol XKB_KEY_scedilla = "scedilla"
-- #define XKB_KEY_tcaron                        0x01bb  /* U+0165 LATIN SMALL LETTER T WITH CARON */
showKeySymbol XKB_KEY_tcaron = "tcaron"
-- #define XKB_KEY_zacute                        0x01bc  /* U+017A LATIN SMALL LETTER Z WITH ACUTE */
showKeySymbol XKB_KEY_zacute = "zacute"
-- #define XKB_KEY_doubleacute                   0x01bd  /* U+02DD DOUBLE ACUTE ACCENT */
showKeySymbol XKB_KEY_doubleacute = "doubleacute"
-- #define XKB_KEY_zcaron                        0x01be  /* U+017E LATIN SMALL LETTER Z WITH CARON */
showKeySymbol XKB_KEY_zcaron = "zcaron"
-- #define XKB_KEY_zabovedot                     0x01bf  /* U+017C LATIN SMALL LETTER Z WITH DOT ABOVE */
showKeySymbol XKB_KEY_zabovedot = "zabovedot"
-- #define XKB_KEY_Racute                        0x01c0  /* U+0154 LATIN CAPITAL LETTER R WITH ACUTE */
showKeySymbol XKB_KEY_Racute = "Racute"
-- #define XKB_KEY_Abreve                        0x01c3  /* U+0102 LATIN CAPITAL LETTER A WITH BREVE */
showKeySymbol XKB_KEY_Abreve = "Abreve"
-- #define XKB_KEY_Lacute                        0x01c5  /* U+0139 LATIN CAPITAL LETTER L WITH ACUTE */
showKeySymbol XKB_KEY_Lacute = "Lacute"
-- #define XKB_KEY_Cacute                        0x01c6  /* U+0106 LATIN CAPITAL LETTER C WITH ACUTE */
showKeySymbol XKB_KEY_Cacute = "Cacute"
-- #define XKB_KEY_Ccaron                        0x01c8  /* U+010C LATIN CAPITAL LETTER C WITH CARON */
showKeySymbol XKB_KEY_Ccaron = "Ccaron"
-- #define XKB_KEY_Eogonek                       0x01ca  /* U+0118 LATIN CAPITAL LETTER E WITH OGONEK */
showKeySymbol XKB_KEY_Eogonek = "Eogonek"
-- #define XKB_KEY_Ecaron                        0x01cc  /* U+011A LATIN CAPITAL LETTER E WITH CARON */
showKeySymbol XKB_KEY_Ecaron = "Ecaron"
-- #define XKB_KEY_Dcaron                        0x01cf  /* U+010E LATIN CAPITAL LETTER D WITH CARON */
showKeySymbol XKB_KEY_Dcaron = "Dcaron"
-- #define XKB_KEY_Dstroke                       0x01d0  /* U+0110 LATIN CAPITAL LETTER D WITH STROKE */
showKeySymbol XKB_KEY_Dstroke = "Dstroke"
-- #define XKB_KEY_Nacute                        0x01d1  /* U+0143 LATIN CAPITAL LETTER N WITH ACUTE */
showKeySymbol XKB_KEY_Nacute = "Nacute"
-- #define XKB_KEY_Ncaron                        0x01d2  /* U+0147 LATIN CAPITAL LETTER N WITH CARON */
showKeySymbol XKB_KEY_Ncaron = "Ncaron"
-- #define XKB_KEY_Odoubleacute                  0x01d5  /* U+0150 LATIN CAPITAL LETTER O WITH DOUBLE ACUTE */
showKeySymbol XKB_KEY_Odoubleacute = "Odoubleacute"
-- #define XKB_KEY_Rcaron                        0x01d8  /* U+0158 LATIN CAPITAL LETTER R WITH CARON */
showKeySymbol XKB_KEY_Rcaron = "Rcaron"
-- #define XKB_KEY_Uring                         0x01d9  /* U+016E LATIN CAPITAL LETTER U WITH RING ABOVE */
showKeySymbol XKB_KEY_Uring = "Uring"
-- #define XKB_KEY_Udoubleacute                  0x01db  /* U+0170 LATIN CAPITAL LETTER U WITH DOUBLE ACUTE */
showKeySymbol XKB_KEY_Udoubleacute = "Udoubleacute"
-- #define XKB_KEY_Tcedilla                      0x01de  /* U+0162 LATIN CAPITAL LETTER T WITH CEDILLA */
showKeySymbol XKB_KEY_Tcedilla = "Tcedilla"
-- #define XKB_KEY_racute                        0x01e0  /* U+0155 LATIN SMALL LETTER R WITH ACUTE */
showKeySymbol XKB_KEY_racute = "racute"
-- #define XKB_KEY_abreve                        0x01e3  /* U+0103 LATIN SMALL LETTER A WITH BREVE */
showKeySymbol XKB_KEY_abreve = "abreve"
-- #define XKB_KEY_lacute                        0x01e5  /* U+013A LATIN SMALL LETTER L WITH ACUTE */
showKeySymbol XKB_KEY_lacute = "lacute"
-- #define XKB_KEY_cacute                        0x01e6  /* U+0107 LATIN SMALL LETTER C WITH ACUTE */
showKeySymbol XKB_KEY_cacute = "cacute"
-- #define XKB_KEY_ccaron                        0x01e8  /* U+010D LATIN SMALL LETTER C WITH CARON */
showKeySymbol XKB_KEY_ccaron = "ccaron"
-- #define XKB_KEY_eogonek                       0x01ea  /* U+0119 LATIN SMALL LETTER E WITH OGONEK */
showKeySymbol XKB_KEY_eogonek = "eogonek"
-- #define XKB_KEY_ecaron                        0x01ec  /* U+011B LATIN SMALL LETTER E WITH CARON */
showKeySymbol XKB_KEY_ecaron = "ecaron"
-- #define XKB_KEY_dcaron                        0x01ef  /* U+010F LATIN SMALL LETTER D WITH CARON */
showKeySymbol XKB_KEY_dcaron = "dcaron"
-- #define XKB_KEY_dstroke                       0x01f0  /* U+0111 LATIN SMALL LETTER D WITH STROKE */
showKeySymbol XKB_KEY_dstroke = "dstroke"
-- #define XKB_KEY_nacute                        0x01f1  /* U+0144 LATIN SMALL LETTER N WITH ACUTE */
showKeySymbol XKB_KEY_nacute = "nacute"
-- #define XKB_KEY_ncaron                        0x01f2  /* U+0148 LATIN SMALL LETTER N WITH CARON */
showKeySymbol XKB_KEY_ncaron = "ncaron"
-- #define XKB_KEY_odoubleacute                  0x01f5  /* U+0151 LATIN SMALL LETTER O WITH DOUBLE ACUTE */
showKeySymbol XKB_KEY_odoubleacute = "odoubleacute"
-- #define XKB_KEY_rcaron                        0x01f8  /* U+0159 LATIN SMALL LETTER R WITH CARON */
showKeySymbol XKB_KEY_rcaron = "rcaron"
-- #define XKB_KEY_uring                         0x01f9  /* U+016F LATIN SMALL LETTER U WITH RING ABOVE */
showKeySymbol XKB_KEY_uring = "uring"
-- #define XKB_KEY_udoubleacute                  0x01fb  /* U+0171 LATIN SMALL LETTER U WITH DOUBLE ACUTE */
showKeySymbol XKB_KEY_udoubleacute = "udoubleacute"
-- #define XKB_KEY_tcedilla                      0x01fe  /* U+0163 LATIN SMALL LETTER T WITH CEDILLA */
showKeySymbol XKB_KEY_tcedilla = "tcedilla"
-- #define XKB_KEY_abovedot                      0x01ff  /* U+02D9 DOT ABOVE */
showKeySymbol XKB_KEY_abovedot = "abovedot"
-- #define XKB_KEY_Hstroke                       0x02a1  /* U+0126 LATIN CAPITAL LETTER H WITH STROKE */
showKeySymbol XKB_KEY_Hstroke = "Hstroke"
-- #define XKB_KEY_Hcircumflex                   0x02a6  /* U+0124 LATIN CAPITAL LETTER H WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_Hcircumflex = "Hcircumflex"
-- #define XKB_KEY_Iabovedot                     0x02a9  /* U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE */
showKeySymbol XKB_KEY_Iabovedot = "Iabovedot"
-- #define XKB_KEY_Gbreve                        0x02ab  /* U+011E LATIN CAPITAL LETTER G WITH BREVE */
showKeySymbol XKB_KEY_Gbreve = "Gbreve"
-- #define XKB_KEY_Jcircumflex                   0x02ac  /* U+0134 LATIN CAPITAL LETTER J WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_Jcircumflex = "Jcircumflex"
-- #define XKB_KEY_hstroke                       0x02b1  /* U+0127 LATIN SMALL LETTER H WITH STROKE */
showKeySymbol XKB_KEY_hstroke = "hstroke"
-- #define XKB_KEY_hcircumflex                   0x02b6  /* U+0125 LATIN SMALL LETTER H WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_hcircumflex = "hcircumflex"
-- #define XKB_KEY_idotless                      0x02b9  /* U+0131 LATIN SMALL LETTER DOTLESS I */
showKeySymbol XKB_KEY_idotless = "idotless"
-- #define XKB_KEY_gbreve                        0x02bb  /* U+011F LATIN SMALL LETTER G WITH BREVE */
showKeySymbol XKB_KEY_gbreve = "gbreve"
-- #define XKB_KEY_jcircumflex                   0x02bc  /* U+0135 LATIN SMALL LETTER J WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_jcircumflex = "jcircumflex"
-- #define XKB_KEY_Cabovedot                     0x02c5  /* U+010A LATIN CAPITAL LETTER C WITH DOT ABOVE */
showKeySymbol XKB_KEY_Cabovedot = "Cabovedot"
-- #define XKB_KEY_Ccircumflex                   0x02c6  /* U+0108 LATIN CAPITAL LETTER C WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_Ccircumflex = "Ccircumflex"
-- #define XKB_KEY_Gabovedot                     0x02d5  /* U+0120 LATIN CAPITAL LETTER G WITH DOT ABOVE */
showKeySymbol XKB_KEY_Gabovedot = "Gabovedot"
-- #define XKB_KEY_Gcircumflex                   0x02d8  /* U+011C LATIN CAPITAL LETTER G WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_Gcircumflex = "Gcircumflex"
-- #define XKB_KEY_Ubreve                        0x02dd  /* U+016C LATIN CAPITAL LETTER U WITH BREVE */
showKeySymbol XKB_KEY_Ubreve = "Ubreve"
-- #define XKB_KEY_Scircumflex                   0x02de  /* U+015C LATIN CAPITAL LETTER S WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_Scircumflex = "Scircumflex"
-- #define XKB_KEY_cabovedot                     0x02e5  /* U+010B LATIN SMALL LETTER C WITH DOT ABOVE */
showKeySymbol XKB_KEY_cabovedot = "cabovedot"
-- #define XKB_KEY_ccircumflex                   0x02e6  /* U+0109 LATIN SMALL LETTER C WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_ccircumflex = "ccircumflex"
-- #define XKB_KEY_gabovedot                     0x02f5  /* U+0121 LATIN SMALL LETTER G WITH DOT ABOVE */
showKeySymbol XKB_KEY_gabovedot = "gabovedot"
-- #define XKB_KEY_gcircumflex                   0x02f8  /* U+011D LATIN SMALL LETTER G WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_gcircumflex = "gcircumflex"
-- #define XKB_KEY_ubreve                        0x02fd  /* U+016D LATIN SMALL LETTER U WITH BREVE */
showKeySymbol XKB_KEY_ubreve = "ubreve"
-- #define XKB_KEY_scircumflex                   0x02fe  /* U+015D LATIN SMALL LETTER S WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_scircumflex = "scircumflex"
-- #define XKB_KEY_kra                           0x03a2  /* U+0138 LATIN SMALL LETTER KRA */
showKeySymbol XKB_KEY_kra = "kra"
-- #define XKB_KEY_kappa                         0x03a2  /* deprecated */
showKeySymbol XKB_KEY_kappa = "kappa"
-- #define XKB_KEY_Rcedilla                      0x03a3  /* U+0156 LATIN CAPITAL LETTER R WITH CEDILLA */
showKeySymbol XKB_KEY_Rcedilla = "Rcedilla"
-- #define XKB_KEY_Itilde                        0x03a5  /* U+0128 LATIN CAPITAL LETTER I WITH TILDE */
showKeySymbol XKB_KEY_Itilde = "Itilde"
-- #define XKB_KEY_Lcedilla                      0x03a6  /* U+013B LATIN CAPITAL LETTER L WITH CEDILLA */
showKeySymbol XKB_KEY_Lcedilla = "Lcedilla"
-- #define XKB_KEY_Emacron                       0x03aa  /* U+0112 LATIN CAPITAL LETTER E WITH MACRON */
showKeySymbol XKB_KEY_Emacron = "Emacron"
-- #define XKB_KEY_Gcedilla                      0x03ab  /* U+0122 LATIN CAPITAL LETTER G WITH CEDILLA */
showKeySymbol XKB_KEY_Gcedilla = "Gcedilla"
-- #define XKB_KEY_Tslash                        0x03ac  /* U+0166 LATIN CAPITAL LETTER T WITH STROKE */
showKeySymbol XKB_KEY_Tslash = "Tslash"
-- #define XKB_KEY_rcedilla                      0x03b3  /* U+0157 LATIN SMALL LETTER R WITH CEDILLA */
showKeySymbol XKB_KEY_rcedilla = "rcedilla"
-- #define XKB_KEY_itilde                        0x03b5  /* U+0129 LATIN SMALL LETTER I WITH TILDE */
showKeySymbol XKB_KEY_itilde = "itilde"
-- #define XKB_KEY_lcedilla                      0x03b6  /* U+013C LATIN SMALL LETTER L WITH CEDILLA */
showKeySymbol XKB_KEY_lcedilla = "lcedilla"
-- #define XKB_KEY_emacron                       0x03ba  /* U+0113 LATIN SMALL LETTER E WITH MACRON */
showKeySymbol XKB_KEY_emacron = "emacron"
-- #define XKB_KEY_gcedilla                      0x03bb  /* U+0123 LATIN SMALL LETTER G WITH CEDILLA */
showKeySymbol XKB_KEY_gcedilla = "gcedilla"
-- #define XKB_KEY_tslash                        0x03bc  /* U+0167 LATIN SMALL LETTER T WITH STROKE */
showKeySymbol XKB_KEY_tslash = "tslash"
-- #define XKB_KEY_ENG                           0x03bd  /* U+014A LATIN CAPITAL LETTER ENG */
showKeySymbol XKB_KEY_ENG = "ENG"
-- #define XKB_KEY_eng                           0x03bf  /* U+014B LATIN SMALL LETTER ENG */
showKeySymbol XKB_KEY_eng = "eng"
-- #define XKB_KEY_Amacron                       0x03c0  /* U+0100 LATIN CAPITAL LETTER A WITH MACRON */
showKeySymbol XKB_KEY_Amacron = "Amacron"
-- #define XKB_KEY_Iogonek                       0x03c7  /* U+012E LATIN CAPITAL LETTER I WITH OGONEK */
showKeySymbol XKB_KEY_Iogonek = "Iogonek"
-- #define XKB_KEY_Eabovedot                     0x03cc  /* U+0116 LATIN CAPITAL LETTER E WITH DOT ABOVE */
showKeySymbol XKB_KEY_Eabovedot = "Eabovedot"
-- #define XKB_KEY_Imacron                       0x03cf  /* U+012A LATIN CAPITAL LETTER I WITH MACRON */
showKeySymbol XKB_KEY_Imacron = "Imacron"
-- #define XKB_KEY_Ncedilla                      0x03d1  /* U+0145 LATIN CAPITAL LETTER N WITH CEDILLA */
showKeySymbol XKB_KEY_Ncedilla = "Ncedilla"
-- #define XKB_KEY_Omacron                       0x03d2  /* U+014C LATIN CAPITAL LETTER O WITH MACRON */
showKeySymbol XKB_KEY_Omacron = "Omacron"
-- #define XKB_KEY_Kcedilla                      0x03d3  /* U+0136 LATIN CAPITAL LETTER K WITH CEDILLA */
showKeySymbol XKB_KEY_Kcedilla = "Kcedilla"
-- #define XKB_KEY_Uogonek                       0x03d9  /* U+0172 LATIN CAPITAL LETTER U WITH OGONEK */
showKeySymbol XKB_KEY_Uogonek = "Uogonek"
-- #define XKB_KEY_Utilde                        0x03dd  /* U+0168 LATIN CAPITAL LETTER U WITH TILDE */
showKeySymbol XKB_KEY_Utilde = "Utilde"
-- #define XKB_KEY_Umacron                       0x03de  /* U+016A LATIN CAPITAL LETTER U WITH MACRON */
showKeySymbol XKB_KEY_Umacron = "Umacron"
-- #define XKB_KEY_amacron                       0x03e0  /* U+0101 LATIN SMALL LETTER A WITH MACRON */
showKeySymbol XKB_KEY_amacron = "amacron"
-- #define XKB_KEY_iogonek                       0x03e7  /* U+012F LATIN SMALL LETTER I WITH OGONEK */
showKeySymbol XKB_KEY_iogonek = "iogonek"
-- #define XKB_KEY_eabovedot                     0x03ec  /* U+0117 LATIN SMALL LETTER E WITH DOT ABOVE */
showKeySymbol XKB_KEY_eabovedot = "eabovedot"
-- #define XKB_KEY_imacron                       0x03ef  /* U+012B LATIN SMALL LETTER I WITH MACRON */
showKeySymbol XKB_KEY_imacron = "imacron"
-- #define XKB_KEY_ncedilla                      0x03f1  /* U+0146 LATIN SMALL LETTER N WITH CEDILLA */
showKeySymbol XKB_KEY_ncedilla = "ncedilla"
-- #define XKB_KEY_omacron                       0x03f2  /* U+014D LATIN SMALL LETTER O WITH MACRON */
showKeySymbol XKB_KEY_omacron = "omacron"
-- #define XKB_KEY_kcedilla                      0x03f3  /* U+0137 LATIN SMALL LETTER K WITH CEDILLA */
showKeySymbol XKB_KEY_kcedilla = "kcedilla"
-- #define XKB_KEY_uogonek                       0x03f9  /* U+0173 LATIN SMALL LETTER U WITH OGONEK */
showKeySymbol XKB_KEY_uogonek = "uogonek"
-- #define XKB_KEY_utilde                        0x03fd  /* U+0169 LATIN SMALL LETTER U WITH TILDE */
showKeySymbol XKB_KEY_utilde = "utilde"
-- #define XKB_KEY_umacron                       0x03fe  /* U+016B LATIN SMALL LETTER U WITH MACRON */
showKeySymbol XKB_KEY_umacron = "umacron"
-- #define XKB_KEY_Wcircumflex                0x1000174  /* U+0174 LATIN CAPITAL LETTER W WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_Wcircumflex = "Wcircumflex"
-- #define XKB_KEY_wcircumflex                0x1000175  /* U+0175 LATIN SMALL LETTER W WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_wcircumflex = "wcircumflex"
-- #define XKB_KEY_Ycircumflex                0x1000176  /* U+0176 LATIN CAPITAL LETTER Y WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_Ycircumflex = "Ycircumflex"
-- #define XKB_KEY_ycircumflex                0x1000177  /* U+0177 LATIN SMALL LETTER Y WITH CIRCUMFLEX */
showKeySymbol XKB_KEY_ycircumflex = "ycircumflex"
-- #define XKB_KEY_Babovedot                  0x1001e02  /* U+1E02 LATIN CAPITAL LETTER B WITH DOT ABOVE */
showKeySymbol XKB_KEY_Babovedot = "Babovedot"
-- #define XKB_KEY_babovedot                  0x1001e03  /* U+1E03 LATIN SMALL LETTER B WITH DOT ABOVE */
showKeySymbol XKB_KEY_babovedot = "babovedot"
-- #define XKB_KEY_Dabovedot                  0x1001e0a  /* U+1E0A LATIN CAPITAL LETTER D WITH DOT ABOVE */
showKeySymbol XKB_KEY_Dabovedot = "Dabovedot"
-- #define XKB_KEY_dabovedot                  0x1001e0b  /* U+1E0B LATIN SMALL LETTER D WITH DOT ABOVE */
showKeySymbol XKB_KEY_dabovedot = "dabovedot"
-- #define XKB_KEY_Fabovedot                  0x1001e1e  /* U+1E1E LATIN CAPITAL LETTER F WITH DOT ABOVE */
showKeySymbol XKB_KEY_Fabovedot = "Fabovedot"
-- #define XKB_KEY_fabovedot                  0x1001e1f  /* U+1E1F LATIN SMALL LETTER F WITH DOT ABOVE */
showKeySymbol XKB_KEY_fabovedot = "fabovedot"
-- #define XKB_KEY_Mabovedot                  0x1001e40  /* U+1E40 LATIN CAPITAL LETTER M WITH DOT ABOVE */
showKeySymbol XKB_KEY_Mabovedot = "Mabovedot"
-- #define XKB_KEY_mabovedot                  0x1001e41  /* U+1E41 LATIN SMALL LETTER M WITH DOT ABOVE */
showKeySymbol XKB_KEY_mabovedot = "mabovedot"
-- #define XKB_KEY_Pabovedot                  0x1001e56  /* U+1E56 LATIN CAPITAL LETTER P WITH DOT ABOVE */
showKeySymbol XKB_KEY_Pabovedot = "Pabovedot"
-- #define XKB_KEY_pabovedot                  0x1001e57  /* U+1E57 LATIN SMALL LETTER P WITH DOT ABOVE */
showKeySymbol XKB_KEY_pabovedot = "pabovedot"
-- #define XKB_KEY_Sabovedot                  0x1001e60  /* U+1E60 LATIN CAPITAL LETTER S WITH DOT ABOVE */
showKeySymbol XKB_KEY_Sabovedot = "Sabovedot"
-- #define XKB_KEY_sabovedot                  0x1001e61  /* U+1E61 LATIN SMALL LETTER S WITH DOT ABOVE */
showKeySymbol XKB_KEY_sabovedot = "sabovedot"
-- #define XKB_KEY_Tabovedot                  0x1001e6a  /* U+1E6A LATIN CAPITAL LETTER T WITH DOT ABOVE */
showKeySymbol XKB_KEY_Tabovedot = "Tabovedot"
-- #define XKB_KEY_tabovedot                  0x1001e6b  /* U+1E6B LATIN SMALL LETTER T WITH DOT ABOVE */
showKeySymbol XKB_KEY_tabovedot = "tabovedot"
-- #define XKB_KEY_Wgrave                     0x1001e80  /* U+1E80 LATIN CAPITAL LETTER W WITH GRAVE */
showKeySymbol XKB_KEY_Wgrave = "Wgrave"
-- #define XKB_KEY_wgrave                     0x1001e81  /* U+1E81 LATIN SMALL LETTER W WITH GRAVE */
showKeySymbol XKB_KEY_wgrave = "wgrave"
-- #define XKB_KEY_Wacute                     0x1001e82  /* U+1E82 LATIN CAPITAL LETTER W WITH ACUTE */
showKeySymbol XKB_KEY_Wacute = "Wacute"
-- #define XKB_KEY_wacute                     0x1001e83  /* U+1E83 LATIN SMALL LETTER W WITH ACUTE */
showKeySymbol XKB_KEY_wacute = "wacute"
-- #define XKB_KEY_Wdiaeresis                 0x1001e84  /* U+1E84 LATIN CAPITAL LETTER W WITH DIAERESIS */
showKeySymbol XKB_KEY_Wdiaeresis = "Wdiaeresis"
-- #define XKB_KEY_wdiaeresis                 0x1001e85  /* U+1E85 LATIN SMALL LETTER W WITH DIAERESIS */
showKeySymbol XKB_KEY_wdiaeresis = "wdiaeresis"
-- #define XKB_KEY_Ygrave                     0x1001ef2  /* U+1EF2 LATIN CAPITAL LETTER Y WITH GRAVE */
showKeySymbol XKB_KEY_Ygrave = "Ygrave"
-- #define XKB_KEY_ygrave                     0x1001ef3  /* U+1EF3 LATIN SMALL LETTER Y WITH GRAVE */
showKeySymbol XKB_KEY_ygrave = "ygrave"
-- #define XKB_KEY_OE                            0x13bc  /* U+0152 LATIN CAPITAL LIGATURE OE */
showKeySymbol XKB_KEY_OE = "OE"
-- #define XKB_KEY_oe                            0x13bd  /* U+0153 LATIN SMALL LIGATURE OE */
showKeySymbol XKB_KEY_oe = "oe"
-- #define XKB_KEY_Ydiaeresis                    0x13be  /* U+0178 LATIN CAPITAL LETTER Y WITH DIAERESIS */
showKeySymbol XKB_KEY_Ydiaeresis = "Ydiaeresis"
-- #define XKB_KEY_overline                      0x047e  /* U+203E OVERLINE */
showKeySymbol XKB_KEY_overline = "overline"
-- #define XKB_KEY_kana_fullstop                 0x04a1  /* U+3002 IDEOGRAPHIC FULL STOP */
showKeySymbol XKB_KEY_kana_fullstop = "kana_fullstop"
-- #define XKB_KEY_kana_openingbracket           0x04a2  /* U+300C LEFT CORNER BRACKET */
showKeySymbol XKB_KEY_kana_openingbracket = "kana_openingbracket"
-- #define XKB_KEY_kana_closingbracket           0x04a3  /* U+300D RIGHT CORNER BRACKET */
showKeySymbol XKB_KEY_kana_closingbracket = "kana_closingbracket"
-- #define XKB_KEY_kana_comma                    0x04a4  /* U+3001 IDEOGRAPHIC COMMA */
showKeySymbol XKB_KEY_kana_comma = "kana_comma"
-- #define XKB_KEY_kana_conjunctive              0x04a5  /* U+30FB KATAKANA MIDDLE DOT */
showKeySymbol XKB_KEY_kana_conjunctive = "kana_conjunctive"
-- #define XKB_KEY_kana_middledot                0x04a5  /* deprecated */
showKeySymbol XKB_KEY_kana_middledot = "kana_middledot"
-- #define XKB_KEY_kana_WO                       0x04a6  /* U+30F2 KATAKANA LETTER WO */
showKeySymbol XKB_KEY_kana_WO = "kana_WO"
-- #define XKB_KEY_kana_a                        0x04a7  /* U+30A1 KATAKANA LETTER SMALL A */
showKeySymbol XKB_KEY_kana_a = "kana_a"
-- #define XKB_KEY_kana_i                        0x04a8  /* U+30A3 KATAKANA LETTER SMALL I */
showKeySymbol XKB_KEY_kana_i = "kana_i"
-- #define XKB_KEY_kana_u                        0x04a9  /* U+30A5 KATAKANA LETTER SMALL U */
showKeySymbol XKB_KEY_kana_u = "kana_u"
-- #define XKB_KEY_kana_e                        0x04aa  /* U+30A7 KATAKANA LETTER SMALL E */
showKeySymbol XKB_KEY_kana_e = "kana_e"
-- #define XKB_KEY_kana_o                        0x04ab  /* U+30A9 KATAKANA LETTER SMALL O */
showKeySymbol XKB_KEY_kana_o = "kana_o"
-- #define XKB_KEY_kana_ya                       0x04ac  /* U+30E3 KATAKANA LETTER SMALL YA */
showKeySymbol XKB_KEY_kana_ya = "kana_ya"
-- #define XKB_KEY_kana_yu                       0x04ad  /* U+30E5 KATAKANA LETTER SMALL YU */
showKeySymbol XKB_KEY_kana_yu = "kana_yu"
-- #define XKB_KEY_kana_yo                       0x04ae  /* U+30E7 KATAKANA LETTER SMALL YO */
showKeySymbol XKB_KEY_kana_yo = "kana_yo"
-- #define XKB_KEY_kana_tsu                      0x04af  /* U+30C3 KATAKANA LETTER SMALL TU */
showKeySymbol XKB_KEY_kana_tsu = "kana_tsu"
-- #define XKB_KEY_kana_tu                       0x04af  /* deprecated */
showKeySymbol XKB_KEY_kana_tu = "kana_tu"
-- #define XKB_KEY_prolongedsound                0x04b0  /* U+30FC KATAKANA-HIRAGANA PROLONGED SOUND MARK */
showKeySymbol XKB_KEY_prolongedsound = "prolongedsound"
-- #define XKB_KEY_kana_A                        0x04b1  /* U+30A2 KATAKANA LETTER A */
showKeySymbol XKB_KEY_kana_A = "kana_A"
-- #define XKB_KEY_kana_I                        0x04b2  /* U+30A4 KATAKANA LETTER I */
showKeySymbol XKB_KEY_kana_I = "kana_I"
-- #define XKB_KEY_kana_U                        0x04b3  /* U+30A6 KATAKANA LETTER U */
showKeySymbol XKB_KEY_kana_U = "kana_U"
-- #define XKB_KEY_kana_E                        0x04b4  /* U+30A8 KATAKANA LETTER E */
showKeySymbol XKB_KEY_kana_E = "kana_E"
-- #define XKB_KEY_kana_O                        0x04b5  /* U+30AA KATAKANA LETTER O */
showKeySymbol XKB_KEY_kana_O = "kana_O"
-- #define XKB_KEY_kana_KA                       0x04b6  /* U+30AB KATAKANA LETTER KA */
showKeySymbol XKB_KEY_kana_KA = "kana_KA"
-- #define XKB_KEY_kana_KI                       0x04b7  /* U+30AD KATAKANA LETTER KI */
showKeySymbol XKB_KEY_kana_KI = "kana_KI"
-- #define XKB_KEY_kana_KU                       0x04b8  /* U+30AF KATAKANA LETTER KU */
showKeySymbol XKB_KEY_kana_KU = "kana_KU"
-- #define XKB_KEY_kana_KE                       0x04b9  /* U+30B1 KATAKANA LETTER KE */
showKeySymbol XKB_KEY_kana_KE = "kana_KE"
-- #define XKB_KEY_kana_KO                       0x04ba  /* U+30B3 KATAKANA LETTER KO */
showKeySymbol XKB_KEY_kana_KO = "kana_KO"
-- #define XKB_KEY_kana_SA                       0x04bb  /* U+30B5 KATAKANA LETTER SA */
showKeySymbol XKB_KEY_kana_SA = "kana_SA"
-- #define XKB_KEY_kana_SHI                      0x04bc  /* U+30B7 KATAKANA LETTER SI */
showKeySymbol XKB_KEY_kana_SHI = "kana_SHI"
-- #define XKB_KEY_kana_SU                       0x04bd  /* U+30B9 KATAKANA LETTER SU */
showKeySymbol XKB_KEY_kana_SU = "kana_SU"
-- #define XKB_KEY_kana_SE                       0x04be  /* U+30BB KATAKANA LETTER SE */
showKeySymbol XKB_KEY_kana_SE = "kana_SE"
-- #define XKB_KEY_kana_SO                       0x04bf  /* U+30BD KATAKANA LETTER SO */
showKeySymbol XKB_KEY_kana_SO = "kana_SO"
-- #define XKB_KEY_kana_TA                       0x04c0  /* U+30BF KATAKANA LETTER TA */
showKeySymbol XKB_KEY_kana_TA = "kana_TA"
-- #define XKB_KEY_kana_CHI                      0x04c1  /* U+30C1 KATAKANA LETTER TI */
showKeySymbol XKB_KEY_kana_CHI = "kana_CHI"
-- #define XKB_KEY_kana_TI                       0x04c1  /* deprecated */
showKeySymbol XKB_KEY_kana_TI = "kana_TI"
-- #define XKB_KEY_kana_TSU                      0x04c2  /* U+30C4 KATAKANA LETTER TU */
showKeySymbol XKB_KEY_kana_TSU = "kana_TSU"
-- #define XKB_KEY_kana_TU                       0x04c2  /* deprecated */
showKeySymbol XKB_KEY_kana_TU = "kana_TU"
-- #define XKB_KEY_kana_TE                       0x04c3  /* U+30C6 KATAKANA LETTER TE */
showKeySymbol XKB_KEY_kana_TE = "kana_TE"
-- #define XKB_KEY_kana_TO                       0x04c4  /* U+30C8 KATAKANA LETTER TO */
showKeySymbol XKB_KEY_kana_TO = "kana_TO"
-- #define XKB_KEY_kana_NA                       0x04c5  /* U+30CA KATAKANA LETTER NA */
showKeySymbol XKB_KEY_kana_NA = "kana_NA"
-- #define XKB_KEY_kana_NI                       0x04c6  /* U+30CB KATAKANA LETTER NI */
showKeySymbol XKB_KEY_kana_NI = "kana_NI"
-- #define XKB_KEY_kana_NU                       0x04c7  /* U+30CC KATAKANA LETTER NU */
showKeySymbol XKB_KEY_kana_NU = "kana_NU"
-- #define XKB_KEY_kana_NE                       0x04c8  /* U+30CD KATAKANA LETTER NE */
showKeySymbol XKB_KEY_kana_NE = "kana_NE"
-- #define XKB_KEY_kana_NO                       0x04c9  /* U+30CE KATAKANA LETTER NO */
showKeySymbol XKB_KEY_kana_NO = "kana_NO"
-- #define XKB_KEY_kana_HA                       0x04ca  /* U+30CF KATAKANA LETTER HA */
showKeySymbol XKB_KEY_kana_HA = "kana_HA"
-- #define XKB_KEY_kana_HI                       0x04cb  /* U+30D2 KATAKANA LETTER HI */
showKeySymbol XKB_KEY_kana_HI = "kana_HI"
-- #define XKB_KEY_kana_FU                       0x04cc  /* U+30D5 KATAKANA LETTER HU */
showKeySymbol XKB_KEY_kana_FU = "kana_FU"
-- #define XKB_KEY_kana_HU                       0x04cc  /* deprecated */
showKeySymbol XKB_KEY_kana_HU = "kana_HU"
-- #define XKB_KEY_kana_HE                       0x04cd  /* U+30D8 KATAKANA LETTER HE */
showKeySymbol XKB_KEY_kana_HE = "kana_HE"
-- #define XKB_KEY_kana_HO                       0x04ce  /* U+30DB KATAKANA LETTER HO */
showKeySymbol XKB_KEY_kana_HO = "kana_HO"
-- #define XKB_KEY_kana_MA                       0x04cf  /* U+30DE KATAKANA LETTER MA */
showKeySymbol XKB_KEY_kana_MA = "kana_MA"
-- #define XKB_KEY_kana_MI                       0x04d0  /* U+30DF KATAKANA LETTER MI */
showKeySymbol XKB_KEY_kana_MI = "kana_MI"
-- #define XKB_KEY_kana_MU                       0x04d1  /* U+30E0 KATAKANA LETTER MU */
showKeySymbol XKB_KEY_kana_MU = "kana_MU"
-- #define XKB_KEY_kana_ME                       0x04d2  /* U+30E1 KATAKANA LETTER ME */
showKeySymbol XKB_KEY_kana_ME = "kana_ME"
-- #define XKB_KEY_kana_MO                       0x04d3  /* U+30E2 KATAKANA LETTER MO */
showKeySymbol XKB_KEY_kana_MO = "kana_MO"
-- #define XKB_KEY_kana_YA                       0x04d4  /* U+30E4 KATAKANA LETTER YA */
showKeySymbol XKB_KEY_kana_YA = "kana_YA"
-- #define XKB_KEY_kana_YU                       0x04d5  /* U+30E6 KATAKANA LETTER YU */
showKeySymbol XKB_KEY_kana_YU = "kana_YU"
-- #define XKB_KEY_kana_YO                       0x04d6  /* U+30E8 KATAKANA LETTER YO */
showKeySymbol XKB_KEY_kana_YO = "kana_YO"
-- #define XKB_KEY_kana_RA                       0x04d7  /* U+30E9 KATAKANA LETTER RA */
showKeySymbol XKB_KEY_kana_RA = "kana_RA"
-- #define XKB_KEY_kana_RI                       0x04d8  /* U+30EA KATAKANA LETTER RI */
showKeySymbol XKB_KEY_kana_RI = "kana_RI"
-- #define XKB_KEY_kana_RU                       0x04d9  /* U+30EB KATAKANA LETTER RU */
showKeySymbol XKB_KEY_kana_RU = "kana_RU"
-- #define XKB_KEY_kana_RE                       0x04da  /* U+30EC KATAKANA LETTER RE */
showKeySymbol XKB_KEY_kana_RE = "kana_RE"
-- #define XKB_KEY_kana_RO                       0x04db  /* U+30ED KATAKANA LETTER RO */
showKeySymbol XKB_KEY_kana_RO = "kana_RO"
-- #define XKB_KEY_kana_WA                       0x04dc  /* U+30EF KATAKANA LETTER WA */
showKeySymbol XKB_KEY_kana_WA = "kana_WA"
-- #define XKB_KEY_kana_N                        0x04dd  /* U+30F3 KATAKANA LETTER N */
showKeySymbol XKB_KEY_kana_N = "kana_N"
-- #define XKB_KEY_voicedsound                   0x04de  /* U+309B KATAKANA-HIRAGANA VOICED SOUND MARK */
showKeySymbol XKB_KEY_voicedsound = "voicedsound"
-- #define XKB_KEY_semivoicedsound               0x04df  /* U+309C KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK */
showKeySymbol XKB_KEY_semivoicedsound = "semivoicedsound"
-- #define XKB_KEY_kana_switch                   0xff7e  /* Alias for mode_switch */
showKeySymbol XKB_KEY_kana_switch = "kana_switch"
-- #define XKB_KEY_Farsi_0                    0x10006f0  /* U+06F0 EXTENDED ARABIC-INDIC DIGIT ZERO */
showKeySymbol XKB_KEY_Farsi_0 = "Farsi_0"
-- #define XKB_KEY_Farsi_1                    0x10006f1  /* U+06F1 EXTENDED ARABIC-INDIC DIGIT ONE */
showKeySymbol XKB_KEY_Farsi_1 = "Farsi_1"
-- #define XKB_KEY_Farsi_2                    0x10006f2  /* U+06F2 EXTENDED ARABIC-INDIC DIGIT TWO */
showKeySymbol XKB_KEY_Farsi_2 = "Farsi_2"
-- #define XKB_KEY_Farsi_3                    0x10006f3  /* U+06F3 EXTENDED ARABIC-INDIC DIGIT THREE */
showKeySymbol XKB_KEY_Farsi_3 = "Farsi_3"
-- #define XKB_KEY_Farsi_4                    0x10006f4  /* U+06F4 EXTENDED ARABIC-INDIC DIGIT FOUR */
showKeySymbol XKB_KEY_Farsi_4 = "Farsi_4"
-- #define XKB_KEY_Farsi_5                    0x10006f5  /* U+06F5 EXTENDED ARABIC-INDIC DIGIT FIVE */
showKeySymbol XKB_KEY_Farsi_5 = "Farsi_5"
-- #define XKB_KEY_Farsi_6                    0x10006f6  /* U+06F6 EXTENDED ARABIC-INDIC DIGIT SIX */
showKeySymbol XKB_KEY_Farsi_6 = "Farsi_6"
-- #define XKB_KEY_Farsi_7                    0x10006f7  /* U+06F7 EXTENDED ARABIC-INDIC DIGIT SEVEN */
showKeySymbol XKB_KEY_Farsi_7 = "Farsi_7"
-- #define XKB_KEY_Farsi_8                    0x10006f8  /* U+06F8 EXTENDED ARABIC-INDIC DIGIT EIGHT */
showKeySymbol XKB_KEY_Farsi_8 = "Farsi_8"
-- #define XKB_KEY_Farsi_9                    0x10006f9  /* U+06F9 EXTENDED ARABIC-INDIC DIGIT NINE */
showKeySymbol XKB_KEY_Farsi_9 = "Farsi_9"
-- #define XKB_KEY_Arabic_percent             0x100066a  /* U+066A ARABIC PERCENT SIGN */
showKeySymbol XKB_KEY_Arabic_percent = "Arabic_percent"
-- #define XKB_KEY_Arabic_superscript_alef    0x1000670  /* U+0670 ARABIC LETTER SUPERSCRIPT ALEF */
showKeySymbol XKB_KEY_Arabic_superscript_alef = "Arabic_superscript_alef"
-- #define XKB_KEY_Arabic_tteh                0x1000679  /* U+0679 ARABIC LETTER TTEH */
showKeySymbol XKB_KEY_Arabic_tteh = "Arabic_tteh"
-- #define XKB_KEY_Arabic_peh                 0x100067e  /* U+067E ARABIC LETTER PEH */
showKeySymbol XKB_KEY_Arabic_peh = "Arabic_peh"
-- #define XKB_KEY_Arabic_tcheh               0x1000686  /* U+0686 ARABIC LETTER TCHEH */
showKeySymbol XKB_KEY_Arabic_tcheh = "Arabic_tcheh"
-- #define XKB_KEY_Arabic_ddal                0x1000688  /* U+0688 ARABIC LETTER DDAL */
showKeySymbol XKB_KEY_Arabic_ddal = "Arabic_ddal"
-- #define XKB_KEY_Arabic_rreh                0x1000691  /* U+0691 ARABIC LETTER RREH */
showKeySymbol XKB_KEY_Arabic_rreh = "Arabic_rreh"
-- #define XKB_KEY_Arabic_comma                  0x05ac  /* U+060C ARABIC COMMA */
showKeySymbol XKB_KEY_Arabic_comma = "Arabic_comma"
-- #define XKB_KEY_Arabic_fullstop            0x10006d4  /* U+06D4 ARABIC FULL STOP */
showKeySymbol XKB_KEY_Arabic_fullstop = "Arabic_fullstop"
-- #define XKB_KEY_Arabic_0                   0x1000660  /* U+0660 ARABIC-INDIC DIGIT ZERO */
showKeySymbol XKB_KEY_Arabic_0 = "Arabic_0"
-- #define XKB_KEY_Arabic_1                   0x1000661  /* U+0661 ARABIC-INDIC DIGIT ONE */
showKeySymbol XKB_KEY_Arabic_1 = "Arabic_1"
-- #define XKB_KEY_Arabic_2                   0x1000662  /* U+0662 ARABIC-INDIC DIGIT TWO */
showKeySymbol XKB_KEY_Arabic_2 = "Arabic_2"
-- #define XKB_KEY_Arabic_3                   0x1000663  /* U+0663 ARABIC-INDIC DIGIT THREE */
showKeySymbol XKB_KEY_Arabic_3 = "Arabic_3"
-- #define XKB_KEY_Arabic_4                   0x1000664  /* U+0664 ARABIC-INDIC DIGIT FOUR */
showKeySymbol XKB_KEY_Arabic_4 = "Arabic_4"
-- #define XKB_KEY_Arabic_5                   0x1000665  /* U+0665 ARABIC-INDIC DIGIT FIVE */
showKeySymbol XKB_KEY_Arabic_5 = "Arabic_5"
-- #define XKB_KEY_Arabic_6                   0x1000666  /* U+0666 ARABIC-INDIC DIGIT SIX */
showKeySymbol XKB_KEY_Arabic_6 = "Arabic_6"
-- #define XKB_KEY_Arabic_7                   0x1000667  /* U+0667 ARABIC-INDIC DIGIT SEVEN */
showKeySymbol XKB_KEY_Arabic_7 = "Arabic_7"
-- #define XKB_KEY_Arabic_8                   0x1000668  /* U+0668 ARABIC-INDIC DIGIT EIGHT */
showKeySymbol XKB_KEY_Arabic_8 = "Arabic_8"
-- #define XKB_KEY_Arabic_9                   0x1000669  /* U+0669 ARABIC-INDIC DIGIT NINE */
showKeySymbol XKB_KEY_Arabic_9 = "Arabic_9"
-- #define XKB_KEY_Arabic_semicolon              0x05bb  /* U+061B ARABIC SEMICOLON */
showKeySymbol XKB_KEY_Arabic_semicolon = "Arabic_semicolon"
-- #define XKB_KEY_Arabic_question_mark          0x05bf  /* U+061F ARABIC QUESTION MARK */
showKeySymbol XKB_KEY_Arabic_question_mark = "Arabic_question_mark"
-- #define XKB_KEY_Arabic_hamza                  0x05c1  /* U+0621 ARABIC LETTER HAMZA */
showKeySymbol XKB_KEY_Arabic_hamza = "Arabic_hamza"
-- #define XKB_KEY_Arabic_maddaonalef            0x05c2  /* U+0622 ARABIC LETTER ALEF WITH MADDA ABOVE */
showKeySymbol XKB_KEY_Arabic_maddaonalef = "Arabic_maddaonalef"
-- #define XKB_KEY_Arabic_hamzaonalef            0x05c3  /* U+0623 ARABIC LETTER ALEF WITH HAMZA ABOVE */
showKeySymbol XKB_KEY_Arabic_hamzaonalef = "Arabic_hamzaonalef"
-- #define XKB_KEY_Arabic_hamzaonwaw             0x05c4  /* U+0624 ARABIC LETTER WAW WITH HAMZA ABOVE */
showKeySymbol XKB_KEY_Arabic_hamzaonwaw = "Arabic_hamzaonwaw"
-- #define XKB_KEY_Arabic_hamzaunderalef         0x05c5  /* U+0625 ARABIC LETTER ALEF WITH HAMZA BELOW */
showKeySymbol XKB_KEY_Arabic_hamzaunderalef = "Arabic_hamzaunderalef"
-- #define XKB_KEY_Arabic_hamzaonyeh             0x05c6  /* U+0626 ARABIC LETTER YEH WITH HAMZA ABOVE */
showKeySymbol XKB_KEY_Arabic_hamzaonyeh = "Arabic_hamzaonyeh"
-- #define XKB_KEY_Arabic_alef                   0x05c7  /* U+0627 ARABIC LETTER ALEF */
showKeySymbol XKB_KEY_Arabic_alef = "Arabic_alef"
-- #define XKB_KEY_Arabic_beh                    0x05c8  /* U+0628 ARABIC LETTER BEH */
showKeySymbol XKB_KEY_Arabic_beh = "Arabic_beh"
-- #define XKB_KEY_Arabic_tehmarbuta             0x05c9  /* U+0629 ARABIC LETTER TEH MARBUTA */
showKeySymbol XKB_KEY_Arabic_tehmarbuta = "Arabic_tehmarbuta"
-- #define XKB_KEY_Arabic_teh                    0x05ca  /* U+062A ARABIC LETTER TEH */
showKeySymbol XKB_KEY_Arabic_teh = "Arabic_teh"
-- #define XKB_KEY_Arabic_theh                   0x05cb  /* U+062B ARABIC LETTER THEH */
showKeySymbol XKB_KEY_Arabic_theh = "Arabic_theh"
-- #define XKB_KEY_Arabic_jeem                   0x05cc  /* U+062C ARABIC LETTER JEEM */
showKeySymbol XKB_KEY_Arabic_jeem = "Arabic_jeem"
-- #define XKB_KEY_Arabic_hah                    0x05cd  /* U+062D ARABIC LETTER HAH */
showKeySymbol XKB_KEY_Arabic_hah = "Arabic_hah"
-- #define XKB_KEY_Arabic_khah                   0x05ce  /* U+062E ARABIC LETTER KHAH */
showKeySymbol XKB_KEY_Arabic_khah = "Arabic_khah"
-- #define XKB_KEY_Arabic_dal                    0x05cf  /* U+062F ARABIC LETTER DAL */
showKeySymbol XKB_KEY_Arabic_dal = "Arabic_dal"
-- #define XKB_KEY_Arabic_thal                   0x05d0  /* U+0630 ARABIC LETTER THAL */
showKeySymbol XKB_KEY_Arabic_thal = "Arabic_thal"
-- #define XKB_KEY_Arabic_ra                     0x05d1  /* U+0631 ARABIC LETTER REH */
showKeySymbol XKB_KEY_Arabic_ra = "Arabic_ra"
-- #define XKB_KEY_Arabic_zain                   0x05d2  /* U+0632 ARABIC LETTER ZAIN */
showKeySymbol XKB_KEY_Arabic_zain = "Arabic_zain"
-- #define XKB_KEY_Arabic_seen                   0x05d3  /* U+0633 ARABIC LETTER SEEN */
showKeySymbol XKB_KEY_Arabic_seen = "Arabic_seen"
-- #define XKB_KEY_Arabic_sheen                  0x05d4  /* U+0634 ARABIC LETTER SHEEN */
showKeySymbol XKB_KEY_Arabic_sheen = "Arabic_sheen"
-- #define XKB_KEY_Arabic_sad                    0x05d5  /* U+0635 ARABIC LETTER SAD */
showKeySymbol XKB_KEY_Arabic_sad = "Arabic_sad"
-- #define XKB_KEY_Arabic_dad                    0x05d6  /* U+0636 ARABIC LETTER DAD */
showKeySymbol XKB_KEY_Arabic_dad = "Arabic_dad"
-- #define XKB_KEY_Arabic_tah                    0x05d7  /* U+0637 ARABIC LETTER TAH */
showKeySymbol XKB_KEY_Arabic_tah = "Arabic_tah"
-- #define XKB_KEY_Arabic_zah                    0x05d8  /* U+0638 ARABIC LETTER ZAH */
showKeySymbol XKB_KEY_Arabic_zah = "Arabic_zah"
-- #define XKB_KEY_Arabic_ain                    0x05d9  /* U+0639 ARABIC LETTER AIN */
showKeySymbol XKB_KEY_Arabic_ain = "Arabic_ain"
-- #define XKB_KEY_Arabic_ghain                  0x05da  /* U+063A ARABIC LETTER GHAIN */
showKeySymbol XKB_KEY_Arabic_ghain = "Arabic_ghain"
-- #define XKB_KEY_Arabic_tatweel                0x05e0  /* U+0640 ARABIC TATWEEL */
showKeySymbol XKB_KEY_Arabic_tatweel = "Arabic_tatweel"
-- #define XKB_KEY_Arabic_feh                    0x05e1  /* U+0641 ARABIC LETTER FEH */
showKeySymbol XKB_KEY_Arabic_feh = "Arabic_feh"
-- #define XKB_KEY_Arabic_qaf                    0x05e2  /* U+0642 ARABIC LETTER QAF */
showKeySymbol XKB_KEY_Arabic_qaf = "Arabic_qaf"
-- #define XKB_KEY_Arabic_kaf                    0x05e3  /* U+0643 ARABIC LETTER KAF */
showKeySymbol XKB_KEY_Arabic_kaf = "Arabic_kaf"
-- #define XKB_KEY_Arabic_lam                    0x05e4  /* U+0644 ARABIC LETTER LAM */
showKeySymbol XKB_KEY_Arabic_lam = "Arabic_lam"
-- #define XKB_KEY_Arabic_meem                   0x05e5  /* U+0645 ARABIC LETTER MEEM */
showKeySymbol XKB_KEY_Arabic_meem = "Arabic_meem"
-- #define XKB_KEY_Arabic_noon                   0x05e6  /* U+0646 ARABIC LETTER NOON */
showKeySymbol XKB_KEY_Arabic_noon = "Arabic_noon"
-- #define XKB_KEY_Arabic_ha                     0x05e7  /* U+0647 ARABIC LETTER HEH */
showKeySymbol XKB_KEY_Arabic_ha = "Arabic_ha"
-- #define XKB_KEY_Arabic_heh                    0x05e7  /* deprecated */
showKeySymbol XKB_KEY_Arabic_heh = "Arabic_heh"
-- #define XKB_KEY_Arabic_waw                    0x05e8  /* U+0648 ARABIC LETTER WAW */
showKeySymbol XKB_KEY_Arabic_waw = "Arabic_waw"
-- #define XKB_KEY_Arabic_alefmaksura            0x05e9  /* U+0649 ARABIC LETTER ALEF MAKSURA */
showKeySymbol XKB_KEY_Arabic_alefmaksura = "Arabic_alefmaksura"
-- #define XKB_KEY_Arabic_yeh                    0x05ea  /* U+064A ARABIC LETTER YEH */
showKeySymbol XKB_KEY_Arabic_yeh = "Arabic_yeh"
-- #define XKB_KEY_Arabic_fathatan               0x05eb  /* U+064B ARABIC FATHATAN */
showKeySymbol XKB_KEY_Arabic_fathatan = "Arabic_fathatan"
-- #define XKB_KEY_Arabic_dammatan               0x05ec  /* U+064C ARABIC DAMMATAN */
showKeySymbol XKB_KEY_Arabic_dammatan = "Arabic_dammatan"
-- #define XKB_KEY_Arabic_kasratan               0x05ed  /* U+064D ARABIC KASRATAN */
showKeySymbol XKB_KEY_Arabic_kasratan = "Arabic_kasratan"
-- #define XKB_KEY_Arabic_fatha                  0x05ee  /* U+064E ARABIC FATHA */
showKeySymbol XKB_KEY_Arabic_fatha = "Arabic_fatha"
-- #define XKB_KEY_Arabic_damma                  0x05ef  /* U+064F ARABIC DAMMA */
showKeySymbol XKB_KEY_Arabic_damma = "Arabic_damma"
-- #define XKB_KEY_Arabic_kasra                  0x05f0  /* U+0650 ARABIC KASRA */
showKeySymbol XKB_KEY_Arabic_kasra = "Arabic_kasra"
-- #define XKB_KEY_Arabic_shadda                 0x05f1  /* U+0651 ARABIC SHADDA */
showKeySymbol XKB_KEY_Arabic_shadda = "Arabic_shadda"
-- #define XKB_KEY_Arabic_sukun                  0x05f2  /* U+0652 ARABIC SUKUN */
showKeySymbol XKB_KEY_Arabic_sukun = "Arabic_sukun"
-- #define XKB_KEY_Arabic_madda_above         0x1000653  /* U+0653 ARABIC MADDAH ABOVE */
showKeySymbol XKB_KEY_Arabic_madda_above = "Arabic_madda_above"
-- #define XKB_KEY_Arabic_hamza_above         0x1000654  /* U+0654 ARABIC HAMZA ABOVE */
showKeySymbol XKB_KEY_Arabic_hamza_above = "Arabic_hamza_above"
-- #define XKB_KEY_Arabic_hamza_below         0x1000655  /* U+0655 ARABIC HAMZA BELOW */
showKeySymbol XKB_KEY_Arabic_hamza_below = "Arabic_hamza_below"
-- #define XKB_KEY_Arabic_jeh                 0x1000698  /* U+0698 ARABIC LETTER JEH */
showKeySymbol XKB_KEY_Arabic_jeh = "Arabic_jeh"
-- #define XKB_KEY_Arabic_veh                 0x10006a4  /* U+06A4 ARABIC LETTER VEH */
showKeySymbol XKB_KEY_Arabic_veh = "Arabic_veh"
-- #define XKB_KEY_Arabic_keheh               0x10006a9  /* U+06A9 ARABIC LETTER KEHEH */
showKeySymbol XKB_KEY_Arabic_keheh = "Arabic_keheh"
-- #define XKB_KEY_Arabic_gaf                 0x10006af  /* U+06AF ARABIC LETTER GAF */
showKeySymbol XKB_KEY_Arabic_gaf = "Arabic_gaf"
-- #define XKB_KEY_Arabic_noon_ghunna         0x10006ba  /* U+06BA ARABIC LETTER NOON GHUNNA */
showKeySymbol XKB_KEY_Arabic_noon_ghunna = "Arabic_noon_ghunna"
-- #define XKB_KEY_Arabic_heh_doachashmee     0x10006be  /* U+06BE ARABIC LETTER HEH DOACHASHMEE */
showKeySymbol XKB_KEY_Arabic_heh_doachashmee = "Arabic_heh_doachashmee"
-- #define XKB_KEY_Farsi_yeh                  0x10006cc  /* U+06CC ARABIC LETTER FARSI YEH */
showKeySymbol XKB_KEY_Farsi_yeh = "Farsi_yeh"
-- #define XKB_KEY_Arabic_farsi_yeh           0x10006cc  /* U+06CC ARABIC LETTER FARSI YEH */
showKeySymbol XKB_KEY_Arabic_farsi_yeh = "Arabic_farsi_yeh"
-- #define XKB_KEY_Arabic_yeh_baree           0x10006d2  /* U+06D2 ARABIC LETTER YEH BARREE */
showKeySymbol XKB_KEY_Arabic_yeh_baree = "Arabic_yeh_baree"
-- #define XKB_KEY_Arabic_heh_goal            0x10006c1  /* U+06C1 ARABIC LETTER HEH GOAL */
showKeySymbol XKB_KEY_Arabic_heh_goal = "Arabic_heh_goal"
-- #define XKB_KEY_Arabic_switch                 0xff7e  /* Alias for mode_switch */
showKeySymbol XKB_KEY_Arabic_switch = "Arabic_switch"
-- #define XKB_KEY_Cyrillic_GHE_bar           0x1000492  /* U+0492 CYRILLIC CAPITAL LETTER GHE WITH STROKE */
showKeySymbol XKB_KEY_Cyrillic_GHE_bar = "Cyrillic_GHE_bar"
-- #define XKB_KEY_Cyrillic_ghe_bar           0x1000493  /* U+0493 CYRILLIC SMALL LETTER GHE WITH STROKE */
showKeySymbol XKB_KEY_Cyrillic_ghe_bar = "Cyrillic_ghe_bar"
-- #define XKB_KEY_Cyrillic_ZHE_descender     0x1000496  /* U+0496 CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER */
showKeySymbol XKB_KEY_Cyrillic_ZHE_descender = "Cyrillic_ZHE_descender"
-- #define XKB_KEY_Cyrillic_zhe_descender     0x1000497  /* U+0497 CYRILLIC SMALL LETTER ZHE WITH DESCENDER */
showKeySymbol XKB_KEY_Cyrillic_zhe_descender = "Cyrillic_zhe_descender"
-- #define XKB_KEY_Cyrillic_KA_descender      0x100049a  /* U+049A CYRILLIC CAPITAL LETTER KA WITH DESCENDER */
showKeySymbol XKB_KEY_Cyrillic_KA_descender = "Cyrillic_KA_descender"
-- #define XKB_KEY_Cyrillic_ka_descender      0x100049b  /* U+049B CYRILLIC SMALL LETTER KA WITH DESCENDER */
showKeySymbol XKB_KEY_Cyrillic_ka_descender = "Cyrillic_ka_descender"
-- #define XKB_KEY_Cyrillic_KA_vertstroke     0x100049c  /* U+049C CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE */
showKeySymbol XKB_KEY_Cyrillic_KA_vertstroke = "Cyrillic_KA_vertstroke"
-- #define XKB_KEY_Cyrillic_ka_vertstroke     0x100049d  /* U+049D CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE */
showKeySymbol XKB_KEY_Cyrillic_ka_vertstroke = "Cyrillic_ka_vertstroke"
-- #define XKB_KEY_Cyrillic_EN_descender      0x10004a2  /* U+04A2 CYRILLIC CAPITAL LETTER EN WITH DESCENDER */
showKeySymbol XKB_KEY_Cyrillic_EN_descender = "Cyrillic_EN_descender"
-- #define XKB_KEY_Cyrillic_en_descender      0x10004a3  /* U+04A3 CYRILLIC SMALL LETTER EN WITH DESCENDER */
showKeySymbol XKB_KEY_Cyrillic_en_descender = "Cyrillic_en_descender"
-- #define XKB_KEY_Cyrillic_U_straight        0x10004ae  /* U+04AE CYRILLIC CAPITAL LETTER STRAIGHT U */
showKeySymbol XKB_KEY_Cyrillic_U_straight = "Cyrillic_U_straight"
-- #define XKB_KEY_Cyrillic_u_straight        0x10004af  /* U+04AF CYRILLIC SMALL LETTER STRAIGHT U */
showKeySymbol XKB_KEY_Cyrillic_u_straight = "Cyrillic_u_straight"
-- #define XKB_KEY_Cyrillic_U_straight_bar    0x10004b0  /* U+04B0 CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE */
showKeySymbol XKB_KEY_Cyrillic_U_straight_bar = "Cyrillic_U_straight_bar"
-- #define XKB_KEY_Cyrillic_u_straight_bar    0x10004b1  /* U+04B1 CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE */
showKeySymbol XKB_KEY_Cyrillic_u_straight_bar = "Cyrillic_u_straight_bar"
-- #define XKB_KEY_Cyrillic_HA_descender      0x10004b2  /* U+04B2 CYRILLIC CAPITAL LETTER HA WITH DESCENDER */
showKeySymbol XKB_KEY_Cyrillic_HA_descender = "Cyrillic_HA_descender"
-- #define XKB_KEY_Cyrillic_ha_descender      0x10004b3  /* U+04B3 CYRILLIC SMALL LETTER HA WITH DESCENDER */
showKeySymbol XKB_KEY_Cyrillic_ha_descender = "Cyrillic_ha_descender"
-- #define XKB_KEY_Cyrillic_CHE_descender     0x10004b6  /* U+04B6 CYRILLIC CAPITAL LETTER CHE WITH DESCENDER */
showKeySymbol XKB_KEY_Cyrillic_CHE_descender = "Cyrillic_CHE_descender"
-- #define XKB_KEY_Cyrillic_che_descender     0x10004b7  /* U+04B7 CYRILLIC SMALL LETTER CHE WITH DESCENDER */
showKeySymbol XKB_KEY_Cyrillic_che_descender = "Cyrillic_che_descender"
-- #define XKB_KEY_Cyrillic_CHE_vertstroke    0x10004b8  /* U+04B8 CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE */
showKeySymbol XKB_KEY_Cyrillic_CHE_vertstroke = "Cyrillic_CHE_vertstroke"
-- #define XKB_KEY_Cyrillic_che_vertstroke    0x10004b9  /* U+04B9 CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE */
showKeySymbol XKB_KEY_Cyrillic_che_vertstroke = "Cyrillic_che_vertstroke"
-- #define XKB_KEY_Cyrillic_SHHA              0x10004ba  /* U+04BA CYRILLIC CAPITAL LETTER SHHA */
showKeySymbol XKB_KEY_Cyrillic_SHHA = "Cyrillic_SHHA"
-- #define XKB_KEY_Cyrillic_shha              0x10004bb  /* U+04BB CYRILLIC SMALL LETTER SHHA */
showKeySymbol XKB_KEY_Cyrillic_shha = "Cyrillic_shha"
-- #define XKB_KEY_Cyrillic_SCHWA             0x10004d8  /* U+04D8 CYRILLIC CAPITAL LETTER SCHWA */
showKeySymbol XKB_KEY_Cyrillic_SCHWA = "Cyrillic_SCHWA"
-- #define XKB_KEY_Cyrillic_schwa             0x10004d9  /* U+04D9 CYRILLIC SMALL LETTER SCHWA */
showKeySymbol XKB_KEY_Cyrillic_schwa = "Cyrillic_schwa"
-- #define XKB_KEY_Cyrillic_I_macron          0x10004e2  /* U+04E2 CYRILLIC CAPITAL LETTER I WITH MACRON */
showKeySymbol XKB_KEY_Cyrillic_I_macron = "Cyrillic_I_macron"
-- #define XKB_KEY_Cyrillic_i_macron          0x10004e3  /* U+04E3 CYRILLIC SMALL LETTER I WITH MACRON */
showKeySymbol XKB_KEY_Cyrillic_i_macron = "Cyrillic_i_macron"
-- #define XKB_KEY_Cyrillic_O_bar             0x10004e8  /* U+04E8 CYRILLIC CAPITAL LETTER BARRED O */
showKeySymbol XKB_KEY_Cyrillic_O_bar = "Cyrillic_O_bar"
-- #define XKB_KEY_Cyrillic_o_bar             0x10004e9  /* U+04E9 CYRILLIC SMALL LETTER BARRED O */
showKeySymbol XKB_KEY_Cyrillic_o_bar = "Cyrillic_o_bar"
-- #define XKB_KEY_Cyrillic_U_macron          0x10004ee  /* U+04EE CYRILLIC CAPITAL LETTER U WITH MACRON */
showKeySymbol XKB_KEY_Cyrillic_U_macron = "Cyrillic_U_macron"
-- #define XKB_KEY_Cyrillic_u_macron          0x10004ef  /* U+04EF CYRILLIC SMALL LETTER U WITH MACRON */
showKeySymbol XKB_KEY_Cyrillic_u_macron = "Cyrillic_u_macron"
-- #define XKB_KEY_Serbian_dje                   0x06a1  /* U+0452 CYRILLIC SMALL LETTER DJE */
showKeySymbol XKB_KEY_Serbian_dje = "Serbian_dje"
-- #define XKB_KEY_Macedonia_gje                 0x06a2  /* U+0453 CYRILLIC SMALL LETTER GJE */
showKeySymbol XKB_KEY_Macedonia_gje = "Macedonia_gje"
-- #define XKB_KEY_Cyrillic_io                   0x06a3  /* U+0451 CYRILLIC SMALL LETTER IO */
showKeySymbol XKB_KEY_Cyrillic_io = "Cyrillic_io"
-- #define XKB_KEY_Ukrainian_ie                  0x06a4  /* U+0454 CYRILLIC SMALL LETTER UKRAINIAN IE */
showKeySymbol XKB_KEY_Ukrainian_ie = "Ukrainian_ie"
-- #define XKB_KEY_Ukranian_je                   0x06a4  /* deprecated */
showKeySymbol XKB_KEY_Ukranian_je = "Ukranian_je"
-- #define XKB_KEY_Macedonia_dse                 0x06a5  /* U+0455 CYRILLIC SMALL LETTER DZE */
showKeySymbol XKB_KEY_Macedonia_dse = "Macedonia_dse"
-- #define XKB_KEY_Ukrainian_i                   0x06a6  /* U+0456 CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I */
showKeySymbol XKB_KEY_Ukrainian_i = "Ukrainian_i"
-- #define XKB_KEY_Ukranian_i                    0x06a6  /* deprecated */
showKeySymbol XKB_KEY_Ukranian_i = "Ukranian_i"
-- #define XKB_KEY_Ukrainian_yi                  0x06a7  /* U+0457 CYRILLIC SMALL LETTER YI */
showKeySymbol XKB_KEY_Ukrainian_yi = "Ukrainian_yi"
-- #define XKB_KEY_Ukranian_yi                   0x06a7  /* deprecated */
showKeySymbol XKB_KEY_Ukranian_yi = "Ukranian_yi"
-- #define XKB_KEY_Cyrillic_je                   0x06a8  /* U+0458 CYRILLIC SMALL LETTER JE */
showKeySymbol XKB_KEY_Cyrillic_je = "Cyrillic_je"
-- #define XKB_KEY_Serbian_je                    0x06a8  /* deprecated */
showKeySymbol XKB_KEY_Serbian_je = "Serbian_je"
-- #define XKB_KEY_Cyrillic_lje                  0x06a9  /* U+0459 CYRILLIC SMALL LETTER LJE */
showKeySymbol XKB_KEY_Cyrillic_lje = "Cyrillic_lje"
-- #define XKB_KEY_Serbian_lje                   0x06a9  /* deprecated */
showKeySymbol XKB_KEY_Serbian_lje = "Serbian_lje"
-- #define XKB_KEY_Cyrillic_nje                  0x06aa  /* U+045A CYRILLIC SMALL LETTER NJE */
showKeySymbol XKB_KEY_Cyrillic_nje = "Cyrillic_nje"
-- #define XKB_KEY_Serbian_nje                   0x06aa  /* deprecated */
showKeySymbol XKB_KEY_Serbian_nje = "Serbian_nje"
-- #define XKB_KEY_Serbian_tshe                  0x06ab  /* U+045B CYRILLIC SMALL LETTER TSHE */
showKeySymbol XKB_KEY_Serbian_tshe = "Serbian_tshe"
-- #define XKB_KEY_Macedonia_kje                 0x06ac  /* U+045C CYRILLIC SMALL LETTER KJE */
showKeySymbol XKB_KEY_Macedonia_kje = "Macedonia_kje"
-- #define XKB_KEY_Ukrainian_ghe_with_upturn     0x06ad  /* U+0491 CYRILLIC SMALL LETTER GHE WITH UPTURN */
showKeySymbol XKB_KEY_Ukrainian_ghe_with_upturn = "Ukrainian_ghe_with_upturn"
-- #define XKB_KEY_Byelorussian_shortu           0x06ae  /* U+045E CYRILLIC SMALL LETTER SHORT U */
showKeySymbol XKB_KEY_Byelorussian_shortu = "Byelorussian_shortu"
-- #define XKB_KEY_Cyrillic_dzhe                 0x06af  /* U+045F CYRILLIC SMALL LETTER DZHE */
showKeySymbol XKB_KEY_Cyrillic_dzhe = "Cyrillic_dzhe"
-- #define XKB_KEY_Serbian_dze                   0x06af  /* deprecated */
showKeySymbol XKB_KEY_Serbian_dze = "Serbian_dze"
-- #define XKB_KEY_numerosign                    0x06b0  /* U+2116 NUMERO SIGN */
showKeySymbol XKB_KEY_numerosign = "numerosign"
-- #define XKB_KEY_Serbian_DJE                   0x06b1  /* U+0402 CYRILLIC CAPITAL LETTER DJE */
showKeySymbol XKB_KEY_Serbian_DJE = "Serbian_DJE"
-- #define XKB_KEY_Macedonia_GJE                 0x06b2  /* U+0403 CYRILLIC CAPITAL LETTER GJE */
showKeySymbol XKB_KEY_Macedonia_GJE = "Macedonia_GJE"
-- #define XKB_KEY_Cyrillic_IO                   0x06b3  /* U+0401 CYRILLIC CAPITAL LETTER IO */
showKeySymbol XKB_KEY_Cyrillic_IO = "Cyrillic_IO"
-- #define XKB_KEY_Ukrainian_IE                  0x06b4  /* U+0404 CYRILLIC CAPITAL LETTER UKRAINIAN IE */
showKeySymbol XKB_KEY_Ukrainian_IE = "Ukrainian_IE"
-- #define XKB_KEY_Ukranian_JE                   0x06b4  /* deprecated */
showKeySymbol XKB_KEY_Ukranian_JE = "Ukranian_JE"
-- #define XKB_KEY_Macedonia_DSE                 0x06b5  /* U+0405 CYRILLIC CAPITAL LETTER DZE */
showKeySymbol XKB_KEY_Macedonia_DSE = "Macedonia_DSE"
-- #define XKB_KEY_Ukrainian_I                   0x06b6  /* U+0406 CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I */
showKeySymbol XKB_KEY_Ukrainian_I = "Ukrainian_I"
-- #define XKB_KEY_Ukranian_I                    0x06b6  /* deprecated */
showKeySymbol XKB_KEY_Ukranian_I = "Ukranian_I"
-- #define XKB_KEY_Ukrainian_YI                  0x06b7  /* U+0407 CYRILLIC CAPITAL LETTER YI */
showKeySymbol XKB_KEY_Ukrainian_YI = "Ukrainian_YI"
-- #define XKB_KEY_Ukranian_YI                   0x06b7  /* deprecated */
showKeySymbol XKB_KEY_Ukranian_YI = "Ukranian_YI"
-- #define XKB_KEY_Cyrillic_JE                   0x06b8  /* U+0408 CYRILLIC CAPITAL LETTER JE */
showKeySymbol XKB_KEY_Cyrillic_JE = "Cyrillic_JE"
-- #define XKB_KEY_Serbian_JE                    0x06b8  /* deprecated */
showKeySymbol XKB_KEY_Serbian_JE = "Serbian_JE"
-- #define XKB_KEY_Cyrillic_LJE                  0x06b9  /* U+0409 CYRILLIC CAPITAL LETTER LJE */
showKeySymbol XKB_KEY_Cyrillic_LJE = "Cyrillic_LJE"
-- #define XKB_KEY_Serbian_LJE                   0x06b9  /* deprecated */
showKeySymbol XKB_KEY_Serbian_LJE = "Serbian_LJE"
-- #define XKB_KEY_Cyrillic_NJE                  0x06ba  /* U+040A CYRILLIC CAPITAL LETTER NJE */
showKeySymbol XKB_KEY_Cyrillic_NJE = "Cyrillic_NJE"
-- #define XKB_KEY_Serbian_NJE                   0x06ba  /* deprecated */
showKeySymbol XKB_KEY_Serbian_NJE = "Serbian_NJE"
-- #define XKB_KEY_Serbian_TSHE                  0x06bb  /* U+040B CYRILLIC CAPITAL LETTER TSHE */
showKeySymbol XKB_KEY_Serbian_TSHE = "Serbian_TSHE"
-- #define XKB_KEY_Macedonia_KJE                 0x06bc  /* U+040C CYRILLIC CAPITAL LETTER KJE */
showKeySymbol XKB_KEY_Macedonia_KJE = "Macedonia_KJE"
-- #define XKB_KEY_Ukrainian_GHE_WITH_UPTURN     0x06bd  /* U+0490 CYRILLIC CAPITAL LETTER GHE WITH UPTURN */
showKeySymbol XKB_KEY_Ukrainian_GHE_WITH_UPTURN = "Ukrainian_GHE_WITH_UPTURN"
-- #define XKB_KEY_Byelorussian_SHORTU           0x06be  /* U+040E CYRILLIC CAPITAL LETTER SHORT U */
showKeySymbol XKB_KEY_Byelorussian_SHORTU = "Byelorussian_SHORTU"
-- #define XKB_KEY_Cyrillic_DZHE                 0x06bf  /* U+040F CYRILLIC CAPITAL LETTER DZHE */
showKeySymbol XKB_KEY_Cyrillic_DZHE = "Cyrillic_DZHE"
-- #define XKB_KEY_Serbian_DZE                   0x06bf  /* deprecated */
showKeySymbol XKB_KEY_Serbian_DZE = "Serbian_DZE"
-- #define XKB_KEY_Cyrillic_yu                   0x06c0  /* U+044E CYRILLIC SMALL LETTER YU */
showKeySymbol XKB_KEY_Cyrillic_yu = "Cyrillic_yu"
-- #define XKB_KEY_Cyrillic_a                    0x06c1  /* U+0430 CYRILLIC SMALL LETTER A */
showKeySymbol XKB_KEY_Cyrillic_a = "Cyrillic_a"
-- #define XKB_KEY_Cyrillic_be                   0x06c2  /* U+0431 CYRILLIC SMALL LETTER BE */
showKeySymbol XKB_KEY_Cyrillic_be = "Cyrillic_be"
-- #define XKB_KEY_Cyrillic_tse                  0x06c3  /* U+0446 CYRILLIC SMALL LETTER TSE */
showKeySymbol XKB_KEY_Cyrillic_tse = "Cyrillic_tse"
-- #define XKB_KEY_Cyrillic_de                   0x06c4  /* U+0434 CYRILLIC SMALL LETTER DE */
showKeySymbol XKB_KEY_Cyrillic_de = "Cyrillic_de"
-- #define XKB_KEY_Cyrillic_ie                   0x06c5  /* U+0435 CYRILLIC SMALL LETTER IE */
showKeySymbol XKB_KEY_Cyrillic_ie = "Cyrillic_ie"
-- #define XKB_KEY_Cyrillic_ef                   0x06c6  /* U+0444 CYRILLIC SMALL LETTER EF */
showKeySymbol XKB_KEY_Cyrillic_ef = "Cyrillic_ef"
-- #define XKB_KEY_Cyrillic_ghe                  0x06c7  /* U+0433 CYRILLIC SMALL LETTER GHE */
showKeySymbol XKB_KEY_Cyrillic_ghe = "Cyrillic_ghe"
-- #define XKB_KEY_Cyrillic_ha                   0x06c8  /* U+0445 CYRILLIC SMALL LETTER HA */
showKeySymbol XKB_KEY_Cyrillic_ha = "Cyrillic_ha"
-- #define XKB_KEY_Cyrillic_i                    0x06c9  /* U+0438 CYRILLIC SMALL LETTER I */
showKeySymbol XKB_KEY_Cyrillic_i = "Cyrillic_i"
-- #define XKB_KEY_Cyrillic_shorti               0x06ca  /* U+0439 CYRILLIC SMALL LETTER SHORT I */
showKeySymbol XKB_KEY_Cyrillic_shorti = "Cyrillic_shorti"
-- #define XKB_KEY_Cyrillic_ka                   0x06cb  /* U+043A CYRILLIC SMALL LETTER KA */
showKeySymbol XKB_KEY_Cyrillic_ka = "Cyrillic_ka"
-- #define XKB_KEY_Cyrillic_el                   0x06cc  /* U+043B CYRILLIC SMALL LETTER EL */
showKeySymbol XKB_KEY_Cyrillic_el = "Cyrillic_el"
-- #define XKB_KEY_Cyrillic_em                   0x06cd  /* U+043C CYRILLIC SMALL LETTER EM */
showKeySymbol XKB_KEY_Cyrillic_em = "Cyrillic_em"
-- #define XKB_KEY_Cyrillic_en                   0x06ce  /* U+043D CYRILLIC SMALL LETTER EN */
showKeySymbol XKB_KEY_Cyrillic_en = "Cyrillic_en"
-- #define XKB_KEY_Cyrillic_o                    0x06cf  /* U+043E CYRILLIC SMALL LETTER O */
showKeySymbol XKB_KEY_Cyrillic_o = "Cyrillic_o"
-- #define XKB_KEY_Cyrillic_pe                   0x06d0  /* U+043F CYRILLIC SMALL LETTER PE */
showKeySymbol XKB_KEY_Cyrillic_pe = "Cyrillic_pe"
-- #define XKB_KEY_Cyrillic_ya                   0x06d1  /* U+044F CYRILLIC SMALL LETTER YA */
showKeySymbol XKB_KEY_Cyrillic_ya = "Cyrillic_ya"
-- #define XKB_KEY_Cyrillic_er                   0x06d2  /* U+0440 CYRILLIC SMALL LETTER ER */
showKeySymbol XKB_KEY_Cyrillic_er = "Cyrillic_er"
-- #define XKB_KEY_Cyrillic_es                   0x06d3  /* U+0441 CYRILLIC SMALL LETTER ES */
showKeySymbol XKB_KEY_Cyrillic_es = "Cyrillic_es"
-- #define XKB_KEY_Cyrillic_te                   0x06d4  /* U+0442 CYRILLIC SMALL LETTER TE */
showKeySymbol XKB_KEY_Cyrillic_te = "Cyrillic_te"
-- #define XKB_KEY_Cyrillic_u                    0x06d5  /* U+0443 CYRILLIC SMALL LETTER U */
showKeySymbol XKB_KEY_Cyrillic_u = "Cyrillic_u"
-- #define XKB_KEY_Cyrillic_zhe                  0x06d6  /* U+0436 CYRILLIC SMALL LETTER ZHE */
showKeySymbol XKB_KEY_Cyrillic_zhe = "Cyrillic_zhe"
-- #define XKB_KEY_Cyrillic_ve                   0x06d7  /* U+0432 CYRILLIC SMALL LETTER VE */
showKeySymbol XKB_KEY_Cyrillic_ve = "Cyrillic_ve"
-- #define XKB_KEY_Cyrillic_softsign             0x06d8  /* U+044C CYRILLIC SMALL LETTER SOFT SIGN */
showKeySymbol XKB_KEY_Cyrillic_softsign = "Cyrillic_softsign"
-- #define XKB_KEY_Cyrillic_yeru                 0x06d9  /* U+044B CYRILLIC SMALL LETTER YERU */
showKeySymbol XKB_KEY_Cyrillic_yeru = "Cyrillic_yeru"
-- #define XKB_KEY_Cyrillic_ze                   0x06da  /* U+0437 CYRILLIC SMALL LETTER ZE */
showKeySymbol XKB_KEY_Cyrillic_ze = "Cyrillic_ze"
-- #define XKB_KEY_Cyrillic_sha                  0x06db  /* U+0448 CYRILLIC SMALL LETTER SHA */
showKeySymbol XKB_KEY_Cyrillic_sha = "Cyrillic_sha"
-- #define XKB_KEY_Cyrillic_e                    0x06dc  /* U+044D CYRILLIC SMALL LETTER E */
showKeySymbol XKB_KEY_Cyrillic_e = "Cyrillic_e"
-- #define XKB_KEY_Cyrillic_shcha                0x06dd  /* U+0449 CYRILLIC SMALL LETTER SHCHA */
showKeySymbol XKB_KEY_Cyrillic_shcha = "Cyrillic_shcha"
-- #define XKB_KEY_Cyrillic_che                  0x06de  /* U+0447 CYRILLIC SMALL LETTER CHE */
showKeySymbol XKB_KEY_Cyrillic_che = "Cyrillic_che"
-- #define XKB_KEY_Cyrillic_hardsign             0x06df  /* U+044A CYRILLIC SMALL LETTER HARD SIGN */
showKeySymbol XKB_KEY_Cyrillic_hardsign = "Cyrillic_hardsign"
-- #define XKB_KEY_Cyrillic_YU                   0x06e0  /* U+042E CYRILLIC CAPITAL LETTER YU */
showKeySymbol XKB_KEY_Cyrillic_YU = "Cyrillic_YU"
-- #define XKB_KEY_Cyrillic_A                    0x06e1  /* U+0410 CYRILLIC CAPITAL LETTER A */
showKeySymbol XKB_KEY_Cyrillic_A = "Cyrillic_A"
-- #define XKB_KEY_Cyrillic_BE                   0x06e2  /* U+0411 CYRILLIC CAPITAL LETTER BE */
showKeySymbol XKB_KEY_Cyrillic_BE = "Cyrillic_BE"
-- #define XKB_KEY_Cyrillic_TSE                  0x06e3  /* U+0426 CYRILLIC CAPITAL LETTER TSE */
showKeySymbol XKB_KEY_Cyrillic_TSE = "Cyrillic_TSE"
-- #define XKB_KEY_Cyrillic_DE                   0x06e4  /* U+0414 CYRILLIC CAPITAL LETTER DE */
showKeySymbol XKB_KEY_Cyrillic_DE = "Cyrillic_DE"
-- #define XKB_KEY_Cyrillic_IE                   0x06e5  /* U+0415 CYRILLIC CAPITAL LETTER IE */
showKeySymbol XKB_KEY_Cyrillic_IE = "Cyrillic_IE"
-- #define XKB_KEY_Cyrillic_EF                   0x06e6  /* U+0424 CYRILLIC CAPITAL LETTER EF */
showKeySymbol XKB_KEY_Cyrillic_EF = "Cyrillic_EF"
-- #define XKB_KEY_Cyrillic_GHE                  0x06e7  /* U+0413 CYRILLIC CAPITAL LETTER GHE */
showKeySymbol XKB_KEY_Cyrillic_GHE = "Cyrillic_GHE"
-- #define XKB_KEY_Cyrillic_HA                   0x06e8  /* U+0425 CYRILLIC CAPITAL LETTER HA */
showKeySymbol XKB_KEY_Cyrillic_HA = "Cyrillic_HA"
-- #define XKB_KEY_Cyrillic_I                    0x06e9  /* U+0418 CYRILLIC CAPITAL LETTER I */
showKeySymbol XKB_KEY_Cyrillic_I = "Cyrillic_I"
-- #define XKB_KEY_Cyrillic_SHORTI               0x06ea  /* U+0419 CYRILLIC CAPITAL LETTER SHORT I */
showKeySymbol XKB_KEY_Cyrillic_SHORTI = "Cyrillic_SHORTI"
-- #define XKB_KEY_Cyrillic_KA                   0x06eb  /* U+041A CYRILLIC CAPITAL LETTER KA */
showKeySymbol XKB_KEY_Cyrillic_KA = "Cyrillic_KA"
-- #define XKB_KEY_Cyrillic_EL                   0x06ec  /* U+041B CYRILLIC CAPITAL LETTER EL */
showKeySymbol XKB_KEY_Cyrillic_EL = "Cyrillic_EL"
-- #define XKB_KEY_Cyrillic_EM                   0x06ed  /* U+041C CYRILLIC CAPITAL LETTER EM */
showKeySymbol XKB_KEY_Cyrillic_EM = "Cyrillic_EM"
-- #define XKB_KEY_Cyrillic_EN                   0x06ee  /* U+041D CYRILLIC CAPITAL LETTER EN */
showKeySymbol XKB_KEY_Cyrillic_EN = "Cyrillic_EN"
-- #define XKB_KEY_Cyrillic_O                    0x06ef  /* U+041E CYRILLIC CAPITAL LETTER O */
showKeySymbol XKB_KEY_Cyrillic_O = "Cyrillic_O"
-- #define XKB_KEY_Cyrillic_PE                   0x06f0  /* U+041F CYRILLIC CAPITAL LETTER PE */
showKeySymbol XKB_KEY_Cyrillic_PE = "Cyrillic_PE"
-- #define XKB_KEY_Cyrillic_YA                   0x06f1  /* U+042F CYRILLIC CAPITAL LETTER YA */
showKeySymbol XKB_KEY_Cyrillic_YA = "Cyrillic_YA"
-- #define XKB_KEY_Cyrillic_ER                   0x06f2  /* U+0420 CYRILLIC CAPITAL LETTER ER */
showKeySymbol XKB_KEY_Cyrillic_ER = "Cyrillic_ER"
-- #define XKB_KEY_Cyrillic_ES                   0x06f3  /* U+0421 CYRILLIC CAPITAL LETTER ES */
showKeySymbol XKB_KEY_Cyrillic_ES = "Cyrillic_ES"
-- #define XKB_KEY_Cyrillic_TE                   0x06f4  /* U+0422 CYRILLIC CAPITAL LETTER TE */
showKeySymbol XKB_KEY_Cyrillic_TE = "Cyrillic_TE"
-- #define XKB_KEY_Cyrillic_U                    0x06f5  /* U+0423 CYRILLIC CAPITAL LETTER U */
showKeySymbol XKB_KEY_Cyrillic_U = "Cyrillic_U"
-- #define XKB_KEY_Cyrillic_ZHE                  0x06f6  /* U+0416 CYRILLIC CAPITAL LETTER ZHE */
showKeySymbol XKB_KEY_Cyrillic_ZHE = "Cyrillic_ZHE"
-- #define XKB_KEY_Cyrillic_VE                   0x06f7  /* U+0412 CYRILLIC CAPITAL LETTER VE */
showKeySymbol XKB_KEY_Cyrillic_VE = "Cyrillic_VE"
-- #define XKB_KEY_Cyrillic_SOFTSIGN             0x06f8  /* U+042C CYRILLIC CAPITAL LETTER SOFT SIGN */
showKeySymbol XKB_KEY_Cyrillic_SOFTSIGN = "Cyrillic_SOFTSIGN"
-- #define XKB_KEY_Cyrillic_YERU                 0x06f9  /* U+042B CYRILLIC CAPITAL LETTER YERU */
showKeySymbol XKB_KEY_Cyrillic_YERU = "Cyrillic_YERU"
-- #define XKB_KEY_Cyrillic_ZE                   0x06fa  /* U+0417 CYRILLIC CAPITAL LETTER ZE */
showKeySymbol XKB_KEY_Cyrillic_ZE = "Cyrillic_ZE"
-- #define XKB_KEY_Cyrillic_SHA                  0x06fb  /* U+0428 CYRILLIC CAPITAL LETTER SHA */
showKeySymbol XKB_KEY_Cyrillic_SHA = "Cyrillic_SHA"
-- #define XKB_KEY_Cyrillic_E                    0x06fc  /* U+042D CYRILLIC CAPITAL LETTER E */
showKeySymbol XKB_KEY_Cyrillic_E = "Cyrillic_E"
-- #define XKB_KEY_Cyrillic_SHCHA                0x06fd  /* U+0429 CYRILLIC CAPITAL LETTER SHCHA */
showKeySymbol XKB_KEY_Cyrillic_SHCHA = "Cyrillic_SHCHA"
-- #define XKB_KEY_Cyrillic_CHE                  0x06fe  /* U+0427 CYRILLIC CAPITAL LETTER CHE */
showKeySymbol XKB_KEY_Cyrillic_CHE = "Cyrillic_CHE"
-- #define XKB_KEY_Cyrillic_HARDSIGN             0x06ff  /* U+042A CYRILLIC CAPITAL LETTER HARD SIGN */
showKeySymbol XKB_KEY_Cyrillic_HARDSIGN = "Cyrillic_HARDSIGN"
-- #define XKB_KEY_Greek_ALPHAaccent             0x07a1  /* U+0386 GREEK CAPITAL LETTER ALPHA WITH TONOS */
showKeySymbol XKB_KEY_Greek_ALPHAaccent = "Greek_ALPHAaccent"
-- #define XKB_KEY_Greek_EPSILONaccent           0x07a2  /* U+0388 GREEK CAPITAL LETTER EPSILON WITH TONOS */
showKeySymbol XKB_KEY_Greek_EPSILONaccent = "Greek_EPSILONaccent"
-- #define XKB_KEY_Greek_ETAaccent               0x07a3  /* U+0389 GREEK CAPITAL LETTER ETA WITH TONOS */
showKeySymbol XKB_KEY_Greek_ETAaccent = "Greek_ETAaccent"
-- #define XKB_KEY_Greek_IOTAaccent              0x07a4  /* U+038A GREEK CAPITAL LETTER IOTA WITH TONOS */
showKeySymbol XKB_KEY_Greek_IOTAaccent = "Greek_IOTAaccent"
-- #define XKB_KEY_Greek_IOTAdieresis            0x07a5  /* U+03AA GREEK CAPITAL LETTER IOTA WITH DIALYTIKA */
showKeySymbol XKB_KEY_Greek_IOTAdieresis = "Greek_IOTAdieresis"
-- #define XKB_KEY_Greek_IOTAdiaeresis           0x07a5  /* old typo */
showKeySymbol XKB_KEY_Greek_IOTAdiaeresis = "Greek_IOTAdiaeresis"
-- #define XKB_KEY_Greek_OMICRONaccent           0x07a7  /* U+038C GREEK CAPITAL LETTER OMICRON WITH TONOS */
showKeySymbol XKB_KEY_Greek_OMICRONaccent = "Greek_OMICRONaccent"
-- #define XKB_KEY_Greek_UPSILONaccent           0x07a8  /* U+038E GREEK CAPITAL LETTER UPSILON WITH TONOS */
showKeySymbol XKB_KEY_Greek_UPSILONaccent = "Greek_UPSILONaccent"
-- #define XKB_KEY_Greek_UPSILONdieresis         0x07a9  /* U+03AB GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA */
showKeySymbol XKB_KEY_Greek_UPSILONdieresis = "Greek_UPSILONdieresis"
-- #define XKB_KEY_Greek_OMEGAaccent             0x07ab  /* U+038F GREEK CAPITAL LETTER OMEGA WITH TONOS */
showKeySymbol XKB_KEY_Greek_OMEGAaccent = "Greek_OMEGAaccent"
-- #define XKB_KEY_Greek_accentdieresis          0x07ae  /* U+0385 GREEK DIALYTIKA TONOS */
showKeySymbol XKB_KEY_Greek_accentdieresis = "Greek_accentdieresis"
-- #define XKB_KEY_Greek_horizbar                0x07af  /* U+2015 HORIZONTAL BAR */
showKeySymbol XKB_KEY_Greek_horizbar = "Greek_horizbar"
-- #define XKB_KEY_Greek_alphaaccent             0x07b1  /* U+03AC GREEK SMALL LETTER ALPHA WITH TONOS */
showKeySymbol XKB_KEY_Greek_alphaaccent = "Greek_alphaaccent"
-- #define XKB_KEY_Greek_epsilonaccent           0x07b2  /* U+03AD GREEK SMALL LETTER EPSILON WITH TONOS */
showKeySymbol XKB_KEY_Greek_epsilonaccent = "Greek_epsilonaccent"
-- #define XKB_KEY_Greek_etaaccent               0x07b3  /* U+03AE GREEK SMALL LETTER ETA WITH TONOS */
showKeySymbol XKB_KEY_Greek_etaaccent = "Greek_etaaccent"
-- #define XKB_KEY_Greek_iotaaccent              0x07b4  /* U+03AF GREEK SMALL LETTER IOTA WITH TONOS */
showKeySymbol XKB_KEY_Greek_iotaaccent = "Greek_iotaaccent"
-- #define XKB_KEY_Greek_iotadieresis            0x07b5  /* U+03CA GREEK SMALL LETTER IOTA WITH DIALYTIKA */
showKeySymbol XKB_KEY_Greek_iotadieresis = "Greek_iotadieresis"
-- #define XKB_KEY_Greek_iotaaccentdieresis      0x07b6  /* U+0390 GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS */
showKeySymbol XKB_KEY_Greek_iotaaccentdieresis = "Greek_iotaaccentdieresis"
-- #define XKB_KEY_Greek_omicronaccent           0x07b7  /* U+03CC GREEK SMALL LETTER OMICRON WITH TONOS */
showKeySymbol XKB_KEY_Greek_omicronaccent = "Greek_omicronaccent"
-- #define XKB_KEY_Greek_upsilonaccent           0x07b8  /* U+03CD GREEK SMALL LETTER UPSILON WITH TONOS */
showKeySymbol XKB_KEY_Greek_upsilonaccent = "Greek_upsilonaccent"
-- #define XKB_KEY_Greek_upsilondieresis         0x07b9  /* U+03CB GREEK SMALL LETTER UPSILON WITH DIALYTIKA */
showKeySymbol XKB_KEY_Greek_upsilondieresis = "Greek_upsilondieresis"
-- #define XKB_KEY_Greek_upsilonaccentdieresis   0x07ba  /* U+03B0 GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS */
showKeySymbol XKB_KEY_Greek_upsilonaccentdieresis = "Greek_upsilonaccentdieresis"
-- #define XKB_KEY_Greek_omegaaccent             0x07bb  /* U+03CE GREEK SMALL LETTER OMEGA WITH TONOS */
showKeySymbol XKB_KEY_Greek_omegaaccent = "Greek_omegaaccent"
-- #define XKB_KEY_Greek_ALPHA                   0x07c1  /* U+0391 GREEK CAPITAL LETTER ALPHA */
showKeySymbol XKB_KEY_Greek_ALPHA = "Greek_ALPHA"
-- #define XKB_KEY_Greek_BETA                    0x07c2  /* U+0392 GREEK CAPITAL LETTER BETA */
showKeySymbol XKB_KEY_Greek_BETA = "Greek_BETA"
-- #define XKB_KEY_Greek_GAMMA                   0x07c3  /* U+0393 GREEK CAPITAL LETTER GAMMA */
showKeySymbol XKB_KEY_Greek_GAMMA = "Greek_GAMMA"
-- #define XKB_KEY_Greek_DELTA                   0x07c4  /* U+0394 GREEK CAPITAL LETTER DELTA */
showKeySymbol XKB_KEY_Greek_DELTA = "Greek_DELTA"
-- #define XKB_KEY_Greek_EPSILON                 0x07c5  /* U+0395 GREEK CAPITAL LETTER EPSILON */
showKeySymbol XKB_KEY_Greek_EPSILON = "Greek_EPSILON"
-- #define XKB_KEY_Greek_ZETA                    0x07c6  /* U+0396 GREEK CAPITAL LETTER ZETA */
showKeySymbol XKB_KEY_Greek_ZETA = "Greek_ZETA"
-- #define XKB_KEY_Greek_ETA                     0x07c7  /* U+0397 GREEK CAPITAL LETTER ETA */
showKeySymbol XKB_KEY_Greek_ETA = "Greek_ETA"
-- #define XKB_KEY_Greek_THETA                   0x07c8  /* U+0398 GREEK CAPITAL LETTER THETA */
showKeySymbol XKB_KEY_Greek_THETA = "Greek_THETA"
-- #define XKB_KEY_Greek_IOTA                    0x07c9  /* U+0399 GREEK CAPITAL LETTER IOTA */
showKeySymbol XKB_KEY_Greek_IOTA = "Greek_IOTA"
-- #define XKB_KEY_Greek_KAPPA                   0x07ca  /* U+039A GREEK CAPITAL LETTER KAPPA */
showKeySymbol XKB_KEY_Greek_KAPPA = "Greek_KAPPA"
-- #define XKB_KEY_Greek_LAMDA                   0x07cb  /* U+039B GREEK CAPITAL LETTER LAMDA */
showKeySymbol XKB_KEY_Greek_LAMDA = "Greek_LAMDA"
-- #define XKB_KEY_Greek_LAMBDA                  0x07cb  /* U+039B GREEK CAPITAL LETTER LAMDA */
showKeySymbol XKB_KEY_Greek_LAMBDA = "Greek_LAMBDA"
-- #define XKB_KEY_Greek_MU                      0x07cc  /* U+039C GREEK CAPITAL LETTER MU */
showKeySymbol XKB_KEY_Greek_MU = "Greek_MU"
-- #define XKB_KEY_Greek_NU                      0x07cd  /* U+039D GREEK CAPITAL LETTER NU */
showKeySymbol XKB_KEY_Greek_NU = "Greek_NU"
-- #define XKB_KEY_Greek_XI                      0x07ce  /* U+039E GREEK CAPITAL LETTER XI */
showKeySymbol XKB_KEY_Greek_XI = "Greek_XI"
-- #define XKB_KEY_Greek_OMICRON                 0x07cf  /* U+039F GREEK CAPITAL LETTER OMICRON */
showKeySymbol XKB_KEY_Greek_OMICRON = "Greek_OMICRON"
-- #define XKB_KEY_Greek_PI                      0x07d0  /* U+03A0 GREEK CAPITAL LETTER PI */
showKeySymbol XKB_KEY_Greek_PI = "Greek_PI"
-- #define XKB_KEY_Greek_RHO                     0x07d1  /* U+03A1 GREEK CAPITAL LETTER RHO */
showKeySymbol XKB_KEY_Greek_RHO = "Greek_RHO"
-- #define XKB_KEY_Greek_SIGMA                   0x07d2  /* U+03A3 GREEK CAPITAL LETTER SIGMA */
showKeySymbol XKB_KEY_Greek_SIGMA = "Greek_SIGMA"
-- #define XKB_KEY_Greek_TAU                     0x07d4  /* U+03A4 GREEK CAPITAL LETTER TAU */
showKeySymbol XKB_KEY_Greek_TAU = "Greek_TAU"
-- #define XKB_KEY_Greek_UPSILON                 0x07d5  /* U+03A5 GREEK CAPITAL LETTER UPSILON */
showKeySymbol XKB_KEY_Greek_UPSILON = "Greek_UPSILON"
-- #define XKB_KEY_Greek_PHI                     0x07d6  /* U+03A6 GREEK CAPITAL LETTER PHI */
showKeySymbol XKB_KEY_Greek_PHI = "Greek_PHI"
-- #define XKB_KEY_Greek_CHI                     0x07d7  /* U+03A7 GREEK CAPITAL LETTER CHI */
showKeySymbol XKB_KEY_Greek_CHI = "Greek_CHI"
-- #define XKB_KEY_Greek_PSI                     0x07d8  /* U+03A8 GREEK CAPITAL LETTER PSI */
showKeySymbol XKB_KEY_Greek_PSI = "Greek_PSI"
-- #define XKB_KEY_Greek_OMEGA                   0x07d9  /* U+03A9 GREEK CAPITAL LETTER OMEGA */
showKeySymbol XKB_KEY_Greek_OMEGA = "Greek_OMEGA"
-- #define XKB_KEY_Greek_alpha                   0x07e1  /* U+03B1 GREEK SMALL LETTER ALPHA */
showKeySymbol XKB_KEY_Greek_alpha = "Greek_alpha"
-- #define XKB_KEY_Greek_beta                    0x07e2  /* U+03B2 GREEK SMALL LETTER BETA */
showKeySymbol XKB_KEY_Greek_beta = "Greek_beta"
-- #define XKB_KEY_Greek_gamma                   0x07e3  /* U+03B3 GREEK SMALL LETTER GAMMA */
showKeySymbol XKB_KEY_Greek_gamma = "Greek_gamma"
-- #define XKB_KEY_Greek_delta                   0x07e4  /* U+03B4 GREEK SMALL LETTER DELTA */
showKeySymbol XKB_KEY_Greek_delta = "Greek_delta"
-- #define XKB_KEY_Greek_epsilon                 0x07e5  /* U+03B5 GREEK SMALL LETTER EPSILON */
showKeySymbol XKB_KEY_Greek_epsilon = "Greek_epsilon"
-- #define XKB_KEY_Greek_zeta                    0x07e6  /* U+03B6 GREEK SMALL LETTER ZETA */
showKeySymbol XKB_KEY_Greek_zeta = "Greek_zeta"
-- #define XKB_KEY_Greek_eta                     0x07e7  /* U+03B7 GREEK SMALL LETTER ETA */
showKeySymbol XKB_KEY_Greek_eta = "Greek_eta"
-- #define XKB_KEY_Greek_theta                   0x07e8  /* U+03B8 GREEK SMALL LETTER THETA */
showKeySymbol XKB_KEY_Greek_theta = "Greek_theta"
-- #define XKB_KEY_Greek_iota                    0x07e9  /* U+03B9 GREEK SMALL LETTER IOTA */
showKeySymbol XKB_KEY_Greek_iota = "Greek_iota"
-- #define XKB_KEY_Greek_kappa                   0x07ea  /* U+03BA GREEK SMALL LETTER KAPPA */
showKeySymbol XKB_KEY_Greek_kappa = "Greek_kappa"
-- #define XKB_KEY_Greek_lamda                   0x07eb  /* U+03BB GREEK SMALL LETTER LAMDA */
showKeySymbol XKB_KEY_Greek_lamda = "Greek_lamda"
-- #define XKB_KEY_Greek_lambda                  0x07eb  /* U+03BB GREEK SMALL LETTER LAMDA */
showKeySymbol XKB_KEY_Greek_lambda = "Greek_lambda"
-- #define XKB_KEY_Greek_mu                      0x07ec  /* U+03BC GREEK SMALL LETTER MU */
showKeySymbol XKB_KEY_Greek_mu = "Greek_mu"
-- #define XKB_KEY_Greek_nu                      0x07ed  /* U+03BD GREEK SMALL LETTER NU */
showKeySymbol XKB_KEY_Greek_nu = "Greek_nu"
-- #define XKB_KEY_Greek_xi                      0x07ee  /* U+03BE GREEK SMALL LETTER XI */
showKeySymbol XKB_KEY_Greek_xi = "Greek_xi"
-- #define XKB_KEY_Greek_omicron                 0x07ef  /* U+03BF GREEK SMALL LETTER OMICRON */
showKeySymbol XKB_KEY_Greek_omicron = "Greek_omicron"
-- #define XKB_KEY_Greek_pi                      0x07f0  /* U+03C0 GREEK SMALL LETTER PI */
showKeySymbol XKB_KEY_Greek_pi = "Greek_pi"
-- #define XKB_KEY_Greek_rho                     0x07f1  /* U+03C1 GREEK SMALL LETTER RHO */
showKeySymbol XKB_KEY_Greek_rho = "Greek_rho"
-- #define XKB_KEY_Greek_sigma                   0x07f2  /* U+03C3 GREEK SMALL LETTER SIGMA */
showKeySymbol XKB_KEY_Greek_sigma = "Greek_sigma"
-- #define XKB_KEY_Greek_finalsmallsigma         0x07f3  /* U+03C2 GREEK SMALL LETTER FINAL SIGMA */
showKeySymbol XKB_KEY_Greek_finalsmallsigma = "Greek_finalsmallsigma"
-- #define XKB_KEY_Greek_tau                     0x07f4  /* U+03C4 GREEK SMALL LETTER TAU */
showKeySymbol XKB_KEY_Greek_tau = "Greek_tau"
-- #define XKB_KEY_Greek_upsilon                 0x07f5  /* U+03C5 GREEK SMALL LETTER UPSILON */
showKeySymbol XKB_KEY_Greek_upsilon = "Greek_upsilon"
-- #define XKB_KEY_Greek_phi                     0x07f6  /* U+03C6 GREEK SMALL LETTER PHI */
showKeySymbol XKB_KEY_Greek_phi = "Greek_phi"
-- #define XKB_KEY_Greek_chi                     0x07f7  /* U+03C7 GREEK SMALL LETTER CHI */
showKeySymbol XKB_KEY_Greek_chi = "Greek_chi"
-- #define XKB_KEY_Greek_psi                     0x07f8  /* U+03C8 GREEK SMALL LETTER PSI */
showKeySymbol XKB_KEY_Greek_psi = "Greek_psi"
-- #define XKB_KEY_Greek_omega                   0x07f9  /* U+03C9 GREEK SMALL LETTER OMEGA */
showKeySymbol XKB_KEY_Greek_omega = "Greek_omega"
-- #define XKB_KEY_Greek_switch                  0xff7e  /* Alias for mode_switch */
showKeySymbol XKB_KEY_Greek_switch = "Greek_switch"
-- #define XKB_KEY_leftradical                   0x08a1  /* U+23B7 RADICAL SYMBOL BOTTOM */
showKeySymbol XKB_KEY_leftradical = "leftradical"
-- #define XKB_KEY_topleftradical                0x08a2  /*(U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT)*/
showKeySymbol XKB_KEY_topleftradical = "topleftradical"
-- #define XKB_KEY_horizconnector                0x08a3  /*(U+2500 BOX DRAWINGS LIGHT HORIZONTAL)*/
showKeySymbol XKB_KEY_horizconnector = "horizconnector"
-- #define XKB_KEY_topintegral                   0x08a4  /* U+2320 TOP HALF INTEGRAL */
showKeySymbol XKB_KEY_topintegral = "topintegral"
-- #define XKB_KEY_botintegral                   0x08a5  /* U+2321 BOTTOM HALF INTEGRAL */
showKeySymbol XKB_KEY_botintegral = "botintegral"
-- #define XKB_KEY_vertconnector                 0x08a6  /*(U+2502 BOX DRAWINGS LIGHT VERTICAL)*/
showKeySymbol XKB_KEY_vertconnector = "vertconnector"
-- #define XKB_KEY_topleftsqbracket              0x08a7  /* U+23A1 LEFT SQUARE BRACKET UPPER CORNER */
showKeySymbol XKB_KEY_topleftsqbracket = "topleftsqbracket"
-- #define XKB_KEY_botleftsqbracket              0x08a8  /* U+23A3 LEFT SQUARE BRACKET LOWER CORNER */
showKeySymbol XKB_KEY_botleftsqbracket = "botleftsqbracket"
-- #define XKB_KEY_toprightsqbracket             0x08a9  /* U+23A4 RIGHT SQUARE BRACKET UPPER CORNER */
showKeySymbol XKB_KEY_toprightsqbracket = "toprightsqbracket"
-- #define XKB_KEY_botrightsqbracket             0x08aa  /* U+23A6 RIGHT SQUARE BRACKET LOWER CORNER */
showKeySymbol XKB_KEY_botrightsqbracket = "botrightsqbracket"
-- #define XKB_KEY_topleftparens                 0x08ab  /* U+239B LEFT PARENTHESIS UPPER HOOK */
showKeySymbol XKB_KEY_topleftparens = "topleftparens"
-- #define XKB_KEY_botleftparens                 0x08ac  /* U+239D LEFT PARENTHESIS LOWER HOOK */
showKeySymbol XKB_KEY_botleftparens = "botleftparens"
-- #define XKB_KEY_toprightparens                0x08ad  /* U+239E RIGHT PARENTHESIS UPPER HOOK */
showKeySymbol XKB_KEY_toprightparens = "toprightparens"
-- #define XKB_KEY_botrightparens                0x08ae  /* U+23A0 RIGHT PARENTHESIS LOWER HOOK */
showKeySymbol XKB_KEY_botrightparens = "botrightparens"
-- #define XKB_KEY_leftmiddlecurlybrace          0x08af  /* U+23A8 LEFT CURLY BRACKET MIDDLE PIECE */
showKeySymbol XKB_KEY_leftmiddlecurlybrace = "leftmiddlecurlybrace"
-- #define XKB_KEY_rightmiddlecurlybrace         0x08b0  /* U+23AC RIGHT CURLY BRACKET MIDDLE PIECE */
showKeySymbol XKB_KEY_rightmiddlecurlybrace = "rightmiddlecurlybrace"
-- #define XKB_KEY_topleftsummation              0x08b1
showKeySymbol XKB_KEY_topleftsummation = "topleftsummation"
-- #define XKB_KEY_botleftsummation              0x08b2
showKeySymbol XKB_KEY_botleftsummation = "botleftsummation"
-- #define XKB_KEY_topvertsummationconnector     0x08b3
showKeySymbol XKB_KEY_topvertsummationconnector = "topvertsummationconnector"
-- #define XKB_KEY_botvertsummationconnector     0x08b4
showKeySymbol XKB_KEY_botvertsummationconnector = "botvertsummationconnector"
-- #define XKB_KEY_toprightsummation             0x08b5
showKeySymbol XKB_KEY_toprightsummation = "toprightsummation"
-- #define XKB_KEY_botrightsummation             0x08b6
showKeySymbol XKB_KEY_botrightsummation = "botrightsummation"
-- #define XKB_KEY_rightmiddlesummation          0x08b7
showKeySymbol XKB_KEY_rightmiddlesummation = "rightmiddlesummation"
-- #define XKB_KEY_lessthanequal                 0x08bc  /* U+2264 LESS-THAN OR EQUAL TO */
showKeySymbol XKB_KEY_lessthanequal = "lessthanequal"
-- #define XKB_KEY_notequal                      0x08bd  /* U+2260 NOT EQUAL TO */
showKeySymbol XKB_KEY_notequal = "notequal"
-- #define XKB_KEY_greaterthanequal              0x08be  /* U+2265 GREATER-THAN OR EQUAL TO */
showKeySymbol XKB_KEY_greaterthanequal = "greaterthanequal"
-- #define XKB_KEY_integral                      0x08bf  /* U+222B INTEGRAL */
showKeySymbol XKB_KEY_integral = "integral"
-- #define XKB_KEY_therefore                     0x08c0  /* U+2234 THEREFORE */
showKeySymbol XKB_KEY_therefore = "therefore"
-- #define XKB_KEY_variation                     0x08c1  /* U+221D PROPORTIONAL TO */
showKeySymbol XKB_KEY_variation = "variation"
-- #define XKB_KEY_infinity                      0x08c2  /* U+221E INFINITY */
showKeySymbol XKB_KEY_infinity = "infinity"
-- #define XKB_KEY_nabla                         0x08c5  /* U+2207 NABLA */
showKeySymbol XKB_KEY_nabla = "nabla"
-- #define XKB_KEY_approximate                   0x08c8  /* U+223C TILDE OPERATOR */
showKeySymbol XKB_KEY_approximate = "approximate"
-- #define XKB_KEY_similarequal                  0x08c9  /* U+2243 ASYMPTOTICALLY EQUAL TO */
showKeySymbol XKB_KEY_similarequal = "similarequal"
-- #define XKB_KEY_ifonlyif                      0x08cd  /* U+21D4 LEFT RIGHT DOUBLE ARROW */
showKeySymbol XKB_KEY_ifonlyif = "ifonlyif"
-- #define XKB_KEY_implies                       0x08ce  /* U+21D2 RIGHTWARDS DOUBLE ARROW */
showKeySymbol XKB_KEY_implies = "implies"
-- #define XKB_KEY_identical                     0x08cf  /* U+2261 IDENTICAL TO */
showKeySymbol XKB_KEY_identical = "identical"
-- #define XKB_KEY_radical                       0x08d6  /* U+221A SQUARE ROOT */
showKeySymbol XKB_KEY_radical = "radical"
-- #define XKB_KEY_includedin                    0x08da  /* U+2282 SUBSET OF */
showKeySymbol XKB_KEY_includedin = "includedin"
-- #define XKB_KEY_includes                      0x08db  /* U+2283 SUPERSET OF */
showKeySymbol XKB_KEY_includes = "includes"
-- #define XKB_KEY_intersection                  0x08dc  /* U+2229 INTERSECTION */
showKeySymbol XKB_KEY_intersection = "intersection"
-- #define XKB_KEY_union                         0x08dd  /* U+222A UNION */
showKeySymbol XKB_KEY_union = "union"
-- #define XKB_KEY_logicaland                    0x08de  /* U+2227 LOGICAL AND */
showKeySymbol XKB_KEY_logicaland = "logicaland"
-- #define XKB_KEY_logicalor                     0x08df  /* U+2228 LOGICAL OR */
showKeySymbol XKB_KEY_logicalor = "logicalor"
-- #define XKB_KEY_partialderivative             0x08ef  /* U+2202 PARTIAL DIFFERENTIAL */
showKeySymbol XKB_KEY_partialderivative = "partialderivative"
-- #define XKB_KEY_function                      0x08f6  /* U+0192 LATIN SMALL LETTER F WITH HOOK */
showKeySymbol XKB_KEY_function = "function"
-- #define XKB_KEY_leftarrow                     0x08fb  /* U+2190 LEFTWARDS ARROW */
showKeySymbol XKB_KEY_leftarrow = "leftarrow"
-- #define XKB_KEY_uparrow                       0x08fc  /* U+2191 UPWARDS ARROW */
showKeySymbol XKB_KEY_uparrow = "uparrow"
-- #define XKB_KEY_rightarrow                    0x08fd  /* U+2192 RIGHTWARDS ARROW */
showKeySymbol XKB_KEY_rightarrow = "rightarrow"
-- #define XKB_KEY_downarrow                     0x08fe  /* U+2193 DOWNWARDS ARROW */
showKeySymbol XKB_KEY_downarrow = "downarrow"
-- #define XKB_KEY_blank                         0x09df
showKeySymbol XKB_KEY_blank = "blank"
-- #define XKB_KEY_soliddiamond                  0x09e0  /* U+25C6 BLACK DIAMOND */
showKeySymbol XKB_KEY_soliddiamond = "soliddiamond"
-- #define XKB_KEY_checkerboard                  0x09e1  /* U+2592 MEDIUM SHADE */
showKeySymbol XKB_KEY_checkerboard = "checkerboard"
-- #define XKB_KEY_ht                            0x09e2  /* U+2409 SYMBOL FOR HORIZONTAL TABULATION */
showKeySymbol XKB_KEY_ht = "ht"
-- #define XKB_KEY_ff                            0x09e3  /* U+240C SYMBOL FOR FORM FEED */
showKeySymbol XKB_KEY_ff = "ff"
-- #define XKB_KEY_cr                            0x09e4  /* U+240D SYMBOL FOR CARRIAGE RETURN */
showKeySymbol XKB_KEY_cr = "cr"
-- #define XKB_KEY_lf                            0x09e5  /* U+240A SYMBOL FOR LINE FEED */
showKeySymbol XKB_KEY_lf = "lf"
-- #define XKB_KEY_nl                            0x09e8  /* U+2424 SYMBOL FOR NEWLINE */
showKeySymbol XKB_KEY_nl = "nl"
-- #define XKB_KEY_vt                            0x09e9  /* U+240B SYMBOL FOR VERTICAL TABULATION */
showKeySymbol XKB_KEY_vt = "vt"
-- #define XKB_KEY_lowrightcorner                0x09ea  /* U+2518 BOX DRAWINGS LIGHT UP AND LEFT */
showKeySymbol XKB_KEY_lowrightcorner = "lowrightcorner"
-- #define XKB_KEY_uprightcorner                 0x09eb  /* U+2510 BOX DRAWINGS LIGHT DOWN AND LEFT */
showKeySymbol XKB_KEY_uprightcorner = "uprightcorner"
-- #define XKB_KEY_upleftcorner                  0x09ec  /* U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT */
showKeySymbol XKB_KEY_upleftcorner = "upleftcorner"
-- #define XKB_KEY_lowleftcorner                 0x09ed  /* U+2514 BOX DRAWINGS LIGHT UP AND RIGHT */
showKeySymbol XKB_KEY_lowleftcorner = "lowleftcorner"
-- #define XKB_KEY_crossinglines                 0x09ee  /* U+253C BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL */
showKeySymbol XKB_KEY_crossinglines = "crossinglines"
-- #define XKB_KEY_horizlinescan1                0x09ef  /* U+23BA HORIZONTAL SCAN LINE-1 */
showKeySymbol XKB_KEY_horizlinescan1 = "horizlinescan1"
-- #define XKB_KEY_horizlinescan3                0x09f0  /* U+23BB HORIZONTAL SCAN LINE-3 */
showKeySymbol XKB_KEY_horizlinescan3 = "horizlinescan3"
-- #define XKB_KEY_horizlinescan5                0x09f1  /* U+2500 BOX DRAWINGS LIGHT HORIZONTAL */
showKeySymbol XKB_KEY_horizlinescan5 = "horizlinescan5"
-- #define XKB_KEY_horizlinescan7                0x09f2  /* U+23BC HORIZONTAL SCAN LINE-7 */
showKeySymbol XKB_KEY_horizlinescan7 = "horizlinescan7"
-- #define XKB_KEY_horizlinescan9                0x09f3  /* U+23BD HORIZONTAL SCAN LINE-9 */
showKeySymbol XKB_KEY_horizlinescan9 = "horizlinescan9"
-- #define XKB_KEY_leftt                         0x09f4  /* U+251C BOX DRAWINGS LIGHT VERTICAL AND RIGHT */
showKeySymbol XKB_KEY_leftt = "leftt"
-- #define XKB_KEY_rightt                        0x09f5  /* U+2524 BOX DRAWINGS LIGHT VERTICAL AND LEFT */
showKeySymbol XKB_KEY_rightt = "rightt"
-- #define XKB_KEY_bott                          0x09f6  /* U+2534 BOX DRAWINGS LIGHT UP AND HORIZONTAL */
showKeySymbol XKB_KEY_bott = "bott"
-- #define XKB_KEY_topt                          0x09f7  /* U+252C BOX DRAWINGS LIGHT DOWN AND HORIZONTAL */
showKeySymbol XKB_KEY_topt = "topt"
-- #define XKB_KEY_vertbar                       0x09f8  /* U+2502 BOX DRAWINGS LIGHT VERTICAL */
showKeySymbol XKB_KEY_vertbar = "vertbar"
-- #define XKB_KEY_emspace                       0x0aa1  /* U+2003 EM SPACE */
showKeySymbol XKB_KEY_emspace = "emspace"
-- #define XKB_KEY_enspace                       0x0aa2  /* U+2002 EN SPACE */
showKeySymbol XKB_KEY_enspace = "enspace"
-- #define XKB_KEY_em3space                      0x0aa3  /* U+2004 THREE-PER-EM SPACE */
showKeySymbol XKB_KEY_em3space = "em3space"
-- #define XKB_KEY_em4space                      0x0aa4  /* U+2005 FOUR-PER-EM SPACE */
showKeySymbol XKB_KEY_em4space = "em4space"
-- #define XKB_KEY_digitspace                    0x0aa5  /* U+2007 FIGURE SPACE */
showKeySymbol XKB_KEY_digitspace = "digitspace"
-- #define XKB_KEY_punctspace                    0x0aa6  /* U+2008 PUNCTUATION SPACE */
showKeySymbol XKB_KEY_punctspace = "punctspace"
-- #define XKB_KEY_thinspace                     0x0aa7  /* U+2009 THIN SPACE */
showKeySymbol XKB_KEY_thinspace = "thinspace"
-- #define XKB_KEY_hairspace                     0x0aa8  /* U+200A HAIR SPACE */
showKeySymbol XKB_KEY_hairspace = "hairspace"
-- #define XKB_KEY_emdash                        0x0aa9  /* U+2014 EM DASH */
showKeySymbol XKB_KEY_emdash = "emdash"
-- #define XKB_KEY_endash                        0x0aaa  /* U+2013 EN DASH */
showKeySymbol XKB_KEY_endash = "endash"
-- #define XKB_KEY_signifblank                   0x0aac  /*(U+2423 OPEN BOX)*/
showKeySymbol XKB_KEY_signifblank = "signifblank"
-- #define XKB_KEY_ellipsis                      0x0aae  /* U+2026 HORIZONTAL ELLIPSIS */
showKeySymbol XKB_KEY_ellipsis = "ellipsis"
-- #define XKB_KEY_doubbaselinedot               0x0aaf  /* U+2025 TWO DOT LEADER */
showKeySymbol XKB_KEY_doubbaselinedot = "doubbaselinedot"
-- #define XKB_KEY_onethird                      0x0ab0  /* U+2153 VULGAR FRACTION ONE THIRD */
showKeySymbol XKB_KEY_onethird = "onethird"
-- #define XKB_KEY_twothirds                     0x0ab1  /* U+2154 VULGAR FRACTION TWO THIRDS */
showKeySymbol XKB_KEY_twothirds = "twothirds"
-- #define XKB_KEY_onefifth                      0x0ab2  /* U+2155 VULGAR FRACTION ONE FIFTH */
showKeySymbol XKB_KEY_onefifth = "onefifth"
-- #define XKB_KEY_twofifths                     0x0ab3  /* U+2156 VULGAR FRACTION TWO FIFTHS */
showKeySymbol XKB_KEY_twofifths = "twofifths"
-- #define XKB_KEY_threefifths                   0x0ab4  /* U+2157 VULGAR FRACTION THREE FIFTHS */
showKeySymbol XKB_KEY_threefifths = "threefifths"
-- #define XKB_KEY_fourfifths                    0x0ab5  /* U+2158 VULGAR FRACTION FOUR FIFTHS */
showKeySymbol XKB_KEY_fourfifths = "fourfifths"
-- #define XKB_KEY_onesixth                      0x0ab6  /* U+2159 VULGAR FRACTION ONE SIXTH */
showKeySymbol XKB_KEY_onesixth = "onesixth"
-- #define XKB_KEY_fivesixths                    0x0ab7  /* U+215A VULGAR FRACTION FIVE SIXTHS */
showKeySymbol XKB_KEY_fivesixths = "fivesixths"
-- #define XKB_KEY_careof                        0x0ab8  /* U+2105 CARE OF */
showKeySymbol XKB_KEY_careof = "careof"
-- #define XKB_KEY_figdash                       0x0abb  /* U+2012 FIGURE DASH */
showKeySymbol XKB_KEY_figdash = "figdash"
-- #define XKB_KEY_leftanglebracket              0x0abc  /*(U+27E8 MATHEMATICAL LEFT ANGLE BRACKET)*/
showKeySymbol XKB_KEY_leftanglebracket = "leftanglebracket"
-- #define XKB_KEY_decimalpoint                  0x0abd  /*(U+002E FULL STOP)*/
showKeySymbol XKB_KEY_decimalpoint = "decimalpoint"
-- #define XKB_KEY_rightanglebracket             0x0abe  /*(U+27E9 MATHEMATICAL RIGHT ANGLE BRACKET)*/
showKeySymbol XKB_KEY_rightanglebracket = "rightanglebracket"
-- #define XKB_KEY_marker                        0x0abf
showKeySymbol XKB_KEY_marker = "marker"
-- #define XKB_KEY_oneeighth                     0x0ac3  /* U+215B VULGAR FRACTION ONE EIGHTH */
showKeySymbol XKB_KEY_oneeighth = "oneeighth"
-- #define XKB_KEY_threeeighths                  0x0ac4  /* U+215C VULGAR FRACTION THREE EIGHTHS */
showKeySymbol XKB_KEY_threeeighths = "threeeighths"
-- #define XKB_KEY_fiveeighths                   0x0ac5  /* U+215D VULGAR FRACTION FIVE EIGHTHS */
showKeySymbol XKB_KEY_fiveeighths = "fiveeighths"
-- #define XKB_KEY_seveneighths                  0x0ac6  /* U+215E VULGAR FRACTION SEVEN EIGHTHS */
showKeySymbol XKB_KEY_seveneighths = "seveneighths"
-- #define XKB_KEY_trademark                     0x0ac9  /* U+2122 TRADE MARK SIGN */
showKeySymbol XKB_KEY_trademark = "trademark"
-- #define XKB_KEY_signaturemark                 0x0aca  /*(U+2613 SALTIRE)*/
showKeySymbol XKB_KEY_signaturemark = "signaturemark"
-- #define XKB_KEY_trademarkincircle             0x0acb
showKeySymbol XKB_KEY_trademarkincircle = "trademarkincircle"
-- #define XKB_KEY_leftopentriangle              0x0acc  /*(U+25C1 WHITE LEFT-POINTING TRIANGLE)*/
showKeySymbol XKB_KEY_leftopentriangle = "leftopentriangle"
-- #define XKB_KEY_rightopentriangle             0x0acd  /*(U+25B7 WHITE RIGHT-POINTING TRIANGLE)*/
showKeySymbol XKB_KEY_rightopentriangle = "rightopentriangle"
-- #define XKB_KEY_emopencircle                  0x0ace  /*(U+25CB WHITE CIRCLE)*/
showKeySymbol XKB_KEY_emopencircle = "emopencircle"
-- #define XKB_KEY_emopenrectangle               0x0acf  /*(U+25AF WHITE VERTICAL RECTANGLE)*/
showKeySymbol XKB_KEY_emopenrectangle = "emopenrectangle"
-- #define XKB_KEY_leftsinglequotemark           0x0ad0  /* U+2018 LEFT SINGLE QUOTATION MARK */
showKeySymbol XKB_KEY_leftsinglequotemark = "leftsinglequotemark"
-- #define XKB_KEY_rightsinglequotemark          0x0ad1  /* U+2019 RIGHT SINGLE QUOTATION MARK */
showKeySymbol XKB_KEY_rightsinglequotemark = "rightsinglequotemark"
-- #define XKB_KEY_leftdoublequotemark           0x0ad2  /* U+201C LEFT DOUBLE QUOTATION MARK */
showKeySymbol XKB_KEY_leftdoublequotemark = "leftdoublequotemark"
-- #define XKB_KEY_rightdoublequotemark          0x0ad3  /* U+201D RIGHT DOUBLE QUOTATION MARK */
showKeySymbol XKB_KEY_rightdoublequotemark = "rightdoublequotemark"
-- #define XKB_KEY_prescription                  0x0ad4  /* U+211E PRESCRIPTION TAKE */
showKeySymbol XKB_KEY_prescription = "prescription"
-- #define XKB_KEY_permille                      0x0ad5  /* U+2030 PER MILLE SIGN */
showKeySymbol XKB_KEY_permille = "permille"
-- #define XKB_KEY_minutes                       0x0ad6  /* U+2032 PRIME */
showKeySymbol XKB_KEY_minutes = "minutes"
-- #define XKB_KEY_seconds                       0x0ad7  /* U+2033 DOUBLE PRIME */
showKeySymbol XKB_KEY_seconds = "seconds"
-- #define XKB_KEY_latincross                    0x0ad9  /* U+271D LATIN CROSS */
showKeySymbol XKB_KEY_latincross = "latincross"
-- #define XKB_KEY_hexagram                      0x0ada
showKeySymbol XKB_KEY_hexagram = "hexagram"
-- #define XKB_KEY_filledrectbullet              0x0adb  /*(U+25AC BLACK RECTANGLE)*/
showKeySymbol XKB_KEY_filledrectbullet = "filledrectbullet"
-- #define XKB_KEY_filledlefttribullet           0x0adc  /*(U+25C0 BLACK LEFT-POINTING TRIANGLE)*/
showKeySymbol XKB_KEY_filledlefttribullet = "filledlefttribullet"
-- #define XKB_KEY_filledrighttribullet          0x0add  /*(U+25B6 BLACK RIGHT-POINTING TRIANGLE)*/
showKeySymbol XKB_KEY_filledrighttribullet = "filledrighttribullet"
-- #define XKB_KEY_emfilledcircle                0x0ade  /*(U+25CF BLACK CIRCLE)*/
showKeySymbol XKB_KEY_emfilledcircle = "emfilledcircle"
-- #define XKB_KEY_emfilledrect                  0x0adf  /*(U+25AE BLACK VERTICAL RECTANGLE)*/
showKeySymbol XKB_KEY_emfilledrect = "emfilledrect"
-- #define XKB_KEY_enopencircbullet              0x0ae0  /*(U+25E6 WHITE BULLET)*/
showKeySymbol XKB_KEY_enopencircbullet = "enopencircbullet"
-- #define XKB_KEY_enopensquarebullet            0x0ae1  /*(U+25AB WHITE SMALL SQUARE)*/
showKeySymbol XKB_KEY_enopensquarebullet = "enopensquarebullet"
-- #define XKB_KEY_openrectbullet                0x0ae2  /*(U+25AD WHITE RECTANGLE)*/
showKeySymbol XKB_KEY_openrectbullet = "openrectbullet"
-- #define XKB_KEY_opentribulletup               0x0ae3  /*(U+25B3 WHITE UP-POINTING TRIANGLE)*/
showKeySymbol XKB_KEY_opentribulletup = "opentribulletup"
-- #define XKB_KEY_opentribulletdown             0x0ae4  /*(U+25BD WHITE DOWN-POINTING TRIANGLE)*/
showKeySymbol XKB_KEY_opentribulletdown = "opentribulletdown"
-- #define XKB_KEY_openstar                      0x0ae5  /*(U+2606 WHITE STAR)*/
showKeySymbol XKB_KEY_openstar = "openstar"
-- #define XKB_KEY_enfilledcircbullet            0x0ae6  /*(U+2022 BULLET)*/
showKeySymbol XKB_KEY_enfilledcircbullet = "enfilledcircbullet"
-- #define XKB_KEY_enfilledsqbullet              0x0ae7  /*(U+25AA BLACK SMALL SQUARE)*/
showKeySymbol XKB_KEY_enfilledsqbullet = "enfilledsqbullet"
-- #define XKB_KEY_filledtribulletup             0x0ae8  /*(U+25B2 BLACK UP-POINTING TRIANGLE)*/
showKeySymbol XKB_KEY_filledtribulletup = "filledtribulletup"
-- #define XKB_KEY_filledtribulletdown           0x0ae9  /*(U+25BC BLACK DOWN-POINTING TRIANGLE)*/
showKeySymbol XKB_KEY_filledtribulletdown = "filledtribulletdown"
-- #define XKB_KEY_leftpointer                   0x0aea  /*(U+261C WHITE LEFT POINTING INDEX)*/
showKeySymbol XKB_KEY_leftpointer = "leftpointer"
-- #define XKB_KEY_rightpointer                  0x0aeb  /*(U+261E WHITE RIGHT POINTING INDEX)*/
showKeySymbol XKB_KEY_rightpointer = "rightpointer"
-- #define XKB_KEY_club                          0x0aec  /* U+2663 BLACK CLUB SUIT */
showKeySymbol XKB_KEY_club = "club"
-- #define XKB_KEY_diamond                       0x0aed  /* U+2666 BLACK DIAMOND SUIT */
showKeySymbol XKB_KEY_diamond = "diamond"
-- #define XKB_KEY_heart                         0x0aee  /* U+2665 BLACK HEART SUIT */
showKeySymbol XKB_KEY_heart = "heart"
-- #define XKB_KEY_maltesecross                  0x0af0  /* U+2720 MALTESE CROSS */
showKeySymbol XKB_KEY_maltesecross = "maltesecross"
-- #define XKB_KEY_dagger                        0x0af1  /* U+2020 DAGGER */
showKeySymbol XKB_KEY_dagger = "dagger"
-- #define XKB_KEY_doubledagger                  0x0af2  /* U+2021 DOUBLE DAGGER */
showKeySymbol XKB_KEY_doubledagger = "doubledagger"
-- #define XKB_KEY_checkmark                     0x0af3  /* U+2713 CHECK MARK */
showKeySymbol XKB_KEY_checkmark = "checkmark"
-- #define XKB_KEY_ballotcross                   0x0af4  /* U+2717 BALLOT X */
showKeySymbol XKB_KEY_ballotcross = "ballotcross"
-- #define XKB_KEY_musicalsharp                  0x0af5  /* U+266F MUSIC SHARP SIGN */
showKeySymbol XKB_KEY_musicalsharp = "musicalsharp"
-- #define XKB_KEY_musicalflat                   0x0af6  /* U+266D MUSIC FLAT SIGN */
showKeySymbol XKB_KEY_musicalflat = "musicalflat"
-- #define XKB_KEY_malesymbol                    0x0af7  /* U+2642 MALE SIGN */
showKeySymbol XKB_KEY_malesymbol = "malesymbol"
-- #define XKB_KEY_femalesymbol                  0x0af8  /* U+2640 FEMALE SIGN */
showKeySymbol XKB_KEY_femalesymbol = "femalesymbol"
-- #define XKB_KEY_telephone                     0x0af9  /* U+260E BLACK TELEPHONE */
showKeySymbol XKB_KEY_telephone = "telephone"
-- #define XKB_KEY_telephonerecorder             0x0afa  /* U+2315 TELEPHONE RECORDER */
showKeySymbol XKB_KEY_telephonerecorder = "telephonerecorder"
-- #define XKB_KEY_phonographcopyright           0x0afb  /* U+2117 SOUND RECORDING COPYRIGHT */
showKeySymbol XKB_KEY_phonographcopyright = "phonographcopyright"
-- #define XKB_KEY_caret                         0x0afc  /* U+2038 CARET */
showKeySymbol XKB_KEY_caret = "caret"
-- #define XKB_KEY_singlelowquotemark            0x0afd  /* U+201A SINGLE LOW-9 QUOTATION MARK */
showKeySymbol XKB_KEY_singlelowquotemark = "singlelowquotemark"
-- #define XKB_KEY_doublelowquotemark            0x0afe  /* U+201E DOUBLE LOW-9 QUOTATION MARK */
showKeySymbol XKB_KEY_doublelowquotemark = "doublelowquotemark"
-- #define XKB_KEY_cursor                        0x0aff
showKeySymbol XKB_KEY_cursor = "cursor"
-- #define XKB_KEY_leftcaret                     0x0ba3  /*(U+003C LESS-THAN SIGN)*/
showKeySymbol XKB_KEY_leftcaret = "leftcaret"
-- #define XKB_KEY_rightcaret                    0x0ba6  /*(U+003E GREATER-THAN SIGN)*/
showKeySymbol XKB_KEY_rightcaret = "rightcaret"
-- #define XKB_KEY_downcaret                     0x0ba8  /*(U+2228 LOGICAL OR)*/
showKeySymbol XKB_KEY_downcaret = "downcaret"
-- #define XKB_KEY_upcaret                       0x0ba9  /*(U+2227 LOGICAL AND)*/
showKeySymbol XKB_KEY_upcaret = "upcaret"
-- #define XKB_KEY_overbar                       0x0bc0  /*(U+00AF MACRON)*/
showKeySymbol XKB_KEY_overbar = "overbar"
-- #define XKB_KEY_downtack                      0x0bc2  /* U+22A4 DOWN TACK */
showKeySymbol XKB_KEY_downtack = "downtack"
-- #define XKB_KEY_upshoe                        0x0bc3  /*(U+2229 INTERSECTION)*/
showKeySymbol XKB_KEY_upshoe = "upshoe"
-- #define XKB_KEY_downstile                     0x0bc4  /* U+230A LEFT FLOOR */
showKeySymbol XKB_KEY_downstile = "downstile"
-- #define XKB_KEY_underbar                      0x0bc6  /*(U+005F LOW LINE)*/
showKeySymbol XKB_KEY_underbar = "underbar"
-- #define XKB_KEY_jot                           0x0bca  /* U+2218 RING OPERATOR */
showKeySymbol XKB_KEY_jot = "jot"
-- #define XKB_KEY_quad                          0x0bcc  /* U+2395 APL FUNCTIONAL SYMBOL QUAD */
showKeySymbol XKB_KEY_quad = "quad"
-- #define XKB_KEY_uptack                        0x0bce  /* U+22A5 UP TACK */
showKeySymbol XKB_KEY_uptack = "uptack"
-- #define XKB_KEY_circle                        0x0bcf  /* U+25CB WHITE CIRCLE */
showKeySymbol XKB_KEY_circle = "circle"
-- #define XKB_KEY_upstile                       0x0bd3  /* U+2308 LEFT CEILING */
showKeySymbol XKB_KEY_upstile = "upstile"
-- #define XKB_KEY_downshoe                      0x0bd6  /*(U+222A UNION)*/
showKeySymbol XKB_KEY_downshoe = "downshoe"
-- #define XKB_KEY_rightshoe                     0x0bd8  /*(U+2283 SUPERSET OF)*/
showKeySymbol XKB_KEY_rightshoe = "rightshoe"
-- #define XKB_KEY_leftshoe                      0x0bda  /*(U+2282 SUBSET OF)*/
showKeySymbol XKB_KEY_leftshoe = "leftshoe"
-- #define XKB_KEY_lefttack                      0x0bdc  /* U+22A3 LEFT TACK */
showKeySymbol XKB_KEY_lefttack = "lefttack"
-- #define XKB_KEY_righttack                     0x0bfc  /* U+22A2 RIGHT TACK */
showKeySymbol XKB_KEY_righttack = "righttack"
-- #define XKB_KEY_hebrew_doublelowline          0x0cdf  /* U+2017 DOUBLE LOW LINE */
showKeySymbol XKB_KEY_hebrew_doublelowline = "hebrew_doublelowline"
-- #define XKB_KEY_hebrew_aleph                  0x0ce0  /* U+05D0 HEBREW LETTER ALEF */
showKeySymbol XKB_KEY_hebrew_aleph = "hebrew_aleph"
-- #define XKB_KEY_hebrew_bet                    0x0ce1  /* U+05D1 HEBREW LETTER BET */
showKeySymbol XKB_KEY_hebrew_bet = "hebrew_bet"
-- #define XKB_KEY_hebrew_beth                   0x0ce1  /* deprecated */
showKeySymbol XKB_KEY_hebrew_beth = "hebrew_beth"
-- #define XKB_KEY_hebrew_gimel                  0x0ce2  /* U+05D2 HEBREW LETTER GIMEL */
showKeySymbol XKB_KEY_hebrew_gimel = "hebrew_gimel"
-- #define XKB_KEY_hebrew_gimmel                 0x0ce2  /* deprecated */
showKeySymbol XKB_KEY_hebrew_gimmel = "hebrew_gimmel"
-- #define XKB_KEY_hebrew_dalet                  0x0ce3  /* U+05D3 HEBREW LETTER DALET */
showKeySymbol XKB_KEY_hebrew_dalet = "hebrew_dalet"
-- #define XKB_KEY_hebrew_daleth                 0x0ce3  /* deprecated */
showKeySymbol XKB_KEY_hebrew_daleth = "hebrew_daleth"
-- #define XKB_KEY_hebrew_he                     0x0ce4  /* U+05D4 HEBREW LETTER HE */
showKeySymbol XKB_KEY_hebrew_he = "hebrew_he"
-- #define XKB_KEY_hebrew_waw                    0x0ce5  /* U+05D5 HEBREW LETTER VAV */
showKeySymbol XKB_KEY_hebrew_waw = "hebrew_waw"
-- #define XKB_KEY_hebrew_zain                   0x0ce6  /* U+05D6 HEBREW LETTER ZAYIN */
showKeySymbol XKB_KEY_hebrew_zain = "hebrew_zain"
-- #define XKB_KEY_hebrew_zayin                  0x0ce6  /* deprecated */
showKeySymbol XKB_KEY_hebrew_zayin = "hebrew_zayin"
-- #define XKB_KEY_hebrew_chet                   0x0ce7  /* U+05D7 HEBREW LETTER HET */
showKeySymbol XKB_KEY_hebrew_chet = "hebrew_chet"
-- #define XKB_KEY_hebrew_het                    0x0ce7  /* deprecated */
showKeySymbol XKB_KEY_hebrew_het = "hebrew_het"
-- #define XKB_KEY_hebrew_tet                    0x0ce8  /* U+05D8 HEBREW LETTER TET */
showKeySymbol XKB_KEY_hebrew_tet = "hebrew_tet"
-- #define XKB_KEY_hebrew_teth                   0x0ce8  /* deprecated */
showKeySymbol XKB_KEY_hebrew_teth = "hebrew_teth"
-- #define XKB_KEY_hebrew_yod                    0x0ce9  /* U+05D9 HEBREW LETTER YOD */
showKeySymbol XKB_KEY_hebrew_yod = "hebrew_yod"
-- #define XKB_KEY_hebrew_finalkaph              0x0cea  /* U+05DA HEBREW LETTER FINAL KAF */
showKeySymbol XKB_KEY_hebrew_finalkaph = "hebrew_finalkaph"
-- #define XKB_KEY_hebrew_kaph                   0x0ceb  /* U+05DB HEBREW LETTER KAF */
showKeySymbol XKB_KEY_hebrew_kaph = "hebrew_kaph"
-- #define XKB_KEY_hebrew_lamed                  0x0cec  /* U+05DC HEBREW LETTER LAMED */
showKeySymbol XKB_KEY_hebrew_lamed = "hebrew_lamed"
-- #define XKB_KEY_hebrew_finalmem               0x0ced  /* U+05DD HEBREW LETTER FINAL MEM */
showKeySymbol XKB_KEY_hebrew_finalmem = "hebrew_finalmem"
-- #define XKB_KEY_hebrew_mem                    0x0cee  /* U+05DE HEBREW LETTER MEM */
showKeySymbol XKB_KEY_hebrew_mem = "hebrew_mem"
-- #define XKB_KEY_hebrew_finalnun               0x0cef  /* U+05DF HEBREW LETTER FINAL NUN */
showKeySymbol XKB_KEY_hebrew_finalnun = "hebrew_finalnun"
-- #define XKB_KEY_hebrew_nun                    0x0cf0  /* U+05E0 HEBREW LETTER NUN */
showKeySymbol XKB_KEY_hebrew_nun = "hebrew_nun"
-- #define XKB_KEY_hebrew_samech                 0x0cf1  /* U+05E1 HEBREW LETTER SAMEKH */
showKeySymbol XKB_KEY_hebrew_samech = "hebrew_samech"
-- #define XKB_KEY_hebrew_samekh                 0x0cf1  /* deprecated */
showKeySymbol XKB_KEY_hebrew_samekh = "hebrew_samekh"
-- #define XKB_KEY_hebrew_ayin                   0x0cf2  /* U+05E2 HEBREW LETTER AYIN */
showKeySymbol XKB_KEY_hebrew_ayin = "hebrew_ayin"
-- #define XKB_KEY_hebrew_finalpe                0x0cf3  /* U+05E3 HEBREW LETTER FINAL PE */
showKeySymbol XKB_KEY_hebrew_finalpe = "hebrew_finalpe"
-- #define XKB_KEY_hebrew_pe                     0x0cf4  /* U+05E4 HEBREW LETTER PE */
showKeySymbol XKB_KEY_hebrew_pe = "hebrew_pe"
-- #define XKB_KEY_hebrew_finalzade              0x0cf5  /* U+05E5 HEBREW LETTER FINAL TSADI */
showKeySymbol XKB_KEY_hebrew_finalzade = "hebrew_finalzade"
-- #define XKB_KEY_hebrew_finalzadi              0x0cf5  /* deprecated */
showKeySymbol XKB_KEY_hebrew_finalzadi = "hebrew_finalzadi"
-- #define XKB_KEY_hebrew_zade                   0x0cf6  /* U+05E6 HEBREW LETTER TSADI */
showKeySymbol XKB_KEY_hebrew_zade = "hebrew_zade"
-- #define XKB_KEY_hebrew_zadi                   0x0cf6  /* deprecated */
showKeySymbol XKB_KEY_hebrew_zadi = "hebrew_zadi"
-- #define XKB_KEY_hebrew_qoph                   0x0cf7  /* U+05E7 HEBREW LETTER QOF */
showKeySymbol XKB_KEY_hebrew_qoph = "hebrew_qoph"
-- #define XKB_KEY_hebrew_kuf                    0x0cf7  /* deprecated */
showKeySymbol XKB_KEY_hebrew_kuf = "hebrew_kuf"
-- #define XKB_KEY_hebrew_resh                   0x0cf8  /* U+05E8 HEBREW LETTER RESH */
showKeySymbol XKB_KEY_hebrew_resh = "hebrew_resh"
-- #define XKB_KEY_hebrew_shin                   0x0cf9  /* U+05E9 HEBREW LETTER SHIN */
showKeySymbol XKB_KEY_hebrew_shin = "hebrew_shin"
-- #define XKB_KEY_hebrew_taw                    0x0cfa  /* U+05EA HEBREW LETTER TAV */
showKeySymbol XKB_KEY_hebrew_taw = "hebrew_taw"
-- #define XKB_KEY_hebrew_taf                    0x0cfa  /* deprecated */
showKeySymbol XKB_KEY_hebrew_taf = "hebrew_taf"
-- #define XKB_KEY_Hebrew_switch                 0xff7e  /* Alias for mode_switch */
showKeySymbol XKB_KEY_Hebrew_switch = "Hebrew_switch"
-- #define XKB_KEY_Thai_kokai                    0x0da1  /* U+0E01 THAI CHARACTER KO KAI */
showKeySymbol XKB_KEY_Thai_kokai = "Thai_kokai"
-- #define XKB_KEY_Thai_khokhai                  0x0da2  /* U+0E02 THAI CHARACTER KHO KHAI */
showKeySymbol XKB_KEY_Thai_khokhai = "Thai_khokhai"
-- #define XKB_KEY_Thai_khokhuat                 0x0da3  /* U+0E03 THAI CHARACTER KHO KHUAT */
showKeySymbol XKB_KEY_Thai_khokhuat = "Thai_khokhuat"
-- #define XKB_KEY_Thai_khokhwai                 0x0da4  /* U+0E04 THAI CHARACTER KHO KHWAI */
showKeySymbol XKB_KEY_Thai_khokhwai = "Thai_khokhwai"
-- #define XKB_KEY_Thai_khokhon                  0x0da5  /* U+0E05 THAI CHARACTER KHO KHON */
showKeySymbol XKB_KEY_Thai_khokhon = "Thai_khokhon"
-- #define XKB_KEY_Thai_khorakhang               0x0da6  /* U+0E06 THAI CHARACTER KHO RAKHANG */
showKeySymbol XKB_KEY_Thai_khorakhang = "Thai_khorakhang"
-- #define XKB_KEY_Thai_ngongu                   0x0da7  /* U+0E07 THAI CHARACTER NGO NGU */
showKeySymbol XKB_KEY_Thai_ngongu = "Thai_ngongu"
-- #define XKB_KEY_Thai_chochan                  0x0da8  /* U+0E08 THAI CHARACTER CHO CHAN */
showKeySymbol XKB_KEY_Thai_chochan = "Thai_chochan"
-- #define XKB_KEY_Thai_choching                 0x0da9  /* U+0E09 THAI CHARACTER CHO CHING */
showKeySymbol XKB_KEY_Thai_choching = "Thai_choching"
-- #define XKB_KEY_Thai_chochang                 0x0daa  /* U+0E0A THAI CHARACTER CHO CHANG */
showKeySymbol XKB_KEY_Thai_chochang = "Thai_chochang"
-- #define XKB_KEY_Thai_soso                     0x0dab  /* U+0E0B THAI CHARACTER SO SO */
showKeySymbol XKB_KEY_Thai_soso = "Thai_soso"
-- #define XKB_KEY_Thai_chochoe                  0x0dac  /* U+0E0C THAI CHARACTER CHO CHOE */
showKeySymbol XKB_KEY_Thai_chochoe = "Thai_chochoe"
-- #define XKB_KEY_Thai_yoying                   0x0dad  /* U+0E0D THAI CHARACTER YO YING */
showKeySymbol XKB_KEY_Thai_yoying = "Thai_yoying"
-- #define XKB_KEY_Thai_dochada                  0x0dae  /* U+0E0E THAI CHARACTER DO CHADA */
showKeySymbol XKB_KEY_Thai_dochada = "Thai_dochada"
-- #define XKB_KEY_Thai_topatak                  0x0daf  /* U+0E0F THAI CHARACTER TO PATAK */
showKeySymbol XKB_KEY_Thai_topatak = "Thai_topatak"
-- #define XKB_KEY_Thai_thothan                  0x0db0  /* U+0E10 THAI CHARACTER THO THAN */
showKeySymbol XKB_KEY_Thai_thothan = "Thai_thothan"
-- #define XKB_KEY_Thai_thonangmontho            0x0db1  /* U+0E11 THAI CHARACTER THO NANGMONTHO */
showKeySymbol XKB_KEY_Thai_thonangmontho = "Thai_thonangmontho"
-- #define XKB_KEY_Thai_thophuthao               0x0db2  /* U+0E12 THAI CHARACTER THO PHUTHAO */
showKeySymbol XKB_KEY_Thai_thophuthao = "Thai_thophuthao"
-- #define XKB_KEY_Thai_nonen                    0x0db3  /* U+0E13 THAI CHARACTER NO NEN */
showKeySymbol XKB_KEY_Thai_nonen = "Thai_nonen"
-- #define XKB_KEY_Thai_dodek                    0x0db4  /* U+0E14 THAI CHARACTER DO DEK */
showKeySymbol XKB_KEY_Thai_dodek = "Thai_dodek"
-- #define XKB_KEY_Thai_totao                    0x0db5  /* U+0E15 THAI CHARACTER TO TAO */
showKeySymbol XKB_KEY_Thai_totao = "Thai_totao"
-- #define XKB_KEY_Thai_thothung                 0x0db6  /* U+0E16 THAI CHARACTER THO THUNG */
showKeySymbol XKB_KEY_Thai_thothung = "Thai_thothung"
-- #define XKB_KEY_Thai_thothahan                0x0db7  /* U+0E17 THAI CHARACTER THO THAHAN */
showKeySymbol XKB_KEY_Thai_thothahan = "Thai_thothahan"
-- #define XKB_KEY_Thai_thothong                 0x0db8  /* U+0E18 THAI CHARACTER THO THONG */
showKeySymbol XKB_KEY_Thai_thothong = "Thai_thothong"
-- #define XKB_KEY_Thai_nonu                     0x0db9  /* U+0E19 THAI CHARACTER NO NU */
showKeySymbol XKB_KEY_Thai_nonu = "Thai_nonu"
-- #define XKB_KEY_Thai_bobaimai                 0x0dba  /* U+0E1A THAI CHARACTER BO BAIMAI */
showKeySymbol XKB_KEY_Thai_bobaimai = "Thai_bobaimai"
-- #define XKB_KEY_Thai_popla                    0x0dbb  /* U+0E1B THAI CHARACTER PO PLA */
showKeySymbol XKB_KEY_Thai_popla = "Thai_popla"
-- #define XKB_KEY_Thai_phophung                 0x0dbc  /* U+0E1C THAI CHARACTER PHO PHUNG */
showKeySymbol XKB_KEY_Thai_phophung = "Thai_phophung"
-- #define XKB_KEY_Thai_fofa                     0x0dbd  /* U+0E1D THAI CHARACTER FO FA */
showKeySymbol XKB_KEY_Thai_fofa = "Thai_fofa"
-- #define XKB_KEY_Thai_phophan                  0x0dbe  /* U+0E1E THAI CHARACTER PHO PHAN */
showKeySymbol XKB_KEY_Thai_phophan = "Thai_phophan"
-- #define XKB_KEY_Thai_fofan                    0x0dbf  /* U+0E1F THAI CHARACTER FO FAN */
showKeySymbol XKB_KEY_Thai_fofan = "Thai_fofan"
-- #define XKB_KEY_Thai_phosamphao               0x0dc0  /* U+0E20 THAI CHARACTER PHO SAMPHAO */
showKeySymbol XKB_KEY_Thai_phosamphao = "Thai_phosamphao"
-- #define XKB_KEY_Thai_moma                     0x0dc1  /* U+0E21 THAI CHARACTER MO MA */
showKeySymbol XKB_KEY_Thai_moma = "Thai_moma"
-- #define XKB_KEY_Thai_yoyak                    0x0dc2  /* U+0E22 THAI CHARACTER YO YAK */
showKeySymbol XKB_KEY_Thai_yoyak = "Thai_yoyak"
-- #define XKB_KEY_Thai_rorua                    0x0dc3  /* U+0E23 THAI CHARACTER RO RUA */
showKeySymbol XKB_KEY_Thai_rorua = "Thai_rorua"
-- #define XKB_KEY_Thai_ru                       0x0dc4  /* U+0E24 THAI CHARACTER RU */
showKeySymbol XKB_KEY_Thai_ru = "Thai_ru"
-- #define XKB_KEY_Thai_loling                   0x0dc5  /* U+0E25 THAI CHARACTER LO LING */
showKeySymbol XKB_KEY_Thai_loling = "Thai_loling"
-- #define XKB_KEY_Thai_lu                       0x0dc6  /* U+0E26 THAI CHARACTER LU */
showKeySymbol XKB_KEY_Thai_lu = "Thai_lu"
-- #define XKB_KEY_Thai_wowaen                   0x0dc7  /* U+0E27 THAI CHARACTER WO WAEN */
showKeySymbol XKB_KEY_Thai_wowaen = "Thai_wowaen"
-- #define XKB_KEY_Thai_sosala                   0x0dc8  /* U+0E28 THAI CHARACTER SO SALA */
showKeySymbol XKB_KEY_Thai_sosala = "Thai_sosala"
-- #define XKB_KEY_Thai_sorusi                   0x0dc9  /* U+0E29 THAI CHARACTER SO RUSI */
showKeySymbol XKB_KEY_Thai_sorusi = "Thai_sorusi"
-- #define XKB_KEY_Thai_sosua                    0x0dca  /* U+0E2A THAI CHARACTER SO SUA */
showKeySymbol XKB_KEY_Thai_sosua = "Thai_sosua"
-- #define XKB_KEY_Thai_hohip                    0x0dcb  /* U+0E2B THAI CHARACTER HO HIP */
showKeySymbol XKB_KEY_Thai_hohip = "Thai_hohip"
-- #define XKB_KEY_Thai_lochula                  0x0dcc  /* U+0E2C THAI CHARACTER LO CHULA */
showKeySymbol XKB_KEY_Thai_lochula = "Thai_lochula"
-- #define XKB_KEY_Thai_oang                     0x0dcd  /* U+0E2D THAI CHARACTER O ANG */
showKeySymbol XKB_KEY_Thai_oang = "Thai_oang"
-- #define XKB_KEY_Thai_honokhuk                 0x0dce  /* U+0E2E THAI CHARACTER HO NOKHUK */
showKeySymbol XKB_KEY_Thai_honokhuk = "Thai_honokhuk"
-- #define XKB_KEY_Thai_paiyannoi                0x0dcf  /* U+0E2F THAI CHARACTER PAIYANNOI */
showKeySymbol XKB_KEY_Thai_paiyannoi = "Thai_paiyannoi"
-- #define XKB_KEY_Thai_saraa                    0x0dd0  /* U+0E30 THAI CHARACTER SARA A */
showKeySymbol XKB_KEY_Thai_saraa = "Thai_saraa"
-- #define XKB_KEY_Thai_maihanakat               0x0dd1  /* U+0E31 THAI CHARACTER MAI HAN-AKAT */
showKeySymbol XKB_KEY_Thai_maihanakat = "Thai_maihanakat"
-- #define XKB_KEY_Thai_saraaa                   0x0dd2  /* U+0E32 THAI CHARACTER SARA AA */
showKeySymbol XKB_KEY_Thai_saraaa = "Thai_saraaa"
-- #define XKB_KEY_Thai_saraam                   0x0dd3  /* U+0E33 THAI CHARACTER SARA AM */
showKeySymbol XKB_KEY_Thai_saraam = "Thai_saraam"
-- #define XKB_KEY_Thai_sarai                    0x0dd4  /* U+0E34 THAI CHARACTER SARA I */
showKeySymbol XKB_KEY_Thai_sarai = "Thai_sarai"
-- #define XKB_KEY_Thai_saraii                   0x0dd5  /* U+0E35 THAI CHARACTER SARA II */
showKeySymbol XKB_KEY_Thai_saraii = "Thai_saraii"
-- #define XKB_KEY_Thai_saraue                   0x0dd6  /* U+0E36 THAI CHARACTER SARA UE */
showKeySymbol XKB_KEY_Thai_saraue = "Thai_saraue"
-- #define XKB_KEY_Thai_sarauee                  0x0dd7  /* U+0E37 THAI CHARACTER SARA UEE */
showKeySymbol XKB_KEY_Thai_sarauee = "Thai_sarauee"
-- #define XKB_KEY_Thai_sarau                    0x0dd8  /* U+0E38 THAI CHARACTER SARA U */
showKeySymbol XKB_KEY_Thai_sarau = "Thai_sarau"
-- #define XKB_KEY_Thai_sarauu                   0x0dd9  /* U+0E39 THAI CHARACTER SARA UU */
showKeySymbol XKB_KEY_Thai_sarauu = "Thai_sarauu"
-- #define XKB_KEY_Thai_phinthu                  0x0dda  /* U+0E3A THAI CHARACTER PHINTHU */
showKeySymbol XKB_KEY_Thai_phinthu = "Thai_phinthu"
-- #define XKB_KEY_Thai_maihanakat_maitho        0x0dde
showKeySymbol XKB_KEY_Thai_maihanakat_maitho = "Thai_maihanakat_maitho"
-- #define XKB_KEY_Thai_baht                     0x0ddf  /* U+0E3F THAI CURRENCY SYMBOL BAHT */
showKeySymbol XKB_KEY_Thai_baht = "Thai_baht"
-- #define XKB_KEY_Thai_sarae                    0x0de0  /* U+0E40 THAI CHARACTER SARA E */
showKeySymbol XKB_KEY_Thai_sarae = "Thai_sarae"
-- #define XKB_KEY_Thai_saraae                   0x0de1  /* U+0E41 THAI CHARACTER SARA AE */
showKeySymbol XKB_KEY_Thai_saraae = "Thai_saraae"
-- #define XKB_KEY_Thai_sarao                    0x0de2  /* U+0E42 THAI CHARACTER SARA O */
showKeySymbol XKB_KEY_Thai_sarao = "Thai_sarao"
-- #define XKB_KEY_Thai_saraaimaimuan            0x0de3  /* U+0E43 THAI CHARACTER SARA AI MAIMUAN */
showKeySymbol XKB_KEY_Thai_saraaimaimuan = "Thai_saraaimaimuan"
-- #define XKB_KEY_Thai_saraaimaimalai           0x0de4  /* U+0E44 THAI CHARACTER SARA AI MAIMALAI */
showKeySymbol XKB_KEY_Thai_saraaimaimalai = "Thai_saraaimaimalai"
-- #define XKB_KEY_Thai_lakkhangyao              0x0de5  /* U+0E45 THAI CHARACTER LAKKHANGYAO */
showKeySymbol XKB_KEY_Thai_lakkhangyao = "Thai_lakkhangyao"
-- #define XKB_KEY_Thai_maiyamok                 0x0de6  /* U+0E46 THAI CHARACTER MAIYAMOK */
showKeySymbol XKB_KEY_Thai_maiyamok = "Thai_maiyamok"
-- #define XKB_KEY_Thai_maitaikhu                0x0de7  /* U+0E47 THAI CHARACTER MAITAIKHU */
showKeySymbol XKB_KEY_Thai_maitaikhu = "Thai_maitaikhu"
-- #define XKB_KEY_Thai_maiek                    0x0de8  /* U+0E48 THAI CHARACTER MAI EK */
showKeySymbol XKB_KEY_Thai_maiek = "Thai_maiek"
-- #define XKB_KEY_Thai_maitho                   0x0de9  /* U+0E49 THAI CHARACTER MAI THO */
showKeySymbol XKB_KEY_Thai_maitho = "Thai_maitho"
-- #define XKB_KEY_Thai_maitri                   0x0dea  /* U+0E4A THAI CHARACTER MAI TRI */
showKeySymbol XKB_KEY_Thai_maitri = "Thai_maitri"
-- #define XKB_KEY_Thai_maichattawa              0x0deb  /* U+0E4B THAI CHARACTER MAI CHATTAWA */
showKeySymbol XKB_KEY_Thai_maichattawa = "Thai_maichattawa"
-- #define XKB_KEY_Thai_thanthakhat              0x0dec  /* U+0E4C THAI CHARACTER THANTHAKHAT */
showKeySymbol XKB_KEY_Thai_thanthakhat = "Thai_thanthakhat"
-- #define XKB_KEY_Thai_nikhahit                 0x0ded  /* U+0E4D THAI CHARACTER NIKHAHIT */
showKeySymbol XKB_KEY_Thai_nikhahit = "Thai_nikhahit"
-- #define XKB_KEY_Thai_leksun                   0x0df0  /* U+0E50 THAI DIGIT ZERO */
showKeySymbol XKB_KEY_Thai_leksun = "Thai_leksun"
-- #define XKB_KEY_Thai_leknung                  0x0df1  /* U+0E51 THAI DIGIT ONE */
showKeySymbol XKB_KEY_Thai_leknung = "Thai_leknung"
-- #define XKB_KEY_Thai_leksong                  0x0df2  /* U+0E52 THAI DIGIT TWO */
showKeySymbol XKB_KEY_Thai_leksong = "Thai_leksong"
-- #define XKB_KEY_Thai_leksam                   0x0df3  /* U+0E53 THAI DIGIT THREE */
showKeySymbol XKB_KEY_Thai_leksam = "Thai_leksam"
-- #define XKB_KEY_Thai_leksi                    0x0df4  /* U+0E54 THAI DIGIT FOUR */
showKeySymbol XKB_KEY_Thai_leksi = "Thai_leksi"
-- #define XKB_KEY_Thai_lekha                    0x0df5  /* U+0E55 THAI DIGIT FIVE */
showKeySymbol XKB_KEY_Thai_lekha = "Thai_lekha"
-- #define XKB_KEY_Thai_lekhok                   0x0df6  /* U+0E56 THAI DIGIT SIX */
showKeySymbol XKB_KEY_Thai_lekhok = "Thai_lekhok"
-- #define XKB_KEY_Thai_lekchet                  0x0df7  /* U+0E57 THAI DIGIT SEVEN */
showKeySymbol XKB_KEY_Thai_lekchet = "Thai_lekchet"
-- #define XKB_KEY_Thai_lekpaet                  0x0df8  /* U+0E58 THAI DIGIT EIGHT */
showKeySymbol XKB_KEY_Thai_lekpaet = "Thai_lekpaet"
-- #define XKB_KEY_Thai_lekkao                   0x0df9  /* U+0E59 THAI DIGIT NINE */
showKeySymbol XKB_KEY_Thai_lekkao = "Thai_lekkao"
-- #define XKB_KEY_Hangul                        0xff31  /* Hangul start/stop(toggle) */
showKeySymbol XKB_KEY_Hangul = "Hangul"
-- #define XKB_KEY_Hangul_Start                  0xff32  /* Hangul start */
showKeySymbol XKB_KEY_Hangul_Start = "Hangul_Start"
-- #define XKB_KEY_Hangul_End                    0xff33  /* Hangul end, English start */
showKeySymbol XKB_KEY_Hangul_End = "Hangul_End"
-- #define XKB_KEY_Hangul_Hanja                  0xff34  /* Start Hangul->Hanja Conversion */
showKeySymbol XKB_KEY_Hangul_Hanja = "Hangul_Hanja"
-- #define XKB_KEY_Hangul_Jamo                   0xff35  /* Hangul Jamo mode */
showKeySymbol XKB_KEY_Hangul_Jamo = "Hangul_Jamo"
-- #define XKB_KEY_Hangul_Romaja                 0xff36  /* Hangul Romaja mode */
showKeySymbol XKB_KEY_Hangul_Romaja = "Hangul_Romaja"
-- #define XKB_KEY_Hangul_Codeinput              0xff37  /* Hangul code input mode */
showKeySymbol XKB_KEY_Hangul_Codeinput = "Hangul_Codeinput"
-- #define XKB_KEY_Hangul_Jeonja                 0xff38  /* Jeonja mode */
showKeySymbol XKB_KEY_Hangul_Jeonja = "Hangul_Jeonja"
-- #define XKB_KEY_Hangul_Banja                  0xff39  /* Banja mode */
showKeySymbol XKB_KEY_Hangul_Banja = "Hangul_Banja"
-- #define XKB_KEY_Hangul_PreHanja               0xff3a  /* Pre Hanja conversion */
showKeySymbol XKB_KEY_Hangul_PreHanja = "Hangul_PreHanja"
-- #define XKB_KEY_Hangul_PostHanja              0xff3b  /* Post Hanja conversion */
showKeySymbol XKB_KEY_Hangul_PostHanja = "Hangul_PostHanja"
-- #define XKB_KEY_Hangul_SingleCandidate        0xff3c  /* Single candidate */
showKeySymbol XKB_KEY_Hangul_SingleCandidate = "Hangul_SingleCandidate"
-- #define XKB_KEY_Hangul_MultipleCandidate      0xff3d  /* Multiple candidate */
showKeySymbol XKB_KEY_Hangul_MultipleCandidate = "Hangul_MultipleCandidate"
-- #define XKB_KEY_Hangul_PreviousCandidate      0xff3e  /* Previous candidate */
showKeySymbol XKB_KEY_Hangul_PreviousCandidate = "Hangul_PreviousCandidate"
-- #define XKB_KEY_Hangul_Special                0xff3f  /* Special symbols */
showKeySymbol XKB_KEY_Hangul_Special = "Hangul_Special"
-- #define XKB_KEY_Hangul_switch                 0xff7e  /* Alias for mode_switch */
showKeySymbol XKB_KEY_Hangul_switch = "Hangul_switch"
-- #define XKB_KEY_Hangul_Kiyeog                 0x0ea1
showKeySymbol XKB_KEY_Hangul_Kiyeog = "Hangul_Kiyeog"
-- #define XKB_KEY_Hangul_SsangKiyeog            0x0ea2
showKeySymbol XKB_KEY_Hangul_SsangKiyeog = "Hangul_SsangKiyeog"
-- #define XKB_KEY_Hangul_KiyeogSios             0x0ea3
showKeySymbol XKB_KEY_Hangul_KiyeogSios = "Hangul_KiyeogSios"
-- #define XKB_KEY_Hangul_Nieun                  0x0ea4
showKeySymbol XKB_KEY_Hangul_Nieun = "Hangul_Nieun"
-- #define XKB_KEY_Hangul_NieunJieuj             0x0ea5
showKeySymbol XKB_KEY_Hangul_NieunJieuj = "Hangul_NieunJieuj"
-- #define XKB_KEY_Hangul_NieunHieuh             0x0ea6
showKeySymbol XKB_KEY_Hangul_NieunHieuh = "Hangul_NieunHieuh"
-- #define XKB_KEY_Hangul_Dikeud                 0x0ea7
showKeySymbol XKB_KEY_Hangul_Dikeud = "Hangul_Dikeud"
-- #define XKB_KEY_Hangul_SsangDikeud            0x0ea8
showKeySymbol XKB_KEY_Hangul_SsangDikeud = "Hangul_SsangDikeud"
-- #define XKB_KEY_Hangul_Rieul                  0x0ea9
showKeySymbol XKB_KEY_Hangul_Rieul = "Hangul_Rieul"
-- #define XKB_KEY_Hangul_RieulKiyeog            0x0eaa
showKeySymbol XKB_KEY_Hangul_RieulKiyeog = "Hangul_RieulKiyeog"
-- #define XKB_KEY_Hangul_RieulMieum             0x0eab
showKeySymbol XKB_KEY_Hangul_RieulMieum = "Hangul_RieulMieum"
-- #define XKB_KEY_Hangul_RieulPieub             0x0eac
showKeySymbol XKB_KEY_Hangul_RieulPieub = "Hangul_RieulPieub"
-- #define XKB_KEY_Hangul_RieulSios              0x0ead
showKeySymbol XKB_KEY_Hangul_RieulSios = "Hangul_RieulSios"
-- #define XKB_KEY_Hangul_RieulTieut             0x0eae
showKeySymbol XKB_KEY_Hangul_RieulTieut = "Hangul_RieulTieut"
-- #define XKB_KEY_Hangul_RieulPhieuf            0x0eaf
showKeySymbol XKB_KEY_Hangul_RieulPhieuf = "Hangul_RieulPhieuf"
-- #define XKB_KEY_Hangul_RieulHieuh             0x0eb0
showKeySymbol XKB_KEY_Hangul_RieulHieuh = "Hangul_RieulHieuh"
-- #define XKB_KEY_Hangul_Mieum                  0x0eb1
showKeySymbol XKB_KEY_Hangul_Mieum = "Hangul_Mieum"
-- #define XKB_KEY_Hangul_Pieub                  0x0eb2
showKeySymbol XKB_KEY_Hangul_Pieub = "Hangul_Pieub"
-- #define XKB_KEY_Hangul_SsangPieub             0x0eb3
showKeySymbol XKB_KEY_Hangul_SsangPieub = "Hangul_SsangPieub"
-- #define XKB_KEY_Hangul_PieubSios              0x0eb4
showKeySymbol XKB_KEY_Hangul_PieubSios = "Hangul_PieubSios"
-- #define XKB_KEY_Hangul_Sios                   0x0eb5
showKeySymbol XKB_KEY_Hangul_Sios = "Hangul_Sios"
-- #define XKB_KEY_Hangul_SsangSios              0x0eb6
showKeySymbol XKB_KEY_Hangul_SsangSios = "Hangul_SsangSios"
-- #define XKB_KEY_Hangul_Ieung                  0x0eb7
showKeySymbol XKB_KEY_Hangul_Ieung = "Hangul_Ieung"
-- #define XKB_KEY_Hangul_Jieuj                  0x0eb8
showKeySymbol XKB_KEY_Hangul_Jieuj = "Hangul_Jieuj"
-- #define XKB_KEY_Hangul_SsangJieuj             0x0eb9
showKeySymbol XKB_KEY_Hangul_SsangJieuj = "Hangul_SsangJieuj"
-- #define XKB_KEY_Hangul_Cieuc                  0x0eba
showKeySymbol XKB_KEY_Hangul_Cieuc = "Hangul_Cieuc"
-- #define XKB_KEY_Hangul_Khieuq                 0x0ebb
showKeySymbol XKB_KEY_Hangul_Khieuq = "Hangul_Khieuq"
-- #define XKB_KEY_Hangul_Tieut                  0x0ebc
showKeySymbol XKB_KEY_Hangul_Tieut = "Hangul_Tieut"
-- #define XKB_KEY_Hangul_Phieuf                 0x0ebd
showKeySymbol XKB_KEY_Hangul_Phieuf = "Hangul_Phieuf"
-- #define XKB_KEY_Hangul_Hieuh                  0x0ebe
showKeySymbol XKB_KEY_Hangul_Hieuh = "Hangul_Hieuh"
-- #define XKB_KEY_Hangul_A                      0x0ebf
showKeySymbol XKB_KEY_Hangul_A = "Hangul_A"
-- #define XKB_KEY_Hangul_AE                     0x0ec0
showKeySymbol XKB_KEY_Hangul_AE = "Hangul_AE"
-- #define XKB_KEY_Hangul_YA                     0x0ec1
showKeySymbol XKB_KEY_Hangul_YA = "Hangul_YA"
-- #define XKB_KEY_Hangul_YAE                    0x0ec2
showKeySymbol XKB_KEY_Hangul_YAE = "Hangul_YAE"
-- #define XKB_KEY_Hangul_EO                     0x0ec3
showKeySymbol XKB_KEY_Hangul_EO = "Hangul_EO"
-- #define XKB_KEY_Hangul_E                      0x0ec4
showKeySymbol XKB_KEY_Hangul_E = "Hangul_E"
-- #define XKB_KEY_Hangul_YEO                    0x0ec5
showKeySymbol XKB_KEY_Hangul_YEO = "Hangul_YEO"
-- #define XKB_KEY_Hangul_YE                     0x0ec6
showKeySymbol XKB_KEY_Hangul_YE = "Hangul_YE"
-- #define XKB_KEY_Hangul_O                      0x0ec7
showKeySymbol XKB_KEY_Hangul_O = "Hangul_O"
-- #define XKB_KEY_Hangul_WA                     0x0ec8
showKeySymbol XKB_KEY_Hangul_WA = "Hangul_WA"
-- #define XKB_KEY_Hangul_WAE                    0x0ec9
showKeySymbol XKB_KEY_Hangul_WAE = "Hangul_WAE"
-- #define XKB_KEY_Hangul_OE                     0x0eca
showKeySymbol XKB_KEY_Hangul_OE = "Hangul_OE"
-- #define XKB_KEY_Hangul_YO                     0x0ecb
showKeySymbol XKB_KEY_Hangul_YO = "Hangul_YO"
-- #define XKB_KEY_Hangul_U                      0x0ecc
showKeySymbol XKB_KEY_Hangul_U = "Hangul_U"
-- #define XKB_KEY_Hangul_WEO                    0x0ecd
showKeySymbol XKB_KEY_Hangul_WEO = "Hangul_WEO"
-- #define XKB_KEY_Hangul_WE                     0x0ece
showKeySymbol XKB_KEY_Hangul_WE = "Hangul_WE"
-- #define XKB_KEY_Hangul_WI                     0x0ecf
showKeySymbol XKB_KEY_Hangul_WI = "Hangul_WI"
-- #define XKB_KEY_Hangul_YU                     0x0ed0
showKeySymbol XKB_KEY_Hangul_YU = "Hangul_YU"
-- #define XKB_KEY_Hangul_EU                     0x0ed1
showKeySymbol XKB_KEY_Hangul_EU = "Hangul_EU"
-- #define XKB_KEY_Hangul_YI                     0x0ed2
showKeySymbol XKB_KEY_Hangul_YI = "Hangul_YI"
-- #define XKB_KEY_Hangul_I                      0x0ed3
showKeySymbol XKB_KEY_Hangul_I = "Hangul_I"
-- #define XKB_KEY_Hangul_J_Kiyeog               0x0ed4
showKeySymbol XKB_KEY_Hangul_J_Kiyeog = "Hangul_J_Kiyeog"
-- #define XKB_KEY_Hangul_J_SsangKiyeog          0x0ed5
showKeySymbol XKB_KEY_Hangul_J_SsangKiyeog = "Hangul_J_SsangKiyeog"
-- #define XKB_KEY_Hangul_J_KiyeogSios           0x0ed6
showKeySymbol XKB_KEY_Hangul_J_KiyeogSios = "Hangul_J_KiyeogSios"
-- #define XKB_KEY_Hangul_J_Nieun                0x0ed7
showKeySymbol XKB_KEY_Hangul_J_Nieun = "Hangul_J_Nieun"
-- #define XKB_KEY_Hangul_J_NieunJieuj           0x0ed8
showKeySymbol XKB_KEY_Hangul_J_NieunJieuj = "Hangul_J_NieunJieuj"
-- #define XKB_KEY_Hangul_J_NieunHieuh           0x0ed9
showKeySymbol XKB_KEY_Hangul_J_NieunHieuh = "Hangul_J_NieunHieuh"
-- #define XKB_KEY_Hangul_J_Dikeud               0x0eda
showKeySymbol XKB_KEY_Hangul_J_Dikeud = "Hangul_J_Dikeud"
-- #define XKB_KEY_Hangul_J_Rieul                0x0edb
showKeySymbol XKB_KEY_Hangul_J_Rieul = "Hangul_J_Rieul"
-- #define XKB_KEY_Hangul_J_RieulKiyeog          0x0edc
showKeySymbol XKB_KEY_Hangul_J_RieulKiyeog = "Hangul_J_RieulKiyeog"
-- #define XKB_KEY_Hangul_J_RieulMieum           0x0edd
showKeySymbol XKB_KEY_Hangul_J_RieulMieum = "Hangul_J_RieulMieum"
-- #define XKB_KEY_Hangul_J_RieulPieub           0x0ede
showKeySymbol XKB_KEY_Hangul_J_RieulPieub = "Hangul_J_RieulPieub"
-- #define XKB_KEY_Hangul_J_RieulSios            0x0edf
showKeySymbol XKB_KEY_Hangul_J_RieulSios = "Hangul_J_RieulSios"
-- #define XKB_KEY_Hangul_J_RieulTieut           0x0ee0
showKeySymbol XKB_KEY_Hangul_J_RieulTieut = "Hangul_J_RieulTieut"
-- #define XKB_KEY_Hangul_J_RieulPhieuf          0x0ee1
showKeySymbol XKB_KEY_Hangul_J_RieulPhieuf = "Hangul_J_RieulPhieuf"
-- #define XKB_KEY_Hangul_J_RieulHieuh           0x0ee2
showKeySymbol XKB_KEY_Hangul_J_RieulHieuh = "Hangul_J_RieulHieuh"
-- #define XKB_KEY_Hangul_J_Mieum                0x0ee3
showKeySymbol XKB_KEY_Hangul_J_Mieum = "Hangul_J_Mieum"
-- #define XKB_KEY_Hangul_J_Pieub                0x0ee4
showKeySymbol XKB_KEY_Hangul_J_Pieub = "Hangul_J_Pieub"
-- #define XKB_KEY_Hangul_J_PieubSios            0x0ee5
showKeySymbol XKB_KEY_Hangul_J_PieubSios = "Hangul_J_PieubSios"
-- #define XKB_KEY_Hangul_J_Sios                 0x0ee6
showKeySymbol XKB_KEY_Hangul_J_Sios = "Hangul_J_Sios"
-- #define XKB_KEY_Hangul_J_SsangSios            0x0ee7
showKeySymbol XKB_KEY_Hangul_J_SsangSios = "Hangul_J_SsangSios"
-- #define XKB_KEY_Hangul_J_Ieung                0x0ee8
showKeySymbol XKB_KEY_Hangul_J_Ieung = "Hangul_J_Ieung"
-- #define XKB_KEY_Hangul_J_Jieuj                0x0ee9
showKeySymbol XKB_KEY_Hangul_J_Jieuj = "Hangul_J_Jieuj"
-- #define XKB_KEY_Hangul_J_Cieuc                0x0eea
showKeySymbol XKB_KEY_Hangul_J_Cieuc = "Hangul_J_Cieuc"
-- #define XKB_KEY_Hangul_J_Khieuq               0x0eeb
showKeySymbol XKB_KEY_Hangul_J_Khieuq = "Hangul_J_Khieuq"
-- #define XKB_KEY_Hangul_J_Tieut                0x0eec
showKeySymbol XKB_KEY_Hangul_J_Tieut = "Hangul_J_Tieut"
-- #define XKB_KEY_Hangul_J_Phieuf               0x0eed
showKeySymbol XKB_KEY_Hangul_J_Phieuf = "Hangul_J_Phieuf"
-- #define XKB_KEY_Hangul_J_Hieuh                0x0eee
showKeySymbol XKB_KEY_Hangul_J_Hieuh = "Hangul_J_Hieuh"
-- #define XKB_KEY_Hangul_RieulYeorinHieuh       0x0eef
showKeySymbol XKB_KEY_Hangul_RieulYeorinHieuh = "Hangul_RieulYeorinHieuh"
-- #define XKB_KEY_Hangul_SunkyeongeumMieum      0x0ef0
showKeySymbol XKB_KEY_Hangul_SunkyeongeumMieum = "Hangul_SunkyeongeumMieum"
-- #define XKB_KEY_Hangul_SunkyeongeumPieub      0x0ef1
showKeySymbol XKB_KEY_Hangul_SunkyeongeumPieub = "Hangul_SunkyeongeumPieub"
-- #define XKB_KEY_Hangul_PanSios                0x0ef2
showKeySymbol XKB_KEY_Hangul_PanSios = "Hangul_PanSios"
-- #define XKB_KEY_Hangul_KkogjiDalrinIeung      0x0ef3
showKeySymbol XKB_KEY_Hangul_KkogjiDalrinIeung = "Hangul_KkogjiDalrinIeung"
-- #define XKB_KEY_Hangul_SunkyeongeumPhieuf     0x0ef4
showKeySymbol XKB_KEY_Hangul_SunkyeongeumPhieuf = "Hangul_SunkyeongeumPhieuf"
-- #define XKB_KEY_Hangul_YeorinHieuh            0x0ef5
showKeySymbol XKB_KEY_Hangul_YeorinHieuh = "Hangul_YeorinHieuh"
-- #define XKB_KEY_Hangul_AraeA                  0x0ef6
showKeySymbol XKB_KEY_Hangul_AraeA = "Hangul_AraeA"
-- #define XKB_KEY_Hangul_AraeAE                 0x0ef7
showKeySymbol XKB_KEY_Hangul_AraeAE = "Hangul_AraeAE"
-- #define XKB_KEY_Hangul_J_PanSios              0x0ef8
showKeySymbol XKB_KEY_Hangul_J_PanSios = "Hangul_J_PanSios"
-- #define XKB_KEY_Hangul_J_KkogjiDalrinIeung    0x0ef9
showKeySymbol XKB_KEY_Hangul_J_KkogjiDalrinIeung = "Hangul_J_KkogjiDalrinIeung"
-- #define XKB_KEY_Hangul_J_YeorinHieuh          0x0efa
showKeySymbol XKB_KEY_Hangul_J_YeorinHieuh = "Hangul_J_YeorinHieuh"
-- #define XKB_KEY_Korean_Won                    0x0eff  /*(U+20A9 WON SIGN)*/
showKeySymbol XKB_KEY_Korean_Won = "Korean_Won"
-- #define XKB_KEY_Armenian_ligature_ew       0x1000587  /* U+0587 ARMENIAN SMALL LIGATURE ECH YIWN */
showKeySymbol XKB_KEY_Armenian_ligature_ew = "Armenian_ligature_ew"
-- #define XKB_KEY_Armenian_full_stop         0x1000589  /* U+0589 ARMENIAN FULL STOP */
showKeySymbol XKB_KEY_Armenian_full_stop = "Armenian_full_stop"
-- #define XKB_KEY_Armenian_verjaket          0x1000589  /* U+0589 ARMENIAN FULL STOP */
showKeySymbol XKB_KEY_Armenian_verjaket = "Armenian_verjaket"
-- #define XKB_KEY_Armenian_separation_mark   0x100055d  /* U+055D ARMENIAN COMMA */
showKeySymbol XKB_KEY_Armenian_separation_mark = "Armenian_separation_mark"
-- #define XKB_KEY_Armenian_but               0x100055d  /* U+055D ARMENIAN COMMA */
showKeySymbol XKB_KEY_Armenian_but = "Armenian_but"
-- #define XKB_KEY_Armenian_hyphen            0x100058a  /* U+058A ARMENIAN HYPHEN */
showKeySymbol XKB_KEY_Armenian_hyphen = "Armenian_hyphen"
-- #define XKB_KEY_Armenian_yentamna          0x100058a  /* U+058A ARMENIAN HYPHEN */
showKeySymbol XKB_KEY_Armenian_yentamna = "Armenian_yentamna"
-- #define XKB_KEY_Armenian_exclam            0x100055c  /* U+055C ARMENIAN EXCLAMATION MARK */
showKeySymbol XKB_KEY_Armenian_exclam = "Armenian_exclam"
-- #define XKB_KEY_Armenian_amanak            0x100055c  /* U+055C ARMENIAN EXCLAMATION MARK */
showKeySymbol XKB_KEY_Armenian_amanak = "Armenian_amanak"
-- #define XKB_KEY_Armenian_accent            0x100055b  /* U+055B ARMENIAN EMPHASIS MARK */
showKeySymbol XKB_KEY_Armenian_accent = "Armenian_accent"
-- #define XKB_KEY_Armenian_shesht            0x100055b  /* U+055B ARMENIAN EMPHASIS MARK */
showKeySymbol XKB_KEY_Armenian_shesht = "Armenian_shesht"
-- #define XKB_KEY_Armenian_question          0x100055e  /* U+055E ARMENIAN QUESTION MARK */
showKeySymbol XKB_KEY_Armenian_question = "Armenian_question"
-- #define XKB_KEY_Armenian_paruyk            0x100055e  /* U+055E ARMENIAN QUESTION MARK */
showKeySymbol XKB_KEY_Armenian_paruyk = "Armenian_paruyk"
-- #define XKB_KEY_Armenian_AYB               0x1000531  /* U+0531 ARMENIAN CAPITAL LETTER AYB */
showKeySymbol XKB_KEY_Armenian_AYB = "Armenian_AYB"
-- #define XKB_KEY_Armenian_ayb               0x1000561  /* U+0561 ARMENIAN SMALL LETTER AYB */
showKeySymbol XKB_KEY_Armenian_ayb = "Armenian_ayb"
-- #define XKB_KEY_Armenian_BEN               0x1000532  /* U+0532 ARMENIAN CAPITAL LETTER BEN */
showKeySymbol XKB_KEY_Armenian_BEN = "Armenian_BEN"
-- #define XKB_KEY_Armenian_ben               0x1000562  /* U+0562 ARMENIAN SMALL LETTER BEN */
showKeySymbol XKB_KEY_Armenian_ben = "Armenian_ben"
-- #define XKB_KEY_Armenian_GIM               0x1000533  /* U+0533 ARMENIAN CAPITAL LETTER GIM */
showKeySymbol XKB_KEY_Armenian_GIM = "Armenian_GIM"
-- #define XKB_KEY_Armenian_gim               0x1000563  /* U+0563 ARMENIAN SMALL LETTER GIM */
showKeySymbol XKB_KEY_Armenian_gim = "Armenian_gim"
-- #define XKB_KEY_Armenian_DA                0x1000534  /* U+0534 ARMENIAN CAPITAL LETTER DA */
showKeySymbol XKB_KEY_Armenian_DA = "Armenian_DA"
-- #define XKB_KEY_Armenian_da                0x1000564  /* U+0564 ARMENIAN SMALL LETTER DA */
showKeySymbol XKB_KEY_Armenian_da = "Armenian_da"
-- #define XKB_KEY_Armenian_YECH              0x1000535  /* U+0535 ARMENIAN CAPITAL LETTER ECH */
showKeySymbol XKB_KEY_Armenian_YECH = "Armenian_YECH"
-- #define XKB_KEY_Armenian_yech              0x1000565  /* U+0565 ARMENIAN SMALL LETTER ECH */
showKeySymbol XKB_KEY_Armenian_yech = "Armenian_yech"
-- #define XKB_KEY_Armenian_ZA                0x1000536  /* U+0536 ARMENIAN CAPITAL LETTER ZA */
showKeySymbol XKB_KEY_Armenian_ZA = "Armenian_ZA"
-- #define XKB_KEY_Armenian_za                0x1000566  /* U+0566 ARMENIAN SMALL LETTER ZA */
showKeySymbol XKB_KEY_Armenian_za = "Armenian_za"
-- #define XKB_KEY_Armenian_E                 0x1000537  /* U+0537 ARMENIAN CAPITAL LETTER EH */
showKeySymbol XKB_KEY_Armenian_E = "Armenian_E"
-- #define XKB_KEY_Armenian_e                 0x1000567  /* U+0567 ARMENIAN SMALL LETTER EH */
showKeySymbol XKB_KEY_Armenian_e = "Armenian_e"
-- #define XKB_KEY_Armenian_AT                0x1000538  /* U+0538 ARMENIAN CAPITAL LETTER ET */
showKeySymbol XKB_KEY_Armenian_AT = "Armenian_AT"
-- #define XKB_KEY_Armenian_at                0x1000568  /* U+0568 ARMENIAN SMALL LETTER ET */
showKeySymbol XKB_KEY_Armenian_at = "Armenian_at"
-- #define XKB_KEY_Armenian_TO                0x1000539  /* U+0539 ARMENIAN CAPITAL LETTER TO */
showKeySymbol XKB_KEY_Armenian_TO = "Armenian_TO"
-- #define XKB_KEY_Armenian_to                0x1000569  /* U+0569 ARMENIAN SMALL LETTER TO */
showKeySymbol XKB_KEY_Armenian_to = "Armenian_to"
-- #define XKB_KEY_Armenian_ZHE               0x100053a  /* U+053A ARMENIAN CAPITAL LETTER ZHE */
showKeySymbol XKB_KEY_Armenian_ZHE = "Armenian_ZHE"
-- #define XKB_KEY_Armenian_zhe               0x100056a  /* U+056A ARMENIAN SMALL LETTER ZHE */
showKeySymbol XKB_KEY_Armenian_zhe = "Armenian_zhe"
-- #define XKB_KEY_Armenian_INI               0x100053b  /* U+053B ARMENIAN CAPITAL LETTER INI */
showKeySymbol XKB_KEY_Armenian_INI = "Armenian_INI"
-- #define XKB_KEY_Armenian_ini               0x100056b  /* U+056B ARMENIAN SMALL LETTER INI */
showKeySymbol XKB_KEY_Armenian_ini = "Armenian_ini"
-- #define XKB_KEY_Armenian_LYUN              0x100053c  /* U+053C ARMENIAN CAPITAL LETTER LIWN */
showKeySymbol XKB_KEY_Armenian_LYUN = "Armenian_LYUN"
-- #define XKB_KEY_Armenian_lyun              0x100056c  /* U+056C ARMENIAN SMALL LETTER LIWN */
showKeySymbol XKB_KEY_Armenian_lyun = "Armenian_lyun"
-- #define XKB_KEY_Armenian_KHE               0x100053d  /* U+053D ARMENIAN CAPITAL LETTER XEH */
showKeySymbol XKB_KEY_Armenian_KHE = "Armenian_KHE"
-- #define XKB_KEY_Armenian_khe               0x100056d  /* U+056D ARMENIAN SMALL LETTER XEH */
showKeySymbol XKB_KEY_Armenian_khe = "Armenian_khe"
-- #define XKB_KEY_Armenian_TSA               0x100053e  /* U+053E ARMENIAN CAPITAL LETTER CA */
showKeySymbol XKB_KEY_Armenian_TSA = "Armenian_TSA"
-- #define XKB_KEY_Armenian_tsa               0x100056e  /* U+056E ARMENIAN SMALL LETTER CA */
showKeySymbol XKB_KEY_Armenian_tsa = "Armenian_tsa"
-- #define XKB_KEY_Armenian_KEN               0x100053f  /* U+053F ARMENIAN CAPITAL LETTER KEN */
showKeySymbol XKB_KEY_Armenian_KEN = "Armenian_KEN"
-- #define XKB_KEY_Armenian_ken               0x100056f  /* U+056F ARMENIAN SMALL LETTER KEN */
showKeySymbol XKB_KEY_Armenian_ken = "Armenian_ken"
-- #define XKB_KEY_Armenian_HO                0x1000540  /* U+0540 ARMENIAN CAPITAL LETTER HO */
showKeySymbol XKB_KEY_Armenian_HO = "Armenian_HO"
-- #define XKB_KEY_Armenian_ho                0x1000570  /* U+0570 ARMENIAN SMALL LETTER HO */
showKeySymbol XKB_KEY_Armenian_ho = "Armenian_ho"
-- #define XKB_KEY_Armenian_DZA               0x1000541  /* U+0541 ARMENIAN CAPITAL LETTER JA */
showKeySymbol XKB_KEY_Armenian_DZA = "Armenian_DZA"
-- #define XKB_KEY_Armenian_dza               0x1000571  /* U+0571 ARMENIAN SMALL LETTER JA */
showKeySymbol XKB_KEY_Armenian_dza = "Armenian_dza"
-- #define XKB_KEY_Armenian_GHAT              0x1000542  /* U+0542 ARMENIAN CAPITAL LETTER GHAD */
showKeySymbol XKB_KEY_Armenian_GHAT = "Armenian_GHAT"
-- #define XKB_KEY_Armenian_ghat              0x1000572  /* U+0572 ARMENIAN SMALL LETTER GHAD */
showKeySymbol XKB_KEY_Armenian_ghat = "Armenian_ghat"
-- #define XKB_KEY_Armenian_TCHE              0x1000543  /* U+0543 ARMENIAN CAPITAL LETTER CHEH */
showKeySymbol XKB_KEY_Armenian_TCHE = "Armenian_TCHE"
-- #define XKB_KEY_Armenian_tche              0x1000573  /* U+0573 ARMENIAN SMALL LETTER CHEH */
showKeySymbol XKB_KEY_Armenian_tche = "Armenian_tche"
-- #define XKB_KEY_Armenian_MEN               0x1000544  /* U+0544 ARMENIAN CAPITAL LETTER MEN */
showKeySymbol XKB_KEY_Armenian_MEN = "Armenian_MEN"
-- #define XKB_KEY_Armenian_men               0x1000574  /* U+0574 ARMENIAN SMALL LETTER MEN */
showKeySymbol XKB_KEY_Armenian_men = "Armenian_men"
-- #define XKB_KEY_Armenian_HI                0x1000545  /* U+0545 ARMENIAN CAPITAL LETTER YI */
showKeySymbol XKB_KEY_Armenian_HI = "Armenian_HI"
-- #define XKB_KEY_Armenian_hi                0x1000575  /* U+0575 ARMENIAN SMALL LETTER YI */
showKeySymbol XKB_KEY_Armenian_hi = "Armenian_hi"
-- #define XKB_KEY_Armenian_NU                0x1000546  /* U+0546 ARMENIAN CAPITAL LETTER NOW */
showKeySymbol XKB_KEY_Armenian_NU = "Armenian_NU"
-- #define XKB_KEY_Armenian_nu                0x1000576  /* U+0576 ARMENIAN SMALL LETTER NOW */
showKeySymbol XKB_KEY_Armenian_nu = "Armenian_nu"
-- #define XKB_KEY_Armenian_SHA               0x1000547  /* U+0547 ARMENIAN CAPITAL LETTER SHA */
showKeySymbol XKB_KEY_Armenian_SHA = "Armenian_SHA"
-- #define XKB_KEY_Armenian_sha               0x1000577  /* U+0577 ARMENIAN SMALL LETTER SHA */
showKeySymbol XKB_KEY_Armenian_sha = "Armenian_sha"
-- #define XKB_KEY_Armenian_VO                0x1000548  /* U+0548 ARMENIAN CAPITAL LETTER VO */
showKeySymbol XKB_KEY_Armenian_VO = "Armenian_VO"
-- #define XKB_KEY_Armenian_vo                0x1000578  /* U+0578 ARMENIAN SMALL LETTER VO */
showKeySymbol XKB_KEY_Armenian_vo = "Armenian_vo"
-- #define XKB_KEY_Armenian_CHA               0x1000549  /* U+0549 ARMENIAN CAPITAL LETTER CHA */
showKeySymbol XKB_KEY_Armenian_CHA = "Armenian_CHA"
-- #define XKB_KEY_Armenian_cha               0x1000579  /* U+0579 ARMENIAN SMALL LETTER CHA */
showKeySymbol XKB_KEY_Armenian_cha = "Armenian_cha"
-- #define XKB_KEY_Armenian_PE                0x100054a  /* U+054A ARMENIAN CAPITAL LETTER PEH */
showKeySymbol XKB_KEY_Armenian_PE = "Armenian_PE"
-- #define XKB_KEY_Armenian_pe                0x100057a  /* U+057A ARMENIAN SMALL LETTER PEH */
showKeySymbol XKB_KEY_Armenian_pe = "Armenian_pe"
-- #define XKB_KEY_Armenian_JE                0x100054b  /* U+054B ARMENIAN CAPITAL LETTER JHEH */
showKeySymbol XKB_KEY_Armenian_JE = "Armenian_JE"
-- #define XKB_KEY_Armenian_je                0x100057b  /* U+057B ARMENIAN SMALL LETTER JHEH */
showKeySymbol XKB_KEY_Armenian_je = "Armenian_je"
-- #define XKB_KEY_Armenian_RA                0x100054c  /* U+054C ARMENIAN CAPITAL LETTER RA */
showKeySymbol XKB_KEY_Armenian_RA = "Armenian_RA"
-- #define XKB_KEY_Armenian_ra                0x100057c  /* U+057C ARMENIAN SMALL LETTER RA */
showKeySymbol XKB_KEY_Armenian_ra = "Armenian_ra"
-- #define XKB_KEY_Armenian_SE                0x100054d  /* U+054D ARMENIAN CAPITAL LETTER SEH */
showKeySymbol XKB_KEY_Armenian_SE = "Armenian_SE"
-- #define XKB_KEY_Armenian_se                0x100057d  /* U+057D ARMENIAN SMALL LETTER SEH */
showKeySymbol XKB_KEY_Armenian_se = "Armenian_se"
-- #define XKB_KEY_Armenian_VEV               0x100054e  /* U+054E ARMENIAN CAPITAL LETTER VEW */
showKeySymbol XKB_KEY_Armenian_VEV = "Armenian_VEV"
-- #define XKB_KEY_Armenian_vev               0x100057e  /* U+057E ARMENIAN SMALL LETTER VEW */
showKeySymbol XKB_KEY_Armenian_vev = "Armenian_vev"
-- #define XKB_KEY_Armenian_TYUN              0x100054f  /* U+054F ARMENIAN CAPITAL LETTER TIWN */
showKeySymbol XKB_KEY_Armenian_TYUN = "Armenian_TYUN"
-- #define XKB_KEY_Armenian_tyun              0x100057f  /* U+057F ARMENIAN SMALL LETTER TIWN */
showKeySymbol XKB_KEY_Armenian_tyun = "Armenian_tyun"
-- #define XKB_KEY_Armenian_RE                0x1000550  /* U+0550 ARMENIAN CAPITAL LETTER REH */
showKeySymbol XKB_KEY_Armenian_RE = "Armenian_RE"
-- #define XKB_KEY_Armenian_re                0x1000580  /* U+0580 ARMENIAN SMALL LETTER REH */
showKeySymbol XKB_KEY_Armenian_re = "Armenian_re"
-- #define XKB_KEY_Armenian_TSO               0x1000551  /* U+0551 ARMENIAN CAPITAL LETTER CO */
showKeySymbol XKB_KEY_Armenian_TSO = "Armenian_TSO"
-- #define XKB_KEY_Armenian_tso               0x1000581  /* U+0581 ARMENIAN SMALL LETTER CO */
showKeySymbol XKB_KEY_Armenian_tso = "Armenian_tso"
-- #define XKB_KEY_Armenian_VYUN              0x1000552  /* U+0552 ARMENIAN CAPITAL LETTER YIWN */
showKeySymbol XKB_KEY_Armenian_VYUN = "Armenian_VYUN"
-- #define XKB_KEY_Armenian_vyun              0x1000582  /* U+0582 ARMENIAN SMALL LETTER YIWN */
showKeySymbol XKB_KEY_Armenian_vyun = "Armenian_vyun"
-- #define XKB_KEY_Armenian_PYUR              0x1000553  /* U+0553 ARMENIAN CAPITAL LETTER PIWR */
showKeySymbol XKB_KEY_Armenian_PYUR = "Armenian_PYUR"
-- #define XKB_KEY_Armenian_pyur              0x1000583  /* U+0583 ARMENIAN SMALL LETTER PIWR */
showKeySymbol XKB_KEY_Armenian_pyur = "Armenian_pyur"
-- #define XKB_KEY_Armenian_KE                0x1000554  /* U+0554 ARMENIAN CAPITAL LETTER KEH */
showKeySymbol XKB_KEY_Armenian_KE = "Armenian_KE"
-- #define XKB_KEY_Armenian_ke                0x1000584  /* U+0584 ARMENIAN SMALL LETTER KEH */
showKeySymbol XKB_KEY_Armenian_ke = "Armenian_ke"
-- #define XKB_KEY_Armenian_O                 0x1000555  /* U+0555 ARMENIAN CAPITAL LETTER OH */
showKeySymbol XKB_KEY_Armenian_O = "Armenian_O"
-- #define XKB_KEY_Armenian_o                 0x1000585  /* U+0585 ARMENIAN SMALL LETTER OH */
showKeySymbol XKB_KEY_Armenian_o = "Armenian_o"
-- #define XKB_KEY_Armenian_FE                0x1000556  /* U+0556 ARMENIAN CAPITAL LETTER FEH */
showKeySymbol XKB_KEY_Armenian_FE = "Armenian_FE"
-- #define XKB_KEY_Armenian_fe                0x1000586  /* U+0586 ARMENIAN SMALL LETTER FEH */
showKeySymbol XKB_KEY_Armenian_fe = "Armenian_fe"
-- #define XKB_KEY_Armenian_apostrophe        0x100055a  /* U+055A ARMENIAN APOSTROPHE */
showKeySymbol XKB_KEY_Armenian_apostrophe = "Armenian_apostrophe"
-- #define XKB_KEY_Georgian_an                0x10010d0  /* U+10D0 GEORGIAN LETTER AN */
showKeySymbol XKB_KEY_Georgian_an = "Georgian_an"
-- #define XKB_KEY_Georgian_ban               0x10010d1  /* U+10D1 GEORGIAN LETTER BAN */
showKeySymbol XKB_KEY_Georgian_ban = "Georgian_ban"
-- #define XKB_KEY_Georgian_gan               0x10010d2  /* U+10D2 GEORGIAN LETTER GAN */
showKeySymbol XKB_KEY_Georgian_gan = "Georgian_gan"
-- #define XKB_KEY_Georgian_don               0x10010d3  /* U+10D3 GEORGIAN LETTER DON */
showKeySymbol XKB_KEY_Georgian_don = "Georgian_don"
-- #define XKB_KEY_Georgian_en                0x10010d4  /* U+10D4 GEORGIAN LETTER EN */
showKeySymbol XKB_KEY_Georgian_en = "Georgian_en"
-- #define XKB_KEY_Georgian_vin               0x10010d5  /* U+10D5 GEORGIAN LETTER VIN */
showKeySymbol XKB_KEY_Georgian_vin = "Georgian_vin"
-- #define XKB_KEY_Georgian_zen               0x10010d6  /* U+10D6 GEORGIAN LETTER ZEN */
showKeySymbol XKB_KEY_Georgian_zen = "Georgian_zen"
-- #define XKB_KEY_Georgian_tan               0x10010d7  /* U+10D7 GEORGIAN LETTER TAN */
showKeySymbol XKB_KEY_Georgian_tan = "Georgian_tan"
-- #define XKB_KEY_Georgian_in                0x10010d8  /* U+10D8 GEORGIAN LETTER IN */
showKeySymbol XKB_KEY_Georgian_in = "Georgian_in"
-- #define XKB_KEY_Georgian_kan               0x10010d9  /* U+10D9 GEORGIAN LETTER KAN */
showKeySymbol XKB_KEY_Georgian_kan = "Georgian_kan"
-- #define XKB_KEY_Georgian_las               0x10010da  /* U+10DA GEORGIAN LETTER LAS */
showKeySymbol XKB_KEY_Georgian_las = "Georgian_las"
-- #define XKB_KEY_Georgian_man               0x10010db  /* U+10DB GEORGIAN LETTER MAN */
showKeySymbol XKB_KEY_Georgian_man = "Georgian_man"
-- #define XKB_KEY_Georgian_nar               0x10010dc  /* U+10DC GEORGIAN LETTER NAR */
showKeySymbol XKB_KEY_Georgian_nar = "Georgian_nar"
-- #define XKB_KEY_Georgian_on                0x10010dd  /* U+10DD GEORGIAN LETTER ON */
showKeySymbol XKB_KEY_Georgian_on = "Georgian_on"
-- #define XKB_KEY_Georgian_par               0x10010de  /* U+10DE GEORGIAN LETTER PAR */
showKeySymbol XKB_KEY_Georgian_par = "Georgian_par"
-- #define XKB_KEY_Georgian_zhar              0x10010df  /* U+10DF GEORGIAN LETTER ZHAR */
showKeySymbol XKB_KEY_Georgian_zhar = "Georgian_zhar"
-- #define XKB_KEY_Georgian_rae               0x10010e0  /* U+10E0 GEORGIAN LETTER RAE */
showKeySymbol XKB_KEY_Georgian_rae = "Georgian_rae"
-- #define XKB_KEY_Georgian_san               0x10010e1  /* U+10E1 GEORGIAN LETTER SAN */
showKeySymbol XKB_KEY_Georgian_san = "Georgian_san"
-- #define XKB_KEY_Georgian_tar               0x10010e2  /* U+10E2 GEORGIAN LETTER TAR */
showKeySymbol XKB_KEY_Georgian_tar = "Georgian_tar"
-- #define XKB_KEY_Georgian_un                0x10010e3  /* U+10E3 GEORGIAN LETTER UN */
showKeySymbol XKB_KEY_Georgian_un = "Georgian_un"
-- #define XKB_KEY_Georgian_phar              0x10010e4  /* U+10E4 GEORGIAN LETTER PHAR */
showKeySymbol XKB_KEY_Georgian_phar = "Georgian_phar"
-- #define XKB_KEY_Georgian_khar              0x10010e5  /* U+10E5 GEORGIAN LETTER KHAR */
showKeySymbol XKB_KEY_Georgian_khar = "Georgian_khar"
-- #define XKB_KEY_Georgian_ghan              0x10010e6  /* U+10E6 GEORGIAN LETTER GHAN */
showKeySymbol XKB_KEY_Georgian_ghan = "Georgian_ghan"
-- #define XKB_KEY_Georgian_qar               0x10010e7  /* U+10E7 GEORGIAN LETTER QAR */
showKeySymbol XKB_KEY_Georgian_qar = "Georgian_qar"
-- #define XKB_KEY_Georgian_shin              0x10010e8  /* U+10E8 GEORGIAN LETTER SHIN */
showKeySymbol XKB_KEY_Georgian_shin = "Georgian_shin"
-- #define XKB_KEY_Georgian_chin              0x10010e9  /* U+10E9 GEORGIAN LETTER CHIN */
showKeySymbol XKB_KEY_Georgian_chin = "Georgian_chin"
-- #define XKB_KEY_Georgian_can               0x10010ea  /* U+10EA GEORGIAN LETTER CAN */
showKeySymbol XKB_KEY_Georgian_can = "Georgian_can"
-- #define XKB_KEY_Georgian_jil               0x10010eb  /* U+10EB GEORGIAN LETTER JIL */
showKeySymbol XKB_KEY_Georgian_jil = "Georgian_jil"
-- #define XKB_KEY_Georgian_cil               0x10010ec  /* U+10EC GEORGIAN LETTER CIL */
showKeySymbol XKB_KEY_Georgian_cil = "Georgian_cil"
-- #define XKB_KEY_Georgian_char              0x10010ed  /* U+10ED GEORGIAN LETTER CHAR */
showKeySymbol XKB_KEY_Georgian_char = "Georgian_char"
-- #define XKB_KEY_Georgian_xan               0x10010ee  /* U+10EE GEORGIAN LETTER XAN */
showKeySymbol XKB_KEY_Georgian_xan = "Georgian_xan"
-- #define XKB_KEY_Georgian_jhan              0x10010ef  /* U+10EF GEORGIAN LETTER JHAN */
showKeySymbol XKB_KEY_Georgian_jhan = "Georgian_jhan"
-- #define XKB_KEY_Georgian_hae               0x10010f0  /* U+10F0 GEORGIAN LETTER HAE */
showKeySymbol XKB_KEY_Georgian_hae = "Georgian_hae"
-- #define XKB_KEY_Georgian_he                0x10010f1  /* U+10F1 GEORGIAN LETTER HE */
showKeySymbol XKB_KEY_Georgian_he = "Georgian_he"
-- #define XKB_KEY_Georgian_hie               0x10010f2  /* U+10F2 GEORGIAN LETTER HIE */
showKeySymbol XKB_KEY_Georgian_hie = "Georgian_hie"
-- #define XKB_KEY_Georgian_we                0x10010f3  /* U+10F3 GEORGIAN LETTER WE */
showKeySymbol XKB_KEY_Georgian_we = "Georgian_we"
-- #define XKB_KEY_Georgian_har               0x10010f4  /* U+10F4 GEORGIAN LETTER HAR */
showKeySymbol XKB_KEY_Georgian_har = "Georgian_har"
-- #define XKB_KEY_Georgian_hoe               0x10010f5  /* U+10F5 GEORGIAN LETTER HOE */
showKeySymbol XKB_KEY_Georgian_hoe = "Georgian_hoe"
-- #define XKB_KEY_Georgian_fi                0x10010f6  /* U+10F6 GEORGIAN LETTER FI */
showKeySymbol XKB_KEY_Georgian_fi = "Georgian_fi"
-- #define XKB_KEY_Xabovedot                  0x1001e8a  /* U+1E8A LATIN CAPITAL LETTER X WITH DOT ABOVE */
showKeySymbol XKB_KEY_Xabovedot = "Xabovedot"
-- #define XKB_KEY_Ibreve                     0x100012c  /* U+012C LATIN CAPITAL LETTER I WITH BREVE */
showKeySymbol XKB_KEY_Ibreve = "Ibreve"
-- #define XKB_KEY_Zstroke                    0x10001b5  /* U+01B5 LATIN CAPITAL LETTER Z WITH STROKE */
showKeySymbol XKB_KEY_Zstroke = "Zstroke"
-- #define XKB_KEY_Gcaron                     0x10001e6  /* U+01E6 LATIN CAPITAL LETTER G WITH CARON */
showKeySymbol XKB_KEY_Gcaron = "Gcaron"
-- #define XKB_KEY_Ocaron                     0x10001d1  /* U+01D2 LATIN CAPITAL LETTER O WITH CARON */
showKeySymbol XKB_KEY_Ocaron = "Ocaron"
-- #define XKB_KEY_Obarred                    0x100019f  /* U+019F LATIN CAPITAL LETTER O WITH MIDDLE TILDE */
showKeySymbol XKB_KEY_Obarred = "Obarred"
-- #define XKB_KEY_xabovedot                  0x1001e8b  /* U+1E8B LATIN SMALL LETTER X WITH DOT ABOVE */
showKeySymbol XKB_KEY_xabovedot = "xabovedot"
-- #define XKB_KEY_ibreve                     0x100012d  /* U+012D LATIN SMALL LETTER I WITH BREVE */
showKeySymbol XKB_KEY_ibreve = "ibreve"
-- #define XKB_KEY_zstroke                    0x10001b6  /* U+01B6 LATIN SMALL LETTER Z WITH STROKE */
showKeySymbol XKB_KEY_zstroke = "zstroke"
-- #define XKB_KEY_gcaron                     0x10001e7  /* U+01E7 LATIN SMALL LETTER G WITH CARON */
showKeySymbol XKB_KEY_gcaron = "gcaron"
-- #define XKB_KEY_ocaron                     0x10001d2  /* U+01D2 LATIN SMALL LETTER O WITH CARON */
showKeySymbol XKB_KEY_ocaron = "ocaron"
-- #define XKB_KEY_obarred                    0x1000275  /* U+0275 LATIN SMALL LETTER BARRED O */
showKeySymbol XKB_KEY_obarred = "obarred"
-- #define XKB_KEY_SCHWA                      0x100018f  /* U+018F LATIN CAPITAL LETTER SCHWA */
showKeySymbol XKB_KEY_SCHWA = "SCHWA"
-- #define XKB_KEY_schwa                      0x1000259  /* U+0259 LATIN SMALL LETTER SCHWA */
showKeySymbol XKB_KEY_schwa = "schwa"
-- #define XKB_KEY_EZH                        0x10001b7  /* U+01B7 LATIN CAPITAL LETTER EZH */
showKeySymbol XKB_KEY_EZH = "EZH"
-- #define XKB_KEY_ezh                        0x1000292  /* U+0292 LATIN SMALL LETTER EZH */
showKeySymbol XKB_KEY_ezh = "ezh"
-- #define XKB_KEY_Lbelowdot                  0x1001e36  /* U+1E36 LATIN CAPITAL LETTER L WITH DOT BELOW */
showKeySymbol XKB_KEY_Lbelowdot = "Lbelowdot"
-- #define XKB_KEY_lbelowdot                  0x1001e37  /* U+1E37 LATIN SMALL LETTER L WITH DOT BELOW */
showKeySymbol XKB_KEY_lbelowdot = "lbelowdot"
-- #define XKB_KEY_Abelowdot                  0x1001ea0  /* U+1EA0 LATIN CAPITAL LETTER A WITH DOT BELOW */
showKeySymbol XKB_KEY_Abelowdot = "Abelowdot"
-- #define XKB_KEY_abelowdot                  0x1001ea1  /* U+1EA1 LATIN SMALL LETTER A WITH DOT BELOW */
showKeySymbol XKB_KEY_abelowdot = "abelowdot"
-- #define XKB_KEY_Ahook                      0x1001ea2  /* U+1EA2 LATIN CAPITAL LETTER A WITH HOOK ABOVE */
showKeySymbol XKB_KEY_Ahook = "Ahook"
-- #define XKB_KEY_ahook                      0x1001ea3  /* U+1EA3 LATIN SMALL LETTER A WITH HOOK ABOVE */
showKeySymbol XKB_KEY_ahook = "ahook"
-- #define XKB_KEY_Acircumflexacute           0x1001ea4  /* U+1EA4 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE */
showKeySymbol XKB_KEY_Acircumflexacute = "Acircumflexacute"
-- #define XKB_KEY_acircumflexacute           0x1001ea5  /* U+1EA5 LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE */
showKeySymbol XKB_KEY_acircumflexacute = "acircumflexacute"
-- #define XKB_KEY_Acircumflexgrave           0x1001ea6  /* U+1EA6 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE */
showKeySymbol XKB_KEY_Acircumflexgrave = "Acircumflexgrave"
-- #define XKB_KEY_acircumflexgrave           0x1001ea7  /* U+1EA7 LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE */
showKeySymbol XKB_KEY_acircumflexgrave = "acircumflexgrave"
-- #define XKB_KEY_Acircumflexhook            0x1001ea8  /* U+1EA8 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE */
showKeySymbol XKB_KEY_Acircumflexhook = "Acircumflexhook"
-- #define XKB_KEY_acircumflexhook            0x1001ea9  /* U+1EA9 LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE */
showKeySymbol XKB_KEY_acircumflexhook = "acircumflexhook"
-- #define XKB_KEY_Acircumflextilde           0x1001eaa  /* U+1EAA LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE */
showKeySymbol XKB_KEY_Acircumflextilde = "Acircumflextilde"
-- #define XKB_KEY_acircumflextilde           0x1001eab  /* U+1EAB LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE */
showKeySymbol XKB_KEY_acircumflextilde = "acircumflextilde"
-- #define XKB_KEY_Acircumflexbelowdot        0x1001eac  /* U+1EAC LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW */
showKeySymbol XKB_KEY_Acircumflexbelowdot = "Acircumflexbelowdot"
-- #define XKB_KEY_acircumflexbelowdot        0x1001ead  /* U+1EAD LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW */
showKeySymbol XKB_KEY_acircumflexbelowdot = "acircumflexbelowdot"
-- #define XKB_KEY_Abreveacute                0x1001eae  /* U+1EAE LATIN CAPITAL LETTER A WITH BREVE AND ACUTE */
showKeySymbol XKB_KEY_Abreveacute = "Abreveacute"
-- #define XKB_KEY_abreveacute                0x1001eaf  /* U+1EAF LATIN SMALL LETTER A WITH BREVE AND ACUTE */
showKeySymbol XKB_KEY_abreveacute = "abreveacute"
-- #define XKB_KEY_Abrevegrave                0x1001eb0  /* U+1EB0 LATIN CAPITAL LETTER A WITH BREVE AND GRAVE */
showKeySymbol XKB_KEY_Abrevegrave = "Abrevegrave"
-- #define XKB_KEY_abrevegrave                0x1001eb1  /* U+1EB1 LATIN SMALL LETTER A WITH BREVE AND GRAVE */
showKeySymbol XKB_KEY_abrevegrave = "abrevegrave"
-- #define XKB_KEY_Abrevehook                 0x1001eb2  /* U+1EB2 LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE */
showKeySymbol XKB_KEY_Abrevehook = "Abrevehook"
-- #define XKB_KEY_abrevehook                 0x1001eb3  /* U+1EB3 LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE */
showKeySymbol XKB_KEY_abrevehook = "abrevehook"
-- #define XKB_KEY_Abrevetilde                0x1001eb4  /* U+1EB4 LATIN CAPITAL LETTER A WITH BREVE AND TILDE */
showKeySymbol XKB_KEY_Abrevetilde = "Abrevetilde"
-- #define XKB_KEY_abrevetilde                0x1001eb5  /* U+1EB5 LATIN SMALL LETTER A WITH BREVE AND TILDE */
showKeySymbol XKB_KEY_abrevetilde = "abrevetilde"
-- #define XKB_KEY_Abrevebelowdot             0x1001eb6  /* U+1EB6 LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW */
showKeySymbol XKB_KEY_Abrevebelowdot = "Abrevebelowdot"
-- #define XKB_KEY_abrevebelowdot             0x1001eb7  /* U+1EB7 LATIN SMALL LETTER A WITH BREVE AND DOT BELOW */
showKeySymbol XKB_KEY_abrevebelowdot = "abrevebelowdot"
-- #define XKB_KEY_Ebelowdot                  0x1001eb8  /* U+1EB8 LATIN CAPITAL LETTER E WITH DOT BELOW */
showKeySymbol XKB_KEY_Ebelowdot = "Ebelowdot"
-- #define XKB_KEY_ebelowdot                  0x1001eb9  /* U+1EB9 LATIN SMALL LETTER E WITH DOT BELOW */
showKeySymbol XKB_KEY_ebelowdot = "ebelowdot"
-- #define XKB_KEY_Ehook                      0x1001eba  /* U+1EBA LATIN CAPITAL LETTER E WITH HOOK ABOVE */
showKeySymbol XKB_KEY_Ehook = "Ehook"
-- #define XKB_KEY_ehook                      0x1001ebb  /* U+1EBB LATIN SMALL LETTER E WITH HOOK ABOVE */
showKeySymbol XKB_KEY_ehook = "ehook"
-- #define XKB_KEY_Etilde                     0x1001ebc  /* U+1EBC LATIN CAPITAL LETTER E WITH TILDE */
showKeySymbol XKB_KEY_Etilde = "Etilde"
-- #define XKB_KEY_etilde                     0x1001ebd  /* U+1EBD LATIN SMALL LETTER E WITH TILDE */
showKeySymbol XKB_KEY_etilde = "etilde"
-- #define XKB_KEY_Ecircumflexacute           0x1001ebe  /* U+1EBE LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE */
showKeySymbol XKB_KEY_Ecircumflexacute = "Ecircumflexacute"
-- #define XKB_KEY_ecircumflexacute           0x1001ebf  /* U+1EBF LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE */
showKeySymbol XKB_KEY_ecircumflexacute = "ecircumflexacute"
-- #define XKB_KEY_Ecircumflexgrave           0x1001ec0  /* U+1EC0 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE */
showKeySymbol XKB_KEY_Ecircumflexgrave = "Ecircumflexgrave"
-- #define XKB_KEY_ecircumflexgrave           0x1001ec1  /* U+1EC1 LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE */
showKeySymbol XKB_KEY_ecircumflexgrave = "ecircumflexgrave"
-- #define XKB_KEY_Ecircumflexhook            0x1001ec2  /* U+1EC2 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE */
showKeySymbol XKB_KEY_Ecircumflexhook = "Ecircumflexhook"
-- #define XKB_KEY_ecircumflexhook            0x1001ec3  /* U+1EC3 LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE */
showKeySymbol XKB_KEY_ecircumflexhook = "ecircumflexhook"
-- #define XKB_KEY_Ecircumflextilde           0x1001ec4  /* U+1EC4 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE */
showKeySymbol XKB_KEY_Ecircumflextilde = "Ecircumflextilde"
-- #define XKB_KEY_ecircumflextilde           0x1001ec5  /* U+1EC5 LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE */
showKeySymbol XKB_KEY_ecircumflextilde = "ecircumflextilde"
-- #define XKB_KEY_Ecircumflexbelowdot        0x1001ec6  /* U+1EC6 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW */
showKeySymbol XKB_KEY_Ecircumflexbelowdot = "Ecircumflexbelowdot"
-- #define XKB_KEY_ecircumflexbelowdot        0x1001ec7  /* U+1EC7 LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW */
showKeySymbol XKB_KEY_ecircumflexbelowdot = "ecircumflexbelowdot"
-- #define XKB_KEY_Ihook                      0x1001ec8  /* U+1EC8 LATIN CAPITAL LETTER I WITH HOOK ABOVE */
showKeySymbol XKB_KEY_Ihook = "Ihook"
-- #define XKB_KEY_ihook                      0x1001ec9  /* U+1EC9 LATIN SMALL LETTER I WITH HOOK ABOVE */
showKeySymbol XKB_KEY_ihook = "ihook"
-- #define XKB_KEY_Ibelowdot                  0x1001eca  /* U+1ECA LATIN CAPITAL LETTER I WITH DOT BELOW */
showKeySymbol XKB_KEY_Ibelowdot = "Ibelowdot"
-- #define XKB_KEY_ibelowdot                  0x1001ecb  /* U+1ECB LATIN SMALL LETTER I WITH DOT BELOW */
showKeySymbol XKB_KEY_ibelowdot = "ibelowdot"
-- #define XKB_KEY_Obelowdot                  0x1001ecc  /* U+1ECC LATIN CAPITAL LETTER O WITH DOT BELOW */
showKeySymbol XKB_KEY_Obelowdot = "Obelowdot"
-- #define XKB_KEY_obelowdot                  0x1001ecd  /* U+1ECD LATIN SMALL LETTER O WITH DOT BELOW */
showKeySymbol XKB_KEY_obelowdot = "obelowdot"
-- #define XKB_KEY_Ohook                      0x1001ece  /* U+1ECE LATIN CAPITAL LETTER O WITH HOOK ABOVE */
showKeySymbol XKB_KEY_Ohook = "Ohook"
-- #define XKB_KEY_ohook                      0x1001ecf  /* U+1ECF LATIN SMALL LETTER O WITH HOOK ABOVE */
showKeySymbol XKB_KEY_ohook = "ohook"
-- #define XKB_KEY_Ocircumflexacute           0x1001ed0  /* U+1ED0 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE */
showKeySymbol XKB_KEY_Ocircumflexacute = "Ocircumflexacute"
-- #define XKB_KEY_ocircumflexacute           0x1001ed1  /* U+1ED1 LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE */
showKeySymbol XKB_KEY_ocircumflexacute = "ocircumflexacute"
-- #define XKB_KEY_Ocircumflexgrave           0x1001ed2  /* U+1ED2 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE */
showKeySymbol XKB_KEY_Ocircumflexgrave = "Ocircumflexgrave"
-- #define XKB_KEY_ocircumflexgrave           0x1001ed3  /* U+1ED3 LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE */
showKeySymbol XKB_KEY_ocircumflexgrave = "ocircumflexgrave"
-- #define XKB_KEY_Ocircumflexhook            0x1001ed4  /* U+1ED4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE */
showKeySymbol XKB_KEY_Ocircumflexhook = "Ocircumflexhook"
-- #define XKB_KEY_ocircumflexhook            0x1001ed5  /* U+1ED5 LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE */
showKeySymbol XKB_KEY_ocircumflexhook = "ocircumflexhook"
-- #define XKB_KEY_Ocircumflextilde           0x1001ed6  /* U+1ED6 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE */
showKeySymbol XKB_KEY_Ocircumflextilde = "Ocircumflextilde"
-- #define XKB_KEY_ocircumflextilde           0x1001ed7  /* U+1ED7 LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE */
showKeySymbol XKB_KEY_ocircumflextilde = "ocircumflextilde"
-- #define XKB_KEY_Ocircumflexbelowdot        0x1001ed8  /* U+1ED8 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW */
showKeySymbol XKB_KEY_Ocircumflexbelowdot = "Ocircumflexbelowdot"
-- #define XKB_KEY_ocircumflexbelowdot        0x1001ed9  /* U+1ED9 LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW */
showKeySymbol XKB_KEY_ocircumflexbelowdot = "ocircumflexbelowdot"
-- #define XKB_KEY_Ohornacute                 0x1001eda  /* U+1EDA LATIN CAPITAL LETTER O WITH HORN AND ACUTE */
showKeySymbol XKB_KEY_Ohornacute = "Ohornacute"
-- #define XKB_KEY_ohornacute                 0x1001edb  /* U+1EDB LATIN SMALL LETTER O WITH HORN AND ACUTE */
showKeySymbol XKB_KEY_ohornacute = "ohornacute"
-- #define XKB_KEY_Ohorngrave                 0x1001edc  /* U+1EDC LATIN CAPITAL LETTER O WITH HORN AND GRAVE */
showKeySymbol XKB_KEY_Ohorngrave = "Ohorngrave"
-- #define XKB_KEY_ohorngrave                 0x1001edd  /* U+1EDD LATIN SMALL LETTER O WITH HORN AND GRAVE */
showKeySymbol XKB_KEY_ohorngrave = "ohorngrave"
-- #define XKB_KEY_Ohornhook                  0x1001ede  /* U+1EDE LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE */
showKeySymbol XKB_KEY_Ohornhook = "Ohornhook"
-- #define XKB_KEY_ohornhook                  0x1001edf  /* U+1EDF LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE */
showKeySymbol XKB_KEY_ohornhook = "ohornhook"
-- #define XKB_KEY_Ohorntilde                 0x1001ee0  /* U+1EE0 LATIN CAPITAL LETTER O WITH HORN AND TILDE */
showKeySymbol XKB_KEY_Ohorntilde = "Ohorntilde"
-- #define XKB_KEY_ohorntilde                 0x1001ee1  /* U+1EE1 LATIN SMALL LETTER O WITH HORN AND TILDE */
showKeySymbol XKB_KEY_ohorntilde = "ohorntilde"
-- #define XKB_KEY_Ohornbelowdot              0x1001ee2  /* U+1EE2 LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW */
showKeySymbol XKB_KEY_Ohornbelowdot = "Ohornbelowdot"
-- #define XKB_KEY_ohornbelowdot              0x1001ee3  /* U+1EE3 LATIN SMALL LETTER O WITH HORN AND DOT BELOW */
showKeySymbol XKB_KEY_ohornbelowdot = "ohornbelowdot"
-- #define XKB_KEY_Ubelowdot                  0x1001ee4  /* U+1EE4 LATIN CAPITAL LETTER U WITH DOT BELOW */
showKeySymbol XKB_KEY_Ubelowdot = "Ubelowdot"
-- #define XKB_KEY_ubelowdot                  0x1001ee5  /* U+1EE5 LATIN SMALL LETTER U WITH DOT BELOW */
showKeySymbol XKB_KEY_ubelowdot = "ubelowdot"
-- #define XKB_KEY_Uhook                      0x1001ee6  /* U+1EE6 LATIN CAPITAL LETTER U WITH HOOK ABOVE */
showKeySymbol XKB_KEY_Uhook = "Uhook"
-- #define XKB_KEY_uhook                      0x1001ee7  /* U+1EE7 LATIN SMALL LETTER U WITH HOOK ABOVE */
showKeySymbol XKB_KEY_uhook = "uhook"
-- #define XKB_KEY_Uhornacute                 0x1001ee8  /* U+1EE8 LATIN CAPITAL LETTER U WITH HORN AND ACUTE */
showKeySymbol XKB_KEY_Uhornacute = "Uhornacute"
-- #define XKB_KEY_uhornacute                 0x1001ee9  /* U+1EE9 LATIN SMALL LETTER U WITH HORN AND ACUTE */
showKeySymbol XKB_KEY_uhornacute = "uhornacute"
-- #define XKB_KEY_Uhorngrave                 0x1001eea  /* U+1EEA LATIN CAPITAL LETTER U WITH HORN AND GRAVE */
showKeySymbol XKB_KEY_Uhorngrave = "Uhorngrave"
-- #define XKB_KEY_uhorngrave                 0x1001eeb  /* U+1EEB LATIN SMALL LETTER U WITH HORN AND GRAVE */
showKeySymbol XKB_KEY_uhorngrave = "uhorngrave"
-- #define XKB_KEY_Uhornhook                  0x1001eec  /* U+1EEC LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE */
showKeySymbol XKB_KEY_Uhornhook = "Uhornhook"
-- #define XKB_KEY_uhornhook                  0x1001eed  /* U+1EED LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE */
showKeySymbol XKB_KEY_uhornhook = "uhornhook"
-- #define XKB_KEY_Uhorntilde                 0x1001eee  /* U+1EEE LATIN CAPITAL LETTER U WITH HORN AND TILDE */
showKeySymbol XKB_KEY_Uhorntilde = "Uhorntilde"
-- #define XKB_KEY_uhorntilde                 0x1001eef  /* U+1EEF LATIN SMALL LETTER U WITH HORN AND TILDE */
showKeySymbol XKB_KEY_uhorntilde = "uhorntilde"
-- #define XKB_KEY_Uhornbelowdot              0x1001ef0  /* U+1EF0 LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW */
showKeySymbol XKB_KEY_Uhornbelowdot = "Uhornbelowdot"
-- #define XKB_KEY_uhornbelowdot              0x1001ef1  /* U+1EF1 LATIN SMALL LETTER U WITH HORN AND DOT BELOW */
showKeySymbol XKB_KEY_uhornbelowdot = "uhornbelowdot"
-- #define XKB_KEY_Ybelowdot                  0x1001ef4  /* U+1EF4 LATIN CAPITAL LETTER Y WITH DOT BELOW */
showKeySymbol XKB_KEY_Ybelowdot = "Ybelowdot"
-- #define XKB_KEY_ybelowdot                  0x1001ef5  /* U+1EF5 LATIN SMALL LETTER Y WITH DOT BELOW */
showKeySymbol XKB_KEY_ybelowdot = "ybelowdot"
-- #define XKB_KEY_Yhook                      0x1001ef6  /* U+1EF6 LATIN CAPITAL LETTER Y WITH HOOK ABOVE */
showKeySymbol XKB_KEY_Yhook = "Yhook"
-- #define XKB_KEY_yhook                      0x1001ef7  /* U+1EF7 LATIN SMALL LETTER Y WITH HOOK ABOVE */
showKeySymbol XKB_KEY_yhook = "yhook"
-- #define XKB_KEY_Ytilde                     0x1001ef8  /* U+1EF8 LATIN CAPITAL LETTER Y WITH TILDE */
showKeySymbol XKB_KEY_Ytilde = "Ytilde"
-- #define XKB_KEY_ytilde                     0x1001ef9  /* U+1EF9 LATIN SMALL LETTER Y WITH TILDE */
showKeySymbol XKB_KEY_ytilde = "ytilde"
-- #define XKB_KEY_Ohorn                      0x10001a0  /* U+01A0 LATIN CAPITAL LETTER O WITH HORN */
showKeySymbol XKB_KEY_Ohorn = "Ohorn"
-- #define XKB_KEY_ohorn                      0x10001a1  /* U+01A1 LATIN SMALL LETTER O WITH HORN */
showKeySymbol XKB_KEY_ohorn = "ohorn"
-- #define XKB_KEY_Uhorn                      0x10001af  /* U+01AF LATIN CAPITAL LETTER U WITH HORN */
showKeySymbol XKB_KEY_Uhorn = "Uhorn"
-- #define XKB_KEY_uhorn                      0x10001b0  /* U+01B0 LATIN SMALL LETTER U WITH HORN */
showKeySymbol XKB_KEY_uhorn = "uhorn"
-- #define XKB_KEY_EcuSign                    0x10020a0  /* U+20A0 EURO-CURRENCY SIGN */
showKeySymbol XKB_KEY_EcuSign = "EcuSign"
-- #define XKB_KEY_ColonSign                  0x10020a1  /* U+20A1 COLON SIGN */
showKeySymbol XKB_KEY_ColonSign = "ColonSign"
-- #define XKB_KEY_CruzeiroSign               0x10020a2  /* U+20A2 CRUZEIRO SIGN */
showKeySymbol XKB_KEY_CruzeiroSign = "CruzeiroSign"
-- #define XKB_KEY_FFrancSign                 0x10020a3  /* U+20A3 FRENCH FRANC SIGN */
showKeySymbol XKB_KEY_FFrancSign = "FFrancSign"
-- #define XKB_KEY_LiraSign                   0x10020a4  /* U+20A4 LIRA SIGN */
showKeySymbol XKB_KEY_LiraSign = "LiraSign"
-- #define XKB_KEY_MillSign                   0x10020a5  /* U+20A5 MILL SIGN */
showKeySymbol XKB_KEY_MillSign = "MillSign"
-- #define XKB_KEY_NairaSign                  0x10020a6  /* U+20A6 NAIRA SIGN */
showKeySymbol XKB_KEY_NairaSign = "NairaSign"
-- #define XKB_KEY_PesetaSign                 0x10020a7  /* U+20A7 PESETA SIGN */
showKeySymbol XKB_KEY_PesetaSign = "PesetaSign"
-- #define XKB_KEY_RupeeSign                  0x10020a8  /* U+20A8 RUPEE SIGN */
showKeySymbol XKB_KEY_RupeeSign = "RupeeSign"
-- #define XKB_KEY_WonSign                    0x10020a9  /* U+20A9 WON SIGN */
showKeySymbol XKB_KEY_WonSign = "WonSign"
-- #define XKB_KEY_NewSheqelSign              0x10020aa  /* U+20AA NEW SHEQEL SIGN */
showKeySymbol XKB_KEY_NewSheqelSign = "NewSheqelSign"
-- #define XKB_KEY_DongSign                   0x10020ab  /* U+20AB DONG SIGN */
showKeySymbol XKB_KEY_DongSign = "DongSign"
-- #define XKB_KEY_EuroSign                      0x20ac  /* U+20AC EURO SIGN */
showKeySymbol XKB_KEY_EuroSign = "EuroSign"
-- #define XKB_KEY_zerosuperior               0x1002070  /* U+2070 SUPERSCRIPT ZERO */
showKeySymbol XKB_KEY_zerosuperior = "zerosuperior"
-- #define XKB_KEY_foursuperior               0x1002074  /* U+2074 SUPERSCRIPT FOUR */
showKeySymbol XKB_KEY_foursuperior = "foursuperior"
-- #define XKB_KEY_fivesuperior               0x1002075  /* U+2075 SUPERSCRIPT FIVE */
showKeySymbol XKB_KEY_fivesuperior = "fivesuperior"
-- #define XKB_KEY_sixsuperior                0x1002076  /* U+2076 SUPERSCRIPT SIX */
showKeySymbol XKB_KEY_sixsuperior = "sixsuperior"
-- #define XKB_KEY_sevensuperior              0x1002077  /* U+2077 SUPERSCRIPT SEVEN */
showKeySymbol XKB_KEY_sevensuperior = "sevensuperior"
-- #define XKB_KEY_eightsuperior              0x1002078  /* U+2078 SUPERSCRIPT EIGHT */
showKeySymbol XKB_KEY_eightsuperior = "eightsuperior"
-- #define XKB_KEY_ninesuperior               0x1002079  /* U+2079 SUPERSCRIPT NINE */
showKeySymbol XKB_KEY_ninesuperior = "ninesuperior"
-- #define XKB_KEY_zerosubscript              0x1002080  /* U+2080 SUBSCRIPT ZERO */
showKeySymbol XKB_KEY_zerosubscript = "zerosubscript"
-- #define XKB_KEY_onesubscript               0x1002081  /* U+2081 SUBSCRIPT ONE */
showKeySymbol XKB_KEY_onesubscript = "onesubscript"
-- #define XKB_KEY_twosubscript               0x1002082  /* U+2082 SUBSCRIPT TWO */
showKeySymbol XKB_KEY_twosubscript = "twosubscript"
-- #define XKB_KEY_threesubscript             0x1002083  /* U+2083 SUBSCRIPT THREE */
showKeySymbol XKB_KEY_threesubscript = "threesubscript"
-- #define XKB_KEY_foursubscript              0x1002084  /* U+2084 SUBSCRIPT FOUR */
showKeySymbol XKB_KEY_foursubscript = "foursubscript"
-- #define XKB_KEY_fivesubscript              0x1002085  /* U+2085 SUBSCRIPT FIVE */
showKeySymbol XKB_KEY_fivesubscript = "fivesubscript"
-- #define XKB_KEY_sixsubscript               0x1002086  /* U+2086 SUBSCRIPT SIX */
showKeySymbol XKB_KEY_sixsubscript = "sixsubscript"
-- #define XKB_KEY_sevensubscript             0x1002087  /* U+2087 SUBSCRIPT SEVEN */
showKeySymbol XKB_KEY_sevensubscript = "sevensubscript"
-- #define XKB_KEY_eightsubscript             0x1002088  /* U+2088 SUBSCRIPT EIGHT */
showKeySymbol XKB_KEY_eightsubscript = "eightsubscript"
-- #define XKB_KEY_ninesubscript              0x1002089  /* U+2089 SUBSCRIPT NINE */
showKeySymbol XKB_KEY_ninesubscript = "ninesubscript"
-- #define XKB_KEY_partdifferential           0x1002202  /* U+2202 PARTIAL DIFFERENTIAL */
showKeySymbol XKB_KEY_partdifferential = "partdifferential"
-- #define XKB_KEY_emptyset                   0x1002205  /* U+2205 NULL SET */
showKeySymbol XKB_KEY_emptyset = "emptyset"
-- #define XKB_KEY_elementof                  0x1002208  /* U+2208 ELEMENT OF */
showKeySymbol XKB_KEY_elementof = "elementof"
-- #define XKB_KEY_notelementof               0x1002209  /* U+2209 NOT AN ELEMENT OF */
showKeySymbol XKB_KEY_notelementof = "notelementof"
-- #define XKB_KEY_containsas                 0x100220B  /* U+220B CONTAINS AS MEMBER */
showKeySymbol XKB_KEY_containsas = "containsas"
-- #define XKB_KEY_squareroot                 0x100221A  /* U+221A SQUARE ROOT */
showKeySymbol XKB_KEY_squareroot = "squareroot"
-- #define XKB_KEY_cuberoot                   0x100221B  /* U+221B CUBE ROOT */
showKeySymbol XKB_KEY_cuberoot = "cuberoot"
-- #define XKB_KEY_fourthroot                 0x100221C  /* U+221C FOURTH ROOT */
showKeySymbol XKB_KEY_fourthroot = "fourthroot"
-- #define XKB_KEY_dintegral                  0x100222C  /* U+222C DOUBLE INTEGRAL */
showKeySymbol XKB_KEY_dintegral = "dintegral"
-- #define XKB_KEY_tintegral                  0x100222D  /* U+222D TRIPLE INTEGRAL */
showKeySymbol XKB_KEY_tintegral = "tintegral"
-- #define XKB_KEY_because                    0x1002235  /* U+2235 BECAUSE */
showKeySymbol XKB_KEY_because = "because"
-- #define XKB_KEY_approxeq                   0x1002248  /* U+2245 ALMOST EQUAL TO */
showKeySymbol XKB_KEY_approxeq = "approxeq"
-- #define XKB_KEY_notapproxeq                0x1002247  /* U+2247 NOT ALMOST EQUAL TO */
showKeySymbol XKB_KEY_notapproxeq = "notapproxeq"
-- #define XKB_KEY_notidentical               0x1002262  /* U+2262 NOT IDENTICAL TO */
showKeySymbol XKB_KEY_notidentical = "notidentical"
-- #define XKB_KEY_stricteq                   0x1002263  /* U+2263 STRICTLY EQUIVALENT TO */          
showKeySymbol XKB_KEY_stricteq = "stricteq"
-- #define XKB_KEY_braille_dot_1                 0xfff1
showKeySymbol XKB_KEY_braille_dot_1 = "braille_dot_1"
-- #define XKB_KEY_braille_dot_2                 0xfff2
showKeySymbol XKB_KEY_braille_dot_2 = "braille_dot_2"
-- #define XKB_KEY_braille_dot_3                 0xfff3
showKeySymbol XKB_KEY_braille_dot_3 = "braille_dot_3"
-- #define XKB_KEY_braille_dot_4                 0xfff4
showKeySymbol XKB_KEY_braille_dot_4 = "braille_dot_4"
-- #define XKB_KEY_braille_dot_5                 0xfff5
showKeySymbol XKB_KEY_braille_dot_5 = "braille_dot_5"
-- #define XKB_KEY_braille_dot_6                 0xfff6
showKeySymbol XKB_KEY_braille_dot_6 = "braille_dot_6"
-- #define XKB_KEY_braille_dot_7                 0xfff7
showKeySymbol XKB_KEY_braille_dot_7 = "braille_dot_7"
-- #define XKB_KEY_braille_dot_8                 0xfff8
showKeySymbol XKB_KEY_braille_dot_8 = "braille_dot_8"
-- #define XKB_KEY_braille_dot_9                 0xfff9
showKeySymbol XKB_KEY_braille_dot_9 = "braille_dot_9"
-- #define XKB_KEY_braille_dot_10                0xfffa
showKeySymbol XKB_KEY_braille_dot_10 = "braille_dot_10"
-- #define XKB_KEY_braille_blank              0x1002800  /* U+2800 BRAILLE PATTERN BLANK */
showKeySymbol XKB_KEY_braille_blank = "braille_blank"
-- #define XKB_KEY_braille_dots_1             0x1002801  /* U+2801 BRAILLE PATTERN DOTS-1 */
showKeySymbol XKB_KEY_braille_dots_1 = "braille_dots_1"
-- #define XKB_KEY_braille_dots_2             0x1002802  /* U+2802 BRAILLE PATTERN DOTS-2 */
showKeySymbol XKB_KEY_braille_dots_2 = "braille_dots_2"
-- #define XKB_KEY_braille_dots_12            0x1002803  /* U+2803 BRAILLE PATTERN DOTS-12 */
showKeySymbol XKB_KEY_braille_dots_12 = "braille_dots_12"
-- #define XKB_KEY_braille_dots_3             0x1002804  /* U+2804 BRAILLE PATTERN DOTS-3 */
showKeySymbol XKB_KEY_braille_dots_3 = "braille_dots_3"
-- #define XKB_KEY_braille_dots_13            0x1002805  /* U+2805 BRAILLE PATTERN DOTS-13 */
showKeySymbol XKB_KEY_braille_dots_13 = "braille_dots_13"
-- #define XKB_KEY_braille_dots_23            0x1002806  /* U+2806 BRAILLE PATTERN DOTS-23 */
showKeySymbol XKB_KEY_braille_dots_23 = "braille_dots_23"
-- #define XKB_KEY_braille_dots_123           0x1002807  /* U+2807 BRAILLE PATTERN DOTS-123 */
showKeySymbol XKB_KEY_braille_dots_123 = "braille_dots_123"
-- #define XKB_KEY_braille_dots_4             0x1002808  /* U+2808 BRAILLE PATTERN DOTS-4 */
showKeySymbol XKB_KEY_braille_dots_4 = "braille_dots_4"
-- #define XKB_KEY_braille_dots_14            0x1002809  /* U+2809 BRAILLE PATTERN DOTS-14 */
showKeySymbol XKB_KEY_braille_dots_14 = "braille_dots_14"
-- #define XKB_KEY_braille_dots_24            0x100280a  /* U+280a BRAILLE PATTERN DOTS-24 */
showKeySymbol XKB_KEY_braille_dots_24 = "braille_dots_24"
-- #define XKB_KEY_braille_dots_124           0x100280b  /* U+280b BRAILLE PATTERN DOTS-124 */
showKeySymbol XKB_KEY_braille_dots_124 = "braille_dots_124"
-- #define XKB_KEY_braille_dots_34            0x100280c  /* U+280c BRAILLE PATTERN DOTS-34 */
showKeySymbol XKB_KEY_braille_dots_34 = "braille_dots_34"
-- #define XKB_KEY_braille_dots_134           0x100280d  /* U+280d BRAILLE PATTERN DOTS-134 */
showKeySymbol XKB_KEY_braille_dots_134 = "braille_dots_134"
-- #define XKB_KEY_braille_dots_234           0x100280e  /* U+280e BRAILLE PATTERN DOTS-234 */
showKeySymbol XKB_KEY_braille_dots_234 = "braille_dots_234"
-- #define XKB_KEY_braille_dots_1234          0x100280f  /* U+280f BRAILLE PATTERN DOTS-1234 */
showKeySymbol XKB_KEY_braille_dots_1234 = "braille_dots_1234"
-- #define XKB_KEY_braille_dots_5             0x1002810  /* U+2810 BRAILLE PATTERN DOTS-5 */
showKeySymbol XKB_KEY_braille_dots_5 = "braille_dots_5"
-- #define XKB_KEY_braille_dots_15            0x1002811  /* U+2811 BRAILLE PATTERN DOTS-15 */
showKeySymbol XKB_KEY_braille_dots_15 = "braille_dots_15"
-- #define XKB_KEY_braille_dots_25            0x1002812  /* U+2812 BRAILLE PATTERN DOTS-25 */
showKeySymbol XKB_KEY_braille_dots_25 = "braille_dots_25"
-- #define XKB_KEY_braille_dots_125           0x1002813  /* U+2813 BRAILLE PATTERN DOTS-125 */
showKeySymbol XKB_KEY_braille_dots_125 = "braille_dots_125"
-- #define XKB_KEY_braille_dots_35            0x1002814  /* U+2814 BRAILLE PATTERN DOTS-35 */
showKeySymbol XKB_KEY_braille_dots_35 = "braille_dots_35"
-- #define XKB_KEY_braille_dots_135           0x1002815  /* U+2815 BRAILLE PATTERN DOTS-135 */
showKeySymbol XKB_KEY_braille_dots_135 = "braille_dots_135"
-- #define XKB_KEY_braille_dots_235           0x1002816  /* U+2816 BRAILLE PATTERN DOTS-235 */
showKeySymbol XKB_KEY_braille_dots_235 = "braille_dots_235"
-- #define XKB_KEY_braille_dots_1235          0x1002817  /* U+2817 BRAILLE PATTERN DOTS-1235 */
showKeySymbol XKB_KEY_braille_dots_1235 = "braille_dots_1235"
-- #define XKB_KEY_braille_dots_45            0x1002818  /* U+2818 BRAILLE PATTERN DOTS-45 */
showKeySymbol XKB_KEY_braille_dots_45 = "braille_dots_45"
-- #define XKB_KEY_braille_dots_145           0x1002819  /* U+2819 BRAILLE PATTERN DOTS-145 */
showKeySymbol XKB_KEY_braille_dots_145 = "braille_dots_145"
-- #define XKB_KEY_braille_dots_245           0x100281a  /* U+281a BRAILLE PATTERN DOTS-245 */
showKeySymbol XKB_KEY_braille_dots_245 = "braille_dots_245"
-- #define XKB_KEY_braille_dots_1245          0x100281b  /* U+281b BRAILLE PATTERN DOTS-1245 */
showKeySymbol XKB_KEY_braille_dots_1245 = "braille_dots_1245"
-- #define XKB_KEY_braille_dots_345           0x100281c  /* U+281c BRAILLE PATTERN DOTS-345 */
showKeySymbol XKB_KEY_braille_dots_345 = "braille_dots_345"
-- #define XKB_KEY_braille_dots_1345          0x100281d  /* U+281d BRAILLE PATTERN DOTS-1345 */
showKeySymbol XKB_KEY_braille_dots_1345 = "braille_dots_1345"
-- #define XKB_KEY_braille_dots_2345          0x100281e  /* U+281e BRAILLE PATTERN DOTS-2345 */
showKeySymbol XKB_KEY_braille_dots_2345 = "braille_dots_2345"
-- #define XKB_KEY_braille_dots_12345         0x100281f  /* U+281f BRAILLE PATTERN DOTS-12345 */
showKeySymbol XKB_KEY_braille_dots_12345 = "braille_dots_12345"
-- #define XKB_KEY_braille_dots_6             0x1002820  /* U+2820 BRAILLE PATTERN DOTS-6 */
showKeySymbol XKB_KEY_braille_dots_6 = "braille_dots_6"
-- #define XKB_KEY_braille_dots_16            0x1002821  /* U+2821 BRAILLE PATTERN DOTS-16 */
showKeySymbol XKB_KEY_braille_dots_16 = "braille_dots_16"
-- #define XKB_KEY_braille_dots_26            0x1002822  /* U+2822 BRAILLE PATTERN DOTS-26 */
showKeySymbol XKB_KEY_braille_dots_26 = "braille_dots_26"
-- #define XKB_KEY_braille_dots_126           0x1002823  /* U+2823 BRAILLE PATTERN DOTS-126 */
showKeySymbol XKB_KEY_braille_dots_126 = "braille_dots_126"
-- #define XKB_KEY_braille_dots_36            0x1002824  /* U+2824 BRAILLE PATTERN DOTS-36 */
showKeySymbol XKB_KEY_braille_dots_36 = "braille_dots_36"
-- #define XKB_KEY_braille_dots_136           0x1002825  /* U+2825 BRAILLE PATTERN DOTS-136 */
showKeySymbol XKB_KEY_braille_dots_136 = "braille_dots_136"
-- #define XKB_KEY_braille_dots_236           0x1002826  /* U+2826 BRAILLE PATTERN DOTS-236 */
showKeySymbol XKB_KEY_braille_dots_236 = "braille_dots_236"
-- #define XKB_KEY_braille_dots_1236          0x1002827  /* U+2827 BRAILLE PATTERN DOTS-1236 */
showKeySymbol XKB_KEY_braille_dots_1236 = "braille_dots_1236"
-- #define XKB_KEY_braille_dots_46            0x1002828  /* U+2828 BRAILLE PATTERN DOTS-46 */
showKeySymbol XKB_KEY_braille_dots_46 = "braille_dots_46"
-- #define XKB_KEY_braille_dots_146           0x1002829  /* U+2829 BRAILLE PATTERN DOTS-146 */
showKeySymbol XKB_KEY_braille_dots_146 = "braille_dots_146"
-- #define XKB_KEY_braille_dots_246           0x100282a  /* U+282a BRAILLE PATTERN DOTS-246 */
showKeySymbol XKB_KEY_braille_dots_246 = "braille_dots_246"
-- #define XKB_KEY_braille_dots_1246          0x100282b  /* U+282b BRAILLE PATTERN DOTS-1246 */
showKeySymbol XKB_KEY_braille_dots_1246 = "braille_dots_1246"
-- #define XKB_KEY_braille_dots_346           0x100282c  /* U+282c BRAILLE PATTERN DOTS-346 */
showKeySymbol XKB_KEY_braille_dots_346 = "braille_dots_346"
-- #define XKB_KEY_braille_dots_1346          0x100282d  /* U+282d BRAILLE PATTERN DOTS-1346 */
showKeySymbol XKB_KEY_braille_dots_1346 = "braille_dots_1346"
-- #define XKB_KEY_braille_dots_2346          0x100282e  /* U+282e BRAILLE PATTERN DOTS-2346 */
showKeySymbol XKB_KEY_braille_dots_2346 = "braille_dots_2346"
-- #define XKB_KEY_braille_dots_12346         0x100282f  /* U+282f BRAILLE PATTERN DOTS-12346 */
showKeySymbol XKB_KEY_braille_dots_12346 = "braille_dots_12346"
-- #define XKB_KEY_braille_dots_56            0x1002830  /* U+2830 BRAILLE PATTERN DOTS-56 */
showKeySymbol XKB_KEY_braille_dots_56 = "braille_dots_56"
-- #define XKB_KEY_braille_dots_156           0x1002831  /* U+2831 BRAILLE PATTERN DOTS-156 */
showKeySymbol XKB_KEY_braille_dots_156 = "braille_dots_156"
-- #define XKB_KEY_braille_dots_256           0x1002832  /* U+2832 BRAILLE PATTERN DOTS-256 */
showKeySymbol XKB_KEY_braille_dots_256 = "braille_dots_256"
-- #define XKB_KEY_braille_dots_1256          0x1002833  /* U+2833 BRAILLE PATTERN DOTS-1256 */
showKeySymbol XKB_KEY_braille_dots_1256 = "braille_dots_1256"
-- #define XKB_KEY_braille_dots_356           0x1002834  /* U+2834 BRAILLE PATTERN DOTS-356 */
showKeySymbol XKB_KEY_braille_dots_356 = "braille_dots_356"
-- #define XKB_KEY_braille_dots_1356          0x1002835  /* U+2835 BRAILLE PATTERN DOTS-1356 */
showKeySymbol XKB_KEY_braille_dots_1356 = "braille_dots_1356"
-- #define XKB_KEY_braille_dots_2356          0x1002836  /* U+2836 BRAILLE PATTERN DOTS-2356 */
showKeySymbol XKB_KEY_braille_dots_2356 = "braille_dots_2356"
-- #define XKB_KEY_braille_dots_12356         0x1002837  /* U+2837 BRAILLE PATTERN DOTS-12356 */
showKeySymbol XKB_KEY_braille_dots_12356 = "braille_dots_12356"
-- #define XKB_KEY_braille_dots_456           0x1002838  /* U+2838 BRAILLE PATTERN DOTS-456 */
showKeySymbol XKB_KEY_braille_dots_456 = "braille_dots_456"
-- #define XKB_KEY_braille_dots_1456          0x1002839  /* U+2839 BRAILLE PATTERN DOTS-1456 */
showKeySymbol XKB_KEY_braille_dots_1456 = "braille_dots_1456"
-- #define XKB_KEY_braille_dots_2456          0x100283a  /* U+283a BRAILLE PATTERN DOTS-2456 */
showKeySymbol XKB_KEY_braille_dots_2456 = "braille_dots_2456"
-- #define XKB_KEY_braille_dots_12456         0x100283b  /* U+283b BRAILLE PATTERN DOTS-12456 */
showKeySymbol XKB_KEY_braille_dots_12456 = "braille_dots_12456"
-- #define XKB_KEY_braille_dots_3456          0x100283c  /* U+283c BRAILLE PATTERN DOTS-3456 */
showKeySymbol XKB_KEY_braille_dots_3456 = "braille_dots_3456"
-- #define XKB_KEY_braille_dots_13456         0x100283d  /* U+283d BRAILLE PATTERN DOTS-13456 */
showKeySymbol XKB_KEY_braille_dots_13456 = "braille_dots_13456"
-- #define XKB_KEY_braille_dots_23456         0x100283e  /* U+283e BRAILLE PATTERN DOTS-23456 */
showKeySymbol XKB_KEY_braille_dots_23456 = "braille_dots_23456"
-- #define XKB_KEY_braille_dots_123456        0x100283f  /* U+283f BRAILLE PATTERN DOTS-123456 */
showKeySymbol XKB_KEY_braille_dots_123456 = "braille_dots_123456"
-- #define XKB_KEY_braille_dots_7             0x1002840  /* U+2840 BRAILLE PATTERN DOTS-7 */
showKeySymbol XKB_KEY_braille_dots_7 = "braille_dots_7"
-- #define XKB_KEY_braille_dots_17            0x1002841  /* U+2841 BRAILLE PATTERN DOTS-17 */
showKeySymbol XKB_KEY_braille_dots_17 = "braille_dots_17"
-- #define XKB_KEY_braille_dots_27            0x1002842  /* U+2842 BRAILLE PATTERN DOTS-27 */
showKeySymbol XKB_KEY_braille_dots_27 = "braille_dots_27"
-- #define XKB_KEY_braille_dots_127           0x1002843  /* U+2843 BRAILLE PATTERN DOTS-127 */
showKeySymbol XKB_KEY_braille_dots_127 = "braille_dots_127"
-- #define XKB_KEY_braille_dots_37            0x1002844  /* U+2844 BRAILLE PATTERN DOTS-37 */
showKeySymbol XKB_KEY_braille_dots_37 = "braille_dots_37"
-- #define XKB_KEY_braille_dots_137           0x1002845  /* U+2845 BRAILLE PATTERN DOTS-137 */
showKeySymbol XKB_KEY_braille_dots_137 = "braille_dots_137"
-- #define XKB_KEY_braille_dots_237           0x1002846  /* U+2846 BRAILLE PATTERN DOTS-237 */
showKeySymbol XKB_KEY_braille_dots_237 = "braille_dots_237"
-- #define XKB_KEY_braille_dots_1237          0x1002847  /* U+2847 BRAILLE PATTERN DOTS-1237 */
showKeySymbol XKB_KEY_braille_dots_1237 = "braille_dots_1237"
-- #define XKB_KEY_braille_dots_47            0x1002848  /* U+2848 BRAILLE PATTERN DOTS-47 */
showKeySymbol XKB_KEY_braille_dots_47 = "braille_dots_47"
-- #define XKB_KEY_braille_dots_147           0x1002849  /* U+2849 BRAILLE PATTERN DOTS-147 */
showKeySymbol XKB_KEY_braille_dots_147 = "braille_dots_147"
-- #define XKB_KEY_braille_dots_247           0x100284a  /* U+284a BRAILLE PATTERN DOTS-247 */
showKeySymbol XKB_KEY_braille_dots_247 = "braille_dots_247"
-- #define XKB_KEY_braille_dots_1247          0x100284b  /* U+284b BRAILLE PATTERN DOTS-1247 */
showKeySymbol XKB_KEY_braille_dots_1247 = "braille_dots_1247"
-- #define XKB_KEY_braille_dots_347           0x100284c  /* U+284c BRAILLE PATTERN DOTS-347 */
showKeySymbol XKB_KEY_braille_dots_347 = "braille_dots_347"
-- #define XKB_KEY_braille_dots_1347          0x100284d  /* U+284d BRAILLE PATTERN DOTS-1347 */
showKeySymbol XKB_KEY_braille_dots_1347 = "braille_dots_1347"
-- #define XKB_KEY_braille_dots_2347          0x100284e  /* U+284e BRAILLE PATTERN DOTS-2347 */
showKeySymbol XKB_KEY_braille_dots_2347 = "braille_dots_2347"
-- #define XKB_KEY_braille_dots_12347         0x100284f  /* U+284f BRAILLE PATTERN DOTS-12347 */
showKeySymbol XKB_KEY_braille_dots_12347 = "braille_dots_12347"
-- #define XKB_KEY_braille_dots_57            0x1002850  /* U+2850 BRAILLE PATTERN DOTS-57 */
showKeySymbol XKB_KEY_braille_dots_57 = "braille_dots_57"
-- #define XKB_KEY_braille_dots_157           0x1002851  /* U+2851 BRAILLE PATTERN DOTS-157 */
showKeySymbol XKB_KEY_braille_dots_157 = "braille_dots_157"
-- #define XKB_KEY_braille_dots_257           0x1002852  /* U+2852 BRAILLE PATTERN DOTS-257 */
showKeySymbol XKB_KEY_braille_dots_257 = "braille_dots_257"
-- #define XKB_KEY_braille_dots_1257          0x1002853  /* U+2853 BRAILLE PATTERN DOTS-1257 */
showKeySymbol XKB_KEY_braille_dots_1257 = "braille_dots_1257"
-- #define XKB_KEY_braille_dots_357           0x1002854  /* U+2854 BRAILLE PATTERN DOTS-357 */
showKeySymbol XKB_KEY_braille_dots_357 = "braille_dots_357"
-- #define XKB_KEY_braille_dots_1357          0x1002855  /* U+2855 BRAILLE PATTERN DOTS-1357 */
showKeySymbol XKB_KEY_braille_dots_1357 = "braille_dots_1357"
-- #define XKB_KEY_braille_dots_2357          0x1002856  /* U+2856 BRAILLE PATTERN DOTS-2357 */
showKeySymbol XKB_KEY_braille_dots_2357 = "braille_dots_2357"
-- #define XKB_KEY_braille_dots_12357         0x1002857  /* U+2857 BRAILLE PATTERN DOTS-12357 */
showKeySymbol XKB_KEY_braille_dots_12357 = "braille_dots_12357"
-- #define XKB_KEY_braille_dots_457           0x1002858  /* U+2858 BRAILLE PATTERN DOTS-457 */
showKeySymbol XKB_KEY_braille_dots_457 = "braille_dots_457"
-- #define XKB_KEY_braille_dots_1457          0x1002859  /* U+2859 BRAILLE PATTERN DOTS-1457 */
showKeySymbol XKB_KEY_braille_dots_1457 = "braille_dots_1457"
-- #define XKB_KEY_braille_dots_2457          0x100285a  /* U+285a BRAILLE PATTERN DOTS-2457 */
showKeySymbol XKB_KEY_braille_dots_2457 = "braille_dots_2457"
-- #define XKB_KEY_braille_dots_12457         0x100285b  /* U+285b BRAILLE PATTERN DOTS-12457 */
showKeySymbol XKB_KEY_braille_dots_12457 = "braille_dots_12457"
-- #define XKB_KEY_braille_dots_3457          0x100285c  /* U+285c BRAILLE PATTERN DOTS-3457 */
showKeySymbol XKB_KEY_braille_dots_3457 = "braille_dots_3457"
-- #define XKB_KEY_braille_dots_13457         0x100285d  /* U+285d BRAILLE PATTERN DOTS-13457 */
showKeySymbol XKB_KEY_braille_dots_13457 = "braille_dots_13457"
-- #define XKB_KEY_braille_dots_23457         0x100285e  /* U+285e BRAILLE PATTERN DOTS-23457 */
showKeySymbol XKB_KEY_braille_dots_23457 = "braille_dots_23457"
-- #define XKB_KEY_braille_dots_123457        0x100285f  /* U+285f BRAILLE PATTERN DOTS-123457 */
showKeySymbol XKB_KEY_braille_dots_123457 = "braille_dots_123457"
-- #define XKB_KEY_braille_dots_67            0x1002860  /* U+2860 BRAILLE PATTERN DOTS-67 */
showKeySymbol XKB_KEY_braille_dots_67 = "braille_dots_67"
-- #define XKB_KEY_braille_dots_167           0x1002861  /* U+2861 BRAILLE PATTERN DOTS-167 */
showKeySymbol XKB_KEY_braille_dots_167 = "braille_dots_167"
-- #define XKB_KEY_braille_dots_267           0x1002862  /* U+2862 BRAILLE PATTERN DOTS-267 */
showKeySymbol XKB_KEY_braille_dots_267 = "braille_dots_267"
-- #define XKB_KEY_braille_dots_1267          0x1002863  /* U+2863 BRAILLE PATTERN DOTS-1267 */
showKeySymbol XKB_KEY_braille_dots_1267 = "braille_dots_1267"
-- #define XKB_KEY_braille_dots_367           0x1002864  /* U+2864 BRAILLE PATTERN DOTS-367 */
showKeySymbol XKB_KEY_braille_dots_367 = "braille_dots_367"
-- #define XKB_KEY_braille_dots_1367          0x1002865  /* U+2865 BRAILLE PATTERN DOTS-1367 */
showKeySymbol XKB_KEY_braille_dots_1367 = "braille_dots_1367"
-- #define XKB_KEY_braille_dots_2367          0x1002866  /* U+2866 BRAILLE PATTERN DOTS-2367 */
showKeySymbol XKB_KEY_braille_dots_2367 = "braille_dots_2367"
-- #define XKB_KEY_braille_dots_12367         0x1002867  /* U+2867 BRAILLE PATTERN DOTS-12367 */
showKeySymbol XKB_KEY_braille_dots_12367 = "braille_dots_12367"
-- #define XKB_KEY_braille_dots_467           0x1002868  /* U+2868 BRAILLE PATTERN DOTS-467 */
showKeySymbol XKB_KEY_braille_dots_467 = "braille_dots_467"
-- #define XKB_KEY_braille_dots_1467          0x1002869  /* U+2869 BRAILLE PATTERN DOTS-1467 */
showKeySymbol XKB_KEY_braille_dots_1467 = "braille_dots_1467"
-- #define XKB_KEY_braille_dots_2467          0x100286a  /* U+286a BRAILLE PATTERN DOTS-2467 */
showKeySymbol XKB_KEY_braille_dots_2467 = "braille_dots_2467"
-- #define XKB_KEY_braille_dots_12467         0x100286b  /* U+286b BRAILLE PATTERN DOTS-12467 */
showKeySymbol XKB_KEY_braille_dots_12467 = "braille_dots_12467"
-- #define XKB_KEY_braille_dots_3467          0x100286c  /* U+286c BRAILLE PATTERN DOTS-3467 */
showKeySymbol XKB_KEY_braille_dots_3467 = "braille_dots_3467"
-- #define XKB_KEY_braille_dots_13467         0x100286d  /* U+286d BRAILLE PATTERN DOTS-13467 */
showKeySymbol XKB_KEY_braille_dots_13467 = "braille_dots_13467"
-- #define XKB_KEY_braille_dots_23467         0x100286e  /* U+286e BRAILLE PATTERN DOTS-23467 */
showKeySymbol XKB_KEY_braille_dots_23467 = "braille_dots_23467"
-- #define XKB_KEY_braille_dots_123467        0x100286f  /* U+286f BRAILLE PATTERN DOTS-123467 */
showKeySymbol XKB_KEY_braille_dots_123467 = "braille_dots_123467"
-- #define XKB_KEY_braille_dots_567           0x1002870  /* U+2870 BRAILLE PATTERN DOTS-567 */
showKeySymbol XKB_KEY_braille_dots_567 = "braille_dots_567"
-- #define XKB_KEY_braille_dots_1567          0x1002871  /* U+2871 BRAILLE PATTERN DOTS-1567 */
showKeySymbol XKB_KEY_braille_dots_1567 = "braille_dots_1567"
-- #define XKB_KEY_braille_dots_2567          0x1002872  /* U+2872 BRAILLE PATTERN DOTS-2567 */
showKeySymbol XKB_KEY_braille_dots_2567 = "braille_dots_2567"
-- #define XKB_KEY_braille_dots_12567         0x1002873  /* U+2873 BRAILLE PATTERN DOTS-12567 */
showKeySymbol XKB_KEY_braille_dots_12567 = "braille_dots_12567"
-- #define XKB_KEY_braille_dots_3567          0x1002874  /* U+2874 BRAILLE PATTERN DOTS-3567 */
showKeySymbol XKB_KEY_braille_dots_3567 = "braille_dots_3567"
-- #define XKB_KEY_braille_dots_13567         0x1002875  /* U+2875 BRAILLE PATTERN DOTS-13567 */
showKeySymbol XKB_KEY_braille_dots_13567 = "braille_dots_13567"
-- #define XKB_KEY_braille_dots_23567         0x1002876  /* U+2876 BRAILLE PATTERN DOTS-23567 */
showKeySymbol XKB_KEY_braille_dots_23567 = "braille_dots_23567"
-- #define XKB_KEY_braille_dots_123567        0x1002877  /* U+2877 BRAILLE PATTERN DOTS-123567 */
showKeySymbol XKB_KEY_braille_dots_123567 = "braille_dots_123567"
-- #define XKB_KEY_braille_dots_4567          0x1002878  /* U+2878 BRAILLE PATTERN DOTS-4567 */
showKeySymbol XKB_KEY_braille_dots_4567 = "braille_dots_4567"
-- #define XKB_KEY_braille_dots_14567         0x1002879  /* U+2879 BRAILLE PATTERN DOTS-14567 */
showKeySymbol XKB_KEY_braille_dots_14567 = "braille_dots_14567"
-- #define XKB_KEY_braille_dots_24567         0x100287a  /* U+287a BRAILLE PATTERN DOTS-24567 */
showKeySymbol XKB_KEY_braille_dots_24567 = "braille_dots_24567"
-- #define XKB_KEY_braille_dots_124567        0x100287b  /* U+287b BRAILLE PATTERN DOTS-124567 */
showKeySymbol XKB_KEY_braille_dots_124567 = "braille_dots_124567"
-- #define XKB_KEY_braille_dots_34567         0x100287c  /* U+287c BRAILLE PATTERN DOTS-34567 */
showKeySymbol XKB_KEY_braille_dots_34567 = "braille_dots_34567"
-- #define XKB_KEY_braille_dots_134567        0x100287d  /* U+287d BRAILLE PATTERN DOTS-134567 */
showKeySymbol XKB_KEY_braille_dots_134567 = "braille_dots_134567"
-- #define XKB_KEY_braille_dots_234567        0x100287e  /* U+287e BRAILLE PATTERN DOTS-234567 */
showKeySymbol XKB_KEY_braille_dots_234567 = "braille_dots_234567"
-- #define XKB_KEY_braille_dots_1234567       0x100287f  /* U+287f BRAILLE PATTERN DOTS-1234567 */
showKeySymbol XKB_KEY_braille_dots_1234567 = "braille_dots_1234567"
-- #define XKB_KEY_braille_dots_8             0x1002880  /* U+2880 BRAILLE PATTERN DOTS-8 */
showKeySymbol XKB_KEY_braille_dots_8 = "braille_dots_8"
-- #define XKB_KEY_braille_dots_18            0x1002881  /* U+2881 BRAILLE PATTERN DOTS-18 */
showKeySymbol XKB_KEY_braille_dots_18 = "braille_dots_18"
-- #define XKB_KEY_braille_dots_28            0x1002882  /* U+2882 BRAILLE PATTERN DOTS-28 */
showKeySymbol XKB_KEY_braille_dots_28 = "braille_dots_28"
-- #define XKB_KEY_braille_dots_128           0x1002883  /* U+2883 BRAILLE PATTERN DOTS-128 */
showKeySymbol XKB_KEY_braille_dots_128 = "braille_dots_128"
-- #define XKB_KEY_braille_dots_38            0x1002884  /* U+2884 BRAILLE PATTERN DOTS-38 */
showKeySymbol XKB_KEY_braille_dots_38 = "braille_dots_38"
-- #define XKB_KEY_braille_dots_138           0x1002885  /* U+2885 BRAILLE PATTERN DOTS-138 */
showKeySymbol XKB_KEY_braille_dots_138 = "braille_dots_138"
-- #define XKB_KEY_braille_dots_238           0x1002886  /* U+2886 BRAILLE PATTERN DOTS-238 */
showKeySymbol XKB_KEY_braille_dots_238 = "braille_dots_238"
-- #define XKB_KEY_braille_dots_1238          0x1002887  /* U+2887 BRAILLE PATTERN DOTS-1238 */
showKeySymbol XKB_KEY_braille_dots_1238 = "braille_dots_1238"
-- #define XKB_KEY_braille_dots_48            0x1002888  /* U+2888 BRAILLE PATTERN DOTS-48 */
showKeySymbol XKB_KEY_braille_dots_48 = "braille_dots_48"
-- #define XKB_KEY_braille_dots_148           0x1002889  /* U+2889 BRAILLE PATTERN DOTS-148 */
showKeySymbol XKB_KEY_braille_dots_148 = "braille_dots_148"
-- #define XKB_KEY_braille_dots_248           0x100288a  /* U+288a BRAILLE PATTERN DOTS-248 */
showKeySymbol XKB_KEY_braille_dots_248 = "braille_dots_248"
-- #define XKB_KEY_braille_dots_1248          0x100288b  /* U+288b BRAILLE PATTERN DOTS-1248 */
showKeySymbol XKB_KEY_braille_dots_1248 = "braille_dots_1248"
-- #define XKB_KEY_braille_dots_348           0x100288c  /* U+288c BRAILLE PATTERN DOTS-348 */
showKeySymbol XKB_KEY_braille_dots_348 = "braille_dots_348"
-- #define XKB_KEY_braille_dots_1348          0x100288d  /* U+288d BRAILLE PATTERN DOTS-1348 */
showKeySymbol XKB_KEY_braille_dots_1348 = "braille_dots_1348"
-- #define XKB_KEY_braille_dots_2348          0x100288e  /* U+288e BRAILLE PATTERN DOTS-2348 */
showKeySymbol XKB_KEY_braille_dots_2348 = "braille_dots_2348"
-- #define XKB_KEY_braille_dots_12348         0x100288f  /* U+288f BRAILLE PATTERN DOTS-12348 */
showKeySymbol XKB_KEY_braille_dots_12348 = "braille_dots_12348"
-- #define XKB_KEY_braille_dots_58            0x1002890  /* U+2890 BRAILLE PATTERN DOTS-58 */
showKeySymbol XKB_KEY_braille_dots_58 = "braille_dots_58"
-- #define XKB_KEY_braille_dots_158           0x1002891  /* U+2891 BRAILLE PATTERN DOTS-158 */
showKeySymbol XKB_KEY_braille_dots_158 = "braille_dots_158"
-- #define XKB_KEY_braille_dots_258           0x1002892  /* U+2892 BRAILLE PATTERN DOTS-258 */
showKeySymbol XKB_KEY_braille_dots_258 = "braille_dots_258"
-- #define XKB_KEY_braille_dots_1258          0x1002893  /* U+2893 BRAILLE PATTERN DOTS-1258 */
showKeySymbol XKB_KEY_braille_dots_1258 = "braille_dots_1258"
-- #define XKB_KEY_braille_dots_358           0x1002894  /* U+2894 BRAILLE PATTERN DOTS-358 */
showKeySymbol XKB_KEY_braille_dots_358 = "braille_dots_358"
-- #define XKB_KEY_braille_dots_1358          0x1002895  /* U+2895 BRAILLE PATTERN DOTS-1358 */
showKeySymbol XKB_KEY_braille_dots_1358 = "braille_dots_1358"
-- #define XKB_KEY_braille_dots_2358          0x1002896  /* U+2896 BRAILLE PATTERN DOTS-2358 */
showKeySymbol XKB_KEY_braille_dots_2358 = "braille_dots_2358"
-- #define XKB_KEY_braille_dots_12358         0x1002897  /* U+2897 BRAILLE PATTERN DOTS-12358 */
showKeySymbol XKB_KEY_braille_dots_12358 = "braille_dots_12358"
-- #define XKB_KEY_braille_dots_458           0x1002898  /* U+2898 BRAILLE PATTERN DOTS-458 */
showKeySymbol XKB_KEY_braille_dots_458 = "braille_dots_458"
-- #define XKB_KEY_braille_dots_1458          0x1002899  /* U+2899 BRAILLE PATTERN DOTS-1458 */
showKeySymbol XKB_KEY_braille_dots_1458 = "braille_dots_1458"
-- #define XKB_KEY_braille_dots_2458          0x100289a  /* U+289a BRAILLE PATTERN DOTS-2458 */
showKeySymbol XKB_KEY_braille_dots_2458 = "braille_dots_2458"
-- #define XKB_KEY_braille_dots_12458         0x100289b  /* U+289b BRAILLE PATTERN DOTS-12458 */
showKeySymbol XKB_KEY_braille_dots_12458 = "braille_dots_12458"
-- #define XKB_KEY_braille_dots_3458          0x100289c  /* U+289c BRAILLE PATTERN DOTS-3458 */
showKeySymbol XKB_KEY_braille_dots_3458 = "braille_dots_3458"
-- #define XKB_KEY_braille_dots_13458         0x100289d  /* U+289d BRAILLE PATTERN DOTS-13458 */
showKeySymbol XKB_KEY_braille_dots_13458 = "braille_dots_13458"
-- #define XKB_KEY_braille_dots_23458         0x100289e  /* U+289e BRAILLE PATTERN DOTS-23458 */
showKeySymbol XKB_KEY_braille_dots_23458 = "braille_dots_23458"
-- #define XKB_KEY_braille_dots_123458        0x100289f  /* U+289f BRAILLE PATTERN DOTS-123458 */
showKeySymbol XKB_KEY_braille_dots_123458 = "braille_dots_123458"
-- #define XKB_KEY_braille_dots_68            0x10028a0  /* U+28a0 BRAILLE PATTERN DOTS-68 */
showKeySymbol XKB_KEY_braille_dots_68 = "braille_dots_68"
-- #define XKB_KEY_braille_dots_168           0x10028a1  /* U+28a1 BRAILLE PATTERN DOTS-168 */
showKeySymbol XKB_KEY_braille_dots_168 = "braille_dots_168"
-- #define XKB_KEY_braille_dots_268           0x10028a2  /* U+28a2 BRAILLE PATTERN DOTS-268 */
showKeySymbol XKB_KEY_braille_dots_268 = "braille_dots_268"
-- #define XKB_KEY_braille_dots_1268          0x10028a3  /* U+28a3 BRAILLE PATTERN DOTS-1268 */
showKeySymbol XKB_KEY_braille_dots_1268 = "braille_dots_1268"
-- #define XKB_KEY_braille_dots_368           0x10028a4  /* U+28a4 BRAILLE PATTERN DOTS-368 */
showKeySymbol XKB_KEY_braille_dots_368 = "braille_dots_368"
-- #define XKB_KEY_braille_dots_1368          0x10028a5  /* U+28a5 BRAILLE PATTERN DOTS-1368 */
showKeySymbol XKB_KEY_braille_dots_1368 = "braille_dots_1368"
-- #define XKB_KEY_braille_dots_2368          0x10028a6  /* U+28a6 BRAILLE PATTERN DOTS-2368 */
showKeySymbol XKB_KEY_braille_dots_2368 = "braille_dots_2368"
-- #define XKB_KEY_braille_dots_12368         0x10028a7  /* U+28a7 BRAILLE PATTERN DOTS-12368 */
showKeySymbol XKB_KEY_braille_dots_12368 = "braille_dots_12368"
-- #define XKB_KEY_braille_dots_468           0x10028a8  /* U+28a8 BRAILLE PATTERN DOTS-468 */
showKeySymbol XKB_KEY_braille_dots_468 = "braille_dots_468"
-- #define XKB_KEY_braille_dots_1468          0x10028a9  /* U+28a9 BRAILLE PATTERN DOTS-1468 */
showKeySymbol XKB_KEY_braille_dots_1468 = "braille_dots_1468"
-- #define XKB_KEY_braille_dots_2468          0x10028aa  /* U+28aa BRAILLE PATTERN DOTS-2468 */
showKeySymbol XKB_KEY_braille_dots_2468 = "braille_dots_2468"
-- #define XKB_KEY_braille_dots_12468         0x10028ab  /* U+28ab BRAILLE PATTERN DOTS-12468 */
showKeySymbol XKB_KEY_braille_dots_12468 = "braille_dots_12468"
-- #define XKB_KEY_braille_dots_3468          0x10028ac  /* U+28ac BRAILLE PATTERN DOTS-3468 */
showKeySymbol XKB_KEY_braille_dots_3468 = "braille_dots_3468"
-- #define XKB_KEY_braille_dots_13468         0x10028ad  /* U+28ad BRAILLE PATTERN DOTS-13468 */
showKeySymbol XKB_KEY_braille_dots_13468 = "braille_dots_13468"
-- #define XKB_KEY_braille_dots_23468         0x10028ae  /* U+28ae BRAILLE PATTERN DOTS-23468 */
showKeySymbol XKB_KEY_braille_dots_23468 = "braille_dots_23468"
-- #define XKB_KEY_braille_dots_123468        0x10028af  /* U+28af BRAILLE PATTERN DOTS-123468 */
showKeySymbol XKB_KEY_braille_dots_123468 = "braille_dots_123468"
-- #define XKB_KEY_braille_dots_568           0x10028b0  /* U+28b0 BRAILLE PATTERN DOTS-568 */
showKeySymbol XKB_KEY_braille_dots_568 = "braille_dots_568"
-- #define XKB_KEY_braille_dots_1568          0x10028b1  /* U+28b1 BRAILLE PATTERN DOTS-1568 */
showKeySymbol XKB_KEY_braille_dots_1568 = "braille_dots_1568"
-- #define XKB_KEY_braille_dots_2568          0x10028b2  /* U+28b2 BRAILLE PATTERN DOTS-2568 */
showKeySymbol XKB_KEY_braille_dots_2568 = "braille_dots_2568"
-- #define XKB_KEY_braille_dots_12568         0x10028b3  /* U+28b3 BRAILLE PATTERN DOTS-12568 */
showKeySymbol XKB_KEY_braille_dots_12568 = "braille_dots_12568"
-- #define XKB_KEY_braille_dots_3568          0x10028b4  /* U+28b4 BRAILLE PATTERN DOTS-3568 */
showKeySymbol XKB_KEY_braille_dots_3568 = "braille_dots_3568"
-- #define XKB_KEY_braille_dots_13568         0x10028b5  /* U+28b5 BRAILLE PATTERN DOTS-13568 */
showKeySymbol XKB_KEY_braille_dots_13568 = "braille_dots_13568"
-- #define XKB_KEY_braille_dots_23568         0x10028b6  /* U+28b6 BRAILLE PATTERN DOTS-23568 */
showKeySymbol XKB_KEY_braille_dots_23568 = "braille_dots_23568"
-- #define XKB_KEY_braille_dots_123568        0x10028b7  /* U+28b7 BRAILLE PATTERN DOTS-123568 */
showKeySymbol XKB_KEY_braille_dots_123568 = "braille_dots_123568"
-- #define XKB_KEY_braille_dots_4568          0x10028b8  /* U+28b8 BRAILLE PATTERN DOTS-4568 */
showKeySymbol XKB_KEY_braille_dots_4568 = "braille_dots_4568"
-- #define XKB_KEY_braille_dots_14568         0x10028b9  /* U+28b9 BRAILLE PATTERN DOTS-14568 */
showKeySymbol XKB_KEY_braille_dots_14568 = "braille_dots_14568"
-- #define XKB_KEY_braille_dots_24568         0x10028ba  /* U+28ba BRAILLE PATTERN DOTS-24568 */
showKeySymbol XKB_KEY_braille_dots_24568 = "braille_dots_24568"
-- #define XKB_KEY_braille_dots_124568        0x10028bb  /* U+28bb BRAILLE PATTERN DOTS-124568 */
showKeySymbol XKB_KEY_braille_dots_124568 = "braille_dots_124568"
-- #define XKB_KEY_braille_dots_34568         0x10028bc  /* U+28bc BRAILLE PATTERN DOTS-34568 */
showKeySymbol XKB_KEY_braille_dots_34568 = "braille_dots_34568"
-- #define XKB_KEY_braille_dots_134568        0x10028bd  /* U+28bd BRAILLE PATTERN DOTS-134568 */
showKeySymbol XKB_KEY_braille_dots_134568 = "braille_dots_134568"
-- #define XKB_KEY_braille_dots_234568        0x10028be  /* U+28be BRAILLE PATTERN DOTS-234568 */
showKeySymbol XKB_KEY_braille_dots_234568 = "braille_dots_234568"
-- #define XKB_KEY_braille_dots_1234568       0x10028bf  /* U+28bf BRAILLE PATTERN DOTS-1234568 */
showKeySymbol XKB_KEY_braille_dots_1234568 = "braille_dots_1234568"
-- #define XKB_KEY_braille_dots_78            0x10028c0  /* U+28c0 BRAILLE PATTERN DOTS-78 */
showKeySymbol XKB_KEY_braille_dots_78 = "braille_dots_78"
-- #define XKB_KEY_braille_dots_178           0x10028c1  /* U+28c1 BRAILLE PATTERN DOTS-178 */
showKeySymbol XKB_KEY_braille_dots_178 = "braille_dots_178"
-- #define XKB_KEY_braille_dots_278           0x10028c2  /* U+28c2 BRAILLE PATTERN DOTS-278 */
showKeySymbol XKB_KEY_braille_dots_278 = "braille_dots_278"
-- #define XKB_KEY_braille_dots_1278          0x10028c3  /* U+28c3 BRAILLE PATTERN DOTS-1278 */
showKeySymbol XKB_KEY_braille_dots_1278 = "braille_dots_1278"
-- #define XKB_KEY_braille_dots_378           0x10028c4  /* U+28c4 BRAILLE PATTERN DOTS-378 */
showKeySymbol XKB_KEY_braille_dots_378 = "braille_dots_378"
-- #define XKB_KEY_braille_dots_1378          0x10028c5  /* U+28c5 BRAILLE PATTERN DOTS-1378 */
showKeySymbol XKB_KEY_braille_dots_1378 = "braille_dots_1378"
-- #define XKB_KEY_braille_dots_2378          0x10028c6  /* U+28c6 BRAILLE PATTERN DOTS-2378 */
showKeySymbol XKB_KEY_braille_dots_2378 = "braille_dots_2378"
-- #define XKB_KEY_braille_dots_12378         0x10028c7  /* U+28c7 BRAILLE PATTERN DOTS-12378 */
showKeySymbol XKB_KEY_braille_dots_12378 = "braille_dots_12378"
-- #define XKB_KEY_braille_dots_478           0x10028c8  /* U+28c8 BRAILLE PATTERN DOTS-478 */
showKeySymbol XKB_KEY_braille_dots_478 = "braille_dots_478"
-- #define XKB_KEY_braille_dots_1478          0x10028c9  /* U+28c9 BRAILLE PATTERN DOTS-1478 */
showKeySymbol XKB_KEY_braille_dots_1478 = "braille_dots_1478"
-- #define XKB_KEY_braille_dots_2478          0x10028ca  /* U+28ca BRAILLE PATTERN DOTS-2478 */
showKeySymbol XKB_KEY_braille_dots_2478 = "braille_dots_2478"
-- #define XKB_KEY_braille_dots_12478         0x10028cb  /* U+28cb BRAILLE PATTERN DOTS-12478 */
showKeySymbol XKB_KEY_braille_dots_12478 = "braille_dots_12478"
-- #define XKB_KEY_braille_dots_3478          0x10028cc  /* U+28cc BRAILLE PATTERN DOTS-3478 */
showKeySymbol XKB_KEY_braille_dots_3478 = "braille_dots_3478"
-- #define XKB_KEY_braille_dots_13478         0x10028cd  /* U+28cd BRAILLE PATTERN DOTS-13478 */
showKeySymbol XKB_KEY_braille_dots_13478 = "braille_dots_13478"
-- #define XKB_KEY_braille_dots_23478         0x10028ce  /* U+28ce BRAILLE PATTERN DOTS-23478 */
showKeySymbol XKB_KEY_braille_dots_23478 = "braille_dots_23478"
-- #define XKB_KEY_braille_dots_123478        0x10028cf  /* U+28cf BRAILLE PATTERN DOTS-123478 */
showKeySymbol XKB_KEY_braille_dots_123478 = "braille_dots_123478"
-- #define XKB_KEY_braille_dots_578           0x10028d0  /* U+28d0 BRAILLE PATTERN DOTS-578 */
showKeySymbol XKB_KEY_braille_dots_578 = "braille_dots_578"
-- #define XKB_KEY_braille_dots_1578          0x10028d1  /* U+28d1 BRAILLE PATTERN DOTS-1578 */
showKeySymbol XKB_KEY_braille_dots_1578 = "braille_dots_1578"
-- #define XKB_KEY_braille_dots_2578          0x10028d2  /* U+28d2 BRAILLE PATTERN DOTS-2578 */
showKeySymbol XKB_KEY_braille_dots_2578 = "braille_dots_2578"
-- #define XKB_KEY_braille_dots_12578         0x10028d3  /* U+28d3 BRAILLE PATTERN DOTS-12578 */
showKeySymbol XKB_KEY_braille_dots_12578 = "braille_dots_12578"
-- #define XKB_KEY_braille_dots_3578          0x10028d4  /* U+28d4 BRAILLE PATTERN DOTS-3578 */
showKeySymbol XKB_KEY_braille_dots_3578 = "braille_dots_3578"
-- #define XKB_KEY_braille_dots_13578         0x10028d5  /* U+28d5 BRAILLE PATTERN DOTS-13578 */
showKeySymbol XKB_KEY_braille_dots_13578 = "braille_dots_13578"
-- #define XKB_KEY_braille_dots_23578         0x10028d6  /* U+28d6 BRAILLE PATTERN DOTS-23578 */
showKeySymbol XKB_KEY_braille_dots_23578 = "braille_dots_23578"
-- #define XKB_KEY_braille_dots_123578        0x10028d7  /* U+28d7 BRAILLE PATTERN DOTS-123578 */
showKeySymbol XKB_KEY_braille_dots_123578 = "braille_dots_123578"
-- #define XKB_KEY_braille_dots_4578          0x10028d8  /* U+28d8 BRAILLE PATTERN DOTS-4578 */
showKeySymbol XKB_KEY_braille_dots_4578 = "braille_dots_4578"
-- #define XKB_KEY_braille_dots_14578         0x10028d9  /* U+28d9 BRAILLE PATTERN DOTS-14578 */
showKeySymbol XKB_KEY_braille_dots_14578 = "braille_dots_14578"
-- #define XKB_KEY_braille_dots_24578         0x10028da  /* U+28da BRAILLE PATTERN DOTS-24578 */
showKeySymbol XKB_KEY_braille_dots_24578 = "braille_dots_24578"
-- #define XKB_KEY_braille_dots_124578        0x10028db  /* U+28db BRAILLE PATTERN DOTS-124578 */
showKeySymbol XKB_KEY_braille_dots_124578 = "braille_dots_124578"
-- #define XKB_KEY_braille_dots_34578         0x10028dc  /* U+28dc BRAILLE PATTERN DOTS-34578 */
showKeySymbol XKB_KEY_braille_dots_34578 = "braille_dots_34578"
-- #define XKB_KEY_braille_dots_134578        0x10028dd  /* U+28dd BRAILLE PATTERN DOTS-134578 */
showKeySymbol XKB_KEY_braille_dots_134578 = "braille_dots_134578"
-- #define XKB_KEY_braille_dots_234578        0x10028de  /* U+28de BRAILLE PATTERN DOTS-234578 */
showKeySymbol XKB_KEY_braille_dots_234578 = "braille_dots_234578"
-- #define XKB_KEY_braille_dots_1234578       0x10028df  /* U+28df BRAILLE PATTERN DOTS-1234578 */
showKeySymbol XKB_KEY_braille_dots_1234578 = "braille_dots_1234578"
-- #define XKB_KEY_braille_dots_678           0x10028e0  /* U+28e0 BRAILLE PATTERN DOTS-678 */
showKeySymbol XKB_KEY_braille_dots_678 = "braille_dots_678"
-- #define XKB_KEY_braille_dots_1678          0x10028e1  /* U+28e1 BRAILLE PATTERN DOTS-1678 */
showKeySymbol XKB_KEY_braille_dots_1678 = "braille_dots_1678"
-- #define XKB_KEY_braille_dots_2678          0x10028e2  /* U+28e2 BRAILLE PATTERN DOTS-2678 */
showKeySymbol XKB_KEY_braille_dots_2678 = "braille_dots_2678"
-- #define XKB_KEY_braille_dots_12678         0x10028e3  /* U+28e3 BRAILLE PATTERN DOTS-12678 */
showKeySymbol XKB_KEY_braille_dots_12678 = "braille_dots_12678"
-- #define XKB_KEY_braille_dots_3678          0x10028e4  /* U+28e4 BRAILLE PATTERN DOTS-3678 */
showKeySymbol XKB_KEY_braille_dots_3678 = "braille_dots_3678"
-- #define XKB_KEY_braille_dots_13678         0x10028e5  /* U+28e5 BRAILLE PATTERN DOTS-13678 */
showKeySymbol XKB_KEY_braille_dots_13678 = "braille_dots_13678"
-- #define XKB_KEY_braille_dots_23678         0x10028e6  /* U+28e6 BRAILLE PATTERN DOTS-23678 */
showKeySymbol XKB_KEY_braille_dots_23678 = "braille_dots_23678"
-- #define XKB_KEY_braille_dots_123678        0x10028e7  /* U+28e7 BRAILLE PATTERN DOTS-123678 */
showKeySymbol XKB_KEY_braille_dots_123678 = "braille_dots_123678"
-- #define XKB_KEY_braille_dots_4678          0x10028e8  /* U+28e8 BRAILLE PATTERN DOTS-4678 */
showKeySymbol XKB_KEY_braille_dots_4678 = "braille_dots_4678"
-- #define XKB_KEY_braille_dots_14678         0x10028e9  /* U+28e9 BRAILLE PATTERN DOTS-14678 */
showKeySymbol XKB_KEY_braille_dots_14678 = "braille_dots_14678"
-- #define XKB_KEY_braille_dots_24678         0x10028ea  /* U+28ea BRAILLE PATTERN DOTS-24678 */
showKeySymbol XKB_KEY_braille_dots_24678 = "braille_dots_24678"
-- #define XKB_KEY_braille_dots_124678        0x10028eb  /* U+28eb BRAILLE PATTERN DOTS-124678 */
showKeySymbol XKB_KEY_braille_dots_124678 = "braille_dots_124678"
-- #define XKB_KEY_braille_dots_34678         0x10028ec  /* U+28ec BRAILLE PATTERN DOTS-34678 */
showKeySymbol XKB_KEY_braille_dots_34678 = "braille_dots_34678"
-- #define XKB_KEY_braille_dots_134678        0x10028ed  /* U+28ed BRAILLE PATTERN DOTS-134678 */
showKeySymbol XKB_KEY_braille_dots_134678 = "braille_dots_134678"
-- #define XKB_KEY_braille_dots_234678        0x10028ee  /* U+28ee BRAILLE PATTERN DOTS-234678 */
showKeySymbol XKB_KEY_braille_dots_234678 = "braille_dots_234678"
-- #define XKB_KEY_braille_dots_1234678       0x10028ef  /* U+28ef BRAILLE PATTERN DOTS-1234678 */
showKeySymbol XKB_KEY_braille_dots_1234678 = "braille_dots_1234678"
-- #define XKB_KEY_braille_dots_5678          0x10028f0  /* U+28f0 BRAILLE PATTERN DOTS-5678 */
showKeySymbol XKB_KEY_braille_dots_5678 = "braille_dots_5678"
-- #define XKB_KEY_braille_dots_15678         0x10028f1  /* U+28f1 BRAILLE PATTERN DOTS-15678 */
showKeySymbol XKB_KEY_braille_dots_15678 = "braille_dots_15678"
-- #define XKB_KEY_braille_dots_25678         0x10028f2  /* U+28f2 BRAILLE PATTERN DOTS-25678 */
showKeySymbol XKB_KEY_braille_dots_25678 = "braille_dots_25678"
-- #define XKB_KEY_braille_dots_125678        0x10028f3  /* U+28f3 BRAILLE PATTERN DOTS-125678 */
showKeySymbol XKB_KEY_braille_dots_125678 = "braille_dots_125678"
-- #define XKB_KEY_braille_dots_35678         0x10028f4  /* U+28f4 BRAILLE PATTERN DOTS-35678 */
showKeySymbol XKB_KEY_braille_dots_35678 = "braille_dots_35678"
-- #define XKB_KEY_braille_dots_135678        0x10028f5  /* U+28f5 BRAILLE PATTERN DOTS-135678 */
showKeySymbol XKB_KEY_braille_dots_135678 = "braille_dots_135678"
-- #define XKB_KEY_braille_dots_235678        0x10028f6  /* U+28f6 BRAILLE PATTERN DOTS-235678 */
showKeySymbol XKB_KEY_braille_dots_235678 = "braille_dots_235678"
-- #define XKB_KEY_braille_dots_1235678       0x10028f7  /* U+28f7 BRAILLE PATTERN DOTS-1235678 */
showKeySymbol XKB_KEY_braille_dots_1235678 = "braille_dots_1235678"
-- #define XKB_KEY_braille_dots_45678         0x10028f8  /* U+28f8 BRAILLE PATTERN DOTS-45678 */
showKeySymbol XKB_KEY_braille_dots_45678 = "braille_dots_45678"
-- #define XKB_KEY_braille_dots_145678        0x10028f9  /* U+28f9 BRAILLE PATTERN DOTS-145678 */
showKeySymbol XKB_KEY_braille_dots_145678 = "braille_dots_145678"
-- #define XKB_KEY_braille_dots_245678        0x10028fa  /* U+28fa BRAILLE PATTERN DOTS-245678 */
showKeySymbol XKB_KEY_braille_dots_245678 = "braille_dots_245678"
-- #define XKB_KEY_braille_dots_1245678       0x10028fb  /* U+28fb BRAILLE PATTERN DOTS-1245678 */
showKeySymbol XKB_KEY_braille_dots_1245678 = "braille_dots_1245678"
-- #define XKB_KEY_braille_dots_345678        0x10028fc  /* U+28fc BRAILLE PATTERN DOTS-345678 */
showKeySymbol XKB_KEY_braille_dots_345678 = "braille_dots_345678"
-- #define XKB_KEY_braille_dots_1345678       0x10028fd  /* U+28fd BRAILLE PATTERN DOTS-1345678 */
showKeySymbol XKB_KEY_braille_dots_1345678 = "braille_dots_1345678"
-- #define XKB_KEY_braille_dots_2345678       0x10028fe  /* U+28fe BRAILLE PATTERN DOTS-2345678 */
showKeySymbol XKB_KEY_braille_dots_2345678 = "braille_dots_2345678"
-- #define XKB_KEY_braille_dots_12345678      0x10028ff  /* U+28ff BRAILLE PATTERN DOTS-12345678 */
showKeySymbol XKB_KEY_braille_dots_12345678 = "braille_dots_12345678"
-- #define XKB_KEY_Sinh_ng            0x1000d82  /* U+0D82 SINHALA ANUSVARAYA */
showKeySymbol XKB_KEY_Sinh_ng = "Sinh_ng"
-- #define XKB_KEY_Sinh_h2            0x1000d83  /* U+0D83 SINHALA VISARGAYA */
showKeySymbol XKB_KEY_Sinh_h2 = "Sinh_h2"
-- #define XKB_KEY_Sinh_a             0x1000d85  /* U+0D85 SINHALA AYANNA */
showKeySymbol XKB_KEY_Sinh_a = "Sinh_a"
-- #define XKB_KEY_Sinh_aa            0x1000d86  /* U+0D86 SINHALA AAYANNA */
showKeySymbol XKB_KEY_Sinh_aa = "Sinh_aa"
-- #define XKB_KEY_Sinh_ae            0x1000d87  /* U+0D87 SINHALA AEYANNA */
showKeySymbol XKB_KEY_Sinh_ae = "Sinh_ae"
-- #define XKB_KEY_Sinh_aee           0x1000d88  /* U+0D88 SINHALA AEEYANNA */
showKeySymbol XKB_KEY_Sinh_aee = "Sinh_aee"
-- #define XKB_KEY_Sinh_i             0x1000d89  /* U+0D89 SINHALA IYANNA */
showKeySymbol XKB_KEY_Sinh_i = "Sinh_i"
-- #define XKB_KEY_Sinh_ii            0x1000d8a  /* U+0D8A SINHALA IIYANNA */
showKeySymbol XKB_KEY_Sinh_ii = "Sinh_ii"
-- #define XKB_KEY_Sinh_u             0x1000d8b  /* U+0D8B SINHALA UYANNA */
showKeySymbol XKB_KEY_Sinh_u = "Sinh_u"
-- #define XKB_KEY_Sinh_uu            0x1000d8c  /* U+0D8C SINHALA UUYANNA */
showKeySymbol XKB_KEY_Sinh_uu = "Sinh_uu"
-- #define XKB_KEY_Sinh_ri            0x1000d8d  /* U+0D8D SINHALA IRUYANNA */
showKeySymbol XKB_KEY_Sinh_ri = "Sinh_ri"
-- #define XKB_KEY_Sinh_rii           0x1000d8e  /* U+0D8E SINHALA IRUUYANNA */
showKeySymbol XKB_KEY_Sinh_rii = "Sinh_rii"
-- #define XKB_KEY_Sinh_lu            0x1000d8f  /* U+0D8F SINHALA ILUYANNA */
showKeySymbol XKB_KEY_Sinh_lu = "Sinh_lu"
-- #define XKB_KEY_Sinh_luu           0x1000d90  /* U+0D90 SINHALA ILUUYANNA */
showKeySymbol XKB_KEY_Sinh_luu = "Sinh_luu"
-- #define XKB_KEY_Sinh_e             0x1000d91  /* U+0D91 SINHALA EYANNA */
showKeySymbol XKB_KEY_Sinh_e = "Sinh_e"
-- #define XKB_KEY_Sinh_ee            0x1000d92  /* U+0D92 SINHALA EEYANNA */
showKeySymbol XKB_KEY_Sinh_ee = "Sinh_ee"
-- #define XKB_KEY_Sinh_ai            0x1000d93  /* U+0D93 SINHALA AIYANNA */
showKeySymbol XKB_KEY_Sinh_ai = "Sinh_ai"
-- #define XKB_KEY_Sinh_o             0x1000d94  /* U+0D94 SINHALA OYANNA */
showKeySymbol XKB_KEY_Sinh_o = "Sinh_o"
-- #define XKB_KEY_Sinh_oo            0x1000d95  /* U+0D95 SINHALA OOYANNA */
showKeySymbol XKB_KEY_Sinh_oo = "Sinh_oo"
-- #define XKB_KEY_Sinh_au            0x1000d96  /* U+0D96 SINHALA AUYANNA */
showKeySymbol XKB_KEY_Sinh_au = "Sinh_au"
-- #define XKB_KEY_Sinh_ka            0x1000d9a  /* U+0D9A SINHALA KAYANNA */
showKeySymbol XKB_KEY_Sinh_ka = "Sinh_ka"
-- #define XKB_KEY_Sinh_kha           0x1000d9b  /* U+0D9B SINHALA MAHA. KAYANNA */
showKeySymbol XKB_KEY_Sinh_kha = "Sinh_kha"
-- #define XKB_KEY_Sinh_ga            0x1000d9c  /* U+0D9C SINHALA GAYANNA */
showKeySymbol XKB_KEY_Sinh_ga = "Sinh_ga"
-- #define XKB_KEY_Sinh_gha           0x1000d9d  /* U+0D9D SINHALA MAHA. GAYANNA */
showKeySymbol XKB_KEY_Sinh_gha = "Sinh_gha"
-- #define XKB_KEY_Sinh_ng2           0x1000d9e  /* U+0D9E SINHALA KANTAJA NAASIKYAYA */
showKeySymbol XKB_KEY_Sinh_ng2 = "Sinh_ng2"
-- #define XKB_KEY_Sinh_nga           0x1000d9f  /* U+0D9F SINHALA SANYAKA GAYANNA */
showKeySymbol XKB_KEY_Sinh_nga = "Sinh_nga"
-- #define XKB_KEY_Sinh_ca            0x1000da0  /* U+0DA0 SINHALA CAYANNA */
showKeySymbol XKB_KEY_Sinh_ca = "Sinh_ca"
-- #define XKB_KEY_Sinh_cha           0x1000da1  /* U+0DA1 SINHALA MAHA. CAYANNA */
showKeySymbol XKB_KEY_Sinh_cha = "Sinh_cha"
-- #define XKB_KEY_Sinh_ja            0x1000da2  /* U+0DA2 SINHALA JAYANNA */
showKeySymbol XKB_KEY_Sinh_ja = "Sinh_ja"
-- #define XKB_KEY_Sinh_jha           0x1000da3  /* U+0DA3 SINHALA MAHA. JAYANNA */
showKeySymbol XKB_KEY_Sinh_jha = "Sinh_jha"
-- #define XKB_KEY_Sinh_nya           0x1000da4  /* U+0DA4 SINHALA TAALUJA NAASIKYAYA */
showKeySymbol XKB_KEY_Sinh_nya = "Sinh_nya"
-- #define XKB_KEY_Sinh_jnya          0x1000da5  /* U+0DA5 SINHALA TAALUJA SANYOOGA NAASIKYAYA */
showKeySymbol XKB_KEY_Sinh_jnya = "Sinh_jnya"
-- #define XKB_KEY_Sinh_nja           0x1000da6  /* U+0DA6 SINHALA SANYAKA JAYANNA */
showKeySymbol XKB_KEY_Sinh_nja = "Sinh_nja"
-- #define XKB_KEY_Sinh_tta           0x1000da7  /* U+0DA7 SINHALA TTAYANNA */
showKeySymbol XKB_KEY_Sinh_tta = "Sinh_tta"
-- #define XKB_KEY_Sinh_ttha          0x1000da8  /* U+0DA8 SINHALA MAHA. TTAYANNA */
showKeySymbol XKB_KEY_Sinh_ttha = "Sinh_ttha"
-- #define XKB_KEY_Sinh_dda           0x1000da9  /* U+0DA9 SINHALA DDAYANNA */
showKeySymbol XKB_KEY_Sinh_dda = "Sinh_dda"
-- #define XKB_KEY_Sinh_ddha          0x1000daa  /* U+0DAA SINHALA MAHA. DDAYANNA */
showKeySymbol XKB_KEY_Sinh_ddha = "Sinh_ddha"
-- #define XKB_KEY_Sinh_nna           0x1000dab  /* U+0DAB SINHALA MUURDHAJA NAYANNA */
showKeySymbol XKB_KEY_Sinh_nna = "Sinh_nna"
-- #define XKB_KEY_Sinh_ndda          0x1000dac  /* U+0DAC SINHALA SANYAKA DDAYANNA */
showKeySymbol XKB_KEY_Sinh_ndda = "Sinh_ndda"
-- #define XKB_KEY_Sinh_tha           0x1000dad  /* U+0DAD SINHALA TAYANNA */
showKeySymbol XKB_KEY_Sinh_tha = "Sinh_tha"
-- #define XKB_KEY_Sinh_thha          0x1000dae  /* U+0DAE SINHALA MAHA. TAYANNA */
showKeySymbol XKB_KEY_Sinh_thha = "Sinh_thha"
-- #define XKB_KEY_Sinh_dha           0x1000daf  /* U+0DAF SINHALA DAYANNA */
showKeySymbol XKB_KEY_Sinh_dha = "Sinh_dha"
-- #define XKB_KEY_Sinh_dhha          0x1000db0  /* U+0DB0 SINHALA MAHA. DAYANNA */
showKeySymbol XKB_KEY_Sinh_dhha = "Sinh_dhha"
-- #define XKB_KEY_Sinh_na            0x1000db1  /* U+0DB1 SINHALA DANTAJA NAYANNA */
showKeySymbol XKB_KEY_Sinh_na = "Sinh_na"
-- #define XKB_KEY_Sinh_ndha          0x1000db3  /* U+0DB3 SINHALA SANYAKA DAYANNA */
showKeySymbol XKB_KEY_Sinh_ndha = "Sinh_ndha"
-- #define XKB_KEY_Sinh_pa            0x1000db4  /* U+0DB4 SINHALA PAYANNA */
showKeySymbol XKB_KEY_Sinh_pa = "Sinh_pa"
-- #define XKB_KEY_Sinh_pha           0x1000db5  /* U+0DB5 SINHALA MAHA. PAYANNA */
showKeySymbol XKB_KEY_Sinh_pha = "Sinh_pha"
-- #define XKB_KEY_Sinh_ba            0x1000db6  /* U+0DB6 SINHALA BAYANNA */
showKeySymbol XKB_KEY_Sinh_ba = "Sinh_ba"
-- #define XKB_KEY_Sinh_bha           0x1000db7  /* U+0DB7 SINHALA MAHA. BAYANNA */
showKeySymbol XKB_KEY_Sinh_bha = "Sinh_bha"
-- #define XKB_KEY_Sinh_ma            0x1000db8  /* U+0DB8 SINHALA MAYANNA */
showKeySymbol XKB_KEY_Sinh_ma = "Sinh_ma"
-- #define XKB_KEY_Sinh_mba           0x1000db9  /* U+0DB9 SINHALA AMBA BAYANNA */
showKeySymbol XKB_KEY_Sinh_mba = "Sinh_mba"
-- #define XKB_KEY_Sinh_ya            0x1000dba  /* U+0DBA SINHALA YAYANNA */
showKeySymbol XKB_KEY_Sinh_ya = "Sinh_ya"
-- #define XKB_KEY_Sinh_ra            0x1000dbb  /* U+0DBB SINHALA RAYANNA */
showKeySymbol XKB_KEY_Sinh_ra = "Sinh_ra"
-- #define XKB_KEY_Sinh_la            0x1000dbd  /* U+0DBD SINHALA DANTAJA LAYANNA */
showKeySymbol XKB_KEY_Sinh_la = "Sinh_la"
-- #define XKB_KEY_Sinh_va            0x1000dc0  /* U+0DC0 SINHALA VAYANNA */
showKeySymbol XKB_KEY_Sinh_va = "Sinh_va"
-- #define XKB_KEY_Sinh_sha           0x1000dc1  /* U+0DC1 SINHALA TAALUJA SAYANNA */
showKeySymbol XKB_KEY_Sinh_sha = "Sinh_sha"
-- #define XKB_KEY_Sinh_ssha          0x1000dc2  /* U+0DC2 SINHALA MUURDHAJA SAYANNA */
showKeySymbol XKB_KEY_Sinh_ssha = "Sinh_ssha"
-- #define XKB_KEY_Sinh_sa            0x1000dc3  /* U+0DC3 SINHALA DANTAJA SAYANNA */
showKeySymbol XKB_KEY_Sinh_sa = "Sinh_sa"
-- #define XKB_KEY_Sinh_ha            0x1000dc4  /* U+0DC4 SINHALA HAYANNA */
showKeySymbol XKB_KEY_Sinh_ha = "Sinh_ha"
-- #define XKB_KEY_Sinh_lla           0x1000dc5  /* U+0DC5 SINHALA MUURDHAJA LAYANNA */
showKeySymbol XKB_KEY_Sinh_lla = "Sinh_lla"
-- #define XKB_KEY_Sinh_fa            0x1000dc6  /* U+0DC6 SINHALA FAYANNA */
showKeySymbol XKB_KEY_Sinh_fa = "Sinh_fa"
-- #define XKB_KEY_Sinh_al            0x1000dca  /* U+0DCA SINHALA AL-LAKUNA */
showKeySymbol XKB_KEY_Sinh_al = "Sinh_al"
-- #define XKB_KEY_Sinh_aa2           0x1000dcf  /* U+0DCF SINHALA AELA-PILLA */
showKeySymbol XKB_KEY_Sinh_aa2 = "Sinh_aa2"
-- #define XKB_KEY_Sinh_ae2           0x1000dd0  /* U+0DD0 SINHALA AEDA-PILLA */
showKeySymbol XKB_KEY_Sinh_ae2 = "Sinh_ae2"
-- #define XKB_KEY_Sinh_aee2          0x1000dd1  /* U+0DD1 SINHALA DIGA AEDA-PILLA */
showKeySymbol XKB_KEY_Sinh_aee2 = "Sinh_aee2"
-- #define XKB_KEY_Sinh_i2            0x1000dd2  /* U+0DD2 SINHALA IS-PILLA */
showKeySymbol XKB_KEY_Sinh_i2 = "Sinh_i2"
-- #define XKB_KEY_Sinh_ii2           0x1000dd3  /* U+0DD3 SINHALA DIGA IS-PILLA */
showKeySymbol XKB_KEY_Sinh_ii2 = "Sinh_ii2"
-- #define XKB_KEY_Sinh_u2            0x1000dd4  /* U+0DD4 SINHALA PAA-PILLA */
showKeySymbol XKB_KEY_Sinh_u2 = "Sinh_u2"
-- #define XKB_KEY_Sinh_uu2           0x1000dd6  /* U+0DD6 SINHALA DIGA PAA-PILLA */
showKeySymbol XKB_KEY_Sinh_uu2 = "Sinh_uu2"
-- #define XKB_KEY_Sinh_ru2           0x1000dd8  /* U+0DD8 SINHALA GAETTA-PILLA */
showKeySymbol XKB_KEY_Sinh_ru2 = "Sinh_ru2"
-- #define XKB_KEY_Sinh_e2            0x1000dd9  /* U+0DD9 SINHALA KOMBUVA */
showKeySymbol XKB_KEY_Sinh_e2 = "Sinh_e2"
-- #define XKB_KEY_Sinh_ee2           0x1000dda  /* U+0DDA SINHALA DIGA KOMBUVA */
showKeySymbol XKB_KEY_Sinh_ee2 = "Sinh_ee2"
-- #define XKB_KEY_Sinh_ai2           0x1000ddb  /* U+0DDB SINHALA KOMBU DEKA */
showKeySymbol XKB_KEY_Sinh_ai2 = "Sinh_ai2"
-- #define XKB_KEY_Sinh_o2            0x1000ddc  /* U+0DDC SINHALA KOMBUVA HAA AELA-PILLA*/
showKeySymbol XKB_KEY_Sinh_o2 = "Sinh_o2"
-- #define XKB_KEY_Sinh_oo2           0x1000ddd  /* U+0DDD SINHALA KOMBUVA HAA DIGA AELA-PILLA*/
showKeySymbol XKB_KEY_Sinh_oo2 = "Sinh_oo2"
-- #define XKB_KEY_Sinh_au2           0x1000dde  /* U+0DDE SINHALA KOMBUVA HAA GAYANUKITTA */
showKeySymbol XKB_KEY_Sinh_au2 = "Sinh_au2"
-- #define XKB_KEY_Sinh_lu2           0x1000ddf  /* U+0DDF SINHALA GAYANUKITTA */
showKeySymbol XKB_KEY_Sinh_lu2 = "Sinh_lu2"
-- #define XKB_KEY_Sinh_ruu2          0x1000df2  /* U+0DF2 SINHALA DIGA GAETTA-PILLA */
showKeySymbol XKB_KEY_Sinh_ruu2 = "Sinh_ruu2"
-- #define XKB_KEY_Sinh_luu2          0x1000df3  /* U+0DF3 SINHALA DIGA GAYANUKITTA */
showKeySymbol XKB_KEY_Sinh_luu2 = "Sinh_luu2"
-- #define XKB_KEY_Sinh_kunddaliya    0x1000df4  /* U+0DF4 SINHALA KUNDDALIYA */
showKeySymbol XKB_KEY_Sinh_kunddaliya = "Sinh_kunddaliya"
-- #define XKB_KEY_XF86ModeLock        0x1008FF01  /* Mode Switch Lock */
showKeySymbol XKB_KEY_XF86ModeLock = "XF86ModeLock"
-- #define XKB_KEY_XF86MonBrightnessUp   0x1008FF02  /* Monitor/panel brightness */
showKeySymbol XKB_KEY_XF86MonBrightnessUp = "XF86MonBrightnessUp"
-- #define XKB_KEY_XF86MonBrightnessDown 0x1008FF03  /* Monitor/panel brightness */
showKeySymbol XKB_KEY_XF86MonBrightnessDown = "XF86MonBrightnessDown"
-- #define XKB_KEY_XF86KbdLightOnOff     0x1008FF04  /* Keyboards may be lit     */
showKeySymbol XKB_KEY_XF86KbdLightOnOff = "XF86KbdLightOnOff"
-- #define XKB_KEY_XF86KbdBrightnessUp   0x1008FF05  /* Keyboards may be lit     */
showKeySymbol XKB_KEY_XF86KbdBrightnessUp = "XF86KbdBrightnessUp"
-- #define XKB_KEY_XF86KbdBrightnessDown 0x1008FF06  /* Keyboards may be lit     */
showKeySymbol XKB_KEY_XF86KbdBrightnessDown = "XF86KbdBrightnessDown"
-- #define XKB_KEY_XF86Standby     0x1008FF10   /* System into standby mode   */
showKeySymbol XKB_KEY_XF86Standby = "XF86Standby"
-- #define XKB_KEY_XF86AudioLowerVolume    0x1008FF11   /* Volume control down        */
showKeySymbol XKB_KEY_XF86AudioLowerVolume = "XF86AudioLowerVolume"
-- #define XKB_KEY_XF86AudioMute   0x1008FF12   /* Mute sound from the system */
showKeySymbol XKB_KEY_XF86AudioMute = "XF86AudioMute"
-- #define XKB_KEY_XF86AudioRaiseVolume    0x1008FF13   /* Volume control up          */
showKeySymbol XKB_KEY_XF86AudioRaiseVolume = "XF86AudioRaiseVolume"
-- #define XKB_KEY_XF86AudioPlay   0x1008FF14   /* Start playing of audio >   */
showKeySymbol XKB_KEY_XF86AudioPlay = "XF86AudioPlay"
-- #define XKB_KEY_XF86AudioStop   0x1008FF15   /* Stop playing audio         */
showKeySymbol XKB_KEY_XF86AudioStop = "XF86AudioStop"
-- #define XKB_KEY_XF86AudioPrev   0x1008FF16   /* Previous track             */
showKeySymbol XKB_KEY_XF86AudioPrev = "XF86AudioPrev"
-- #define XKB_KEY_XF86AudioNext   0x1008FF17   /* Next track                 */
showKeySymbol XKB_KEY_XF86AudioNext = "XF86AudioNext"
-- #define XKB_KEY_XF86HomePage        0x1008FF18   /* Display user's home page   */
showKeySymbol XKB_KEY_XF86HomePage = "XF86HomePage"
-- #define XKB_KEY_XF86Mail        0x1008FF19   /* Invoke user's mail program */
showKeySymbol XKB_KEY_XF86Mail = "XF86Mail"
-- #define XKB_KEY_XF86Start       0x1008FF1A   /* Start application          */
showKeySymbol XKB_KEY_XF86Start = "XF86Start"
-- #define XKB_KEY_XF86Search      0x1008FF1B   /* Search                     */
showKeySymbol XKB_KEY_XF86Search = "XF86Search"
-- #define XKB_KEY_XF86AudioRecord 0x1008FF1C   /* Record audio application   */
showKeySymbol XKB_KEY_XF86AudioRecord = "XF86AudioRecord"
-- #define XKB_KEY_XF86Calculator  0x1008FF1D   /* Invoke calculator program  */
showKeySymbol XKB_KEY_XF86Calculator = "XF86Calculator"
-- #define XKB_KEY_XF86Memo        0x1008FF1E   /* Invoke Memo taking program */
showKeySymbol XKB_KEY_XF86Memo = "XF86Memo"
-- #define XKB_KEY_XF86ToDoList        0x1008FF1F   /* Invoke To Do List program  */
showKeySymbol XKB_KEY_XF86ToDoList = "XF86ToDoList"
-- #define XKB_KEY_XF86Calendar        0x1008FF20   /* Invoke Calendar program    */
showKeySymbol XKB_KEY_XF86Calendar = "XF86Calendar"
-- #define XKB_KEY_XF86PowerDown   0x1008FF21   /* Deep sleep the system      */
showKeySymbol XKB_KEY_XF86PowerDown = "XF86PowerDown"
-- #define XKB_KEY_XF86ContrastAdjust  0x1008FF22   /* Adjust screen contrast     */
showKeySymbol XKB_KEY_XF86ContrastAdjust = "XF86ContrastAdjust"
-- #define XKB_KEY_XF86RockerUp        0x1008FF23   /* Rocker switches exist up   */
showKeySymbol XKB_KEY_XF86RockerUp = "XF86RockerUp"
-- #define XKB_KEY_XF86RockerDown  0x1008FF24   /* and down                   */
showKeySymbol XKB_KEY_XF86RockerDown = "XF86RockerDown"
-- #define XKB_KEY_XF86RockerEnter 0x1008FF25   /* and let you press them     */
showKeySymbol XKB_KEY_XF86RockerEnter = "XF86RockerEnter"
-- #define XKB_KEY_XF86Back        0x1008FF26   /* Like back on a browser     */
showKeySymbol XKB_KEY_XF86Back = "XF86Back"
-- #define XKB_KEY_XF86Forward     0x1008FF27   /* Like forward on a browser  */
showKeySymbol XKB_KEY_XF86Forward = "XF86Forward"
-- #define XKB_KEY_XF86Stop        0x1008FF28   /* Stop current operation     */
showKeySymbol XKB_KEY_XF86Stop = "XF86Stop"
-- #define XKB_KEY_XF86Refresh     0x1008FF29   /* Refresh the page           */
showKeySymbol XKB_KEY_XF86Refresh = "XF86Refresh"
-- #define XKB_KEY_XF86PowerOff        0x1008FF2A   /* Power off system entirely  */
showKeySymbol XKB_KEY_XF86PowerOff = "XF86PowerOff"
-- #define XKB_KEY_XF86WakeUp      0x1008FF2B   /* Wake up system from sleep  */
showKeySymbol XKB_KEY_XF86WakeUp = "XF86WakeUp"
-- #define XKB_KEY_XF86Eject            0x1008FF2C   /* Eject device (e.g. DVD)    */
showKeySymbol XKB_KEY_XF86Eject = "XF86Eject"
-- #define XKB_KEY_XF86ScreenSaver      0x1008FF2D   /* Invoke screensaver         */
showKeySymbol XKB_KEY_XF86ScreenSaver = "XF86ScreenSaver"
-- #define XKB_KEY_XF86WWW              0x1008FF2E   /* Invoke web browser         */
showKeySymbol XKB_KEY_XF86WWW = "XF86WWW"
-- #define XKB_KEY_XF86Sleep            0x1008FF2F   /* Put system to sleep        */
showKeySymbol XKB_KEY_XF86Sleep = "XF86Sleep"
-- #define XKB_KEY_XF86Favorites   0x1008FF30   /* Show favorite locations    */
showKeySymbol XKB_KEY_XF86Favorites = "XF86Favorites"
-- #define XKB_KEY_XF86AudioPause  0x1008FF31   /* Pause audio playing        */
showKeySymbol XKB_KEY_XF86AudioPause = "XF86AudioPause"
-- #define XKB_KEY_XF86AudioMedia  0x1008FF32   /* Launch media collection app */
showKeySymbol XKB_KEY_XF86AudioMedia = "XF86AudioMedia"
-- #define XKB_KEY_XF86MyComputer  0x1008FF33   /* Display "My Computer" window */
showKeySymbol XKB_KEY_XF86MyComputer = "XF86MyComputer"
-- #define XKB_KEY_XF86VendorHome  0x1008FF34   /* Display vendor home web site */
showKeySymbol XKB_KEY_XF86VendorHome = "XF86VendorHome"
-- #define XKB_KEY_XF86LightBulb   0x1008FF35   /* Light bulb keys exist       */
showKeySymbol XKB_KEY_XF86LightBulb = "XF86LightBulb"
-- #define XKB_KEY_XF86Shop        0x1008FF36   /* Display shopping web site   */
showKeySymbol XKB_KEY_XF86Shop = "XF86Shop"
-- #define XKB_KEY_XF86History     0x1008FF37   /* Show history of web surfing */
showKeySymbol XKB_KEY_XF86History = "XF86History"
-- #define XKB_KEY_XF86OpenURL     0x1008FF38   /* Open selected URL           */
showKeySymbol XKB_KEY_XF86OpenURL = "XF86OpenURL"
-- #define XKB_KEY_XF86AddFavorite 0x1008FF39   /* Add URL to favorites list   */
showKeySymbol XKB_KEY_XF86AddFavorite = "XF86AddFavorite"
-- #define XKB_KEY_XF86HotLinks        0x1008FF3A   /* Show "hot" links            */
showKeySymbol XKB_KEY_XF86HotLinks = "XF86HotLinks"
-- #define XKB_KEY_XF86BrightnessAdjust    0x1008FF3B   /* Invoke brightness adj. UI   */
showKeySymbol XKB_KEY_XF86BrightnessAdjust = "XF86BrightnessAdjust"
-- #define XKB_KEY_XF86Finance     0x1008FF3C   /* Display financial site      */
showKeySymbol XKB_KEY_XF86Finance = "XF86Finance"
-- #define XKB_KEY_XF86Community   0x1008FF3D   /* Display user's community    */
showKeySymbol XKB_KEY_XF86Community = "XF86Community"
-- #define XKB_KEY_XF86AudioRewind 0x1008FF3E   /* "rewind" audio track        */
showKeySymbol XKB_KEY_XF86AudioRewind = "XF86AudioRewind"
-- #define XKB_KEY_XF86BackForward 0x1008FF3F   /* ??? */
showKeySymbol XKB_KEY_XF86BackForward = "XF86BackForward"
-- #define XKB_KEY_XF86Launch0     0x1008FF40   /* Launch Application          */
showKeySymbol XKB_KEY_XF86Launch0 = "XF86Launch0"
-- #define XKB_KEY_XF86Launch1     0x1008FF41   /* Launch Application          */
showKeySymbol XKB_KEY_XF86Launch1 = "XF86Launch1"
-- #define XKB_KEY_XF86Launch2     0x1008FF42   /* Launch Application          */
showKeySymbol XKB_KEY_XF86Launch2 = "XF86Launch2"
-- #define XKB_KEY_XF86Launch3     0x1008FF43   /* Launch Application          */
showKeySymbol XKB_KEY_XF86Launch3 = "XF86Launch3"
-- #define XKB_KEY_XF86Launch4     0x1008FF44   /* Launch Application          */
showKeySymbol XKB_KEY_XF86Launch4 = "XF86Launch4"
-- #define XKB_KEY_XF86Launch5     0x1008FF45   /* Launch Application          */
showKeySymbol XKB_KEY_XF86Launch5 = "XF86Launch5"
-- #define XKB_KEY_XF86Launch6     0x1008FF46   /* Launch Application          */
showKeySymbol XKB_KEY_XF86Launch6 = "XF86Launch6"
-- #define XKB_KEY_XF86Launch7     0x1008FF47   /* Launch Application          */
showKeySymbol XKB_KEY_XF86Launch7 = "XF86Launch7"
-- #define XKB_KEY_XF86Launch8     0x1008FF48   /* Launch Application          */
showKeySymbol XKB_KEY_XF86Launch8 = "XF86Launch8"
-- #define XKB_KEY_XF86Launch9     0x1008FF49   /* Launch Application          */
showKeySymbol XKB_KEY_XF86Launch9 = "XF86Launch9"
-- #define XKB_KEY_XF86LaunchA     0x1008FF4A   /* Launch Application          */
showKeySymbol XKB_KEY_XF86LaunchA = "XF86LaunchA"
-- #define XKB_KEY_XF86LaunchB     0x1008FF4B   /* Launch Application          */
showKeySymbol XKB_KEY_XF86LaunchB = "XF86LaunchB"
-- #define XKB_KEY_XF86LaunchC     0x1008FF4C   /* Launch Application          */
showKeySymbol XKB_KEY_XF86LaunchC = "XF86LaunchC"
-- #define XKB_KEY_XF86LaunchD     0x1008FF4D   /* Launch Application          */
showKeySymbol XKB_KEY_XF86LaunchD = "XF86LaunchD"
-- #define XKB_KEY_XF86LaunchE     0x1008FF4E   /* Launch Application          */
showKeySymbol XKB_KEY_XF86LaunchE = "XF86LaunchE"
-- #define XKB_KEY_XF86LaunchF     0x1008FF4F   /* Launch Application          */
showKeySymbol XKB_KEY_XF86LaunchF = "XF86LaunchF"
-- #define XKB_KEY_XF86ApplicationLeft 0x1008FF50   /* switch to application, left */
showKeySymbol XKB_KEY_XF86ApplicationLeft = "XF86ApplicationLeft"
-- #define XKB_KEY_XF86ApplicationRight    0x1008FF51   /* switch to application, right*/
showKeySymbol XKB_KEY_XF86ApplicationRight = "XF86ApplicationRight"
-- #define XKB_KEY_XF86Book        0x1008FF52   /* Launch bookreader           */
showKeySymbol XKB_KEY_XF86Book = "XF86Book"
-- #define XKB_KEY_XF86CD      0x1008FF53   /* Launch CD/DVD player        */
showKeySymbol XKB_KEY_XF86CD = "XF86CD"
-- #define XKB_KEY_XF86Calculater  0x1008FF54   /* Launch Calculater           */
showKeySymbol XKB_KEY_XF86Calculater = "XF86Calculater"
-- #define XKB_KEY_XF86Clear       0x1008FF55   /* Clear window, screen        */
showKeySymbol XKB_KEY_XF86Clear = "XF86Clear"
-- #define XKB_KEY_XF86Close       0x1008FF56   /* Close window                */
showKeySymbol XKB_KEY_XF86Close = "XF86Close"
-- #define XKB_KEY_XF86Copy        0x1008FF57   /* Copy selection              */
showKeySymbol XKB_KEY_XF86Copy = "XF86Copy"
-- #define XKB_KEY_XF86Cut     0x1008FF58   /* Cut selection               */
showKeySymbol XKB_KEY_XF86Cut = "XF86Cut"
-- #define XKB_KEY_XF86Display     0x1008FF59   /* Output switch key           */
showKeySymbol XKB_KEY_XF86Display = "XF86Display"
-- #define XKB_KEY_XF86DOS     0x1008FF5A   /* Launch DOS (emulation)      */
showKeySymbol XKB_KEY_XF86DOS = "XF86DOS"
-- #define XKB_KEY_XF86Documents   0x1008FF5B   /* Open documents window       */
showKeySymbol XKB_KEY_XF86Documents = "XF86Documents"
-- #define XKB_KEY_XF86Excel       0x1008FF5C   /* Launch spread sheet         */
showKeySymbol XKB_KEY_XF86Excel = "XF86Excel"
-- #define XKB_KEY_XF86Explorer        0x1008FF5D   /* Launch file explorer        */
showKeySymbol XKB_KEY_XF86Explorer = "XF86Explorer"
-- #define XKB_KEY_XF86Game        0x1008FF5E   /* Launch game                 */
showKeySymbol XKB_KEY_XF86Game = "XF86Game"
-- #define XKB_KEY_XF86Go      0x1008FF5F   /* Go to URL                   */
showKeySymbol XKB_KEY_XF86Go = "XF86Go"
-- #define XKB_KEY_XF86iTouch      0x1008FF60   /* Logitch iTouch- don't use   */
showKeySymbol XKB_KEY_XF86iTouch = "XF86iTouch"
-- #define XKB_KEY_XF86LogOff      0x1008FF61   /* Log off system              */
showKeySymbol XKB_KEY_XF86LogOff = "XF86LogOff"
-- #define XKB_KEY_XF86Market      0x1008FF62   /* ??                          */
showKeySymbol XKB_KEY_XF86Market = "XF86Market"
-- #define XKB_KEY_XF86Meeting     0x1008FF63   /* enter meeting in calendar   */
showKeySymbol XKB_KEY_XF86Meeting = "XF86Meeting"
-- #define XKB_KEY_XF86MenuKB      0x1008FF65   /* distingush keyboard from PB */
showKeySymbol XKB_KEY_XF86MenuKB = "XF86MenuKB"
-- #define XKB_KEY_XF86MenuPB      0x1008FF66   /* distinuish PB from keyboard */
showKeySymbol XKB_KEY_XF86MenuPB = "XF86MenuPB"
-- #define XKB_KEY_XF86MySites     0x1008FF67   /* Favourites                  */
showKeySymbol XKB_KEY_XF86MySites = "XF86MySites"
-- #define XKB_KEY_XF86New     0x1008FF68   /* New (folder, document...    */
showKeySymbol XKB_KEY_XF86New = "XF86New"
-- #define XKB_KEY_XF86News        0x1008FF69   /* News                        */
showKeySymbol XKB_KEY_XF86News = "XF86News"
-- #define XKB_KEY_XF86OfficeHome  0x1008FF6A   /* Office home (old Staroffice)*/
showKeySymbol XKB_KEY_XF86OfficeHome = "XF86OfficeHome"
-- #define XKB_KEY_XF86Open        0x1008FF6B   /* Open                        */
showKeySymbol XKB_KEY_XF86Open = "XF86Open"
-- #define XKB_KEY_XF86Option      0x1008FF6C   /* ?? */
showKeySymbol XKB_KEY_XF86Option = "XF86Option"
-- #define XKB_KEY_XF86Paste       0x1008FF6D   /* Paste                       */
showKeySymbol XKB_KEY_XF86Paste = "XF86Paste"
-- #define XKB_KEY_XF86Phone       0x1008FF6E   /* Launch phone; dial number   */
showKeySymbol XKB_KEY_XF86Phone = "XF86Phone"
-- #define XKB_KEY_XF86Q       0x1008FF70   /* Compaq's Q - don't use      */
showKeySymbol XKB_KEY_XF86Q = "XF86Q"
-- #define XKB_KEY_XF86Reply       0x1008FF72   /* Reply e.g., mail            */
showKeySymbol XKB_KEY_XF86Reply = "XF86Reply"
-- #define XKB_KEY_XF86Reload      0x1008FF73   /* Reload web page, file, etc. */
showKeySymbol XKB_KEY_XF86Reload = "XF86Reload"
-- #define XKB_KEY_XF86RotateWindows   0x1008FF74   /* Rotate windows e.g. xrandr  */
showKeySymbol XKB_KEY_XF86RotateWindows = "XF86RotateWindows"
-- #define XKB_KEY_XF86RotationPB  0x1008FF75   /* don't use                   */
showKeySymbol XKB_KEY_XF86RotationPB = "XF86RotationPB"
-- #define XKB_KEY_XF86RotationKB  0x1008FF76   /* don't use                   */
showKeySymbol XKB_KEY_XF86RotationKB = "XF86RotationKB"
-- #define XKB_KEY_XF86Save        0x1008FF77   /* Save (file, document, state */
showKeySymbol XKB_KEY_XF86Save = "XF86Save"
-- #define XKB_KEY_XF86ScrollUp        0x1008FF78   /* Scroll window/contents up   */
showKeySymbol XKB_KEY_XF86ScrollUp = "XF86ScrollUp"
-- #define XKB_KEY_XF86ScrollDown  0x1008FF79   /* Scrool window/contentd down */
showKeySymbol XKB_KEY_XF86ScrollDown = "XF86ScrollDown"
-- #define XKB_KEY_XF86ScrollClick 0x1008FF7A   /* Use XKB mousekeys instead   */
showKeySymbol XKB_KEY_XF86ScrollClick = "XF86ScrollClick"
-- #define XKB_KEY_XF86Send        0x1008FF7B   /* Send mail, file, object     */
showKeySymbol XKB_KEY_XF86Send = "XF86Send"
-- #define XKB_KEY_XF86Spell       0x1008FF7C   /* Spell checker               */
showKeySymbol XKB_KEY_XF86Spell = "XF86Spell"
-- #define XKB_KEY_XF86SplitScreen 0x1008FF7D   /* Split window or screen      */
showKeySymbol XKB_KEY_XF86SplitScreen = "XF86SplitScreen"
-- #define XKB_KEY_XF86Support     0x1008FF7E   /* Get support (??)            */
showKeySymbol XKB_KEY_XF86Support = "XF86Support"
-- #define XKB_KEY_XF86TaskPane        0x1008FF7F   /* Show tasks */
showKeySymbol XKB_KEY_XF86TaskPane = "XF86TaskPane"
-- #define XKB_KEY_XF86Terminal        0x1008FF80   /* Launch terminal emulator    */
showKeySymbol XKB_KEY_XF86Terminal = "XF86Terminal"
-- #define XKB_KEY_XF86Tools       0x1008FF81   /* toolbox of desktop/app.     */
showKeySymbol XKB_KEY_XF86Tools = "XF86Tools"
-- #define XKB_KEY_XF86Travel      0x1008FF82   /* ?? */
showKeySymbol XKB_KEY_XF86Travel = "XF86Travel"
-- #define XKB_KEY_XF86UserPB      0x1008FF84   /* ?? */
showKeySymbol XKB_KEY_XF86UserPB = "XF86UserPB"
-- #define XKB_KEY_XF86User1KB     0x1008FF85   /* ?? */
showKeySymbol XKB_KEY_XF86User1KB = "XF86User1KB"
-- #define XKB_KEY_XF86User2KB     0x1008FF86   /* ?? */
showKeySymbol XKB_KEY_XF86User2KB = "XF86User2KB"
-- #define XKB_KEY_XF86Video       0x1008FF87   /* Launch video player       */
showKeySymbol XKB_KEY_XF86Video = "XF86Video"
-- #define XKB_KEY_XF86WheelButton 0x1008FF88   /* button from a mouse wheel */
showKeySymbol XKB_KEY_XF86WheelButton = "XF86WheelButton"
-- #define XKB_KEY_XF86Word        0x1008FF89   /* Launch word processor     */
showKeySymbol XKB_KEY_XF86Word = "XF86Word"
-- #define XKB_KEY_XF86Xfer        0x1008FF8A
showKeySymbol XKB_KEY_XF86Xfer = "XF86Xfer"
-- #define XKB_KEY_XF86ZoomIn      0x1008FF8B   /* zoom in view, map, etc.   */
showKeySymbol XKB_KEY_XF86ZoomIn = "XF86ZoomIn"
-- #define XKB_KEY_XF86ZoomOut     0x1008FF8C   /* zoom out view, map, etc.  */
showKeySymbol XKB_KEY_XF86ZoomOut = "XF86ZoomOut"
-- #define XKB_KEY_XF86Away        0x1008FF8D   /* mark yourself as away     */
showKeySymbol XKB_KEY_XF86Away = "XF86Away"
-- #define XKB_KEY_XF86Messenger   0x1008FF8E   /* as in instant messaging   */
showKeySymbol XKB_KEY_XF86Messenger = "XF86Messenger"
-- #define XKB_KEY_XF86WebCam      0x1008FF8F   /* Launch web camera app.    */
showKeySymbol XKB_KEY_XF86WebCam = "XF86WebCam"
-- #define XKB_KEY_XF86MailForward 0x1008FF90   /* Forward in mail           */
showKeySymbol XKB_KEY_XF86MailForward = "XF86MailForward"
-- #define XKB_KEY_XF86Pictures        0x1008FF91   /* Show pictures             */
showKeySymbol XKB_KEY_XF86Pictures = "XF86Pictures"
-- #define XKB_KEY_XF86Music       0x1008FF92   /* Launch music application  */
showKeySymbol XKB_KEY_XF86Music = "XF86Music"
-- #define XKB_KEY_XF86Battery     0x1008FF93   /* Display battery information */
showKeySymbol XKB_KEY_XF86Battery = "XF86Battery"
-- #define XKB_KEY_XF86Bluetooth   0x1008FF94   /* Enable/disable Bluetooth    */
showKeySymbol XKB_KEY_XF86Bluetooth = "XF86Bluetooth"
-- #define XKB_KEY_XF86WLAN        0x1008FF95   /* Enable/disable WLAN         */
showKeySymbol XKB_KEY_XF86WLAN = "XF86WLAN"
-- #define XKB_KEY_XF86UWB     0x1008FF96   /* Enable/disable UWB      */
showKeySymbol XKB_KEY_XF86UWB = "XF86UWB"
-- #define XKB_KEY_XF86AudioForward    0x1008FF97   /* fast-forward audio track    */
showKeySymbol XKB_KEY_XF86AudioForward = "XF86AudioForward"
-- #define XKB_KEY_XF86AudioRepeat 0x1008FF98   /* toggle repeat mode          */
showKeySymbol XKB_KEY_XF86AudioRepeat = "XF86AudioRepeat"
-- #define XKB_KEY_XF86AudioRandomPlay 0x1008FF99   /* toggle shuffle mode         */
showKeySymbol XKB_KEY_XF86AudioRandomPlay = "XF86AudioRandomPlay"
-- #define XKB_KEY_XF86Subtitle        0x1008FF9A   /* cycle through subtitle      */
showKeySymbol XKB_KEY_XF86Subtitle = "XF86Subtitle"
-- #define XKB_KEY_XF86AudioCycleTrack 0x1008FF9B   /* cycle through audio tracks  */
showKeySymbol XKB_KEY_XF86AudioCycleTrack = "XF86AudioCycleTrack"
-- #define XKB_KEY_XF86CycleAngle  0x1008FF9C   /* cycle through angles        */
showKeySymbol XKB_KEY_XF86CycleAngle = "XF86CycleAngle"
-- #define XKB_KEY_XF86FrameBack   0x1008FF9D   /* video: go one frame back    */
showKeySymbol XKB_KEY_XF86FrameBack = "XF86FrameBack"
-- #define XKB_KEY_XF86FrameForward    0x1008FF9E   /* video: go one frame forward */
showKeySymbol XKB_KEY_XF86FrameForward = "XF86FrameForward"
-- #define XKB_KEY_XF86Time        0x1008FF9F   /* display, or shows an entry for time seeking */
showKeySymbol XKB_KEY_XF86Time = "XF86Time"
-- #define XKB_KEY_XF86Select      0x1008FFA0   /* Select button on joypads and remotes */
showKeySymbol XKB_KEY_XF86Select = "XF86Select"
-- #define XKB_KEY_XF86View        0x1008FFA1   /* Show a view options/properties */
showKeySymbol XKB_KEY_XF86View = "XF86View"
-- #define XKB_KEY_XF86TopMenu     0x1008FFA2   /* Go to a top-level menu in a video */
showKeySymbol XKB_KEY_XF86TopMenu = "XF86TopMenu"
-- #define XKB_KEY_XF86Red     0x1008FFA3   /* Red button                  */
showKeySymbol XKB_KEY_XF86Red = "XF86Red"
-- #define XKB_KEY_XF86Green       0x1008FFA4   /* Green button                */
showKeySymbol XKB_KEY_XF86Green = "XF86Green"
-- #define XKB_KEY_XF86Yellow      0x1008FFA5   /* Yellow button               */
showKeySymbol XKB_KEY_XF86Yellow = "XF86Yellow"
-- #define XKB_KEY_XF86Blue             0x1008FFA6   /* Blue button                 */
showKeySymbol XKB_KEY_XF86Blue = "XF86Blue"
-- #define XKB_KEY_XF86Suspend     0x1008FFA7   /* Sleep to RAM                */
showKeySymbol XKB_KEY_XF86Suspend = "XF86Suspend"
-- #define XKB_KEY_XF86Hibernate   0x1008FFA8   /* Sleep to disk               */
showKeySymbol XKB_KEY_XF86Hibernate = "XF86Hibernate"
-- #define XKB_KEY_XF86TouchpadToggle  0x1008FFA9   /* Toggle between touchpad/trackstick */
showKeySymbol XKB_KEY_XF86TouchpadToggle = "XF86TouchpadToggle"
-- #define XKB_KEY_XF86TouchpadOn  0x1008FFB0   /* The touchpad got switched on */
showKeySymbol XKB_KEY_XF86TouchpadOn = "XF86TouchpadOn"
-- #define XKB_KEY_XF86TouchpadOff 0x1008FFB1   /* The touchpad got switched off */
showKeySymbol XKB_KEY_XF86TouchpadOff = "XF86TouchpadOff"
-- #define XKB_KEY_XF86AudioMicMute    0x1008FFB2   /* Mute the Mic from the system */
showKeySymbol XKB_KEY_XF86AudioMicMute = "XF86AudioMicMute"
-- #define XKB_KEY_XF86Switch_VT_1 0x1008FE01
showKeySymbol XKB_KEY_XF86Switch_VT_1 = "XF86Switch_VT_1"
-- #define XKB_KEY_XF86Switch_VT_2 0x1008FE02
showKeySymbol XKB_KEY_XF86Switch_VT_2 = "XF86Switch_VT_2"
-- #define XKB_KEY_XF86Switch_VT_3 0x1008FE03
showKeySymbol XKB_KEY_XF86Switch_VT_3 = "XF86Switch_VT_3"
-- #define XKB_KEY_XF86Switch_VT_4 0x1008FE04
showKeySymbol XKB_KEY_XF86Switch_VT_4 = "XF86Switch_VT_4"
-- #define XKB_KEY_XF86Switch_VT_5 0x1008FE05
showKeySymbol XKB_KEY_XF86Switch_VT_5 = "XF86Switch_VT_5"
-- #define XKB_KEY_XF86Switch_VT_6 0x1008FE06
showKeySymbol XKB_KEY_XF86Switch_VT_6 = "XF86Switch_VT_6"
-- #define XKB_KEY_XF86Switch_VT_7 0x1008FE07
showKeySymbol XKB_KEY_XF86Switch_VT_7 = "XF86Switch_VT_7"
-- #define XKB_KEY_XF86Switch_VT_8 0x1008FE08
showKeySymbol XKB_KEY_XF86Switch_VT_8 = "XF86Switch_VT_8"
-- #define XKB_KEY_XF86Switch_VT_9 0x1008FE09
showKeySymbol XKB_KEY_XF86Switch_VT_9 = "XF86Switch_VT_9"
-- #define XKB_KEY_XF86Switch_VT_10    0x1008FE0A
showKeySymbol XKB_KEY_XF86Switch_VT_10 = "XF86Switch_VT_10"
-- #define XKB_KEY_XF86Switch_VT_11    0x1008FE0B
showKeySymbol XKB_KEY_XF86Switch_VT_11 = "XF86Switch_VT_11"
-- #define XKB_KEY_XF86Switch_VT_12    0x1008FE0C
showKeySymbol XKB_KEY_XF86Switch_VT_12 = "XF86Switch_VT_12"
-- #define XKB_KEY_XF86Ungrab      0x1008FE20   /* force ungrab               */
showKeySymbol XKB_KEY_XF86Ungrab = "XF86Ungrab"
-- #define XKB_KEY_XF86ClearGrab   0x1008FE21   /* kill application with grab */
showKeySymbol XKB_KEY_XF86ClearGrab = "XF86ClearGrab"
-- #define XKB_KEY_XF86Next_VMode  0x1008FE22   /* next video mode available  */
showKeySymbol XKB_KEY_XF86Next_VMode = "XF86Next_VMode"
-- #define XKB_KEY_XF86Prev_VMode  0x1008FE23   /* prev. video mode available */
showKeySymbol XKB_KEY_XF86Prev_VMode = "XF86Prev_VMode"
-- #define XKB_KEY_XF86LogWindowTree   0x1008FE24   /* print window tree to log   */
showKeySymbol XKB_KEY_XF86LogWindowTree = "XF86LogWindowTree"
-- #define XKB_KEY_XF86LogGrabInfo 0x1008FE25   /* print all active grabs to log */
showKeySymbol XKB_KEY_XF86LogGrabInfo = "XF86LogGrabInfo"
-- #define XKB_KEY_SunFA_Grave     0x1005FF00
showKeySymbol XKB_KEY_SunFA_Grave = "SunFA_Grave"
-- #define XKB_KEY_SunFA_Circum        0x1005FF01
showKeySymbol XKB_KEY_SunFA_Circum = "SunFA_Circum"
-- #define XKB_KEY_SunFA_Tilde     0x1005FF02
showKeySymbol XKB_KEY_SunFA_Tilde = "SunFA_Tilde"
-- #define XKB_KEY_SunFA_Acute     0x1005FF03
showKeySymbol XKB_KEY_SunFA_Acute = "SunFA_Acute"
-- #define XKB_KEY_SunFA_Diaeresis 0x1005FF04
showKeySymbol XKB_KEY_SunFA_Diaeresis = "SunFA_Diaeresis"
-- #define XKB_KEY_SunFA_Cedilla   0x1005FF05
showKeySymbol XKB_KEY_SunFA_Cedilla = "SunFA_Cedilla"
-- #define XKB_KEY_SunF36      0x1005FF10  /* Labeled F11 */
showKeySymbol XKB_KEY_SunF36 = "SunF36"
-- #define XKB_KEY_SunF37      0x1005FF11  /* Labeled F12 */
showKeySymbol XKB_KEY_SunF37 = "SunF37"
-- #define XKB_KEY_SunSys_Req      0x1005FF60
showKeySymbol XKB_KEY_SunSys_Req = "SunSys_Req"
-- #define XKB_KEY_SunPrint_Screen 0x0000FF61  /* Same as XK_Print */
showKeySymbol XKB_KEY_SunPrint_Screen = "SunPrint_Screen"
-- #define XKB_KEY_SunCompose      0x0000FF20  /* Same as XK_Multi_key */
showKeySymbol XKB_KEY_SunCompose = "SunCompose"
-- #define XKB_KEY_SunAltGraph     0x0000FF7E  /* Same as XK_Mode_switch */
showKeySymbol XKB_KEY_SunAltGraph = "SunAltGraph"
-- #define XKB_KEY_SunPageUp       0x0000FF55  /* Same as XK_Prior */
showKeySymbol XKB_KEY_SunPageUp = "SunPageUp"
-- #define XKB_KEY_SunPageDown     0x0000FF56  /* Same as XK_Next */
showKeySymbol XKB_KEY_SunPageDown = "SunPageDown"
-- #define XKB_KEY_SunUndo     0x0000FF65  /* Same as XK_Undo */
showKeySymbol XKB_KEY_SunUndo = "SunUndo"
-- #define XKB_KEY_SunAgain        0x0000FF66  /* Same as XK_Redo */
showKeySymbol XKB_KEY_SunAgain = "SunAgain"
-- #define XKB_KEY_SunFind     0x0000FF68  /* Same as XK_Find */
showKeySymbol XKB_KEY_SunFind = "SunFind"
-- #define XKB_KEY_SunStop     0x0000FF69  /* Same as XK_Cancel */
showKeySymbol XKB_KEY_SunStop = "SunStop"
-- #define XKB_KEY_SunProps        0x1005FF70
showKeySymbol XKB_KEY_SunProps = "SunProps"
-- #define XKB_KEY_SunFront        0x1005FF71
showKeySymbol XKB_KEY_SunFront = "SunFront"
-- #define XKB_KEY_SunCopy     0x1005FF72
showKeySymbol XKB_KEY_SunCopy = "SunCopy"
-- #define XKB_KEY_SunOpen     0x1005FF73
showKeySymbol XKB_KEY_SunOpen = "SunOpen"
-- #define XKB_KEY_SunPaste        0x1005FF74
showKeySymbol XKB_KEY_SunPaste = "SunPaste"
-- #define XKB_KEY_SunCut      0x1005FF75
showKeySymbol XKB_KEY_SunCut = "SunCut"
-- #define XKB_KEY_SunPowerSwitch      0x1005FF76
showKeySymbol XKB_KEY_SunPowerSwitch = "SunPowerSwitch"
-- #define XKB_KEY_SunAudioLowerVolume     0x1005FF77
showKeySymbol XKB_KEY_SunAudioLowerVolume = "SunAudioLowerVolume"
-- #define XKB_KEY_SunAudioMute            0x1005FF78
showKeySymbol XKB_KEY_SunAudioMute = "SunAudioMute"
-- #define XKB_KEY_SunAudioRaiseVolume     0x1005FF79
showKeySymbol XKB_KEY_SunAudioRaiseVolume = "SunAudioRaiseVolume"
-- #define XKB_KEY_SunVideoDegauss     0x1005FF7A
showKeySymbol XKB_KEY_SunVideoDegauss = "SunVideoDegauss"
-- #define XKB_KEY_SunVideoLowerBrightness 0x1005FF7B
showKeySymbol XKB_KEY_SunVideoLowerBrightness = "SunVideoLowerBrightness"
-- #define XKB_KEY_SunVideoRaiseBrightness 0x1005FF7C
showKeySymbol XKB_KEY_SunVideoRaiseBrightness = "SunVideoRaiseBrightness"
-- #define XKB_KEY_SunPowerSwitchShift     0x1005FF7D
showKeySymbol XKB_KEY_SunPowerSwitchShift = "SunPowerSwitchShift"
-- #define XKB_KEY_Dring_accent         0x1000FEB0
showKeySymbol XKB_KEY_Dring_accent = "Dring_accent"
-- #define XKB_KEY_Dcircumflex_accent   0x1000FE5E
showKeySymbol XKB_KEY_Dcircumflex_accent = "Dcircumflex_accent"
-- #define XKB_KEY_Dcedilla_accent      0x1000FE2C
showKeySymbol XKB_KEY_Dcedilla_accent = "Dcedilla_accent"
-- #define XKB_KEY_Dacute_accent        0x1000FE27
showKeySymbol XKB_KEY_Dacute_accent = "Dacute_accent"
-- #define XKB_KEY_Dgrave_accent        0x1000FE60
showKeySymbol XKB_KEY_Dgrave_accent = "Dgrave_accent"
-- #define XKB_KEY_Dtilde               0x1000FE7E
showKeySymbol XKB_KEY_Dtilde = "Dtilde"
-- #define XKB_KEY_Ddiaeresis           0x1000FE22
showKeySymbol XKB_KEY_Ddiaeresis = "Ddiaeresis"
-- #define XKB_KEY_DRemove 0x1000FF00   /* Remove */
showKeySymbol XKB_KEY_DRemove = "DRemove"
-- #define XKB_KEY_hpClearLine     0x1000FF6F
showKeySymbol XKB_KEY_hpClearLine = "hpClearLine"
-- #define XKB_KEY_hpInsertLine        0x1000FF70
showKeySymbol XKB_KEY_hpInsertLine = "hpInsertLine"
-- #define XKB_KEY_hpDeleteLine        0x1000FF71
showKeySymbol XKB_KEY_hpDeleteLine = "hpDeleteLine"
-- #define XKB_KEY_hpInsertChar        0x1000FF72
showKeySymbol XKB_KEY_hpInsertChar = "hpInsertChar"
-- #define XKB_KEY_hpDeleteChar        0x1000FF73
showKeySymbol XKB_KEY_hpDeleteChar = "hpDeleteChar"
-- #define XKB_KEY_hpBackTab       0x1000FF74
showKeySymbol XKB_KEY_hpBackTab = "hpBackTab"
-- #define XKB_KEY_hpKP_BackTab        0x1000FF75
showKeySymbol XKB_KEY_hpKP_BackTab = "hpKP_BackTab"
-- #define XKB_KEY_hpModelock1     0x1000FF48
showKeySymbol XKB_KEY_hpModelock1 = "hpModelock1"
-- #define XKB_KEY_hpModelock2     0x1000FF49
showKeySymbol XKB_KEY_hpModelock2 = "hpModelock2"
-- #define XKB_KEY_hpReset     0x1000FF6C
showKeySymbol XKB_KEY_hpReset = "hpReset"
-- #define XKB_KEY_hpSystem        0x1000FF6D
showKeySymbol XKB_KEY_hpSystem = "hpSystem"
-- #define XKB_KEY_hpUser      0x1000FF6E
showKeySymbol XKB_KEY_hpUser = "hpUser"
-- #define XKB_KEY_hpmute_acute        0x100000A8
showKeySymbol XKB_KEY_hpmute_acute = "hpmute_acute"
-- #define XKB_KEY_hpmute_grave        0x100000A9
showKeySymbol XKB_KEY_hpmute_grave = "hpmute_grave"
-- #define XKB_KEY_hpmute_asciicircum  0x100000AA
showKeySymbol XKB_KEY_hpmute_asciicircum = "hpmute_asciicircum"
-- #define XKB_KEY_hpmute_diaeresis    0x100000AB
showKeySymbol XKB_KEY_hpmute_diaeresis = "hpmute_diaeresis"
-- #define XKB_KEY_hpmute_asciitilde   0x100000AC
showKeySymbol XKB_KEY_hpmute_asciitilde = "hpmute_asciitilde"
-- #define XKB_KEY_hplira      0x100000AF
showKeySymbol XKB_KEY_hplira = "hplira"
-- #define XKB_KEY_hpguilder       0x100000BE
showKeySymbol XKB_KEY_hpguilder = "hpguilder"
-- #define XKB_KEY_hpYdiaeresis        0x100000EE
showKeySymbol XKB_KEY_hpYdiaeresis = "hpYdiaeresis"
-- #define XKB_KEY_hpIO            0x100000EE
showKeySymbol XKB_KEY_hpIO = "hpIO"
-- #define XKB_KEY_hplongminus     0x100000F6
showKeySymbol XKB_KEY_hplongminus = "hplongminus"
-- #define XKB_KEY_hpblock     0x100000FC
showKeySymbol XKB_KEY_hpblock = "hpblock"
-- #define XKB_KEY_osfCopy     0x1004FF02
showKeySymbol XKB_KEY_osfCopy = "osfCopy"
-- #define XKB_KEY_osfCut      0x1004FF03
showKeySymbol XKB_KEY_osfCut = "osfCut"
-- #define XKB_KEY_osfPaste        0x1004FF04
showKeySymbol XKB_KEY_osfPaste = "osfPaste"
-- #define XKB_KEY_osfBackTab      0x1004FF07
showKeySymbol XKB_KEY_osfBackTab = "osfBackTab"
-- #define XKB_KEY_osfBackSpace        0x1004FF08
showKeySymbol XKB_KEY_osfBackSpace = "osfBackSpace"
-- #define XKB_KEY_osfClear        0x1004FF0B
showKeySymbol XKB_KEY_osfClear = "osfClear"
-- #define XKB_KEY_osfEscape       0x1004FF1B
showKeySymbol XKB_KEY_osfEscape = "osfEscape"
-- #define XKB_KEY_osfAddMode      0x1004FF31
showKeySymbol XKB_KEY_osfAddMode = "osfAddMode"
-- #define XKB_KEY_osfPrimaryPaste 0x1004FF32
showKeySymbol XKB_KEY_osfPrimaryPaste = "osfPrimaryPaste"
-- #define XKB_KEY_osfQuickPaste   0x1004FF33
showKeySymbol XKB_KEY_osfQuickPaste = "osfQuickPaste"
-- #define XKB_KEY_osfPageLeft     0x1004FF40
showKeySymbol XKB_KEY_osfPageLeft = "osfPageLeft"
-- #define XKB_KEY_osfPageUp       0x1004FF41
showKeySymbol XKB_KEY_osfPageUp = "osfPageUp"
-- #define XKB_KEY_osfPageDown     0x1004FF42
showKeySymbol XKB_KEY_osfPageDown = "osfPageDown"
-- #define XKB_KEY_osfPageRight        0x1004FF43
showKeySymbol XKB_KEY_osfPageRight = "osfPageRight"
-- #define XKB_KEY_osfActivate     0x1004FF44
showKeySymbol XKB_KEY_osfActivate = "osfActivate"
-- #define XKB_KEY_osfMenuBar      0x1004FF45
showKeySymbol XKB_KEY_osfMenuBar = "osfMenuBar"
-- #define XKB_KEY_osfLeft     0x1004FF51
showKeySymbol XKB_KEY_osfLeft = "osfLeft"
-- #define XKB_KEY_osfUp       0x1004FF52
showKeySymbol XKB_KEY_osfUp = "osfUp"
-- #define XKB_KEY_osfRight        0x1004FF53
showKeySymbol XKB_KEY_osfRight = "osfRight"
-- #define XKB_KEY_osfDown     0x1004FF54
showKeySymbol XKB_KEY_osfDown = "osfDown"
-- #define XKB_KEY_osfEndLine      0x1004FF57
showKeySymbol XKB_KEY_osfEndLine = "osfEndLine"
-- #define XKB_KEY_osfBeginLine        0x1004FF58
showKeySymbol XKB_KEY_osfBeginLine = "osfBeginLine"
-- #define XKB_KEY_osfEndData      0x1004FF59
showKeySymbol XKB_KEY_osfEndData = "osfEndData"
-- #define XKB_KEY_osfBeginData        0x1004FF5A
showKeySymbol XKB_KEY_osfBeginData = "osfBeginData"
-- #define XKB_KEY_osfPrevMenu     0x1004FF5B
showKeySymbol XKB_KEY_osfPrevMenu = "osfPrevMenu"
-- #define XKB_KEY_osfNextMenu     0x1004FF5C
showKeySymbol XKB_KEY_osfNextMenu = "osfNextMenu"
-- #define XKB_KEY_osfPrevField        0x1004FF5D
showKeySymbol XKB_KEY_osfPrevField = "osfPrevField"
-- #define XKB_KEY_osfNextField        0x1004FF5E
showKeySymbol XKB_KEY_osfNextField = "osfNextField"
-- #define XKB_KEY_osfSelect       0x1004FF60
showKeySymbol XKB_KEY_osfSelect = "osfSelect"
-- #define XKB_KEY_osfInsert       0x1004FF63
showKeySymbol XKB_KEY_osfInsert = "osfInsert"
-- #define XKB_KEY_osfUndo     0x1004FF65
showKeySymbol XKB_KEY_osfUndo = "osfUndo"
-- #define XKB_KEY_osfMenu     0x1004FF67
showKeySymbol XKB_KEY_osfMenu = "osfMenu"
-- #define XKB_KEY_osfCancel       0x1004FF69
showKeySymbol XKB_KEY_osfCancel = "osfCancel"
-- #define XKB_KEY_osfHelp     0x1004FF6A
showKeySymbol XKB_KEY_osfHelp = "osfHelp"
-- #define XKB_KEY_osfSelectAll        0x1004FF71
showKeySymbol XKB_KEY_osfSelectAll = "osfSelectAll"
-- #define XKB_KEY_osfDeselectAll  0x1004FF72
showKeySymbol XKB_KEY_osfDeselectAll = "osfDeselectAll"
-- #define XKB_KEY_osfReselect     0x1004FF73
showKeySymbol XKB_KEY_osfReselect = "osfReselect"
-- #define XKB_KEY_osfExtend       0x1004FF74
showKeySymbol XKB_KEY_osfExtend = "osfExtend"
-- #define XKB_KEY_osfRestore      0x1004FF78
showKeySymbol XKB_KEY_osfRestore = "osfRestore"
-- #define XKB_KEY_osfDelete       0x1004FFFF
showKeySymbol XKB_KEY_osfDelete = "osfDelete"
-- #define XKB_KEY_Reset                0x1000FF6C
showKeySymbol XKB_KEY_Reset = "Reset"
-- #define XKB_KEY_System               0x1000FF6D
showKeySymbol XKB_KEY_System = "System"
-- #define XKB_KEY_User                 0x1000FF6E
showKeySymbol XKB_KEY_User = "User"
-- #define XKB_KEY_ClearLine            0x1000FF6F
showKeySymbol XKB_KEY_ClearLine = "ClearLine"
-- #define XKB_KEY_InsertLine           0x1000FF70
showKeySymbol XKB_KEY_InsertLine = "InsertLine"
-- #define XKB_KEY_DeleteLine           0x1000FF71
showKeySymbol XKB_KEY_DeleteLine = "DeleteLine"
-- #define XKB_KEY_InsertChar           0x1000FF72
showKeySymbol XKB_KEY_InsertChar = "InsertChar"
-- #define XKB_KEY_DeleteChar           0x1000FF73
showKeySymbol XKB_KEY_DeleteChar = "DeleteChar"
-- #define XKB_KEY_BackTab              0x1000FF74
showKeySymbol XKB_KEY_BackTab = "BackTab"
-- #define XKB_KEY_KP_BackTab           0x1000FF75
showKeySymbol XKB_KEY_KP_BackTab = "KP_BackTab"
-- #define XKB_KEY_Ext16bit_L           0x1000FF76
showKeySymbol XKB_KEY_Ext16bit_L = "Ext16bit_L"
-- #define XKB_KEY_Ext16bit_R           0x1000FF77
showKeySymbol XKB_KEY_Ext16bit_R = "Ext16bit_R"
-- #define XKB_KEY_mute_acute           0x100000a8
showKeySymbol XKB_KEY_mute_acute = "mute_acute"
-- #define XKB_KEY_mute_grave           0x100000a9
showKeySymbol XKB_KEY_mute_grave = "mute_grave"
-- #define XKB_KEY_mute_asciicircum     0x100000aa
showKeySymbol XKB_KEY_mute_asciicircum = "mute_asciicircum"
-- #define XKB_KEY_mute_diaeresis       0x100000ab
showKeySymbol XKB_KEY_mute_diaeresis = "mute_diaeresis"
-- #define XKB_KEY_mute_asciitilde      0x100000ac
showKeySymbol XKB_KEY_mute_asciitilde = "mute_asciitilde"
-- #define XKB_KEY_lira                 0x100000af
showKeySymbol XKB_KEY_lira = "lira"
-- #define XKB_KEY_guilder              0x100000be
showKeySymbol XKB_KEY_guilder = "guilder"
-- #define XKB_KEY_IO                   0x100000ee
showKeySymbol XKB_KEY_IO = "IO"
-- #define XKB_KEY_longminus            0x100000f6
showKeySymbol XKB_KEY_longminus = "longminus"
-- #define XKB_KEY_block                0x100000fc
showKeySymbol XKB_KEY_block = "block"
showKeySymbol _ = "Unknown symbol"
