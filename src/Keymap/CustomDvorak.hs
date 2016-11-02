{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Keymap.CustomDvorak
  ( customDvorakKeymap
  , customDvorak
  , customDvorakSticky
  ) where

import           Data.Default
import qualified Data.Vector  as V
import           Protolude    hiding (State, group)

import KeySymbolDefinitions
import Modifiers
import OnKeyEvent
import State

customDvorak :: State
customDvorak =
  def
  { sKeymap = keyCodeAndGroupsToKeymap customDvorakKeymap
  , sIdentifyModifiers = whatToDoWithKeySymbol
  , sCalculateLevel = shiftIsLevelTwoCalculateLevel
  , sConsumeModifiersUsedForCalculatingLevel = shiftIsLevelTwoConsumeModifiers
  , sName = "customDvorak"
  }

customDvorakSticky :: State
customDvorakSticky =
  def
  { sKeymap = keyCodeAndGroupsToKeymap customDvorakKeymap
  , sIdentifyModifiers = stickyWhatToDoWithKeySymbol
  , sCalculateLevel = shiftIsLevelTwoCalculateLevel
  , sConsumeModifiersUsedForCalculatingLevel = shiftIsLevelTwoConsumeModifiers
  , sName = "customDvorakSticky"
  }

-- instead of the below use evtest to get the actual keycodes
-- http://unix.stackexchange.com/a/130762
-- use the below commands:
-- xkbcomp :0 /tmp/server-0.xkb
--   get the keycodes to a file /tmp/keycodes.txt
--   and the list of keysyms to a file /tmp/keysyms.txt
--   from /tmp/keysyms.txt, join all lines between { } using the command
--   awk '/\{[^}]*$/ { line = $0; while (getline && !/}/) line = line $0; $0 = line "}" } { print }' /tmp/keysyms.txt >| /tmp/keysyms-modified.txt
--   for f in $(cat /tmp/keycodes.txt | sed --expression="s/\(<.*>\) = .*/\1/"); do grep "${f}" /tmp/keysyms-modified.txt | tr -d '\n' | sed --expression="s/.*= \(.*\);.*\(\[.*\]\).*/,(\1, V.fromList \2)/" ; echo "\r"; done >| /tmp/parsed-keymap.txt
customDvorakKeymap :: [(KeyCode, Group)]
customDvorakKeymap =
  [ (9, group [XKB_KEY_Escape])
  , (10, group [XKB_KEY_1, XKB_KEY_exclam])
  , (11, group [XKB_KEY_2, XKB_KEY_at])
  , (12, group [XKB_KEY_3, XKB_KEY_numbersign])
  , (13, group [XKB_KEY_4, XKB_KEY_dollar])
  , (14, group [XKB_KEY_5, XKB_KEY_percent])
  , ( 15
    , group
        [ XKB_KEY_6
        , XKB_KEY_asciicircum
        , XKB_KEY_dead_circumflex
        , XKB_KEY_dead_circumflex
        ])
  , (16, group [XKB_KEY_7, XKB_KEY_ampersand])
  , (17, group [XKB_KEY_8, XKB_KEY_asterisk])
  , ( 18
    , group [XKB_KEY_9, XKB_KEY_parenleft, XKB_KEY_dead_grave, XKB_KEY_NoSymbol])
  , (19, group [XKB_KEY_0, XKB_KEY_parenright])
  , (20, group [XKB_KEY_bracketleft, XKB_KEY_braceleft])
  , ( 21
    , group
        [ XKB_KEY_bracketright
        , XKB_KEY_braceright
        , XKB_KEY_dead_tilde
        , XKB_KEY_NoSymbol
        ])
  , ( 22
    , group
        [ XKB_KEY_BackSpace
        , XKB_KEY_BackSpace
        , XKB_KEY_NoSymbol
        , XKB_KEY_NoSymbol
        , XKB_KEY_Terminate_Server
        ])
  , (23, group [XKB_KEY_Tab, XKB_KEY_ISO_Left_Tab])
  , ( 24
    , group
        [ XKB_KEY_apostrophe
        , XKB_KEY_quotedbl
        , XKB_KEY_dead_acute
        , XKB_KEY_dead_diaeresis
        ])
  , ( 25
    , group
        [XKB_KEY_comma, XKB_KEY_less, XKB_KEY_dead_cedilla, XKB_KEY_dead_caron])
  , ( 26
    , group
        [ XKB_KEY_period
        , XKB_KEY_greater
        , XKB_KEY_dead_abovedot
        , XKB_KEY_periodcentered
        ])
  , (27, group [XKB_KEY_p, XKB_KEY_P])
  , (28, group [XKB_KEY_y, XKB_KEY_Y])
  , (29, group [XKB_KEY_f, XKB_KEY_F])
  , (30, group [XKB_KEY_g, XKB_KEY_G])
  , (31, group [XKB_KEY_c, XKB_KEY_C])
  , (32, group [XKB_KEY_r, XKB_KEY_R])
  , (33, group [XKB_KEY_l, XKB_KEY_L])
  , (34, group [XKB_KEY_slash, XKB_KEY_question])
  , (35, group [XKB_KEY_equal, XKB_KEY_plus])
  , (36, group [XKB_KEY_Return])
  , (37, group [XKB_KEY_Caps_Lock])
  , (38, group [XKB_KEY_a, XKB_KEY_A])
  , (39, group [XKB_KEY_o, XKB_KEY_O])
  , (40, group [XKB_KEY_e, XKB_KEY_E])
  , (41, group [XKB_KEY_i, XKB_KEY_I])
  , (42, group [XKB_KEY_u, XKB_KEY_U])
  , (43, group [XKB_KEY_d, XKB_KEY_D])
  , (44, group [XKB_KEY_h, XKB_KEY_H])
  , (45, group [XKB_KEY_t, XKB_KEY_T])
  , (46, group [XKB_KEY_n, XKB_KEY_N])
  , (47, group [XKB_KEY_s, XKB_KEY_S])
  , (48, group [XKB_KEY_minus, XKB_KEY_underscore])
  , ( 49
    , group
        [ XKB_KEY_grave
        , XKB_KEY_asciitilde
        , XKB_KEY_dead_grave
        , XKB_KEY_dead_tilde
        ])
  , (50, group [XKB_KEY_Shift_L])
  , (51, group [XKB_KEY_backslash, XKB_KEY_bar])
  , ( 52
    , group
        [ XKB_KEY_colon
        , XKB_KEY_semicolon
        , XKB_KEY_dead_ogonek
        , XKB_KEY_dead_doubleacute
        ])
  , (53, group [XKB_KEY_q, XKB_KEY_Q])
  , (54, group [XKB_KEY_j, XKB_KEY_J])
  , (55, group [XKB_KEY_k, XKB_KEY_K])
  , (56, group [XKB_KEY_x, XKB_KEY_X])
  , (57, group [XKB_KEY_b, XKB_KEY_B])
  , (58, group [XKB_KEY_m, XKB_KEY_M])
  , (59, group [XKB_KEY_w, XKB_KEY_W])
  , (60, group [XKB_KEY_v, XKB_KEY_V])
  , (61, group [XKB_KEY_z, XKB_KEY_Z])
  , (62, group [XKB_KEY_Shift_R])
  , (63, group [XKB_KEY_KP_Multiply, XKB_KEY_KP_Multiply, XKB_KEY_KP_Multiply])
  , (64, group [XKB_KEY_Meta_L, XKB_KEY_Meta_L])
  , (65, group [XKB_KEY_space])
  , (66, group [XKB_KEY_Control_L])
  , (67, group [XKB_KEY_F1, XKB_KEY_F1, XKB_KEY_F1])
  , (68, group [XKB_KEY_F2, XKB_KEY_F2, XKB_KEY_F2])
  , (69, group [XKB_KEY_F3, XKB_KEY_F3, XKB_KEY_F3])
  , (70, group [XKB_KEY_F4, XKB_KEY_F4, XKB_KEY_F4])
  , (71, group [XKB_KEY_F5, XKB_KEY_F5, XKB_KEY_F5])
  , (72, group [XKB_KEY_F6, XKB_KEY_F6, XKB_KEY_F6])
  , (73, group [XKB_KEY_F7, XKB_KEY_F7, XKB_KEY_F7])
  , (74, group [XKB_KEY_F8, XKB_KEY_F8, XKB_KEY_F8])
  , (75, group [XKB_KEY_F9, XKB_KEY_F9, XKB_KEY_F9])
  , (76, group [XKB_KEY_F10, XKB_KEY_F10, XKB_KEY_F10])
  , (77, group [XKB_KEY_Num_Lock])
  , (78, group [XKB_KEY_Scroll_Lock])
  , (79, group [XKB_KEY_KP_Home, XKB_KEY_KP_7])
  , (80, group [XKB_KEY_KP_Up, XKB_KEY_KP_8])
  , (81, group [XKB_KEY_KP_Prior, XKB_KEY_KP_9])
  , (82, group [XKB_KEY_KP_Subtract, XKB_KEY_KP_Subtract, XKB_KEY_KP_Subtract])
  , (83, group [XKB_KEY_KP_Left, XKB_KEY_KP_4])
  , (84, group [XKB_KEY_KP_Begin, XKB_KEY_KP_5])
  , (85, group [XKB_KEY_KP_Right, XKB_KEY_KP_6])
  , (86, group [XKB_KEY_KP_Add, XKB_KEY_KP_Add, XKB_KEY_KP_Add])
  , (87, group [XKB_KEY_KP_End, XKB_KEY_KP_1])
  , (88, group [XKB_KEY_KP_Down, XKB_KEY_KP_2])
  , (89, group [XKB_KEY_KP_Next, XKB_KEY_KP_3])
  , (90, group [XKB_KEY_KP_Insert, XKB_KEY_KP_0])
  , (91, group [XKB_KEY_KP_Delete, XKB_KEY_KP_Decimal])
  , (92, group [XKB_KEY_ISO_Level3_Shift])
  , (94, group [XKB_KEY_less, XKB_KEY_greater, XKB_KEY_bar, XKB_KEY_brokenbar])
  , (95, group [XKB_KEY_F11, XKB_KEY_F11, XKB_KEY_F11])
  , (96, group [XKB_KEY_F12, XKB_KEY_F12, XKB_KEY_F12])
  , (98, group [XKB_KEY_Katakana])
  , (99, group [XKB_KEY_Hiragana])
  , (100, group [XKB_KEY_Henkan_Mode])
  , (101, group [XKB_KEY_Hiragana_Katakana])
  , (102, group [XKB_KEY_Muhenkan])
  , (104, group [XKB_KEY_KP_Enter])
  , (105, group [XKB_KEY_Control_R])
  , (106, group [XKB_KEY_KP_Divide, XKB_KEY_KP_Divide, XKB_KEY_KP_Divide])
  , (107, group [XKB_KEY_Print, XKB_KEY_Sys_Req])
  , (108, group [XKB_KEY_Alt_L, XKB_KEY_Alt_L])
  , (109, group [XKB_KEY_Linefeed])
  , (110, group [XKB_KEY_Home])
  , (111, group [XKB_KEY_Up])
  , (112, group [XKB_KEY_Prior])
  , (113, group [XKB_KEY_Left])
  , (114, group [XKB_KEY_Right])
  , (115, group [XKB_KEY_End])
  , (116, group [XKB_KEY_Down])
  , (117, group [XKB_KEY_Next])
  , (118, group [XKB_KEY_Insert])
  , (119, group [XKB_KEY_Delete])
  , (125, group [XKB_KEY_KP_Equal])
  , (126, group [XKB_KEY_plusminus])
  , (127, group [XKB_KEY_Pause, XKB_KEY_Break])
  , (129, group [XKB_KEY_KP_Decimal, XKB_KEY_KP_Decimal])
  , (130, group [XKB_KEY_Hangul])
  , (131, group [XKB_KEY_Hangul_Hanja])
  , (133, group [XKB_KEY_Super_L])
  , (134, group [XKB_KEY_Super_R])
  , (135, group [XKB_KEY_Menu])
  , (136, group [XKB_KEY_Cancel])
  , (137, group [XKB_KEY_Redo])
  , (139, group [XKB_KEY_Undo])
  , (144, group [XKB_KEY_Find])
  , (146, group [XKB_KEY_Help])
  , (187, group [XKB_KEY_parenleft])
  , (188, group [XKB_KEY_parenright])
  , (190, group [XKB_KEY_Redo])
  , (191, group [XKB_KEY_bar])
  , (192, group [XKB_KEY_parenleft])
  , (193, group [XKB_KEY_parenright])
  , (194, group [XKB_KEY_Insert])
  , (195, group [XKB_KEY_Alt_L, XKB_KEY_Alt_L])
  , (203, group [XKB_KEY_Mode_switch])
  , (204, group [XKB_KEY_NoSymbol, XKB_KEY_Alt_L])
  , (205, group [XKB_KEY_NoSymbol, XKB_KEY_Meta_L])
  , (206, group [XKB_KEY_NoSymbol, XKB_KEY_Super_L])
  , (207, group [XKB_KEY_NoSymbol, XKB_KEY_Hyper_L])
  , (218, group [XKB_KEY_Print])
  , (231, group [XKB_KEY_Cancel])
  ]
