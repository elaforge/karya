module Ui.Key where
import Prelude hiding (Left, Right)

-- | A keystroke, which is not just a character but also back
data Key = KeyChar Char
    | Escape | Backspace | Tab | Enter | Print | ScrollLock
    | Pause | Insert | Home | PageUp | Delete | End | PageDown
    | Left | Up | Right | Down
    | ShiftL | ShiftR | ControlL | ControlR | CapsLock | AltL | AltR
    | MetaL | MetaR | Menu | NumLock | KPEnter
    | Unknown Int
    deriving (Eq, Show)

-- Actually just need FL/Fl_Enumerations.H
#include "c_interface.h"

decode_key :: Int -> Key
decode_key code
    | code <= 127 = KeyChar (toEnum code)
    | otherwise = case code of
        (#const FL_Escape) -> Escape
        (#const FL_BackSpace) -> Backspace
        (#const FL_Tab) -> Tab
        (#const FL_Enter) -> Enter
        (#const FL_Print) -> Print
        (#const FL_Scroll_Lock) -> ScrollLock
        (#const FL_Pause) -> Pause
        (#const FL_Insert) -> Insert
        (#const FL_Home) -> Home
        (#const FL_Page_Up) -> PageUp
        (#const FL_Delete) -> Delete
        (#const FL_End) -> End
        (#const FL_Page_Down) -> PageDown
        (#const FL_Left) -> Left
        (#const FL_Up) -> Up
        (#const FL_Right) -> Right
        (#const FL_Down) -> Down
        (#const FL_Shift_L) -> ShiftL
        (#const FL_Shift_R) -> ShiftR
        (#const FL_Control_L) -> ControlL
        (#const FL_Control_R) -> ControlR
        (#const FL_Caps_Lock) -> CapsLock
        (#const FL_Alt_L) -> AltL
        (#const FL_Alt_R) -> AltR
        (#const FL_Meta_L) -> MetaL
        (#const FL_Meta_R) -> MetaR
        (#const FL_Menu) -> Menu
        (#const FL_Num_Lock) -> NumLock
        (#const FL_KP_Enter) -> KPEnter
        _ -> Unknown code
