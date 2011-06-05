module Ui.Key where
import Prelude hiding (Left, Right)
import Data.Bits ((.&.))
import qualified Data.Char as Char
import Foreign.C


-- | A keystroke, which is not just a character but also back
data Key = KeyChar Char
    | Escape | Backspace | Tab | Enter | Print | KScrollLock
    | Pause | Insert | Home | PageUp | Delete | End | PageDown
    | Left | Up | Right | Down
    | ShiftL | ShiftR | ControlL | ControlR | KCapsLock | AltL | AltR
    | MetaL | MetaR | Menu | KNumLock | KPEnter
    | Keypad Char
    | Unknown Int
    deriving (Eq, Ord, Read, Show)

data Modifier = Shift | CapsLock | Control | Alt | NumLock | Meta | ScrollLock
    deriving (Eq, Ord, Read, Show)

-- Actually just need FL/Fl_Enumerations.H
#include "c_interface.h"

-- | The cmd binding assumes that shifted chars will also be uppercase, but
-- fltk can't be trusted to do that consistently.
decode :: CInt -> CInt -> ([Modifier], Key)
decode mcode kcode = (mods, if Shift `elem` mods then toupper key else key)
    where
    mods = decode_modifiers mcode
    key = decode_key kcode
    toupper (KeyChar c) = KeyChar (Char.toUpper c)
    toupper k = k

decode_key :: CInt -> Key
decode_key code
    | code /= (#const FL_KP_Enter)
        && (#const FL_KP) < code && code <= (#const FL_KP_Last)
            = Keypad (toEnum (fromIntegral (code - (#const FL_KP))))
    | code <= 127 = KeyChar (toEnum (fromIntegral code))
    | otherwise = case code of
        (#const FL_Escape) -> Escape
        (#const FL_BackSpace) -> Backspace
        (#const FL_Tab) -> Tab
        (#const FL_Enter) -> Enter
        (#const FL_Print) -> Print
        (#const FL_Scroll_Lock) -> KScrollLock
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
        (#const FL_Caps_Lock) -> KCapsLock
        (#const FL_Alt_L) -> AltL
        (#const FL_Alt_R) -> AltR
        (#const FL_Meta_L) -> MetaL
        (#const FL_Meta_R) -> MetaR
        (#const FL_Menu) -> Menu
        (#const FL_Num_Lock) -> KNumLock
        (#const FL_KP_Enter) -> KPEnter
        _ -> Unknown (fromIntegral code)

decode_modifiers :: CInt -> [Modifier]
decode_modifiers code = foldr f [] pairs
    where
    f (bit, mod) mods
        | bit .&. code /= 0 = mod : mods
        | otherwise = mods
    pairs =
        [ ((#const FL_SHIFT), Shift)
        , ((#const FL_CAPS_LOCK), CapsLock)
        , ((#const FL_CTRL), Control)
        , ((#const FL_ALT), Alt)
        , ((#const FL_NUM_LOCK), NumLock)
        , ((#const FL_META), Meta)
        , ((#const FL_SCROLL_LOCK), ScrollLock)
        ]
