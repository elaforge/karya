module Ui.Key (Key(..), Modifier(..), decode_key, decode_modifiers) where
import Prelude hiding (Char, Left, Right)
import Data.Bits ((.&.))
import qualified Data.Map as Map
import qualified Data.Char as Char
import Foreign.C
import qualified System.Info

import qualified Util.Pretty as Pretty

-- Actually just need FL/Fl_Enumerations.H
#include "Ui/c_interface.h"


-- | A keystroke, which is not just a character but also back
data Key = Char Char.Char
    | Escape | Backspace | Tab | Enter | Print | KScrollLock
    | Pause | Insert | Home | PageUp | Delete | End | PageDown
    | Left | Up | Right | Down
    | ShiftL | ShiftR | ControlL | ControlR | KCapsLock | AltL | AltR
    | MetaL | MetaR | Menu | KNumLock | KPEnter
    | Keypad Char.Char
    | Unknown Int
    deriving (Eq, Ord, Read, Show)

data Modifier = Shift | CapsLock | Control | Alt | NumLock | Meta | ScrollLock
    deriving (Eq, Ord, Read, Show)

instance Pretty.Pretty Key where
    pretty (Char c) = c : ""
    pretty key = map Char.toLower (show key)

decode_key :: CInt -> Key
decode_key code
    | code /= (#const FL_KP_Enter)
        && (#const FL_KP) < code && code <= (#const FL_KP_Last)
            = Keypad (toEnum (fromIntegral (code - (#const FL_KP))))
    | code <= 127 = Char (unshift (toEnum (fromIntegral code)))
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

unshift :: Char.Char -> Char.Char
unshift = case System.Info.os of
    "darwin" -> \c -> Map.findWithDefault c c unshifted_osx_usa
    _ -> id

-- | Map a shifted symbol back to its unshifted variant for a US key layout.
-- This is because fltk has a bug on OS X that makes shifted symbols emit the
-- key text, rather than the keycap.
unshifted_osx_usa :: Map.Map Char.Char Char.Char
unshifted_osx_usa = Map.fromList
    [ ('~', '`'), ('_', '-'), ('+', '=')
    , ('{', '['), ('}', ']'), ('|', '\\')
    , (':', ';'), ('"', '\'')
    , ('<', ','), ('>', '.'), ('?', '/')
    ]
