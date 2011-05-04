{-# LANGUAGE ViewPatterns #-}
{- | Support for efficient keymaps.

    The sequece of Cmds which return Continue or Done is flexible, but probably
    inefficient in the presence of hundreds of commands.  In addition, it can't
    warn about Cmds that respond to overlapping Msgs, e.g. respond to the same
    key.

    Keymaps provide an efficient way to respond to a useful subset of Msgs,
    i.e.  those which are considered 'key down' type msgs.  The exact
    definition is in 'Bindable'.
-}
module Cmd.Keymap where
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Msg as Msg
import qualified Cmd.Cmd as Cmd


-- * building

-- | Binding with no modifiers.
bind_key :: (Cmd.M m) => Key.Key -> String -> m a -> [Binding m]
bind_key = bind_mod []

-- | Bind a char with no modifiers.
--
-- Binding functions that take Char will add a Shift if it's uppercase.
bind_char :: (Cmd.M m) => Char -> String -> m a -> [Binding m]
bind_char char = bind_mod (char_shift char []) (Key.KeyChar char)

-- | Many cmds are mapped to both a plain keystroke and command key version.
-- This is a little unusual, but it means the command can still be invoked when
-- an edit mode has taken over the alphanumeric keys.
command :: (Cmd.M m) => Key.Key -> String -> m a -> [Binding m]
command key desc cmd =
    bind_key key desc cmd ++ bind_mod [PrimaryCommand] key desc cmd

command_char :: (Cmd.M m) => Char -> String -> m a -> [Binding m]
command_char char desc cmd =
    bind_char char desc cmd ++ command_only char desc cmd

-- | But some commands are too dangerous to get a plain keystroke version.
command_only :: (Cmd.M m) => Char -> String -> m a -> [Binding m]
command_only char =
    bind_mod (char_shift char [PrimaryCommand]) (Key.KeyChar char)

-- | Bind a key with the given modifiers.
bind_mod :: (Cmd.M m) => [SimpleMod] -> Key.Key -> String -> m a -> [Binding m]
bind_mod smods bindable desc cmd = bind smods (Key bindable) desc (const cmd)

-- | 'bind_click' passes the Msg to the cmd, since mouse cmds are more likely
-- to want the msg to find out where the click was.  @clicks@ is 1 for a single
-- click, 2 for a double click, etc.
bind_click :: (Cmd.M m) => [SimpleMod] -> UiMsg.MouseButton -> Int -> String
    -> (Msg.Msg -> m a) -> [Binding m]
bind_click smods btn clicks desc cmd =
    bind smods (Click btn (clicks-1)) desc cmd

-- | A 'bind_drag' binds both the click and the drag.  It's conceivable to have
-- click and drag bound to different commands, but I don't have any yet.
bind_drag :: (Cmd.M m) => [SimpleMod] -> UiMsg.MouseButton -> String
    -> (Msg.Msg -> m a) -> [Binding m]
bind_drag smods btn desc cmd =
    bind mods (Click btn 0) desc cmd ++ bind mods (Drag btn) desc cmd
    -- You can't have a click or drag without having that button down!
    where mods = Mouse btn : smods

-- | Bind a key with the given modifiers.
bind :: (Cmd.M m) => [SimpleMod] -> Bindable -> String
    -> (Msg.Msg -> m a) -> [Binding m]
bind smods bindable desc bcmd =
    [(key_spec mods bindable, cspec desc cmd) | mods <- expand_mods smods]
    where cmd msg = bcmd msg >> return Cmd.Done

-- ** util

expand_mods :: [SimpleMod] -> [[Cmd.Modifier]]
expand_mods [] = [[]]
expand_mods smods = Seq.cartesian (map simple_to_mods smods)

char_shift :: Char -> [SimpleMod] -> [SimpleMod]
char_shift c mods
    | Char.isUpper c = Shift : mods
    | otherwise = mods

-- * CmdMap

-- | Create a CmdMap for efficient lookup and return warnings encountered
-- during construction.
make_cmd_map :: (Monad m) => [Binding m] -> (CmdMap m, [String])
make_cmd_map bindings = (Map.fromList bindings, warns)
    where
    warns = ["cmds overlap, picking the last one: " ++ Seq.join ", " cmds
        | cmds <- overlaps bindings]

-- | Create a cmd that dispatches into the given CmdMap.
--
-- To look up a cmd, the Msg is restricted to a 'Bindable'.  Then modifiers
-- that are allowed to overlap (such as keys) are stripped out of the mods and
-- the KeySpec is looked up in the keymap.
make_cmd :: (Cmd.M m) => CmdMap m -> Msg.Msg -> m Cmd.Status
make_cmd cmd_map msg = do
    bindable <- Cmd.require (msg_to_bindable msg)
    mods <- mods_down
    case Map.lookup (KeySpec mods bindable) cmd_map of
        Nothing -> return Cmd.Continue
        Just (CmdSpec name cmd) -> do
            Log.debug $ "running command " ++ show name
            cmd msg


-- | The Msg contains the low level key information, but most commands should
-- probably use these higher level modifiers.  That way left and right shifts
-- work the same, and cmds can use Command as customary on the Mac and Control
-- as customary on linux.
--
-- Things you have to inspect the Msg directly for:
--
-- - differentiate ShiftL and ShiftR
--
-- - chorded keys
--
-- - use option on the mac
data SimpleMod =
    Shift
    -- | Primary command key: command on mac, control on linux and windows
    -- This should be used for core and global commands.
    | PrimaryCommand
    -- | Secondary comamnd key: control or option on mac, alt on linux and
    -- windows.  I'm not sure what this should be used for, but it should be
    -- different than Mod1 stuff.  Maybe static config user-added commands.
    | SecondaryCommand
    -- | Having mouse here allows for mouse button chording.
    | Mouse UiMsg.MouseButton
    deriving (Eq, Ord, Show)

-- * implementation

-- | TODO This is a hardcoded mac layout, when I support other platforms
-- it'll have to be configurable.
simple_mod_map :: [(SimpleMod, [Key.Modifier])]
simple_mod_map =
    [ (Shift, [Key.Shift])
    , (PrimaryCommand, [Key.Meta])
    -- Alt is the mac's option key.
    , (SecondaryCommand, [Key.Control, Key.Alt])
    ]

simple_to_mods :: SimpleMod -> [Cmd.Modifier]
simple_to_mods (Mouse btn) = [Cmd.MouseMod btn Nothing]
simple_to_mods simple = maybe [] (map Cmd.KeyMod) (lookup simple simple_mod_map)

-- ** Binding

type Binding m = (KeySpec, CmdSpec m)

data KeySpec = KeySpec (Set.Set Cmd.Modifier) Bindable deriving (Eq, Ord, Show)

key_spec :: [Cmd.Modifier] -> Bindable -> KeySpec
key_spec mods bindable = KeySpec (Set.fromList mods) bindable

-- | Pair a Cmd with a descriptive string that can be used for logging, undo,
-- etc.
data CmdSpec m = CmdSpec String (Msg.Msg -> m Cmd.Status)

cspec :: String -> (Msg.Msg -> m Cmd.Status) -> CmdSpec m
cspec = CmdSpec

-- | Make a CmdSpec for a CmdM, i.e. a Cmd that doesn't take a Msg.
cspec_ :: String -> m Cmd.Status -> CmdSpec m
cspec_ desc cmd = CmdSpec desc (const cmd)

-- ** CmdMap

type CmdMap m = Map.Map KeySpec (CmdSpec m)

overlaps :: [Binding m] -> [[String]]
overlaps bindings =
    [map cmd_name grp | grp <- Seq.group_on fst bindings, length grp > 1]
    where cmd_name (kspec, CmdSpec name _) = show kspec ++ ": " ++ name

-- | Return the mods currently down, stripping out non-modifier keys and notes,
-- so that overlapping keys will still match.  Mouse mods are not filtered, so
-- each mouse chord can be bound individually.
mods_down :: (Cmd.M m) => m (Set.Set Cmd.Modifier)
mods_down = do
    mods <- fmap (filter is_mod . Map.keys) Cmd.keys_down
    return $ Set.fromList mods
    where
    is_mod (Cmd.KeyMod _) = True
    is_mod (Cmd.MidiMod _ _) = False
    is_mod (Cmd.MouseMod _ _) = True

msg_to_bindable :: Msg.Msg -> Maybe Bindable
msg_to_bindable msg = case msg of
    (Msg.key_down -> Just key) -> Just $ Key key
    (Msg.mouse -> Just mouse) -> case UiMsg.mouse_state mouse of
        UiMsg.MouseDown btn -> Just $ Click btn (UiMsg.mouse_clicks mouse)
        UiMsg.MouseDrag btn -> Just $ Drag btn
        _ -> Nothing
    (Msg.midi -> Just (Midi.ChannelMessage chan (Midi.NoteOn key _))) ->
        Just $ Note chan key
    _ -> Nothing

data Bindable = Key Key.Key
    -- | Click MouseButton Clicks
    | Click UiMsg.MouseButton Int
    | Drag UiMsg.MouseButton
    -- | Channel can be used to restrict bindings to a certain keyboard.  This
    -- should probably be something more abstract though, such as a device
    -- which can be set by the static config.
    | Note Midi.Channel Midi.Key
    deriving (Eq, Ord, Show, Read)


-- * key layout

-- | Map a physical key, written relative to USA qwerty layout, to whatever
-- character that key actually emits (if the layout is already USA qwerty then
-- it's id of course).  This is for layouts which should be done based on
-- physical key position, like piano-style keyboards.  It makes the
-- overlapping-ness of non-mapped keys hard to predict though.
--
-- Since it's intended to map literal key characters, there is no accomodation
-- for failure.  Really this should be done at compile time, so it's
-- conceptually a compile time error.
--
-- TODO isn't there some way I can get this at compile time?  template haskell?
physical_key :: Char -> Char
physical_key c =
    maybe (error $ "Keymap.physical_key " ++ show c ++ " not found") id $
        Map.lookup c hardcoded_kbd_layout


qwerty :: [Char]
qwerty = "1234567890-="
    ++ "qwertyuiop[]\\"
    ++ "asdfghjkl;'"
    ++ "zxcvbnm,./"

    ++ "!@#$%^&*()_+"
    ++ "QWERTYUIOP{}|"
    ++ "ASDFGHJKL;\""
    ++ "ZXCVBNM<>?"

-- | Not just dvorak, but my slightly modified version.
dvorak :: [Char]
dvorak = "1234567890-="
    ++ "',.pyfgcrl[]\\"
    ++ "aoeuidhtns/"
    ++ ";qjkxbmwvz"

    ++ "!@#$%^&*()_+"
    ++ "\"<>PYFGCRL{}|"
    ++ "AOEUIDHTNS?"
    ++ ":QJKXBMWVZ"

qwerty_to_dvorak :: Map.Map Char Char
qwerty_to_dvorak = Map.fromList (zip qwerty dvorak)

-- TODO presumably this should eventually be easier to change
hardcoded_kbd_layout :: Map.Map Char Char
hardcoded_kbd_layout = qwerty_to_dvorak
