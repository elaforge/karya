{- | Support for efficient keymaps.

    The sequece of Cmds which return Continue or Done is flexible, but probably
    inefficient in the presence of hundreds of commands.  In addition, it can't
    warn about Cmds that respond to overlapping Msgs, i.e. respond to the same
    key.

    Keymaps provide an efficient way to respond to a useful subset of Msgs,
    i.e.  those which are considered "key down" msgs or "controller change"
    type msgs.  The exact definition is in 'Key'.
-}
module Cmd.Keymap where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Log as Log

import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg

import qualified Midi.Midi as Midi

import qualified Cmd.Msg as Msg
import qualified Cmd.Cmd as Cmd


type Binding m = (KeySpec, CmdSpec m)

data KeySpec = KeySpec Mods Key deriving (Eq, Ord, Show)
-- | Pair a Cmd with a descriptive string that can be used for logging, undo,
-- etc.
data CmdSpec m = CmdSpec String (Msg.Msg -> Cmd.CmdM m)

-- | A Key is much like a 'Msg.Msg', but with less detail.
data Key = UiKey UiMsg.KbdState Key.Key | MidiKey MidiKey
    deriving (Eq, Ord, Show)

-- | Which modifiers have to be down for this to match?
-- TODO support higher-level modifiers like Shift (either shift key)
data Mods =
    -- | Match regardless of whether any other modifiers are down.
    AnyMod
    -- | Match if only "character" modifiers are down, but don't match if e.g.
    -- shift, control, or a midi key is down.
    | AnyCharMod
    -- | Match only if the specified list of modifiers is down.
    | Mods [Cmd.Modifier]
    deriving (Eq, Ord, Show)
is_char_mod (Cmd.KeyMod (Key.KeyChar _)) = True
is_char_mod _ = False

mods_match :: Mods -> Map.Map Cmd.Modifier Cmd.Modifier -> Bool
mods_match AnyMod _ = True
mods_match AnyCharMod mod_map = all is_char_mod (Map.keys mod_map)
mods_match (Mods mods) mod_map = mods == Map.keys mod_map

-- | This isn't used here, but other cmds may want to use keymap Mods.
-- TODO maybe it shoud move to Cmd.Cmd then?
require_mods :: (Monad m) => Mods -> Cmd.CmdT m ()
require_mods mods = do
    keys <- Cmd.keys_down
    if mods_match mods keys then return () else Cmd.abort

data MidiKey = NoteOn Midi.Key | NoteOff Midi.Key
    | Controller Midi.Controller Midi.ControlValue
    deriving (Eq, Ord, Show)

-- * building keymaps

-- | Simple cmd with keyboard key and no modifiers.
bind_key :: Key.Key -> String -> Cmd.CmdM m -> Binding m
bind_key = bind_mod []

-- | Bind a key with other keys for modifiers.
bind_kmod key_mods = bind_mod (map Cmd.KeyMod key_mods)

-- | Bind a key with anything as a modifier.
bind_mod :: [Cmd.Modifier] -> Key.Key -> String -> Cmd.CmdM m -> Binding m
bind_mod mods key desc cmd =
    (KeySpec (Mods mods) (UiKey UiMsg.KeyDown key), cspec_ desc cmd)

cspec :: String -> (Msg.Msg -> Cmd.CmdM m) -> CmdSpec m
cspec desc cmd = CmdSpec desc cmd

-- | Make a CmdSpec for a CmdM, i.e. a Cmd that doesn't take a Msg.
cspec_ :: String -> Cmd.CmdM m -> CmdSpec m
cspec_ desc cmd = CmdSpec desc (const cmd)


-- * keymap -> Cmd

-- | Merge the given KeySpecs into a map for efficient lookup, and return
-- a combined Cmd that will dispatch on the map.
make_cmd :: Monad m => [Binding m] -> Msg.Msg -> Cmd.CmdM m
make_cmd keyspecs msg = do
    key <- Cmd.require (msg_to_key msg)
    modifiers <- fmap Map.elems Cmd.keys_down
    let check_mods = [Mods modifiers]
            ++ if all is_char_mod modifiers then [AnyCharMod] else []
            ++ [AnyMod]
        look mods = Map.lookup (KeySpec mods key) keymap
    case Maybe.catMaybes (map look check_mods) of
        [] -> do
            -- Log.notice $ "no match for " ++ show (KeySpec modifiers key)
            return Cmd.Continue
        (CmdSpec name cmd : _) -> do
            Log.notice $ "running command " ++ show name
            cmd msg
            -- TODO move quit back into its own cmd and turn this on
            -- return Cmd.Done
    where
    -- TODO warn about overlapping mappings.
    keymap = Map.fromList keyspecs

msg_to_key :: Msg.Msg -> Maybe Key
msg_to_key msg = case msg of
    Msg.Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent (UiMsg.Kbd state key))) ->
        Just (UiKey state key)
    Msg.Midi (Midi.ReadMessage _ _ (Midi.ChannelMessage _ msg)) -> case msg of
        Midi.NoteOn key _ -> Just (MidiKey (NoteOn key))
        Midi.NoteOff key _ -> Just (MidiKey (NoteOff key))
        Midi.ControlChange c v -> Just (MidiKey (Controller c v))
        _ -> Nothing
    _ -> Nothing


-- * keymaps

-- This way I can set up the mapping relative to key position and have it come
-- out right for both qwerty and dvorak.  It could make the overlapping-ness of
-- non-mapped keys kind of hard to predict though.

qwerty = "1234567890-="
    ++ "qwertyuiop[]\\"
    ++ "asdfghjkl;'"
    ++ "zxcvbnm,./"

dvorak = "1234567890-="
    ++ "',.pyfgcrl[]\\"
    ++ "aoeuidhtns/"
    ++ ";qjkxbmwvz"

qwerty_to_dvorak = Map.fromList (zip qwerty dvorak)
-- TODO presumably this should eventually be easier to change
hardcoded_kbd_layout = qwerty_to_dvorak
