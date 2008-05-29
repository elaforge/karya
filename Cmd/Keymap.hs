{- | Support for efficient keymaps.

The sequece of Cmds which return Continue or Done is flexible, but probably
inefficient in the presence of hundreds of commands.  In addition, it can't
warn about Cmds that respond to overlapping Msgs, i.e. respond to the same key.

Keymaps provide an efficient way to respond to a useful subset of Msgs, i.e.
those which are considered "key down" msgs or "controller change" type msgs.
The exact definition is in 'Key'.
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

data MidiKey = NoteOn Midi.Key | NoteOff Midi.Key
    | Controller Midi.Controller Midi.ControlValue
    deriving (Eq, Ord, Show)


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
