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

import qualified Util.Log as Log

import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg

import qualified Midi.Midi as Midi

import qualified Cmd.Msg as Msg
import qualified Cmd.Cmd as Cmd


data KeySpec = KeySpec [Cmd.Modifier] Key deriving (Eq, Ord, Show)
-- | Pair a Cmd with a descriptive string that can be used for logging, undo,
-- etc.
data CmdSpec m = CmdSpec String (MCmd m)

type MCmd m = Msg.Msg -> Cmd.CmdT m Cmd.Status

-- | A Key is much like a 'Msg.Msg', but with less detail.
data Key = UiKey Key.Key
    | MidiKey MidiKey
    deriving (Eq, Ord, Show)

data MidiKey = NoteOn Midi.Key | NoteOff Midi.Key
    | Controller Midi.Controller Midi.ControlValue
    deriving (Eq, Ord, Show)


-- | Merge the given KeySpecs into a map for efficient lookup, and return
-- a combined Cmd that will dispatch on the map.
make_cmd :: Monad m => [(KeySpec, CmdSpec m)] -> MCmd m
make_cmd keyspecs msg = do
    mod <- Cmd.require (keydown_mod msg)
    key <- Cmd.require (msg_to_key msg)
    -- Take the current key out of the modifiers map, so keymaps don't have to
    -- mention the current key in both the key and the modifier list.
    modifiers <- fmap (Map.elems . Map.delete mod) Cmd.keys_down
    case Map.lookup (KeySpec modifiers key) keymap of
        Nothing -> do
            -- Log.notice $ "no match for " ++ show (KeySpec modifiers key)
            return Cmd.Continue
        Just (CmdSpec name cmd) -> do
            Log.notice $ "running command " ++ show name
            cmd msg
            -- TODO move quit back into its own cmd and turn this on
            -- return Cmd.Done
    where
    -- TODO warn about overlapping mappings.
    keymap = Map.fromList keyspecs

keydown_mod msg = case Cmd.msg_to_mod msg of
    Nothing -> Nothing
    Just (False, _) -> Nothing
    Just (True, mod) -> Just mod

msg_to_key :: Msg.Msg -> Maybe Key
msg_to_key msg = case msg of
    Msg.Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent (UiMsg.Kbd UiMsg.KeyDown key))) ->
        Just (UiKey key)
    Msg.Midi (Midi.ReadMessage _ _ (Midi.ChannelMessage _ msg)) -> case msg of
        Midi.NoteOn key _ -> Just (MidiKey (NoteOn key))
        Midi.NoteOff key _ -> Just (MidiKey (NoteOff key))
        Midi.ControlChange c v -> Just (MidiKey (Controller c v))
        _ -> Nothing
    _ -> Nothing
