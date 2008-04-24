{- | Support for efficient keymaps.

The sequece of Cmds which return Continue or Done is flexible, but probably
inefficient in the presence of hundreds of commands.  In addition, it can't
warn about Cmds that respond to overlapping Msgs, i.e. respond to the same key.

Keymaps simplify Msgs and 
-}
module Cmd.Keymap where
import qualified Data.Map as Map

import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Msg as Msg
import qualified Cmd.Cmd as Cmd


data KeySpec = KeySpec [Cmd.Modifier] Key deriving (Eq, Ord, Show)
-- | A Key is much like a 'Msg.Msg', but with less detail.
data Key = UiKey Key.Key
    -- | A Qwerty key is given by its position on the qwerty layout.  This is
    -- so I can specify keys by position and have them be placed properly in
    -- qwerty and dvorak and whatever other layout.
    | QwertyKey Key.Key
    -- MidiKey .. nn, controller, or pb
    deriving (Eq, Ord, Show)


-- | Merge the given KeySpecs into a map for efficient lookup, and return
-- a combined Cmd that will dispatch on the map.
make_cmd :: [(KeySpec, Cmd.Cmd)] -> Cmd.Cmd
make_cmd keyspecs msg = do
    modifiers <- Cmd.keys_down
    key <- Cmd.require (msg_to_key msg)
    case Map.lookup (KeySpec modifiers key) keymap of
        Nothing -> return Cmd.Continue
        Just cmd -> cmd msg
    where
    keymap = Map.fromList keyspecs

msg_to_key :: Msg.Msg -> Maybe Key
msg_to_key msg = case msg of
    Msg.Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent (UiMsg.Kbd UiMsg.KeyDown key))) ->
        Just (UiKey key)
    _ -> Nothing
