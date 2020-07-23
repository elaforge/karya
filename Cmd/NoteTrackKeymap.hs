-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Keymap cmds for a NoteTrack.  These apply regardless of the edit mode.
module Cmd.NoteTrackKeymap where
import qualified Ui.Key as Key
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Keymap as Keymap
import qualified Cmd.ModifyEvents as ModifyEvents

import qualified Derive.ParseTitle as ParseTitle
import Global


keymap :: Cmd.Keymap Cmd.CmdId
errors :: [Text]
(keymap, errors) = Keymap.make_keymap $ concat $
    map add_transform
        [('.', "."), ('v', "v"), ('6', "^"), ('=', "+"), ('-', "-")]
    where
    add_transform (c, name) = Keymap.bind_key [Keymap.SecondaryCommand]
        (Key.Char c) ("add " <> name) (add_transform_generator name)

-- | Add a call that works both as a transformer and generator, as long as
-- it isn't already there.
add_transform_generator :: Cmd.M m => Text -> m ()
add_transform_generator text = do
    ModifyEvents.selection_visible $
        ModifyEvents.tracks_named ParseTitle.is_note_track $
        ModifyEvents.text $ ModifyEvents.pipeline add
    ModifyEvents.advance_if_point
    where
    add [] = [[text]]
    add calls
        | [text] `elem` calls = calls
        | otherwise = [text] : calls
