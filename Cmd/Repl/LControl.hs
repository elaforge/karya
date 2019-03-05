-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Operations on control tracks.
module Cmd.Repl.LControl where
import qualified Prelude
import           Prelude hiding (round)

import qualified Cmd.Cmd as Cmd
import qualified Cmd.ControlTrack as ControlTrack
import qualified Cmd.ModifyEvents as ModifyEvents

import qualified Derive.DeriveT as DeriveT
import qualified Derive.Parse
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Signal as Signal

import           Global
import           Types


-- | Multiply the controls in the selection by the given amount.
multiply :: Signal.Y -> Cmd.CmdL ()
multiply factor = map_val (*factor)

-- | Round selected controls to the given number of decimal places.  Useful
-- after a 'multiply'.
round :: Int -> Cmd.CmdL ()
round places = map_val $
    (/ 10^places) . fromIntegral . Prelude.round . (* 10^places)

map_val :: (Signal.Y -> Signal.Y) -> Cmd.CmdL ()
map_val f = ModifyEvents.selection $
    ModifyEvents.text $ \text -> fromMaybe text (ControlTrack.modify_val f text)

-- | Map controls with the given name.
map_named :: Cmd.M m => Text -> (Signal.Y -> Signal.Y) -> ModifyEvents.Track m
map_named name f =
    ModifyEvents.tracks_named (==name) $ ModifyEvents.text $ \text ->
        fromMaybe text (ControlTrack.modify_val f text)

score_to_hex :: Cmd.CmdL ()
score_to_hex = ModifyEvents.all_blocks $
    ModifyEvents.tracks_named ParseTitle.is_signal_track $
        ModifyEvents.text to_hex

block_to_hex :: BlockId -> Cmd.CmdL ()
block_to_hex block_id = ModifyEvents.block block_id $
    ModifyEvents.tracks_named ParseTitle.is_signal_track $
        ModifyEvents.text to_hex

to_hex :: Text -> Text
to_hex text =
    case Derive.Parse.parse_val (ControlTrack.event_val event) of
        Right (DeriveT.VNum (ScoreT.Typed ScoreT.Untyped n))
            | 0 <= n && n <= 1 -> ControlTrack.unparse $
                event { ControlTrack.event_val = ShowVal.show_hex_val n }
        _ -> text
    where event = ControlTrack.parse text

