-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Operations on control tracks.
module Cmd.Repl.LControl where
import qualified Prelude
import           Prelude hiding (round)

import qualified Util.Num as Num
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
multiply factor = mapVal (*factor)

-- | Multiply but keep in 0--1.
multiply01 :: Signal.Y -> Cmd.CmdL ()
multiply01 factor = mapVal (Num.clamp 0 1 . (*factor))

-- | Multiply but keep in -1 -- 1.
multiply11 :: Signal.Y -> Cmd.CmdL ()
multiply11 factor = mapVal (Num.clamp (-1) 1 . (*factor))

-- | Round selected controls to the given number of decimal places.  Useful
-- after a 'multiply'.
round :: Int -> Cmd.CmdL ()
round places = mapVal $
    (/ 10^places) . fromIntegral . Prelude.round . (* 10^places)

mapVal :: Cmd.M m => (Signal.Y -> Signal.Y) -> m ()
mapVal f = ModifyEvents.selection $
    ModifyEvents.text $ \text -> fromMaybe text (ControlTrack.modifyVal f text)

-- | Map controls with the given name.
mapNamed :: Cmd.M m => Text -> (Signal.Y -> Signal.Y) -> ModifyEvents.Track m
mapNamed name f =
    ModifyEvents.tracksNamed (==name) $ ModifyEvents.text $ \text ->
        fromMaybe text (ControlTrack.modifyVal f text)

scoreToHex :: Cmd.CmdL ()
scoreToHex = ModifyEvents.allBlocks $
    ModifyEvents.tracksNamed ParseTitle.isSignalTrack $
        ModifyEvents.text toHex

blockToHex :: BlockId -> Cmd.CmdL ()
blockToHex blockId = ModifyEvents.block blockId $
    ModifyEvents.tracksNamed ParseTitle.isSignalTrack $
        ModifyEvents.text toHex

toHex :: Text -> Text
toHex text =
    either (const text) toNum $
        Derive.Parse.parseVal (ControlTrack._val partial)
    where
    toNum v = case DeriveT.constantVal v of
        Just (ScoreT.Typed ScoreT.Untyped n)
            | 0 <= n && n <= 1 -> ControlTrack.unparse $
                partial { ControlTrack._val = ShowVal.showHexVal n }
        _ -> text
    partial = ControlTrack.parse text
