-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities to set up the initial score.
-- Mostly just testing hackery.
module Local.Setup where
import qualified Control.Monad.Trans as Trans

import Util.Control
import qualified Util.Seq as Seq
import qualified Midi.Midi as Midi
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Load.Midi as Load.Midi
import qualified Cmd.Load.Mod as Load.Mod
import qualified Cmd.Meters as Meters
import qualified Cmd.RulerUtil as RulerUtil
import qualified Cmd.Selection as Selection

import qualified Derive.Derive_profile as Derive_profile
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Pitch as Pitch
import Types


arrival_beats :: Bool
arrival_beats = False

auto_setup_cmd :: Cmd.CmdIO
auto_setup_cmd = setup_small

setup_generate :: String -> Cmd.CmdIO
setup_generate gen = do
    case gen of
        "simple" -> Derive_profile.make_simple 200
        "nested" -> Derive_profile.make_nested_controls 8 3 64
        "nested-small" -> Derive_profile.make_nested_controls 8 1 64
        "control" -> Derive_profile.make_big_control 15000
        "shared" -> Derive_profile.make_shared_control 2000
        _ -> error gen
    State.set_midi_config $
        make_midi_config "fm8" [("fm8/1", [0..2]), ("fm8/2", [3])]
    Create.unfitted_view (UiTest.bid "b01")
    Create.map_track_titles set_inst
    return Cmd.Done
    where
    set_inst title
        | untxt title == Derive_profile.inst1 = ">fm8/1"
        | untxt title == Derive_profile.inst2 = ">fm8/2"
        | otherwise = title

load_mod :: FilePath -> Cmd.CmdIO
load_mod fn = do
    blocks <- either Cmd.throw return =<< Trans.liftIO (Load.Mod.parse fn)
    let blocks2 = map
            (Load.Mod.map_block (Load.Mod.add_default_volume 1 38)) blocks
    Load.Mod.create (Id.unsafe_namespace $ head (Seq.split "." fn))
        (Load.Mod.convert_blocks 0.25 blocks2)
    State.set_midi_config $ make_midi_config "ptq" [("ptq/c1", [0..8])]
    return Cmd.Done

load_midi :: FilePath -> Cmd.CmdIO
load_midi fn = do
    block_id <- Load.Midi.load fn
    Create.unfitted_view block_id
    return Cmd.Done

setup_small :: (Cmd.M m) => m Cmd.Status
setup_small = do
    bid <- empty_block
    note <- Create.empty_track bid 2
    State.insert_events note $ map UiTest.make_event
        [(0, 1, ""), (1, 1, ""), (2, 1, ""), (3, 1, "")]
    State.set_track_title note ">ptq/c"
    pitch <- Create.empty_track bid 4
    State.insert_events pitch $ map UiTest.make_event
        [(0, 0, "5c"), (1, 0, "5d"), (2, 0, "5e"), (3, 0, "5f")]
    State.set_track_title pitch "*"
    State.modify_track_render pitch $ \render ->
        render { Track.render_style = Track.Line Nothing }
    State.set_skeleton bid $ Skeleton.make [(1, 2), (2, 3)]
    State.set_midi_config (make_midi_config "ptq" [("ptq/c", [0..2])])
    vid <- Create.view bid
    Selection.set vid (Just (Types.point_selection 2 0))
    return Cmd.Done

setup_normal :: (Cmd.M m) => m Cmd.Status
setup_normal = do
    bid <- empty_block
    -- tempo is track 1

    mod <- Create.empty_track bid 2
    State.insert_events mod $ map (control_event . UiTest.make_event)
        [(0, 0, "0"), (1, 0, "i 1"), (2, 0, "i 0"), (2.5, 0, "1"),
            (3, 0, ".5")]
    State.set_track_title mod "mod"
    State.modify_track_render mod $ \render ->
        render { Track.render_style = Track.Filled Nothing }

    note <- Create.empty_track bid 3
    State.insert_events note $ map (note_event . UiTest.make_event)
        [(0, 1, ""), (1, 1, ""), (2, 1, ""), (3, 1, "")]
    State.set_track_title note ">fm8/bass"

    pitch <- Create.empty_track bid 4
    State.insert_events pitch $ map (control_event . UiTest.make_event)
        [(0, 0, "`tr` (5c) 2 3"), (1, 0, "n (5d)"), (2, 0, "5e"),
            (3, 0, "i (5f)")]
    State.set_track_title pitch "*twelve | key = 'c-maj'"
    State.modify_track_render pitch $ \render ->
        render { Track.render_style = Track.Line Nothing }
    State.set_track_width bid 3 50

    dyn <- Create.empty_track bid 5
    State.insert_events dyn $ map (control_event . UiTest.make_event)
        [ (0, 0, untxt $ ShowVal.show_hex_val 0.7)
        , (1, 0, untxt $ ShowVal.show_hex_val 0.4)
        ]
    State.set_track_title dyn "dyn"

    -- tempo 1 -> mod -> note -> pitch
    State.set_skeleton bid $ Skeleton.make [(1, 2), (2, 3), (3, 4), (4, 5)]

    State.set_midi_config (make_midi_config "fm8" [("fm8/bass", [0..2])])
    vid <- Create.view bid
    Selection.set vid (Just (Types.point_selection 0 0))
    return Cmd.Done
    where
    note_event event
        | arrival_beats = Event.move (+ Event.duration event) $
            Event.modify_duration negate event
        | otherwise = event
    control_event event
        | arrival_beats = Event.move (+1) $ Event.modify_duration negate event
        | otherwise = event

setup_big :: (Cmd.M m) => m Cmd.Status
setup_big = do
    b <- empty_block
    t0 <- Create.empty_track b 2
    State.set_track_title t0 ">fm8/bass"
    t0_p <- Create.empty_track b 3
    State.set_track_title t0_p "p"

    let notes = [0, 3, 2, 5, 3, 6, 4, 7]
        vels = [1, 0.9, 0.8, 0.7, 0.6, 0.4, 0.3, 0.2]
        mknotes notes = map UiTest.make_event
            [(i*0.25, 0.2, to_str (oct*12 + n))
                | (i, (oct, n)) <- zip (Seq.range_ 0 1) notes]
        to_str n = case Scale.scale_input_to_note Twelve.scale Nothing
                (Pitch.InputKey n) of
            Just (Pitch.Note s) -> untxt s
            Nothing -> error $ "converting " ++ show n
        mkdyn vels = map UiTest.make_event
            [(i*0.25, 0, show vel) | (i, vel) <- zip (Seq.range_ 0 1) vels]

    State.insert_events t0 (take 100 (mknotes (cycle (map ((,) 5) notes))))
    State.insert_events t0_p (take 100 (mkdyn (cycle vels)))

    t1 <- Create.empty_track b 4
    State.set_track_title t1 ">fm8/bass"
    t1_p <- Create.empty_track b 5
    State.set_track_title t1_p "dyn"
    State.insert_events t1
        (take 100 (mknotes (cycle (reverse (map ((,) 6) notes)))))
    State.insert_events t1_p (take 100 (mkdyn (cycle (reverse vels))))

    State.set_midi_config (make_midi_config "fm8" [("fm8/bass", [0..2])])
    State.modify_default $ \d ->
        d { State.default_instrument = Just (Score.Instrument "fm8/bass") }
    vid <- Create.view b
    Selection.set vid (Just (Types.point_selection 0 0))
    return Cmd.Done

empty_block :: (Cmd.M m) => m BlockId
empty_block = do
    rid <- Create.ruler "meter44"
        (RulerUtil.meter_ruler 16 (replicate 4 Meters.m44_4))
        { Ruler.ruler_align_to_bottom = arrival_beats }

    bid <- Create.block rid
    t_tempo <- Create.track bid 1 "tempo" $
        Events.from_list $ map UiTest.make_event [(0, 0, "1")]
    State.set_track_width bid 1 40
    State.insert_events t_tempo $ map UiTest.make_event [(0, 0, "1")]
    return bid

make_midi_config :: Text -> [(Text, [Midi.Channel])] -> Instrument.Configs
make_midi_config dev config = Instrument.configs
    [(Score.Instrument inst, map mkaddr chans) | (inst, chans) <- config]
    where mkaddr chan = (Midi.write_device dev, chan)
