-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Load.Mod_test where
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

import Util.Test
import qualified Ui.Event as Event
import qualified Ui.Skeleton as Skeleton
import qualified Ui.UiTest as UiTest

import qualified Cmd.Load.Med as Med
import qualified Cmd.Load.Mod as Mod
import qualified Cmd.Load.ModTypes as M

import qualified Derive.ScoreT as ScoreT
import qualified Perform.Pitch as Pitch
import Global
import Types


load = do
    mod <- Med.load mempty "underwater"
    let bs = M._blocks mod
    -- pprint b
    -- pprint $ Mod.convert_block initial_state b

    let e = convert mod
    pprint ((!!1) <$> e)

    -- pprint (M._tracks b)

convert :: M.Module
    -> Either Text [(BlockId, (UiTest.BlockSpec, [Skeleton.Edge]))]
convert = (bimap pretty UiTest.dump_blocks) . Mod.convert UiTest.test_ns

tempo :: M.Tempo
tempo = M.Tempo 33 6

inst :: M.Instrument
inst = M.Instrument (ScoreT.Instrument "inst") (Just 0.5)

initial_state :: Mod.State
initial_state = Mod.State
    { _tempo = M.Tempo 33 6
    , _instruments = IntMap.fromList [(0, inst)]
    }

test_convert_block = do
    let f = Mod.convert_block initial_state
    let empty = M.Line Nothing 0 []
    pprint (f (M.Block [[empty, empty], [empty, empty]] 2))

test_make_skeleton = do
    let f = Skeleton.flatten . Mod.make_skeleton
    equal (f []) []
    equal (f [['a'], ['b', 'c']]) [(2, 3)]
    equal (f [['a', 'b'], ['c', 'd', 'e']]) [(1, 2), (3, 4), (4, 5)]

test_convert_note = do
    let f linenum cmds lines =
            extract $ e_note $ convert_note linenum cmds lines
        extract (s, d, _inst, call, _, _) = (s, d, call)
    equal (f 1 [] []) (1/8, 4 - 1/8, "")
    -- stops at next note
    equal (f 8 [] [(16, M.Line (Just 1) 0 [])]) (1, 1, "")
    -- unless it's empty
    equal (f 8 [] [(16, M.Line Nothing 0 [])]) (1, 3, "")

    -- DelayRepeat
    equal (f 0 [M.DelayRepeat 3 0] []) (0, 4, "d 3/48t |")

test_convert_note_controls = do
    let f linenum cmds lines =
            extract $ e_note $ convert_note linenum cmds lines
        extract (s, d, _, _, _, controls) = (s, d, controls)
    let next = [(16, M.Line (Just 1) 0 [])]
        cmd c = M.Line Nothing 0 [c]
    equal (f 0 [M.Volume 0.5] next) (0, 2, [("dyn", [(0, 0, "`0x`80")])])
    equal (f 0 [] ((8, cmd (M.Volume 0.25)) : next
            ++ [(64, cmd (M.Volume 0.75))]))
        (0, 2, [("dyn", [(0, 0, "`0x`80"), (1, 0, "`0x`40")])])

    let e_controls (_, _, a) = a
    let up = M.VolumeSlide 1
    equal (e_controls $ f 0 [up] []) [("dyn", [(0, 1/8, "from=`0x`80 | u 1")])]
    -- volume + crescendo
    equal (e_controls $ f 0 [M.Volume 0, up] [])
        [("dyn", [(0, 1/8, "from=`0x`00 | u 1")])]
    -- grouping
    equal (e_controls $ f 0 [up] [(1, cmd up), (2, cmd (M.VolumeSlide 2))])
        [ ( "dyn"
          , [ (0, 1/8, "from=`0x`80 | u 1")
            , (1/8, 1/8, "u 1")
            , (2/8, 1/8, "u 2")
            ]
          )
        ]
    equal (e_controls $ f 0 [] [(1, cmd up), (2, cmd up)])
        [("dyn", [(0, 0, "`0x`80"), (1/8, 2/8, "u 1")])]
    equal (e_controls $ f 0 [] [(1, cmd up), (3, cmd up)])
        [("dyn", [(0, 0, "`0x`80"), (1/8, 1/8, "u 1"), (3/8, 1/8, "u 1")])]

    -- pprint (Mod.merge_notes $ (:[]) $
    --     convert_note 0 [] [(1, cmd up), (3, cmd up), (5, cmd up)])

convert_note :: Mod.LineNum -> [M.Command] -> [(Mod.LineNum, M.Line)]
    -> Mod.Note
convert_note linenum cmds lines =
    Mod.convert_note 32 tempo inst linenum 60 cmds lines


e_note :: Mod.Note -> (TrackTime, TrackTime, Text, Text, Pitch.NoteNumber,
    [(Text, [(TrackTime, TrackTime, Text)])])
e_note (Mod.Note start dur inst call pitch controls) =
    ( start, dur
    , pretty (M._instrument_name inst)
    , call
    , pitch
    , Map.toList $ map e_event <$> controls
    )

e_event :: Event.Event -> (TrackTime, TrackTime, Text)
e_event e = (Event.start e, Event.duration e, Event.text e)
