-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Load.Mod_test where
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

import qualified Cmd.Load.Mod as Mod
import qualified Cmd.Load.ModT as ModT
import qualified Derive.ScoreT as ScoreT
import qualified Perform.Pitch as Pitch
import qualified Ui.Event as Event
import qualified Ui.Skeleton as Skeleton
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


convert :: ModT.Module
    -> Either Text [(BlockId, (UiTest.BlockSpec, [Skeleton.Edge]))]
convert = (bimap pretty UiTest.dumpBlocks) . Mod.convert UiTest.test_ns

test_convert :: Test
test_convert = do
    let f = fmap (map (fst . snd)) . convert . mkmod ["s1", "s2"]
    rightEqual
        (f
            [ (8, [ [(0, (Just 60, 1, []))] ])
            , (8, [ [(1, (Just 62, 2, [ModT.Volume 0.5]))] ])
            ])
        [ ( "b1", [(">s1", [(0, 1, "")]), ("*", [(0, 0, "4c")])]
          )
        , ( "b2"
          , [ (">s2", [(1/8, 7/8, "")]), ("*", [(1/8, 0, "4d")])
            , ("dyn", [(1/8, 0, "`0x`80")])
            ]
          )
        , ("score", [(">", [(0, 1, "b1"), (1, 1, "b2")])])
        ]
    -- CutBlock drops notes at and after it, but after the line its own.
    rightEqual
        (f
            [ ( 8
              , [ [ (0, (Just 60, 1, []))
                  , (2, (Nothing, 1, [ModT.VolumeSlide 1]))
                  , (6, (Nothing, 1, [ModT.VolumeSlide 1]))
                  ]
                , [(3, (Just 72, 1, [ModT.CutBlock])), (6, (Just 80, 1, []))]
                ]
              )
            ])
        [ ( "b1"
          , [ (">s1", [(0, 0.5, "")]), ("*", [(0, 0, "4c")])
            , ("dyn", [(0, 0, "`0x`ff"), (2/8, 1/8, "u 1")])
            , (">s1", [(3/8, 1/8, "")]), ("*", [(3/8, 0, "5c")])
            ]
          )
        , ("score", [(">", [(0, 0.5, "b1")])])
        ]

test_make_skeleton :: Test
test_make_skeleton = do
    let f = Skeleton.flatten . Mod.makeSkeleton
    equal (f []) []
    equal (f [['a'], ['b', 'c']]) [(2, 3)]
    equal (f [['a', 'b'], ['c', 'd', 'e']]) [(1, 2), (3, 4), (4, 5)]

test_convert_note :: Test
test_convert_note = do
    let f linenum cmds lines =
            extract $ eNote $ convertNote linenum cmds lines
        extract (s, d, _inst, call, _, _) = (s, d, call)
    equal (f 1 [] []) (1/8, 4 - 1/8, "")
    -- stops at next note
    equal (f 8 [] [(16, ModT.Line (Just 1) 0 [])]) (1, 1, "")
    -- unless it's empty
    equal (f 8 [] [(16, ModT.Line Nothing 0 [])]) (1, 3, "")
    -- DelayRepeat
    equal (f 0 [ModT.DelayRepeat 3 0] []) (0, 4, "d 3/48t |")

test_convert_note_controls :: Test
test_convert_note_controls = do
    let f linenum cmds futureLines =
            extract $ eNote $ convertNote linenum cmds futureLines
        extract (s, d, _, _, _, controls) = (s, d, controls)
    let next = [(16, ModT.Line (Just 1) 0 [])]
        cmd c = ModT.Line Nothing 0 [c]

    equal (f 0 [ModT.Volume 0.5] next) (0, 2, [("dyn", [(0, 0, "`0x`80")])])
    equal (f 0 [] ((8, cmd (ModT.Volume 0.25)) : next
            ++ [(64, cmd (ModT.Volume 0.75))]))
        (0, 2, [("dyn", [(0, 0, "`0x`80"), (1, 0, "`0x`40")])])

    let eControls (_, _, a) = a
    let up = ModT.VolumeSlide
        up1 = up 1
    -- volume + crescendo
    equal (eControls $ f 0 [up1] []) [("dyn", [(0, 1/8, "uf `0x`80 1")])]
    equal (eControls $ f 0 [up1] [(1, cmd up1), (2, cmd up1)])
        [("dyn", [(0, 3/8, "uf `0x`80 1")])]
    equal (eControls $ f 0 [up1] [(1, cmd up1), (2, cmd (up 2))])
        [("dyn", [(0, 2/8, "uf `0x`80 1"), (2/8, 1/8, "u 2")])]

convertNote :: Mod.LineNum -> [ModT.Command] -> [(Mod.LineNum, ModT.Line)]
    -> Mod.Note
convertNote linenum cmds lines =
    Mod.convertNote 32 defaultTempo inst linenum 60 cmds lines
    where inst = ModT.Instrument (ScoreT.Instrument "inst") (Just 0.5)

-- * make

type TrackSpec = [(Int, (Maybe Pitch.NoteNumber, Int, [ModT.Command]))]

mkmod :: [ScoreT.Instrument] -> [(Int, [TrackSpec])] -> ModT.Module
mkmod insts blocks = ModT.Module
    { _instruments = IntMap.fromList $ zip [1..]
        [ModT.Instrument inst Nothing | inst <- insts]
    , _defaultTempo = defaultTempo
    , _blocks = map mkblock blocks
    , _blockOrder = Map.singleton "score" [0 .. length blocks - 1]
    }

mkblock :: (Int, [TrackSpec]) -> ModT.Block
mkblock (len, tracks) = ModT.Block len (map mktrack tracks)

mktrack :: TrackSpec -> ModT.Track
mktrack = ModT.makeTrack . map mkline

mkline :: (Int, (Maybe Pitch.NoteNumber, Int, [ModT.Command]))
    -> (Int, ModT.Line)
mkline (row, (pitch, inst, cmds)) = (row, ModT.Line pitch inst cmds)

defaultTempo :: ModT.Tempo
defaultTempo = ModT.Tempo 33 6

-- * extract

eNote :: Mod.Note -> (TrackTime, TrackTime, Text, Text, Pitch.NoteNumber,
    [(Text, [(TrackTime, TrackTime, Text)])])
eNote (Mod.Note start dur inst call pitch controls) =
    ( start, dur
    , pretty (ModT._instrumentName inst)
    , call
    , pitch
    , Map.toList $ map eEvent <$> controls
    )

eEvent :: Event.Event -> (TrackTime, TrackTime, Text)
eEvent e = (Event.start e, Event.duration e, Event.text e)
