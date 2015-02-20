-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Bali.Gangsa_test where
import qualified Data.Map as Map

import qualified Util.Lens as Lens
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Environ as Environ
import qualified Derive.Flags as Flags
import qualified Derive.RestrictedEnviron as RestrictedEnviron
import qualified Derive.Score as Score

import qualified Perform.Midi.Instrument as Instrument
import Global
import Types


test_norot = do
    let run = derive extract title
        title = inst_title <> " | inst-top = (pitch (4f)) | norot-dur=1"
            <> " | norot-arrival=f"
        extract e = (DeriveTest.e_note e, Score.event_instrument e)
    equal (run [(2, -2, "norot -- 3a")])
        ([((0, 1, "3a"), pasang), ((1, 1, "3b"), pasang),
            ((2, 1, "3a"), pasang)], [])
    equal (run [(2, -2, "norot -- 4f")])
        ([((0, 1, "4f"), pasang), ((1, 1, "4e"), pasang),
            ((2, 1, "4f"), pasang)], [])
    equal (run [(2, -2, "norot _ _ diamond -- 4c")])
        ([ ((0, 1, "4c"), polos), ((0, 1, "4c"), sangsih)
         , ((1, 1, "4d"), polos), ((1, 1, "3b"), sangsih)
         , ((2, 1, "4c"), polos), ((2, 1, "4c"), sangsih)
         ], [])
    -- Under threshold, split sangsih and polos.
    equal (run [(2, -2, "kotekan = 2 | norot -- 3a")])
        ([((0, 1, "3a"), polos), ((1, 1, "3b"), sangsih),
            ((2, 1, "3a"), polos)], [])

    -- >norot is a Once pattern.
    equal (run [(8, -8, "kotekan = 2 | >norot -- 3a")])
        ([ ((5, 1, "3a"), polos), ((5, 1, "3a"), sangsih)
         , ((6, 1, "3a"), polos), ((6, 1, "3a"), sangsih)
         , ((7, 1, "3b"), sangsih)
         , ((8, 1, "3a"), polos)
         ], [])

    equal (derive DeriveTest.e_note title [(4, -2, "norot f -- 3a")])
        ([(2, 1, "3a"), (3, 1, "3b"), (4, 1, "3a")], [])

    equal (derive Score.event_flags title [(4, -2, "norot -- 3a")])
        ([Flags.can_cancel, mempty, Flags.infer_duration], [])
    -- Flags aren't messed up from starting at 0.  Also, non-negative duration
    -- is the same as negative.
    equal (derive Score.event_flags title [(0, 4, "norot -- 3a")])
        ([Flags.can_cancel, mempty, mempty, mempty, Flags.infer_duration], [])

test_norot_arrival = do
    let run extract = derive extract title
        title = inst_title <> " | norot-dur=1 | infer-duration 2"
    let a = "3a"; b = "3b"; c = "4c"
        ts start = Seq.range_ start 1
    equal (run DeriveTest.e_start_note [(4, 4, "norot t -- 3a")])
        ([(1, a), (2, a), (3, b), (4, a), (5, b), (6, a), (7, b), (8, a)], [])
    -- The second half of the first call is cancelled out.
    equal (run DeriveTest.e_start_note
            [(0, 8, "norot f -- 3a"), (8, 4, "norot t -- 3b")])
        (zip (ts 0) [a, b, a, b, a, b, b, c, b, c, b, c, b], [])

    -- Ensure that a 0 dur event at the end of the block still has an arrival.
    -- This actually tests that (ts e) works at the end of the block.
    let run_block extract notes = DeriveTest.extract extract $
            DeriveTest.derive_blocks [("b1=ruler -- import bali.gangsa",
                UiTest.note_spec (title, notes, []))]
    equal (run_block DeriveTest.e_start_note
            [ (0, 8, "norot f \"(ts e) -- 3a")
            , (8, 0, "norot t \"(ts e) -- 3b")
            ])
        (zip (ts 0) [a, b, a, b, a, b, b, c, b], [])

test_norot_infer_duration = do
    let run = derive DeriveTest.e_pitch (inst_title <> " | infer-duration")
    -- First note is cancelled out.
    equal (run [(0, 2, "norot f 1 -- 3a"), (2, 2, "norot f 1 -- 3c")])
        (["3a", "3b", "3a", "3d", "3c"], [])

    let run = derive_pasang extract (" | noltol | infer-duration | kotekan=2")
        extract e = (DeriveTest.e_pitch e, DeriveTest.e_attributes e)
    let mute = "+mute"
    equal (run [(0, 2, "norot f 1 -- 3a"), (2, 2, "norot f 1 -- 3c")])
        (([("3a", "+"), ("3a", mute), ("3a", "+"), ("3a", mute), ("3c", "+")]
        , [("3b", "+"), ("3b", mute), ("3d", "+")])
        , []
        )

test_gender_norot = do
    let run = e_pattern 0 . derive_kotekan ""
    equal (run [(0, 4, "gnorot 1 -- 4e")])
        ([(polos, "32123"), (sangsih, "34343")], [])

test_kotekan_irregular = do
    let run kotekan = e_pattern 0 . derive_kotekan (ngotek kotekan)
    equal (run True [(0, 8, "k_\\ -- 4c")])
        ([(polos, "1-11-1-21"), (sangsih, "4-44-43-4")], [])
    equal (run False [(0, 8, "k_\\ -- 4c")])
        ([(pasang, "1-11-1321")], [])

test_kotekan_cancel = do
    -- The end of the previous kotekan will cancel the first note of the
    -- next one.
    let run extract title kotekan =
            e_by_inst extract . derive_kotekan (title <> ngotek kotekan)
    let notes = [(0, 8, "-12-1-21 -- 4c"), (8, 8, "-12-1-21 -- 4c")]
    equal (first head $
            run DeriveTest.e_start_note " | infer-duration" True notes)
        ( (polos,
            [ (0, "4c"), (2, "4c"), (3, "4d"), (5, "4c"), (7, "4d")
            , (8, "4c"), (10, "4c"), (11, "4d"), (13, "4c"), (15, "4d")
            , (16, "4c")
            ])
        , []
        )
    let notes = [(0, 8, "-12-1-21 -- 4c"), (8, 8, "-12-1-21 -- 4d")]
    equal (first head $
            run DeriveTest.e_start_note " | infer-duration" True notes)
        ( (polos,
            [ (0, "4c"), (2, "4c"), (3, "4d"), (5, "4c"), (7, "4d")
            , (8, "4c"), (10, "4d"), (11, "4e"), (13, "4d"), (15, "4e")
            , (16, "4d")
            ])
        , []
        )

test_kotekan_regular = do
    let run kotekan = e_pattern 2 . derive_kotekan (ngotek kotekan)
    equal (run True [(2, 8, "-12-1-21 -- 4c")])
        ([(polos, "1-12-1-21"), (sangsih, "-3-23-32")], [])
    equal (run False [(2, 8, "-12-1-21 -- 4c")])
        ([(pasang, "131231321")], [])
    equal (run True [(2, 8, "-12-1-21 _ pat -- 4c")])
        ([(polos, "1-12-1-21"), (sangsih, "434-343-4")], [])
    equal (run False [(2, 8, "-12-1-21 _ pat -- 4c")])
        ([(polos, "131231321"), (sangsih, "434234324")], [])
    equal (run True [(2, 8, "-12-1-21 d -- 4e")])
        ([(polos, "3-34-3-43"), (sangsih, "323-232-3")], [])
    equal (run False [(2, 8, "-12-1-21 d -- 4e")])
        ([(pasang, "323423243")], [])
    equal (run True [(2, 8, "-12-1-21 d pat -- 4e")])
        ([(polos, "3-34-3-43"), (sangsih, "-2-12-21")], [])
    equal (run False [(2, 8, "-12-1-21 d pat -- 4e")])
        ([(polos, "323423243"), (sangsih, "323123213")], [])

test_unison = do
    let run = DeriveTest.extract extract
            . DeriveTest.derive_tracks_with_ui id config_inst title
            . UiTest.note_track
        title = "import bali.gangsa | inst = >" <> inst_title
            <> " | scale=wayang | unison"
        extract e = (pretty $ Score.event_instrument e, Score.initial_nn e)
        config_inst = set DeriveTest.i1 Environ.umbang
            . set DeriveTest.i2 Environ.isep
        set inst tuning = modify_instrument inst $
            Instrument.cenviron #= RestrictedEnviron.make
                [(Environ.tuning, RestrictedEnviron.to_val tuning)]
    equal (run [(0, 1, "4i")]) ([(">i1", Just 62.95), (">i2", Just 62.5)], [])

modify_instrument :: Score.Instrument
    -> (Instrument.Config -> Instrument.Config) -> State.State -> State.State
modify_instrument inst modify state =
    case Map.lookup inst $ State.config#State.midi #$ state of
        Nothing -> state
        Just config ->
            (State.config#State.midi#Lens.map inst #= Just (modify config))
            state

test_kempyung = do
    let run title = derive extract (inst_title <> title <> " | kempyung")
        extract e = (Score.event_start e, Score.initial_note e)
        notes = [(0, 1, "4c"), (1, 1, "4d")]
    equal (run "" notes)
        ([(0, Just "4c"), (0, Just "4f"), (1, Just "4d"), (1, Just "4g")], [])
    equal (run " | inst-top = (pitch (4f))" notes)
        ([(0, Just "4c"), (0, Just "4f"), (1, Just "4d"), (1, Just "4d")], [])

test_nyogcag = do
    let run = derive extract (inst_title <> " | nyog")
        extract e = (Score.event_start e, DeriveTest.e_inst e)
    let notes = [(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e")]
    equal (run notes) ([(0, "i1"), (1, "i2"), (2, "i1")], [])

test_noltol = do
    let run arg = derive extract (" | noltol " <> arg)
        extract e = (Score.event_start e, DeriveTest.e_inst e,
            DeriveTest.e_pitch e, DeriveTest.e_attributes e == "+mute")
    let notes = [(0, 1, "n >i1 -- 4c"), (1, 1, "n >i2 -- 4d"),
            (2, 1, "n >i1 -- 4e")]
    -- 1s of free time between i1
    equal (run "1.1" notes)
        ([(0, "i1", "4c", False), (1, "i2", "4d", False),
            (2, "i1", "4e", False)], [])
    equal (run "1" notes)
        ([ (0, "i1", "4c", False), (1, "i1", "4c", True)
         , (1, "i2", "4d", False), (2, "i1", "4e", False)
         ], [])

    let run2 postproc = derive extract (inst_title <> " | noltol 1" <> postproc)
    equal (run2 " | nyog"
            [(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e"), (3, 1, "4f")])
        ([ (0, "i1", "4c", False), (1, "i1", "4c", True)
         , (1, "i2", "4d", False), (2, "i2", "4d", True)
         , (2, "i1", "4e", False)
         , (3, "i2", "4f", False)
         ], [])

ngotek :: Bool -> String
ngotek b = " | kotekan=" <> if b then "2" else "1"

derive_pasang :: (Score.Event -> a) -> String -> [UiTest.EventSpec]
    -> (([a], [a]), [String])
derive_pasang extract title notes =
    e_pasang extract $ derive_kotekan title notes

derive_kotekan :: String -> [UiTest.EventSpec] -> Derive.Result
derive_kotekan title notes = derive_tracks $
    UiTest.note_spec (inst_title <> title, notes, [])

e_by_inst :: (Score.Event -> a) -> Derive.Result
    -> ([(Score.Instrument, [a])], [String])
e_by_inst extract =
    first Seq.group_fst
        . DeriveTest.extract (\e -> (Score.event_instrument e, extract e))

e_pattern :: RealTime -> Derive.Result
    -> ([(Score.Instrument, String)], [String])
e_pattern start =
    first (map (second (as_pattern start)))
        . e_by_inst DeriveTest.e_start_note
    where
    as_pattern _ [] = ""
    as_pattern at ns@((t, pitch) : rest) = case compare at t of
        GT -> as_pattern at rest
        EQ -> to_digit pitch : as_pattern (at+1) rest
        LT -> '-' : as_pattern (at+1) ns
    to_digit p = case p of
        "4c" -> '1'
        "4d" -> '2'
        "4e" -> '3'
        "4f" -> '4'
        _ -> error $ "unknown pitch: " <> show p

e_pasang :: (Score.Event -> a) -> Derive.Result -> (([a], [a]), [String])
e_pasang extract = first group_inst
    . DeriveTest.extract (\e -> (Score.event_instrument e, extract e))
    where
    group_inst ns = ([n | (inst, n) <- ns, inst == polos],
        [n | (inst, n) <- ns, inst == sangsih])

derive :: (Score.Event -> a) -> String -> [UiTest.EventSpec] -> ([a], [String])
derive extract title notes = DeriveTest.extract extract $ derive_tracks $
    UiTest.note_spec (title, notes, [])

inst_title :: String
inst_title = "i3 | inst-polos = >i1 | inst-sangsih = >i2"

polos :: Score.Instrument
polos = Score.Instrument "i1"

sangsih :: Score.Instrument
sangsih = Score.Instrument "i2"

pasang :: Score.Instrument
pasang = Score.Instrument "i3"

derive_tracks :: [UiTest.TrackSpec] -> Derive.Result
derive_tracks = DeriveTest.derive_tracks "import bali.gangsa"
