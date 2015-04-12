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
            <> " | norot-arrival=f | initial=t"
        extract e = (DeriveTest.e_note e, Score.event_instrument e)
    equal (run [(2, -2, "norot -- 3a")])
        ([((0, 1, "3a"), pasang), ((1, 1, "3b"), pasang),
            ((2, 1, "3a"), pasang)], [])
    equal (run [(2, -2, "norot -- 4f")])
        ([((0, 1, "4f"), pasang), ((1, 1, "4e"), pasang),
            ((2, 1, "4f"), pasang)], [])
    equal (run [(2, -2, "norot _ diamond -- 4c")])
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

    equal (derive DeriveTest.e_note title [(4, -2, "norot -- 3a")])
        ([(2, 1, "3a"), (3, 1, "3b"), (4, 1, "3a")], [])
    equal (derive Score.event_flags title [(4, -2, "norot -- 3a")])
        ([mempty, mempty, Flags.infer_duration <> Flags.strong], [])

    -- Flags aren't messed up from starting at 0.  Also, non-negative duration
    -- is the same as negative.
    equal (derive Score.event_flags title [(0, 4, "norot -- 3a")])
        ([mempty, mempty, mempty, mempty,
            Flags.infer_duration <> Flags.strong], [])

test_norot_arrival = do
    let run = e_pattern 0 . derive_kotekan title
        title = " | norot-dur=1 | infer-duration 2"
    equal (run [(4, 4, "norot t -- 4c")]) ([(pasang, "-11212121")], [])
    -- The second half of the first call is cancelled out.
    equal (run [(0, 8, "initial=t | norot f -- 4c"), (8, 4, "norot t -- 4d")])
        ([(pasang, "1212122323232")], [])

    -- Ensure that a 0 dur event at the end of the block still has an arrival.
    -- This actually tests that (ts e) works at the end of the block.
    let run_block notes = e_pattern 0 $
            DeriveTest.derive_blocks [("b1=ruler -- import bali.gangsa",
                UiTest.note_spec (inst_title <> title, notes, []))]
    equal (run_block [(0, 8, "norot f -- 4c"), (8, 0, "norot t -- 4d")])
        ([(pasang, "-21212232")], [])

    -- First note is cancelled out.
    equal (run [(0, 2, "initial=t | norot f -- 4c"), (2, 2, "norot f -- 4d")])
        ([(pasang, "12132")], [])

test_gender_norot = do
    let run = e_pattern 0 . derive_kotekan ""
    equal (run [(0, 4, "initial=t | gnorot -- 4e")])
        ([(polos, "32123"), (sangsih, "34343")], [])

test_kotekan_irregular = do
    let run kotekan = e_pattern 0 . derive_kotekan (ngotek kotekan)
    equal (run True [(0, 8, "k_\\ -- 4c")])
        ([(polos, "--11-1-21"), (sangsih, "--44-43-4")], [])
    equal (run False [(0, 8, "k_\\ -- 4c")])
        ([(pasang, "--11-1321")], [])

test_kotekan_consecutive = do
    -- Consecutive kotekan calls don't result in doubled notes.  Originally
    -- this involved cancelling notes, but since kotekan now omits the first
    -- note it happens naturally.
    let run = e_pattern 0 . derive_kotekan (" | infer-duration" <> ngotek True)
    equal (run [(0, 8, "initial=t | k k-12-1-21 -- 4c"),
            (8, 8, "k k-12-1-21 -- 4c")])
        ( [ (polos,     "1-12-1-21-12-1-21")
          , (sangsih,   "-3-23-32-3-23-32-")
          ]
        , []
        )
    equal (run [(0, 8, "initial=t | k k-12-1-21 -- 4c"),
            (8, 8, "k k-12-1-21 -- 4d")])
        ( [ (polos,     "1-12-1-21-23-2-32")
          , (sangsih,   "-3-23-32-4-34-43-")
          ]
        , []
        )
    -- Majalan up and down.
    equal (run [(0, 8, "initial=t | k k2-12-12- pat -- 4e"),
            (8, 8, "k k21-21-21 pat -- 4c")])
        ( [ (polos,   "-2-12-12-21-21-21")
          , (sangsih, "3-34-34-3-43-43-4")
          ]
        , []
        )

test_kotekan_cancel = do
    -- The final note of the kotekan is cancelled.
    let run = derive_kotekan (" | infer-duration | unison" <> ngotek True)
    -- Because there's no sangsih at that point, only the polos is replaced.
    -- I would have to have >polos replace >pasang, and after that apply unison
    -- or whatever.
    equal (e_pattern 0 $ run [(0, 8, "k// -- 4e"), (8, 8, "4f")])
        ([(polos, "--3-23-23"), (sangsih, "-2-12-123")], [])
    pprint
        (e_pasang DeriveTest.e_note $ run [(0, 8, "k// -- 4e"), (8, 8, "4f")])

test_kotekan_continuation = do
    -- Kotekan followed by normal notes works "as expected".
    let run postproc = derive_kotekan
            (" | infer-duration" <> ngotek True <> " | " <> postproc)
    equal (e_pattern 0 $ run "unison"
            [(0, 8, "final=f | k// -- 4e"), (8, 2, "4f")])
        ([(polos, "--3-23-24"), (sangsih, "-2-12-124")], [])
    let extract e =
            (Score.event_start e, Score.event_duration e, e_digit e <> m)
            where m = if DeriveTest.e_attributes e == "+mute" then "+" else ""
    equal (e_by_inst extract $ run "noltol"
            [(0, 8, "final=f | k// -- 4e"), (8, 2, "kempyung | -- 4f")])
        ([ (polos,
            [ (2, 1, "3"), (3, 1, "3+"), (4, 1, "2"), (5, 1, "3")
            , (6, 1, "3+"), (7, 1, "2"), (8, 2, "4")
            ])
         , (sangsih,
            [ (1, 1, "2"), (2, 1, "2+"), (3, 1, "1"), (4, 1, "2"), (5, 1, "2+")
            , (6, 1, "1"), (7, 1, "2"), (8, 2, "7")
            ])
         ], [])

test_kotekan_infer_duration = do
    let run = derive_polos DeriveTest.e_note
            (" | infer-duration 2 | unison" <> ngotek True)
    -- The last note of the kotekan gets an inferred duration to fill the gap.
    equal (run [(0, 4, "k k2-21 -- 4c"), (8, 2, "4d")])
        ([(1, 1, "4d"), (3, 1, "4d"), (4, 4, "4c"), (8, 2, "4d")], [])

test_kotekan_regular = do
    let run kotekan = e_pattern 2 . derive_kotekan (ngotek kotekan)
    -- Start at 2 to avoid accidentally working from 0.
    equal (run True [(2, 8, "initial=t | k k-12-1-21 -- 4c")])
        ([(polos, "1-12-1-21"), (sangsih, "-3-23-32-")], [])
    equal (run True [(2, 8, "k k-12-1-21 -- 4c")])
        ([(polos, "--12-1-21"), (sangsih, "-3-23-32-")], [])
    equal (run False [(2, 8, "k k-12-1-21 -- 4c")])
        ([(pasang, "-31231321")], [])
    equal (run True [(2, 8, "k k-12-1-21 pat -- 4c")])
        ([(polos, "--12-1-21"), (sangsih, "-34-343-4")], [])
    equal (run False [(2, 8, "k k-12-1-21 pat -- 4c")])
        ([(polos, "-31231321"), (sangsih, "-34234324")], [])
    equal (run True [(2, 8, "k k-12-1-21 _ d -- 4e")])
        ([(polos, "--34-3-43"), (sangsih, "-23-232-3")], [])
    equal (run False [(2, 8, "k k-12-1-21 _ d -- 4e")])
        ([(pasang, "-23423243")], [])
    equal (run True [(2, 8, "k k-12-1-21 pat d -- 4e")])
        ([(polos, "--34-3-43"), (sangsih, "-2-12-21-")], [])
    equal (run False [(2, 8, "k k-12-1-21 pat d -- 4e")])
        ([(polos, "-23423243"), (sangsih, "-23123213")], [])

test_kotekan_regular_jalan = do
    -- k// and k\\ work as expected.
    let run kotekan = e_pattern 0 . derive_kotekan (ngotek kotekan)
    equal (run True [(0, 8, "k// -- 4e")])
        ([(polos, "--3-23-23"), (sangsih, "-2-12-12-")], [])
    equal (run True [(0, 8, "k\\\\ -- 4c")])
        ([(polos, "--1-21-21"), (sangsih, "-2-32-32-")], [])

test_kotekan_strange_length = do
    let run start kotekan = e_pattern start . derive_kotekan (ngotek kotekan)
    equal (run 0 True [(0, 8, "k k-121 -- 4c")])
        ([(polos, "--121-121"), (sangsih, "-3-2-3-2-")], [])
    strings_like (snd $ run 0 True [(0, 12, "k k-12-21 -- 4c")])
        ["not a multiple of 4"]
    equal (run 0 True [(0, 12, "k '-12-21' -- 4c")])
        ([(polos, "--12-21-12-21"), (sangsih, "-3-232-3-232-")], [])

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
    let run title = derive extract (inst_title <> " | " <> title)
            -- (inst_title <> " | noltol " <> arg <> postproc)
        extract e =
            (Score.event_start e, DeriveTest.e_inst e,
                DeriveTest.e_pitch e <> m)
            where m = if DeriveTest.e_attributes e == "+mute" then "+" else ""
    let notes = [(0, 1, "n >i1 -- 4c"), (1, 1, "n >i2 -- 4d"),
            (2, 1, "n >i1 -- 4e")]
    -- 1s of free time between i1
    equal (run "noltol 1.1" notes)
        ([(0, "i1", "4c"), (1, "i2", "4d"), (2, "i1", "4e")], [])
    equal (run "noltol 1" notes)
        ([ (0, "i1", "4c"), (1, "i1", "4c+")
         , (1, "i2", "4d"), (2, "i1", "4e")
         ], [])

    equal (run "kotekan-dur=2 | noltol 1" [(0, 8, "4c"), (10, 4, "4d")])
        ([(0, "i3", "4c"), (10, "i3", "4d")], [])
    equal (run "kotekan-dur=2 | noltol 1" [(0, 2, "4c"), (4, 4, "4d")])
        ([(0, "i3", "4c"), (2, "i3", "4c+"), (4, "i3", "4d")], [])
    equal (run "noltol 1 | nyog"
            [(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e"), (3, 1, "4f")])
        ([ (0, "i1", "4c"), (1, "i1", "4c+")
         , (1, "i2", "4d"), (2, "i2", "4d+")
         , (2, "i1", "4e")
         , (3, "i2", "4f")
         ], [])

ngotek :: Bool -> String
ngotek b = " | kotekan=" <> if b then "2" else "1"

derive_pasang :: (Score.Event -> a) -> String -> [UiTest.EventSpec]
    -> (([a], [a]), [String])
derive_pasang extract title notes =
    e_pasang extract $ derive_kotekan title notes

derive_polos :: (Score.Event -> a) -> String -> [UiTest.EventSpec]
    -> ([a], [String])
derive_polos extract title = first fst . derive_pasang extract title

derive_kotekan :: String -> [UiTest.EventSpec] -> Derive.Result
derive_kotekan title notes = derive_tracks $
    UiTest.note_spec (inst_title <> title, notes, [])

e_by_inst :: (Score.Event -> a) -> Derive.Result
    -> ([(Score.Instrument, [a])], [String])
e_by_inst extract = first Seq.group_fst
    . DeriveTest.extract (\e -> (Score.event_instrument e, extract e))

e_pattern :: RealTime -> Derive.Result
    -> ([(Score.Instrument, String)], [String])
e_pattern start = first extract . e_by_inst DeriveTest.e_start_note
    where
    extract :: [(Score.Instrument, [(RealTime, String)])]
        -> [(Score.Instrument, String)]
    extract inst_notes = map (second (as_pattern start end)) inst_notes
        where
        end = fromMaybe 0 $ Seq.maximum $ map fst $ concatMap snd inst_notes

as_pattern :: RealTime -> RealTime -> [(RealTime, String)] -> String
as_pattern start end = concat . map format . collect start
    where
    collect at [] = replicate (round (end - at) + 1) []
    -- for each number, collect everything below
    collect at xs = map snd pre : collect (at+1) post
        where (pre, post) = span ((<=at) . fst) xs
    format [] = "-"
    format ns = Seq.join "+" (map pitch_digit ns)

e_digit :: Score.Event -> String
e_digit = pitch_digit . DeriveTest.e_pitch

pitch_digit :: String -> String
pitch_digit p = case p of
    "4c" -> "1"; "4d" -> "2"; "4e" -> "3"; "4f" -> "4"
    "4g" -> "5"; "4a" -> "6"; "4b" -> "7"
    _ -> "unknown pitch: " <> show p

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
