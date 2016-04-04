-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Prelude.Equal_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import Global
import Types


test_equal = do
    -- Test the '=' call, but also test the special parsing Derive.Note deriver
    -- eval in general.
    let run title evts = DeriveTest.extract e_instrument $
            DeriveTest.derive_tracks "" [(title, evts)]
    strings_like (snd $ run ">" [(0, 1, "%c = >i |")])
        ["expected *, but got Instrument"]
    -- log stack should be at the track level
    let (evts, logs) = run "> | inst = inst" [(0, 1, "")]
    equal evts []
    strings_like logs ["expected Instrument"]

    -- only the event with the error is omitted
    let (evts, logs) = run ">" [(0, 1, "inst = inst |"), (1, 1, "")]
    equal evts [(1, "")]
    strings_like logs ["expected Instrument"]

    equal (run ">i" [(0, 1, ""), (1, 1, "inst = >i2 |"), (2, 1, "n >i1 |")])
        ([(0, "i"), (1, "i2"), (2, "i1")], [])
    equal (run ">" [(0, 1, "inst = >nonexistent |")])
        ([], ["Error: no instrument found for >nonexistent"])

    -- Unset a val.
    equal (run ">i" [(0, 1, ""), (1, 1, "inst = _ |")])
        ([(0, "i"), (1, "")], [])

test_equal_modify = do
    let run evts = DeriveTest.extract (DeriveTest.e_control "c") $
            DeriveTest.derive_tracks "" [(">", evts)]
    strings_like (snd $ run [(0, 1, "c = .5 add |")])
        ["merge is only supported when"]
    equal (run [(0, 1, "%c = .5 | %c = .5 add |")]) ([[(0, 1)]], [])

test_equal_inst = do
    let run with_ui title track = DeriveTest.extract DeriveTest.e_instrument $
            DeriveTest.derive_tracks_setup with_ui title
                [(track, [(0, 1, "")])]
    strings_like (snd $ run mempty ">new = hi" ">new")
        ["expected an instrument"]
    strings_like (snd $ run mempty ">new = >nonexistent" ">new")
        ["instrument alias destination doesn't exist"]
    equal (run mempty ">new = >i1" ">new") (["i1"], [])
    equal (run mempty ">new = >i1 | >newer = >new" ">newer") (["i1"], [])

test_equal_note_transformer = do
    let run events = DeriveTest.extract e_instrument $
            DeriveTest.derive_tracks_linear ""
                [ (">", events)
                , (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
                ]
    equal (run []) ([(0, ""), (1, ""), (2, "")], [])
    equal (run [(0, 2, "inst = >i")]) ([(0, "i"), (1, "i"), (2, "")], [])
    equal (run [(0, 3, "inst = >i")]) ([(0, "i"), (1, "i"), (2, "i")], [])
    equal (run [(0, 1, "inst = >i1"), (1, 1, "inst = >i2")])
        ([(0, "i1"), (1, "i2"), (2, "")], [])

test_equal_call = do
    let run = DeriveTest.extract DeriveTest.e_note . DeriveTest.derive_tracks ""
    -- Rebind a note call.
    equal (run [(">", [(0, 1, "^zzz = n | zzz")])]) ([(0, 1, "?")], [])
    -- Bind to a instrument transformer, as called by note-track.
    let run_env = DeriveTest.extract extract
            . DeriveTest.derive_tracks ">>i1 = \"(v=1)"
        extract e = (DeriveTest.e_instrument e, DeriveTest.e_environ "v" e)
    equal (run_env [(">i1", [(0, 1, "")])]) ([("i1", Just "1")], [])

    equal (run [("> | *zzz = set", [(0, 1, "")]), ("*", [(0, 0, "zzz (4c)")])])
        ([(0, 1, "4c")], [])
    equal (run [("> | *4 = set", [(0, 1, "")]), ("*", [(0, 0, "4 (4c)")])])
        ([(0, 1, "4c")], [])
    equal (run [("> | -zzz = e | p = (4c)", [(0, 1, "")]),
            ("*", [(0, 0, "set (zzz p)")])])
        ([(0, 1, "4c")], [])

    -- RHS that's not a symbol.
    let run2 = DeriveTest.extract DeriveTest.e_attributes
            . DeriveTest.derive_tracks ""
    equal (run2 [("> | ^左 = +left", [(0, 1, "左")])]) (["+left"], [])

test_equal_quoted = do
    let run title note = DeriveTest.extract extract $
            DeriveTest.derive_tracks "" [(title, [(0, 1, note)])]
        extract e = (DeriveTest.e_pitch e, DeriveTest.e_attributes e)
    -- Just make sure assigning to # works.
    equal (run ">" "# = (4c) |") ([("4c", "+")], [])
    -- Bind val call to (4c).
    equal (run "> | -zzz = \"(4c)" "# = (zzz) |") ([("4c", "+")], [])
    -- Bind argument to 'n'.
    equal (run "> | ^zzz = \"(n +a)" "zzz") ([("?", "+a")], [])
    -- Recursive binding doesn't work.  To fix this, I should look up the
    -- calls in the quoted text.
    let (vals, logs) = run "> | ^n = \"(n +a)" "n"
    equal vals []
    strings_like logs ["too many arguments"]
    equal (run "> | ^z = \"(# = (4c) |)" "z") ([("4c", "+")], [])

test_default_merge = do
    let run title = DeriveTest.extract (DeriveTest.e_control "c") $
            DeriveTest.derive_tracks title
                [ ("c", [(0, 0, ".5")])
                , ("c", [(0, 0, ".5")])
                , (">", [(0, 1, "")])
                ]
    equal (run "") ([[(0, 0.25)]], [])
    equal (run "default-merge set c") ([[(0, 0.5)]], [])
    equal (run "default-merge add c") ([[(0, 1)]], [])
    -- 'mul' is the default for most controls.
    equal (run "default-merge _ c") ([[(0, 0.25)]], [])

e_instrument :: Score.Event -> (RealTime, Text)
e_instrument e = (Score.event_start e, DeriveTest.e_instrument e)
