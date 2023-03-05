-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.Equal_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import Global
import Types


test_equal :: Test
test_equal = do
    -- Test the '=' call, but also test the special parsing Derive.Note deriver
    -- eval in general.
    let run title evts = DeriveTest.extract e_instrument $
            DeriveTest.derive_tracks "" [(title, evts)]
    strings_like (snd $ run ">" [(0, 1, "%c = i |")])
        ["tried to assign to invalid symbol name"]
    -- log stack should be at the track level
    let (evts, logs) = run "> | inst = 42" [(0, 1, "")]
    equal evts []
    strings_like logs ["expected Str"]

    -- only the event with the error is omitted
    let (evts, logs) = run ">" [(0, 1, "inst = 42 |"), (1, 1, "")]
    equal evts [(1, "")]
    strings_like logs ["expected Str"]

    equal (run ">i" [(0, 1, ""), (1, 1, "inst = i2 |"), (2, 1, "inst=i1 |")])
        ([(0, "i"), (1, "i2"), (2, "i1")], [])
    equal (run ">" [(0, 1, "inst = nonexistent |")])
        ([], ["instrument 'nonexistent': no alloc for nonexistent"])
    equal (run "> | inst=i2" [(0, 1, "")]) ([(0, "i2")], [])

    -- Unset a val.
    equal (run ">i" [(0, 1, ""), (1, 1, "inst = _ |")])
        ([(0, "i"), (1, "")], [])

test_equal_merge_env :: Test
test_equal_merge_env = do
    let run evt = DeriveTest.extract (fromMaybe "" . DeriveTest.e_environ "k")
            (DeriveTest.derive_tracks "" [(">", [(0, 1, evt)])])
    equal (run "k=1 | k=+1 |") (["2"], [])
    strings_like (snd $ run "k=c | k=+1 |") ["can't be coerced to signal: c"]
    strings_like (snd $ run "k=1 | k=+c |")
        [ "merge is only supported for numeric types, tried to merge Str\
          \ with add"
        ]
    -- Types are preserved.
    equal (run "k=1c | k=+1 |") (["2c"], [])

test_equal_merge_control :: Test
test_equal_merge_control = do
    let run control evts =
            DeriveTest.extract (DeriveTest.e_control_constant control) $
            DeriveTest.derive_tracks "" [(">", evts)]
    equal (run "c" [(0, 1, "c = .5 | c = .5 add |")]) ([Just 1], [])
    -- Default is multiply.
    equal (run "c" [(0, 1, "c = .5 | c = .25 default |")])
        ([Just (0.5 * 0.25)], [])
    equal (run "c" [(0, 1, "c = .5 | c =+ .5 |")]) ([Just 1], [])
    equal (run "c" [(0, 1, "c = .5 | c =* .5 |")]) ([Just 0.25], [])
    equal (run "c" [(0, 1, "c=5 | c =- 2 |")]) ([Just 3], [])
    equal (run "c" [(0, 1, "c =- 2 |")]) ([Just (-2)], [])

    equal (run "c" [(0, 1, "c = 2 sub |")]) ([Just (-2)], [])
    -- =-1 is parsed as =- 1.
    equal (run "c" [(0, 1, "c = 2 | c=-1 |")]) ([Just 1], [])

    -- Transposers default to add.
    equal (run "t-dia" [(0, 1, "t-dia = 1 | t-dia = 1 default |")])
        ([Just 2], [])

test_equal_inst_alias :: Test
test_equal_inst_alias = do
    let run with_ui title track = DeriveTest.extract DeriveTest.e_instrument $
            DeriveTest.derive_tracks_setup with_ui title
                [(track, [(0, 1, "")])]
    strings_like (snd $ run mempty ">new = 42" ">new")
        ["expected an instrument"]
    equal (run mempty ">new = i1" ">new") (["i1"], [])
    equal (run mempty ">new = i1 | >newer = new" ">newer") (["i1"], [])

test_equal_note_transformer :: Test
test_equal_note_transformer = do
    let run events = DeriveTest.extract e_instrument $
            DeriveTest.derive_tracks_linear ""
                [ (">", events)
                , (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
                ]
    equal (run []) ([(0, ""), (1, ""), (2, "")], [])
    equal (run [(0, 2, "inst = i")]) ([(0, "i"), (1, "i"), (2, "")], [])
    equal (run [(0, 3, "inst = i")]) ([(0, "i"), (1, "i"), (2, "i")], [])
    equal (run [(0, 1, "inst = i1"), (1, 1, "inst = i2")])
        ([(0, "i1"), (1, "i2"), (2, "")], [])

test_equal_call :: Test
test_equal_call = do
    let run = DeriveTest.extract DeriveTest.e_note . DeriveTest.derive_tracks ""
    -- Rebind a note call.
    equal (run [(">", [(0, 1, "^zzz = attr | zzz")])]) ([(0, 1, "?")], [])
    -- Bind to a instrument transformer, as called by note-track.
    let run_env = DeriveTest.extract extract
            . DeriveTest.derive_tracks ">>i1 = \"(v=1)"
        extract e = (DeriveTest.e_instrument e, DeriveTest.e_environ "v" e)
    equal (run_env [(">i1", [(0, 1, "")])]) ([("i1", Just "1")], [])

    equal (run [("> | *zzz = set", [(0, 1, "")]), ("*", [(0, 0, "zzz (4c)")])])
        ([(0, 1, "4c")], [])
    equal (run [("> | *4 = set", [(0, 1, "")]), ("*", [(0, 0, "4 (4c)")])])
        ([(0, 1, "4c")], [])
    equal (run [("> | -zzz = env | p = (4c)", [(0, 1, "")]),
            ("*", [(0, 0, "set (zzz p)")])])
        ([(0, 1, "4c")], [])

    -- RHS that's not a symbol.
    let run2 = DeriveTest.extract DeriveTest.e_attributes
            . DeriveTest.derive_tracks ""
    equal (run2 [("> | ^左 = +left", [(0, 1, "左")])]) (["+left"], [])

test_equal_quoted :: Test
test_equal_quoted = do
    let run title note = DeriveTest.extract extract $
            DeriveTest.derive_tracks "" [(title, [(0, 1, note)])]
        extract e = (DeriveTest.e_pitch e, DeriveTest.e_attributes e)
    -- Just make sure assigning to # works.
    equal (run ">" "# = (4c) |") ([("4c", "+")], [])
    -- Bind val call to (4c).
    equal (run "> | -zzz = \"(4c)" "# = (zzz) |") ([("4c", "+")], [])
    -- Bind argument to 'n'.
    equal (run "> | ^zzz = \"(attr +a)" "zzz") ([("?", "+a")], [])
    -- Recursive binding doesn't work.  To fix this, I should look up the
    -- calls in the quoted text.
    let (vals, logs) = run "> | ^n = \"(n +a)" "n"
    equal vals []
    strings_like logs ["too many arguments"]
    equal (run "> | ^z = \"(# = (4c) |)" "z") ([("4c", "+")], [])

test_default_merge :: Test
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
    equal (run "default-merge default c") ([[(0, 0.25)]], [])

e_instrument :: Score.Event -> (RealTime, Text)
e_instrument e = (Score.event_start e, DeriveTest.e_instrument e)
