module Derive.Note_test where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Util.Test
import qualified Util.Log as Log

import Ui.Types
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Derive.Derive_test as Derive_test
import qualified Derive.Note as Note
import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import qualified Perform.Warning as Warning

import qualified Derive.Derive as Derive


-- * parse

test_parse_note = do
    let f = Note.parse_note
    equal (f (inst "k") no_attrs ">i" "1x 1y 2 b")
        (Note.Parsed "1x" [Note.Number 2, Note.String "b"] (inst "i") no_attrs
        ,["event word 1: can't parse number \"1y\"",
            "call \"1x\" starts with non-letter '1'"])

test_preprocess_words = do
    let f inst attrs s = (inst2, attrs2, map snd rest)
            where
            (inst2, attrs2, rest) = Note.preprocess_words
                inst attrs (zip (map show [0..]) (words s))

    equal (f Nothing no_attrs "--blah +a1 >i1") (Nothing, no_attrs, [])
    equal (f Nothing (attrs ["a4"]) "+a1 +a2 -a1 x")
        (Nothing, attrs ["a2", "a4"], ["x"])
    equal (f Nothing (attrs ["a4"]) "+a1 +a2 =a3 x")
        (Nothing, attrs ["a3"], ["x"])
    equal (f Nothing (attrs ["a4"]) "+a1 +a2 = x")
        (Nothing, no_attrs, ["x"])
    equal (f (inst "i1") no_attrs "+a1 > ")
        (inst "i1", attrs ["a1"], [])
    equal (f (inst "i1") no_attrs ">i2 x >i3 +a1 --blah blah")
        (inst "i3", attrs ["a1"], ["x"])

test_parse_args = do
    let f s = Note.parse_args (zip (map show [0..]) (words s))
    equal (f "x y 1 *a")
        ("x", [Note.String "y", Note.Number 1, Note.Note (Pitch.Note "a")], [])
    equal (f "x 1y z")
        ("x", [Note.String "z"], ["1: can't parse number \"1y\""])
    equal (f "1x")
        ("1x", [], ["call \"1x\" starts with non-letter '1'"])
    equal (f "") ("", [], [])

no_attrs = Score.no_attrs
inst = Just . Score.Instrument
attrs = Set.fromList :: [String] -> Score.Attributes

test_parse_arg = do
    let f s = Note.parse_arg ("desc", s)
    equal (f "") $ Left "desc: empty word, this shouldn't happen!"
    equal (f ".1") $ Right (Note.Number 0.1)
    equal (f ".") $ Left "desc: can't parse number \".\""
    equal (f "1s") $ Left "desc: can't parse number \"1s\""
    equal (f "*ho") $ Right (Note.Note (Pitch.Note "ho"))
    equal (f "hoho") $ Right (Note.String "hoho")

-- * derivers

test_derive_notes = do
    let mkevt (pos, dur, text) = (event, parsed)
            where
            (parsed, _) = Note.parse_note Nothing no_attrs "" text
            event = UiTest.mkevent (pos, dur, text)
        run evts = (map extract_event sevts, map extract_log logs)
            where
            (sevts, logs) =
                Derive_test.derive_events_lookup lookup ui_state deriver
            deriver = Note.derive_notes (map mkevt evts)
            lookup = lookup_deriver d_fake_sub
            (_, ui_state) = UiTest.run State.empty $ do
                UiTest.mkstate sub_name [(">", [(0, 1, "")])]
                UiTest.mkstate "b1" [(">", evts)]
            extract_log msg = (Log.msg_text msg, Log.msg_stack msg)

    equal (run []) ([], [])
    equal (run [(0, 1, ">i +a")])
        ([(0, 1, ">i +a", mkstack [("b1", Just (0, 1))], inst "i",
            attrs ["a"])], [])

    let (evts, logs) = run [(0, 1, "nosuch")]
    equal evts []
    strings_like (map fst logs)
        ["error sub-deriving.* unknown \\(bid \"test/nosuch\""]
    equal (map snd logs) [Just (mkstack [("b1", Just (0, 1))])]

    -- subderived stuff is stretched and placed, inherits instrument
    let (evts, logs) = run [(0, 1, sub_name), (1, 2, ">i +a " ++ sub_name)]
    equal logs []
    equal evts
        [ (0, 1, "hi", mkstack [("sub", Nothing), ("b1", Just (0, 1))],
            Nothing, no_attrs)
        , (1, 2, "hi", mkstack [("sub", Nothing), ("b1", Just (1, 2))],
            inst "i", attrs ["a"])
        ]

d_fake_sub = do
    st <- Derive.get
    start <- Derive.local_to_global 0
    end <- Derive.local_to_global 1
    return [Score.Event start (end-start) "hi" Map.empty (Derive.state_stack st)
        (Derive.state_instrument st) (Derive.state_attributes st)]

mkstack :: [(String, Maybe (TrackPos, TrackPos))] -> Warning.Stack
mkstack = map $ \(bid, pos) -> (UiTest.bid bid, Nothing, pos)

-- | Events aren't in Eq, so extract the bits I want to test.
extract_event e = (Score.event_start e, Score.event_duration e,
    Score.event_text e, Score.event_stack e, Score.event_instrument e,
    Score.event_attributes e)

sub_name = "sub"

lookup_deriver deriver block_id
    | block_id == UiTest.bid sub_name = Right deriver
    | otherwise = Left (State.StateError "not found")
