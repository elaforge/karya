-- | Test the whole path from score to lilypond.
module Perform.Lilypond.LilypondDerive_test where
import qualified Data.Text as Text

import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Derive.Attrs as Attrs
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Pitch as Pitch

import Types


test_tempo = do
    let (events, logs) = derive 1
            [ ("tempo", [(0, 0, "3")])
            , (">s/1", [(0, 1, ""), (1, 2, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4d")])
            ]
        extract e = (Lilypond.event_start e, Lilypond.event_duration e)
    equal logs []
    pprint (map extract events)
    putStrLn $ make_ly default_score events

test_trill = do
    let (events, logs) = derive 1
            [ (">s/1", [(0, 2, "tr"), (2, 2, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4d")])
            ]
        extract e = (Lilypond.event_start e, Lilypond.event_pitch e,
            Lilypond.event_attributes e)
    equal logs []
    equal (map extract events)
        [(0, "c'", Attrs.trill), (64, "d'", mempty)]
    putStrLn $ make_ly default_score events

default_score :: Lilypond.Score
default_score = make_score "c-maj" "treble" "4/4"

-- test:
-- clef via attr
-- time sig via attr
-- left and right hand
-- manual ly code fragments, e.g. page breaks
-- set voices with attrs
--
-- pitch spelling comes through (need PitchSignal.Pitch support)


derive :: RealTime -> [UiTest.TrackSpec] -> ([Lilypond.Event], [String])
derive quarter tracks = (ly_events, extract_logs logs)
    where
    (ly_events, logs) = LEvent.partition $ Convert.convert quarter $
        Derive.r_events (derive_ly tracks)
    extract_logs = map DeriveTest.show_log . DeriveTest.trace_low_prio

-- TODO use Cmd.Lilypond.derive instead?
-- derive :: (Cmd.M m) => BlockId -> m Derive.Result
derive_ly :: [UiTest.TrackSpec] -> Derive.Result
derive_ly = DeriveTest.derive_tracks_with_ui
    (Derive.with_val TrackLang.v_lilypond_derive "true")
    (State.config#State.default_#State.tempo #= 1)

make_ly :: Lilypond.Score -> [Lilypond.Event] -> String
make_ly score events = strip texts
    where
    strip = Text.unpack . Text.unlines . Seq.rdrop 3 . drop 4 . Text.lines
        . Text.strip . mconcat
    (texts, _stack_map) = Lilypond.make_ly Lilypond.default_config score events

make_score :: String -> Lilypond.Clef -> String -> Lilypond.Score
make_score key_str clef time_sig =
    either (error . ("make_score: " ++)) id $ do
        key <- Lilypond.parse_key (Pitch.Key key_str)
        tsig <- Lilypond.parse_time_signature time_sig
        return $ Lilypond.Score
            { Lilypond.score_title = "title"
            , Lilypond.score_time = tsig
            , Lilypond.score_clef = clef
            , Lilypond.score_key = key
            }
