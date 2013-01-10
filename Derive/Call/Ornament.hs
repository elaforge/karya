module Derive.Call.Ornament where
import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import Derive.CallSig (optional)
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Types


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("`mordent`", c_mordent (Pitch.Diatonic 1))
    , ("`rmordent`", c_mordent (Pitch.Diatonic (-1)))
    , ("g", c_grace)
    ]

-- | Mordent.  The generated grace notes are constant time and short, and
-- fall before the onset of the main note.
--
-- [neighbor /Transpose/ @default_neighbor@] Neighbor note.
--
-- [dyn /Number/ @.3@] Scale the dynamic of the generated grace notes.
c_mordent :: Pitch.Transpose -> Derive.NoteCall
c_mordent default_neighbor = Derive.stream_generator "mordent"
    ("Generate some grace notes for a mordent, similar to a brief trill.\
    \ The grace notes fall before the onset of the main note, and\
    \ are in absolute RealTime, unaffected by tempo changes."
    ) $ CallSig.call2g
    ( optional "neighbor" default_neighbor "Neighbor pitch."
    , optional "dyn" 0.5 "Scale the dyn of the generated grace notes."
    ) $ \neighbor dyn -> Note.inverting $ \args ->
        mordent grace_dur grace_overlap (Args.extent args) dyn neighbor

grace_dur, grace_overlap :: RealTime
grace_dur = RealTime.seconds (1/12)
grace_overlap = grace_dur / 2

mordent :: RealTime -> RealTime -> (ScoreTime, ScoreTime) -> Signal.Y
    -> Pitch.Transpose -> Derive.EventDeriver
mordent grace_dur grace_overlap (start, dur) dyn_scale neighbor = do
    pos <- Derive.real start
    pitch <- Derive.require ("pitch at " ++ Pretty.pretty pos)
        =<< Util.pitch pos
    dyn <- (*dyn_scale) <$> Util.dynamic pos
    grace_notes pos grace_dur grace_overlap
        [ Util.pitched_note pitch dyn
        , Util.pitched_note (Pitches.transpose neighbor pitch) dyn
        ]
        <> Derive.d_place start dur Util.note

c_grace :: Derive.NoteCall
c_grace = Derive.stream_generator "grace" "Emit grace notes." $
    CallSig.parsed_manually "dyn? pitch*" $ Note.inverting $ \args -> do
        (dyn_scale, pitches) <- parse args
        grace (Args.extent args) dyn_scale pitches
    where
    parse args = parse_dyn args (zip [0..] (Derive.passed_vals args))
    parse_dyn _ ((_, TrackLang.VNum (Score.Typed Score.Untyped dyn)) : vals) =
        ((,) dyn) <$> parse_pitches vals
    parse_dyn args vals = (,) <$> default_dyn args <*> parse_pitches vals
    parse_pitches = mapM parse_pitch
    parse_pitch (i, val) = CallSig.extract_arg i
        (CallSig.Arg "pitch" "pitch" Nothing) (Just val)
    default_dyn args = CallSig.default_arg args 0.5 "dyn"

grace :: (ScoreTime, ScoreTime) -> Signal.Y -> [PitchSignal.Pitch] ->
    Derive.EventDeriver
grace (start, dur) dyn_scale pitches = do
    pos <- Derive.real start
    dyn <- (*dyn_scale) <$> Util.dynamic pos
    grace_notes pos grace_dur grace_overlap
        (map (flip Util.pitched_note dyn) pitches)
        <> Derive.d_place start dur Util.note


grace_notes :: RealTime -- ^ note start time, grace notes fall before this
    -> RealTime -- ^ duration of each grace note
    -> RealTime -- ^ overlap between each grace note and the next
    -> [Derive.EventDeriver]
    -> Derive.EventDeriver
grace_notes start dur overlap notes = Derive.d_merge placed
    where
    placed = zipWith (flip place (dur + overlap)) starts notes
    starts = Seq.range_ (start - dur * fromIntegral (length notes)) dur

place :: RealTime -> RealTime -> Derive.Deriver d -> Derive.Deriver d
place start dur d = do
    rstart <- Derive.score start
    rend <- Derive.score (start + dur)
    Derive.d_place rstart (rend - rstart) d
