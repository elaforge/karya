module Derive.Call.Ornament where
import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Util as Util
import qualified Derive.Sig as Sig
import Derive.Sig (optional, defaulted, many)
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
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

c_mordent :: Pitch.Transpose -> Derive.NoteCall
c_mordent default_neighbor = Derive.stream_generator "mordent"
    ("Generate some grace notes for a mordent, similar to a brief trill.\
    \ The grace notes fall before the onset of the main note, and\
    \ are in absolute RealTime, unaffected by tempo changes.\n" <> grace_doc) $
    Sig.call ((,)
    <$> defaulted "neighbor" default_neighbor "Neighbor pitch."
    <*> defaulted "dyn" 0.5 "Scale the dyn of the generated grace notes."
    ) $ \(neighbor, dyn) -> Note.inverting $ \args ->
        mordent (Args.extent args) dyn neighbor

mordent :: (ScoreTime, ScoreTime) -> Signal.Y -> Pitch.Transpose
    -> Derive.EventDeriver
mordent (start, dur) dyn_scale neighbor = do
    pos <- Derive.real start
    pitch <- Derive.require ("pitch at " ++ Pretty.pretty pos)
        =<< Util.pitch pos
    dyn <- (*dyn_scale) <$> Util.dynamic pos
    (grace_dur, overlap) <- grace_dur_overlap
    grace_notes pos grace_dur overlap
        [ Util.pitched_note pitch dyn
        , Util.pitched_note (Pitches.transpose neighbor pitch) dyn
        ]
        <> Derive.d_place start dur Util.note

c_grace :: Derive.NoteCall
c_grace = Derive.stream_generator "grace"
    ("Emit grace notes.\n" <> grace_doc) $ Sig.call ((,)
    <$> optional "dyn" "Scale the dyn of the grace notes."
    <*> many "pitch" "Grace note pitches."
    ) $ \(dyn, pitches) -> Note.inverting $ \args -> do
        grace (Args.extent args) (fromMaybe 0.5 dyn) pitches

grace :: (ScoreTime, ScoreTime) -> Signal.Y -> [PitchSignal.Pitch] ->
    Derive.EventDeriver
grace (start, dur) dyn_scale pitches = do
    pos <- Derive.real start
    dyn <- (*dyn_scale) <$> Util.dynamic pos
    (grace_dur, overlap) <- grace_dur_overlap
    grace_notes pos grace_dur overlap (map (flip Util.pitched_note dyn) pitches)
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

grace_dur_overlap :: Derive.Deriver (RealTime, RealTime)
grace_dur_overlap = (,) <$> dur <*> overlap
    where
    dur = fromMaybe (RealTime.seconds (1/12)) <$>
        Derive.lookup_val (TrackLang.Symbol "grace-dur")
    overlap = fromMaybe (RealTime.seconds (1/24)) <$>
        Derive.lookup_val (TrackLang.Symbol "grace-overlap")

grace_dur_default, grace_overlap_default :: RealTime
grace_dur_default = RealTime.seconds 0.8
grace_overlap_default = grace_dur_default / 2

grace_doc :: String
grace_doc = "Grace note duration is set by `grace-dur` in the environment and\
    \ defaults to " <> Pretty.pretty grace_dur_default <> ", overlap time is\
    \ `grace-overlap` and defaults to " <> Pretty.pretty grace_overlap_default
    <> "."
