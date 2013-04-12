-- | Calls that generate grace notes.  These are short sequences of quick notes
-- whose duration is generally independent of the tempo.
module Derive.Call.Grace where
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import Derive.Sig (optional, defaulted, many)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Lilypond.Lilypond as Lilypond
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
c_mordent default_neighbor = Derive.stream_generator "mordent" Tags.ornament
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

type Pitch = Either PitchSignal.Pitch Pitch.Transpose

c_grace :: Derive.NoteCall
c_grace = Derive.stream_generator "grace" (Tags.ornament <> Tags.ly)
    ("Emit grace notes.\n" <> grace_doc) $ Sig.call ((,)
    <$> optional "dyn" "Scale the dyn of the grace notes."
    <*> many "pitch" "Grace note pitches."
    ) $ \(dyn, pitches) -> Note.inverting $ \args -> do
        (_, pitches) <- resolve_pitches args pitches
        Lily.when_lilypond (lily_grace args pitches) $
            grace (Args.extent args) (fromMaybe 0.5 dyn) pitches

lily_grace :: Derive.PassedArgs d -> [PitchSignal.Pitch] -> Derive.EventDeriver
lily_grace args pitches = do
    pitches <- mapM Lily.pitch_to_lily pitches
    let ly_notes = map (++ Lilypond.to_lily Lilypond.D8) pitches
        beamed = Seq.first_last (++"[") (++"]") ly_notes
        -- I use \acciaccatura instead of \grace because it adds a slur
        -- automatically.
        code = "\\acciaccatura { " <> unwords beamed <> " }"
    Lily.code (Args.start args, 0) code <> Util.place args Util.note

grace :: (ScoreTime, ScoreTime) -> Signal.Y -> [PitchSignal.Pitch]
    -> Derive.EventDeriver
grace (start, dur) dyn_scale pitches = do
    pos <- Derive.real start
    dyn <- (*dyn_scale) <$> Util.dynamic pos
    (grace_dur, overlap) <- grace_dur_overlap
    let notes = map (flip Util.pitched_note dyn) pitches
    grace_notes pos grace_dur overlap notes
        <> Derive.d_place start dur Util.note

resolve_pitches :: Derive.PassedArgs d
    -> [Either PitchSignal.Pitch Pitch.Transpose]
    -> Derive.Deriver (PitchSignal.Pitch, [PitchSignal.Pitch])
resolve_pitches args pitches = do
    base <- Derive.require "no pitch" =<< Derive.pitch_at
        =<< Args.real_start args
    return (base, map (either id (flip Pitches.transpose base)) pitches)

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

grace_doc :: Text
grace_doc = "This variant of grace note doesn't affect the start time of the\
    \ destination note, and is of a uniform speed, regardless of the tempo.\
    \ Grace note duration is set by `grace-dur` in the environment and\
    \ defaults to " <> txt (Pretty.pretty grace_dur_default)
    <> ", overlap time is `grace-overlap` and defaults to "
    <> txt (Pretty.pretty grace_overlap_default)
    <> "."

c_grace_attr :: Map.Map Int Score.Attributes
    -- ^ Map intervals in semitones (positive or negative) to attrs.
    -> Derive.NoteCall
c_grace_attr supported =
    Derive.stream_generator "grace" (Tags.ornament <> Tags.ly)
    ("Emit grace notes as attrs, given a set of possible interval attrs.\
    \ If the grace note can't be expressed by the supported attrs, then emit\
    \ notes like the normal grace call.\nSupported: "
    <> Text.intercalate ", "
        (map (txt . ShowVal.show_val) (Map.elems supported))
    ) $ Sig.call ((,)
    <$> optional "dyn" "Scale the dyn of the grace notes."
    <*> many "pitch" "Grace note pitches."
    ) $ \(dyn, pitches) -> Note.inverting $ \args -> do
        (base, pitches) <- resolve_pitches args pitches
        Lily.when_lilypond (lily_grace args pitches) $ do
            maybe_attrs <- grace_attrs supported pitches base
            case maybe_attrs of
                Just attrs -> Util.add_attrs attrs (Util.placed_note args)
                -- Fall back on normal grace.  TODO I might want to stick
                -- a legato attr on that or something.
                Nothing -> grace (Args.extent args) (fromMaybe 0.5 dyn) pitches

grace_attrs :: Map.Map Int Score.Attributes -> [PitchSignal.Pitch]
    -> PitchSignal.Pitch -> Derive.Deriver (Maybe Score.Attributes)
grace_attrs supported [grace] base = do
    diff <- (-) <$> Pitches.pitch_nn base <*> Pitches.pitch_nn grace
    return $ Map.lookup (round diff) supported
grace_attrs _ _ _ = return Nothing
