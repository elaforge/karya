-- | Utilities to construct scales.
module Derive.Scale.Util where
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Ui.Track as Track
import qualified Derive.Call as Call
import qualified Derive.Call.Pitch as Call.Pitch
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import Types


-- * ScaleMap

-- | A number between -1 and 1, representing the portion of the way between
-- two scale degrees.  I could have used \"Cents\" for this, but that implies
-- equal temperedness.
type Frac = Double

data ScaleMap = ScaleMap {
    smap_note_to_degree :: Map.Map Pitch.Note Pitch.Degree
    -- | Should always be the inverse of the above.
    , smap_degree_to_note :: Map.Map Pitch.Degree Pitch.Note
    , smap_degree_to_nn :: Map.Map Pitch.Degree Pitch.NoteNumber
    , smap_input_to_note :: InputMap
    } deriving (Show)

-- | All input lists should be the same length.
scale_map :: [Pitch.Note] -> [Pitch.InputKey] -> [Pitch.NoteNumber] -> ScaleMap
scale_map notes inputs nns = ScaleMap
    (Map.fromList (zip notes degrees))
    (Map.fromList (zip degrees notes))
    (Map.fromList (zip degrees nns))
    (Map.fromList (zip inputs (zip nns notes)))
    where degrees = [0 .. fromIntegral (length notes)]

type InputMap = Map.Map Pitch.InputKey (Pitch.NoteNumber, Pitch.Note)

make_scale_map :: ScaleMap -> Track.ScaleMap
make_scale_map smap = Track.make_scale_map
    [(n, d) | (n, d) <- Map.assocs (smap_note_to_degree smap)]

transpose :: ScaleMap -> Pitch.Octave -> Derive.Transpose
transpose scale_map per_octave = \_key octaves steps note -> do
    note_degree <- maybe (Left Scale.UnparseableNote) Right $
        Map.lookup note (smap_note_to_degree scale_map)
    let degrees = case steps of
            Pitch.Diatonic steps -> d (floor steps)
            Pitch.Chromatic steps -> d (floor steps)
    maybe (Left Scale.InvalidTransposition) Right $
        Map.lookup (note_degree + d octaves * d per_octave + degrees)
            (smap_degree_to_note scale_map)
    where d = Pitch.Degree

-- | Create a note call that respects chromatic and diatonic transposition.
-- However, diatonic transposition is mapped to chromatic transposition
-- and key is ignored, so this is for scales that don't have a distinction
-- between chromatic and diatonic.
note_to_call :: ScaleMap -> Pitch.Note -> Maybe Derive.ValCall
note_to_call smap note = case Map.lookup note (smap_note_to_degree smap) of
        Nothing -> Nothing
        Just degree -> Just $ Call.Pitch.note_call note (note_number degree)
    where
    note_number :: Pitch.Degree -> Scale.GetNoteNumber
    note_number (Pitch.Degree degree) chromatic diatonic _key
        | frac == 0 = maybe (Left Scale.InvalidTransposition) Right maybe_nn
        | otherwise = case (maybe_nn, maybe_nn1) of
            (Just nn, Just nn1) ->
                Right $ Num.scale nn nn1 (Pitch.NoteNumber frac)
            _ -> Left Scale.InvalidTransposition
        where
        (int, frac) = properFraction $
            fromIntegral degree + chromatic + diatonic
        maybe_nn = Map.lookup int (smap_degree_to_nn smap)
        maybe_nn1 = Map.lookup (int+1) (smap_degree_to_nn smap)

input_to_note :: ScaleMap -> Maybe Pitch.Key -> Pitch.InputKey
    -> Maybe Pitch.Note
input_to_note smap _key input = flip fmap (lookup_input input input_map) $
    \(_, step, frac) -> join_note (Pitch.note_text step) frac
    where input_map = smap_input_to_note smap

-- | Map an InputKey to a NoteNumber through the ScaleMap.
mapped_input_to_nn :: ScaleMap -> ScoreTime -> Pitch.InputKey
    -> Derive.Deriver (Maybe Pitch.NoteNumber)
mapped_input_to_nn smap _ input = return $
    fmap (\(nn, _, _) -> nn) (lookup_input input input_map)
    where input_map = smap_input_to_note smap

-- | An InputKey maps directly to a NoteNumber.  This is for scales tuned to
-- 12TET.
direct_input_to_nn :: ScoreTime -> Pitch.InputKey
    -> Derive.Deriver (Maybe Pitch.NoteNumber)
direct_input_to_nn _ (Pitch.InputKey nn) = return $ Just (Pitch.NoteNumber nn)

-- | Convert input to nn by going through note_to_call.  This works for
-- complicated scales that retune based on position but is more work.
input_to_nn ::  (Maybe Pitch.Key -> Pitch.InputKey -> Maybe Pitch.Note)
    -> (Pitch.Note -> Maybe Derive.ValCall)
    -> ScoreTime -> Pitch.InputKey -> Derive.Deriver (Maybe Pitch.NoteNumber)
input_to_nn input_to_note note_to_call pos input
    | Just note <- input_to_note Nothing input,
            Just call <- note_to_call note = do
        val <- Call.apply (TrackLang.Symbol (Pitch.note_text note)) call []
        case val of
            TrackLang.VPitch pitch -> do
                controls <- Derive.controls_at =<< Derive.real pos
                return $ either (const Nothing) Just $
                    PitchSignal.eval_pitch pitch controls
            _ -> return $ Nothing
    | otherwise = return Nothing

lookup_input :: Pitch.InputKey -> InputMap
    -> Maybe (Pitch.NoteNumber, Pitch.Note, Frac)
lookup_input input input_map
    | Just (nn, step) <- at = Just (nn, step, 0)
    | Map.null pre || Map.null post = Nothing
    | otherwise =
        let (prev_input, (prev_nn, prev_degree)) = Map.findMax pre
            (next_input, (next_nn, next_degree)) = Map.findMin post
            dist = Num.normalize (i prev_input) (i next_input) (i input)
            scaled_nn = Num.scale prev_nn next_nn (Pitch.NoteNumber dist)
        in if dist > 0.5
            then Just (scaled_nn, next_degree, dist - 1)
            else Just (scaled_nn, prev_degree, dist)
    | otherwise = Nothing
    where
    -- TODO use zip_neighbors?
    (pre, at, post) = Map.splitLookup input input_map
    i (Pitch.InputKey nn) = nn

make_nn :: Maybe Pitch.NoteNumber -> Pitch.NoteNumber -> Maybe Pitch.NoteNumber
    -> Frac -> Maybe Pitch.NoteNumber
make_nn mprev nn mnext frac
    | frac == 0 = Just nn
    | frac > 0 = fmap (\next -> interpolate nn next) mnext
    | otherwise = fmap (\prev -> interpolate prev nn) mprev
    where
    interpolate low high = Num.scale low high (Pitch.NoteNumber frac)

-- I use + and - for fractional offset.  This means that scales that use these
-- utils can't use those chars themselves or the parser will get confused.
--
-- As a special case for scales that start with possibly negative number, the
-- step may start with a + or -, since a null step isn't very useful.
join_note :: String -> Frac -> Pitch.Note
join_note step frac = Pitch.Note $ step ++ frac_s
    where
    frac_s
        | frac == 0 = ""
        | otherwise = ' ' : TrackLang.show_val frac

-- * other utils

-- | Transpose function for a non-transposing scale.
non_transposing :: Derive.Transpose
non_transposing _ _ _ _ = Left Scale.InvalidTransposition

no_enharmonics :: Derive.Enharmonics
no_enharmonics _ _ = Right []

-- * misc

-- | Symbolic names for input keys.
[i_c, i_cs, i_d, i_ds, i_e, i_f, i_fs, i_g, i_gs, i_a, i_as, i_b]
    = map Pitch.InputKey (Seq.range 0 11 1) :: [Pitch.InputKey]

standard_transposers :: Set.Set Score.Control
standard_transposers = Set.fromList
    [Score.c_chromatic, Score.c_diatonic, Score.c_hz]
