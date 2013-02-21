-- | Utilities to construct scales.
module Derive.Scale.Util where
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Map as Map
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Call as Call
import qualified Derive.Call.Pitch as Call.Pitch
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Types


-- | Make a simple scale where there is a simple mapping from input to note to
-- nn.
simple_scale :: String -> Pitch.Octave -> String -> Pitch.ScaleId
    -> [Pitch.InputKey] -> [Pitch.Note] -> [Pitch.NoteNumber] -> Scale.Scale
simple_scale doc per_octave note_pattern scale_id inputs notes nns = Scale.Scale
    { Scale.scale_id = scale_id
    , Scale.scale_pattern = note_pattern
    , Scale.scale_symbols = []
    , Scale.scale_transposers = standard_transposers
    , Scale.scale_transpose = transpose dmap per_octave
    , Scale.scale_enharmonics = no_enharmonics
    , Scale.scale_note_to_call = mapped_note_to_call nn_map dmap
    , Scale.scale_input_to_note = input_to_note input_map dmap
    , Scale.scale_input_to_nn = mapped_input_to_nn input_map nn_map
    , Scale.scale_call_doc = call_doc standard_transposers dmap input_map doc
    }
    where
    dmap = degree_map notes
    nn_map = note_number_map nns
    input_map = Map.fromList (zip inputs [0..])

-- * types

-- | A number between -1 and 1 exclusive, representing the portion of the way
-- between two scale degrees.  I could have used \"Cents\" for this, but that
-- implies equal temperedness.
type Frac = Double

type InputMap = Map.Map Pitch.InputKey Pitch.Degree

lookup_input :: Pitch.InputKey -> InputMap -> Maybe (Pitch.Degree, Frac)
lookup_input input input_map
    | Just degree <- at = Just (degree, 0)
    | Map.null pre || Map.null post = Nothing
    | otherwise =
        let (prev_input, prev_degree) = Map.findMax pre
            (next_input, next_degree) = Map.findMin post
            frac = Num.normalize (i prev_input) (i next_input) (i input)
        in if frac > 0.5
            then Just (next_degree, frac - 1)
            else Just (prev_degree, frac)
    where
    (pre, at, post) = Map.splitLookup input input_map
    i (Pitch.InputKey nn) = nn

data DegreeMap = DegreeMap {
    dm_to_degree :: Map.Map Pitch.Note Pitch.Degree
    , dm_to_note :: Map.Map Pitch.Degree Pitch.Note
    } deriving (Show)

degree_map :: [Pitch.Note] -> DegreeMap
degree_map notes =
    DegreeMap (Map.fromList (zip notes degrees))
        (Map.fromList (zip degrees notes))
    where
    degrees = [0 .. fromIntegral (length notes)]

type NoteNumberMap = Map.Map Pitch.Degree Pitch.NoteNumber

type DegreeToNoteNumber = TrackLang.Environ -> PitchSignal.Controls
    -> Pitch.Degree -> Either Scale.ScaleError Pitch.NoteNumber

note_number_map :: [Pitch.NoteNumber] -> NoteNumberMap
note_number_map nns = Map.fromList (zip [0..] nns)

-- * scale functions

-- ** transpose

transpose :: DegreeMap -> Pitch.Octave -> Derive.Transpose
transpose dmap per_octave =
    \_key octaves steps note -> do
        note_degree <- maybe (Left Scale.UnparseableNote) Right
            (Map.lookup note (dm_to_degree dmap))
        let degrees = case steps of
                Pitch.Diatonic steps -> d (floor steps)
                Pitch.Chromatic steps -> d (floor steps)
        maybe (Left Scale.InvalidTransposition) Right $
            Map.lookup (note_degree + d octaves * d per_octave + degrees)
                (dm_to_note dmap)
        where d = Pitch.Degree

-- | Transpose function for a non-transposing scale.
non_transposing :: Derive.Transpose
non_transposing _ _ _ _ = Left Scale.InvalidTransposition

standard_transposers :: Set.Set Score.Control
standard_transposers = Set.fromList
    [Score.c_chromatic, Score.c_diatonic, Score.c_hz]

-- ** note_to_call

-- | A specialization of 'note_to_call' that operates on scales with
-- a ScaleMap, i.e. a static map from notes to degrees, and from degrees to
-- NNs.
mapped_note_to_call :: NoteNumberMap -> DegreeMap
    -> Pitch.Note -> Maybe Derive.ValCall
mapped_note_to_call nn_map dmap = note_to_call dmap to_nn
    where
    to_nn _env _controls degree =
        maybe (Left Scale.InvalidTransposition) Right $
            Map.lookup degree nn_map

-- | Create a note call that respects chromatic and diatonic transposition.
-- However, diatonic transposition is mapped to chromatic transposition,
-- so this is for scales that don't distinguish.
note_to_call :: DegreeMap -> DegreeToNoteNumber -> Pitch.Note
    -> Maybe Derive.ValCall
note_to_call dmap degree_to_nn note =
    case Map.lookup note (dm_to_degree dmap) of
        Nothing -> Nothing
        Just degree -> Just $ Call.Pitch.scale_degree (pitch_nn degree)
            (pitch_note degree)
    where
    pitch_nn :: Pitch.Degree -> Scale.PitchNn
    pitch_nn (Pitch.Degree degree) env controls =
        scale_to_pitch_error diatonic chromatic $
            to_note (fromIntegral degree + chromatic + diatonic)
                env controls
        where
        chromatic = Map.findWithDefault 0 Score.c_chromatic controls
        diatonic = Map.findWithDefault 0 Score.c_diatonic controls
    to_note degree env controls
        | frac == 0 = to_nn int
        | otherwise = do
            Num.scale <$> to_nn int <*> to_nn (int+1)
                <*> return (Pitch.NoteNumber frac)
        where
        (int, frac) = properFraction degree
        to_nn = degree_to_nn env controls . Pitch.Degree . fromIntegral
    pitch_note :: Pitch.Degree -> Scale.PitchNote
    pitch_note (Pitch.Degree degree) _env controls =
        maybe (Left err) Right $ Map.lookup transposed (dm_to_note dmap)
        where
        err = invalid_transposition diatonic chromatic
        transposed = Pitch.Degree $ round $
            fromIntegral degree + chromatic + diatonic
        chromatic = Map.findWithDefault 0 Score.c_chromatic controls
        diatonic = Map.findWithDefault 0 Score.c_diatonic controls

lookup_key :: TrackLang.Environ -> Maybe Pitch.Key
lookup_key = fmap Pitch.Key . TrackLang.maybe_val TrackLang.v_key

scale_to_pitch_error :: Signal.Y -> Signal.Y
    -> Either Scale.ScaleError a -> Either PitchSignal.PitchError a
scale_to_pitch_error diatonic chromatic = either (Left . msg) Right
    where
    msg err = case err of
        Scale.InvalidTransposition -> invalid_transposition diatonic chromatic
        Scale.KeyNeeded -> PitchSignal.PitchError
            "no key is set, but this transposition needs one"
        Scale.UnparseableEnviron name val -> PitchSignal.PitchError $
            Pretty.pretty name ++ " unparseable by given scale: " ++ val
        Scale.UnparseableNote -> PitchSignal.PitchError
            "unparseable note (shouldn't happen)"

invalid_transposition :: Signal.Y -> Signal.Y -> PitchSignal.PitchError
invalid_transposition diatonic chromatic =
    PitchSignal.PitchError $ "note can't be transposed: "
        ++ unwords (filter (not . null)
            [show_val "d" diatonic, show_val "c" chromatic])
    where
    show_val _ 0 = ""
    show_val code val = Pretty.pretty val ++ code

-- ** input

input_to_note :: InputMap
    -> DegreeMap -> Maybe Pitch.Key -> Pitch.InputKey -> Maybe Pitch.Note
input_to_note input_map dmap _key input = do
    (degree, frac) <- lookup_input input input_map
    note <- Map.lookup degree (dm_to_note dmap)
    return $ join_note (Pitch.note_text note) frac

-- | Input to NoteNumber for scales that have a direct relationship between
-- Degree and NoteNumber.
mapped_input_to_nn :: InputMap -> NoteNumberMap
    -> (ScoreTime -> Pitch.InputKey -> Derive.Deriver (Maybe Pitch.NoteNumber))
mapped_input_to_nn input_map nn_map = \_pos input -> return $ do
    (degree, frac) <- lookup_input input input_map
    to_nn degree frac
    where
    to_nn degree frac
        | frac == 0 = lookup degree
        | frac > 0 = do
            nn <- lookup degree
            next <- lookup (degree + 1)
            return $ Num.scale nn next (Pitch.NoteNumber frac)
        | otherwise = do
            nn <- lookup degree
            prev <- lookup (degree - 1)
            return $ Num.scale prev nn (Pitch.NoteNumber (frac + 1))
    lookup d = Map.lookup d nn_map

-- | An InputKey maps directly to a NoteNumber.  This is an efficient
-- implementation for scales tuned to 12TET.
direct_input_to_nn :: ScoreTime -> Pitch.InputKey
    -> Derive.Deriver (Maybe Pitch.NoteNumber)
direct_input_to_nn _ (Pitch.InputKey nn) = return $ Just (Pitch.NoteNumber nn)

-- | Convert input to nn by going through note_to_call.  This works for
-- complicated scales that retune based on the environment but is more work.
computed_input_to_nn :: (Maybe Pitch.Key -> Pitch.InputKey -> Maybe Pitch.Note)
    -> (Pitch.Note -> Maybe Derive.ValCall)
    -> ScoreTime -> Pitch.InputKey -> Derive.Deriver (Maybe Pitch.NoteNumber)
computed_input_to_nn input_to_note note_to_call pos input
    | Just note <- input_to_note Nothing input, Just call <- note_to_call note =
        Call.apply_pitch pos call >>= \val -> case val of
            TrackLang.VPitch pitch -> do
                controls <- Derive.controls_at =<< Derive.real pos
                return $ either (const Nothing) Just $
                    PitchSignal.eval_pitch pitch controls
            _ -> return Nothing
    | otherwise = return Nothing

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

-- ** call_doc

call_doc :: Set.Set Score.Control -> DegreeMap -> InputMap -> String
    -> Derive.DocumentedCall
call_doc transposers dmap imap doc =
    annotate_call_doc transposers doc fields $ scale_degree_doc
    where
    fields =
        [ ("note range", map_range snd (dm_to_note dmap))
        , ("input range", map_range fst imap)
        ]
    map_range extract fm = case (Map.min fm, Map.max fm) of
        (Just kv1, Just kv2) ->
            Pretty.pretty (extract kv1) ++ " to " ++ Pretty.pretty (extract kv2)
        _ -> ""

-- | Documentation of the standard 'Call.Pitch.scale_degree'.
scale_degree_doc :: Derive.DocumentedCall
scale_degree_doc = Derive.extract_val_doc $
    Call.Pitch.scale_degree err err
        where err _ _ = Left $ PitchSignal.PitchError "it was just an example!"

annotate_call_doc :: Set.Set Score.Control -> String -> [(String, String)]
    -> Derive.DocumentedCall -> Derive.DocumentedCall
annotate_call_doc transposers doc fields = Derive.prepend_doc extra_doc
    where
    extra_doc = doc ++ "\n\n" ++ join (transposers_field ++ fields)
    transposers_field = if Set.null transposers then []
        else [("transposers", Pretty.pretty transposers)]
    join = unlines . map (\(k, v) -> k ++ ": " ++ v) . filter (not . null . snd)

add_doc :: Scale.Scale -> String -> Scale.Scale
add_doc scale doc = scale
    { Scale.scale_call_doc = Derive.prepend_doc doc (Scale.scale_call_doc scale)
    }

-- * util

no_enharmonics :: Derive.Enharmonics
no_enharmonics _ _ = Right []

read_environ :: (TrackLang.Typecheck a) => (a -> Maybe val) -> val
    -> TrackLang.ValName -> TrackLang.Environ -> Either Scale.ScaleError val
read_environ read_val deflt name env = case TrackLang.get_val name env of
    Left (TrackLang.WrongType expected) ->
        unparseable ("expected type " ++ Pretty.pretty expected)
    Left TrackLang.NotFound -> Right deflt
    Right val -> parse val
    where
    parse val = maybe (unparseable (ShowVal.show_val val)) Right (read_val val)
    unparseable = Left . Scale.UnparseableEnviron name

-- | Symbolic names for input keys.
[i_c, i_cs, i_d, i_ds, i_e, i_f, i_fs, i_g, i_gs, i_a, i_as, i_b]
    = map Pitch.InputKey (Seq.range 0 11 1) :: [Pitch.InputKey]
