-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Calls that generate notes for instruments that come in polos and sangsih
    pairs.

    Kotekan patterns have a number of features in common.  They are all
    transpositions from a base pitch.  Rhythmically, they consist of notes with
    a constant duration, that line up at the end of an event's range, and the
    last duration is negative (i.e. implicit, depending on the next note).
    They use polos and sangsih and may switch patterns when the a kotekan speed
    threshold is passed.  Notes are also possible muted.

    There are a number of ways this can be extended:
    - Use any attribute instead of just mute.
    - More instruments than just polos and sangsih.
    - Multiple kotekan thresholds.

    The first two are supported at the 'PatternNote' level of abstraction.  For
    the others, I have to either directly use 'Note's or create a new
    abstraction:
    - Variable durations.
    - Line up at the start of the event instead of the end.
-}
module Derive.Call.Bali.Kotekan where
import qualified Data.List as List

import Util.Control
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Cmd.Meter as Meter
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [ ("norot", c_norot)
    , (">norot", c_norot_pickup)
    , ("gnorot", c_gender_norot)
    , ("k/_\\", c_kotekan $ regular_pattern "-12-1-21" "3-23-32-" "34-343-4")
    -- sangsih telu is below, but sangsih pat is above
    , ("k//",   c_kotekan $ regular_pattern "23-23-23" "2-12-12-" "5-45-45-")
    , ("k\\\\", c_kotekan $ regular_pattern "21-21-21" "2-32-32-" "-43-43-4")
    , ("k\\/",  c_kotekan $ regular_pattern "-12-12-2 1-21-12-"
                                            "3-23-232 -32-3-23"
                                            "44-34-3- 43-434-3")
    , ("k_\\",  c_kotekan $ regular_pattern "1-1-21" "3-32-32" "4-43-4")
    ]
    [ ("nyog", c_nyogcag)
    , ("unison", c_unison)
    , ("kempyung", c_kempyung)
    , ("noltol", c_noltol)
    ]

parse_pattern :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
    -> KotekanPattern
parse_pattern unison polos_pat sangsih_pat polos_telu sangsih_telu =
    KotekanPattern (parse unison) (parse polos_pat) (parse sangsih_pat)
        (parse polos_telu) (parse sangsih_telu)
    where
    Just destination = Seq.last $ mapMaybe Num.read_digit unison
    parse = map (fmap (subtract destination) . Num.read_digit) . filter (/=' ')

data KotekanPattern = KotekanPattern {
    kotekan_unison :: [Maybe Pitch.Step]
    , kotekan_polos_pat :: [Maybe Pitch.Step]
    , kotekan_sangsih_pat :: [Maybe Pitch.Step]
    , kotekan_polos_telu :: [Maybe Pitch.Step]
    , kotekan_sangsih_telu :: [Maybe Pitch.Step]
    } deriving (Eq, Show)

instance Pretty.Pretty KotekanPattern where
    format (KotekanPattern unison p4 s4 p3 s3) =
        Pretty.record_title "KotekanPattern" $ zip
            ["unison", "polos pat", "sangsih pat", "polos telu", "sangsih telu"]
            (map Pretty.format [unison, p4, s4, p3, s3])

regular_pattern :: [Char] -> [Char] -> [Char] -> KotekanPattern
regular_pattern polos sangsih_telu sangsih_pat =
    parse_pattern (zipWith merge polos sangsih_telu)
        polos sangsih_pat polos sangsih_telu
    where
    merge '-' n = n
    merge n '-' = n
    merge n _ = n

postproc :: Tags.Tags
postproc = Tags.bali <> Tags.postproc


-- * patterns

-- | Initially I implemented this as a postproc, but it now seems to me that
-- it would be more convenient as a generator.  In any case, as a postproc it
-- gets really complicated.
c_norot :: Derive.Generator Derive.Note
c_norot = Derive.make_call "norot" Tags.bali
    "Emit the basic norot pattern. The last note will line up with the end of\
    \ the event, so this is most suitable for a negative duration event."
    $ Sig.call ((,,,,)
    <$> dur_arg
    <*> Sig.defaulted "style" (TrackLang.E Default) "Norot style."
    <*> kotekan_env <*> instrument_top_env <*> pasang_env
    ) $ \(maybe_dur, TrackLang.E style, kotekan, inst_top, pasang) ->
    Sub.inverting $ \args -> do
        dur <- get_dur_arg args maybe_dur
        pitch <- Util.get_pitch =<< Args.real_start args
        scale <- Util.get_scale
        let nsteps = norot_steps scale inst_top pitch style
        under_threshold <- under_threshold_function kotekan dur
        let (start, end) = Args.range args
        realize_notes start pitch $ realize_pattern True start end dur $
            gangsa_norot style under_threshold pasang nsteps

c_norot_pickup :: Derive.Generator Derive.Note
c_norot_pickup = Derive.make_call "norot-pickup" Tags.bali
    "Emit norot pickup."
    $ Sig.call ((,,,,)
    <$> dur_arg
    <*> Sig.defaulted "style" (TrackLang.E Default) "Norot style."
    <*> kotekan_env <*> instrument_top_env <*> pasang_env
    ) $ \(maybe_dur, TrackLang.E style, kotekan, inst_top, pasang) ->
    Sub.inverting $ \args -> do
        dur <- get_dur_arg args maybe_dur
        pitch <- Util.get_pitch =<< Args.real_start args
        scale <- Util.get_scale
        let nsteps = norot_steps scale inst_top pitch style
        under_threshold <- under_threshold_function kotekan dur
        let (start, end) = Args.range args
        realize_notes start pitch $ realize_pattern False start end dur $
            gangsa_norot_pickup style under_threshold pasang nsteps

norot_steps :: Scale.Scale -> Maybe Pitch.Pitch -> PitchSignal.Pitch
    -> NorotStyle -> ((Pitch.Step, Pitch.Step), (Pitch.Step, Pitch.Step))
norot_steps scale inst_top pitch style
    | out_of_range 1 = ((-1, 0), (-1, 0))
    | otherwise = case style of
        Diamond -> ((1, 0), (-1, 0))
        -- Sangsih is only used if non-interlocking and using Diamond style.
        -- So the snd pair should be ignored.
        Default -> ((1, 0), (1, 0))
    where
    out_of_range steps = note_too_high scale inst_top $
        Pitches.transpose_d steps pitch

under_threshold_function :: TrackLang.ValControl -> ScoreTime
    -> Derive.Deriver (ScoreTime -> Bool)
under_threshold_function kotekan dur = do
    to_real <- Derive.real_function
    kotekan <- Util.to_untyped_signal kotekan
    return $ \t ->
        let real = to_real t
        in to_real (t+dur) - real < RealTime.seconds (Signal.at real kotekan)

c_gender_norot :: Derive.Generator Derive.Note
c_gender_norot = Derive.make_call "gender-norot" Tags.bali
    "Gender-style norot."
    $ Sig.call ((,,) <$> dur_arg <*> kotekan_env <*> pasang_env)
    $ \(maybe_dur, kotekan, pasang) -> Sub.inverting $ \args -> do
        dur <- get_dur_arg args maybe_dur
        pitch <- Util.get_pitch =<< Args.real_start args
        under_threshold <- under_threshold_function kotekan dur
        let (start, end) = Args.range args
        realize_notes start pitch $ realize_pattern True start end dur $
            gender_norot under_threshold pasang

gangsa_norot_pickup :: NorotStyle -> (ScoreTime -> Bool) -> Pasang
    -> ((Pitch.Step, Pitch.Step), (Pitch.Step, Pitch.Step))
    -> ScoreTime -> [[PatternNote]]
gangsa_norot_pickup style under_threshold pasang (pstep, sstep) start
    | under_threshold start =
        [ [polos (snd pstep) mempty, sangsih (snd pstep) mempty]
        , [polos (snd pstep) mempty, sangsih (snd pstep) mempty]
        , [sangsih (fst pstep) mempty]
        , [polos (snd pstep) mempty]
        ]
    | otherwise = case style of
        Default -> [[(Nothing, step, attr)] | (step, attr) <-
            [(snd pstep, mute), (snd pstep, mempty), (fst pstep, mempty),
                (snd pstep, mempty)]]
        Diamond ->
            [ [polos (snd pstep) mute, sangsih (snd sstep) mute]
            , [polos (snd pstep) mempty, sangsih (snd sstep) mempty]
            , [polos (fst pstep) mempty, sangsih (fst sstep) mempty]
            , [polos (snd pstep) mempty, sangsih (snd sstep) mempty]
            ]
    where
    mute = Attrs.mute -- TODO configurable
    polos steps attrs = (Just $ fst pasang, steps, attrs)
    sangsih steps attrs = (Just $ snd pasang, steps, attrs)

gangsa_norot :: NorotStyle -> (ScoreTime -> Bool) -> Pasang
    -> ((Pitch.Step, Pitch.Step), (Pitch.Step, Pitch.Step))
    -> MakePattern
gangsa_norot style under_threshold pasang (pstep, sstep) start
    | under_threshold start = map (:[])
        [sangsih (fst pstep), polos (snd pstep)]
    | otherwise = case style of
        Default -> [[(Nothing, step, mempty)] | step <- [fst pstep, snd pstep]]
        Diamond ->
            [ [polos (fst pstep), sangsih (fst sstep)]
            , [polos (snd pstep), sangsih (snd sstep)]
            ]
    where
    polos steps = (Just $ fst pasang, steps, mempty)
    sangsih steps = (Just $ snd pasang, steps, mempty)

gender_norot :: (ScoreTime -> Bool) -> Pasang -> MakePattern
gender_norot under_threshold pasang start
    | under_threshold start =
        [[sangsih 1], [polos 0], [sangsih 1], [polos 0]]
    | otherwise =
        [ [polos (-1), sangsih 1]
        , [polos (-2), sangsih 0]
        , [polos (-1), sangsih 1]
        , if include_unison then [polos 0, sangsih 0] else [sangsih 0]
        ]
    where
    include_unison = True -- TODO chance based on signal
    polos steps = (Just $ fst pasang, steps, mempty)
    sangsih steps = (Just $ snd pasang, steps, mempty)

c_kotekan :: KotekanPattern -> Derive.Generator Derive.Note
c_kotekan pattern = Derive.make_call "kotekan" Tags.bali
    "Kotekan calls perform a pattern with `inst-polos` and `inst-sangsih`.\
    \ They line up at the end of the event, and are intended to be used with\
    \ negative durations."
    $ Sig.call ((,,,)
    <$> dur_arg
    <*> Sig.defaulted "style" (TrackLang.E Telu) "Kotekan style."
    <*> kotekan_env <*> pasang_env
    ) $ \(maybe_dur, TrackLang.E style, kotekan, pasang) ->
    Sub.inverting $ \args -> do
        dur <- get_dur_arg args maybe_dur
        pitch <- Util.get_pitch =<< Args.real_start args
        under_threshold <- under_threshold_function kotekan dur
        let (start, end) = Args.range args
        realize_notes start pitch $ realize_pattern True start end dur $
            kotekan_pattern pattern style under_threshold pasang

kotekan_pattern :: KotekanPattern -> KotekanStyle -> (ScoreTime -> Bool)
    -> Pasang -> ScoreTime -> [[PatternNote]]
kotekan_pattern pattern style under_threshold pasang start =
    map (map realize) $
        pattern_steps style (under_threshold start) pasang pattern
    where realize (inst, steps) = (inst, steps, mempty)

pattern_steps :: KotekanStyle -> Bool -> Pasang -> KotekanPattern
    -> [[(Maybe Score.Instrument, Pitch.Step)]]
pattern_steps style interlock (polos, sangsih)
        (KotekanPattern unison p4 s4 p3 s3)
    | interlock = case style of
        Telu -> interlocking p3 s3
        Pat -> interlocking p4 s4
    | otherwise = normal unison
    where
    interlocking ps ss =
        [ mapMaybe id [(,) (Just polos) <$> p, (,) (Just sangsih) <$> s]
        | (p, s) <- zip ps ss
        ]
    normal = map $ \n -> case n of
        Nothing -> []
        Just steps -> [(Nothing, steps)]


-- ** implementation

type MakePattern = ScoreTime -> [[PatternNote]]
type PatternNote = (Maybe Score.Instrument, Pitch.Step, Score.Attributes)

realize_pattern :: Bool -> ScoreTime -> ScoreTime -> ScoreTime -> MakePattern
    -> [Note]
realize_pattern repeat pattern_start pattern_end dur get_cycle =
    concat $ reverse $ Seq.map_head negate_dur $ concat $
        (if repeat then id else take 1) $
        realize_cycle $ Seq.range' pattern_end pattern_start (-dur)
    where
    negate_dur = map $ \n -> n { note_duration = negate $ note_duration n }
    realize_cycle [] = []
    realize_cycle ts@(t:_) = map realize pairs : realize_cycle rest_ts
        where (pairs, rest_ts) = Seq.zip_remainder (reverse (get_cycle t)) ts
    realize (notes, start) = map (realize1 start ndur) notes
        where ndur = if start == pattern_start then -dur else dur
    realize1 start dur (inst, steps, attrs) = Note start dur inst steps attrs

realize_notes :: ScoreTime -> PitchSignal.Pitch -> [Note] -> Derive.NoteDeriver
realize_notes start pitch =
    mconcat . map note . dropWhile ((<=start) . note_start)
    where
    note (Note start dur inst steps attrs) =
        Derive.d_place start dur $
        maybe id Derive.with_instrument inst $
        Util.add_attrs attrs $
        Util.pitched_note (Pitches.transpose_d steps pitch)

data Note = Note {
    note_start :: !ScoreTime
    , note_duration :: !ScoreTime
    -- | If Nothing, retain the instrument in scope.  Presumably it will be
    -- later split into polos and sangsih by a @unison@ or @kempyung@ call.
    , note_instrument :: !(Maybe Score.Instrument)
    , note_steps :: !Pitch.Step
    , note_attributes :: !Score.Attributes
    } deriving (Show)

-- | Style for non-interlocking norot.  Interlocking norot is always the upper
-- neighbor (or lower on the top key).
data NorotStyle =
    -- | Norot is emitted as the current instrument, which should be converted
    -- into kempyung or unison by a postproc.
    Default
    -- | Norot in the diamond pattern, where sangsih goes down.
    | Diamond
    deriving (Bounded, Eq, Enum, Show)

instance ShowVal.ShowVal NorotStyle where show_val = TrackLang.default_show_val
instance TrackLang.TypecheckEnum NorotStyle

data KotekanStyle = Telu | Pat deriving (Bounded, Eq, Enum, Show)
instance ShowVal.ShowVal KotekanStyle where
    show_val = TrackLang.default_show_val
instance TrackLang.TypecheckEnum KotekanStyle

-- * postproc

c_unison :: Derive.Transformer Derive.Note
c_unison = Derive.transformer "unison" postproc
    "Split part into unison polos and sangsih."
    $ Sig.callt pasang_env $ \(polos, sangsih) _args deriver -> do
        inst <- Util.get_instrument
        Post.map_events_asc_ (unison inst polos sangsih) <$> deriver
    where
    unison inst polos sangsih event
        | Score.event_instrument event == inst =
            [ event { Score.event_instrument = polos }
            , event { Score.event_instrument = sangsih }
            ]
        | otherwise = [event]

-- | I could do this in two different ways:  Eval normally, then eval with
-- +kempyung, and make instrument note call understand it.  Or, postproc,
-- transpose, and check if the nn is above a limit.  The first one would let
-- the instrument choose how it wants to interpret +kempyung while letting this
-- call remain generic, but let's face it, it only really means one thing.  The
-- second seems a little simpler since it doesn't need a cooperating note call.
--
-- So postproc it is.
c_kempyung :: Derive.Transformer Derive.Note
c_kempyung = Derive.transformer "kempyung" postproc
    "Split part into kempyung, with `polos-inst` below and `sangsih-inst`\
    \ above."
    $ Sig.callt ((,)
    <$> instrument_top_env <*> pasang_env
    ) $ \(maybe_top, (polos, sangsih)) _args deriver -> do
        inst <- Util.get_instrument
        scale <- Util.get_scale
        let too_high = pitch_too_high scale maybe_top
        Post.map_events_asc_ (kempyung too_high inst polos sangsih) <$> deriver
    where
    kempyung too_high inst polos sangsih event
        | Score.event_instrument event == inst =
            [ event { Score.event_instrument = polos }
            , transpose too_high $ event { Score.event_instrument = sangsih }
            ]
        | otherwise = [event]
    transpose too_high event
        | too_high transposed = event
        | otherwise = transposed
        where
        transposed = event
            { Score.event_pitch =
                PitchSignal.map_y (Pitches.transpose (Pitch.Diatonic 3))
                    (Score.event_pitch event)
            }

c_nyogcag :: Derive.Transformer Derive.Note
c_nyogcag = Derive.transformer "nyog" postproc
    "Split a single part into polos and sangsih parts by assigning\
    \ polos and sangsih to alternating notes."
    $ Sig.callt pasang_env $ \pasang _args deriver ->
        snd . Post.map_events_asc (nyogcag pasang) True <$> deriver

nyogcag :: Pasang -> Bool -> Score.Event -> (Bool, [Score.Event])
nyogcag (polos, sangsih) is_polos event = (not is_polos, [with_inst])
    where
    with_inst = event
        { Score.event_instrument = if is_polos then polos else sangsih }

c_noltol :: Derive.Transformer Derive.Note
c_noltol = Derive.transformer "noltol" postproc
    "Play the transformed notes in noltol style. If the distance between each\
    \ note and the next note of the same instrument is above a threshold,\
    \ end the note with a `+mute`d copy of itself."
    $ Sig.callt
    (Sig.defaulted "time" (Sig.control "noltol" 0.1)
        "Play noltol if the time available exceeds this threshold.")
    $ \time _args deriver -> do
        events <- deriver
        times <- Post.time_control time events
        return $ Post.map_events_asc_ (Post.uncurry3 noltol)
            (LEvent.zip3 times (Post.nexts events) events)

-- Postproc is seems like the wrong time to be doing this, I can't even change
-- the dyn conveniently.  However, postproc is the only time I reliably know
-- when the next note is.  Could I create the note with a reapply instead?
-- Then I can configure the noltol mute attributes and dynamic change in one
-- place.

-- | If the next note of the same instrument is below a threshold, the note's
-- off time is replaced with a +mute.
noltol :: RealTime -> [Score.Event] -> Score.Event -> [Score.Event]
noltol threshold nexts event
    | maybe False ((>=threshold) . subtract (Score.event_start event)) next =
        [event, muted]
    | otherwise = [event]
    where
    muted = Score.add_attributes (Attrs.mute <> Attrs.loose) $
        -- TODO this should probably be configurable
        Score.modify_dynamic (*0.65) $
        Score.move (+ Score.event_duration event) event
    next = Score.event_start <$>
        List.find ((== Score.event_instrument event) . Score.event_instrument)
            nexts


-- * util

dur_arg :: Sig.Parser (Maybe ScoreTime)
dur_arg = Sig.defaulted "dur" Nothing
    "Duration of derived notes. Defaults to `\"(ts e)`."

-- | Run this on the result of 'dur_arg'.  Ideally, this would be packaged
-- together and Derive.Sig would call it for me.
get_dur_arg :: Derive.PassedArgs a -> Maybe ScoreTime
    -> Derive.Deriver ScoreTime
get_dur_arg args = Util.default_timestep args Meter.E

kotekan_env :: Sig.Parser TrackLang.ValControl
kotekan_env =
    Sig.environ "kotekan" Sig.Unprefixed (TrackLang.constant_control 0.15)
        "If note durations are below this, divide the parts between polos and\
        \ sangsih."

instrument_top_env :: Sig.Parser (Maybe Pitch.Pitch)
instrument_top_env =
    Sig.environ (TrackLang.unsym Environ.instrument_top) Sig.Unprefixed Nothing
        "Top pitch this instrument can play. Normally the instrument sets\
        \ it via the instrument environ."

note_too_high :: Scale.Scale -> Maybe Pitch.Pitch -> PitchSignal.Pitch -> Bool
note_too_high scale maybe_top pitchv = fromMaybe False $ do
    top <- maybe_top
    note <- either (const Nothing) Just $ PitchSignal.pitch_note pitchv
    pitch <- either (const Nothing) Just $ Scale.scale_read scale Nothing note
    return $ pitch > top

pitch_too_high :: Scale.Scale -> Maybe Pitch.Pitch -> Score.Event -> Bool
pitch_too_high scale maybe_top =
    maybe False (note_too_high scale maybe_top) . Score.initial_pitch

-- | (polos, sangsih)
type Pasang = (Score.Instrument, Score.Instrument)

pasang_env :: Sig.Parser Pasang
pasang_env = (,)
    <$> Sig.required_environ (TrackLang.unsym inst_polos) Sig.Unprefixed
        "Polos instrument."
    <*> Sig.required_environ (TrackLang.unsym inst_sangsih) Sig.Unprefixed
        "Sangsih instrument."

inst_polos :: TrackLang.ValName
inst_polos = "inst-polos"

inst_sangsih :: TrackLang.ValName
inst_sangsih = "inst-sangsih"
