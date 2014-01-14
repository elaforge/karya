-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls that generate notes for instruments that come in polos and sangsih
-- pairs.
module Derive.Call.Bali.Kotekan where
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Seq as Seq
import qualified Util.Then as Then

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
    [ ("norot", c_norot) ]
    [ ("nyog", c_nyogcag)
    , ("unison", c_unison)
    , ("kempyung", c_kempyung)
    , ("noltol", c_noltol)
    ]

postproc :: Tags.Tags
postproc = Tags.bali <> Tags.postproc

-- | Initially I implemented this as a postproc, but it now seems to me that
-- it would be more convenient as a generator.  In any case, as a postproc it
-- gets really complicated.
c_norot :: Derive.Generator Derive.Note
c_norot = Derive.make_call "norot" Tags.bali
    "Emit the basic norot pattern. The last note will line up with the end of\
    \ the event, so this is most suitable for a negative duration event."
    $ Sig.call ((,,,,)
    <$> Sig.defaulted "dur" Nothing
        "Duration of derived notes. Defaults to `\"(ts s)`."
    <*> Sig.defaulted "style" (TrackLang.E Kempyung) "Norot style."
    <*> kotekan_env <*> instrument_top_env <*> pasang_env
    ) $ \(maybe_dur, TrackLang.E style, kotekan, inst_top, pasang) ->
    Sub.inverting $ \args -> do
        dur <- maybe (Util.meter_duration (Args.start args) Meter.S 1)
            return maybe_dur
        pitch <- Util.get_pitch =<< Args.real_start args
        scale <- Util.get_scale
        let (start, end) = Args.range args
            out_of_range steps = note_too_high scale inst_top $
                Pitches.transpose_d steps pitch
        to_real <- Derive.real_function
        kotekan <- Util.to_untyped_signal kotekan
        mconcat $ map (note pitch) $ norot out_of_range to_real kotekan style
            pasang dur start end
    where
    note pitch (start, dur, inst, steps) = Derive.d_place start dur $
        Derive.with_instrument inst $
        Util.pitched_note (Pitches.transpose_d steps pitch)

norot :: (Pitch.Step -> Bool) -> (ScoreTime -> RealTime) -> Signal.Control
    -> NorotStyle -> Pasang -> ScoreTime -> ScoreTime
    -> ScoreTime -> [(ScoreTime, ScoreTime, Score.Instrument, Pitch.Step)]
norot out_of_range to_real kotekan style (polos, sangsih) dur start end =
    dropWhile (\(t, _, _, _) -> t <= start) $
        concatMap (one_cycle . second Maybe.isNothing) $
        Seq.zip_next $ end_on start end (dur*2)
    where
    one_cycle (start, is_last)
        | under_threshold start =
            [ (start - dur, dur, sangsih, polos_step1)
            , (start, ndur, polos, polos_step2)
            ]
        | otherwise =
            [ (start - dur, dur, polos, polos_step1)
            , (start - dur, dur, sangsih, sangsih_step1)
            , (start, ndur, polos, polos_step2)
            , (start, ndur, sangsih, sangsih_step2)
            ]
        where ndur = if is_last then -dur else dur
    (polos_step1, polos_step2, sangsih_step1, sangsih_step2)
        | out_of_range 1 = (-1, 0, -1, 0)
        | otherwise = case style of
            Diamond -> (1, 0, -1, 0)
            Kempyung
                | not (out_of_range 4) -> (1, 0, 4, 3)
                | otherwise -> (1, 0, 1, 3)
    under_threshold t =
        to_real (t+dur) - real < RealTime.seconds (Signal.at real kotekan)
        where real = to_real t

end_on :: (Num a, Ord a) => a -> a -> a -> [a]
end_on start end dur =
    reverse $ Then.takeWhile1 (>start) $ Seq.range_ end (-dur)

data NorotStyle = Kempyung | Diamond deriving (Bounded, Eq, Enum, Show)
instance ShowVal.ShowVal NorotStyle where show_val = TrackLang.default_show_val
instance TrackLang.TypecheckEnum NorotStyle

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

kotekan_env :: Sig.Parser TrackLang.ValControl
kotekan_env =
    Sig.environ "kotekan" Sig.Unprefixed (TrackLang.constant_control 0.15)
        "If note durations are below this, kotekan calls will split polos and\
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
