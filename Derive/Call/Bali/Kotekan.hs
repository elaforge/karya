-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls that generate notes for instruments that come in polos and sangsih
-- pairs.
module Derive.Call.Bali.Kotekan where
import qualified Data.List as List

import Util.Control
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps []
    [ ("nyog", c_nyogcag)
    , ("unison", c_unison)
    , ("kempyung", c_kempyung)
    , ("noltol", c_noltol)
    ]

-- Similar to reyong, extract a pokok from the events and generate from
-- a pattern.
c_norot :: Derive.Generator Derive.Note
c_norot = undefined

postproc :: Tags.Tags
postproc = Tags.idiom <> Tags.bali <> Tags.postproc

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
c_kempyung :: Derive.Transformer Derive.Note
c_kempyung = Derive.transformer "kempyung" postproc
    "Split part into kempyung, with `polos-inst` below and `sangsih-inst`\
    \ above."
    $ Sig.callt ((,)
    <$> Sig.defaulted "top" Nothing
        "Any pitches above this will be in unison. Normally the instrument\
        \ sets it via the environ."
    <*> pasang_env
    ) $ \(top_pitch, (polos, sangsih)) _args deriver -> do
        inst <- Util.get_instrument
        maybe_top <- case top_pitch of
            Nothing -> return Nothing
            Just pitch -> Just <$>
                Derive.with_instrument sangsih (Pitches.pitch_nn pitch)
        Post.map_events_asc_ (kempyung maybe_top inst polos sangsih) <$> deriver
    where
    kempyung maybe_top inst polos sangsih event
        | Score.event_instrument event == inst =
            [ event { Score.event_instrument = polos }
            , transpose maybe_top $ event { Score.event_instrument = sangsih }
            ]
        | otherwise = [event]
    transpose maybe_top event
        | Just top <- maybe_top, Just nn <- Score.initial_nn transposed,
            nn > top = event
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
    \ `inst-polos` and `inst-sangsih` to alternating notes."
    $ Sig.callt pasang_env $ \(polos, sangsih) _args deriver ->
        snd . Post.map_events_asc (nyogcag polos sangsih) True <$> deriver

nyogcag :: Score.Instrument -> Score.Instrument
    -> Bool -> Score.Event -> (Bool, [Score.Event])
nyogcag polos sangsih is_polos event = (not is_polos, [with_inst])
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

pasang_env :: Sig.Parser (Score.Instrument, Score.Instrument)
pasang_env = (,)
    <$> Sig.required_environ (TrackLang.unsym inst_polos) Sig.Unprefixed
        "Polos instrument."
    <*> Sig.required_environ (TrackLang.unsym inst_sangsih) Sig.Unprefixed
        "Sangsih instrument."

inst_polos :: TrackLang.ValName
inst_polos = "inst-polos"

inst_sangsih :: TrackLang.ValName
inst_sangsih = "inst-sangsih"
