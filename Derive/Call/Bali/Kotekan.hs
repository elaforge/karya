-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls that generate notes for instruments that come in polos and sangsih
-- pairs.
module Derive.Call.Bali.Kotekan where
import Util.Control
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


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps []
    [ ("nyog", c_nyogcag)
    , ("unison", c_unison)
    , ("kempyung", c_kempyung)
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
    $ Sig.call0t $ \_args deriver -> do
        inst <- Util.get_instrument
        (polos, sangsih) <- get_pasang
        Post.concat_map (unison inst polos sangsih) <$> deriver
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
    $ Sig.callt (Sig.defaulted "top" Nothing
        "Any pitches above this will be in unison. Normally the instrument\
        \ sets it via the environ.")
    $ \top_pitch _args deriver -> do
        inst <- Util.get_instrument
        (polos, sangsih) <- get_pasang
        maybe_top <- case top_pitch of
            Nothing -> return Nothing
            Just pitch -> Just <$>
                Derive.with_instrument sangsih (Pitches.pitch_nn pitch)
        Post.concat_map (kempyung maybe_top inst polos sangsih) <$> deriver
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
    $ Sig.call0t $ \_args deriver -> do
        events <- deriver
        (polos, sangsih) <- get_pasang
        return $ snd $ Post.map_state (nyogcag polos sangsih)
            (True, LEvent.events_of events) events

-- How about the case where I want each part to switch out noltol around
-- the same point, but slightly different?

nyogcag :: Score.Instrument -> Score.Instrument -> (Bool, [Score.Event])
    -> Score.Event -> ((Bool, [Score.Event]), [Score.Event])
nyogcag polos sangsih (is_polos, next) event =
    ((not is_polos, drop 1 next), [with_inst])
    where
    with_inst = event
        { Score.event_instrument = if is_polos then polos else sangsih }

get_pasang :: Derive.Deriver (Score.Instrument, Score.Instrument)
get_pasang = (,) <$> Derive.get_val inst_polos <*> Derive.get_val inst_sangsih

inst_polos :: TrackLang.ValName
inst_polos = "inst-polos"

inst_sangsih :: TrackLang.ValName
inst_sangsih = "inst-sangsih"
