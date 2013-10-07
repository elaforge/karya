-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls that generate notes for instruments that come in polos and sangsih
-- pairs.
module Derive.Call.Bali.Kotekan where
import Util.Control
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [ ("norot", c_norot)
    ]
    [ ("nyog", c_nyogcag) ]

-- Similar to reyong, extract a pokok from the events and generate from
-- a pattern.
c_norot :: Derive.Generator Derive.Note
c_norot = undefined

c_nyogcag :: Derive.Transformer Derive.Note
c_nyogcag = Derive.transformer "nyog" (Tags.idiom <> Tags.bali <> Tags.postproc)
    "Split a single part into polos and sangsih parts by assigning\
    \ `inst-polos` and `inst-sangsih` to alternating notes."
    $ Sig.call0t $ \_args deriver -> do
        events <- deriver
        (polos, sangsih) <- get_pasang
        return $ snd $ Post.map_state (nyogcag polos sangsih)
            (True, LEvent.events_of events) events

get_pasang :: Derive.Deriver (Score.Instrument, Score.Instrument)
get_pasang = (,) <$> Derive.get_val inst_polos <*> Derive.get_val inst_sangsih

inst_polos :: TrackLang.ValName
inst_polos = "inst-polos"

inst_sangsih :: TrackLang.ValName
inst_sangsih = "inst-sangsih"

{- If it's post, I can't ask the note call to decide on the instrument, so
    I can't put the polos/sangsih decision in the instrument note call.
    So I think I have to put this in the inst call.

    Or, I can get inst-polos and inst-sangsih from the environ.  That way
    I can interlock any two instruments, not just those that provide it.
-}

nyogcag :: Score.Instrument -> Score.Instrument -> (Bool, [Score.Event])
    -> Score.Event -> ((Bool, [Score.Event]), [Score.Event])
nyogcag polos sangsih (is_polos, next) event =
    ((not is_polos, drop 1 next), [with_inst])
    where
    with_inst = event
        { Score.event_instrument = if is_polos then polos else sangsih }
