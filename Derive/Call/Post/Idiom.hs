-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Idiomatic things for various instruments.
module Derive.Call.Post.Idiom where
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Seq as Seq
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, control)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps []
    [ ("pizz-arp", c_pizz_arp)
    , ("avoid-overlap", c_avoid_overlap)
    ]

-- * pizz arp

c_pizz_arp :: Derive.Transformer Derive.Note
c_pizz_arp = Derive.transformer "pizz-arp" (Tags.postproc <> Tags.idiom)
    "Arpeggiate simultaneous notes with `+pizz`. The order is arbitrary but\
    \ probably in track order.  TODO sort by pitch?" $
    Sig.callt (defaulted "time" (control "pizz-arp-time" 0.02)
        "Insert this much time between each note.") $
    \time _args deriver -> Lily.when_lilypond deriver $
        pizz_arp time =<< deriver

pizz_arp :: TrackLang.ValControl -> Derive.Events -> Derive.NoteDeriver
pizz_arp time = map_simultaneous 0.025 (Score.has_attribute Attrs.pizz) $
    \(event :| chord) -> do
        let start = Score.event_start event
        time <- RealTime.seconds <$> Util.control_at time start
        return [Score.move (+t) event
            | (t, event) <- zip (Seq.range_ 0 time) (event : chord)]

map_simultaneous :: RealTime
    -- ^ events starting closer than this amount are considered simultaneous
    -> (Score.Event -> Bool)
    -- ^ only process events that pass this predicate
    -> (NonEmpty Score.Event -> Derive.Deriver [Score.Event])
    -- ^ process simultaneous events
    -> Derive.Events -> Derive.NoteDeriver
map_simultaneous eta accept f = go
    where
    go [] = return []
    go (LEvent.Log log : events) = (LEvent.Log log :) <$> go events
    go (LEvent.Event event : events)
        | accept event = collect event events
        | otherwise = (LEvent.Event event :) <$> go events
    collect event events = do
        out <- f (event :| wanted)
        out_rest <- go rest
        return $ map LEvent.Event (out ++ unwanted) ++ map LEvent.Log logs
            ++ out_rest
        where
        start = Score.event_start event
        (with, rest) = span
            (LEvent.log_or ((<=start) . subtract eta . Score.event_start))
            events
        (chord, logs) = LEvent.partition with
        (wanted, unwanted) = List.partition accept chord

-- * avoid overlap

c_avoid_overlap :: Derive.Transformer Derive.Note
c_avoid_overlap = Derive.transformer "avoid-overlap"
    (Tags.postproc <> Tags.idiom)
    "Notes with the same starting pitch are shortened so they don't overlap\
    \ with each other.  This simulates keyboard instruments, where you have\
    \ to release a key before striking the same key again. This also happens\
    \ to be what MIDI expects, since it's based on keyboards."
    $ Sig.callt (defaulted "time" 0.1
        "Ensure at least this much time between two notes of the same pitch.")
    $ \time _args deriver -> Lily.when_lilypond deriver $
        avoid_overlap time =<< deriver

avoid_overlap :: RealTime -> Derive.Events -> Derive.NoteDeriver
avoid_overlap time = return . Post.map_around go
    where
    go _ event future = case List.find same (takeWhile overlaps future) of
        Nothing -> event
        Just next -> Score.set_duration
                (Score.event_start next - time - Score.event_start event) event
        where
        overlaps next = Score.event_end event + time > Score.event_start next
        nn = Score.initial_nn event
        same next = Score.event_instrument event == Score.event_instrument next
            && Maybe.isJust nn && nn == Score.initial_nn next
