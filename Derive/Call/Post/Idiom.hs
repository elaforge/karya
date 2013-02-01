-- | Idiomatic things for various instruments.
module Derive.Call.Post.Idiom where
import qualified Data.List as List

import Util.Control
import qualified Util.Seq as Seq
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, control)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import Types


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("arp-pizz", c_arp_pizz)
    ]

c_arp_pizz :: Derive.NoteCall
c_arp_pizz = Derive.transformer "arp-pizz"
    "Arpeggiate simultaneous notes with `+pizz`. The order is arbitrary but\
    \ probably in track order.  TODO sort by pitch?" $
    Sig.callt (defaulted "time" (control "arp-pizz-time" 0.02)
        "Insert this much time between each note.") $
    \time _args deriver -> arp_pizz time =<< deriver

arp_pizz :: TrackLang.ValControl -> Derive.Events -> Derive.EventDeriver
arp_pizz time = map_contemporary 0.025 (Score.has_attribute Attrs.pizz) $
    \(event :| chord) -> do
        let start = Score.event_start event
        time <- RealTime.seconds <$> Util.control_at time start
        return [Score.move (+t) event
            | (t, event) <- zip (Seq.range_ 0 time) (event : chord)]

-- TODO I know there's a better word than contemporary
-- coincident?  simultenous?
map_contemporary :: RealTime
    -- ^ events starting closer than this amount are considered contemporary
    -> (Score.Event -> Bool)
    -- ^ only process events that pass this predicate
    -> (NonEmpty Score.Event -> Derive.Deriver [Score.Event])
    -- ^ process contemporary events
    -> Derive.Events -> Derive.EventDeriver
map_contemporary eta accept f = go
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
