module Derive.Call.Ornament where
import Util.Control
import Ui
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import Derive.CallSig (optional)
import qualified Derive.Derive as Derive

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("`mordent`", c_mordent 1)
    , ("`rmordent`", c_mordent (-1))
    ]

-- | Mordent.
--
-- TODO not scale aware.  It seems like I should have some diatonic awareness,
-- so I could ask the scale for a diatonic transposition.
c_mordent :: Double -> Derive.NoteCall
c_mordent default_neighbor = Derive.stream_generator "mordent" $
    Note.inverting $ \args -> CallSig.call2 args
    (optional "neighbor" default_neighbor, optional "vel" 0.3) $
    \neighbor vel ->
        mordent grace_dur (Derive.passed_extent args) vel
            (Pitch.Degree neighbor)
    where grace_dur = RealTime.seconds (1/12)

mordent :: RealTime -> (ScoreTime, ScoreTime) -> Signal.Y -> Pitch.Degree
    -> Derive.EventDeriver
mordent grace_dur (start, dur) velocity_scale neighbor = do
    pos <- Derive.real start
    pitch <- Util.degree pos
    vel <- (*velocity_scale) <$> Util.velocity pos
    grace_notes pos
        [ (grace_dur, Util.simple_note pitch vel)
        , (grace_dur, Util.simple_note (pitch + neighbor) vel)
        ]
        <> Derive.d_place start dur Util.note

grace_notes :: RealTime -> [(RealTime, Derive.EventDeriver)]
    -> Derive.EventDeriver
grace_notes start notes = Derive.d_merge placed
    where
    placed = map (\(p, (dur, deriver)) -> place (start - p) dur deriver)
        (zip pos notes)
    pos = reverse $ drop 1 $ scanl (+) 0 $ map fst notes

place :: RealTime -> RealTime -> Derive.Deriver d -> Derive.Deriver d
place start dur d = do
    rstart <- Derive.score start
    rend <- Derive.score (start + dur)
    Derive.d_place rstart (rend - rstart) d
