-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for solkattu.  This re-exports "Derive.Solkattu.Db" so I can
-- find pre-defined korvais.
--
-- E.g.:
-- > return $ LSol.search $ LSol.has_instrument "kendang_tunggal"
-- > 59: .... etc
-- > LSol.insert_k1 True 0 (LSol.korvais !! 59) 0
module Cmd.Repl.LSol (
    module Cmd.Repl.LSol
    , module Derive.Solkattu.Db
) where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Ui as Ui

import qualified Cmd.Cmd as Cmd
import qualified Cmd.ModifyNotes as ModifyNotes
import qualified Cmd.Selection as Selection

import qualified Derive.Expr as Expr
import qualified Derive.ScoreTypes as ScoreTypes
import qualified Derive.ShowVal as ShowVal
import Derive.Solkattu.Db
import qualified Derive.Solkattu.Instrument.ToScore as ToScore
import qualified Derive.Solkattu.Korvai as Korvai
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu

import qualified Perform.Pitch as Pitch
import Global
import Types


insert_m :: Cmd.M m => Bool -> TrackTime -> Korvai.Korvai -> Int -> m ()
insert_m = insert Korvai.mridangam

insert_k1 :: Cmd.M m => Bool -> TrackTime -> Korvai.Korvai -> Int -> m ()
insert_k1 = insert Korvai.kendang_tunggal

insert_r :: Cmd.M m => Bool -> TrackTime -> Korvai.Korvai -> Int -> m ()
insert_r = insert Korvai.reyong

insert_sargam :: Cmd.M m => TrackTime -> Korvai.Korvai -> Int -> m ()
insert_sargam = insert Korvai.sargam True

-- | Insert the korvai at the selection.
-- TODO implement ModifyNotes.replace_tracks to clear existing notes first
insert :: (Pretty stroke, Cmd.M m) => Korvai.GetInstrument stroke -> Bool
    -> TrackTime -> Korvai.Korvai -> Int -> m ()
insert instrument realize_patterns akshara_dur korvai index = do
    (strokes, warning) <- Ui.require_right id $
        Korvai.realize instrument realize_patterns korvai !! index
    unless (Text.null warning) $ Ui.throw warning
    (block_id, _, track_id, at) <- Selection.get_insert
    let note_track = to_note_track (Korvai.get_to_score instrument)
            akshara_dur at strokes
    ModifyNotes.write_tracks block_id [track_id] [note_track]

to_note_track :: ToScore.ToScore stroke -> TrackTime -> TrackTime
    -> [(Sequence.Tempo, Realize.Note stroke)] -> ModifyNotes.NoteTrack
to_note_track to_score stretch shift strokes =
    ModifyNotes.NoteTrack (mk_events notes) control_tracks
    where
    controls :: [(Text, [ToScore.Event])]
    (notes, controls) = to_score $ Sequence.tempo_to_duration strokes
    pitches = fromMaybe [] $ lookup "*" controls
    pitch_track = if null pitches then Nothing
        else Just (ModifyNotes.Pitch Pitch.empty_scale, mk_events pitches)
    control_tracks = Map.fromList $ maybe id (:) pitch_track $
        [ (ModifyNotes.Control (ScoreTypes.Control control), mk_events events)
        | (control, events) <- controls
        , control /= "*"
        ]
    mk_events = Events.from_list . map mk_event
    mk_event (start, dur, text) = Event.event (mk_time start) (mk_time dur) text
    mk_time = (+shift) . (*stretch) . realToFrac

realize_korvai ::
    (Expr.ToExpr (Realize.Stroke stroke), Pretty stroke, Ui.M m) =>
    Korvai.GetInstrument stroke -> Bool -> Korvai.Korvai -> Int
    -> m Events.Events
realize_korvai instrument realize_patterns korvai index = do
    (strokes, warning) <- Ui.require_right id $
        Korvai.realize instrument realize_patterns korvai !! index
    unless (Text.null warning) $ Ui.throw warning
    return $ Events.from_list $ strokes_to_events strokes

strokes_to_events :: Expr.ToExpr (Realize.Stroke a) =>
    [(Sequence.Tempo, Realize.Note a)] -> [Event.Event]
strokes_to_events strokes =
    [ Event.event (realToFrac start) (if has_dur then realToFrac dur else 0)
        (ShowVal.show_val expr)
    | (start, dur, Just (expr, has_dur)) <- zip3 starts durs (map to_expr notes)
    ]
    where
    starts = scanl (+) 0 durs
    (durs, notes) = unzip $ Sequence.tempo_to_duration strokes
    to_expr s = case s of
        Realize.Note stroke -> Just (Expr.to_expr stroke, False)
        Realize.Pattern p -> Just (Expr.to_expr p, True)
        Realize.Space Solkattu.Rest -> Nothing
        Realize.Space Solkattu.Sarva -> Nothing -- TODO
