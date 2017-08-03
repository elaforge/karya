-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for solkattu.  This re-exports "Derive.Solkattu.Db" so I can
-- find pre-defined korvais.
--
-- E.g.:
-- > return $ LSol.search $ LSol.has_instrument "kendang_tunggal"
-- > return $ LSol.search $ LSol.around_date (LSol.date 2017 7 10) 10
-- > 59: .... etc
-- > LSol.insert_k1 True 1 (LSol.korvais !! 59) 0
module Cmd.Repl.LSol (
    module Cmd.Repl.LSol
    , module Derive.Solkattu.Db
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.ParseText as ParseText
import qualified Util.Seq as Seq
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Ui as Ui

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Integrate as Integrate
import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.ModifyNotes as ModifyNotes
import qualified Cmd.Selection as Selection

import qualified Derive.Expr as Expr
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.ScoreTypes as ScoreTypes
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Solkattu.Db as Db
import Derive.Solkattu.Db hiding (realize)
import qualified Derive.Solkattu.Instrument.ToScore as ToScore
import qualified Derive.Solkattu.Korvai as Korvai
import qualified Derive.Solkattu.Metadata as Metadata
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Stack as Stack

import qualified Perform.Pitch as Pitch
import qualified App.ReplProtocol as ReplProtocol
import Global
import Types


-- * search

search_date :: Monad m => Int -> Int -> Int -> Integer -> m Text
search_date y m d days = return $ search $ around_date (date y m d) days

-- * realize

type Index = Int

insert_m :: Cmd.M m => Bool -> TrackTime -> Korvai.Korvai -> Index -> m ()
insert_m = insert Korvai.mridangam

insert_k1 :: Cmd.M m => Bool -> TrackTime -> Korvai.Korvai -> Index -> m ()
insert_k1 = insert Korvai.kendang_tunggal

insert_r :: Cmd.M m => Bool -> TrackTime -> Korvai.Korvai -> Index -> m ()
insert_r = insert Korvai.reyong

insert_sargam :: Cmd.M m => TrackTime -> Korvai.Korvai -> Index -> m ()
insert_sargam = insert Korvai.sargam True

-- | Insert the korvai at the selection.
-- TODO implement ModifyNotes.replace_tracks to clear existing notes first
insert :: (Pretty stroke, Cmd.M m) => Korvai.Instrument stroke -> Bool
    -> TrackTime -> Korvai.Korvai -> Index -> m ()
insert instrument realize_patterns akshara_dur korvai index = do
    (block_id, _, track_id, at) <- Selection.get_insert
    note_track <- realize instrument realize_patterns korvai index akshara_dur
        at
    ModifyNotes.write_tracks block_id [track_id] [note_track]

realize :: (Ui.M m, Pretty stroke) => Korvai.Instrument stroke -> Bool
    -> Korvai.Korvai -> Index -> TrackTime -> TrackTime
    -> m ModifyNotes.NoteTrack
realize instrument realize_patterns korvai index akshara_dur at = do
    (strokes, warning) <- Ui.require_right id
        <=< Ui.require ("no korvai at index " <> showt index) $
            Seq.at (Korvai.realize instrument realize_patterns korvai) index
    unless (Text.null warning) $ Ui.throw warning
    return $ to_note_track (Korvai.inst_to_score instrument) akshara_dur at
        strokes

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
    mk_event (start, dur, text) = place shift stretch $
        Event.event (realToFrac start) (realToFrac dur) text

place :: TrackTime -> TrackTime -> Event.Event -> Event.Event
place shift stretch = (Event.duration_ %= (*stretch))
    . (Event.start_ %= ((+shift) . (*stretch)))

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


-- * integrate

-- | Find the korvai, do 'integrate_track' for it, and open an editor on the
-- source file.  The editor has bindings to 'reintegrate' after an edit.
edit_new :: Cmd.M m => Korvai.Korvai -> Index -> Text -> m ReplProtocol.Result
edit_new korvai index instrument = do
    key <- integrate_track korvai index instrument
    edit key

-- | Find the SourceKey of integrated events around the cursor.
get_key :: Cmd.M m => m Block.SourceKey
get_key = do
    (_, events) <- Selection.track_events
    Cmd.require "no key" $ msum $ map event_key events

edit :: Ui.M m => Block.SourceKey -> m ReplProtocol.Result
edit key = do
    (korvai, _, _) <- Ui.require ("no korvai for " <> showt key) $
        get_by_key key
    let line_number = fromMaybe 0 $ ParseText.maybe_parse ParseText.p_nat
            =<< Seq.head (Metadata.get Metadata.t_line_number korvai)
        fname = module_to_fname <$>
            Seq.head (Metadata.get Metadata.t_module korvai)
    return $ ReplProtocol.Edit $ ReplProtocol.Editor
        { _file =
            maybe (ReplProtocol.Text "no file") ReplProtocol.FileName fname
        , _line_number = line_number
        , _on_save = Nothing
        , _on_send = Just $ ":reload; LSol.reintegrate " <> showt key
        }

module_to_fname :: Text -> FilePath
module_to_fname = untxt . (<>".hs") . Text.replace "." "/"

reintegrate :: Ui.M m => Block.SourceKey -> m ()
reintegrate key = do
    (korvai, index, inst) <- Ui.require ("no korvai for " <> showt key) $
        get_by_key key
    -- TODO I need to store realize_patterns and akshara_dur somewhere.
    (notes, controls) <- convert_note_track key <$> case inst of
        Korvai.GInstrument inst ->
            realize inst True korvai index akshara_dur start
    Integrate.manual_integrate key notes controls
    where
    akshara_dur = 1
    start = 0

convert_note_track :: Block.SourceKey -> ModifyNotes.NoteTrack
    -> (Convert.Track, [Convert.Track])
convert_note_track key (ModifyNotes.NoteTrack notes controls) =
    ( convert_track ParseTitle.note_track notes
    , map convert (Map.toAscList controls)
    )
    where
    convert (ModifyNotes.Pitch scale_id, events) =
        convert_track (ParseTitle.scale_to_title scale_id) events
    convert (ModifyNotes.Control control, events) =
        convert_track (ParseTitle.control_to_title (Score.untyped control))
            events
    convert_track title = Convert.Track title
        . map (add_stack key) . Events.ascending

add_stack :: Block.SourceKey -> Event.Event -> Event.Event
add_stack key event =
    Event.stack_ #= Just (Event.Stack stack (Event.start event)) $ event
    where stack = Stack.add (Stack.Call key) Stack.empty

event_key :: Event.Event -> Maybe Block.SourceKey
event_key event = case Event.stack event of
    Just (Event.Stack stack _) -> case Stack.innermost stack of
        Stack.Call key : _ -> Just key
        _ -> Nothing
    Nothing -> Nothing

-- | Get the SourceKey, create an empty track with that.
integrate_track :: Cmd.M m => Korvai.Korvai -> Index -> Text
    -> m Block.SourceKey
integrate_track korvai index instrument = do
    key <- Cmd.require "can't get key" $ korvai_key korvai index instrument
    view_id <- Cmd.get_focused_view
    track_id <- Create.track_and_widen False view_id 9999
    block_id <- Ui.block_id_of view_id
    Ui.set_integrated_manual block_id key $
        Just [Block.empty_destination track_id]
    reintegrate key
    return key

-- data NoteTrack = NoteTrack Events.Events Controls

korvai_key :: Korvai.Korvai -> Index -> Text -> Maybe Block.SourceKey
korvai_key korvai index instrument = do
    (mod, variable) <- qualified_name korvai
    return $ Text.intercalate "/" [mod, variable, showt index, instrument]

qualified_name :: Korvai.Korvai -> Maybe (Text, Text)
qualified_name korvai =
    case (get Metadata.t_module, get Metadata.t_variable_name) of
        ([mod], [variable]) -> Just (mod, variable)
        _ -> Nothing
    where
    get tag = Metadata.get tag korvai

get_by_key :: Block.SourceKey
    -> Maybe (Korvai.Korvai, Index, Korvai.GInstrument)
get_by_key key = do
    -- (mod, variable, index) <- split3 key
    [mod, variable, index, instrument] <- return $ Text.splitOn "/" key
    index <- ParseText.maybe_parse ParseText.p_nat index
    korvai <- List.find (matches mod variable) Db.korvais
    instrument <- Map.lookup instrument Korvai.instruments
    return (korvai, index, instrument)
    where
    -- split3 t = case Text.splitOn "/" t of
    --     [a, b, c] -> Just (a, b, c)
    --     _ -> Nothing
    matches mod variable korvai = case qualified_name korvai of
        Just (m, v) -> m == mod && v == variable
        _ -> False
