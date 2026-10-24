-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for solkattu.  This re-exports "Solkattu.Db" so I can
-- find pre-defined korvais.
--
-- E.g.:
-- > return $ LSol.search $ LSol.hasInstrument "kendang_tunggal"
-- > return $ LSol.search $ LSol.aroundDate (LSol.date 2017 7 10) 10
-- > 59: .... etc
-- > LSol.insert_k1 True 1 59 0
module Cmd.Repl.LSol (
    module Cmd.Repl.LSol
    , module Solkattu.Db
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.ParseA as ParseA
import qualified App.ReplProtocol as ReplProtocol
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Integrate as Integrate
import qualified Cmd.Integrate.Manual as Manual
import qualified Cmd.ModifyNotes as ModifyNotes
import qualified Cmd.Selection as Selection

import qualified Derive.Expr as Expr
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack

import qualified Perform.Pitch as Pitch
import qualified Solkattu.Db as Db
import           Solkattu.Db hiding (search, searchp)
import qualified Solkattu.Format.Format as Format
import qualified Solkattu.Instrument.ToScore as ToScore
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Metadata as Metadata
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Ui as Ui

import           Global
import           Types


type ScoreIndex = Int
type SectionIndex = Int

-- * search

search :: Monad m => [Korvai.Score -> Bool] -> m Text
search = pure . Db.formats . Db.searchAll id

-- * realize

insertM :: Cmd.M m => Bool -> TrackTime -> ScoreIndex -> SectionIndex -> m ()
insertM = insert Korvai.IMridangam

insertK1 :: Cmd.M m => Bool -> TrackTime -> ScoreIndex -> SectionIndex -> m ()
insertK1 = insert Korvai.IKendangTunggal

insertR :: Cmd.M m => Bool -> TrackTime -> ScoreIndex -> SectionIndex -> m ()
insertR = insert Korvai.IReyong

insertSargam :: Cmd.M m => TrackTime -> ScoreIndex -> SectionIndex -> m ()
insertSargam = insert Korvai.ISargam True

-- | Insert the korvai at the selection.
-- TODO implement ModifyNotes.replace_tracks to clear existing notes first
insert :: (Cmd.M m, Solkattu.Notation stroke,
        Expr.ToExpr (Realize.Stroke stroke), Ord stroke)
     => Korvai.Instrument stroke -> Bool -> TrackTime -> ScoreIndex
    -> SectionIndex -> m ()
insert instrument realizePatterns aksharaDur scoreI sectionI = do
    (blockId, _, trackId, at) <- Selection.getInsert
    noteTrack <- case Lists.at Db.scores scoreI of
        Just (_, Korvai.Single k) -> realize instrument realizePatterns k
            sectionI aksharaDur at
        Just (_, Korvai.Tani {}) -> Cmd.throw "tani not supported"
        Nothing -> Cmd.throw $
            "LSol.insert: index out of range: " <> showt scoreI
    ModifyNotes.writeTracks blockId [trackId] [noteTrack]

realize :: (Ui.M m, Solkattu.Notation stroke,
        Expr.ToExpr (Realize.Stroke stroke), Ord stroke)
    => Korvai.Instrument stroke -> Bool -> Korvai.Korvai -> SectionIndex
    -> TrackTime -> TrackTime -> m ModifyNotes.NoteTrack
realize instrument realizePatterns korvai sectionI aksharaDur at = do
    results <- Ui.requireRight id $ sequence $
        Korvai.realize instrument $ Korvai.index sectionI korvai
    -- TODO I could probbaly abstract more than just patterns
    let abstraction = if realizePatterns
            then mempty else Format.abstract Solkattu.GPattern
    let strokes = Format.makeGroupsAbstractScore abstraction $
            concatMap fst results
            -- snd is an alignment warning, which I can see well enough on the
            -- track already.
    pure $
        toNoteTrack (Korvai.instToScore instrument) aksharaDur at strokes

toNoteTrack :: ToScore.ToScore stroke -> TrackTime -> TrackTime
    -> [S.Flat g (Realize.Note stroke)] -> ModifyNotes.NoteTrack
toNoteTrack toScore stretch shift strokes =
    ModifyNotes.NoteTrack (mkEvents notes) controlTracks
    where
    controls :: [(Text, [ToScore.Event])]
    (notes, controls) = ToScore.fromStrokes toScore strokes
    pitches = fromMaybe [] $ lookup "*" controls
    pitchTrack = if null pitches then Nothing
        else Just (ModifyNotes.Pitch Pitch.emptyScale, mkEvents pitches)
    controlTracks = Map.fromList $ maybe id (:) pitchTrack $
        [ (ModifyNotes.Control (ScoreT.Control control), mkEvents events)
        | (control, events) <- controls
        , control /= "*"
        ]
    mkEvents = Events.fromList . map mkEvent
    mkEvent (start, dur, text) = place shift stretch $
        Event.event (realToFrac start) (realToFrac dur) text

place :: TrackTime -> TrackTime -> Event.Event -> Event.Event
place shift stretch = (Event.duration_ %= (*stretch))
    . (Event.start_ %= ((+shift) . (*stretch)))

strokesToEvents :: Expr.ToExpr (Realize.Stroke a)
    => [S.Flat g (Realize.Note a)] -> [Event.Event]
strokesToEvents strokes =
    [ Event.event (realToFrac start) (if hasDur then realToFrac dur else 0)
        (ShowVal.showVal expr)
    | (start, dur, Just expr, hasDur) <-
        List.zip4 starts durs
            (map ToScore.toExpr notes) (map S.hasSustain notes)
    ]
    where
    starts = scanl (+) 0 durs
    (durs, notes) = unzip $ S.flattenedNotes $ S.withDurations strokes

-- * integrate

-- | Find the korvai, do 'integrate_track' for it, and open an editor on the
-- source file.  The editor has bindings to 'reintegrate' after an edit.
editNew :: Cmd.M m => Korvai.Korvai -> SectionIndex -> Text
    -> m ReplProtocol.Result
editNew korvai index instrument = do
    key <- integrateTrack korvai index instrument
    edit key

-- | Find the SourceKey of integrated events around the cursor.
getKey :: Cmd.M m => m Block.SourceKey
getKey = do
    (_, events) <- Selection.trackEvents
    Cmd.require "no key" $ msum $ map eventKey events

edit :: Ui.M m => Block.SourceKey -> m ReplProtocol.Result
edit key = do
    (korvai, _, _) <- Ui.require ("no korvai for " <> showt key) $
        getByKey key
    let (module_, lineNumber, _) = Metadata.korvaiLocation korvai
        fname = moduleToFname module_
    pure $ ReplProtocol.Edit $ (:| []) $ ReplProtocol.Editor
        { _file = ReplProtocol.FileName fname
        , _lineNumber = lineNumber
        , _onSave = Nothing
        , _onSend = Just $ ":reload; LSol.reintegrate " <> showt key
        }

moduleToFname :: Text -> FilePath
moduleToFname = untxt . (<>".hs") . Text.replace "." "/"

-- | This can be called manually to reintegrate after a change, but is also
-- called automatically by 'edit'.
reintegrate :: Ui.M m => Block.SourceKey -> m ()
reintegrate key = do
    (korvai, index, inst) <- Ui.require ("no korvai for " <> showt key) $
        getByKey key
    -- TODO I need to store realize_patterns and akshara_dur somewhere.
    (note, controls) <- Manual.convertNoteTrack key <$> case inst of
        Korvai.GInstrument inst ->
            realize inst True korvai index aksharaDur start
    Integrate.manualIntegrate key note controls
    where
    aksharaDur = 1
    start = 0

eventKey :: Event.Event -> Maybe Block.SourceKey
eventKey event = case Event.stack event of
    Just (Event.Stack stack _) -> case Stack.innermost stack of
        Stack.Call key : _ -> Just key
        _ -> Nothing
    Nothing -> Nothing

-- | Get the SourceKey, create an empty track with that.
integrateTrack :: Cmd.M m => Korvai.Korvai -> SectionIndex -> Text
    -> m Block.SourceKey
integrateTrack korvai index instrument = do
    key <- Cmd.require "can't get key" $ korvaiKey korvai index instrument
    viewId <- Cmd.getFocusedView
    trackId <- Create.trackAndWiden False viewId 9999
    -- This is surely wrong, but I don't know the intended instrument here, and
    -- it can be fixed by hand, and it only happens the first time.
    Ui.setTrackTitle trackId (">" <> instrument)
    blockId <- Ui.blockIdOf viewId
    Ui.setIntegratedManual blockId key $
        Just [Block.emptyDestination key trackId []]
    reintegrate key
    pure key

korvaiKey :: Korvai.Korvai -> SectionIndex -> Text -> Maybe Block.SourceKey
korvaiKey korvai index instrument = do
    let (module_, _, variable) = Metadata.korvaiLocation korvai
    pure $ Text.intercalate "/" [module_, variable, showt index, instrument]

getByKey :: Block.SourceKey
    -> Maybe (Korvai.Korvai, SectionIndex, Korvai.GInstrument)
getByKey key = do
    -- (mod, variable, index) <- split3 key
    [mod, variable, index, instrument] <- pure $ Text.splitOn "/" key
    index <- ParseA.maybeParse ParseA.pNat index
    score <- List.find (matches mod variable) (map snd Db.scores)
    korvai <- case score of
        Korvai.Single k -> pure k
        Korvai.Tani {} -> error "Tani not supported yet"
    instrument <- List.find ((==instrument) . Korvai.ginstrumentName)
        Korvai.instruments
    -- This means reintegrate only works with a single section a single korvai.
    -- I can extend it if this turns out to be too restrictive.
    pure (korvai, index, instrument)
    where
    -- split3 t = case Text.splitOn "/" t of
    --     [a, b, c] -> Just (a, b, c)
    --     _ -> Nothing
    matches mod variable score = m == mod && v == variable
        where (m, _, v) = Metadata.scoreLocation score
