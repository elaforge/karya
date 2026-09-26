-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Derive events on a note track.  This is the Derive equivalent of
    "Cmd.NoteTrack", but has a different name to avoid clashes.
-}
module Derive.Note (dNoteTrack, stashSignalIfWanted, trackInfo) where
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Tree as Tree

import qualified Util.Lists as Lists
import qualified Derive.Control as Control
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Eval as Eval
import qualified Derive.EvalTrack as EvalTrack
import qualified Derive.PSignal as PSignal
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stream as Stream

import qualified Perform.Signal as Signal
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree

import           Global
import           Types


-- * note track

-- | Top level deriver for note tracks.
{-# SCC dNoteTrack #-}
dNoteTrack :: ([TrackTree.EventsNode] -> Derive.NoteDeriver)
    -- ^ This is used to derive orphans, as documented by
    -- 'EvalTrack.derive_note_track'.
    -> TrackTree.EventsNode -> Derive.NoteDeriver
dNoteTrack deriveTracks (Tree.Node track subs) =
    title $ deriveNotes deriveTracks (trackInfo track subs)
    where
    title = withTitle subs (TrackTree.trackEnd track)
        (TrackTree.trackTitle track)

-- | Note tracks can also have track signals, extracted from the events they
-- produce.
stashSignalIfWanted :: Stream.Stream Score.Event -> TrackTree.Track
    -> Derive.Deriver ()
stashSignalIfWanted events track =
    whenJustM (Control.renderOf track) $ \(blockId, trackId, maybeSource) ->
        whenJust maybeSource $ \source ->
            stashSignal blockId trackId source events

stashSignal :: BlockId -> TrackId -> Track.RenderSource
    -> Stream.Stream Score.Event -> Derive.Deriver ()
stashSignal blockId trackId source events =
    Control.stashSignal blockId trackId $
        extractTrackSignal source (Stream.eventsOf events)

-- | Extract the signals that 'Track.RenderSource' wants, trim them, and concat
-- into a single unwarped signal that can be stashed away for later display.
extractTrackSignal :: Track.RenderSource -> [Score.Event] -> Signal.Control
extractTrackSignal source events = mconcat $ case source of
    Track.Control control -> mapMaybe (extractControl control) events
    Track.Pitch control -> mapMaybe (extractPitch control) events
    where
    -- Since these signals will be concatenated into one signal, I don't
    -- want one event's control at 0 to wipe out the previous events.
    extractControl control event =
        Signal.clipBefore (Score.eventMin event) . ScoreT.valOf <$>
            Score.eventControl control event
    extractPitch pcontrol event =
        convert event <$> Score.eventNamedPitch pcontrol event
    convert event psig = Signal.coerce $ fst $ PSignal.toNn $
        PSignal.clipBefore (Score.eventMin event) $
        PSignal.applyControls (Score.eventControls event) psig

withTitle :: TrackTree.EventsTree -> ScoreTime -> Text -> Derive.NoteDeriver
    -> Derive.NoteDeriver
withTitle subs end title deriver
    | Text.all Char.isSpace title = deriver
    | otherwise = do
        trackExpr <- Derive.requireRight ("track title: "<>) $
            ParseTitle.parseNote title
        Eval.evalTransformers ctx (NonEmpty.toList trackExpr) deriver
    where
    ctx = (Derive.dummyContext 0 end "note track")
        { Derive.ctxSubTracks = subs }

deriveNotes :: ([TrackTree.EventsNode] -> Derive.NoteDeriver)
    -> EvalTrack.TrackInfo Score.Event -> Derive.NoteDeriver
deriveNotes deriveTracks tinfo = do
    state <- Derive.get
    let (streams, threaded, collect) =
            EvalTrack.deriveNoteTrack deriveTracks state tinfo
    Internal.mergeCollect collect
    Internal.setThreaded threaded
    pure $ Stream.mergeAscLists streams

trackInfo :: TrackTree.Track -> [TrackTree.EventsNode]
    -> EvalTrack.TrackInfo Score.Event
trackInfo track subs = EvalTrack.TrackInfo
    { tinfoTrack = track
    , tinfoSubTracks = subs
    , tinfoType = ParseTitle.NoteTrack
    , tinfoGetLastVal = Lists.last
    }
