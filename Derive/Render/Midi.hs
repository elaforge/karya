{- | 'render' function to play a derived score out the midi ports.

    ---- OUT OF DATE ----

        MidiRender

    Convert Score to midi.
    - Sends timestamped midi msgs to a midi player thread that tries to get the
    timing as close as possible.
    - Renders controller curves into a stream of midi msgs, depending on the
    controller resolution settings.
    - Maps pitches to midi note number + pitch bend.
    - Distributes notes to midi channels: if an instrument is given >1 channel,
    it will try to keep notes with overlapping note numbers or controller
    curves on separate channels, so you can e.g. pitch bend one note without
    affecting others, or play two notes on the same nn.
    - Keeps track of outgoing midi bandwidth for display to the user.
    - Saves and sends sysex dumps to configure synths.
    - Can also insert MIDI timing to sync other devices (can I sync cubase this
    way?)


        MidiRecord

    Receive timestamped midi msgs and convert them to Score, which can then be
    converted to Block format and inserted into a block.

        MidiThread

    Receive a stream of timestamped midi msgs and play them accurately.  Also
    receive midi msgs, timestamp them, and relay to the midi input chan.

    Normally the Msg thread is listening to the input chan so it can trigger
    Actions.  However, when recording or monitoring, MidiRecord gets them.  For
    monitoring, MidiRecord also sends incoming msgs back out again, possibly
    after remapping to a different channel and port.
-}
module Derive.Render.Midi where
import qualified Data.Maybe as Maybe
import Data.Function
import qualified Data.List as List

import qualified Util.Seq as Seq
import qualified Util.Log as Log

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.Track as Track
import qualified Ui.Event as Event

import qualified Midi.Midi as Midi
import qualified Derive.Derive as Derive
import qualified Derive.Player as Player
import qualified Derive.Timestamp as Timestamp
import qualified Derive.Twelve as Twelve


render :: Player.State -> Derive.Score -> IO ()
render state (Derive.Score name tracks) = do
    Log.notice $ "play score " ++ show name ++ " starting at "
        ++ show (Player.state_timestamp_offset state)
    let track_msgs =
            (map (render_midi state . Track.event_list . Track.track_events)
            . event_tracks_of) tracks
        -- Each track will be in order, so merge them together sorted.
        msgs = foldr (Seq.merge_by (compare `on` msg_ts)) [] track_msgs
    -- TODO block when I get n seconds ahead of now to avoid flooding the midi
    -- TODO stick around and listen for Stop, and flush the output if I get it
    mapM_ (Player.state_midi_writer state) msgs

msg_ts (_, ts, _) = ts

-- play_midi :: Player.State -> [(TrackPos, Event.Event)] -> IO ()
render_midi :: Player.State -> [(TrackPos, Event.Event)] -> [Midi.WriteMessage]
render_midi state pos_events = concatMap (render_note ts_offset) pos_events
    where
    ts_offset = Player.state_timestamp_offset state

-- | Bubblesort.  It's lazy.
sort2 cmp (a:b:zs) = case cmp a b of
    GT -> b : sort2 cmp (a:zs)
    _ -> a : sort2 cmp (b:zs)
sort2 cmp xs = List.sortBy cmp xs

render_note ts_offset (pos, event) =
    [ (wdev, start_ts, msg (Midi.NoteOn (event_key event + 12*5) vel))
    , (wdev, end_ts, msg (Midi.NoteOff (event_key event + 12*5) 0))
    ]
    where
    wdev = Midi.WriteDevice "XXX"
    start_ts = pos_to_timestamp ts_offset pos
    end_ts = pos_to_timestamp ts_offset (pos + Event.event_duration event)
    msg = Midi.ChannelMessage chan
    event_key event = (Twelve.to_midi_nn . Maybe.fromJust . Twelve.event_pitch
        . Event.event_text) event
    chan = 0
    vel = 78

pos_to_timestamp :: Timestamp.Timestamp -> TrackPos -> Timestamp.Timestamp
pos_to_timestamp ts_offset (TrackPos pos) =
    Timestamp.Timestamp pos * 20 + ts_offset

-- * util

-- TODO duplicated from Derive.Derive
-- this should go in a derive util module
event_tracks_of = Maybe.catMaybes . map event_track
event_track (Block.T track _) = Just track
event_track _ = Nothing
