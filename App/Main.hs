module App.Main where

import Control.Monad
import qualified Control.Concurrent as Concurrent

import qualified Util.Thread as Thread
import qualified Util.Seq as Seq
import qualified Util.Log as Log

-- testing uses these
import qualified Ui.Ui as Ui
import qualified Ui.Color as Color
import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.Event as Event

import qualified Midi.Midi as Midi
import qualified Msg.Responder as Responder

{-
    Initialize UI
    Initialize MIDI, get midi devs

    Start responder thread.  It has access to: midi devs, midi input chan, ui
    msg input chan.

    Create an empty block with a few tracks.
-}
run_midi app = Midi.initialize (Midi.catch app
    (\ (Midi.Error err) -> Log.error ("midi error: " ++ err)))

main = Ui.initialize $ \msg_chan -> Midi.initialize $ do
    Log.notice "app starting"
    midi_chan <- Midi.get_read_chan

    let get_msg = Responder.read_msg_chans msg_chan midi_chan
    devs <- Midi.devices
    putStrLn "devices:"
    putStrLn $ "\t" ++ Seq.join "\n\t" (map Midi.device_name devs)

    let input_dev = filter Midi.device_input devs
    when (not (null input_dev)) $ do
        putStrLn $ "open input " ++ Midi.device_name (head input_dev)
        Midi.open_read_device (head (filter Midi.device_input devs))
    let output_dev = filter Midi.device_output devs
    when (not (null output_dev)) $ do
        putStrLn $ "open output " ++ Midi.device_name (head output_dev)
        Midi.open_write_device (head output_dev)

    make_test_block

    (Responder.responder_thread get_msg Midi.write_msg)


-- * test

make_test_block = do
    block <- Block.create block_config
    mlist <- marklist 64
    track_ruler <- Ruler.create (ruler_config [mlist])
    overlay_ruler <- Ruler.create (overlay_config (ruler_config [mlist]))

    t1 <- Track.create Color.white

    -- Insert some tracks before creating the view, some after.
    Block.insert_track block 0 (Block.T t1 overlay_ruler) 70

    view <- Block.create_view (0, 0) (100, 200) block track_ruler view_config

    Block.insert_track block 1 (Block.D Color.blue) 5
    Block.insert_track block 2 (Block.R track_ruler) 15
    Block.insert_track block 3 (Block.T t1 overlay_ruler) 50

-- * setup

major n = Ruler.Mark 1 3 (Color.rgba 0.45 0.27 0 0.35) (show n) 0 0
minor = Ruler.Mark 2 2 (Color.rgba 1 0.39 0.2 0.35) "" 0 0
marklist n = Ruler.create_marklist $ take n $ zip (map TrackPos [0, 10 ..]) m44


m44 = concatMap (\n -> [major n, minor, minor, minor]) [0..]

ruler_bg = Color.rgb 1 0.85 0.5
ruler_config marklists = Ruler.Config marklists ruler_bg True False False
-- Convert a ruler config for an overlay ruler.
overlay_config config = config
    { Ruler.config_show_names = False
    , Ruler.config_use_alpha = True
    , Ruler.config_full_width = True
    }

block_config = Block.Config
    { Block.config_select_colors =
        let sel = Color.alpha 0.3 . Color.lighten 0.8 in
            [sel Color.blue, sel Color.green, sel Color.red]
    , Block.config_bg_color = Color.gray8
    , Block.config_track_box_color = Color.rgb 0.25 1 1
    , Block.config_sb_box_color = Color.rgb 0.25 1 1
    }

view_config = Block.ViewConfig
    { Block.vconfig_zoom_speed = 1
    , Block.vconfig_block_title_height = 20
    , Block.vconfig_track_title_height = 20
    , Block.vconfig_sb_size = 12
    , Block.vconfig_ruler_size = 18
    , Block.vconfig_status_size = 16
    }

track_bg = Color.white

event s dur = Event.Event s (TrackPos dur) Color.gray7 text_style False
text_style = TextStyle Helvetica [] 12 Color.black
