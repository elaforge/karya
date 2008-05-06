{- | The Render layer takes the Score data and turns it into sound.  It has
    several backends, such as MIDI, OSC, csound, etc.

- Start up rendering threads and return control.

-}
module Derive.Render where
import qualified Control.Exception as Exception
import Control.Monad
import qualified Data.IORef as IORef

import qualified Util.Thread as Thread

import qualified Ui.Block as Block

import qualified Derive.Derive as Derive
import qualified Derive.Player as Player
import qualified Derive.Render.Midi as RenderMidi


-- * render

-- | Given a fully derived score, dispatch on the relevant backends.
--
-- Return a Player.Control which can be used to stop and restart the player.
render :: Player.Info -> Block.BlockId -> Derive.Score -> IO Player.Transport
render player_info block_id score = do
    transport <- fmap Player.Transport (IORef.newIORef Player.Play)
    state <- Player.state player_info transport block_id
    Thread.start_thread "render midi" $
        RenderMidi.render state score `Exception.catch` \exc -> do
            Player.write_status (Player.state_player_chan state)
                (Player.Died exc) (Player.state_block_id state)
            -- The Renderer died so it doesn't need the Stop, but I still need
            -- to stop the updater.
            Player.send_transport transport Player.Stop
    return transport
