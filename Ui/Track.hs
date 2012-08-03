{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The 'Track' type and supporting functions.
module Ui.Track where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Ui.Color as Color
import qualified Ui.Events as Events
import qualified Perform.Signal as Signal
import qualified App.Config as Config
import Types


-- * track

data Track = Track {
    track_title :: !String
    , track_events :: !Events.Events
    , track_bg :: !Color.Color
    , track_render :: !RenderConfig
    -- | Analogous to 'Ui.Block.block_integrated', this is the source track if
    -- this track was integrated from another.
    } deriving (Eq, Show, Read)

instance Pretty.Pretty Track where
    format (Track title events _bg render) =
        Pretty.record (Pretty.text "Track" Pretty.<+> Pretty.format title
                Pretty.<+> Pretty.format render)
            [ ("events", Pretty.format events)
            ]

-- | Construct a new Track.
track :: String -> Events.Events -> Track
track title events = Track
    { track_title = title
    , track_events = events
    , track_bg = Config.track_bg
    , track_render = no_render
    }

empty :: Track
empty = track "" Events.empty

instance DeepSeq.NFData Track where
    rnf track = DeepSeq.rnf (track_events track) `seq` ()

modify_events :: (Events.Events -> Events.Events) -> Track -> Track
modify_events f track@(Track { track_events = events }) =
    track { track_events = f events }

set_events :: Events.Events -> Track -> Track
set_events events = modify_events (const events)

-- * track signal

data RenderConfig = RenderConfig {
    render_style :: !RenderStyle
    , render_color :: !Color.Color
    } deriving (Eq, Show, Read)

no_render :: RenderConfig
no_render = RenderConfig NoRender Config.render_color

instance Pretty.Pretty RenderConfig where
    pretty (RenderConfig style color) = Pretty.pretty (style, color)

data RenderStyle = NoRender | Line | Filled
    deriving (Eq, Show, Read)

instance Pretty.Pretty RenderStyle where pretty = show

set_render_style :: RenderStyle -> Track -> Track
set_render_style style track =
    track { track_render = (track_render track) { render_style = style } }

-- | Each TrackId has a TrackSignal associated with it, or log msgs if the
-- signal derivation failed.  I include both possibilities because I want to
-- ensure that TrackSignals can be derived lazily.  This is because in
-- addition to UI rendering, some Cmds may inspect TrackSignals but I don't
-- know which ones in advance.  Since getting the track signal means deriving
-- the control track again to get it in ScoreTime, I want to avoid doing that
-- if no one is interested in the result.
--
-- The log msgs for a successful derivation are discarded, I assume that
-- anything interesting will have been logged by the normal signal derivation.
--
-- There's no particular reason a ruler couldn't also have a signal in it,
-- except that it might look a little crowded.  But RulerId isn't supported.
-- If there's ever a need I can add it.
--
-- TODO this is by TrackId, but shouldn't it be by (BlockId, TrackNum)?  The
-- same track could derive a different signal in a different context.
type TrackSignals = Map.Map TrackId (Either [Log.Msg] TrackSignal)

-- | Similar to 'Derive.Derive.TrackWarp's, the signal generated by signal
-- tracks is stashed away in TrackSignals during derivation so it can be sent
-- to the UI for display.
--
-- Signals are in real time, but the UI wants to display them in score time.
-- If the block happens to have a linear warp then the mapping is trivial, and
-- I don't have to bother generating another signal just for display.  However,
-- if there is a non-trivial warp, the signal will have to be rederived in an
-- id warp.
data TrackSignal = TrackSignal {
    ts_signal :: !Signal.Display
    , ts_shift :: !ScoreTime
    , ts_stretch :: !ScoreTime
    , ts_scale_map :: Maybe ScaleMap
    } deriving (Show, Eq)

instance Pretty.Pretty TrackSignal where
    format (TrackSignal sig shift stretch scale) =
        Pretty.record (Pretty.text "TrackSignal"
                Pretty.<+> Pretty.format (shift, stretch))
            [("signal", Pretty.format sig), ("scale_map", Pretty.format scale)]

-- | ScaleMaps are sorted by their scale degree number.
newtype ScaleMap = ScaleMap [ValName] deriving (Show, Eq, Pretty.Pretty)
newtype ValName = ValName (Double, String) deriving (Show, Eq, Pretty.Pretty)

make_scale_map :: [(String, Double)] -> ScaleMap
make_scale_map = ScaleMap . map ValName . List.sort . map (\(a, b) -> (b, a))
