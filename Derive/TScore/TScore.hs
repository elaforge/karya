-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Parse tscore, check and postprocess it, and convert to Ui.State.
module Derive.TScore.TScore where
import qualified Util.Debug as Debug
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified App.Config as Config
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.TScore.Check as Check
import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.T as T

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui

import           Global
import           Types


score :: Text -> Either Error Ui.State
score text = do
    T.Score defs <- first txt $ Parse.parse_score text
    first pretty $ Ui.exec Ui.empty $ do
        Ui.set_namespace Parse.default_namespace
        foldM interpret Check.default_config defs
    -- The first block becomes the root_id implicitly.

type Error = Text

interpret :: Ui.M m => Check.Config -> T.Toplevel -> m Check.Config
interpret config (T.ToplevelDirective dir) =
    Ui.require_right ("toplevel: " <>) $ Check.parse_directive dir config
interpret config (T.BlockDefinition block) = do
    interpret_block config block
    return config

-- TODO map config_meter to a ruler
interpret_block :: Ui.M m => Check.Config -> T.Block -> m BlockId
interpret_block config
        (T.Block block_id directives title (T.Tracks tracks)) = do
    block_config <- Ui.require_right ((pretty block_id <> ": ")<>) $
        Check.parse_directives config directives
    track_ids <- concatMapM (interpret_track block_config block_id)
        (zip [1..] tracks)
    let tracks =
            [ Block.track (Block.TId tid Ui.no_ruler) Config.track_width
            | tid <- track_ids
            ]
    Ui.create_block (Id.unpack_id block_id) title $
        Block.track (Block.RId Ui.no_ruler) Config.ruler_width : tracks

interpret_track :: Ui.M m => Check.Config -> BlockId -> (Int, T.Track)
    -> m [TrackId]
interpret_track config block_id (tracknum, track) = do
    tracks <- Ui.require_right id $
        track_events config block_id tracknum track
    mapM (uncurry Ui.create_track) tracks

track_events :: Check.Config -> BlockId -> TrackNum -> T.Track
    -> Either Error [(Id.Id, Track.Track)]
track_events config block_id tracknum (T.Track title tokens) = do
    tokens <- first (Check.show_error (Check.config_meter config)) $
        sequence $ Check.process config tokens
    let pitches = map pitch_event $ Seq.map_maybe_snd T.note_pitch tokens
    let notes = Events.from_list (map (uncurry note_event) tokens)
    let ntitle = TextUtil.joinWith " | " ParseTitle.note_track title
    return $
        (make_track_id block_id tracknum False, Track.track ntitle notes)
        : if null pitches then [] else
            [ (make_track_id block_id tracknum True
              , Track.track "*" (Events.from_list pitches)
              )
            ]

make_track_id :: BlockId -> TrackNum -> Bool -> Id.Id
make_track_id block_id tracknum is_pitch =
    Id.id ns $
        ident <> ".t" <> showt tracknum <> if is_pitch then ".pitch" else ""
    where
    (ns, ident) = Id.un_id $ Id.unpack_id block_id

note_event :: T.Time -> T.Note (Maybe Text) T.Time -> Event.Event
note_event start (T.Note (T.Call call) _ dur) =
    Event.event (track_time start) (track_time dur) call

pitch_event :: (T.Time, Text) -> Event.Event
pitch_event (start, pitch) = Event.event (track_time start) 0 pitch

track_time :: T.Time -> TrackTime
track_time = realToFrac
