-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Parse tscore, check and postprocess it, and convert to Ui.State.
module Derive.TScore.TScore where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified App.Config as Config
import qualified Cmd.BlockConfig as BlockConfig
import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.Integrate.Merge as Merge

import qualified Derive.Stack as Stack
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


-- * types

type Error = Text

type Block = (BlockId, Text, [Track])

data Track = Track {
    -- TODO use (Title, Events) instead of Track
    track_note :: (TrackId, Track.Track)
    , track_controls :: [(TrackId, Track.Track)]
    } deriving (Eq, Show)

track_ids :: Track -> [TrackId]
track_ids (Track note controls) = fst note : map fst controls

-- * integrate

integrate :: Ui.M m => Text -> m [BlockId] -- ^ newly created blocks
integrate text = do
    blocks <- Ui.require_right id $ make_blocks text
    -- TODO trim blocks that disappeared like with tracks?
    mapMaybeM integrate_block blocks

integrate_block :: Ui.M m => Block -> m (Maybe BlockId)
integrate_block (block_id, title, tracks) = do
    exists <- Maybe.isJust <$> Ui.lookup_block block_id
    if exists
        then Ui.set_block_title block_id title
        else void $ Ui.create_block (Id.unpack_id block_id) title
            [Block.track (Block.RId Ui.no_ruler) Config.ruler_width]
    existing_tracks <- Set.fromList <$> Ui.track_ids_of block_id
    -- This will wipe out diffs... do I really want that?
    let new = Set.fromList $ concatMap track_ids tracks
    let gone = existing_tracks `Set.difference` new
    mapM_ Ui.destroy_track (Set.toList gone)
    mapM_ (integrate_track block_id) tracks
    unless exists $
        BlockConfig.toggle_merge_all block_id
    return $ if exists then Nothing else Just block_id

integrate_track :: Ui.M m => BlockId -> Track -> m ()
integrate_track block_id (Track note controls) = do
    exists <- Maybe.isJust <$> Ui.lookup_track (fst note)
    dests <- if exists
        then Ui.require "no manual integration" . Map.lookup source_key
            . Block.block_integrated_manual =<< Ui.get_block block_id
        else do
            forM_ (note : controls) $ \(track_id, track) -> do
                Ui.create_track (Id.unpack_id track_id) $
                    Track.track (Track.track_title track) mempty
                Ui.insert_track block_id 999 $
                    Block.track (Block.TId track_id Ui.no_ruler)
                        Config.track_width
            return [Block.empty_destination (fst note)
                [(Track.track_title track, tid) | (tid, track) <- controls]]
    Ui.set_track_title (fst note) (Track.track_title (snd note))
    -- TODO I actually want to do Merge.score_merge here so I can match the
    -- TrackIds.  Then I don't have to toss track_id below.
    -- return ()
    new_dests <- Merge.merge_tracks block_id
        [(convert note, map convert controls)] dests
    Ui.set_integrated_manual block_id source_key (Just new_dests)
    where
    convert (_track_id, track) = Convert.Track
        { track_title = Track.track_title track
        , track_events = Events.ascending (Track.track_events track)
        }

source_key :: Block.SourceKey
source_key = "tscore"

-- * ui_state

ui_state :: Text -> Either Error Ui.State
ui_state text = do
    blocks <- make_blocks text
    first pretty $ Ui.exec Ui.empty $ do
        Ui.set_namespace Parse.default_namespace
        mapM_ ui_block blocks

ui_block :: Ui.M m => Block -> m ()
ui_block (block_id, title, tracks) = do
    track_ids <- sequence
        [ Ui.create_track (Id.unpack_id track_id) track
        | Track note controls <- tracks
        , (track_id, track) <- note : controls
        ]
    let tracks =
            [ Block.track (Block.TId tid Ui.no_ruler) Config.track_width
            | tid <- track_ids
            ]
    -- The first block becomes the root_id implicitly.
    Ui.create_block (Id.unpack_id block_id) title $
        Block.track (Block.RId Ui.no_ruler) Config.ruler_width : tracks
    return ()
    -- TODO map config_meter to a ruler

-- * make_blocks

make_blocks :: Text -> Either Error [Block]
make_blocks text = do
    T.Score defs <- first txt $ Parse.parse_score text
    fst <$> foldM collect ([], Check.default_config) defs
    where
    collect (accum, config) def = do
        (block_id, config) <- interpret_toplevel config def
        return (maybe id (:) block_id accum, config)

interpret_toplevel :: Check.Config -> T.Toplevel
    -> Either Error (Maybe Block, Check.Config)
interpret_toplevel config (T.ToplevelDirective dir) = (Nothing,) <$>
    first ("toplevel: " <>) (Check.parse_directive dir config)
interpret_toplevel config (T.BlockDefinition block) = do
    block <- interpret_block config block
    return (Just block, config)

interpret_block :: Check.Config -> T.Block -> Either Error Block
interpret_block config
        (T.Block block_id directives title (T.Tracks tracks)) = do
    block_config <- first ((pretty block_id <> ": ")<>) $
        Check.parse_directives config directives
    tracks <- mapM (uncurry (track_events block_config block_id))
        (zip [1..] tracks)
    return (block_id, title, tracks)

track_events :: Check.Config -> BlockId -> TrackNum -> T.Track
    -> Either Error Track
track_events config block_id tracknum (T.Track title tokens) = do
    tokens <- first (Check.show_error (Check.config_meter config)) $
        sequence $ Check.process config tokens
    let pitches = map pitch_event $ Seq.map_maybe_snd T.note_pitch tokens
    let notes = Events.from_list (map (uncurry note_event) tokens)
    return $ Track
        { track_note =
            ( make_track_id block_id tracknum False
            , Track.track (if Text.null title then ">" else title) notes
            )
        , track_controls = if null pitches then [] else
            [ ( make_track_id block_id tracknum True
              , Track.track "*" (Events.from_list pitches)
              )
            ]
        }

make_track_id :: BlockId -> TrackNum -> Bool -> TrackId
make_track_id block_id tracknum is_pitch =
    Id.TrackId $ Id.id ns $
        ident <> ".t" <> showt tracknum <> if is_pitch then ".pitch" else ""
    where
    (ns, ident) = Id.un_id $ Id.unpack_id block_id

note_event :: T.Time -> T.Note (Maybe Text) T.Time -> Event.Event
note_event start (T.Note (T.Call call) _ dur) =
    add_stack $ Event.event (track_time start) (track_time dur) call

pitch_event :: (T.Time, Text) -> Event.Event
pitch_event (start, pitch) = add_stack $ Event.event (track_time start) 0 pitch

track_time :: T.Time -> TrackTime
track_time = realToFrac

-- TODO add a stack to the events so I know where they came from?
-- I do it in LSol, but I think it's just so I can infer the SourceKey
-- from the events later.
add_stack :: Event.Event -> Event.Event
add_stack event =
    Event.stack_ #= Just (Event.Stack stack (Event.start event)) $ event
    where stack = Stack.add (Stack.Call source_key) Stack.empty
