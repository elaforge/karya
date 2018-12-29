-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Parse tscore, check and postprocess it, and convert to Ui.State.
module Derive.TScore.TScore where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil
import qualified App.Config as Config
import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.Integrate.Merge as Merge
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


-- * types

type Error = Text

type Block = (BlockId, Text, [Track])

data Track = Track {
    track_note :: (TrackId, Track.Track)
    , track_controls :: [(TrackId, Track.Track)]
    } deriving (Eq, Show)

track_ids :: Track -> [TrackId]
track_ids (Track note controls) = fst note : map fst controls

-- * integrate

integrate :: Ui.M m => Text -> m ()
integrate text = do
    blocks <- Ui.require_right id $ make_blocks text
    -- TODO trim blocks that disappeared like with tracks?
    mapM_ integrate_block blocks

integrate_block :: Ui.M m => Block -> m ()
integrate_block (block_id, title, tracks) = do
    exists <- Maybe.isJust <$> Ui.lookup_block block_id
    if exists
        then Ui.set_block_title block_id title
        else void $ Ui.create_block (Id.unpack_id block_id) title []
    existing_tracks <- Set.fromList <$> Ui.track_ids_of block_id
    -- This will wipe out diffs... do I really want that?
    let new = Set.fromList $ concatMap track_ids tracks
    let gone = existing_tracks `Set.difference` new
    mapM_ Ui.destroy_track (Set.toList gone)
    mapM_ (integrate_track block_id) tracks

integrate_track :: Ui.M m => BlockId -> Track -> m ()
integrate_track block_id track@(Track note controls) = do
    exists <- Maybe.isJust <$> Ui.lookup_track (fst note)
    dests <- if exists
        then Ui.require "no manual integration" . Map.lookup key
            . Block.block_integrated_manual =<< Ui.get_block block_id
        else do
            sequence_
                [ Ui.create_track (Id.unpack_id track_id) $
                    Track.track (Track.track_title track) mempty
                | (track_id, track) <- note : controls
                ]
            return $ map Block.empty_destination $ track_ids track
    -- TODO I actually want to do Merge.score_merge here so I can match the
    -- TrackIds.  Then I don't have to toss track_id below.
    new_dests <- forM dests $ \dest ->
        Merge.merge_tracks block_id [(convert note, map convert controls)]
            [dest]
    Ui.set_integrated_manual block_id key (Just (concat new_dests))
    where
    key = "tscore"
    convert (_track_id, track) = Convert.Track
        { track_title = Track.track_title track
        , track_events = Events.ascending (Track.track_events track)
        }

-- TODO add a stack to the events so I know where they came from?
-- I do it in LSol, but I think it's just so I can infer the SourceKey
-- from the events later.
-- add_stack :: Block.SourceKey -> Event.Event -> Event.Event
-- add_stack key event =
--     Event.stack_ #= Just (Event.Stack stack (Event.start event)) $ event
--     where stack = Stack.add (Stack.Call key) Stack.empty

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
    let ntitle = TextUtil.joinWith " | " ParseTitle.note_track title
    return $ Track
        { track_note =
            (make_track_id block_id tracknum False, Track.track ntitle notes)
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
    Event.event (track_time start) (track_time dur) call

pitch_event :: (T.Time, Text) -> Event.Event
pitch_event (start, pitch) = Event.event (track_time start) 0 pitch

track_time :: T.Time -> TrackTime
track_time = realToFrac
