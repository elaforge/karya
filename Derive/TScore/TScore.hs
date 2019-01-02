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
import qualified Util.Then as Then
import qualified App.Config as Config
import qualified Cmd.BlockConfig as BlockConfig
import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.Integrate.Merge as Merge
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.Modify as Ruler.Modify

import qualified Derive.Stack as Stack
import qualified Derive.TScore.Check as Check
import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.T as T

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui

import           Global
import           Types


-- * types

type Error = Text

data Block = Block {
    _block_id :: !BlockId
    , _block_title :: !Text
    , _meter :: ![Meter.LabeledMark]
    , _tracks :: ![Track]
    } deriving (Eq, Show)

data Track = Track {
    _note :: !Track1
    , _controls :: [Track1]
    } deriving (Eq, Show)

data Track1 = Track1 {
    _track_id :: !TrackId
    , _title :: !Text
    , _events :: !Events.Events
    } deriving (Eq, Show)

track_ids :: Track -> [TrackId]
track_ids (Track note controls) = _track_id note : map _track_id controls

-- * integrate

integrate :: Ui.M m => Text -> m [BlockId] -- ^ newly created blocks
integrate text = do
    blocks <- Ui.require_right ("make_blocks: "<>) $ make_blocks text
    -- TODO trim blocks that disappeared like with tracks?
    mapMaybeM integrate_block blocks

integrate_block :: Ui.M m => Block -> m (Maybe BlockId)
integrate_block block = do
    let block_id = _block_id block
    ruler_id <- ui_ruler block
    exists <- Maybe.isJust <$> Ui.lookup_block block_id
    if exists
        then Ui.set_block_title block_id (_block_title block)
        else void $ Ui.create_block (Id.unpack_id block_id) (_block_title block)
            [Block.track (Block.RId ruler_id) Config.ruler_width]
    existing_tracks <- Set.fromList <$> Ui.track_ids_of block_id
    -- This will wipe out diffs... do I really want that?
    let new = Set.fromList $ concatMap track_ids (_tracks block)
    let gone = existing_tracks `Set.difference` new
    mapM_ Ui.destroy_track (Set.toList gone)
    mapM_ (integrate_track block_id ruler_id) (_tracks block)
    unless exists $
        BlockConfig.toggle_merge_all block_id
    return $ if exists then Nothing else Just block_id

integrate_track :: Ui.M m => BlockId -> RulerId -> Track -> m ()
integrate_track block_id ruler_id (Track note controls) = do
    exists <- Maybe.isJust <$> Ui.lookup_track (_track_id note)
    dests <- if exists
        then Ui.require "no manual integration" . Map.lookup source_key
            . Block.block_integrated_manual =<< Ui.get_block block_id
        else do
            forM_ (note : controls) $ \track -> do
                Ui.create_track (Id.unpack_id (_track_id track)) $
                    Track.track (_title track) mempty
                Ui.insert_track block_id 999 $
                    Block.track (Block.TId (_track_id track) ruler_id)
                        Config.track_width
            return [Block.empty_destination (_track_id note)
                [(_title track, _track_id track) | track <- controls]]
    Ui.set_track_title (_track_id note) (_title note)
    -- TODO I actually want to do Merge.score_merge here so I can match the
    -- TrackIds.  Then I don't have to toss track_id below.
    new_dests <- Merge.merge_tracks block_id
        [(convert note, map convert controls)] dests
    Ui.set_integrated_manual block_id source_key (Just new_dests)
    where
    convert track = Convert.Track
        { track_title = _title track
        , track_events = Events.ascending (_events track)
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
ui_block block = do
    track_ids <- sequence
        [ Ui.create_track (Id.unpack_id track_id) (Track.track title events)
        | Track note controls <- _tracks block
        , Track1 track_id title events <- note : controls
        ]
    ruler_id <- ui_ruler block
    let tracks =
            [ Block.track (Block.TId tid ruler_id) Config.track_width
            | tid <- track_ids
            ]
    Ui.create_block (Id.unpack_id (_block_id block)) (_block_title block) $
        Block.track (Block.RId ruler_id) Config.ruler_width : tracks
    -- The first block will become the root_id implicitly.
    return ()

ui_ruler :: Ui.M m => Block -> m RulerId
ui_ruler block = make_ruler (_block_id block) (_meter block) end
    where
    end = maximum $ 0 :
        [ Events.time_end (_events t1)
        | Track note controls <- _tracks block, t1 <- note : controls
        ]

make_ruler :: Ui.M m => BlockId -> [Meter.LabeledMark] -> TrackTime -> m RulerId
make_ruler block_id meter end = do
    whenM (Maybe.isNothing <$> Ui.lookup_ruler ruler_id) $
        void $ Ui.create_ruler (Id.unpack_id ruler_id) (Ruler.ruler [])
    Ui.modify_ruler ruler_id (generate_ruler meter end)
    return ruler_id
    where
    ruler_id = Id.RulerId $ Id.unpack_id block_id

generate_ruler :: [Meter.LabeledMark] -> TrackTime
    -> (Ruler.Ruler -> Either Text Ruler.Ruler)
generate_ruler meter end = Ruler.Modify.meter (const generate)
    where
    generate = trim $ cycle $ Seq.rdrop 1 meter
    trim = map snd . Then.takeWhile1 ((<end) . fst)
        . scanl_on (+) Meter.m_duration 0

-- * make_blocks

make_blocks :: Text -> Either Error [Block]
make_blocks text = do
    T.Score defs <- first (("parse: "<>) . txt) $ Parse.parse_score text
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
    return $ Block
        { _block_id = block_id
        , _block_title = title
        , _meter = Check.meter_labeled $ Check.config_meter config
        , _tracks = tracks
        }

track_events :: Check.Config -> BlockId -> TrackNum -> T.Track
    -> Either Error Track
track_events config block_id tracknum (T.Track title tokens) = do
    tokens <- first (Check.show_error (Check.config_meter config)) $
        sequence $ Check.process config tokens
    let pitches = map pitch_event $ Seq.map_maybe_snd T.note_pitch tokens
    return $ Track
        { _note = Track1
            { _track_id = make_track_id block_id tracknum False
            , _title = if Text.null title then ">" else title
            , _events = Events.from_list $ map (uncurry note_event) tokens
            }
        , _controls = if null pitches then [] else (:[]) $ Track1
            { _track_id = make_track_id block_id tracknum True
            , _title = "*"
            , _events = Events.from_list pitches
            }
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

-- * util

scanl_on :: (accum -> key -> accum) -> (a -> key) -> accum -> [a]
    -> [(accum, a)]
scanl_on f key z xs = zip (scanl (\t -> f t . key) z xs) xs
