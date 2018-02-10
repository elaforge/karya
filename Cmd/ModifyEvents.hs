-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities to modify events in tracks.
module Cmd.ModifyEvents where
import qualified Data.Either as Either
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.String as String
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Sel as Sel
import qualified Ui.Ui as Ui

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection
import qualified Derive.Parse as Parse
import qualified Derive.ParseTitle as ParseTitle
import Global
import Types


-- | Map a function over events on a certain track.  Returning Nothing will
-- leave the track unchanged.
type Track m = BlockId -> TrackId -> [Event.Event] -> m (Maybe [Event.Event])

-- | Map a function over a set of events.
events :: Monad m => ([Event.Event] -> m [Event.Event]) -> Track m
events f _ _ = liftM Just . f

-- | Map a function over a single event.
event :: Monad m => (Event.Event -> Event.Event) -> Track m
event f = events (return . map f)

text :: Monad m => (Text -> Text) -> Track m
text = event . (\f -> Event.text_ %= f)

-- | Split up a pipeline and lex the calls.
pipeline :: ([[Text]] -> [[Text]]) -> Text -> Text
pipeline modify = Parse.join_pipeline . modify . Parse.split_pipeline

-- | Take a text transformation that can fail to a Track transformation that
-- transforms all the events and throws if any of the text transformations
-- failed.
failable_text :: Cmd.M m => (Text -> Either Text Text) -> Track m
failable_text f block_id track_id events = do
    let (failed, ok) = Either.partitionEithers $ map (failing_text f) events
        errs = [err <> ": " <> Cmd.log_event block_id track_id evt
            | (err, evt) <- failed]
    unless (null errs) $ Cmd.throw $
        "transformation failed: " <> Text.intercalate ", " errs
    return $ Just ok
    where
    failing_text f event = case f (Event.text event) of
        Left err -> Left (err, event)
        Right text -> Right $ Event.text_ #= text $ event


-- * modify selections

-- | Map a function over the selected events, as per 'Selection.events'.
selection :: Cmd.M m => Track m -> m ()
selection modify = modify_selected True modify =<< Selection.events

-- | Like 'selection', but don't apply to collapsed tracks.  This is
-- appropriate for operations that often apply to note tracks.  If you select
-- multiple note tracks, then the intervening collapsed pitch tracks will also
-- be selected and if you accidentally modify those you won't see the
-- modifications.
selection_visible :: Cmd.M m => Track m -> m ()
selection_visible modify = modify_selected False modify =<< Selection.events

-- | Like 'selection', but only operate on the 'Selection.point_track'.
selected_track :: Cmd.M m => Track m -> m ()
selected_track modify =
    modify_selected False modify . (:[]) =<< Selection.track_events
    -- Don't affect collapsed tracks because why would the point selection be
    -- on one of those?

modify_selected :: Cmd.M m => Bool -- ^ If False, omit collapsed tracks.
    -> Track m -> Selection.SelectedEvents -> m ()
modify_selected include_collapsed modify selected = do
    block_id <- Cmd.get_focused_block
    let wanted track_id
            | include_collapsed = return True
            | otherwise = do
                tracknum <- Ui.get_tracknum_of block_id track_id
                not <$> Ui.track_collapsed block_id tracknum
    forM_ selected $ \(track_id, events) ->
        whenM (wanted track_id) $ do
            maybe_new_events <- modify block_id track_id events
            whenJust maybe_new_events $ \new_events -> do
                Ui.remove_events track_id events
                Ui.insert_block_events block_id track_id new_events

-- | Advance the selection if it was a point.  This is convenient for applying
-- a transformation repeatedly.
advance_if_point :: Cmd.M m => m ()
advance_if_point = whenM (Sel.is_point <$> Selection.get) Selection.advance

-- | Map a function over the events that overlap the selection point.
overlapping :: Cmd.M m => Track m -> m ()
overlapping f = do
    (block_id, _, track_ids, _) <- Selection.tracks
    pos <- Selection.point
    forM_ track_ids $ \track_id -> do
        maybe_event <- Events.overlapping pos <$> Ui.get_events track_id
        whenJust maybe_event $ \old_event ->
            whenJustM (f block_id track_id [old_event]) $ \new_events -> do
                Ui.remove_event track_id old_event
                Ui.insert_block_events block_id track_id new_events

-- | Map over tracks whose name matches the predicate.
tracks_named :: Cmd.M m => (Text -> Bool) -> Track m -> Track m
tracks_named wanted f = \block_id track_id events ->
    ifM (wanted <$> Ui.get_track_title track_id)
        (f block_id track_id events) (return Nothing)

selected_note :: Cmd.M m => Track m -> m ()
selected_note = selection . tracks_named ParseTitle.is_note_track

selected_control :: Cmd.M m => Track m -> m ()
selected_control = selection . tracks_named ParseTitle.is_signal_track

selected_pitch :: Cmd.M m => Track m -> m ()
selected_pitch = selection . tracks_named ParseTitle.is_pitch_track


-- * block tracks

-- | Like 'selection', but maps over an entire block.
block :: Cmd.M m => BlockId -> Track m -> m ()
block block_id f = do
    track_ids <- Block.block_track_ids <$> Ui.get_block block_id
    forM_ track_ids $ \track_id -> do
        events <- Events.ascending <$> Ui.get_events track_id
        maybe (return ()) (Ui.modify_events track_id . const . Events.from_list)
            =<< f block_id track_id events

all_blocks :: Cmd.M m => Track m -> m ()
all_blocks f = mapM_ (flip block f) =<< Ui.all_block_ids

all_tracks_named :: Cmd.M m => (Text -> Bool) -> Track m -> m ()
all_tracks_named wanted = all_blocks . tracks_named wanted

note_tracks :: Cmd.M m => Track m -> m ()
note_tracks = all_tracks_named ParseTitle.is_note_track

control_tracks :: Cmd.M m => Track m -> m ()
control_tracks = all_tracks_named ParseTitle.is_control_track

pitch_tracks :: Cmd.M m => Track m -> m ()
pitch_tracks = all_tracks_named ParseTitle.is_pitch_track

-- * misc

-- | Move everything at or after @start@ by @shift@.
move_track_events :: Ui.M m => ScoreTime -> ScoreTime -> ScoreTime
    -> TrackId -> m ()
move_track_events block_end start shift track_id =
    Ui.modify_events track_id $ \events ->
        move_events block_end start shift events

-- | All events starting at and after a point to the end are shifted by the
-- given amount.
move_events :: ScoreTime -- ^ events past the block end are shortened or removed
    -> ScoreTime -> ScoreTime -> Events.Events -> Events.Events
move_events block_end point shift events = merged
    where
    shifted = Events.clip False block_end $
        map (Event.start_ %= (+shift)) (Events.at_after point events)
    -- +1 in case the last event is +0 duration.
    merged = Events.insert shifted $
        Events.remove (Events.Range point (Events.time_end events + 1)) events

-- * replace tokens

data Replacement =
    RLiteral !Text -- ^ literal text
    | F !Int -- ^ field from match
    deriving (Eq, Show)

instance String.IsString Replacement where
    fromString = RLiteral . txt

-- | Regex-like substitution on tracklang tokens.
--
-- Short names and IsString instances attempt to make it concise enough for
-- inline use.  If the pattern doesn't match, the input is returned unchanged.
substitute :: Parser -> [Replacement] -> Text -> Either Text Text
substitute parser replacements text = case match of
    Nothing -> Right text
    Just matches -> Text.unwords . filter (not . Text.null) <$>
        mapM (replace matches) replacements
    where
    match = IntMap.fromList . zip [0..] <$> parse_tokens parser text
    replace matches r = case r of
        RLiteral text -> Right text
        F n -> justErr ("no match for field " <> showt n) $
            IntMap.lookup n matches

-- ** parser

-- | Yet another \"list of successes\" style parser.
newtype Parser = Parser ([Token] -> [([Match], [Token])])
type Token = Text
type Match = [Token]

parse_tokens :: Parser -> Text -> Maybe [Text]
parse_tokens parser =
    fmap (map Text.unwords) . Seq.head . parse parser . Parse.lex

parse :: Parser -> [Token] -> [[Match]]
parse (Parser p) = map fst . filter (null . snd) . p

instance Semigroup Parser where
    Parser p1 <> Parser p2 = Parser $ \tokens -> do
        (matches1, rest1) <- p1 tokens
        (matches2, rest2) <- p2 rest1
        return (matches1 ++ matches2, rest2)
instance Monoid Parser where
    mempty = Parser $ \tokens -> [([], tokens)]
    mappend = (<>)

instance String.IsString Parser where
    fromString = literal . txt

-- | Match a literal token.
literal :: Text -> Parser
literal token = Parser $ \tokens -> case tokens of
    t : ts
        | t == token -> [([], ts)]
        | otherwise -> []
    [] -> []

-- | Match one token.
w :: Parser
w = Parser $ \tokens -> case tokens of
    t : ts -> [([[t]], ts)]
    [] -> []

-- | Match 0 or more tokens.
ws :: Parser
ws = Parser $ \tokens ->
    [([pre], post) | (pre, post) <- reverse $ splits tokens]

-- | Match 1 or more tokens.
ws1 :: Parser
ws1 = Parser $ \tokens ->
    [([pre], post) | (pre, post) <- reverse $ drop 1 $ splits tokens]

splits :: [a] -> [([a], [a])]
splits xs = zip (List.inits xs) (List.tails xs)
