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

import qualified Util.Lists as Lists
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection
import qualified Derive.Parse as Parse
import qualified Derive.ParseTitle as ParseTitle
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Sel as Sel
import qualified Ui.Ui as Ui

import           Global
import           Types


-- | Map a function over multiple tracks.  It has access to all selected
-- events.  Omit a TrackId from the output to leave it unchanged.
type Tracks m = BlockId -> [(TrackId, [Event.Event])]
    -> m [(TrackId, [Event.Event])]

-- | Map a function over events on a certain track.  Returning Nothing will
-- leave the track unchanged.
type Track m = BlockId -> TrackId -> [Event.Event] -> m (Maybe [Event.Event])

-- | Map a Track transformer over multiple tracks.
track :: Monad m => Track m -> Tracks m
track modify = \blockId trackEvents -> do
    outs <- mapM (uncurry (modify blockId)) trackEvents
    pure
        [ (trackId, events)
        | (trackId, Just events) <- zip (map fst trackEvents) outs
        ]

-- | Map a function over a set of events.
events :: Monad m => ([Event.Event] -> m [Event.Event]) -> Track m
events f _ _ = liftM Just . f

-- | Map a function over a single event.
event :: Monad m => (Event.Event -> Event.Event) -> Track m
event f = events (pure . map f)

text :: Monad m => (Text -> Text) -> Track m
text = event . (\f -> Event.text_ %= f)

-- | Split up a pipeline and lex the calls.
pipeline :: ([[Text]] -> [[Text]]) -> Text -> Text
pipeline modify = Parse.joinPipeline . modify . Parse.splitPipeline

-- | Take a text transformation that can fail to a Track transformation that
-- transforms all the events and throws if any of the text transformations
-- failed.
failableText :: Cmd.M m => (Text -> Either Text Text) -> Track m
failableText f blockId trackId events = do
    let (failed, ok) = Either.partitionEithers $ map (failingText f) events
        errs = [err <> ": " <> Cmd.logEvent blockId trackId evt
            | (err, evt) <- failed]
    unless (null errs) $ Cmd.throw $
        "transformation failed: " <> Text.intercalate ", " errs
    pure $ Just ok
    where
    failingText f event = case f (Event.text event) of
        Left err -> Left (err, event)
        Right text -> Right $ Event.text_ #= text $ event


-- * modify selections

selectionTracks :: Cmd.M m => Tracks m -> m ()
selectionTracks modify =
    modifySelected IncludeCollapsed modify =<< Selection.events

-- | Map a function over the selected events, as per 'Selection.events'.
selection :: Cmd.M m => Track m -> m ()
selection modify =
    modifySelected IncludeCollapsed (track modify) =<< Selection.events

-- | Like 'selection', but don't apply to collapsed tracks.  This is
-- appropriate for operations that often apply to note tracks.  If you select
-- multiple note tracks, then the intervening collapsed pitch tracks will also
-- be selected and if you accidentally modify those you won't see the
-- modifications.
selectionVisible :: Cmd.M m => Track m -> m ()
selectionVisible modify = ctxSelectionVisible modify =<< Selection.context

ctxSelectionVisible :: Ui.M m => Track m -> Selection.Context -> m ()
ctxSelectionVisible modify ctx =
    modifySelectedBlock ExcludeCollapsed (track modify)
        (Selection.ctxBlockId ctx) =<< Selection.ctxEvents ctx

-- | Like 'selection', but only operate on the 'Selection.point_track'.
selectedTrack :: Cmd.M m => Track m -> m ()
selectedTrack modify =
    modifySelected ExcludeCollapsed (track modify) . (:[])
        =<< Selection.trackEvents
    -- Don't affect collapsed tracks because why would the point selection be
    -- on one of those?

data Collapsed = ExcludeCollapsed | IncludeCollapsed
    deriving (Show, Eq)

modifySelected :: Cmd.M m => Collapsed
    -> Tracks m -> Selection.SelectedEvents -> m ()
modifySelected collapsed modify selected = do
    blockId <- Cmd.getFocusedBlock
    modifySelectedBlock collapsed modify blockId selected

-- It would be nice to make this a ctx_ function, but modify_selected is called
-- with different selections.
modifySelectedBlock :: Ui.M m => Collapsed
    -> Tracks m -> BlockId -> Selection.SelectedEvents -> m ()
modifySelectedBlock collapsed modify blockId selected = do
    selected <- filterM (wanted . fst) selected
    new <- modify blockId selected
    forM_ new $ \(trackId, newEvents) -> do
        let events = fromMaybe [] $ lookup trackId selected
        Ui.removeEvents trackId events
        Ui.insertBlockEvents blockId trackId newEvents
    where
    wanted trackId = case collapsed of
        IncludeCollapsed -> pure True
        ExcludeCollapsed -> do
            tracknum <- Ui.getTracknumOf blockId trackId
            not <$> Ui.trackCollapsed blockId tracknum

-- | Advance the selection if it was a point.  This is convenient for applying
-- a transformation repeatedly.
advanceIfPoint :: Cmd.M m => m ()
advanceIfPoint = whenM (Sel.isPoint <$> Selection.get) Selection.advance

-- | Map a function over the events that overlap the selection point.
overlapping :: Cmd.M m => Track m -> m ()
overlapping f = do
    (blockId, _, trackIds, _) <- Selection.tracks
    pos <- Selection.point
    forM_ trackIds $ \trackId -> do
        maybeEvent <- Events.overlapping pos <$> Ui.getEvents trackId
        whenJust maybeEvent $ \oldEvent ->
            whenJustM (f blockId trackId [oldEvent]) $ \newEvents -> do
                Ui.removeEvent trackId oldEvent
                Ui.insertBlockEvents blockId trackId newEvents

-- | Map over tracks whose name matches the predicate.
tracksNamed :: Cmd.M m => (Text -> Bool) -> Track m -> Track m
tracksNamed wanted f = \blockId trackId events ->
    ifM (wanted <$> Ui.getTrackTitle trackId)
        (f blockId trackId events) (pure Nothing)

selectedNote :: Cmd.M m => Track m -> m ()
selectedNote = selection . tracksNamed ParseTitle.isNoteTrack

selectedControl :: Cmd.M m => Track m -> m ()
selectedControl = selection . tracksNamed ParseTitle.isSignalTrack

selectedPitch :: Cmd.M m => Track m -> m ()
selectedPitch = selection . tracksNamed ParseTitle.isPitchTrack


-- * block tracks

-- | Like 'selection', but maps over an entire block.
block :: Cmd.M m => BlockId -> Track m -> m ()
block blockId f = do
    trackIds <- Block.blockTrackIds <$> Ui.getBlock blockId
    forM_ trackIds $ \trackId -> do
        events <- Events.ascending <$> Ui.getEvents trackId
        maybe (pure ()) (Ui.modifyEvents trackId . const . Events.fromList)
            =<< f blockId trackId events

allBlocks :: Cmd.M m => Track m -> m ()
allBlocks f = mapM_ (flip block f) =<< Ui.allBlockIds

allTracksNamed :: Cmd.M m => (Text -> Bool) -> Track m -> m ()
allTracksNamed wanted = allBlocks . tracksNamed wanted

noteTracks :: Cmd.M m => Track m -> m ()
noteTracks = allTracksNamed ParseTitle.isNoteTrack

controlTracks :: Cmd.M m => Track m -> m ()
controlTracks = allTracksNamed ParseTitle.isControlTrack

pitchTracks :: Cmd.M m => Track m -> m ()
pitchTracks = allTracksNamed ParseTitle.isPitchTrack

-- * misc

-- | Move everything at or after @start@ by @shift@.
moveTrackEvents :: Ui.M m => ScoreTime -> ScoreTime -> ScoreTime
    -> TrackId -> m ()
moveTrackEvents blockEnd start shift trackId =
    Ui.modifyEvents trackId $ \events ->
        moveEvents blockEnd start shift events

-- | All events starting at and after a point to the end are shifted by the
-- given amount.
moveEvents :: ScoreTime -- ^ events past the block end are shortened or removed
    -> ScoreTime -> ScoreTime -> Events.Events -> Events.Events
moveEvents blockEnd point shift events = merged
    where
    shifted = Events.clipList False blockEnd $
        map (Event.start_ %= (+shift)) (Events.atAfter point events)
    -- +1 in case the last event is +0 duration.
    merged = Events.insert shifted $
        Events.remove (Events.Range point (Events.timeEnd events + 1)) events

-- * replace tokens

data Replacement
    = RLiteral !Text -- ^ literal text
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
    match = IntMap.fromList . zip [0..] <$> parseTokens parser text
    replace matches r = case r of
        RLiteral text -> Right text
        F n -> justErr ("no match for field " <> showt n) $
            IntMap.lookup n matches

-- ** parser

-- | Yet another \"list of successes\" style parser.
newtype Parser = Parser ([Token] -> [([Match], [Token])])
type Token = Text
type Match = [Token]

parseTokens :: Parser -> Text -> Maybe [Text]
parseTokens parser =
    fmap (map Text.unwords) . Lists.head . parse parser . map Text.strip
    . Parse.lex

parse :: Parser -> [Token] -> [[Match]]
parse (Parser p) = map fst . filter (null . snd) . p

instance Semigroup Parser where
    Parser p1 <> Parser p2 = Parser $ \tokens -> do
        (matches1, rest1) <- p1 tokens
        (matches2, rest2) <- p2 rest1
        pure (matches1 ++ matches2, rest2)
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
