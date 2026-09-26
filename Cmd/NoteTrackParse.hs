-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Ad-hoc parsing for note tracks.  This is a bit hacky because it's trying
-- to guess syntactically how an expression will be evaluated, but it's
-- convenient to do it this way, because otherwise I need to either have a
-- special kind of evaluation, or I need to have evaluate emit metadata, and
-- then wait for a full derive to complete.  In addition, expressions that are
-- being edited might not parse, or their caller might fail, or something.
module Cmd.NoteTrackParse where
import qualified Data.Map as Map

import qualified Util.Lists as Lists
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Parse as Parse

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ui as Ui

import           Global
import           Types


trackBlockCalls :: Ui.M m => Bool -> BlockId -> TrackId
    -> m [(Event.Event, NonEmpty BlockId)]
trackBlockCalls lookInArgs blockId trackId = do
    events <- Events.ascending <$> Ui.getEvents trackId
    toBid <- getToBlockId (Just blockId)
    let bids = map (blockCallsOf lookInArgs toBid . Event.text) events
    pure [(event, b :| bs) | (event, b:bs) <- zip events bids]

exprBlockCalls :: Ui.M m => Bool -> BlockId -> Text -> m [BlockId]
exprBlockCalls lookInArgs caller expr = do
    toBid <- getToBlockId (Just caller)
    pure $ blockCallsOf lookInArgs toBid expr

-- | Try to to figure out any blocks that are referenced in the expression.
--
-- This doesn't use the full Derive.Parse machinery, but is simple and doesn't
-- require the text to be fully parseable.
blockCallsOf :: Bool -- ^ If True, and the call wasn't a block, see if any
    -- of the arguments name blocks.  This is for calls like @alt@, which take
    -- blocks as arguments.
    -> (Text -> Maybe a) -> Text -> [a]
blockCallsOf lookInArgs toBid expr = case syms of
    [] -> []
    b : bs -> case toBid b of
        Nothing -> mapMaybe toBid bs
        Just blockId -> [blockId]
    where
    syms = (if lookInArgs then id else take 1) $ possibleBlockCalls expr

getToBlockId :: Ui.M m => Maybe BlockId -> m (Text -> Maybe BlockId)
getToBlockId caller = do
    blocks <- Ui.gets Ui.stateBlocks
    ns <- Ui.getNamespace
    pure $ toBlockId blocks ns caller

-- | If the first word names a block, then it's probably a block call with
-- args, so return just that.  Otherwise, return any argument that names
-- a block.
toBlockId :: Map BlockId a -> Id.Namespace -> Maybe BlockId -> Text
    -> (Maybe BlockId)
toBlockId blocks ns caller =
    valid <=< Eval.callToBlockId ns caller . Expr.Symbol
    where
    valid blockId
        | Just _ <- Map.lookup blockId blocks = Just blockId
        | otherwise = Nothing

possibleBlockCalls :: Text -> [Text]
possibleBlockCalls = fromMaybe [] . Lists.last . Parse.splitPipeline
