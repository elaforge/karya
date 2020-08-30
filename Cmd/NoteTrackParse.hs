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

import qualified Util.Seq as Seq
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Parse as Parse

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ui as Ui

import           Global
import           Types


track_block_calls :: Ui.M m => Bool -> BlockId -> TrackId
    -> m [(Event.Event, NonEmpty BlockId)]
track_block_calls look_in_args block_id track_id = do
    events <- Events.ascending <$> Ui.get_events track_id
    to_bid <- get_to_block_id (Just block_id)
    let bids = map (block_calls_of look_in_args to_bid . Event.text) events
    return [(event, b :| bs) | (event, b:bs) <- zip events bids]

expr_block_calls :: Ui.M m => Bool -> BlockId -> Text -> m [BlockId]
expr_block_calls look_in_args caller expr = do
    to_bid <- get_to_block_id (Just caller)
    return $ block_calls_of look_in_args to_bid expr

-- | Try to to figure out any blocks that are referenced in the expression.
--
-- This doesn't use the full Derive.Parse machinery, but is simple and doesn't
-- require the text to be fully parseable.
block_calls_of :: Bool -- ^ If True, and the call wasn't a block, see if any
    -- of the arguments name blocks.  This is for calls like @alt@, which take
    -- blocks as arguments.
    -> (Text -> Maybe a) -> Text -> [a]
block_calls_of look_in_args to_bid expr = case syms of
    [] -> []
    b : bs -> case to_bid b of
        Nothing -> mapMaybe to_bid bs
        Just block_id -> [block_id]
    where
    syms = (if look_in_args then id else take 1) $ possible_block_calls expr

get_to_block_id :: Ui.M m => Maybe BlockId -> m (Text -> Maybe BlockId)
get_to_block_id caller = do
    blocks <- Ui.gets Ui.state_blocks
    ns <- Ui.get_namespace
    return $ to_block_id blocks ns caller

-- | If the first word names a block, then it's probably a block call with
-- args, so return just that.  Otherwise, return any argument that names
-- a block.
to_block_id :: Map BlockId a -> Id.Namespace -> Maybe BlockId -> Text
    -> (Maybe BlockId)
to_block_id blocks ns caller =
    valid <=< Eval.call_to_block_id ns caller . Expr.Symbol
    where
    valid block_id
        | Just _ <- Map.lookup block_id blocks = Just block_id
        | otherwise = Nothing

possible_block_calls :: Text -> [Text]
possible_block_calls = fromMaybe [] . Seq.last . Parse.split_pipeline
