module Derive.DeriverDb where
import Control.Monad
import qualified Control.Arrow as Arrow
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Ui.Block as Block
import qualified Ui.Track as Track
import qualified Ui.State as State

import qualified Derive.Score as Score
import qualified Derive.Derive3 as Derive
import qualified Derive.Twelve as Twelve

-- for the bogus default instrument stuff

import qualified Midi.Midi as Midi
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.InstrumentDb as InstrumentDb

-- TODO later this will also have to search plug-ins
get_deriver :: (State.UiStateMonad ui, Monad d) =>
    Block.Block -> ui (Derive.DeriveT d [Score.Event])
get_deriver block = do
    case Block.block_deriver block of
        Nothing -> default_deriver block
        Just deriver_id -> State.lookup_id deriver_id hardcoded_derivers

-- | Make up some kind of sensible default deriver for the given tracks.
default_deriver :: (State.UiStateMonad ui, Monad d) => Block.Block
    -> ui (Derive.DeriveT d [Score.Event])
default_deriver block = do
    let tids = tracks_of block
    names <- mapM (\tid -> fmap Track.track_title (State.get_track tid)) tids
    return $ make_deriver (zip names tids)

-- | Fake up some default instruments based on the track titles.
-- This will go away as soon as I have a real instrument situation.
default_inst_config :: (State.UiStateMonad m) =>
    Block.Block -> m Instrument.Config
default_inst_config block = do
    tracks <- mapM State.get_track (tracks_of block)
    let insts = map (track_title_inst . Track.track_title) tracks
        midi_insts = Maybe.catMaybes $ map InstrumentDb.lookup insts
    return $ Instrument.config [((Midi.WriteDevice "hi", chan), inst)
        | (inst, chan) <- zip midi_insts [0..]]

tracks_of block = [tid | (Block.TId tid _, _) <- Block.block_tracks block]

make_deriver name_tracks = mapM track_deriver name_tracks >>= Derive.d_merge
track_deriver (title, track_id) = Derive.d_track track_id
    >>= Derive.d_instrument (track_title_inst title)
    >>= Twelve.twelve
track_title_inst title = Score.instrument title Score.Midi


-- * hardcoded_derivers

hardcoded_derivers = Map.fromList $ map (Arrow.first Block.DeriverId)
    [
    ]
