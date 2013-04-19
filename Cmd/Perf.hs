{- | This module is for Cmds that want to peek into the derived score for
    whatever reason.  Normally the flow of information goes from Cmd -> Derive
    -> Perform, but this module is for those cases when Cmd wants to know
    about the results of later stages.
-}
module Cmd.Perf where
import qualified Data.List as List
import qualified Data.Map as Map

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree

import qualified Cmd.Cmd as Cmd
import qualified Cmd.PlayUtil as PlayUtil
import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import qualified Perform.Transport as Transport

import Types


-- * notes

note_to_nn :: (Cmd.M m) => Pitch.ScaleId -> Pitch.Note
    -> m (Either String Pitch.NoteNumber)
note_to_nn scale_id note = either Left eval <$> note_to_pitch scale_id note
    where
    eval p = either (Left . Pretty.pretty) Right $
        PitchSignal.eval_pitch p mempty

-- | Convert a note to a note number by running a mini derivation just for it.
--
-- This doesn't apply any of the fancy context-sensitive tuning stuff, so any
-- scales that change tuning will emit their default pitch.  It also doesn't
-- apply any transpositions in scope, so in reflects the pitch of the note as
-- written in the score.
note_to_pitch :: (Cmd.M m) => Pitch.ScaleId -> Pitch.Note
    -> m (Either String PitchSignal.Pitch)
note_to_pitch scale_id note = do
    scale <- Cmd.get_scale "Perf.note_to_pitch" scale_id
    case Scale.scale_note_to_call scale note of
        Nothing -> return $ Left $ "no call for " <> show note
        Just call -> derive $ Call.apply_pitch 0 call >>= \val -> case val of
            TrackLang.VPitch pitch -> return pitch
            _ -> Derive.throw $ "note call returned non-pitch: " <> show val

-- | Like 'note_to_pitch', but supply enough context to get the actual
-- performed pitch.
--
-- I'm not sure if this is actually useful, since if you want the real derived
-- pitch you are probably better off asking 'get_nn_at'.
note_to_transposed_pitch :: (Cmd.M m) => Pitch.ScaleId -> BlockId -> TrackId
    -> ScoreTime -- ^ some scales retune over time
    -> Pitch.Note -> m (Either String PitchSignal.Pitch)
note_to_transposed_pitch scale_id block_id track_id pos note = do
    scale <- Cmd.get_scale "Perf.note_to_pitch" scale_id
    case Scale.scale_note_to_call scale note of
        Nothing -> return $ Left $ "no call for " <> show note
        Just call -> derive_at block_id track_id $
            Call.apply_pitch pos call >>= \val -> case val of
                TrackLang.VPitch pitch -> do
                    controls <- Derive.controls_at =<< Derive.real pos
                    return $ PitchSignal.apply controls pitch
                _ -> Derive.throw $
                    "note call returned non-pitch: " <> show val

-- | A cheap quick derivation that sets up the correct initial state, but
-- runs without the cache and throws away any logs.
derive :: (Cmd.M m) => Derive.Deriver a -> m (Either String a)
derive deriver = do
    (val, _, _) <- PlayUtil.run mempty mempty deriver
    return $ case val of
        Left err -> Left $ Pretty.pretty err
        Right val -> Right val

derive_at :: (Cmd.M m) => BlockId -> TrackId
    -> Derive.Deriver a -> m (Either String a)
derive_at block_id track_id deriver = do
    dynamic <- fromMaybe empty_dynamic <$>
        lookup_dynamic block_id (Just track_id)
    (val, _, _) <- PlayUtil.run_with_dynamic dynamic deriver
    return $ either (Left . Pretty.pretty) Right val
    where
    empty_dynamic = Derive.initial_dynamic Derive.empty_scope mempty


-- * signal

-- TODO this looks in TrackSignal, which are the signals sent to the UI,
-- so they're in score time.  If I want the ones in RealTime, or if I want
-- a PitchSignal, I have to look in TrackDynamic.

-- | Look up the signal of a track from the last derivation.  It's looked up
-- in the root block's performance.
lookup_signal :: (Cmd.M m) => TrackId -> m (Maybe Track.TrackSignal)
lookup_signal track_id = do
    maybe_perf <- lookup_root
    return $ do
        perf <- maybe_perf
        result <- Map.lookup track_id (Cmd.perf_track_signals perf)
        either (const Nothing) Just result

control_at :: Track.TrackSignal -> [ScoreTime] -> [Signal.Y]
control_at (Track.TrackSignal sig shift stretch _) ps =
    map (flip Signal.at sig . warp) ps
    where warp p = RealTime.score (p * stretch + shift)

nn_at :: Track.TrackSignal -> [ScoreTime] -> [Pitch.NoteNumber]
nn_at tsig ps = map Pitch.NoteNumber (control_at tsig ps)

-- | Get the control values at the given points, or fail if there is no
-- signal there.
get_control_at :: (Cmd.M m) => TrackId -> [ScoreTime] -> m [Signal.Y]
get_control_at track_id ps = do
    sig <- Cmd.require_msg ("no signal for " ++ show track_id)
        =<< lookup_signal track_id
    return $ control_at sig ps

-- | Get the pitch values at the given points, or fail if there is no
-- signal there.  TODO can't tell the difference between a pitch and control
-- track.
get_nn_at :: (Cmd.M m) => TrackId -> [ScoreTime] -> m [Pitch.NoteNumber]
get_nn_at track_id ps = do
    sig <- Cmd.require_msg ("no signal for " ++ show track_id)
        =<< lookup_signal track_id
    return $ nn_at sig ps


-- * environ

-- | Get the scale in scope in a certain track on a certain block.
--
-- Unlike the other Environ functions like 'get_key', this looks at the track
-- titles before falling back on the Environ.  That's because the scale is
-- needed to make sure note entry uses the right notes, and it's especially
-- annoying if the first note entry uses the wrong scale.  The Environ for
-- a newly added note track will always be the default scale, even if
-- a different scale is below it, because there aren't any events to trigger
-- inversion.
get_scale_id :: (Cmd.M m) => BlockId -> Maybe TrackId -> m Pitch.ScaleId
get_scale_id block_id maybe_track_id = first_just
    [ maybe (return Nothing) (scale_from_titles block_id) maybe_track_id
    , lookup_val block_id maybe_track_id TrackLang.v_scale
    ]
    (State.get_default State.default_scale)

-- | Try a bunch of actions, and return the first one that is Just, or
-- fall back on a default.
-- TODO could go in Util.Control if it's generally useful.
first_just :: (Monad m) => [m (Maybe a)] -> m a -> m a
first_just [] deflt = deflt
first_just (m:ms) deflt = do
    v <- m
    maybe (first_just ms deflt) return v

scale_from_titles :: (State.M m) => BlockId -> TrackId
    -> m (Maybe Pitch.ScaleId)
scale_from_titles block_id track_id = do
    tracks <- TrackTree.parents_children_of block_id track_id
    return $ case tracks of
        Nothing -> Nothing
        Just (parents, children) -> msum $ map scale_of (children ++ parents)
    where
    scale_of track = case TrackInfo.title_to_scale (State.track_title track) of
        Just scale_id | scale_id /= Pitch.empty_scale -> Just scale_id
        _ -> Nothing

-- | As with 'get_scale_id' but for the Key.
get_key :: (Cmd.M m) => BlockId -> Maybe TrackId -> m (Maybe Pitch.Key)
get_key block_id maybe_track_id =
    maybe (State.get_default State.default_key) (return . Just . Pitch.Key)
        =<< lookup_val block_id maybe_track_id TrackLang.v_key

lookup_instrument :: (Cmd.M m) => BlockId -> Maybe TrackId
    -> m (Maybe Score.Instrument)
lookup_instrument block_id maybe_track_id =
    maybe (State.get_default State.default_instrument) return
        =<< lookup_val block_id maybe_track_id TrackLang.v_instrument

-- | Lookup value from the deriver's Environ at the given block and (possibly)
-- track.  See 'Derive.TrackDynamic' for details on the limitations here.
--
-- The value is taken first from the root performance, and then the given
-- block's performance if not present in the root performance.  This is so
-- that blocks which are not called from the root at all will still have
-- environ values.
lookup_val :: (Cmd.M m, TrackLang.Typecheck a) => BlockId
    -> Maybe TrackId
    -- ^ If Nothing, take the env from the first track.  This is for when you
    -- expect the env val to be constant for the entire block.
    -> TrackLang.ValName -> m (Maybe a)
lookup_val block_id maybe_track_id name =
    justm (lookup_environ block_id maybe_track_id) $ \env ->
        either (Cmd.throw . ("Perf.lookup_val: "++)) return
            (TrackLang.checked_val name env)

lookup_environ :: (Cmd.M m) => BlockId -> Maybe TrackId
    -> m (Maybe TrackLang.Environ)
lookup_environ block_id maybe_track_id =
    fmap Derive.state_environ <$> lookup_dynamic block_id maybe_track_id

get_environ :: (Cmd.M m) => BlockId -> Maybe TrackId -> m TrackLang.Environ
get_environ block_id = fmap (fromMaybe mempty) . lookup_environ block_id

lookup_dynamic :: (Cmd.M m) => BlockId -> Maybe TrackId
    -> m (Maybe Derive.Dynamic)
lookup_dynamic block_id maybe_track_id = do
    maybe_dyn <- maybe Nothing (lookup . Cmd.perf_track_dynamic) <$>
        lookup_root
    case maybe_dyn of
        Just dyn -> return $ Just dyn
        Nothing ->
            maybe Nothing (lookup . Cmd.perf_track_dynamic) <$>
                Cmd.lookup_performance block_id
    where
    lookup track_dyns = case maybe_track_id of
        Nothing -> do
            (_, dyn) <- List.find ((==block_id) . fst . fst)
                (Map.toAscList track_dyns)
            return dyn
        Just track_id -> Map.lookup (block_id, track_id) track_dyns


-- * play

-- | Given a block, track, and time, find the realtime at that position.  If
-- the track is Nothing, use the first track that has tempo information.  This
-- is necessary because if a track is muted it will have no tempo, but it's
-- confusing if playing from a muted track fails.
get_realtime :: (Cmd.M m) => Cmd.Performance -> BlockId -> Maybe TrackId
    -> ScoreTime -> m RealTime
get_realtime perf block_id maybe_track_id pos =
    maybe (Cmd.throw $ show block_id ++ " " ++ Pretty.pretty maybe_track_id
            ++ " has no tempo information, so it probably failed to derive.")
        return =<< lookup_realtime perf block_id maybe_track_id pos

lookup_realtime :: (Cmd.M m) => Cmd.Performance -> BlockId -> Maybe TrackId
    -> ScoreTime -> m (Maybe RealTime)
lookup_realtime perf block_id maybe_track_id pos = do
    track_ids <- maybe (State.track_ids_of block_id) (return . (:[]))
        maybe_track_id
    return $ msum (map tempo track_ids)
    where tempo tid = Seq.head $ Cmd.perf_tempo perf block_id tid pos

-- | Like 'get_realtime', but do multiple at once.
get_realtimes :: Cmd.Performance -> BlockId -> TrackId -> [ScoreTime]
    -> [(ScoreTime, RealTime)]
get_realtimes perf block_id track_id ps =
    [(p, t) | (p, t : _)
        <- zip ps (map (Cmd.perf_tempo perf block_id track_id) ps)]

-- | Take a RealTime to all the ScoreTimes it corresponds to, if any.
find_play_pos :: (State.M m) => Transport.InverseTempoFunction
    -> RealTime -> m [(ViewId, [(TrackNum, ScoreTime)])]
find_play_pos inv_tempo = block_pos_to_play_pos . inv_tempo

-- | Do all the annoying shuffling around to convert the deriver-oriented
-- blocks and tracks to the view-oriented views and tracknums.
--
-- This function has to be careful to not throw on non-existent IDs, because
-- it's called from the monitor_loop.
block_pos_to_play_pos :: (State.M m) => [(BlockId, [(TrackId, ScoreTime)])]
    -> m [(ViewId, [(TrackNum, ScoreTime)])]
block_pos_to_play_pos = concatMapM convert

convert :: (State.M m) => (BlockId, [(TrackId, ScoreTime)])
    -> m [(ViewId, [(TrackNum, ScoreTime)])]
convert (block_id, track_pos) = do
    view_ids <- Map.keys <$> State.views_of block_id
    State.lookup_block block_id >>= \x -> return $ case x of
        Nothing -> []
        Just block ->
            let tracknum_pos = concatMap (tracknums_of block) track_pos
            in [(view_id, tracknum_pos) | view_id <- view_ids]

tracknums_of :: Block.Block -> (TrackId, ScoreTime) -> [(TrackNum, ScoreTime)]
tracknums_of block (track_id, pos) =
    [ (tracknum, pos)
    | (tracknum, Block.TId tid _)
        <- zip [0..] (Block.block_tracklike_ids block)
    , tid == track_id
    ]

-- ** find score times

-- | Give a block and score time, return the play position on sub-blocks.
-- The block itself is filtered out of the result.
sub_pos :: (Cmd.M m) => BlockId -> TrackId -> ScoreTime
    -> m [(BlockId, [(TrackId, ScoreTime)])]
sub_pos block_id track_id pos = fmap (fromMaybe []) $
    justm (Cmd.lookup_performance block_id) $ \perf ->
    justm (lookup_realtime perf block_id (Just track_id) pos) $ \real ->
    return $ Just $ filter ((/=block_id) . fst) (Cmd.perf_inv_tempo perf real)
    -- lookup_realtime gives Nothing if there's no tempo available which is
    -- likely for a newly added track.  Return [] in that case.

-- * util

lookup_root :: (Cmd.M m) => m (Maybe Cmd.Performance)
lookup_root = justm State.lookup_root_id Cmd.lookup_performance

get_root :: (Cmd.M m) => m Cmd.Performance
get_root = Cmd.require_msg "no root performance" =<< lookup_root
