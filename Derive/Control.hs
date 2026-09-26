-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | Derivers for control tracks.  That means tempo, control, and pitch.

    Control and pitch tracks can have a 'Derive.Merger'.  If no merger is
    given, they are combined with 'Derive.get_default_merger'.  @set@ will
    replace the signal.  So two tracks named @c@ will multiply, same as if the
    second were @mul c@.  If you want to override @c@ then @set c@ will do
    that.

    Tempo tracks don't support mergers because they are converted into
    a warp, which is then combined via composition.  Pitch tracks normally
    replace each other because adding together absolute pitches is undefined.
    Relative pitches can be added or multiplied, and this is expressed via
    normal controls using transposition signals like 'Controls.chromatic'.
-}
module Derive.Control (
    Config(..)
    , dControlTrack
    , trackInfo
    -- * TrackSignal
    , stashSignal, renderOf
#ifdef TESTING
    , deriveControl
#endif
) where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Derive.Cache as Cache
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Eval as Eval
import qualified Derive.EvalTrack as EvalTrack
import qualified Derive.Expr as Expr
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stream as Stream
import qualified Derive.Tempo as Tempo

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Ui.Block as Block
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Types as Types

import           Global
import           Types


data Config = Config
    -- | True if this is the single topmost track and is a tempo track.
    -- Ultimately it'is used by "Derive.Tempo".
    { configToplevelTempo :: !Bool
    , configUseCache :: !Bool
    } deriving (Show)

-- | Top level deriver for control tracks.
{-# SCC dControlTrack #-}
dControlTrack :: Config -> TrackTree.Track -> Derive.NoteDeriver
    -> Derive.NoteDeriver
dControlTrack config track deriver
    | Text.all Char.isSpace title = deriver
    | otherwise = do
        (ctype, expr) <- either (\err -> Derive.throw $ "track title: " <> err)
            pure (ParseTitle.parseControlTitle title)
        evalTrack config track expr ctype deriver
    where
    title = TrackTree.trackTitle track

-- * eval_track

evalTrack :: Config -> TrackTree.Track -> [DeriveT.Call]
    -> ParseTitle.ControlType -> Derive.NoteDeriver -> Derive.NoteDeriver
evalTrack config track expr ctype deriver = case ctype of
    ParseTitle.Tempo maybeSym -> do
        isLy <- Derive.isLilypondMode
        let sigDeriver
                | isLy = pure (Signal.constant 1, [])
                | otherwise = withControlEnv
                    Controls.tempo (Derive.Merger "compose" (const id) 0)
                    (inNormalMode $ deriveControl True track transform)
        tempoCall config maybeSym track sigDeriver deriver
    ParseTitle.Control (Right typedControl) maybeMerge -> do
        let control = ScoreT.valOf typedControl
        merger <- getMerger control maybeMerge
        (signal, logs) <- withControlEnv control merger
            (deriveControl False track transform)
        controlCall track typedControl merger (signal, logs) deriver
    ParseTitle.Control (Left tcall) maybeMerge -> do
        (typedControl, sig) <- trackCall tcall track
        merger <- getMerger (ScoreT.valOf typedControl) maybeMerge
        controlCall track typedControl merger (sig, []) deriver
    ParseTitle.Pitch scaleId pcontrolTcall -> case pcontrolTcall of
        Right pcontrol -> pitchCall config track pcontrol scaleId
            transform deriver
        -- TODO have to refactor pitch_call
        Left _tcall -> Derive.throw "unimplemented"
    where
    transform :: Derive.CallableExpr d => Derive.Deriver (Stream.Stream d)
        -> Derive.Deriver (Stream.Stream d)
    transform = Eval.evalTransformers ctx expr
    ctx = Derive.dummyContext 0 (TrackTree.trackEnd track) $ case ctype of
        ParseTitle.Tempo {} -> "tempo track"
        ParseTitle.Control {} -> "control track"
        ParseTitle.Pitch {} -> "pitch track"

trackCall :: Derive.CallableExpr d => Expr.Symbol -> TrackTree.Track
    -> Derive.Deriver (ScoreT.Typed ScoreT.Control, d)
trackCall sym track = do
    call <- Eval.getTrackCall sym
    Derive.callFunc call track

-- | Switch 'Derive.RealDurationQuery' to 'Derive.Normal'.  A RealDurationQuery
-- needs to evaluate until the tempo track of the callee block.  But if I leave
-- it in RealDurationQuery mode, the calls on the tempo signal think I am
-- asking about their durations.
inNormalMode :: Derive.Deriver a -> Derive.Deriver a
inNormalMode deriver = Derive.getMode >>= \mode -> case mode of
    Derive.RealDurationQuery -> Internal.local
        (\st -> st { Derive.stateMode = Derive.Normal }) deriver
    _ -> deriver

-- | Get the combining operator for this track.
--
-- 'Controls.null' is used by control calls, and uses 'Derive.Set' by default.
-- Since the control call emits signal which then goes in a control track,
-- a merge operator would wind up being applied twice.
getMerger :: ScoreT.Control -> Maybe Expr.Symbol
    -> Derive.Deriver Derive.Merger
getMerger control merge = case merge of
    Nothing
        | control == Controls.null -> pure Derive.Set
        | otherwise -> Derive.getDefaultMerger control
    Just sym -> Derive.getControlMerge sym

-- | A tempo track is derived like other signals, but in absolute time.
-- Otherwise it would wind up being composed with the environmental warp twice.
{-# SCC tempoCall #-}
tempoCall :: Config -> Maybe Expr.Symbol -> TrackTree.Track
    -> Derive.Deriver (TrackResults Signal.Control)
    -> Derive.NoteDeriver -> Derive.NoteDeriver
tempoCall config sym track sigDeriver deriver = do
    (signal, logs) <- Internal.inRealTime $ do
        (signal, logs) <- Derive.withVal EnvKey.controlGt0 True sigDeriver
        -- Do this in real time, so 'stash_if_wanted' knows it can directly
        -- reuse the signal.
        stashIfWanted track signal
        pure (signal, logs)

    -- The range is used by the tempo calls to normalize the block to 0--1.
    -- This should only happen for the tempo track at the top of the block.
    range <- maybe (pure Nothing) (fmap Just . Internal.blockLogicalRange) $
        TrackTree.trackBlockId track
    -- 'with_damage' must be applied *inside* 'Tempo.with_tempo'.  If it were
    -- outside, it would get the wrong RealTimes when it tried to create the
    -- ControlDamage.
    mergeLogs logs $ dispatchTempo config sym range
        maybeTrackId (Signal.coerce signal) (withDamage deriver)
    where
    maybeBlockTrackId = TrackTree.blockTrackId track
    maybeTrackId = snd <$> maybeBlockTrackId
    withDamage = maybe id getDamage maybeBlockTrackId
    getDamage (blockId, trackId) deriver = do
        damage <- Cache.getTempoDamage blockId trackId
            (TrackTree.trackEnd track)
            (TrackTree.trackEvents track)
        Internal.withControlDamage damage deriver

dispatchTempo :: Monoid a => Config -> Maybe Expr.Symbol
    -> Maybe (ScoreTime, ScoreTime) -> Maybe TrackId -> Signal.Tempo
    -> Derive.Deriver a -> Derive.Deriver a
dispatchTempo config sym blockRange maybeTrackId signal deriver =
    case sym of
        Nothing -> Tempo.withTempo toplevel blockRange maybeTrackId signal
            deriver
        Just sym
            | sym == "hybrid" -> Tempo.withHybrid toplevel blockRange
                maybeTrackId signal deriver
            | sym == "abs" -> Tempo.withAbsolute toplevel blockRange
                maybeTrackId signal deriver
            | otherwise -> Derive.throw $
                "unknown tempo modifier: " <> ShowVal.showVal sym
    where toplevel = configToplevelTempo config

{-# SCC controlCall #-}
controlCall :: TrackTree.Track -> ScoreT.Typed ScoreT.Control -> Derive.Merger
    -> TrackResults Signal.Control -> Derive.NoteDeriver -> Derive.NoteDeriver
controlCall track control merger (signal, logs) deriver = do
    stashIfWanted track signal
    -- Apply and strip any control modifications made during the above derive.
    end <- Derive.real $ TrackTree.trackEnd track
    Derive.evalControlMods end $ mergeLogs logs $ withDamage $
        withMerger control merger signal deriver
    -- I think this forces sequentialness because 'deriver' runs in the state
    -- from the end of 'sig_deriver'.  To make these parallelize, I need to
    -- run sig_deriver as a sub-derive, then mappend the Collect.
    where
    withDamage = withControlDamage
        (TrackTree.blockTrackId track) (TrackTree.trackRange track)

withMerger :: ScoreT.Typed ScoreT.Control -> Derive.Merger -> Signal.Control
    -> Derive.Deriver a -> Derive.Deriver a
withMerger (ScoreT.Typed typ control) merger signal =
    Derive.withMergedControl merger control (ScoreT.Typed typ signal)

mergeLogs :: [Log.Msg] -> Derive.NoteDeriver -> Derive.NoteDeriver
mergeLogs = fmap . Stream.mergeLogs

pitchCall :: Config -> TrackTree.Track -> ScoreT.PControl
    -> Pitch.ScaleId -> (Derive.PitchDeriver -> Derive.PitchDeriver)
    -> Derive.NoteDeriver -> Derive.NoteDeriver
pitchCall config track pcontrol scaleId transform deriver =
    withScale scaleId $ do
        (signal, logs) <- withPitchEnv pcontrol $
            derivePitch (configUseCache config) track transform
        -- Ignore errors, they should be logged on conversion.
        (nnSig, _) <- psignalToNn signal
        stashIfWanted track (Signal.coerce nnSig)
        -- Apply and strip any control modifications made during the above
        -- derive.
        end <- Derive.real $ TrackTree.trackEnd track
        Derive.evalControlMods end $ mergeLogs logs $ withDamage $
            Derive.withNamedPitch pcontrol signal deriver
    where
    withDamage = withControlDamage (TrackTree.blockTrackId track)
        (TrackTree.trackRange track)

withScale :: Pitch.ScaleId -> Derive.Deriver a -> Derive.Deriver a
withScale scaleId@(Pitch.ScaleId name) deriver
    | scaleId == Pitch.emptyScale = deriver
    | otherwise = Derive.withVal EnvKey.scale name deriver

withControlDamage :: Maybe (BlockId, TrackId) -> (TrackTime, TrackTime)
    -> Derive.Deriver d -> Derive.Deriver d
withControlDamage maybeBlockTrackId trackRange =
    maybe id getDamage maybeBlockTrackId
    where
    getDamage (blockId, trackId) deriver = do
        damage <- Cache.getControlDamage blockId trackId trackRange
        Internal.withControlDamage damage deriver

-- | Split the signal chunks and log msgs of the 'LEvent.LEvents' stream.
-- Return signal chunks merged into a signal, the logs cast to Score.Event
-- logs.
type TrackResults sig = (sig, [Log.Msg])

-- | Derive the signal of a control track.
deriveControl :: Bool -> TrackTree.Track
    -> (Derive.ControlDeriver -> Derive.ControlDeriver)
    -> Derive.Deriver (TrackResults Signal.Control)
deriveControl isTempo track transform = do
    let trackType
            | isTempo = ParseTitle.TempoTrack
            | otherwise = ParseTitle.ControlTrack
    (signal, logs) <- deriveTrack (trackInfo track trackType)
        (Cache.track track mempty . transform)
    signal <- trimSignal Signal.dropAfter Signal.clipAfter track signal
    pure (signal, logs)

{-# SCC derivePitch #-}
derivePitch :: Bool -> TrackTree.Track
    -> (Derive.PitchDeriver -> Derive.PitchDeriver)
    -> Derive.Deriver (TrackResults PSignal.PSignal)
derivePitch useCache track transform = do
    let cacheTrack = if useCache then Cache.track track mempty else id
    (psignal, logs) <- deriveTrack (trackInfo track ParseTitle.PitchTrack)
        (cacheTrack . transform)
    signal <- trimSignal PSignal.dropAfter PSignal.clipAfter track psignal
    pure (signal, logs)

{- | The controls under a note track are intended to apply only to the note
    above them.  However, since signal calls frequently emit samples before,
    "Derive.Slice" includes one event past the end of the slice range.  To
    avoid a sample for the next note getting into this one, I trim off samples
    at and after the end of the slice.  Otherwise, the decay of each note would
    want to change pitch to that of the next note.

    Slices with a zero duration have a special exception that causes them to
    include a sample at the end time, since otherwise they wouldn't have any
    controls.  This also applies to non-zero slices which are nonetheless made
    zero by stretching to 0.
-}
trimSignal :: (RealTime -> sig -> sig) -> (RealTime -> sig -> sig)
    -> TrackTree.Track -> sig -> Derive.Deriver sig
trimSignal dropAfter clipAfter track signal =
    case TrackTree.trackSliced track of
        TrackTree.NotSliced -> pure signal
        TrackTree.Inversion ->
            Derive.throw "unexpected Inversion of a control track"
        TrackTree.Sliced orientation -> do
            start <- Derive.real $ TrackTree.trackStart track
            end <- Derive.real $ TrackTree.trackEnd track
            -- I need to keep a signal exactly at end for certain blocks.
            let trim
                    | start == end || orientation == Types.Negative = dropAfter
                    | otherwise = clipAfter
            pure $ trim end signal
    -- TODO(polymorphic-signals)

{-# SCC deriveTrack #-}
deriveTrack :: (Monoid d, Derive.CallableExpr d) => EvalTrack.TrackInfo d
    -> (Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d))
    -> Derive.Deriver (TrackResults d)
deriveTrack tinfo transform = do
    stream <- transform $ do
        state <- Derive.get
        let (streams, threaded, collect) =
                EvalTrack.deriveControlTrack state tinfo
        Internal.mergeCollect collect
        Internal.setThreaded threaded
        pure $ compact $ concatMap Stream.toList streams
    let (signalChunks, logs) = Stream.partition stream
    -- I just merged the signals in 'compact', so this should just convert [x]
    -- to x.
    pure (mconcat signalChunks, logs)
    where
    -- Merge the signal here so it goes in the cache as one signal event.
    -- I can use concat instead of merge_asc_events because the signals
    -- will be merged with Signal.merge and the logs extracted.
    compact events = Stream.fromEventLogs (mconcat sigs) logs
        where (sigs, logs) = LEvent.partition events

-- | Make a TrackInfo for control tracks.
trackInfo :: Monoid d => TrackTree.Track -> ParseTitle.Type
    -> EvalTrack.TrackInfo d
trackInfo track trackType = EvalTrack.TrackInfo
    { tinfoTrack = track
    , tinfoSubTracks = []
    , tinfoType = trackType
    , tinfoGetLastVal = lastSignalVal
    }

lastSignalVal :: Monoid d => EvalTrack.GetLastVal d
lastSignalVal xs
    | null xs = Nothing
    | otherwise = Just (mconcat xs)

-- * TrackSignal

-- | If this track is to be rendered by the UI, stash the given signal in
-- either 'Derive.collect_track_signals' or 'Derive.collect_signal_fragments'.
{-# SCC stashIfWanted #-}
stashIfWanted :: TrackTree.Track -> Signal.Control -> Derive.Deriver ()
stashIfWanted track sig = whenM isNormalMode $
    whenJustM (renderOf track) $ \(blockId, trackId, _) ->
        case TrackTree.trackSliced track of
            TrackTree.NotSliced -> stashSignal blockId trackId sig
            _ -> stashSignalFragment blockId trackId
                -- The start of the range may not be unique in the case of
                -- orphans, I think because they aren't shifted to 0.  But
                -- the end should be unique.
                (snd $ TrackTree.trackRange track) sig

isNormalMode :: Derive.Deriver Bool
isNormalMode = Derive.getMode >>= \mode -> pure $ case mode of
    Derive.Normal -> True
    _ -> False

stashSignalFragment :: BlockId -> TrackId -> TrackTime -> Signal.Control
    -> Derive.Deriver ()
stashSignalFragment blockId trackId sliceEnd sig =
    -- TODO I think this is faster than Internal.merge_collect, but I haven't
    -- profiled so I don't know
    Internal.modifyCollect $ \collect -> collect
        { Derive.collectSignalFragments =
            Map.alter insert (blockId, trackId) $
                Derive.collectSignalFragments collect
        }
    where
    -- If a control is constant over multiple inverted notes, I'll get a bunch
    -- of identical signal fragments.  Rather than waiting for merge to
    -- eliminate them, it seems a bit more efficient to not collect them in the
    -- first place.
    insert (Just old) = Just $ case Map.lookupMax old of
        Just (_, prev) | sig == prev -> old
        _ -> Map.insert sliceEnd sig old
    insert _ = Just $ Map.singleton sliceEnd sig

stashSignal :: BlockId -> TrackId -> Signal.Control -> Derive.Deriver ()
stashSignal blockId trackId sig = do
    warp <- Internal.getWarp
    putTrackSignal blockId trackId $ EvalTrack.unwarp warp sig

putTrackSignal :: BlockId -> TrackId -> Track.TrackSignal -> Derive.Deriver ()
putTrackSignal blockId trackId tsig = Internal.mergeCollect $ mempty
    { Derive.collectTrackSignals = Map.singleton (blockId, trackId) tsig }

-- | Get render information if this track wants a TrackSignal.
renderOf :: TrackTree.Track
    -> Derive.Deriver (Maybe (BlockId, TrackId, Maybe Track.RenderSource))
renderOf track = case TrackTree.blockTrackId track of
    Nothing -> pure Nothing
    Just (blockId, trackId) -> ifM notRoot (pure Nothing) $ do
        (btrack, track) <- getBlockTrack blockId trackId
        let flags = Block.trackFlags btrack
        pure $ if Block.trackWantsSignal flags track
            then Just (blockId, trackId,
                extract (Track.renderStyle (Track.trackRender track)))
            else Nothing
    where
    notRoot = not <$> Internal.isRootBlock
    extract (Track.Line (Just source)) = Just source
    extract (Track.Filled (Just source)) = Just source
    extract _ = Nothing

getBlockTrack :: BlockId -> TrackId
    -> Derive.Deriver (Block.Track, Track.Track)
getBlockTrack blockId trackId = do
    track <- Derive.getTrack trackId
    block <- Derive.getBlock blockId
    btrack <- Derive.require
        ("get_block_track: " <> showt blockId <> " doesn't have "
            <> showt trackId) $
        List.find ((== Just trackId) . Block.trackId)
            (Block.blockTracks block)
    pure (btrack, track)


-- * util

-- | Reduce a 'PSignal.PSignal' to raw note numbers, taking the current
-- transposition environment into account.
psignalToNn :: PSignal.PSignal
    -> Derive.Deriver (Signal.NoteNumber, [(RealTime, PSignal.ErrorText)])
psignalToNn sig = do
    controls <- Derive.getControlMap
    pure $ PSignal.toNn $ PSignal.applyControls controls sig

withControlEnv :: ScoreT.Control -> Derive.Merger -> Derive.Deriver a
    -> Derive.Deriver a
withControlEnv control merger = Derive.withVals
    [ (EnvKey.control, ScoreT.controlName control)
    , (EnvKey.merge, ShowVal.showVal merger)
    ]

withPitchEnv :: ScoreT.PControl -> Derive.Deriver a -> Derive.Deriver a
withPitchEnv = Derive.withVal EnvKey.control . ScoreT.pcontrolName
