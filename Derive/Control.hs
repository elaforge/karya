{- | Derivers for control tracks.

    Interpolation methods:

    - s - Set value at the point.  This is the default if there is no method.

    - i - Approach with linear interpolation.

    - #e - Approach with exponential interpolation with #.  # defaults to 2.

    - method;val - Approach val with method, then jump to val.
-}
module Derive.Control where
import Prelude
import Control.Monad

import Util.Control
import qualified Util.Pretty as Pretty

import Ui
import qualified Ui.Track as Track

import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal

import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.Scale.Relative as Relative
import qualified Derive.Score as Score
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang


-- | Top level deriver for control tracks.
d_control_track :: BlockId -> TrackId -> Derive.Transformer
d_control_track block_id track_id deriver = do
    track <- Derive.get_track track_id
    (expr, vals) <- either (\err -> Derive.throw $ "track title: " ++ err)
        return (TrackLang.parse_control_track (Track.track_title track))
    -- TODO event calls are evaluated in normalized time, but track calls
    -- aren't.  Should they be?
    eval_track block_id track_id expr vals deriver

eval_track :: BlockId -> TrackId -> TrackLang.Expr -> [TrackLang.Val]
    -> Derive.Transformer
eval_track block_id track_id expr vals deriver = do
    track <- Derive.get_track track_id
    let events = Track.event_list (Track.track_events track)
    case TrackInfo.parse_control_vals vals of
        Right TrackInfo.Tempo -> do
            control_deriver <- derive_control expr events
            tempo_call block_id track_id
                (Signal.coerce <$> control_deriver) deriver
        Right (TrackInfo.Control maybe_op control) -> do
            control_deriver <- derive_control expr events
            control_call track_id control maybe_op control_deriver deriver
        Right (TrackInfo.Pitch ptype maybe_name) ->
            pitch_call track_id maybe_name ptype expr events deriver
        Left msg -> Derive.throw $
            "failed to parse " ++ Pretty.pretty vals ++ ": " ++ msg

-- | A tempo track is derived like other signals, but in absolute time.
-- Otherwise it would wind up being composed with the environmental
-- warp twice.
tempo_call :: BlockId -> TrackId -> Derive.Deriver Signal.Tempo
    -> Derive.Transformer
tempo_call block_id track_id sig_deriver =
    Derive.d_tempo block_id (Just track_id) $
        Derive.setup_without_warp sig_deriver

control_call :: TrackId -> Score.Control -> Maybe TrackLang.CallId
    -> Derive.ControlDeriver -> Derive.Transformer
control_call track_id control maybe_op control_deriver =
    with_control $ Derive.track_setup track_id control_deriver
    where
    with_control control_deriver deriver = do
        signal <- control_deriver
        case maybe_op of
            Nothing -> Derive.with_control control signal deriver
            Just op -> Derive.with_control_operator control op signal deriver

pitch_call :: TrackId -> Maybe Score.Control -> TrackInfo.PitchType
    -> TrackLang.Expr -> [Track.PosEvent] -> Derive.Transformer
pitch_call _ (Just _) _ _ _ _ =
    Derive.throw $ "named pitch tracks not supported yet"
pitch_call track_id Nothing ptype track_expr events deriver =
    Derive.track_setup track_id $ do
        with_scale <- case ptype of
            TrackInfo.PitchRelative _ -> do
                scale <- Derive.lookup_val TrackLang.v_scale
                let relative_scale = maybe Relative.scale Relative.adjust scale
                return $ Derive.with_val TrackLang.v_scale relative_scale
            TrackInfo.PitchAbsolute (Just scale_id) -> do
                scale <- Derive.get_scale "pitch_call" scale_id
                return $ Derive.with_val TrackLang.v_scale scale
            TrackInfo.PitchAbsolute Nothing -> return id
        with_scale $ case ptype of
            TrackInfo.PitchRelative op -> do
                signal <- derive_relative_pitch events
                Derive.with_pitch_operator op signal deriver
            _ -> do
                signal <- join $ derive_pitch track_expr events
                Derive.with_pitch signal deriver

derive_pitch :: TrackLang.Expr -> [Track.PosEvent]
    -> Derive.Deriver Derive.PitchDeriver
derive_pitch track_expr events =
    Call.eval_transformer info 1 track_expr deriver
    where
    deriver = PitchSignal.merge <$> Call.derive_track info
        (\prev chunk -> PitchSignal.last chunk `mplus` prev) events
    info = ("pitch", Derive.no_pitch, Derive.lookup_pitch_call,
        mangle_pitch_call)

derive_relative_pitch :: [Track.PosEvent] -> Derive.PitchDeriver
derive_relative_pitch = fmap PitchSignal.merge
    . Call.derive_track
        ("relative pitch", Derive.no_pitch, Derive.lookup_pitch_call,
            mangle_pitch_call)
        (\prev chunk -> PitchSignal.last chunk `mplus` prev)

-- | I really want to be able to give notes arbitrary names.  Especially
-- numbers are good names for notes.  Unfortunately numbers also look like
-- numbers.  Note literal syntax prefixes *, but a pitch track consists mostly
-- of note literals so it looks ugly.  So a pitch track automatically prefixes
-- *s when it consists of a single token.
mangle_pitch_call :: String -> String
mangle_pitch_call text
    | ' ' `elem` text = text
    | otherwise = '*' : text

derive_control :: TrackLang.Expr -> [Track.PosEvent]
    -> Derive.Deriver Derive.ControlDeriver
derive_control track_expr events =
    Call.eval_transformer info 1 track_expr deriver
    where
    deriver = Signal.merge <$> Call.derive_track info
        (\prev chunk -> Signal.last chunk `mplus` prev) events
    info = ("control", Derive.no_control, Derive.lookup_control_call, id)
