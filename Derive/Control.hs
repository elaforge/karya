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
import qualified Util.Parse as Parse

import Ui
import qualified Ui.Track as Track

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal

import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang
import qualified Derive.Schema.Default as Default


d_relative :: (Monad m) =>
    Score.Control -> TrackLang.CallId -> Derive.DeriveT m Signal.Control
    -> Derive.DeriveT m [Score.Event] -> Derive.DeriveT m [Score.Event]
d_relative cont op signalm eventsm = do
    signal <- signalm
    Derive.with_control_operator cont op signal eventsm

d_relative_pitch :: (Monad m) => TrackLang.CallId
    -> Derive.DeriveT m PitchSignal.Relative
    -> Derive.DeriveT m [Score.Event] -> Derive.DeriveT m [Score.Event]
d_relative_pitch op signalm eventsm = do
    signal <- signalm
    Derive.with_pitch_operator op signal eventsm

-- | Top level deriver for control tracks.
d_control_track :: BlockId -> TrackId -> Derive.Transformer
d_control_track block_id track_id deriver = do
    track <- Derive.get_track track_id
    title_expr <- case TrackLang.parse (Track.track_title track) of
        Left err -> Derive.throw $ "track title: " ++ err
        Right expr -> return expr
    -- TODO event calls are evaluated in normalized time, but track calls
    -- aren't.  Should they be?
    join $ eval_track block_id track_id title_expr deriver

eval_track :: BlockId -> TrackId -> TrackLang.Expr
    -> Derive.EventDeriver -> Derive.Deriver Derive.EventDeriver
eval_track block_id track_id [TrackLang.Call call_id args] deriver = do
    track <- Derive.get_track track_id
    let events = Track.event_list (Track.track_events track)

    -- The control track title doesn't follow the normal syntax of
    -- "symbol val val ...".  At least not for the final expression in the
    -- pipeline.
    let vals = if call_id == TrackLang.Symbol ""
            then args else TrackLang.VSymbol call_id : args
    return $ case Default.parse_control_vals vals of
        Right Default.Tempo -> tempo_call block_id track_id
            (Signal.coerce <$> derive_signal events) deriver
        Right (Default.Control Nothing control) ->
            control_call track_id control (derive_signal events) deriver
        Right (Default.Pitch (Default.PitchAbsolute maybe_scale_id) Nothing) ->
            pitch_call track_id maybe_scale_id events Nothing deriver
        Right track_type ->
            Derive.throw $ "track type not supported yet: " ++ show track_type
        Left msg ->
            Derive.throw $ "failed to parse " ++ show vals ++ ": " ++ msg
eval_track _ _ _expr _ = return $
    Derive.throw "composition not supported on control tracks yet"

-- | A tempo track is derived like other signals, but in absolute time.
-- Otherwise it would wind up being composed with the environmental
-- warp twice.
tempo_call :: BlockId -> TrackId -> Derive.Deriver Signal.Tempo
    -> Derive.Transformer
tempo_call block_id track_id sig = Derive.d_tempo block_id (Just track_id) $
    Derive.with_warp (const Score.id_warp) sig

control_call :: TrackId -> Score.Control -> Derive.ControlDeriver
    -> Derive.Transformer
control_call track_id control deriver = d_control control $
    Derive.with_track_warp track_id deriver

d_control :: Score.Control -> Derive.ControlDeriver -> Derive.Transformer
d_control cont control_deriver deriver = do
    signal <- control_deriver
    Derive.with_control cont signal deriver

pitch_call :: TrackId -> Maybe Pitch.ScaleId -> [Track.PosEvent]
    -> Maybe Score.Control -> Derive.Transformer
pitch_call _ _ _ (Just _) =
    const $ Derive.throw $ "named pitch tracks not supported yet"
pitch_call track_id maybe_scale_id events Nothing = d_pitch maybe_scale_id
    (Derive.with_track_warp track_id (derive_pitch events))

d_pitch :: Maybe Pitch.ScaleId -> Derive.PitchDeriver -> Derive.Transformer
d_pitch (Just scale_id) control_deriver deriver = do
    scale <- Derive.get_scale "d_pitch" scale_id
    Derive.with_val TrackLang.v_scale scale $ do
        signal <- control_deriver
        Derive.with_pitch signal deriver
d_pitch Nothing control_deriver deriver = do
    signal <- control_deriver
    Derive.with_pitch signal deriver


derive_pitch :: [Track.PosEvent] -> Derive.PitchDeriver
derive_pitch = fmap PitchSignal.merge
    . Call.derive_track
        ("pitch", Derive.no_pitch, Derive.lookup_pitch_call, mangle_pitch_call)
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

derive_signal :: [Track.PosEvent] -> Derive.ControlDeriver
derive_signal = fmap Signal.merge
    . Call.derive_track
        ("control", Derive.no_control, Derive.lookup_control_call, id)
        (\prev chunk -> Signal.last chunk `mplus` prev)

-- | A relative pitch track is parsed as an ordinary control track, so
-- a relative pitch is unparsed into a control \"set\" expression.
unparse_relative :: Double -> String
unparse_relative = Parse.show_float (Just 2)
