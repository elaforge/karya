-- | A collection of basic calls.
--
-- Unlike other Call modules, this is imported by "Derive.Call" so it can't
-- import it.  This is because c_block is hardcoded into
-- 'Derive.Call.lookup_note_call'.
module Derive.Call.Basic where
import qualified Data.List as List

import Ui
import qualified Ui.Event as Event
import qualified Ui.Track as Track

import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang
import Derive.TrackLang (Arg, required)
import qualified Derive.Score as Score

import qualified Perform.PitchSignal as PitchSignal


note_calls :: Derive.CallMap
note_calls = Derive.make_calls
    [ ("", c_note)
    , ("=", c_equal)
    ]

control_calls :: Derive.CallMap
control_calls = Derive.make_calls []

-- * note call

-- | The note call is the default deriver for a track.  As a convenience, it
-- will interpret @>inst@ and @+attr@ args as the relevant assignments,
-- which means you can assign these to a note generator or a call with an
-- abbreviated syntax: @+attr@ to generate a note with that attr, or
-- @>i | call@ to run call with that instrument.
c_note :: Derive.Call
c_note = Derive.Call
    (Just $ \args _ event next -> case process (TrackLang.passed_vals args) of
        (inst, rel_attrs, []) ->
            Right $ one_note $ generate_note inst rel_attrs event next
        (_, _, invalid) -> Left $
            TrackLang.ArgError $ "expected inst or attr: " ++ show invalid)
    (Just $ \args _ deriver -> case process (TrackLang.passed_vals args) of
        (inst, rel_attrs, []) -> Right $ transform_note inst rel_attrs deriver
        (_, _, invalid) -> Left $
            TrackLang.ArgError $ "expected inst or attr: " ++ show invalid)
    where
    process = process_note_args Nothing []

generate_note :: Maybe Score.Instrument -> [TrackLang.RelativeAttr]
    -> Track.PosEvent -> [Track.PosEvent] -> Derive.EventDeriver
generate_note n_inst rel_attrs (pos, event) next = do
    -- I could use the same code as the Transformer case, but this is a
    -- common case and it's probably more efficient to do it directly.
    start <- Derive.score_to_real pos
    end <- Derive.score_to_real (pos + Event.event_duration event)
    -- TODO due to negative durations, end could be before start.  I need to
    -- add a post-proc step to calculate the proper durations
    next_start <- case next of
        [] -> return Nothing
        (npos, _) : _ -> fmap Just (Derive.score_to_real npos)
    inst <- case n_inst of
        Just inst -> return (Just inst)
        Nothing -> Derive.lookup_val TrackLang.v_instrument
    attrs <- Derive.get_val Score.no_attrs TrackLang.v_attributes
    (controls, pitch_sig) <- Derive.unwarped_controls
    st <- Derive.get
    return [Score.Event start (end - start)
        (Event.event_text event) controls (trimmed_pitch next_start pitch_sig)
        (Derive.state_stack st) inst (apply rel_attrs attrs)]
    where
    apply rel_attrs attrs =
        List.foldl' (.) id (map TrackLang.set_attr rel_attrs) attrs

transform_note :: Maybe Score.Instrument -> [TrackLang.RelativeAttr]
    -> Derive.EventDeriver -> Derive.EventDeriver
transform_note n_inst rel_attrs deriver = with_inst (with_attrs deriver)
    where
    with_inst = maybe id (Derive.with_val TrackLang.v_instrument) n_inst
    with_attrs =
        foldl (.) id (map (Derive.with_val TrackLang.v_attributes) rel_attrs)

process_note_args :: Maybe Score.Instrument
    -> [TrackLang.RelativeAttr] -> [TrackLang.Val]
    -> (Maybe Score.Instrument, [TrackLang.RelativeAttr], [TrackLang.Val])
process_note_args inst attrs args = (inst', attrs', reverse invalid)
    where
    (inst', attrs', invalid) = List.foldl' go (inst, attrs, []) args
    go (inst, attrs, invalid) arg = case arg of
        TrackLang.VInstrument new_inst
            | TrackLang.is_null_instrument new_inst -> (inst, attrs, invalid)
            | otherwise -> (Just new_inst, attrs, invalid)
        TrackLang.VRelativeAttr rel_attr ->
            (inst, attrs ++ [rel_attr], invalid)
        _ -> (inst, attrs, arg : invalid)

-- | In a note track, the pitch signal for each note is constant as soon as the
-- next note begins.  Otherwise, it looks like each note changes pitch during
-- its decay.
-- TODO when signals are lazy I should drop the heads of the control
-- signals so they can be gced.
trimmed_pitch :: Maybe RealTime -> PitchSignal.PitchSignal
    -> PitchSignal.PitchSignal
trimmed_pitch (Just next) sig = PitchSignal.truncate next sig
trimmed_pitch Nothing sig = sig

-- * block call

c_block :: BlockId -> Derive.Call
c_block block_id = Derive.generate_one $ \args _ (pos, event) _ ->
    if null (TrackLang.passed_vals args)
        then Right $ block_call block_id pos event
        else Left $ TrackLang.ArgError "args for block call not implemented yet"

block_call :: BlockId -> ScoreTime -> Event.Event -> Derive.EventDeriver
block_call block_id pos event =
    Derive.d_at start $ Derive.d_stretch (end-start) $
        Derive.d_sub_derive [] (Derive.d_block block_id)
    where
    -- Derivation happens according to the extent of the note, not the
    -- duration.  This is how negative duration events begin deriving before
    -- arriving at the trigger.
    (start, end) = (Track.event_min (pos, event), Track.event_max (pos, event))

-- * equal

c_equal :: Derive.Call
c_equal = Derive.Call
    (Just $ \args _ _ _ -> with_args args generate)
    (Just $ \args _ deriver -> with_args args (transform deriver))
    where
    with_args args = TrackLang.call2 args
        (required "symbol", required "value" :: Arg TrackLang.Val)
    transform deriver sym val = Derive.with_val sym val deriver
    generate sym val = one_note $ Derive.put_val sym val >> return []

-- * misc

one_note :: Derive.EventDeriver -> (Derive.EventDeriver, Int)
one_note d = (d, 1)
