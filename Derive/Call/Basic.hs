-- | A collection of basic calls.
module Derive.Call.Basic where
import qualified Data.List as List
import qualified Data.Map as Map

import Ui
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

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


-- | This is here instead of Derive because note calls first look at the block
-- ids to derive a block.
lookup_note_call :: TrackLang.CallId -> Derive.Deriver Derive.Call
lookup_note_call call_id = do
    st <- Derive.get
    let default_ns = State.state_project (Derive.state_ui st)
        block_id = Types.BlockId (make_id default_ns call_id)
    let call_map = Derive.calls_note (Derive.state_call_map st)
    if block_id `Map.member` State.state_blocks (Derive.state_ui st)
        then return $ c_block block_id
        else case Map.lookup call_id call_map of
            Nothing -> return (c_not_found call_id)
            Just call -> return call

-- | I don't want to abort all of derivation by throwing, but I do want to
-- abort evaluation of this expression, so consider this a kind of type error,
-- which does just that.
c_not_found :: TrackLang.CallId -> Derive.Call
c_not_found call_id = Derive.Call
    (Just $ \_ _ _ _ -> err) (Just $ \_ _ -> err)
    where err = Left (TrackLang.CallNotFound call_id)

one_note :: Derive.EventDeriver -> (Derive.EventDeriver, Int)
one_note d = (d, 1)

-- * note call

-- | The note call is the default deriver for a track.  As a convenience, it
-- will interpret @>inst@ and @+attr@ args as the relevant assignments,
-- which means you can assign these to a note generator or a call with an
-- abbreviated syntax: @+attr@ to generate a note with that attr, or
-- @>i | call@ to run call with that instrument.
c_note :: Derive.Call
c_note = Derive.Call
    (Just $ \args _ event next -> case process args of
        (inst, rel_attrs, []) ->
            Right $ one_note $ generate_note inst rel_attrs event next
        (_, _, invalid) -> Left $
            TrackLang.ArgError $ "expected inst or attr: " ++ show invalid)
    (Just $ \args deriver -> case process args of
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
    start <- Derive.local_to_global pos
    end <- Derive.local_to_global (pos + Event.event_duration event)
    -- TODO due to negative durations, end could be before start.  I need to
    -- add a post-proc step to calculate the proper durations
    next_start <- case next of
        [] -> return Nothing
        (npos, _) : _ -> fmap Just (Derive.local_to_global npos)
    inst <- case n_inst of
        Just inst -> return (Just inst)
        Nothing -> Derive.lookup_val TrackLang.v_instrument
    attrs <- Derive.get_val Score.no_attrs TrackLang.v_attributes
    st <- Derive.get
    let pitch_sig = trimmed_pitch next_start (Derive.state_pitch st)
    return [Score.Event start (end - start)
        (Event.event_text event) (Derive.state_controls st)
        pitch_sig (Derive.state_stack st) inst (apply rel_attrs attrs)]
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
trimmed_pitch :: Maybe TrackPos -> PitchSignal.PitchSignal
    -> PitchSignal.PitchSignal
trimmed_pitch (Just next) sig = PitchSignal.truncate next sig
trimmed_pitch Nothing sig = sig

-- * block call

c_block :: BlockId -> Derive.Call
c_block block_id = Derive.generate_one $ \args _ (pos, event) _ ->
    if null args then Right $ block_call block_id pos event
    else Left $ TrackLang.ArgError "args for block call not implemented yet"

block_call :: BlockId -> TrackPos -> Event.Event -> Derive.EventDeriver
block_call block_id pos event =
    Derive.d_at start $ Derive.d_stretch (end-start) $
        Derive.d_sub_derive [] (Derive.d_block block_id)
    where
    -- Derivation happens according to the extent of the note, not the
    -- duration.  This is how negative duration events begin deriving before
    -- arriving at the trigger.
    (start, end) = (Track.event_min (pos, event), Track.event_max (pos, event))

-- | Make an Id from a string, relative to the current ns if it doesn't already
-- have one.
--
-- TODO move this to a more generic place since LanguageCmds may want it to?
make_id :: String -> TrackLang.CallId -> Id.Id
make_id default_ns (TrackLang.Symbol ident_str) = Id.id ns ident
    where
    (w0, w1) = break (=='/') ident_str
    (ns, ident) = if null w1 then (default_ns, w0) else (w0, drop 1 w1)

-- * equal

c_equal :: Derive.Call
c_equal = Derive.Call
    (Just $ \args _ _ _ -> with_args args generate)
    (Just $ \args deriver -> with_args args (transform deriver))
    where
    with_args args = TrackLang.call2 args
        (required "symbol", required "value" :: Arg TrackLang.Val)
    transform deriver sym val = Derive.with_val sym val deriver
    generate sym val = one_note $ Derive.put_val sym val >> return []
