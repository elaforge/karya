-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Convert Derive.Score output into Lilypond.Events.
module Perform.Lilypond.Convert (convert, pitch_to_lily, quantize) where
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Identity as Identity
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Util.CallStack as CallStack
import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd
import qualified Derive.Env as Env
import qualified Derive.Flags as Flags
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score

import qualified Perform.ConvertUtil as ConvertUtil
import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Types as Types
import qualified Perform.Midi.Patch as Midi.Patch

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes

import Global
import Types


-- * convert

-- | Convert Score events to Perform events, emitting warnings that may have
-- happened along the way.
--
-- Unlike the other backend converts, this one doesn't need a lookup inst
-- function.  It just fakes up an inst for whatever you ask it.  This means
-- 'Constants.ly_global' doesn't actually need an allocation.  The bad part is
-- that postproc is not applied, but I'll worry about that if I ever have
-- a postproc that affects lilypond.
convert :: Types.Config -> [Score.Event] -> [LEvent.LEvent Types.Event]
convert config =
    ConvertUtil.convert event1 lookup_inst . filter_instruments
    where
    -- Fake an instrument, which 'event1' will ignore.
    lookup_inst = const $ Just $ Cmd.ResolvedInstrument
        { inst_instrument = Inst.Inst
            (Inst.Midi $ Midi.Patch.patch (-1, 1) "ly-fake-inst")
            (Common.common Cmd.empty_code)
        , inst_qualified = InstTypes.Qualified "ly" "ly-fake-inst"
        , inst_common_config = Common.empty_config
        , inst_backend = Nothing
        }
    event1 event _resolved =
        convert_event (Types.config_quarter_duration config) event
    filter_instruments
        | null (Types.config_staves config) = id
        | otherwise = filter ((`Set.member` insts) . Score.event_instrument)
    insts = Set.fromList $ Constants.ly_global :
        [ inst
        | (inst, staff) <- Types.config_staves config
        , Types.staff_display staff
        ]

{- | Normally events have a duration and a pitch, and the lilypond performer
    converts this into a normal lilypond note.  However, the deriver can emit
    lilypond code directly with either a zero duration event or one without
    a pitch:

    - If the event has 0 duration, it must have prepend or append code or have
    the magic 'Constants.ly_global' instrument.  The code will go before or
    after other events at the same time.  Any pitch is ignored.

    - If it doesn't have a pitch, then it must have prepend or append code.
    prepend ++ append will be emitted and considered to have the given amount
    of duration.

    - If the event has a pitch it will be emitted as a note, with optional
    prepended or appended code.
-}
convert_event :: RealTime -> Score.Event -> [LEvent.LEvent Types.Event]
convert_event quarter event = run $ do
    let dur = Types.real_to_time quarter (Score.event_duration event)
    maybe_pitch <- convert_pitch event
    pitch <- case (dur, maybe_pitch) of
        (0, _) -> check_0dur >> return Nothing
        (_, Nothing)
            | not (has_prepend || has_append) ->
                throw "event with non-zero duration and no code requires pitch"
            | otherwise -> return Nothing
        (_, Just pitch) -> return (Just pitch)
    return $ Types.Event
        { event_start = Types.real_to_time quarter (Score.event_start event)
        , event_duration =
            Types.real_to_time quarter (Score.event_duration event)
        , event_pitch = pitch
        , event_instrument = Score.event_instrument event
        , event_environ = Score.event_environ event
        , event_stack = Score.event_stack event
        , event_clipped = False
        }
    where
    check_0dur
        | not is_ly_global && not has_code_flag =
            throw $ "zero duration event must have " <> pretty Flags.ly_code
        | has_prepend && has_append = throw
            "zero duration event with both prepend and append is ambiguous"
        | otherwise = return ()
    has_code_flag = Flags.has (Score.event_flags event) Flags.ly_code
    is_ly_global = Score.event_instrument event == Constants.ly_global
    has_prepend = has Constants.v_prepend
    has_append = has Constants.v_append_all
    has v = Maybe.isJust $ Env.lookup v (Score.event_environ event)
    run = (:[]) . either LEvent.Log LEvent.Event . Identity.runIdentity
        . Except.runExceptT

throw :: (CallStack.Stack, Except.MonadError Log.Msg m) => Text -> m a
throw = Except.throwError . Log.msg Log.Warn Nothing

convert_pitch :: Except.MonadError Log.Msg m => Score.Event
    -> m (Maybe Types.Pitch)
convert_pitch event = case Score.initial_pitch event of
    Nothing -> return Nothing
    Just pitch -> either (throw . ("convert_pitch: "<>)) (return . Just) $
        pitch_to_lily pitch

-- * util

-- | If it's @*twelve@ then use pitch_note, else use pitch_nn and pick the
-- closest pitch.  TODO I should use Pitch for everyone.  Not only does it
-- not go crazy for non 12-tet, but it preserves accidentals.
pitch_to_lily :: PSignal.Transposed -> Either Text Types.Pitch
pitch_to_lily pitch
    | PSignal.pitch_scale_id pitch == Twelve.scale_id = do
        note <- first showt $ PSignal.pitch_note pitch
        parse_note note
    | otherwise = do
        nn <- first showt $ PSignal.pitch_nn pitch
        note <- require ("nn out of range: " <> pretty nn) $
            Twelve.nn_to_note nn
        parse_note note
    where
    parse_note note = do
        pitch <- require ("unparseable note: " <> pretty note) $
            Twelve.read_absolute_pitch note
        Types.parse_pitch pitch
    require msg Nothing = Left msg
    require _ (Just x) = Right x

quantize :: Types.Duration -> [Types.Event] -> [Types.Event]
quantize dur = map $ \e -> e
    { Types.event_start = q (Types.event_start e)
    , Types.event_duration = q (Types.event_duration e)
    }
    where q = quantize_time (Types.dur_to_time dur)

quantize_time :: Types.Time -> Types.Time -> Types.Time
quantize_time time t =
    round (fromIntegral t / fromIntegral time :: Double) * time
