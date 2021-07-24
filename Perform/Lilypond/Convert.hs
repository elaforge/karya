-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE MultiParamTypeClasses #-}
-- | Convert Derive.Score output into Lilypond.Events.
module Perform.Lilypond.Convert (convert, pitch_to_lily, quantize) where
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Identity as Identity
import qualified Data.Set as Set

import qualified Util.CallStack as CallStack
import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.Flags as Flags
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale.All as Scale.All
import qualified Derive.Score as Score

import qualified Perform.ConvertUtil as ConvertUtil
import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Types as Types
import qualified Perform.Midi.Patch as Midi.Patch

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT

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
convert config = ConvertUtil.convert event1 lookup_inst . filter_instruments
    where
    -- Fake an instrument, which 'event1' will ignore.
    lookup_inst = const $ Just $ Cmd.ResolvedInstrument
        { inst_instrument = Inst.Inst
            (Inst.Midi $ Midi.Patch.patch (-1, 1) "ly-fake-inst")
            (Common.common Cmd.empty_code)
        , inst_qualified = InstT.Qualified "ly" "ly-fake-inst"
        , inst_common_config = Common.empty_config
        , inst_backend = Cmd.Dummy
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
    lilypond code directly.  Since zero is not a valid duration in staff
    notation, it's used for lilypond directives.  It should have either
    'Constants.free_code_key', or one of the special directives from Constants.
    All those events should be marked with 'Flags.ly_code'.
-}
convert_event :: RealTime -> Score.Event -> [LEvent.LEvent Types.Event]
convert_event quarter event = run $ do
    let dur = Types.real_to_time quarter (Score.event_duration event)
    maybe_pitch <- convert_pitch event
    -- Otherwise it will be misinterpreted as an explicit rest, which should be
    -- a purely internal concept.
    when (maybe_pitch == Nothing && not has_code_flag) $
        throw "a note without pitch must have code"
    when (dur == 0 && not has_code_flag) $
        throw $ "zero duration event must have " <> pretty Flags.ly_code
    return $ Types.Event
        { event_start = Types.real_to_time quarter (Score.event_start event)
        , event_duration =
            Types.real_to_time quarter (Score.event_duration event)
        , event_pitch = maybe_pitch
        , event_instrument = Score.event_instrument event
        , event_environ = Score.event_environ event
        , event_stack = Score.event_stack event
        , event_clipped = False
        }
    where
    has_code_flag = Flags.has (Score.event_flags event) Flags.ly_code
    run = (:[]) . either LEvent.Log LEvent.Event . Identity.runIdentity
        . Except.runExceptT

throw :: (CallStack.Stack, Except.MonadError Log.Msg m) => Text -> m a
throw = Except.throwError . Log.msg Log.Warn Nothing

convert_pitch :: Except.MonadError Log.Msg m =>
    Score.Event -> m (Maybe Types.Pitch)
convert_pitch event = case Score.initial_pitch event of
    Nothing -> return Nothing
    Just pitch -> either (throw . ("convert_pitch: "<>)) (return . Just) $
        pitch_to_lily (Score.event_environ event) pitch

-- * util

pitch_to_lily :: Env.Environ -> PSignal.Transposed -> Either Text Types.Pitch
pitch_to_lily env pitch = do
    let scale_id = PSignal.pitch_scale_id pitch
    case (\(Derive.LookupScale a) -> a) lookup_scale env scale_id of
        Nothing -> Left $ "scale id not found: " <> pretty scale_id
        Just (Left err) ->
            Left $ "scale " <> pretty scale_id <> ": " <> pretty err
        Just (Right scale) -> do
            note <- first pretty $ PSignal.pitch_note pitch
            pitch <- first pretty $ Derive.scale_read scale env note
            Types.parse_pitch pitch

quantize :: Types.Duration -> [Types.Event] -> [Types.Event]
quantize dur = map $ \e -> e
    { Types.event_start = q (Types.event_start e)
    , Types.event_duration = q (Types.event_duration e)
    }
    where q = quantize_time (Types.dur_to_time dur)

quantize_time :: Types.Time -> Types.Time -> Types.Time
quantize_time time t =
    round (fromIntegral t / fromIntegral time :: Double) * time

lookup_scale :: Derive.LookupScale
lookup_scale = Scale.All.lookup_scale
