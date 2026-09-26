-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE MultiParamTypeClasses #-}
-- | Convert Derive.Score output into Lilypond.Events.
module Perform.Lilypond.Convert (convert, pitchToLily, quantize) where
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Identity as Identity
import qualified Data.Set as Set
import           GHC.Stack (HasCallStack)

import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.Flags as Flags
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale.All as Scale.All
import qualified Derive.Score as Score

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT

import qualified Perform.ConvertUtil as ConvertUtil
import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Types as Types
import qualified Perform.Midi.Patch as Midi.Patch

import           Global
import           Types


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
convert config = ConvertUtil.convert event1 lookupInst . filterInstruments
    where
    -- Fake an instrument, which 'event1' will ignore.
    lookupInst = const $ Just $ Cmd.ResolvedInstrument
        { instInstrument = Inst.Inst
            (Inst.Midi $ Midi.Patch.patch (-1, 1) "ly-fake-inst")
            (Common.common Cmd.emptyCode)
        , instQualified = InstT.Qualified "ly" "ly-fake-inst"
        , instCommonConfig = Common.emptyConfig
        , instBackend = Cmd.Dummy "fake lilypond instrument"
        }
    event1 event _resolved =
        convertEvent (Types.configQuarterDuration config) event
    filterInstruments
        | null (Types.configStaves config) = id
        | otherwise = filter ((`Set.member` insts) . Score.eventInstrument)
    insts = Set.fromList $ Constants.lyGlobal :
        [ inst
        | (inst, staff) <- Types.configStaves config
        , Types.staffDisplay staff
        ]

{- | Normally events have a duration and a pitch, and the lilypond performer
    converts this into a normal lilypond note.  However, the deriver can emit
    lilypond code directly.  Since zero is not a valid duration in staff
    notation, it's used for lilypond directives.  It should have either
    'Constants.free_code_key', or one of the special directives from Constants.
    All those events should be marked with 'Flags.ly_code'.
-}
convertEvent :: RealTime -> Score.Event -> [LEvent.LEvent Types.Event]
convertEvent quarter event = run $ do
    let dur = Types.realToTime quarter (Score.eventDuration event)
    maybePitch <- convertPitch event
    -- Otherwise it will be misinterpreted as an explicit rest, which should be
    -- a purely internal concept.
    when (maybePitch == Nothing && not hasCodeFlag) $
        throw "a note without pitch must have code"
    when (dur == 0 && not hasCodeFlag) $
        throw $ "zero duration event must have " <> pretty Flags.lyCode
    pure $ Types.Event
        { eventStart = Types.realToTime quarter (Score.eventStart event)
        , eventDuration =
            Types.realToTime quarter (Score.eventDuration event)
        , eventPitch = maybePitch
        , eventInstrument = Score.eventInstrument event
        , eventEnviron = Score.eventEnviron event
        , eventStack = Score.eventStack event
        , eventClipped = False
        }
    where
    hasCodeFlag = Flags.has (Score.eventFlags event) Flags.lyCode
    run = (:[]) . either LEvent.Log LEvent.Event . Identity.runIdentity
        . Except.runExceptT

throw :: (HasCallStack, Except.MonadError Log.Msg m) => Text -> m a
throw = Except.throwError . Log.msg Log.Warn Nothing

convertPitch :: Except.MonadError Log.Msg m =>
    Score.Event -> m (Maybe Types.Pitch)
convertPitch event = case Score.initialPitch event of
    Nothing -> pure Nothing
    Just pitch -> either (throw . ("convert_pitch: "<>)) (pure . Just) $
        pitchToLily (Score.eventEnviron event) pitch

-- * util

pitchToLily :: Env.Environ -> PSignal.Transposed -> Either Text Types.Pitch
pitchToLily env pitch =
    case (\(Derive.LookupScale a) -> a) lookupScale env scaleId of
        Nothing -> Left $ "scale id not found: " <> pretty scaleId
        Just (Left err) ->
            Left $ "scale " <> pretty scaleId <> ": " <> pretty err
        Just (Right scale) -> do
            note <- first pretty $ PSignal.pitchNote pitch
            pitch <- first pretty $ Derive.scaleRead scale env note
            Types.parsePitch pitch
    where scaleId = PSignal.pitchScaleId pitch

quantize :: Types.Duration -> [Types.Event] -> [Types.Event]
quantize dur = map $ \e -> e
    { Types.eventStart = q (Types.eventStart e)
    , Types.eventDuration = q (Types.eventDuration e)
    }
    where q = quantizeTime (Types.durToTime dur)

quantizeTime :: Types.Time -> Types.Time -> Types.Time
quantizeTime time t =
    round (fromIntegral t / fromIntegral time :: Double) * time

lookupScale :: Derive.LookupScale
lookupScale = Scale.All.lookupScale
