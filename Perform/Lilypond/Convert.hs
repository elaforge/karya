{-# LANGUAGE FlexibleInstances #-} -- for Error instance
-- | Convert Derive.Score output into Lilypond.Events.
module Perform.Lilypond.Convert where
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State.Strict as State

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Log as Log
import qualified Util.Logger as Logger
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Scale.TwelveUtil as TwelveUtil
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Warning as Warning

import Types

-- TODO lots of copy paste with Perform.Midi.Convert, should factor out the
-- common bits.

-- | Convert Score events to Perform events, emitting warnings that may have
-- happened along the way.
convert :: Lilypond.Duration
    -- ^ 1 second becomes this Duration, e.g. 4 means one second is a quarter
    -- note
    -> Derive.Events -> [LEvent.LEvent Lilypond.Event]
convert dur1 events = go () Nothing events
    where
    go _ _ [] = []
    go state prev (LEvent.Log log : rest) =
        LEvent.Log log : go state prev rest
    go state prev (LEvent.Event event : rest) =
        maybe [] ((:[]) . LEvent.Event) maybe_event ++ logs
            ++ go next_state (Just (Score.event_start event)) rest
        where
        (maybe_event, warns, next_state) = run_convert state
            (Score.event_stack event)
            (convert_event dur1 prev event)
        logs = map (LEvent.Log . warn_to_log) warns

convert_event :: Lilypond.Duration -> Maybe RealTime -> Score.Event
    -> ConvertT Lilypond.Event
convert_event dur1 maybe_prev event = do
    -- Sorted is a postcondition of the deriver.
    when_just maybe_prev $ \prev -> when (Score.event_start event < prev) $
        warn $ "start time " ++ Pretty.pretty (Score.event_start event)
            ++ " less than previous of " ++ Pretty.pretty prev
    pitch <- convert_pitch (Score.event_start event)
        (Score.event_controls event) (Score.event_pitch event)
    pitch <- either (throw . ("show_pitch: " ++)) return
        (Lilypond.show_pitch pitch)
    return $ Lilypond.Event
        { Lilypond.event_start =
            real_to_time dur1 (Score.event_start event)
        , Lilypond.event_duration =
            real_to_time dur1 (Score.event_duration event)
        , Lilypond.event_pitch = pitch
        }

-- TODO This loses the enharmonics.
-- I need to record pitch signals as Strings and not PitchSignal.  This is
-- appropriate for e.g. trill, which shouldn't actually trill the note.
-- I still need the pitch signal in case other things depend on it, but I
-- should record the text of the pitch along with the Score.Event.
-- So events can get metadata too, and 'ly.pitch' holds the text of the pitch.
convert_pitch :: RealTime -> Score.ControlMap -> PitchSignal.Signal
    -> ConvertT Theory.Pitch
convert_pitch start controls psig = do
    when (PitchSignal.sig_scale_id psig /= Twelve.scale_id) $
        throw $ "scale must be " ++ Pretty.pretty Twelve.scale_id ++ ": " ++
            Pretty.pretty (PitchSignal.sig_scale_id psig)
    pitch <- require "pitch" $ PitchSignal.at start psig
    nn <- either (throw . ("convert_pitch: "++) . show) return $
        PitchSignal.pitch_nn $
            PitchSignal.apply (PitchSignal.controls_at start controls) pitch
    require "pitch in range" $
        Map.lookup (Pitch.Degree (floor nn)) degree_to_pitch

degree_to_pitch :: Map.Map Pitch.Degree Theory.Pitch
degree_to_pitch =
    Map.fromList $ Maybe.mapMaybe (Seq.minimum_on (simplicity . snd)) $
        Seq.group_on fst $ map reverse $ Map.elems $
        TwelveUtil.sys_note_to_degree Twelve.system
    where
    reverse (a, b) = (b, a)
    simplicity pitch = (accs < 0, abs accs)
        where accs = Theory.note_accidentals (Theory.pitch_note pitch)

real_to_time :: Lilypond.Duration -> RealTime -> Lilypond.Time
real_to_time (Lilypond.Duration dur1) real = Lilypond.Time $ floor $
    RealTime.to_seconds real * (fromIntegral whole / fromIntegral dur1)
    where Lilypond.Time whole = Lilypond.time_per_whole

--

-- | Convert a Warning into an appropriate log msg.
warn_to_log :: Warning.Warning -> Log.Msg
warn_to_log (Warning.Warning msg stack maybe_range) =
    Log.msg Log.Warn (Just (Stack.to_strings stack)) $
        "Convert: " ++ msg ++ maybe "" ((" range: " ++) . show) maybe_range
    -- TODO It would be more useful to append the range to the stack, but
    -- I would have to convert real -> score.

type ConvertT = Error.ErrorT (Maybe Warning.Warning)
    (State.StateT State
        (Logger.LoggerT Warning.Warning
            (Reader.ReaderT Stack.Stack Identity.Identity)))
type State = ()

run_convert :: State -> Stack.Stack -> ConvertT a
    -> (Maybe a, [Warning.Warning], State)
run_convert state stack conv =
    (either (const Nothing) Just val, warn ++ warns, out_state)
    where
    run = Identity.runIdentity . flip Reader.runReaderT stack
        . Logger.run . flip State.runStateT state . Error.runErrorT
    ((val, out_state), warns) = run conv
    warn = case val of
        Left (Just warn) -> [warn]
        _ -> []

throw :: String -> ConvertT a
throw msg = do
    stack <- Reader.ask
    Error.throwError $ Just $ Warning.warning msg stack Nothing

require :: String -> Maybe a -> ConvertT a
require msg = maybe (throw $ "event requires " ++ msg) return

warn :: String -> ConvertT ()
warn msg = do
    stack <- Reader.ask
    Logger.log (Warning.warning msg stack Nothing)

-- | Bogus mandatory instance.
instance Error.Error (Maybe Warning.Warning) where
    strMsg = Just . Error.strMsg
