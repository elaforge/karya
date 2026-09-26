-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE StrictData #-}
{- | This has Score.Event, which is the main output of the deriver.

    The events here are generated from UI Events, and will eventually be
    transformed into Perform Events, which are specific to the performance
    backend.
-}
module Derive.Score (
    -- * Event
    Event(..)
    , shortEvent, shortEvents
    , emptyEvent, eventEnd, eventMin, eventMax
    , eventsOverlap
    , eventScaleId
    , copy, normalize
    -- ** flags
    , hasFlags, addFlags, removeFlags
    -- ** logs
    , addLog, addLogMsg
    -- ** environ
    , modifyEnviron, modifyVal
    -- ** attributes
    , eventAttributes, hasAttribute, intersectingAttributes
    , modifyAttributes, addAttributes, removeAttributes
    -- ** delayed args
    , putArg, takeArg

    -- ** modify events
    , move, place, moveStart, duration, setDuration, setInstrument
    -- *** control
    , eventControls
    , controlAt, eventControl, initialDynamic, modifyDynamic, setDynamic
    , modifyControlVals, modifyControl, modifySignal
    , setControl, eventControlsAt
    -- *** pitch
    , setPitch
    , setNamedPitch, eventNamedPitch
    , transposedAt, pitchAt, applyControls
    , initialPitch, nnAt, initialNn, noteAt, initialNote
    , nnSignal
) where
import qualified Control.DeepSeq as DeepSeq
import           Control.DeepSeq (rnf)
import qualified Data.Dynamic as Dynamic
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable

import           GHC.Stack (HasCallStack)

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty

import qualified Derive.Attrs as Attrs
import qualified Derive.Controls as Controls
import qualified Derive.DeriveT as DeriveT
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Flags as Flags
import qualified Derive.PSignal as PSignal
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Ui.Color as Color

import           Global
import           Types


-- * Event

data Event = Event
    { eventStart :: RealTime
    , eventDuration :: RealTime
    -- | This is the text of the call that created the event.  It's basically
    -- just for debugging.
    , eventText :: Text
    -- | If the event is integrated back to a Ui.Event, use this text.  This is
    -- so calls can explicitly set how they would like their events to be
    -- integrated.  Otherwise, Integrate.Convert will try to infer something.
    , eventIntegrate :: Text
    , eventPitch :: PSignal.PSignal
    -- | Keep track of where this event originally came from.  That way, if an
    -- error or warning is emitted concerning this event, its position on the
    -- UI can be highlighted.
    , eventStack :: Stack.Stack
    , eventHighlight :: Color.Highlight
    , eventInstrument :: ScoreT.Instrument
    , eventEnviron :: DeriveT.Environ
    -- | Flags have their own field rather than being in 'event_environ', this
    -- emphasizes that they're meant to be used by calls and not from the
    -- score.
    , eventFlags :: Flags.Flags
    -- | This has arguments passed from a call that applies an attribute to one
    -- which is meant to later realize the attribute.  This happens when a call
    -- needs to be configured at the track level, but also needs some
    -- information only available later, such as the real start time or pitch
    -- of the next note.  They are indexed by attribute because there may be
    -- multiple delayed calls on a single note, and the realize postproc may
    -- want to ignore some, e.g. if they are overidden by another attribute.
    --
    -- I couldn't think of a type safe way to do this, but Dynamic should be
    -- safe enough if you use a shared type declaration in both writer and
    -- reader.
    , eventDelayedArgs :: Map Text Dynamic.Dynamic
    -- | Keep track of interesting things that have happened to this event.
    -- Postproc transforms that alter it should prefix a note.
    , eventLogs :: [Log.Msg]
    } deriving (Show)

-- | Format an event in a way suitable for including inline in log messages.
-- It's short, but hopefully enough information to identify the event in
-- question.
--
-- This is the derive equivalent to 'Cmd.Cmd.log_event'.
shortEvent :: Event -> Text
shortEvent e = Text.unwords $ concat
    [ [pretty (eventStart e, eventDuration e)]
    , ["\"" <> eventText e <> "\"" | eventText e /= ""]
    , ["'" <> eventIntegrate e <> "'" | eventIntegrate e /= ""]
    , [pretty (eventInstrument e)]
    , [pretty n | Just n <- [initialNote e]]
    , [pretty attrs |  attrs /= mempty]
    , [stack | Just stack <- [Stack.prettyUiInner (eventStack e)]]
    ]
    where attrs = eventAttributes e

shortEvents :: [Event] -> Text
shortEvents events = mconcat $
    "[" : List.intersperse ", " (map shortEvent events) ++ ["]"]

emptyEvent :: Event
emptyEvent = Event
    { eventStart = 0
    , eventDuration = 0
    , eventText = mempty
    , eventIntegrate = mempty
    , eventPitch = mempty
    , eventStack = Stack.empty
    , eventHighlight = Color.NoHighlight
    , eventInstrument = ScoreT.emptyInstrument
    , eventEnviron = mempty
    , eventFlags = mempty
    , eventDelayedArgs = mempty
    , eventLogs = []
    }

eventEnd :: Event -> RealTime
eventEnd event = eventStart event + eventDuration event

-- | Get minimum and maximum edges of the event.  'event_start' isn't
-- necessarily the minimum because of negative durations.
eventMin, eventMax :: Event -> RealTime
eventMin event = min (eventStart event) (eventEnd event)
eventMax event = max (eventStart event) (eventEnd event)

eventsOverlap :: Event -> Event -> Bool
eventsOverlap e1 e2 =
    not $ eventEnd e1 <= eventEnd e2 || eventStart e1 >= eventEnd e2

eventScaleId :: Event -> Pitch.ScaleId
eventScaleId = PSignal.sigScaleId . eventPitch

-- | If you use an event to create another event, call this to clear out
-- data that shouldn't go with the copy.
copy :: Event -> Event
copy event = event { eventFlags = mempty, eventLogs = [] }

-- | Apply environ and controls to pitches.
--
-- Normally this is done by Convert, but if you want to see an event for
-- debugging it can be nicer to see the normalized version.
--
-- Unlike "Perform.Midi.Convert", this doesn't trim the controls, so it applies
-- out-of-range transpositions.
normalize :: Event -> Event
normalize event = event { eventPitch = apply $ eventPitch event }
    where
    apply = PSignal.applyControls controls
        . PSignal.applyEnviron (eventEnviron event)
    controls = eventControls event

-- ** flags

hasFlags :: Flags.Flags -> Event -> Bool
hasFlags flags = (`Flags.has` flags) . eventFlags

addFlags :: Flags.Flags -> Event -> Event
addFlags flags event = event { eventFlags = flags <> eventFlags event }

removeFlags :: Flags.Flags -> Event -> Event
removeFlags flags event =
    event { eventFlags = eventFlags event Set.\\ flags }

-- ** logs

addLog :: HasCallStack => Text -> Event -> Event
addLog msg = addLogMsg (Log.msg Log.Debug Nothing msg)

addLogMsg :: Log.Msg -> Event -> Event
addLogMsg msg event = event { eventLogs = msg : eventLogs event }

-- ** environ

modifyEnviron :: (DeriveT.Environ -> DeriveT.Environ) -> Event -> Event
modifyEnviron f event = event { eventEnviron = f (eventEnviron event) }

-- | Modify the value at the given key.
modifyVal :: EnvKey.Key -> (Maybe DeriveT.Val -> DeriveT.Val) -> Event -> Event
modifyVal key modify = modifyEnviron $ \(DeriveT.Environ env) ->
    DeriveT.Environ $ Map.alter (Just . modify) key env

putVal :: EnvKey.Key -> DeriveT.Val -> Event -> Event
putVal key = modifyVal key . const

lookupVal :: EnvKey.Key -> Event -> Maybe DeriveT.Val
lookupVal key = DeriveT.lookup key . eventEnviron

-- ** attributes

eventAttributes :: Event -> Attrs.Attributes
eventAttributes = DeriveT.environAttributes . eventEnviron

hasAttribute :: Attrs.Attributes -> Event -> Bool
hasAttribute attr = (`Attrs.contain` attr) . eventAttributes

intersectingAttributes :: Attrs.Attributes -> Event -> Bool
intersectingAttributes attrs event =
    Attrs.intersection attrs (eventAttributes event) /= mempty

modifyAttributes :: (Attrs.Attributes -> Attrs.Attributes) -> Event -> Event
modifyAttributes modify = modifyEnviron $ \env ->
    DeriveT.insert EnvKey.attributes
        (DeriveT.VAttributes (modify (DeriveT.environAttributes env))) env

addAttributes :: Attrs.Attributes -> Event -> Event
addAttributes attrs
    | attrs == mempty = id
    | otherwise = modifyAttributes (<>attrs)

removeAttributes :: Attrs.Attributes -> Event -> Event
removeAttributes attrs event
    | attrs == mempty || not (hasAttribute attrs event) = event
    | otherwise = modifyAttributes (Attrs.remove attrs) event

instance DeepSeq.NFData Event where
    rnf (Event start dur _text _integrate pitch _ _ _ _ flags
            _delayedArgs logs) =
        -- I can't force Dynamic, so leave off _delayed_args.
        rnf (start, dur, pitch, flags, logs)

instance Pretty Event where
    format e@(Event start dur text integrate pitch stack highlight inst env
            flags delayedArgs logs) =
        Pretty.record (foldr1 (Pretty.<+>) $ concat
            [ ["Event", Pretty.format (start, dur)]
            , [Pretty.text $ "\"" <> text <> "\"" | text /= ""]
            , [Pretty.text $ "'" <> integrate <> "'" | integrate /= ""]
            , [Pretty.format n | Just n <- [initialNote e]]
            , [ Pretty.format attrs
              | let attrs = DeriveT.environAttributes env, attrs /= mempty
              ]
            ]) $ concat
            [ [("instrument", Pretty.format inst)]
            , g "pitch" pitch PSignal.null
            , g "stack" stack (== Stack.empty)
            , g "highlight" highlight (== Color.NoHighlight)
            , g "environ" env DeriveT.null
            , g "flags" flags Set.null
            , g "delayed_args" delayedArgs Map.null
            , g "logs" logs null
            ]
        where
        g name val empty = [(name, Pretty.format val) | not (empty val)]

-- ** delayed args

putArg :: Typeable.Typeable a => Text -> a -> Event -> Event
putArg key arg event = event
    { eventDelayedArgs = Map.insert key (Dynamic.toDyn arg)
        (eventDelayedArgs event)
    }

-- | Find an arg in 'event_delayed_args', and remove it from the event if it
-- existed.  Throw an error if it existed but had an unexpected type.
takeArg :: Typeable.Typeable a => Text -> Event
    -> Either Text (Event, Maybe a)
takeArg key event = case Map.lookup key (eventDelayedArgs event) of
    Nothing -> Right (event, Nothing)
    Just arg -> case Dynamic.fromDynamic arg of
        Nothing -> Left $ "incorrect delayed arg type for " <> showt key
            <> ": " <> pretty arg
        Just a -> Right (deleteArg key event, Just a)

deleteArg :: Text -> Event -> Event
deleteArg key event =
    event { eventDelayedArgs = Map.delete key (eventDelayedArgs event) }

-- ** modify events

-- These operate directly on events, so we are in RealTime at this point.

-- | Change the start time of an event and move its controls along with it.
move :: (RealTime -> RealTime) -> Event -> Event
move modify event
    | pos == eventStart event = event
    | otherwise = event
        { eventStart = pos
        , eventEnviron = modifyEnv (map (second shift))
            (eventEnviron event)
        , eventPitch = PSignal.shift delta $ eventPitch event
        }
    where
    shift = \case
        DeriveT.VSignal sig -> DeriveT.VSignal $ Signal.shift delta <$> sig
        DeriveT.VPSignal sig -> DeriveT.VPSignal $ PSignal.shift delta sig
        val -> val
    pos = modify (eventStart event)
    delta = pos - eventStart event

modifyEnv :: ([(EnvKey.Key, DeriveT.Val)] -> [(EnvKey.Key, DeriveT.Val)])
    -> DeriveT.Environ -> DeriveT.Environ
modifyEnv modify (DeriveT.Environ env) = DeriveT.Environ $
    Map.fromAscList $ modify $ Map.toAscList env

place :: RealTime -> RealTime -> Event -> Event
place start dur event = (move (const start) event) { eventDuration = dur }

moveStart :: RealTime -> RealTime -> Event -> Event
moveStart minDuration offset
    | offset == 0 = id
    | otherwise = duration (max minDuration . subtract offset) . move (+offset)

duration :: (RealTime -> RealTime) -> Event -> Event
duration modify event
    | dur == eventDuration event = event
    | otherwise = event { eventDuration = dur }
    where dur = modify (eventDuration event)

setDuration :: RealTime -> Event -> Event
setDuration = duration . const

-- | Set the instrument on an event, and also update its environ from the
-- instrument.  You should really rederive with the new instrument, but this
-- way can be more convenient, if somewhat sketchy.
setInstrument :: ScoreT.Instrument -> DeriveT.Environ -> Event -> Event
setInstrument scoreInst instEnviron event = event
    { eventInstrument = scoreInst
    , eventEnviron = instEnviron <> eventEnviron event
    }

-- *** control

eventControls :: Event -> ScoreT.ControlMap
eventControls = get . eventEnviron
    where
    get (DeriveT.Environ env) = Map.fromAscList $ map (first ScoreT.Control) $
        Lists.mapMaybeSnd isSignal $ Map.toAscList env
    isSignal (DeriveT.VSignal sig) = Just sig
    isSignal _ = Nothing

-- | Get a control value from the event, or Nothing if that control isn't
-- present.
controlAt :: RealTime -> ScoreT.Control -> Event
    -> Maybe (ScoreT.Typed Signal.Y)
controlAt pos control = fmap (fmap (`Signal.at` pos)) . eventControl control

eventControl :: ScoreT.Control -> Event -> Maybe (ScoreT.Typed Signal.Control)
eventControl (ScoreT.Control control) = asSignal <=< lookupVal control

initialDynamic :: Event -> Signal.Y
initialDynamic event = maybe 0 ScoreT.valOf $
     -- Derive.initial_controls should mean this is never Nothing.
    controlAt (eventStart event) Controls.dynamic event

-- | Use this instead of 'modify_control_vals' because it also sets
-- 'EnvKey.dynamic_val'.  This is only valid for linear functions like (+) or
-- (*).
modifyDynamic :: (Signal.Y -> Signal.Y) -> Event -> Event
modifyDynamic modify =
    modifyControlVals (ScoreT.Control EnvKey.dynamicVal) modify
    . modifyControlVals Controls.dynamic modify

-- | Use this instead of 'set_control' because it also sets
-- 'EnvKey.dynamic_val'.
setDynamic :: Signal.Y -> Event -> Event
setDynamic dyn = putVal EnvKey.dynamicVal sig
    . putVal (ScoreT.controlName Controls.dynamic) sig
    where sig = DeriveT.num dyn

modifyControlVals :: ScoreT.Control -> (Signal.Y -> Signal.Y) -> Event
    -> Event
modifyControlVals control modify event = case eventControl control event of
    Nothing -> event
    Just sig -> putVal (ScoreT.controlName control)
        (DeriveT.VSignal (Signal.mapYLinear modify <$> sig)) event

-- | Like 'modify_control', but default to an empty control and retain any
-- type the original had.
modifySignal :: ScoreT.Control -> (Signal.Control -> Signal.Control)
    -> Event -> Event
modifySignal control modify =
    modifyControl control (fmap modify . fromMaybe (ScoreT.untyped mempty))

modifyControl :: ScoreT.Control
    -> (Maybe ScoreT.TypedSignal -> ScoreT.TypedSignal) -> Event -> Event
modifyControl (ScoreT.Control control) modify =
    modifyVal control (DeriveT.VSignal . modify . (asSignal =<<))

asSignal :: DeriveT.Val -> Maybe ScoreT.TypedSignal
asSignal = \case
    DeriveT.VSignal sig -> Just sig
    _ -> Nothing

setControl :: ScoreT.Control -> ScoreT.Typed Signal.Control -> Event -> Event
setControl (ScoreT.Control control) =
    modifyVal control . const . DeriveT.VSignal

eventControlsAt :: RealTime -> Event -> ScoreT.ControlValMap
eventControlsAt t event =
    ScoreT.valOf . fmap (`Signal.at` t) <$> eventControls event

-- *** pitch

setPitch :: PSignal.PSignal -> Event -> Event
setPitch sig event = event { eventPitch = sig }

setNamedPitch :: ScoreT.PControl -> PSignal.PSignal -> Event -> Event
setNamedPitch pcontrol signal event
    | pcontrol == ScoreT.defaultPitch = event { eventPitch = signal }
    | otherwise = modifyVal (ScoreT.pcontrolName pcontrol)
        (const $ DeriveT.VPSignal signal) event

eventNamedPitch :: ScoreT.PControl -> Event -> Maybe PSignal.PSignal
eventNamedPitch pcontrol event
    | pcontrol == ScoreT.defaultPitch = Just (eventPitch event)
    | otherwise = case lookupVal (ScoreT.pcontrolName pcontrol) event of
        Just (DeriveT.VPSignal sig) -> Just sig
        _ -> Nothing

-- | Unlike 'Derive.Derive.pitch_at', the transposition has already been
-- applied.  This is because callers expect to get the actual pitch, not the
-- pitch plus some homework to do on the pitch.  If you use this pitch to emit
-- another pitch you proabbly need the raw pitch, but so far everyone doing
-- that is at the Derive level, not postproc, so they use Derive.pitch_at.
{-# SCC transposedAt #-}
transposedAt :: RealTime -> Event -> Maybe PSignal.Transposed
transposedAt pos event = PSignal.applyConfig config <$> pitchAt pos event
    where
    config = PSignal.PitchConfig (eventEnviron event)
        (eventControlsAt pos event)

pitchAt :: RealTime -> Event -> Maybe PSignal.Pitch
pitchAt pos event = PSignal.at (eventPitch event) pos

applyControls :: Event -> RealTime -> PSignal.Pitch -> PSignal.Transposed
applyControls event pos = PSignal.apply (eventControlsAt pos event)

initialPitch :: Event -> Maybe PSignal.Transposed
initialPitch event = transposedAt (eventStart event) event

nnAt :: RealTime -> Event -> Maybe Pitch.NoteNumber
nnAt pos event = either (const Nothing) Just . PSignal.pitchNn
    =<< transposedAt pos event

{-# SCC initialNn #-}
initialNn :: Event -> Maybe Pitch.NoteNumber
initialNn event = nnAt (eventStart event) event

noteAt :: RealTime -> Event -> Maybe Pitch.Note
noteAt pos event = either (const Nothing) Just . PSignal.pitchNote
    =<< transposedAt pos event

initialNote :: Event -> Maybe Pitch.Note
initialNote event = noteAt (eventStart event) event

{-# SCC nnSignal #-}
nnSignal :: Event -> (Signal.NoteNumber, [(RealTime, Text)])
nnSignal event =
    PSignal.toNn $ PSignal.applyControls (eventControls event) $
        PSignal.applyEnviron (eventEnviron event) $
        eventPitch event
