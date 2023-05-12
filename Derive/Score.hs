-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveDataTypeable #-}
{- | This has Score.Event, which is the main output of the deriver.

    The events here are generated from UI Events, and will eventually be
    transformed into Perform Events, which are specific to the performance
    backend.
-}
module Derive.Score (
    -- * Event
    Event(..)
    , short_event, short_events
    , empty_event, event_end, event_min, event_max
    , events_overlap
    , event_scale_id
    , copy, normalize
    -- ** flags
    , has_flags, add_flags, remove_flags
    -- ** logs
    , add_log, add_log_msg
    -- ** environ
    , modify_environ, modify_val
    -- ** attributes
    , event_attributes, has_attribute, intersecting_attributes
    , modify_attributes, add_attributes, remove_attributes
    -- ** delayed args
    , put_arg, take_arg

    -- ** modify events
    , move, place, move_start, duration, set_duration, set_instrument
    -- *** control
    , event_controls
    , control_at, event_control, initial_dynamic, modify_dynamic, set_dynamic
    , modify_control_vals, modify_control, modify_signal
    , set_control, event_controls_at
    -- *** pitch
    , set_pitch
    , set_named_pitch, event_named_pitch
    , transposed_at, pitch_at, apply_controls
    , initial_pitch, nn_at, initial_nn, note_at, initial_note
    , nn_signal
) where
import qualified Control.DeepSeq as DeepSeq
import           Control.DeepSeq (rnf)
import qualified Data.Dynamic as Dynamic
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable

import qualified Util.CallStack as CallStack
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

data Event = Event {
    event_start :: !RealTime
    , event_duration :: !RealTime
    -- | This is the text of the call that created the event.  It's basically
    -- just for debugging.
    , event_text :: !Text
    -- | If the event is integrated back to a Ui.Event, use this text.  This is
    -- so calls can explicitly set how they would like their events to be
    -- integrated.  Otherwise, Integrate.Convert will try to infer something.
    , event_integrate :: !Text
    , event_pitch :: !PSignal.PSignal
    -- | Keep track of where this event originally came from.  That way, if an
    -- error or warning is emitted concerning this event, its position on the
    -- UI can be highlighted.
    , event_stack :: !Stack.Stack
    , event_highlight :: !Color.Highlight
    , event_instrument :: !ScoreT.Instrument
    , event_environ :: !DeriveT.Environ
    -- | Flags have their own field rather than being in 'event_environ', this
    -- emphasizes that they're meant to be used by calls and not from the
    -- score.
    , event_flags :: !Flags.Flags
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
    , event_delayed_args :: !(Map Text Dynamic.Dynamic)
    -- | Keep track of interesting things that have happened to this event.
    -- Postproc transforms that alter it should prefix a note.
    , event_logs :: ![Log.Msg]
    } deriving (Show, Typeable.Typeable)

-- | Format an event in a way suitable for including inline in log messages.
-- It's short, but hopefully enough information to identify the event in
-- question.
--
-- This is the derive equivalent to 'Cmd.Cmd.log_event'.
short_event :: Event -> Text
short_event e = Text.unwords $ concat
    [ [pretty (event_start e, event_duration e)]
    , ["\"" <> event_text e <> "\"" | event_text e /= ""]
    , ["'" <> event_integrate e <> "'" | event_integrate e /= ""]
    , [pretty (event_instrument e)]
    , [pretty n | Just n <- [initial_note e]]
    , [pretty attrs |  attrs /= mempty]
    , [stack | Just stack <- [Stack.pretty_ui_inner (event_stack e)]]
    ]
    where attrs = event_attributes e

short_events :: [Event] -> Text
short_events events = mconcat $
    "[" : List.intersperse ", " (map short_event events) ++ ["]"]

empty_event :: Event
empty_event = Event
    { event_start = 0
    , event_duration = 0
    , event_text = mempty
    , event_integrate = mempty
    , event_pitch = mempty
    , event_stack = Stack.empty
    , event_highlight = Color.NoHighlight
    , event_instrument = ScoreT.empty_instrument
    , event_environ = mempty
    , event_flags = mempty
    , event_delayed_args = mempty
    , event_logs = []
    }

event_end :: Event -> RealTime
event_end event = event_start event + event_duration event

-- | Get minimum and maximum edges of the event.  'event_start' isn't
-- necessarily the minimum because of negative durations.
event_min, event_max :: Event -> RealTime
event_min event = min (event_start event) (event_end event)
event_max event = max (event_start event) (event_end event)

events_overlap :: Event -> Event -> Bool
events_overlap e1 e2 =
    not $ event_end e1 <= event_end e2 || event_start e1 >= event_end e2

event_scale_id :: Event -> Pitch.ScaleId
event_scale_id = PSignal.sig_scale_id . event_pitch

-- | If you use an event to create another event, call this to clear out
-- data that shouldn't go with the copy.
copy :: Event -> Event
copy event = event { event_flags = mempty, event_logs = [] }

-- | Apply environ and controls to pitches.
--
-- Normally this is done by Convert, but if you want to see an event for
-- debugging it can be nicer to see the normalized version.
--
-- Unlike "Perform.Midi.Convert", this doesn't trim the controls, so it applies
-- out-of-range transpositions.
normalize :: Event -> Event
normalize event = event { event_pitch = apply $ event_pitch event }
    where
    apply = PSignal.apply_controls controls
        . PSignal.apply_environ (event_environ event)
    controls = event_controls event

-- ** flags

has_flags :: Flags.Flags -> Event -> Bool
has_flags flags = (`Flags.has` flags) . event_flags

add_flags :: Flags.Flags -> Event -> Event
add_flags flags event = event { event_flags = flags <> event_flags event }

remove_flags :: Flags.Flags -> Event -> Event
remove_flags flags event =
    event { event_flags = event_flags event Set.\\ flags }

-- ** logs

add_log :: CallStack.Stack => Text -> Event -> Event
add_log msg = add_log_msg (Log.msg Log.Debug Nothing msg)

add_log_msg :: Log.Msg -> Event -> Event
add_log_msg msg event = event { event_logs = msg : event_logs event }

-- ** environ

modify_environ :: (DeriveT.Environ -> DeriveT.Environ) -> Event -> Event
modify_environ f event = event { event_environ = f (event_environ event) }

-- | Modify the value at the given key.
modify_val :: EnvKey.Key -> (Maybe DeriveT.Val -> DeriveT.Val) -> Event -> Event
modify_val key modify = modify_environ $ \(DeriveT.Environ env) ->
    DeriveT.Environ $ Map.alter (Just . modify) key env

put_val :: EnvKey.Key -> DeriveT.Val -> Event -> Event
put_val key = modify_val key . const

lookup_val :: EnvKey.Key -> Event -> Maybe DeriveT.Val
lookup_val key = DeriveT.lookup key . event_environ

-- ** attributes

event_attributes :: Event -> Attrs.Attributes
event_attributes = DeriveT.environ_attributes . event_environ

has_attribute :: Attrs.Attributes -> Event -> Bool
has_attribute attr = (`Attrs.contain` attr) . event_attributes

intersecting_attributes :: Attrs.Attributes -> Event -> Bool
intersecting_attributes attrs event =
    Attrs.intersection attrs (event_attributes event) /= mempty

modify_attributes :: (Attrs.Attributes -> Attrs.Attributes) -> Event -> Event
modify_attributes modify = modify_environ $ \env ->
    DeriveT.insert EnvKey.attributes
        (DeriveT.VAttributes (modify (DeriveT.environ_attributes env))) env

add_attributes :: Attrs.Attributes -> Event -> Event
add_attributes attrs
    | attrs == mempty = id
    | otherwise = modify_attributes (<>attrs)

remove_attributes :: Attrs.Attributes -> Event -> Event
remove_attributes attrs event
    | attrs == mempty || not (has_attribute attrs event) = event
    | otherwise = modify_attributes (Attrs.remove attrs) event

instance DeepSeq.NFData Event where
    rnf (Event start dur _text _integrate pitch _ _ _ _ flags
            _delayed_args logs) =
        -- I can't force Dynamic, so leave off _delayed_args.
        rnf (start, dur, pitch, flags, logs)

instance Pretty Event where
    format e@(Event start dur text integrate pitch stack highlight inst env
            flags delayed_args logs) =
        Pretty.record (foldr1 (Pretty.<+>) $ concat
            [ ["Event", Pretty.format (start, dur)]
            , [Pretty.text $ "\"" <> text <> "\"" | text /= ""]
            , [Pretty.text $ "'" <> integrate <> "'" | integrate /= ""]
            , [Pretty.format n | Just n <- [initial_note e]]
            , [ Pretty.format attrs
              | let attrs = DeriveT.environ_attributes env, attrs /= mempty
              ]
            ]) $ concat
            [ [("instrument", Pretty.format inst)]
            , g "pitch" pitch PSignal.null
            , g "stack" stack (== Stack.empty)
            , g "highlight" highlight (== Color.NoHighlight)
            , g "environ" env DeriveT.null
            , g "flags" flags Set.null
            , g "delayed_args" delayed_args Map.null
            , g "logs" logs null
            ]
        where
        g name val empty = [(name, Pretty.format val) | not (empty val)]

-- ** delayed args

put_arg :: Typeable.Typeable a => Text -> a -> Event -> Event
put_arg key arg event = event
    { event_delayed_args = Map.insert key (Dynamic.toDyn arg)
        (event_delayed_args event)
    }

-- | Find an arg in 'event_delayed_args', and remove it from the event if it
-- existed.  Throw an error if it existed but had an unexpected type.
take_arg :: Typeable.Typeable a => Text -> Event
    -> Either Text (Event, Maybe a)
take_arg key event = case Map.lookup key (event_delayed_args event) of
    Nothing -> Right (event, Nothing)
    Just arg -> case Dynamic.fromDynamic arg of
        Nothing -> Left $ "incorrect delayed arg type for " <> showt key
            <> ": " <> pretty arg
        Just a -> Right (delete_arg key event, Just a)

delete_arg :: Text -> Event -> Event
delete_arg key event =
    event { event_delayed_args = Map.delete key (event_delayed_args event) }

-- ** modify events

-- These operate directly on events, so we are in RealTime at this point.

-- | Change the start time of an event and move its controls along with it.
move :: (RealTime -> RealTime) -> Event -> Event
move modify event
    | pos == event_start event = event
    | otherwise = event
        { event_start = pos
        , event_environ = modify_env (map (second shift))
            (event_environ event)
        , event_pitch = PSignal.shift delta $ event_pitch event
        }
    where
    shift = \case
        DeriveT.VSignal sig -> DeriveT.VSignal $ Signal.shift delta <$> sig
        DeriveT.VPSignal sig -> DeriveT.VPSignal $ PSignal.shift delta sig
        val -> val
    pos = modify (event_start event)
    delta = pos - event_start event

modify_env :: ([(EnvKey.Key, DeriveT.Val)] -> [(EnvKey.Key, DeriveT.Val)])
    -> DeriveT.Environ -> DeriveT.Environ
modify_env modify (DeriveT.Environ env) = DeriveT.Environ $
    Map.fromAscList $ modify $ Map.toAscList env

place :: RealTime -> RealTime -> Event -> Event
place start dur event = (move (const start) event) { event_duration = dur }

move_start :: RealTime -> RealTime -> Event -> Event
move_start min_duration offset
    | offset == 0 = id
    | otherwise = duration (max min_duration . subtract offset) . move (+offset)

duration :: (RealTime -> RealTime) -> Event -> Event
duration modify event
    | dur == event_duration event = event
    | otherwise = event { event_duration = dur }
    where dur = modify (event_duration event)

set_duration :: RealTime -> Event -> Event
set_duration = duration . const

-- | Set the instrument on an event, and also update its environ from the
-- instrument.  You should really rederive with the new instrument, but this
-- way can be more convenient, if somewhat sketchy.
set_instrument :: ScoreT.Instrument -> DeriveT.Environ -> Event -> Event
set_instrument score_inst inst_environ event = event
    { event_instrument = score_inst
    , event_environ = inst_environ <> event_environ event
    }

-- *** control

event_controls :: Event -> ScoreT.ControlMap
event_controls = get . event_environ
    where
    get (DeriveT.Environ env) = Map.fromAscList $ map (first ScoreT.Control) $
        Lists.mapMaybeSnd is_signal $ Map.toAscList env
    is_signal (DeriveT.VSignal sig) = Just sig
    is_signal _ = Nothing

-- | Get a control value from the event, or Nothing if that control isn't
-- present.
control_at :: RealTime -> ScoreT.Control -> Event
    -> Maybe (ScoreT.Typed Signal.Y)
control_at pos control = fmap (fmap (`Signal.at` pos)) . event_control control

event_control :: ScoreT.Control -> Event -> Maybe (ScoreT.Typed Signal.Control)
event_control (ScoreT.Control control) = as_signal <=< lookup_val control

initial_dynamic :: Event -> Signal.Y
initial_dynamic event = maybe 0 ScoreT.val_of $
     -- Derive.initial_controls should mean this is never Nothing.
    control_at (event_start event) Controls.dynamic event

-- | Use this instead of 'modify_control_vals' because it also sets
-- 'EnvKey.dynamic_val'.  This is only valid for linear functions like (+) or
-- (*).
modify_dynamic :: (Signal.Y -> Signal.Y) -> Event -> Event
modify_dynamic modify =
    modify_control_vals (ScoreT.Control EnvKey.dynamic_val) modify
    . modify_control_vals Controls.dynamic modify

-- | Use this instead of 'set_control' because it also sets
-- 'EnvKey.dynamic_val'.
set_dynamic :: Signal.Y -> Event -> Event
set_dynamic dyn = put_val EnvKey.dynamic_val sig
    . put_val (ScoreT.control_name Controls.dynamic) sig
    where sig = DeriveT.num dyn

modify_control_vals :: ScoreT.Control -> (Signal.Y -> Signal.Y) -> Event
    -> Event
modify_control_vals control modify event = case event_control control event of
    Nothing -> event
    Just sig -> put_val (ScoreT.control_name control)
        (DeriveT.VSignal (Signal.map_y_linear modify <$> sig)) event

-- | Like 'modify_control', but default to an empty control and retain any
-- type the original had.
modify_signal :: ScoreT.Control -> (Signal.Control -> Signal.Control)
    -> Event -> Event
modify_signal control modify =
    modify_control control (fmap modify . fromMaybe (ScoreT.untyped mempty))

modify_control :: ScoreT.Control
    -> (Maybe ScoreT.TypedSignal -> ScoreT.TypedSignal) -> Event -> Event
modify_control (ScoreT.Control control) modify =
    modify_val control (DeriveT.VSignal . modify . (as_signal =<<))

as_signal :: DeriveT.Val -> Maybe ScoreT.TypedSignal
as_signal = \case
    DeriveT.VSignal sig -> Just sig
    _ -> Nothing

set_control :: ScoreT.Control -> ScoreT.Typed Signal.Control -> Event -> Event
set_control (ScoreT.Control control) =
    modify_val control . const . DeriveT.VSignal

event_controls_at :: RealTime -> Event -> ScoreT.ControlValMap
event_controls_at t event =
    ScoreT.val_of . fmap (`Signal.at` t) <$> event_controls event

-- *** pitch

set_pitch :: PSignal.PSignal -> Event -> Event
set_pitch sig event = event { event_pitch = sig }

set_named_pitch :: ScoreT.PControl -> PSignal.PSignal -> Event -> Event
set_named_pitch pcontrol signal event
    | pcontrol == ScoreT.default_pitch = event { event_pitch = signal }
    | otherwise = modify_val (ScoreT.pcontrol_name pcontrol)
        (const $ DeriveT.VPSignal signal) event

event_named_pitch :: ScoreT.PControl -> Event -> Maybe PSignal.PSignal
event_named_pitch pcontrol event
    | pcontrol == ScoreT.default_pitch = Just (event_pitch event)
    | otherwise = case lookup_val (ScoreT.pcontrol_name pcontrol) event of
        Just (DeriveT.VPSignal sig) -> Just sig
        _ -> Nothing

-- | Unlike 'Derive.Derive.pitch_at', the transposition has already been
-- applied.  This is because callers expect to get the actual pitch, not the
-- pitch plus some homework to do on the pitch.  If you use this pitch to emit
-- another pitch you proabbly need the raw pitch, but so far everyone doing
-- that is at the Derive level, not postproc, so they use Derive.pitch_at.
{-# SCC transposed_at #-}
transposed_at :: RealTime -> Event -> Maybe PSignal.Transposed
transposed_at pos event = PSignal.apply_config config <$> pitch_at pos event
    where
    config = PSignal.PitchConfig (event_environ event)
        (event_controls_at pos event)

pitch_at :: RealTime -> Event -> Maybe PSignal.Pitch
pitch_at pos event = PSignal.at (event_pitch event) pos

apply_controls :: Event -> RealTime -> PSignal.Pitch -> PSignal.Transposed
apply_controls event pos = PSignal.apply (event_controls_at pos event)

initial_pitch :: Event -> Maybe PSignal.Transposed
initial_pitch event = transposed_at (event_start event) event

nn_at :: RealTime -> Event -> Maybe Pitch.NoteNumber
nn_at pos event = either (const Nothing) Just . PSignal.pitch_nn
    =<< transposed_at pos event

{-# SCC initial_nn #-}
initial_nn :: Event -> Maybe Pitch.NoteNumber
initial_nn event = nn_at (event_start event) event

note_at :: RealTime -> Event -> Maybe Pitch.Note
note_at pos event = either (const Nothing) Just . PSignal.pitch_note
    =<< transposed_at pos event

initial_note :: Event -> Maybe Pitch.Note
initial_note event = note_at (event_start event) event

{-# SCC nn_signal #-}
nn_signal :: Event -> (Signal.NoteNumber, [(RealTime, Text)])
nn_signal event =
    PSignal.to_nn $ PSignal.apply_controls (event_controls event) $
        PSignal.apply_environ (event_environ event) $
        event_pitch event
