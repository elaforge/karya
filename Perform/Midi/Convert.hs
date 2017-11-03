-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Convert from the Derive events to MIDI performer specific events.

    Since this module depends on both the Derive and Perform.Midi layers, it
    should be called from Derive or Cmd, not Perform.Midi, even though it's
    physically located in Perform.Midi.
-}
module Perform.Midi.Convert where
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Util.TextUtil as TextUtil
import qualified Util.TimeVector as TimeVector

import qualified Midi.Midi as Midi
import qualified Cmd.Cmd as Cmd
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score

import qualified Perform.ConvertUtil as ConvertUtil
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.Types as Types
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import qualified Instrument.Common as Common
import Global


data Lookup = Lookup {
    lookup_scale :: Derive.LookupScale
    , lookup_control_defaults :: Score.Instrument -> Score.ControlMap
    }

-- | Convert Score events to Perform events, emitting warnings that may have
-- happened along the way.
convert :: Lookup -> (Score.Instrument -> Maybe Cmd.ResolvedInstrument)
    -> [Score.Event] -> [LEvent.LEvent Types.Event]
convert lookup = ConvertUtil.convert $ \event resolved ->
    case Cmd.inst_backend resolved of
        Just (Cmd.Midi patch config) -> convert_event lookup event patch config
        _ -> []

convert_event :: Lookup -> Score.Event -> Patch.Patch -> Patch.Config
    -> [LEvent.LEvent Types.Event]
convert_event lookup event patch config = run $ do
    let inst = Score.event_instrument event
    let event_controls = Score.event_transformed_controls event
    (perf_patch, pitch) <-
        convert_midi_pitch inst patch config event_controls event
    let controls = convert_controls (Types.patch_control_map perf_patch) $
            convert_dynamic pressure
                (event_controls <> lookup_control_defaults lookup inst)
        pressure = Patch.has_flag config Patch.Pressure
        velocity = fromMaybe Perform.default_velocity $
            Env.maybe_val EnvKey.attack_val env
            <|> Env.maybe_val EnvKey.dynamic_val env
        release_velocity = fromMaybe velocity $
            Env.maybe_val EnvKey.release_val env
        env = Score.event_environ event
    return $ Types.Event
        { event_start = Score.event_start event
        , event_duration = Score.event_duration event
        , event_patch = perf_patch
        , event_controls = controls
        , event_pitch = pitch
        -- If it's a pressure instrument, then I'm using breath instead
        -- of velocity.  I still set velocity because some synths (e.g.
        -- vsl) use the velocity too in certain cases, but it should be
        -- at least 1 to avoid not even starting the note.  Of course
        -- even without pressure, a note on with velocity 0 is kind of
        -- pointless, but maybe someone wants to fade out.
        , event_start_velocity = if pressure then max 0.008 velocity
            else velocity
        , event_end_velocity = release_velocity
        , event_stack = Score.event_stack event
        }

run :: Log.LogT Identity.Identity a -> [LEvent.LEvent a]
run = merge . Identity.runIdentity . Log.run
    where merge (note, logs) = LEvent.Event note : map LEvent.Log logs

-- | If the Event has an attribute matching its keymap, use the pitch from the
-- keymap.  Otherwise convert the pitch signal.
--
-- TODO this used to warn about unmatched attributes, but it got annoying
-- because I use attributes freely.  It still seems like it could be useful,
-- so maybe I want to put it back in again someday.
convert_midi_pitch :: Log.LogMonad m => Score.Instrument -> Patch.Patch
    -> Patch.Config -> Score.ControlMap -> Score.Event
    -> m (Types.Patch, Signal.NoteNumber)
convert_midi_pitch inst patch config controls event =
    case Common.lookup_attributes (Score.event_attributes event) attr_map of
        Nothing -> (perf_patch,) . round_sig <$> get_signal
        Just (_, (keyswitches, maybe_keymap)) -> do
            sig <- maybe get_signal set_keymap maybe_keymap
            return (set_keyswitches keyswitches, round_sig sig)
    where
    set_keyswitches [] = perf_patch
    set_keyswitches keyswitches =
        perf_patch { Types.patch_keyswitch = keyswitches }

    -- A PitchedKeymap is mapped through the Patch.Scale.
    set_keymap (Patch.PitchedKeymap low high low_pitch) =
        convert_pitched_keymap (Midi.from_key low) (Midi.from_key high)
            low_pitch <$> get_signal
    -- But UnpitchedKeymap is a constant.
    set_keymap (Patch.UnpitchedKeymap key) =
        return $ Signal.constant (Midi.from_key key)
    get_signal = apply_patch_scale scale
        =<< convert_event_pitch perf_patch constant_pitch controls event
    scale = Patch.config_scale (Patch.config_settings config)
    round_sig = Signal.map_y round_pitch

    perf_patch = Types.patch inst config patch
    attr_map = Patch.patch_attribute_map patch
    constant_pitch = Patch.has_flag config Patch.ConstantPitch

convert_pitched_keymap :: Signal.Y -> Signal.Y -> Midi.Key
    -> Signal.NoteNumber -> Signal.NoteNumber
convert_pitched_keymap low high low_pitch sig = clipped
    where
    -- TODO warn about out_of_range
    (clipped, out_of_range) = Signal.clip_bounds low high $
        Signal.scalar_add (low - Midi.from_key low_pitch) sig

-- | Get the flattened Signal.NoteNumber from an event.
convert_event_pitch :: Log.LogMonad m => Types.Patch -> Bool -> Score.ControlMap
    -> Score.Event -> m Signal.NoteNumber
convert_event_pitch patch constant_pitch controls event
    | constant_pitch = convert constant_vals $ maybe mempty PSignal.constant $
        Score.pitch_at (Score.event_start event) event
    | otherwise = convert trimmed_vals $ Score.event_transformed_pitch event
    where
    convert = convert_pitch (Score.event_environ event)
    -- An optimization to avoid fiddling with controls unnecessarily.
    constant_vals = Score.untyped . Signal.constant <$>
        Score.event_controls_at (Score.event_start event) event
    -- Trim controls to avoid applying out of range transpositions.
    -- TODO should I also trim the pitch signal to avoid doing extra work?
    trimmed_vals = fmap (fmap (Signal.drop_at_after note_end)) controls
    note_end = Score.event_end event
        + fromMaybe Types.default_decay (Types.patch_decay patch)

-- | Convert deriver controls to performance controls.  Drop all non-MIDI
-- controls, since those will inhibit channel sharing later.
convert_controls :: Control.ControlMap -- ^ Instrument's control map.
    -> Score.ControlMap -- ^ Controls to convert.
    -> Types.ControlMap
convert_controls inst_cmap =
    Map.fromAscList . map (second Score.typed_val) . Map.toAscList
        . Map.filterWithKey (\k _ -> Control.is_midi_control inst_cmap k)

-- | If it's a 'Patch.Pressure' instrument, move the 'Controls.dynamic'
-- control to 'Controls.breath'.
convert_dynamic :: Bool -> Score.ControlMap -> Score.ControlMap
convert_dynamic pressure controls
    | pressure = maybe controls
        (\sig -> Map.insert Controls.breath sig controls)
        (Map.lookup Controls.dynamic controls)
    | otherwise = controls

convert_pitch :: Log.LogMonad m => Env.Environ
    -> Score.ControlMap -> PSignal.PSignal -> m Signal.NoteNumber
convert_pitch env controls psig = do
    let (sig, nn_errs) = PSignal.to_nn $ PSignal.apply_controls controls $
            PSignal.apply_environ env psig
    unless (null nn_errs) $ Log.warn $ "convert pitch: "
        <> Text.intercalate ", " (TextUtil.ellipsisList 4 (map pretty nn_errs))
    return sig

apply_patch_scale :: Log.LogMonad m => Maybe Patch.Scale -> Signal.NoteNumber
    -> m Signal.NoteNumber
apply_patch_scale scale sig = do
    let (nn_sig, scale_errs) = convert_scale scale sig
    unless (null scale_errs) $ Log.warn $
        "out of range for patch scale: " <> pretty scale_errs
    return nn_sig

-- | Round pitches to the nearest tenth of a cent.  Differences below this are
-- probably imperceptible.  Due to floating point inaccuracy, pitches can wind
-- up being slightly off of integral, leading to pitch bending where there
-- should be none.
round_pitch :: Signal.Y -> Signal.Y
round_pitch nn = fromIntegral (round (nn * 1000)) / 1000

convert_scale :: Maybe Patch.Scale -> Signal.NoteNumber
    -> (Signal.NoteNumber, [(Signal.X, Signal.Y)])
convert_scale Nothing = (, [])
convert_scale (Just scale) = Signal.map_err $ \(TimeVector.Sample x y) ->
    case Patch.convert_scale scale (Pitch.NoteNumber y) of
        Just (Pitch.NoteNumber nn) -> Right (TimeVector.Sample x nn)
        Nothing -> Left (x, y)
