-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Convert from the Derive events to MIDI performer specific events.

    Since this module depends on both the Derive and Perform.Midi layers, it
    should be called from Derive or Cmd, not Perform.Midi, even though it's
    physically located in Perform.Midi.
-}
module Perform.Midi.Convert where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Cmd.Cmd as Cmd
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT

import qualified Instrument.Common as Common
import qualified Midi.Midi as Midi
import qualified Perform.ConvertUtil as ConvertUtil
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.MSignal as MSignal
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.Types as Types
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import           Global
import           Types


-- | This is the sampling rate used to convert linear segments from
-- 'Signal.Signal' to 'MSignal.Signal'.
--
-- Since this is only used to interpolate linear segments, it probably doesn't
-- need to be as high as one needed to express the nuances of more complicated
-- curves.  If the tracklang srate is higher, then the MIDI output will be
-- denser and more accurate.
default_srate :: RealTime
default_srate = 1 / 0.015 -- TODO set to PlayUtil.initial_environ[srate]

data MidiLookup = MidiLookup {
    lookup_scale :: Derive.LookupScale
    , lookup_control_defaults
        :: ScoreT.Instrument -> Map ScoreT.Control Signal.Y
    }

-- | Convert Score events to Perform events, emitting warnings that may have
-- happened along the way.
convert :: RealTime -> MidiLookup
    -> (ScoreT.Instrument -> Maybe Cmd.ResolvedInstrument)
    -> [Score.Event] -> [LEvent.LEvent Types.Event]
convert srate lookup = ConvertUtil.convert $ \event resolved ->
    case Cmd.inst_backend resolved of
        Just (Cmd.Midi patch config) ->
            convert_event srate lookup event patch config
        _ -> []

convert_event :: RealTime -> MidiLookup -> Score.Event -> Patch.Patch
    -> Patch.Config -> [LEvent.LEvent Types.Event]
convert_event srate lookup event patch config = run $ do
    let inst = Score.event_instrument event
    let event_controls = Score.event_controls event
    ((perf_patch, ks_controls), pitch) <-
        convert_midi_pitch srate inst patch config event_controls event
    let mode_controls = mode_keyswitches (Score.event_environ event)
            (Patch.patch_mode_map patch)
    let controls = mconcat
            [ make_controls ks_controls
            , MSignal.constant <$> mode_controls
            , convert_controls srate (Types.patch_control_map perf_patch)
                (convert_dynamic pressure event_controls)
            , MSignal.constant <$> lookup_control_defaults lookup inst
            ]
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

-- TODO it's awkward how I have to go from (Midi.Control, Midi.ControlValue)
-- up to (ScoreT.Control, Signal) only to go back down to
-- (Midi.Control, Midi.ControlValue)
make_controls :: [(Midi.Control, Midi.ControlValue)]
    -> Map ScoreT.Control MSignal.Signal
make_controls controls = Map.fromList
    [ (Control.cc_to_control cc, MSignal.constant $ Control.cval_to_val cval)
    | (cc, cval) <- controls
    ]

run :: Log.LogId a -> [LEvent.LEvent a]
run action = LEvent.Event note : map LEvent.Log logs
    where (note, logs) = Log.run_id action

type PitchSignal = MSignal.Signal

-- | If the Event has an attribute matching its keymap, use the pitch from the
-- keymap.  Otherwise convert the pitch signal.
--
-- TODO this used to warn about unmatched attributes, but it got annoying
-- because I use attributes freely.  It still seems like it could be useful,
-- so maybe I want to put it back in again someday.
convert_midi_pitch :: Log.LogMonad m => RealTime -> ScoreT.Instrument
    -> Patch.Patch -> Patch.Config -> DeriveT.ControlMap -> Score.Event
    -> m ((Types.Patch, [(Midi.Control, Midi.ControlValue)]), PitchSignal)
convert_midi_pitch srate inst patch config controls event =
    case Common.lookup_attributes (Score.event_attributes event) attr_map of
        Nothing -> ((perf_patch, []),) . round_sig <$> get_signal
        Just (_, (keyswitches, maybe_keymap)) -> do
            sig <- maybe get_signal set_keymap maybe_keymap
            return (set_keyswitches keyswitches, round_sig sig)
    where
    set_keyswitches [] = (perf_patch, [])
    set_keyswitches keyswitches =
        (perf_patch { Types.patch_keyswitches = ks }, ccs)
        where (ccs, ks) = Seq.partition_on is_control_switch keyswitches
    is_control_switch (Patch.ControlSwitch cc ccval) = Just (cc, ccval)
    is_control_switch _ = Nothing

    -- A PitchedKeymap is mapped through the Patch.Scale.
    set_keymap (Patch.PitchedKeymap low high low_pitch) =
        convert_pitched_keymap (Midi.from_key low) (Midi.from_key high)
            low_pitch <$> get_signal
    -- But UnpitchedKeymap is a constant.
    set_keymap (Patch.UnpitchedKeymap key) =
        return $ MSignal.constant (Midi.from_key key)
    get_signal = apply_patch_scale scale
        =<< convert_event_pitch srate perf_patch controls event
    scale = Patch.config_scale (Patch.config_settings config)
    round_sig = MSignal.map_y round_pitch

    perf_patch = Types.patch inst config patch
    attr_map = Patch.patch_attribute_map patch

mode_keyswitches :: Env.Environ -> Patch.ModeMap -> Map ScoreT.Control Signal.Y
mode_keyswitches (DeriveT.Environ env) (Patch.ModeMap modes) =
    Map.fromList $ map get (Map.toList modes)
    where
    -- for each mode, if set, use that, if unset, use first
    get (key, (deflt, mini_to_control)) = fromMaybe deflt $ do
        mini <- DeriveT.val_to_mini =<< Map.lookup key env
        -- If the lookup fails, then they set a mode, but I don't recognize it.
        -- TODO warn about this?
        Map.lookup mini mini_to_control

convert_pitched_keymap :: Signal.Y -> Signal.Y -> Midi.Key
    -> PitchSignal -> PitchSignal
convert_pitched_keymap low high low_pitch sig = clipped
    where
    -- TODO warn about out_of_range
    (clipped, out_of_range) = MSignal.clip_bounds low high $
        MSignal.scalar_add (low - Midi.from_key low_pitch) sig

-- | Get the flattened Signal.NoteNumber from an event.
convert_event_pitch :: Log.LogMonad m => RealTime -> Types.Patch
    -> DeriveT.ControlMap -> Score.Event -> m PitchSignal
convert_event_pitch srate patch controls event =
    fmap (Signal.to_piecewise_constant srate) $
        convert_pitch (Score.event_environ event) controls note_end $
        Score.event_pitch event
    where
    note_end = Score.event_end event
        + fromMaybe Types.default_decay (Types.patch_decay patch)

convert_pitch :: Log.LogMonad m => Env.Environ
    -> DeriveT.ControlMap -> RealTime -> PSignal.PSignal -> m Signal.NoteNumber
convert_pitch env controls note_end psig = do
    -- Trim controls to avoid applying out of range transpositions.
    -- TODO was drop_at_after
    let trimmed = fmap (fmap (Signal.drop_after note_end)) controls
    let (sig, nn_errs) = PSignal.to_nn $ PSignal.apply_controls trimmed $
            PSignal.apply_environ env psig
    unless (null nn_errs) $ Log.warn $ "convert pitch: "
        <> Text.intercalate ", " (TextUtil.ellipsisList 4
            [pretty x <> ": " <> pretty err | (x, err) <- nn_errs])
    return sig
    where
    -- TODO should I also trim the pitch signal to avoid doing extra work?
    -- trimmed_vals = fmap (fmap (Signal.drop_at_after note_end)) controls

apply_patch_scale :: Log.LogMonad m => Maybe Patch.Scale -> PitchSignal
    -> m PitchSignal
apply_patch_scale scale sig = do
    let (nn_sig, scale_errs) = convert_scale scale sig
    unless (null scale_errs) $ do
        Log.warn $ "out of range for patch scale: "
            <> Text.intercalate ", "
                (TextUtil.ellipsisList 10 (map pretty scale_errs))
    return nn_sig

-- | Round pitches to the nearest tenth of a cent.  Differences below this are
-- probably imperceptible.  Due to floating point inaccuracy, pitches can wind
-- up being slightly off of integral, leading to pitch bending where there
-- should be none.
round_pitch :: Signal.Y -> Signal.Y
round_pitch nn = fromIntegral (round (nn * 1000)) / 1000

convert_scale :: Maybe Patch.Scale -> PitchSignal
    -> (PitchSignal, [(Signal.X, Signal.Y)])
convert_scale Nothing = (, [])
convert_scale (Just scale) = MSignal.map_err $ \(MSignal.Sample x y) ->
    case Patch.convert_scale scale (Pitch.NoteNumber y) of
        Just (Pitch.NoteNumber nn) -> Right (MSignal.Sample x nn)
        Nothing -> Left (x, y)

-- | Convert deriver controls to performance controls.  Drop all non-MIDI
-- controls, since those will inhibit channel sharing later.
convert_controls :: RealTime
    -> Control.ControlMap -- ^ Instrument's control map.
    -> DeriveT.ControlMap -- ^ Controls to convert.
    -> Map ScoreT.Control MSignal.Signal
convert_controls srate inst_cmap =
    Map.fromAscList
        . map (second (Signal.to_piecewise_constant srate . ScoreT.typed_val))
        . filter (Control.is_midi_control inst_cmap . fst)
        . Map.toAscList

-- | If it's a 'Patch.Pressure' instrument, move the 'Controls.dynamic'
-- control to 'Controls.breath'.
convert_dynamic :: Bool -> DeriveT.ControlMap -> DeriveT.ControlMap
convert_dynamic pressure controls
    | pressure = maybe controls
        (\sig -> Map.insert Controls.breath sig controls)
        (Map.lookup Controls.dynamic controls)
    | otherwise = controls
