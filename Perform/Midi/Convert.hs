-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE StrictData #-}
{- | Convert from the Derive events to MIDI performer specific events.

    Since this module depends on both the Derive and Perform.Midi layers, it
    should be called from Derive or Cmd, not Perform.Midi, even though it's
    physically located in Perform.Midi.
-}
module Perform.Midi.Convert where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Texts as Texts

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
defaultSrate :: RealTime
defaultSrate = 1 / 0.015 -- TODO set to PlayUtil.initial_environ[srate]

data MidiLookup = MidiLookup
    { lookupScale :: Derive.LookupScale
    , lookupControlDefaults
        :: ScoreT.Instrument -> Map ScoreT.Control Signal.Y
    }

-- | Convert Score events to Perform events, emitting warnings that may have
-- happened along the way.
convert :: RealTime -> MidiLookup
    -> (ScoreT.Instrument -> Maybe Cmd.ResolvedInstrument)
    -> [Score.Event] -> [LEvent.LEvent Types.Event]
convert srate lookup = ConvertUtil.convert $ \event resolved ->
    case Cmd.instBackend resolved of
        Cmd.Midi patch config ->
            convertEvent srate lookup patch config event
        _ -> []

convertEvent :: RealTime -> MidiLookup -> Patch.Patch -> Patch.Config
    -> Score.Event -> [LEvent.LEvent Types.Event]
convertEvent srate lookup patch config event = run $ do
    let inst = Score.eventInstrument event
    let eventControls = Score.eventControls event
    ((perfPatch, ksControls), pitch) <-
        convertMidiPitch srate inst patch config eventControls event
    let modeControls = modeKeyswitches (Score.eventEnviron event)
            (Patch.patchModeMap patch)
    let controls = mconcat
            [ makeControls ksControls
            , MSignal.constant <$> modeControls
            , convertControls srate (Types.patchControlMap perfPatch)
                (convertDynamic pressure eventControls)
            , MSignal.constant <$> lookupControlDefaults lookup inst
            ]
        pressure = Patch.hasFlag config Patch.Pressure
        velocity = fromMaybe Perform.defaultVelocity $
            Env.maybeVal EnvKey.attackVal env
            <|> Env.maybeVal EnvKey.dynamicVal env
        releaseVelocity = fromMaybe velocity $
            Env.maybeVal EnvKey.releaseVal env
        env = Score.eventEnviron event
    pure $ Types.Event
        { eventStart = Score.eventStart event
        , eventDuration = Score.eventDuration event
        , eventPatch = perfPatch
        , eventControls = controls
        , eventPitch = pitch
        -- If it's a pressure instrument, then I'm using breath instead
        -- of velocity.  I still set velocity because some synths (e.g.
        -- vsl) use the velocity too in certain cases, but it should be
        -- at least 1 to avoid not even starting the note.  Of course
        -- even without pressure, a note on with velocity 0 is kind of
        -- pointless, but maybe someone wants to fade out.
        , eventStartVelocity = if pressure then max 0.008 velocity
            else velocity
        , eventEndVelocity = releaseVelocity
        , eventStack = Score.eventStack event
        }

-- TODO it's awkward how I have to go from (Midi.Control, Midi.ControlValue)
-- up to (ScoreT.Control, Signal) only to go back down to
-- (Midi.Control, Midi.ControlValue)
makeControls :: [(Midi.Control, Midi.ControlValue)]
    -> Map ScoreT.Control MSignal.Signal
makeControls controls = Map.fromList
    [ (Control.ccToControl cc, MSignal.constant $ Control.cvalToVal cval)
    | (cc, cval) <- controls
    ]

run :: Log.LogId a -> [LEvent.LEvent a]
run action = LEvent.Event note : map LEvent.Log logs
    where (note, logs) = Log.runId action

type PitchSignal = MSignal.Signal

-- | If the Event has an attribute matching its keymap, use the pitch from the
-- keymap.  Otherwise convert the pitch signal.
--
-- TODO this used to warn about unmatched attributes, but it got annoying
-- because I use attributes freely.  It still seems like it could be useful,
-- so maybe I want to put it back in again someday.
convertMidiPitch :: Log.LogMonad m => RealTime -> ScoreT.Instrument
    -> Patch.Patch -> Patch.Config -> ScoreT.ControlMap -> Score.Event
    -> m ((Types.Patch, [(Midi.Control, Midi.ControlValue)]), PitchSignal)
convertMidiPitch srate inst patch config controls event =
    case Common.lookupAttributes (Score.eventAttributes event) attrMap of
        Nothing -> ((perfPatch, []),) . roundSig <$> getSignal
        Just (_, (keyswitches, maybeKeymap)) -> do
            sig <- maybe getSignal setKeymap maybeKeymap
            pure (setKeyswitches keyswitches, roundSig sig)
    where
    setKeyswitches [] = (perfPatch, [])
    setKeyswitches keyswitches =
        (perfPatch { Types.patchKeyswitches = ks }, ccs)
        where (ccs, ks) = Lists.partitionOn isControlSwitch keyswitches
    isControlSwitch (Patch.ControlSwitch cc ccval) = Just (cc, ccval)
    isControlSwitch _ = Nothing

    -- A PitchedKeymap is mapped through the Patch.Scale.
    setKeymap (Patch.PitchedKeymap low high lowPitch) =
        convertPitchedKeymap (Midi.fromKey low) (Midi.fromKey high)
            lowPitch <$> getSignal
    -- But UnpitchedKeymap is a constant.
    setKeymap (Patch.UnpitchedKeymap key) =
        pure $ MSignal.constant (Midi.fromKey key)
    getSignal = applyPatchScale scale
        =<< convertEventPitch srate perfPatch controls event
    scale = Patch.configScale (Patch.configSettings config)
    roundSig = MSignal.mapY roundPitch

    perfPatch = Types.patch inst config patch
    attrMap = Patch.patchAttributeMap patch

modeKeyswitches :: Env.Environ -> Patch.ModeMap -> Map ScoreT.Control Signal.Y
modeKeyswitches (DeriveT.Environ env) (Patch.ModeMap modes) =
    Map.fromList $ map get (Map.toList modes)
    where
    -- for each mode, if set, use that, if unset, use first
    get (key, (deflt, miniToControl)) = fromMaybe deflt $ do
        mini <- DeriveT.valToMini =<< Map.lookup key env
        -- If the lookup fails, then they set a mode, but I don't recognize it.
        -- TODO warn about this?
        Map.lookup mini miniToControl

convertPitchedKeymap :: Signal.Y -> Signal.Y -> Midi.Key
    -> PitchSignal -> PitchSignal
convertPitchedKeymap low high lowPitch sig = clipped
    where
    -- TODO warn about out_of_range
    (clipped, outOfRange) = MSignal.clipBounds low high $
        MSignal.scalarAdd (low - Midi.fromKey lowPitch) sig

-- | Get the flattened Signal.NoteNumber from an event.
convertEventPitch :: Log.LogMonad m => RealTime -> Types.Patch
    -> ScoreT.ControlMap -> Score.Event -> m PitchSignal
convertEventPitch srate patch controls event =
    fmap (Signal.toPiecewiseConstant srate) $
        convertPitch (Score.eventEnviron event) controls noteEnd $
        Score.eventPitch event
    where
    noteEnd = Score.eventEnd event
        + fromMaybe Types.defaultDecay (Types.patchDecay patch)

convertPitch :: Log.LogMonad m => Env.Environ
    -> ScoreT.ControlMap -> RealTime -> PSignal.PSignal -> m Signal.NoteNumber
convertPitch env controls noteEnd psig = do
    -- Trim controls to avoid applying out of range transpositions.
    let trimmed = fmap (fmap (Signal.dropAfter noteEnd)) controls
    let (sig, nnErrs) = PSignal.toNn $ PSignal.applyControls trimmed $
            PSignal.applyEnviron env psig
    unless (null nnErrs) $ Log.warn $ "convert pitch: "
        <> Text.intercalate ", " (Texts.ellipsisList 4
            [pretty x <> ": " <> err | (x, err) <- nnErrs])
    pure sig

applyPatchScale :: Log.LogMonad m => Maybe Patch.Scale -> PitchSignal
    -> m PitchSignal
applyPatchScale scale sig = do
    let (nnSig, scaleErrs) = convertScale scale sig
    unless (null scaleErrs) $ do
        Log.warn $ "out of range for patch scale: "
            <> Text.intercalate ", "
                (Texts.ellipsisList 10 (map pretty scaleErrs))
    pure nnSig

-- | Round pitches to the nearest tenth of a cent.  Differences below this are
-- probably imperceptible.  Due to floating point inaccuracy, pitches can wind
-- up being slightly off of integral, leading to pitch bending where there
-- should be none.
roundPitch :: Signal.Y -> Signal.Y
roundPitch nn = fromIntegral (round (nn * 1000)) / 1000

convertScale :: Maybe Patch.Scale -> PitchSignal
    -> (PitchSignal, [(Signal.X, Signal.Y)])
convertScale Nothing = (, [])
convertScale (Just scale) = MSignal.mapErr $ \(MSignal.Sample x y) ->
    case Patch.convertScale scale (Pitch.NoteNumber y) of
        Just (Pitch.NoteNumber nn) -> Right (MSignal.Sample x nn)
        Nothing -> Left (x, y)

-- | Convert deriver controls to performance controls.  Drop all non-MIDI
-- controls, since those will inhibit channel sharing later.
convertControls :: RealTime
    -> Control.ControlMap -- ^ Instrument's control map.
    -> ScoreT.ControlMap -- ^ Controls to convert.
    -> Map ScoreT.Control MSignal.Signal
convertControls srate instCmap =
    Map.fromAscList
        . map (second (Signal.toPiecewiseConstant srate . ScoreT.valOf))
        . filter (Control.isMidiControl instCmap . fst)
        . Map.toAscList

-- | If it's a 'Patch.Pressure' instrument, move the 'Controls.dynamic'
-- control to 'Controls.breath'.
convertDynamic :: Bool -> ScoreT.ControlMap -> ScoreT.ControlMap
convertDynamic pressure controls
    | pressure = maybe controls
        (\sig -> Map.insert Controls.breath sig controls)
        (Map.lookup Controls.dynamic controls)
    | otherwise = controls
