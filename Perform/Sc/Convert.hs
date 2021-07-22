-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
module Perform.Sc.Convert (
    default_srate, convert
#ifdef TESTING
    , module Perform.Sc.Convert
#endif
) where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd
import qualified Derive.DeriveT as DeriveT
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT

import qualified Perform.ConvertUtil as ConvertUtil
import qualified Perform.Midi.MSignal as MSignal
import qualified Perform.Sc.Note as Note
import qualified Perform.Sc.Patch as Patch
import qualified Perform.Signal as Signal

import           Global
import           Types


-- Technically SC should not need this, since it should be able to do
-- interpolation on its own side like with im, but in practice SC patches
-- are probably set up like MIDI ones.
default_srate :: RealTime
default_srate = 1 / 0.015 -- TODO set to PlayUtil.initial_environ[srate]

-- | Convert Score events to Sc Notes.
convert :: RealTime -> (ScoreT.Instrument -> Maybe Cmd.ResolvedInstrument)
    -> [Score.Event] -> Note.Notes
convert srate = ConvertUtil.convert $ \event resolved ->
    case Cmd.inst_backend resolved of
        Cmd.Sc patch -> convert_event srate patch event
        _ -> []

convert_event :: RealTime -> Patch.Patch -> Score.Event -> Note.Notes
convert_event srate patch event = run $ do
    pitch <- if Map.member Patch.c_pitch (Patch.controls patch)
        then Just <$> convert_pitch event
        else return Nothing
    let controls = maybe id
            (Map.insert Patch.c_pitch . ScoreT.untyped . Signal.coerce) pitch $
            Score.event_controls event
    return $ Note.Note
        { patch = Patch.name patch
        , start = Score.event_start event
        , duration = Score.event_duration event
        , duration_control = Patch.duration_control patch
        , controls = convert_controls srate (Patch.controls patch)
            (Score.event_start event) (Score.event_end event) controls
        }

run :: Log.LogId a -> [LEvent.LEvent a]
run action = LEvent.Event note : map LEvent.Log logs
    where (note, logs) = Log.run_id action

-- | Convert deriver controls to performance controls.  Drop all non-MIDI
-- controls, since those will inhibit channel sharing later.
convert_controls :: RealTime
    -> Map ScoreT.Control Note.ControlId -- ^ Patch control map.
    -> RealTime -> RealTime
    -> DeriveT.ControlMap -- ^ Controls to convert.
    -> Map Note.ControlId MSignal.Signal
convert_controls srate patch_controls start end =
    Map.fromAscList . mapMaybe convert . Map.toAscList
    where
    -- TODO trim to start of the next note of this patch.  But this will be
    -- quite inefficient if there's a long signal and a short decay.
    convert (control, signal) = case Map.lookup control patch_controls of
        Nothing -> Nothing
        Just cid -> Just
            ( cid
            , Signal.to_piecewise_constant srate $
                Signal.clip_after end $ Signal.clip_before start $
                ScoreT.typed_val signal
            )
    -- TODO it might be more efficient to skip the MSignal and go directly to
    -- OSC.  Especially because I want just in dur range...  or I guess longer,
    -- to account for decay?

-- TODO same as Im.Convert.convert_pitch
convert_pitch :: Log.LogMonad m => Score.Event -> m Signal.NoteNumber
convert_pitch event = do
    let (sig, warns) = Score.nn_signal event
    unless (null warns) $ Log.warn $
        "convert pitch: " <> Text.intercalate ", " (map pretty warns)
    return sig
