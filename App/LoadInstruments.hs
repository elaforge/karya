-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | Load the instrument db.  This collects together all the local instrument
    definitions.

    MIDI instrument definitions are local configuration, so this expects
    a locally defined Local.Instrument module, which should export:

    > midi_synths :: [MidiInst.Synth]
    > all_loads :: [(InstT.SynthName, ('MidiInst.MakeDb', 'MidiInst.Load'))]

    "Instrument.MakeDb" is used to create the caches that @all_loads@ relies
    on.
-}
module App.LoadInstruments (
    allLoads, load
#ifdef TESTING
    , synthWarnings
    , loadSynths
#endif
) where
import           System.FilePath ((</>))

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT
import qualified Instrument.Parse as Parse
import qualified Local.Instrument
import qualified Perform.Im.Play
import qualified Perform.Lilypond.Constants as Lilypond.Constants
import qualified Perform.Sc.PatchDb as Sc.PatchDb
import qualified Util.Log as Log

-- Disable these for tests, they pull in stuff like _faust_allocate
-- which isn't in ghci.
#include "hsconfig.h"
#if defined(ENABLE_IM) && !defined(TESTING)
import qualified Synth.Faust.PatchDb as Faust.PatchDb
import qualified Synth.Sampler.PatchDb as Sampler.PatchDb
import qualified User.Elaforge.Instrument.Ness as Ness
#endif

import           Global


midiSynths :: [MidiInst.Synth]
midiSynths = Local.Instrument.midiSynths

-- | Each synth that caches to disk has a function to make the cache, and one
-- to load it.
allLoads :: [(InstT.SynthName, (MidiInst.MakeDb, MidiInst.Load))]
allLoads = Local.Instrument.allLoads

imSynths :: [MidiInst.Synth]
imSynths =
    [ Perform.Im.Play.playCacheSynth
#if defined(ENABLE_IM) && !defined(TESTING)
    , Sampler.PatchDb.synth
    , Faust.PatchDb.synth
    , Ness.synth
#endif
    ]

-- | Warnings validating synths.  TODO this should probably be merged with
-- MidiInst.Synth.
synthWarnings :: [Text]
synthWarnings = concat
    [
#if defined(ENABLE_IM) && !defined(TESTING)
    Faust.PatchDb.warnings
#endif
    ]

internalSynths :: [MidiInst.Synth]
internalSynths = [Lilypond.Constants.lySynth Cmd.emptyCode]

load :: Path.AppDir -> IO (Inst.Db Cmd.InstrumentCode)
load appDir = do
    (db, warns) <- loadSynths appDir
    forM_ (synthWarnings ++ warns) $ \msg -> Log.warn $ "inst db: " <> msg
    let annotFn = Path.toAbsolute appDir Config.localDir
            </> "instrument_annotations"
    annots <- Parse.parseAnnotations annotFn >>= \case
        -- The parsec error already includes the filename.
        Left err -> Log.warn (txt err) >> pure mempty
        Right annots -> pure annots
    (db, notFound) <- pure $ Inst.annotate annots db
    unless (null notFound) $
        Log.warn $ "annotated instruments not found: " <> pretty notFound
    pure db

loadSynths :: Path.AppDir -> IO (Inst.Db Cmd.InstrumentCode, [Text])
loadSynths appDir = do
    loaded <- mapMaybeM ($ appDir) $
        Sc.PatchDb.loadSynth : map (snd . snd) allLoads
    pure $ Inst.db $ concat
        [ imSynths
        , loaded
        , midiSynths
        , internalSynths
        ]
