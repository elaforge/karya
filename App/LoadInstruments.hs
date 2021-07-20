-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | Load the instrument db.  This collects together all the local instrument
    definitions.

    MIDI instrument definitions are local configuration, so this expects
    a locally defined Local.Instrument module, which should export:

    > midi_synths :: [MidiInst.Synth]
    > all_loads :: [(InstTypes.SynthName, ('MidiInst.MakeDb', 'MidiInst.Load'))]

    "Instrument.MakeDb" is used to create the caches that @all_loads@ relies
    on.
-}
module App.LoadInstruments where
import           System.FilePath ((</>))

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Instrument.Inst as Inst
import qualified Instrument.Parse as Parse
import qualified Local.Instrument
import qualified Perform.Im.Play
import qualified Perform.Lilypond.Constants as Lilypond.Constants
import qualified Perform.Sc.PatchDb as Sc.PatchDb
import qualified Util.Log as Log

#include "hsconfig.h"
#if defined(ENABLE_IM) && !defined(TESTING)
import qualified Synth.Faust.PatchDb as Faust.PatchDb
import qualified Synth.Sampler.PatchDb as Sampler.PatchDb
import qualified User.Elaforge.Instrument.Ness as Ness
#endif

import           Global


midi_synths :: [MidiInst.Synth]
midi_synths = Local.Instrument.midi_synths

-- | Each synth that caches to disk has a function to make the cache, and one
-- to load it.
all_loads :: [MidiInst.Load]
all_loads = map (snd . snd) Local.Instrument.all_loads

im_synths :: [MidiInst.Synth]
im_synths =
    [ Perform.Im.Play.play_cache_synth
#if defined(ENABLE_IM) && !defined(TESTING)
    , Sampler.PatchDb.synth
    , Faust.PatchDb.synth
    , Ness.synth
#endif
    ]

-- | Warnings validating synths.  TODO this should probably be merged with
-- MidiInst.Synth.
synth_warnings :: [Text]
synth_warnings = concat
    [
#if defined(ENABLE_IM) && !defined(TESTING)
    Faust.PatchDb.warnings
#endif
    ]

internal_synths :: [MidiInst.Synth]
internal_synths = [Lilypond.Constants.ly_synth Cmd.empty_code]

load :: Path.AppDir -> IO (Inst.Db Cmd.InstrumentCode)
load app_dir = do
    loaded <- mapMaybeM ($ app_dir) (Sc.PatchDb.load_synth : all_loads)
    let synths = concat
            [ im_synths
            , loaded
            , midi_synths
            , internal_synths
            ]
    let annot_fn = Path.to_absolute app_dir Config.local_dir
            </> "instrument_annotations"
    annots <- Parse.parse_annotations annot_fn >>= \case
        -- The parsec error already includes the filename.
        Left err -> Log.warn (txt err) >> return mempty
        Right annots -> return annots
    let (db, warns) = Inst.db synths
    forM_ (synth_warnings ++ warns) $ \msg -> Log.warn $ "inst db: " <> msg
    (db, not_found) <- return $ Inst.annotate annots db
    unless (null not_found) $
        Log.warn $ "annotated instruments not found: " <> pretty not_found
    return db
