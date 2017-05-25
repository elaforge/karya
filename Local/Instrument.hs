-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Load the instrument db.  This collects together all the local instrument
    definitions.

    The convention is that each synthesizer has a module in Local\/Instrument\/,
    and each one exports a @load :: 'Load'@, and possibly @make_db :: 'MakeDb'@.

    "Instrument.MakeDb" is used to run the @make_db@s.
-}
module Local.Instrument (
    Load, MakeDb, load, all_loads
    , midi_synths, im_synths
) where
import System.FilePath ((</>))

import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Perform.Im.Play
import qualified Perform.Lilypond.Constants as Lilypond.Constants
import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes
import qualified Instrument.Parse as Parse

import qualified Local.Instrument.Drumaxx as Drumaxx
import qualified Local.Instrument.Fm8 as Fm8
import qualified Local.Instrument.Kontakt as Kontakt
import qualified Local.Instrument.Massive as Massive
import qualified Local.Instrument.Morpheus as Morpheus
import qualified Local.Instrument.Morphine as Morphine
import qualified Local.Instrument.Pianoteq as Pianoteq
import qualified Local.Instrument.Reaktor as Reaktor
import qualified Local.Instrument.Spicy as Spicy
import qualified Local.Instrument.Tassman as Tassman
import qualified Local.Instrument.Vl1 as Vl1
import qualified Local.Instrument.Vsl as Vsl
import qualified Local.Instrument.Z1 as Z1

import qualified Synth.Faust.PatchDb as Faust.PatchDb
import qualified Synth.Sampler.PatchDb as Sampler.PatchDb
import qualified App.Config as Config
import Global


-- | Instrument definition modules that need to load from disk export a
-- function called @load@, with this signature.  The FilePath is the
-- 'Config.instrument_dir' and could hold cached instruments, as created by
-- 'MakeDb'.
type Load = FilePath -> IO (Maybe MidiInst.Synth)

{- | Some synths may require a more expensive load, e.g. they could parse
    a directory full of sysex dumps.  These expose a @make_db@ function with
    this type.  As with 'Load', the FilePath is 'Config.instrument_dir'.  The
    function is expected to do its work and save the results in the instrument
    dir

    You should use 'Cmd.Instrument.MidiInst.save_synth', which will put the
    file into 'Config.instrument_cache_dir' with the same name as the synth.
-}
type MakeDb = FilePath -> IO ()

-- | Synth declarations for each synth that is declared purely.
midi_synths :: [MidiInst.Synth]
midi_synths =
    [ Drumaxx.synth, Fm8.synth, Kontakt.synth, Massive.synth
    , Morphine.synth, Pianoteq.synth, Reaktor.synth, Spicy.synth, Tassman.synth
    , Vsl.synth
    ]

im_synths :: [MidiInst.Synth]
im_synths =
    [ Perform.Im.Play.play_cache_synth
    , Sampler.PatchDb.synth
    , Faust.PatchDb.synth
    ]

internal_synths :: [MidiInst.Synth]
internal_synths = [Lilypond.Constants.ly_synth Cmd.empty_code]

-- | Each synth that caches to disk has a function to make the cache, and one
-- to load it.
all_loads :: [(InstTypes.SynthName, (MakeDb, Load))]
all_loads =
    [ (Morpheus.synth_name, (Morpheus.make_db, Morpheus.load))
    , (Vl1.synth_name, (Vl1.make_db, Vl1.load))
    , (Z1.synth_name, (Z1.make_db, Z1.load))
    ]

load :: FilePath -> IO (Inst.Db Cmd.InstrumentCode)
load app_dir = do
    loaded <- mapMaybeM
        (($ Config.make_path app_dir Config.instrument_dir) . snd . snd)
        all_loads
    let synths = im_synths ++ loaded ++ midi_synths ++ internal_synths
    let annot_fn = Config.make_path app_dir Config.local_dir
            </> "instrument_annotations"
    annots <- Parse.parse_annotations annot_fn >>= \x -> case x of
        -- The parsec error already includes the filename.
        Left err -> Log.warn (txt err) >> return mempty
        Right annots -> return annots
    let (db, warns) = Inst.db synths
    forM_ warns $ \msg -> Log.warn $ "inst db: " <> msg
    (db, not_found) <- return $ Inst.annotate annots db
    unless (null not_found) $
        Log.warn $ "annotated instruments not found: " <> pretty not_found
    return db
