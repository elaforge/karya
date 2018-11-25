-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Tiny program to generate the cached parts of instrument db.  All it does
-- is call 'Instrument.make_dbs', which will dispatch to every synth that
-- wants to save an instrument db cache.
--
-- You can pass synth names to just generate that synth's db.
module Instrument.MakeDb where
import qualified Data.Text.IO as Text.IO
import qualified System.Environment as Environment

import qualified App.LoadInstruments as LoadInstruments
import qualified App.Path as Path
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Instrument.InstTypes as InstTypes

import Global


main :: IO ()
main = do
    db_names <- map txt <$> Environment.getArgs
    app_dir <- Path.get_app_dir
    case db_names of
        [] -> make app_dir LoadInstruments.all_loads
        _ -> do
            let makes = map (`lookup` LoadInstruments.all_loads) db_names
                not_found = [name | (name, Nothing) <- zip db_names makes]
                found = [(name, make) | (name, Just make) <- zip db_names makes]
            unless (null not_found) $
                errorIO $ "dbs not found: " <> showt not_found
            make app_dir found

make :: Path.AppDir -> [(InstTypes.SynthName, (MidiInst.MakeDb, a))] -> IO ()
make app_dir = mapM_ $ \(name, (make, _)) -> do
    Text.IO.putStrLn $ "-------- db: " <> name
    make app_dir
