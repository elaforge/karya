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
import qualified Instrument.InstT as InstT

import Global


main :: IO ()
main = do
    dbNames <- map txt <$> Environment.getArgs
    appDir <- Path.getAppDir
    case dbNames of
        [] -> make appDir LoadInstruments.allLoads
        _ -> do
            let makes = map (`lookup` LoadInstruments.allLoads) dbNames
                notFound = [name | (name, Nothing) <- zip dbNames makes]
                found = [(name, make) | (name, Just make) <- zip dbNames makes]
            unless (null notFound) $
                errorIO $ "dbs not found: " <> showt notFound
            make appDir found

make :: Path.AppDir -> [(InstT.SynthName, (MidiInst.MakeDb, a))] -> IO ()
make appDir = mapM_ $ \(name, (make, _)) -> do
    Text.IO.putStrLn $ "-------- db: " <> name
    make appDir
