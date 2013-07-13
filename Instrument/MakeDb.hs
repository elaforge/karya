-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Tiny program to generate the cached parts of instrument db.  All it does
-- is call 'Local.Instrument.make_dbs', which will dispatch to every synth that
-- wants to save an instrument db cache.
--
-- You can pass synth names to just generate that synth's db.
module Instrument.MakeDb where
import qualified System.Environment as Environment

import qualified Local.Instrument
import qualified App.Config as Config


main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [] -> Local.Instrument.make_dbs =<< Config.get_app_dir
        _ -> Local.Instrument.make_named_dbs args =<< Config.get_app_dir
