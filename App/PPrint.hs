-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Standalone pretty printer for debugging.
import qualified Util.PPrint as PPrint

main :: IO ()
main = putStr . PPrint.format =<< getContents
