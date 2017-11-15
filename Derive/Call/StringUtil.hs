-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for stringed instruments.
module Derive.Call.StringUtil where
import Prelude hiding (String)
import qualified Data.List as List
import qualified Data.Tuple as Tuple

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Sig as Sig

import qualified Perform.Pitch as Pitch
import Global


-- TODO if Sig.Parser supported Deriver eval, I could make these return String.
open_strings_env :: Sig.Parser [PSignal.Pitch]
open_strings_env = Sig.check non_empty $
    Sig.environ_key EnvKey.open_strings [] "Pitches of open strings."
    where
    non_empty [] = Just "open-strings required"
    non_empty _ = Nothing

string_env :: Sig.Parser (Maybe PSignal.Pitch)
string_env =
    Sig.environ_key EnvKey.string Nothing "Play on this string."

with_string :: String -> Derive.Deriver a -> Derive.Deriver a
with_string = Derive.with_val EnvKey.string . str_pitch

data String = String {
    str_pitch :: !PSignal.Pitch
    , str_nn :: !Pitch.NoteNumber
    } deriving (Show)

instance Pretty String where format = Pretty.format . str_pitch

string :: PSignal.Pitch -> Derive.Deriver String
string pitch = String pitch <$> Pitches.pitch_nn (PSignal.coerce pitch)
    -- Coerce is ok because I don't want open strings in the environ to
    -- transpose.
