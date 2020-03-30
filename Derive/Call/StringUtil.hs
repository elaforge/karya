-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for stringed instruments.
module Derive.Call.StringUtil where
import           Prelude hiding (String)
import qualified Data.List as List
import qualified Data.Tuple as Tuple

import qualified Util.Seq as Seq
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch

import           Global


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
with_string = Derive.with_val EnvKey.string . str_val

insert_string :: String -> Env.Environ -> Env.Environ
insert_string = Env.insert_val EnvKey.string . str_val

data String = String {
    str_pitch :: !PSignal.Pitch
    , str_nn :: !Pitch.NoteNumber
    -- | Assign this value to EnvKey.string for this string.
    , str_val :: !DeriveT.Val
    } deriving (Show)

instance Pretty String where
    pretty str = pretty (str_val str) <> "(" <> pretty (str_pitch str) <> ")"

indexed_strings :: [PSignal.Pitch] -> Derive.Deriver [String]
indexed_strings pitches =
    sequence $ zipWith string_val pitches (map Typecheck.to_val [0 :: Int  ..])

string :: PSignal.Pitch -> Derive.Deriver String
string pitch = string_val pitch (Typecheck.to_val pitch)

string_val :: PSignal.Pitch -> DeriveT.Val -> Derive.Deriver String
string_val pitch val = do
    -- Coerce is ok because I don't want open strings in the environ to
    -- transpose.
    nn <- Pitches.pitch_nn (PSignal.coerce pitch)
    return $ String
        { str_pitch = pitch
        , str_nn = nn
        , str_val = val
        }

type Harmonic = Int

-- | If string is given, try to find this pitch in the harmonics of that
-- string.  Otherwise, find the string from open_strings which has this as
-- its lowest harmonic.
find_harmonic :: Bool -> Harmonic -> [String] -> Maybe String
    -> Pitch.NoteNumber -> Either Text (String, Harmonic)
    -- ^ Either Error (selected string, harmonic)
find_harmonic h1_ok highest_harmonic open_strings maybe_string nn =
    maybe (Left err) Right $ case maybe_string of
        Just string ->
            (string,) <$> harmonic_of h1_ok highest_harmonic (str_nn string) nn
        Nothing
            | null open_strings -> Nothing
            | otherwise -> fmap Tuple.swap $ Seq.minimum_on fst $
                Seq.key_on_just harm_of open_strings
                where
                harm_of string =
                    harmonic_of h1_ok highest_harmonic (str_nn string) nn
    where
    err = "can't find " <> pretty nn <> " as a natural harmonic of "
        <> maybe ("open strings: " <> pretty open_strings) pretty maybe_string

harmonic_of :: Bool -> Harmonic -> Pitch.NoteNumber -> Pitch.NoteNumber
    -> Maybe Harmonic
harmonic_of h1_ok limit base pitch =
    (start+) <$> List.findIndex (close pitch) harmonics
    where
    start = if h1_ok then 1 else 2
    harmonics = take limit $
        map ((Pitch.nn_to_hz base *) . fromIntegral) [start..]
    close nn hz = Pitch.nns_close 50 nn (Pitch.hz_to_nn hz)
