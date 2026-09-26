-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for stringed instruments.
module Derive.Call.StringUtil where
import           Prelude hiding (String)
import qualified Data.List as List
import qualified Data.Tuple as Tuple

import qualified Util.Lists as Lists
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


-- | TODO if Sig.Parser supported Deriver eval, I could make these return
-- String, and not need to call 'indexed_strings'.
openStringsEnv :: Sig.Parser [PSignal.Pitch]
openStringsEnv = Sig.check nonEmpty $
    Sig.environKey EnvKey.openStrings ([] :: [PSignal.Pitch])
        "Pitches of open strings."
    where
    nonEmpty [] = Just "open-strings required"
    nonEmpty _ = Nothing

stringEnv :: Sig.Parser (Maybe PSignal.Pitch)
stringEnv =
    Sig.environKey EnvKey.string (Nothing :: Maybe PSignal.Pitch)
        "Play on this string."

withString :: String -> Derive.Deriver a -> Derive.Deriver a
withString = Derive.withVal EnvKey.string . strVal

insertString :: String -> Env.Environ -> Env.Environ
insertString = Env.insertVal EnvKey.string . strVal

data String = String
    { strPitch :: !PSignal.Pitch
    , strNn :: !Pitch.NoteNumber
    -- | Assign this value to EnvKey.string for this string.
    , strVal :: !DeriveT.Val
    } deriving (Show)

instance Pretty String where
    pretty str = pretty (strVal str) <> "(" <> pretty (strPitch str) <> ")"

indexedStrings :: [PSignal.Pitch] -> Derive.Deriver [String]
indexedStrings pitches =
    sequence $ zipWith stringVal pitches (map Typecheck.toVal [0 :: Int  ..])

string :: PSignal.Pitch -> Derive.Deriver String
string pitch = stringVal pitch (Typecheck.toVal pitch)

stringVal :: PSignal.Pitch -> DeriveT.Val -> Derive.Deriver String
stringVal pitch val = do
    -- Coerce is ok because I don't want open strings in the environ to
    -- transpose.
    nn <- Pitches.pitchNn (PSignal.coerce pitch)
    pure $ String
        { strPitch = pitch
        , strNn = nn
        , strVal = val
        }

type Harmonic = Int

-- | If string is given, try to find this pitch in the harmonics of that
-- string.  Otherwise, find the string from open_strings which has this as
-- its lowest harmonic.
findHarmonic :: Bool -> Harmonic -> [String] -> Maybe String
    -> Pitch.NoteNumber -> Either Text (String, Harmonic)
    -- ^ Either Error (selected string, harmonic)
findHarmonic h1Ok highestHarmonic openStrings maybeString nn =
    maybe (Left err) Right $ case maybeString of
        Just string ->
            (string,) <$> harmonicOf h1Ok highestHarmonic (strNn string) nn
        Nothing
            | null openStrings -> Nothing
            | otherwise -> fmap Tuple.swap $ Lists.minimumOn fst $
                Lists.keyOnJust harmOf openStrings
                where
                harmOf string =
                    harmonicOf h1Ok highestHarmonic (strNn string) nn
    where
    err = "can't find " <> pretty nn <> " as a natural harmonic of "
        <> maybe ("open strings: " <> pretty openStrings) pretty maybeString

harmonicOf :: Bool -> Harmonic -> Pitch.NoteNumber -> Pitch.NoteNumber
    -> Maybe Harmonic
harmonicOf h1Ok limit base pitch =
    (start+) <$> List.findIndex (close pitch) harmonics
    where
    start = if h1Ok then 1 else 2
    harmonics = take limit $
        map ((Pitch.nnToHz base *) . fromIntegral) [start..]
    close nn hz = Pitch.nnsClose 50 nn (Pitch.hzToNn hz)
