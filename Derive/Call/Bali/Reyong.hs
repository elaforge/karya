-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for reyong and trompong techniques.
module Derive.Call.Bali.Reyong where
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map

import Util.Control
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty

import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Bali.Kotekan as Kotekan
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig

import qualified Perform.Pitch as Pitch
import Types


{-
    Kotekan uses separate calls for the cycle and the \"pickup\" preparation.
    It would be nicer to do it automatically, but sometimes pickups appear in
    irregular places, so I still need a manual option.

    The pickup is actually a transitition, since it may be different depending
    on source and destination pitches.

       3<------2<------1<------1
    +++3<--3+++2<--2+++1<------1
    3353535322323232112121212121

    Or I could write a UI transformer that is like a score integrate, but with
    a transformation.
-}

note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.generator_call_map
    [ ("kilit", c_kilitan reyong_positions)
    , (">kilit", c_kilitan_pickup reyong_positions)
    , ("/", articulation "cek-loose" pos_cek (Attrs.rim <> Attrs.loose))
    , ("X", articulation "cek" pos_cek Attrs.rim)
    , ("O", articulation "byong" pos_byong mempty)
    , ("+", articulation "byut" pos_byong Attrs.mute)
    ]
    where
    articulation = make_articulation reyong_positions

tags :: Tags.Tags
tags = Tags.bali <> Tags.inst

-- * kilitan

c_kilitan_pickup :: [Position] -> Derive.Generator Derive.Note
c_kilitan_pickup = make_kilitan pos_pickup

c_kilitan :: [Position] -> Derive.Generator Derive.Note
c_kilitan = make_kilitan pos_cycle

-- | Kilitan is implemented as a set of patterns indexed by an absolute pitch
-- degree.  The patterns are similar to kotekan, except with absolute pitches,
-- and without a polos \/ sangsih division.
make_kilitan :: (Position -> Degree -> [[ReyongNote]]) -> [Position]
    -> Derive.Generator Derive.Note
make_kilitan get_notes positions = Derive.make_call "kilitan" tags
    "Emit reyong kilitan for all parts."
    $ Sig.call (Kotekan.dur_arg) $ \dur -> Sub.inverting $ \args -> do
        (parse_pitch, show_pitch, _) <- Util.get_pitch_functions
        pitch <- Util.get_parsed_pitch parse_pitch =<< Args.real_start args
        degree <- Derive.require
            ("can't convert pitch: " <> Pretty.pretty pitch)
            (to_degree (Pitch.pitch_degree pitch))
        mconcat $ map (realize show_pitch (Args.range args) dur degree)
            positions
    where
    realize show_pitch (start, end) dur degree position =
        Kotekan.realize_notes start
            (realize_note show_pitch (pos_voice position) start) $
                Kotekan.realize_pattern Kotekan.Repeat start end
                    dur (const (get_notes position degree))

-- * articulation

make_articulation :: [Position] -> Text -> (Position -> [Pitch.Pitch])
    -> Score.Attributes -> Derive.Generator Derive.Note
make_articulation positions name get_notes attrs =
    Derive.make_call name tags "Reyong articulation."
    $ Sig.call0 $ Sub.inverting $ \args -> do
        (_, show_pitch, _) <- Util.get_pitch_functions
        mconcat $ map (realize show_pitch args) positions
    where
    realize show_pitch args position = mconcat $
        map (Util.place args . realize_note show_pitch (pos_voice position)
                (Args.start args))
            (map (\p -> (p, attrs)) (get_notes position))

realize_note :: (Pitch.Pitch -> Maybe Pitch.Note) -> Voice -> ScoreTime
    -> ReyongNote -> Derive.NoteDeriver
realize_note show_pitch voice start (pitch, attrs) =
    Util.add_attrs attrs $
    Derive.with_val Environ.voice voice $ do
        note <- Derive.require ("unshowable pitch: " ++ Pretty.pretty pitch)
            (show_pitch pitch)
        Util.pitched_note =<< Util.eval_note start note


-- * kilitan implementation

data Position = Position {
    pos_voice :: !Voice
    , pos_cek :: ![Pitch.Pitch]
    , pos_byong :: ![Pitch.Pitch]
    , pos_pickup :: Degree -> [[ReyongNote]]
    , pos_cycle :: Degree -> [[ReyongNote]]
    }

type ReyongNote = (Pitch.Pitch, Score.Attributes)
type Voice = Int

reyong_positions :: [Position]
reyong_positions = [position1, position2, position3, position4]

-- E U A (I) 3 5 6 (1)
position1 :: Position
position1 = make_position 1 "u" "ea" table $ \d -> case d of
    I -> ("uua-", "uau-")
    O -> ("ua:-", ":-:-")
    E -> ("eeue", "ueue")
    U -> ("uuau", "auau")
    A -> ("aaea", "eaea")
    where
    table = Map.insert ':' [reyong_note 3 I, reyong_note 3 E] $
        pattern_table [(E, 3), (U, 3), (A, 3), (I, 4)]

-- I O E -- 1 2 3
position2 :: Position
position2 = make_position 2 "o" "ie" table $ \d -> case d of
    I -> ("iioi", "oioi")
    O -> ("ooeo", "eoeo")
    E -> ("eeie", "ieie")
    U -> ("ioe-", "eoe-")
    A -> ("eeie", "ieie")
    where table = pattern_table [(I, 4), (O, 4), (E, 4)]

-- U A (I) -- 5 6 (1)
position3 :: Position
position3 = make_position 3 "a" "ui" table $ \d -> case d of
    I -> ("uua-", "aua-")
    O -> ("uai-", "iai-")
    E -> ("uau-", "u-u-")
    U -> ("uuau", "auau")
    A -> ("aa-a", "-a-a")
    where table = pattern_table [(U, 4), (A, 4), (I, 5)]

-- I O E U -- 1 2 3 5
position4 :: Position
position4 = make_position 4 "o" "ou" table $ \d -> case d of
    I -> ("iioi", "oioi")
    O -> ("ooeo", "eoeo") -- i oeio
    E -> ("eeue", "ueue")
    U -> ("ioe-", "eoe-")
    A -> ("eeie", "ieie")
    where table = pattern_table [(I, 5), (O, 5), (E, 5), (U, 5)]

make_position :: Voice -> [Char] -> [Char] -> Map.Map Char [ReyongNote]
    -> (Degree -> ([Char], [Char])) -> Position
make_position voice cek byong table degree_notes = Position
    { pos_voice = voice
    , pos_cek = map fst $ take 1 $ concat $ parse cek
    , pos_byong = map fst $ concat $ parse byong
    , pos_pickup = parse . fst . degree_notes
    , pos_cycle = parse . snd . degree_notes
    }
    where
    parse :: [Char] -> [[ReyongNote]]
    parse = map $ \c -> fromMaybe
        (error $ "make_position: not in table: " ++ show (voice, c))
        (Map.lookup c table)

pattern_table :: [(Degree, Pitch.Octave)] -> Map.Map Char [ReyongNote]
pattern_table octaves = Map.fromList $
    ('-', []) : [(char d, [reyong_note oct d]) | (d, oct) <- octaves]
    where char = Char.toLower . head . show

reyong_note :: Pitch.Octave -> Degree -> ReyongNote
reyong_note oct d = (Pitch.Pitch oct (from_degree d), mempty)

char_to_pitch :: [(Char, Pitch.Octave)] -> Char -> Maybe [Pitch.Pitch]
char_to_pitch octaves c = case c of
    '-' -> Just []
    _ -> case lookup c octaves of
        Just oct -> (:[]) . Pitch.Pitch oct <$> num_degree c
        Nothing -> Nothing
    where
    num_degree = fmap (flip Pitch.Degree 0) . Num.read_digit

-- | Pentatonic pitch degree.
data Degree = I | O | E | U | A
    deriving (Eq, Ord, Enum, Show)

instance Pretty.Pretty Degree where pretty = show

to_degree :: Pitch.Degree -> Maybe Degree
to_degree d = case Pitch.degree_pc d of
    0 -> Just I; 1 -> Just O; 2 -> Just E; 3 -> Just U; 4 -> Just A
    _ -> Nothing

from_degree :: Degree -> Pitch.Degree
from_degree d = Pitch.Degree (fromEnum d) 0
