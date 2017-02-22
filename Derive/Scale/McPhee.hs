-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Old scales, from McPhee's "Music In Bali".
--
-- "Distances in terms of Cents can be computed from the vibration numbers of
-- any two tones by means of a logarithm table---a simple but time-consuming
-- process." -- Music in Bali, introduction, page xv.
module Derive.Scale.McPhee where
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.TextUtil as TextUtil
import qualified Derive.Scale.Bali as Bali
import qualified Perform.Pitch as Pitch
import Global


type Hz = Double

data Laras = Laras {
    name :: Text
    , genre :: Text
    , origin :: Text
    -- | 'hz' start on this note, relative to selisir.
    , base :: Bali.Pitch
    , base_octave :: Pitch.Octave
    , hz :: [Hz]
    } deriving (Eq, Show)

extract :: Pitch.Pitch -> Pitch.Pitch -> Laras
    -> (Text, ([Pitch.NoteNumber], Doc.Doc))
extract low high laras = (dashes sname, (hz_to_nn low high laras, doc))
    where
    doc = "From McPhee's \"Music in Bali\", from "
        <> Doc.Doc (Text.toTitle (origin laras)) <> "."
    sname
        | Text.null (name laras) =
            TextUtil.joinWith "-" (genre laras) (origin laras)
        | otherwise = name laras
    dashes = Text.map (\c -> if c == ' ' then '-' else c)

hz_to_nn :: Pitch.Pitch -> Pitch.Pitch -> Laras -> [Pitch.NoteNumber]
hz_to_nn low high laras =
    Bali.extend_scale (length (hz laras)) low high
        (Pitch.pitch (base_octave laras) (base laras))
        (map Pitch.hz_to_nn (hz laras))

laras :: Laras
laras = Laras "" "" "" Bali.I 4 []

-- | Music in Bali, page 42.
--
-- These start at tembung ding.  According to McPhee, selisir starts at 3,
-- which means that tembung's ding is selisir's dung:
--
-- >         0   1   2   3   4   5   6
-- > tembung I   O   E   Es  U   A   Aa
-- > selisir U   A   As  I   O   E   Es
saih_pitu :: [Laras]
saih_pitu =
    [ s "luang" "seseh"             [276, 305, 345, 372, 410, 466, 505]
    , s "gambang" "krobokan"        [275, 305, 326, 360, 405, 440, 465]
    , s "pegulingan" "klungkung"    [325, 360, 402, 437, 490, 564, 614]
    , s "pegulingan" "tampak gangsal" [310, 337, 365, 425, 457, 485, 560]
    , s "gambuh" "tabanan"          [211, 232, 250, 280, 303, 325, 345]
    , s "gambuh" "batuan"           [202, 220, 237.5, 266, 290, 315, 330]
    ]
    where s genre origin = Laras "" genre origin Bali.U 3

-- | These start at selisir ding.
selisir :: [Laras]
selisir =
    -- "typical" selisir
    [ s "gong" "peliatan"   [280, 305, 327, 405, 435]
    , s "gong" "klungkung"  [264, 291, 322, 397, 434]
    , s "gong" "apuan"      [285, 302, 322, 410, 435]
    , s "gong" "sayan"      [275, 290, 325, 403, 427]
    , s "gong" "gianyar"    [274, 296, 327, 415, 435]
    -- unusual selisir
    -- From Gusti Putuh.
    , laras { name = "tembung cenik", hz = [273, 295, 328, 390, 433] }
    -- Used for McPhee's Semar Pegulingan.  Also called selisir or sunaren.
    , s "pelegongan" "sanur"   [310, 345, 372, 450, 490]
    , s "pelegongan" "selisir" [305, 325, 360, 435, 470]
    -- From "gamelan barong".
    , laras
        { genre = "barong", name = "demung", hz = [362, 408, 434, 562, 593]
        }
    ]
    where s genre origin = Laras "" genre origin Bali.I 4

-- | Bebonangan seems to be an old name for beleganjur.  Dong to dang.
bebonangan :: [Laras]
bebonangan =
    [ s "sayan" [290, 325, 403, 427]
    ]
    where s origin = Laras "" "beleganjur" origin Bali.O 4


-- slendro

-- | o e u a i
slendro :: [Laras]
slendro =
    [ s "6-edo"     [183, 210, 241.5, 277, 318.5] -- theoretical
    , s "kuta"      [183, 206, 241, 280, 327]
    , s "klandis"   [180, 203, 235, 274, 317.5]
    , s "tabanan"   [179, 212, 240, 278, 320]
    -- from Buleleng
    , s "patantaran" [175, 200, 225, 260, 300]
    , s "lod peken" [172, 200, 225, 257.5, 305]
    , s "sawan"     [167.5, 191.5, 220, 248, 290]
    ]
    where s origin = Laras "" "gender" origin Bali.O 3

-- | e u a i
angklung :: [Laras]
angklung =
    [ s "mas"       [410, 469, 519, 620]
    , s "kamasan"   [400, 450, 495, 610]
    , s "sayan"     [365, 420, 485, 564]
    , s "tabanan"   [326, 375, 428, 485]
    -- unusual tunings
    , s "mega tiga" [345, 390, 440, 495]
    , s "nyutebel"  [359, 430, 480, 550]
    , s "culik"     [335, 375, 445, 505]
    , s "negara"    [270, 305, 375, 445]
    ]
    where s origin = Laras "" "angklung" origin Bali.O 4
