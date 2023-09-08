-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Support for Hindustani style bols for pakhawaj and tabla.

    It uses the same konnakol / solkattu framework, but of course the
    match is not perfect.  I don't know how much difference because I don't
    know much about Hindustani, but at least these things:

    - Structures are different.  At least there is simple x3 in tihais,
    though they seem to think of them as simply x3 rather than
    seq (gap) seq (gap) seq.

    - On the other hand, there's a notion of structured variation that doesn't
    seem to be present in Carnatic.  It may be too ad-hoc to usefully notate.
    variation by taking from one place and adding to another.  I have
    incomplete similar notions for Carnatic in combinators like "varying"
    and replacements, but those things are probably better done on the fly than
    notated.

    - Hindustani thinks of gaps as "only the space" rather than "karvai stroke
    and space".  This means that "dha.__4" would look like 5 matras from a
    Hindostani point of view.

    - Notion of kali is in Hindustani, but not clear if it could have any
    useful notation presence.

    - Talas are all different.  At the least they need different ruler symbols.

    - Stricter relationship between bols and strokes.  I can probably have a
    single hardcoded stroke map, but beyond that I don't need to realize to
    strokes at all for reading.  It would still be useful for machine
    realization.  However, there is at least some variation between
    instruments, e.g. tabla typically doesn't use the "chapu" stroke, so
    pakhawaj dha is like thom + chapu while tabla is like thom + din.  When
    realizing bols on mridangam, I can use the pakhawaj style.
-}
module Solkattu.Bol (
    Bol(..)
    , parseBols
    , Bols(..)
) where
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Derive.Expr as Expr
import qualified Solkattu.Instrument.Tabla as Tabla
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Solkattu as Solkattu

import           Global


-- | Text representation of a Bol.
type BolT = Text

type Error = Text

data Bol =
    Dha | Dhet |Dhom | Di | Din | Dhi | Dhin | Dhit | Ga | Gi | Ge | Ghen
    | Ka | Kat | Ke | Kre | Ki | Na | Ne | Ra | Ri | Ran
    | Ta | Tak | Taa | Te | Tet | Ti | Tu | Tun
    | Kran -- TODO 2 beats?
    deriving (Eq, Ord, Enum, Bounded, Show)

instance Solkattu.Notation Bol where
    notation = Solkattu.textNotation . Text.toLower . showt
instance Pretty Bol where pretty = Solkattu.notationText

instance Expr.ToExpr Bol where
    to_expr = Expr.generator0 . Expr.Symbol . pretty
instance Expr.ToExpr (Realize.Stroke Bol) where
    to_expr = Realize.toExpr

parseBols :: BolT -> Either Error [Maybe Bols]
parseBols = Solkattu.parseSyllables allBols

allBols :: [(BolT, Bols)]
allBols = sequences
    ++ map (second S1) (Lists.keyOn Solkattu.notationText [minBound ..])

-- | parseBols can return 2nd speed sequences.  This gets turned into a
-- Sequence in Dsl.Bol.
data Bols = S1 Bol | S2 Bol Bol
    deriving (Show)

sequences :: [(BolT, Bols)]
sequences =
    [ ("tr", S2 Ti Ra)
    , ("kt", S2 Ki Ta)
    , ("tk", S2 Ta Ka)
    ]

-- TODO
bolMap :: Realize.SolluMap Bol Tabla.Stroke
Right (bolMap, extras) = Realize.solluMap []

{-
-- Single strokes.
single_bols :: [([Syllable], Bol)]
single_bols =
    [ (["tet"], One Tet)
    , (["te"], One Te)
    , (["ne", "re"], One Ne)
    , (["na"], One Na)
    , (["ta"], One Ta)
    , (["di", "din"], One Di)
    -- bayan
    , (["ka", "kat", "ki"], One Ka)
    , (["ge", "gen", "ga"], One Ge)
    -- both
    , (["dha"], Together Ge Ta)
    , (["dhin"], Together Ge Di)
    , (["dhet"], Together Ge Tette)
    ]

-- kre: p+k
-- dhet: ok
-- dhi: dha
-- dhom: dha

-- TODO the length of the syllables should be the same as the length of the
-- bols or the extras will be silently dropped.
sequences :: [([BoolT], [Note Bol])]
sequences =
    [ (["kre"], note $ Flam Ka Tet)
    , (["gre"], note $ Flam Ge Tet)
    , (["te", "re", "ki", "ta"], notes [Tet, Te, Ka, Tet])
    , (["ki", "ta", "ta", "ka"], notes [Tet, Te, Ka, Tet])
    , (["tr", "kt"], notes2 [[Tet, Te], [Ka, Tet]])
    , (["te", "re", "ki", "ta", "ta", "ka"], notes [Tet, Te, Ka, Tet, Te, Ka])
    , (["tr", "kt", "tk"], notes2 [[Tet, Te], [Ka, Tet], [Te, Ka]])
    , (["kt", "tk"], notes2 [[Tet, Te], [Ka, Tet]])
    , (["ta", "ki"], notes [Tet, Ka])
    , (["te", "ran"], notes [Di3, Di1])
    , (["dhu", "ma"], map Note [Together Ge Di, One Te])
    -- Abbreviations.
    , (["tetekata"], [Notes $ notes [Tet, Te, Ka, Ta, Ge, Di, Ge, Ne]])
    ]
    where
    note = (:[]) . Note
    notes = map (Note . One)
    notes2 = map (Notes . notes)
-}
