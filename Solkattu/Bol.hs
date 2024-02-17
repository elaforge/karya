-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
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
import qualified Data.Char as Char
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Derive.Expr as Expr
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Solkattu as Solkattu

import           Global


-- | Text representation of a Bol.
type BolT = Text

type Error = Text

data Bol =
    Dha | Dhe | Dhen | Dhet | Dhom | Di | Din | Dhi | Dhin | Dhit | Dhu
    | Ga | Gi | Ge | Ghen | Ghin | Gre
    | Ka | Kat | Ke | Kra | Kre | Ki | Ma | Na | Ne | Ra | Re | Ri | Ran
    | Ṭa -- ^ for transcription from devanagari
    | Ta | Tak | TA -- ^ long A is always tin, or chapu on pakhawaj
    | Taa -- ^ kali version of dha, so follows kinar / sur
    | Te | Ten | Tet | The -- ^ fake bol, kali version of dhere
    | Tre | Ti | Tin | Tra | Tu | Tun
    | Kran -- TODO 2 beats?
    deriving (Eq, Ord, Enum, Bounded, Show)

instance Solkattu.Notation Bol where
    notation b = Solkattu.textNotation $ case Text.uncons (showt b) of
        Just (c, cs) -> Text.cons (Char.toLower c) cs
        Nothing -> Text.empty
instance Pretty Bol where pretty = Solkattu.notationText

instance Expr.ToExpr Bol where
    to_expr = Expr.generator0 . Expr.Symbol . pretty
instance Expr.ToExpr (Realize.Stroke Bol) where
    to_expr = Realize.toExpr

parseBols :: BolT -> Either Error [Maybe Bols]
parseBols = Solkattu.parseSyllables False allBols
    . Text.replace "|" "" -- TODO this should be an akshara assertion

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
    -- , ("Ta", S1 Taa)
    ]
