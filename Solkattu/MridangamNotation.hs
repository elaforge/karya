-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for the mridangam-specific DSL.  Sister to "Solkattu.Notation",
-- this is like "Solkattu.MridangamGlobal", but with more complicated
-- implementations, and without the annoying overidden (.).
module Solkattu.MridangamNotation (
    merge
    , makeNote1, makeNote
) where
import qualified Util.CallStack as CallStack
import qualified Util.Seq as Seq
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Notation as Notation
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Sequence as S
import qualified Solkattu.Solkattu as Solkattu

import Global


type NoteT sollu = S.Note Solkattu.Group (Solkattu.Note sollu)
type Stroke = Realize.Stroke Mridangam.Stroke

merge :: CallStack.Stack => [NoteT Stroke] -> [NoteT Stroke] -> [NoteT Stroke]
merge as bs
    | not (null trailing) =
        Solkattu.throw $ "trailing strokes: " <> pretty trailing
    | otherwise = Notation.speed maxSpeed $ map merge1 pairs
    where
    -- At this point TempoChanges and Groups should have been flattened away.
    merge1 (a, b)
        | isRest a = b
        | isRest b = a
        | otherwise = makeNote1 $
            Mridangam.bothRStrokes (toStroke1 a) (toStroke1 b)
    isRest (S.Note (Solkattu.Space Solkattu.Rest)) = True
    isRest _ = False
    (pairs, trailing) = second (either id id) $
        Seq.zip_remainder (flatten as) (flatten bs)

    flatten :: CallStack.Stack => [NoteT Stroke] -> [NoteT Stroke]
    flatten = Solkattu.check
        . (traverse (traverse unstroke) <=< S.flattenSpeed maxSpeed)
        . stripGroups
    maxSpeed = max (S.maxSpeed (S.flatten as)) (S.maxSpeed (S.flatten bs))

-- | 'S.flattenSpeed' gives me 'S.Stroke's, so turn them back into notes and
-- rests.  It's an error to see 'S.Sustain', because that means I'm trying to
-- merge sustained notation, e.g. patterns.
unstroke :: Pretty a => S.Stroke (Solkattu.Note a)
    -> Either Text (Solkattu.Note a)
unstroke = \case
    S.Attack a -> Right a
    S.Rest -> Right $ Solkattu.Space Solkattu.Rest
    S.Sustain space@(Solkattu.Space _) -> Right space
    S.Sustain a -> Left $ "can't merge with pattern: " <> pretty a

toStroke1 :: (CallStack.Stack, Pretty a, Pretty g) =>
    S.Note g (Solkattu.Note a) -> a
toStroke1 (S.Note (Solkattu.Note note)) = Solkattu._sollu note
toStroke1 note = Solkattu.throw $ "expected sollu, but got " <> pretty note

stripGroups :: [S.Note g a] -> [S.Note x a]
stripGroups = concatMap strip
    where
    strip = \case
        S.Note a -> [S.Note a]
        S.TempoChange change subs ->
            [S.TempoChange change (concatMap strip subs)]
        S.Group _ subs -> concatMap strip subs

-- * util

makeNote1 :: stroke -> S.Note g (Solkattu.Note stroke)
makeNote1 stroke = S.Note $ Solkattu.Note $ Solkattu.note stroke

makeNote :: stroke -> [S.Note g (Solkattu.Note stroke)]
makeNote stroke = [makeNote1 stroke]
