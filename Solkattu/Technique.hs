-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Functions to write performance postprocess functions.
module Solkattu.Technique where
import qualified Util.Seq as Seq
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Sequence as S
import Global


-- | A realized note with associated S.Meta.
type Flat stroke =
    S.Flat (Realize.Group (Realize.Stroke stroke)) (Realize.Note stroke)

-- | A Technique is a wrapper around postprocess to write functions which
-- modify strokes during a reduction.
type Technique stroke = [stroke] -- ^ dropped strokes
    -> stroke -- ^ current
    -> [stroke] -- ^ next
    -> Maybe stroke -- ^ Nothing to not modify

postprocess :: Technique (Realize.Stroke stroke)
    -> [Flat stroke] -> [Flat stroke]
postprocess technique = map process . zipNeighbors
    where
    -- TODO I just pick the innermost group, but maybe I should try for each
    -- nested group.
    process (Just (S.FGroup _ _ g), S.FNote tempo note, notes)
        | Just stroke <- Realize.noteOf note,
                Just out <- technique prevs stroke nexts =
            S.FNote tempo $ setNote out note
        where
        prevs = Realize._dropped g
        nexts = mapMaybe Realize.noteOf (S.flattenedNotes notes)
    process (_, note, _) = note
    setNote n (Realize.Note _) = Realize.Note n
    setNote _ n = n

zipNeighbors :: [a] -> [(Maybe a, a, [a])]
zipNeighbors = map merge . Seq.zip_nexts . Seq.zip_prev
    where merge ((prev, cur), nexts) = (prev, cur, map snd nexts)

-- | Techinque that ignores Realize.Stroke details.
plainTechnique :: Technique stroke -> Technique (Realize.Stroke stroke)
plainTechnique technique prevs cur nexts = do
    s <- technique (map Realize._stroke prevs) (Realize._stroke cur)
        (map Realize._stroke nexts)
    return $ cur { Realize._stroke = s }
