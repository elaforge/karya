-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Functions to write performance postprocess functions.
module Solkattu.Technique where
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import Global


-- | A realized note with associated S.Meta.
type Flat stroke =
    S.Flat (Realize.Group (Realize.Stroke stroke)) (Realize.Note stroke)

-- | A Technique is a wrapper around postprocess to write functions which
-- modify strokes during a reduction.
type Technique stroke = [stroke] -- ^ Dropped strokes.  These are in original
    -- order, which means if you want to see the previous strokes, you have to
    -- use Seq.rtake.
    -> stroke -- ^ current
    -> [stroke] -- ^ next
    -> Maybe stroke -- ^ Nothing to not modify

postprocess :: Technique (Realize.Stroke stroke)
    -> [Flat stroke] -> [Flat stroke]
postprocess technique = map process
    where
    -- TODO I just pick the innermost group, but maybe I should try for each
    -- nested group.
    process (S.FGroup tempo g children) = case children of
        S.FNote tempo note : notes
            | Just newNote <- group (Realize._dropped g) note notes ->
                S.FGroup tempo g (S.FNote tempo newNote : notes)
        _ -> S.FGroup tempo g (map process children)
    process note@(S.FNote {}) = note
    group prevs note notes = do
        stroke <- Realize.noteOf note
        let nexts = mapMaybe Realize.noteOf (S.flattenedNotes notes)
        Realize.Note <$> technique prevs stroke nexts

-- | Techinque that ignores Realize.Stroke details.
plain :: Technique stroke -> Technique (Realize.Stroke stroke)
plain technique prevs cur nexts = do
    s <- technique (map Realize._stroke prevs) (Realize._stroke cur)
        (map Realize._stroke nexts)
    return $ cur { Realize._stroke = s }
