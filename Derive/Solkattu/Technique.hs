-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Functions to write performance postprocess functions.
module Derive.Solkattu.Technique where
import qualified Util.Seq as Seq
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Sequence as S
import qualified Derive.Solkattu.Solkattu as Solkattu
import Global


-- | A note with associated S.Meta.
type MetaNote stroke =
    (S.Meta (Solkattu.Group (Realize.Stroke stroke)), Realize.Note stroke)

-- | A Technique is a wrapper around postprocess to write functions which
-- modify strokes during a reduction.
type Technique stroke = [stroke] -- ^ dropped strokes
    -> stroke -- ^ current
    -> [stroke] -- ^ next
    -> Maybe stroke -- ^ Nothing to not modify

postprocess :: Technique (Realize.Stroke stroke)
    -> [MetaNote stroke] -> [MetaNote stroke]
postprocess technique = map (uncurry process) . Seq.zip_nexts
    where
    -- TODO I just pick the first group, but maybe I should try for each nested
    -- group.
    process (meta@(S.Meta ((S.GroupMark _ g) : _) _), note) notes
        | Just stroke <- Realize.note_of note,
                Just out <- technique prevs stroke nexts =
            (meta, set_note out note)
        where
        prevs = Solkattu._dropped g
        nexts = mapMaybe (Realize.note_of . snd) notes
    process (meta, note) _ = (meta, note)
    set_note n (Realize.Note _) = Realize.Note n
    set_note _ n = n

-- | Techinque that ignores Realize.Stroke details.
plain_technique :: Technique stroke -> Technique (Realize.Stroke stroke)
plain_technique technique prevs cur nexts = do
    s <- technique (map Realize._stroke prevs) (Realize._stroke cur)
        (map Realize._stroke nexts)
    return $ cur { Realize._stroke = s }
