-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to operate on lists.
--
-- TODO port over Util.Seq
module Util.Lists (
    -- * split / join
    splitWith
    , breakWith
) where
import           Data.Bifunctor (first)


splitWith :: (a -> Maybe b) -> [a] -> ([a], [(b, [a])])
splitWith match = go1
    where
    go1 as = case breakWith match as of
        (pre, Nothing) -> (pre, [])
        (pre, Just (b, post)) -> (pre, go2 b post)
    go2 b0 as = case breakWith match as of
        (pre, Nothing) -> [(b0, pre)]
        (pre, Just (b1, post)) -> (b0, pre) : go2 b1 post

breakWith :: (a -> Maybe b) -> [a] -> ([a], Maybe (b, [a]))
breakWith f = go
    where
    go (a : as) = case f a of
        Just b -> ([], Just (b, as))
        Nothing -> first (a:) (go as)
    go [] = ([], Nothing)
