-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeFamilies #-}
-- | This is a very simple \"nullary Functor\" to lift functions into newtype
-- wrappers.
module Util.Functor0 (Functor0(..)) where

class Functor0 a where
    type Elem a :: *
    fmap0 :: (Elem a -> Elem a) -> a -> a
