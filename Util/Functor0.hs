{-# LANGUAGE TypeFamilies #-}
-- | This is a very simple \"nullary Functor\" to lift functions into newtype
-- wrappers.
module Util.Functor0 (Functor0(..)) where

class Functor0 a where
    type Elem a :: *
    fmap0 :: (Elem a -> Elem a) -> a -> a
