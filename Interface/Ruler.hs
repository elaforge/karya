{-# OPTIONS_GHC -XBangPatterns #-}
module Interface.Ruler (
    Ruler, Marklist(..), Mark(..)
    , create
) where

import Interface.Ui (send_action)
import qualified Interface.RulerImpl as R
import Interface.RulerImpl (Ruler, Marklist(..), Mark(..))

force = id

create = R.create
