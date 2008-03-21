{-# OPTIONS_GHC -XBangPatterns #-}
module Ui.Ruler (
    Ruler, Config(..), Marklist(..), Mark(..)
    , create
) where

import Ui.Ui (send_action)
import qualified Ui.RulerImpl as R
import Ui.RulerImpl (Ruler, Config(..), Marklist(..), Mark(..))

force = id

create = R.create
