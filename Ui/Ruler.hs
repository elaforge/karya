{-# OPTIONS_GHC -XBangPatterns #-}
module Ui.Ruler (
    Ruler, Config(..), Marklist, Mark(..)
    , marklists
    , create, create_marklist
    -- * marklist query
    , forward, backward, forward_from, backward_from
) where

import Ui.Ui (send_action)
import qualified Ui.RulerImpl as R
import Ui.RulerImpl (Ruler, Config(..), Marklist, Mark(..),
    forward, backward, forward_from, backward_from)

-- | Get a ruler's marklists.
marklists :: Ruler -> [Marklist]
marklists = R.config_marklists . R.ruler_config

force = id

create = R.create
create_marklist = R.create_marklist
