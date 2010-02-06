-- | This re-exports especially popular symbols from the Ui package.  Unlike
-- most of the other modules, it's meant to be imported unqualified.
module Ui (
    TrackNum, ScoreTime(..), RealTime(..)
    , BlockId, ViewId, SchemaId, TrackId, RulerId
    , Color
) where

import Ui.Types
import Ui.Color
