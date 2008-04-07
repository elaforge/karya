module Ui.Update where

import qualified Ui.Block as Block
import Ui.Block (ViewId, BlockId)
import qualified Ui.Ruler as Ruler
import Ui.Ruler (RulerId)


data Update = ViewUpdate ViewId ViewUpdate | BlockUpdate BlockId BlockUpdate
    -- | One of these in the updates means a serious error occurred diffing
    -- the states.
    | Error String
    deriving Show

data ViewUpdate =
    CreateView
    | DestroyView
    | ViewSize Block.Rect
    | ViewConfig Block.ViewConfig
    deriving Show

data BlockUpdate =
    BlockTitle String
    | BlockConfig Block.Config
    | RemoveTrack Int
    | InsertTrack Int Block.Tracklike Block.Width
    deriving Show
