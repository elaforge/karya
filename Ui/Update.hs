module Ui.Update where
import qualified Ui.State as State

data Update =
    CreateView State.ViewId
    | DestroyView State.ViewId
    | SetRuler State.ViewId State.RulerId
    | SetTitle State.BlockId String
    | RemoveTrack State.BlockId Int
    | InsertTrack State.BlockId Int State.Tracklike
    deriving Show
