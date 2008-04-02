module Ui.State where
import Control.Monad
import qualified Data.Map as Map
import qualified Foregin

import Ui.Types
import qualified Ui.Color as Color

import qualified Ui.Ruler as Ruler

data State = State {
    state_blocks :: Map.Map BlockId Block
    , state_views :: Map.Map ViewId (View, Maybe (Foreign.Ptr View))
    -- Track data also gets a symbol table.  This is so that I can
    -- efficiently compare a track for identity, and also so I can
    -- change it here and all of its occurrances change.
    , state_tracks :: Map.Map TrackId TrackData
    , state_rulers :: Map.Map RulerId Ruler.Ruler
    } deriving Show


{-
score = State blocks views tracks rulers
rulers = Map.fromList []
blocks = Map.fromList [("b1", Block "t b1" [track1]),
    ("b2", Block "t b2" [track1, track2])]
views = Map.fromList view_list
view_list = [(1, View "b1" "ruler"), (2, View "b2" "ruler")]
track1 = Track "ruler" "t1"
track2 = Track "ruler" "t2"
tracks = Map.fromList [("t1", TrackData
    (Map.fromList [(TrackPos 0, Event "e1"), (TrackPos 10, Event "e2")]))]

score1 = score
score2 = score { state_views = Map.deleteMin views }
score3 = score { state_views = Map.fromList [(2, View "b2" "ruler2")] }
score4 = score3 { state_blocks = Map.fromList [("b2", Block "t b2" [track2])] }

t1 = diff score1 score1
t2 = diff score1 score2
t3 = diff score1 score3
t4 = diff score1 score4

plist :: Show a => [a] -> IO ()
plist = mapM_ print

-}
