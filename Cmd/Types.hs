module Cmd.Types where

data TimeDirection = Rewind | Advance deriving (Eq, Show)
data TrackDirection = PrevTrack | NextTrack deriving (Eq, Show)
