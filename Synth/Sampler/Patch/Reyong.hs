module Synth.Sampler.Patch.Reyong where

{-
    45-1-31-cek+{closed,open}+v{1..6}.wav
    45-1-31-mute+{closed,open}+v{1..4}.wav
    45-109-127-open+v{1..4}.wav

    keys: 45 48 50 52 55 57 60 62 64 67 69 72 74 76 79
-}


sampleDir :: FilePath
sampleDir = "../data/sampler/reyong"

data Articulation = CekClosed | CekOpen | MuteClosed | MuteOpen | Open
    deriving (Eq, Ord, Show)
