-- Copyright 2025 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | On darwin I have only static librubberband, and ghci doesn't want to load
-- it:
-- Loading static libraries is not supported in this configuration.
-- Try using a dynamic library instead.
module Util.Audio.RubberbandStub (
    Config(..)
    , config
    , Option
    , percussiveOptions
    , offline
) where

data Config = Config {
    _options :: ![Option]
    , _timeRatio :: !Double
    , _pitchRatio :: !Double
    } deriving (Show)

config :: Config
config = Config [] 1 1

data Option deriving (Show)

percussiveOptions :: [Option]
percussiveOptions = []

-- offline :: forall rate chan. (KnownNat rate, KnownNat chan)
--     => Config -> Audio.AudioIO rate chan -> Audio.AudioIO rate chan
offline :: Config -> a -> b
offline _ = error "rubberband not linked in"
