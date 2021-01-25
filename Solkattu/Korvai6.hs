{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module Solkattu.Korvai6 where

import qualified Data.Map as Map

import qualified Solkattu.Dsl.Mridangam as Mridangam
import qualified Solkattu.Instrument.KendangTunggal as KendangTunggal

import           Global

-- There's a thing called a Korvai.  It goes in a db :: [Korvai], so
-- it would be inconvenient for it to have a type argument:
data Korvai = Korvai Sections StrokeMaps
    deriving (Show)

data Instruments f =
    IMridangam (f Mridangam.Stroke)
    | IKendang (f KendangTunggal.Stroke)

deriving instance Show (Instruments [])

-- A Korvai contains Sections, which can contain either Sollu or
-- various instrument-specific stroke types.  So there are two types,
-- a "sollu korvai", and a "stroke korvai", which varies by stroke type:
-- data Sections = SSollu [Sollu] | SInst1 [Stroke1] | SInst2 [Stroke2] -- | ...
--     deriving (Show)
data Sections = SSollu [Sollu] | SInstruments (Instruments [])
    deriving (Show)


data Sollu = Sollu deriving (Show, Eq, Ord)
data Stroke1 = Stroke1 deriving (Show)
data Stroke2 = Stroke2 deriving (Show)
newtype Note stroke = Note stroke deriving (Show)

-- A Korvai also has StrokeMaps, which map Sollu to the various
-- instrument-specific strokes:
data StrokeMaps = StrokeMaps {
    smapInst1 :: Map Sollu Stroke1
    , smapInst2 :: Map Sollu Stroke2
    -- ... etc.
    } deriving (Show)

-- Each instrument corresponds to a stroke type:
data Instrument stroke = Instrument {
    instSmap :: StrokeMaps -> Map Sollu stroke
    , isInst1 :: Maybe ([Stroke1] -> [Note stroke])
    , isInst2 :: Maybe ([Stroke2] -> [Note stroke])
    }
inst1 :: Instrument Stroke1
inst1 = Instrument
    { instSmap = smapInst1
    , isInst1 = Just realizeStroke
    , isInst2 = Nothing
    }
inst2 :: Instrument Stroke2
inst2 = Instrument
    { instSmap = smapInst2
    , isInst1 = Nothing
    , isInst2 = Just realizeStroke
    }

-- This is the key point: a Sollu Korvai can be realized to any instrument,
-- and uses the appropriate field in StrokeMaps to do so.  A stroke Korvai
-- can be realized only to its corresponding instrument.  The 'realize'
-- function takes the desired output Instrument and realizes
-- it, if possible.

-- Now, I want to realize a given Korvai on an instrument.
realize :: Instrument stroke -> Korvai -> Either String [Note stroke]
realize instrument (Korvai sections smaps) = case sections of
    -- Sollu Korvai.
    SSollu sollus -> Right $ realizeSollu (instSmap instrument smaps) sollus
    -- SInst1 strokes -> case isInst1 instrument of
    --     Just realizeStroke -> Right $ realizeStroke strokes
    --     Nothing -> Left "not an inst1"
    -- SInst2 strokes -> case isInst2 instrument of
    --     Just realizeStroke -> Right $ realizeStroke strokes
    --     Nothing -> Left "not an inst2"

realizeSollu :: Map Sollu stroke -> [Sollu] -> [Note stroke]
realizeSollu smap = map Note . mapMaybe (`Map.lookup` smap)

realizeStroke :: [stroke] -> [Note stroke]
realizeStroke = map Note

-- -- The proof it works:
-- smaps = StrokeMaps (Map.fromList [(Sollu, Stroke1)])
--     (Map.fromList [(Sollu, Stroke2)])
-- solluKorvai = Korvai (SSollu [Sollu]) smaps
-- testSollu1 = realize inst1 solluKorvai
-- testSollu2 = realize inst2 solluKorvai
--
-- inst1Korvai = Korvai (SInst1 [Stroke1]) smaps
-- inst2Korvai = Korvai (SInst2 [Stroke2]) smaps
-- testInst1 = realize inst1 inst1Korvai
-- testInst2 = realize inst2 inst2Korvai
