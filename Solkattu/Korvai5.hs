{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
module Solkattu.Korvai5 where

import qualified Data.Map as Map

import           Global

-- There's a thing called a Korvai.  It goes in a db :: [Korvai], so
-- it would be inconvenient for it to have a type argument:
data Korvai = Korvai Sections StrokeMaps
    deriving (Show)

-- A Korvai contains Sections, which can contain either Sollu or
-- various instrument-specific stroke types.  So there are two types,
-- a "sollu korvai", and a "stroke korvai", which varies by stroke type:
data Sections = SSollu [Sollu] | SInst1 [Stroke1] | SInst2 [Stroke2] -- | ...
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
    SInst1 strokes -> case isInst1 instrument of
        Just realizeStroke -> Right $ realizeStroke strokes
        Nothing -> Left "not an inst1"
    SInst2 strokes -> case isInst2 instrument of
        Just realizeStroke -> Right $ realizeStroke strokes
        Nothing -> Left "not an inst2"

realizeSollu :: Map Sollu stroke -> [Sollu] -> [Note stroke]
realizeSollu smap = map Note . mapMaybe (`Map.lookup` smap)

realizeStroke :: [stroke] -> [Note stroke]
realizeStroke = map Note

-- The proof it works:
smaps = StrokeMaps (Map.fromList [(Sollu, Stroke1)])
    (Map.fromList [(Sollu, Stroke2)])
solluKorvai = Korvai (SSollu [Sollu]) smaps
testSollu1 = realize inst1 solluKorvai
testSollu2 = realize inst2 solluKorvai

inst1Korvai = Korvai (SInst1 [Stroke1]) smaps
inst2Korvai = Korvai (SInst2 [Stroke2]) smaps
testInst1 = realize inst1 inst1Korvai
testInst2 = realize inst2 inst2Korvai

{-
This works, but adding a new instrument is ugly, because it goes in many
places:
    1 Sections gets a new constructor
    2 StrokeMaps gets a new field
    3 Instrument gets a new isInstN field
    4 Add a new Instrument
    5 Add a new case to realize

Since all of the instruments realize the same way, it should be possible
to have just the Sollu case, and then the instrument case, polymorphic over the
stroke type.  Unfortunately, try as I might, I can't figure out how get this
to happen.

The most appealing is put a type variable on Section.  This then bubbles up
to Korvai.  That then makes the DB complicated and then makes the UI
complicated.  For the UI to not be complicated, it wants to let you say
"realize this korvai on this instrument, which might fail", which means
the types have to be dynamically encoded somewhere, and given that it has to
happen, it should happen down at the Korvai level to avoid complicating the
stuff in between.

Ok, so just make Section a sum type over all instruments... now I'm back where
I started.  Ok, then, how about pre-apply realize to each section, so I don't
have to do the complicated dance with the Instrument types:

data Sections =
    SSollu [Note Sollu] | SInst1 [Note Stroke1] | SInst2 [Note Stroke2]

Now I would have a "sollu" korvai constructor, and a stroke constructor, which
would pre-apply realize, so the separate Instrument type goes away.  StrokeMaps
still needs a field for each instrument, but I got rid of 3, 4, 5.  But
now how do I ask for a particular instrument?  I need yet another sum type
of instruments, with a 1:1 mapping to Sections.  I could try to factor that
out:

data Instruments f = ISollu (f Sollu) | Inst1 (f Stroke1) | Inst2 (f Stroke2)
type Sections = Instruments (Compose [] Note)
type SelectInstrument = Instruments (Const ()) -- yuck

That's a lot of complexity for very little, and I still have to manually write
the lookup function:

lookup :: SelectInstrument -> Sections -> String
lookup select sections = case select of
    ISollu () -> show [notes | ISollu notes <- sections]
    Inst1 () -> show [notes | Inst1 notes <- sections]

So I got rid of some duplications, and added back a few new ones.  I think the
real problem is that I have a set of instruments, each of which has an
associated stroke type and can simultaneously be a product type with
all of them and a sum type with one of them, with an association between them.

be a record

The problem is now I need a way to ask for a realization.  So I need

So that leads to
so to collect them together I need to resolve that, either
by pre-applying the realization, or wrapping them in an existential with their
Instrumnet, which amounts to the same thing.

Why not give up on the single db, and have multiple DBs?
I still want the simplicity of addressing them as `realize instrument korvai`,
so maybe the lookup function could search sollus, then search each instrument
db.  I would need a mapping

. ExtractKorvai gets more complicated, it makes multiple 'korvais', based on
the filename.
-}
