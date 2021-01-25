{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
module Solkattu.Korvai4 where

import qualified Data.Functor.Const as Const
import qualified Data.Map as Map

import qualified Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Tags as Tags

import           Global

-- There's a thing called a Korvai, which goes in a db :: [Korvai], so
-- it has no type args:
data Korvai = Korvai Sections StrokeMaps

-- A Korvai contains Sections, which can contain either Sollu or
-- various instrument-specific stroke types:
data Sections = SSollu [Sollu] | SInst1 [Stroke1] | SInst2 [Stroke2] -- | ...

-- A Korvai also has StrokeMaps, which map Sollu to the various
-- instrument-specific strokes:
data StrokeMaps = StrokeMaps {
    smapInst1 :: Map Sollu Stroke1
    , smapInst2 :: Map Sollu Stroke2
    -- ... etc.
    }

-- Now, I want to realize a given Korvai on an instrument.
realize :: Instrument stroke -> Korvai -> [Note stroke]
realize instrument (Korvai sections smaps) = case sections of
    -- If it has Sollus in it, it can be realized to any instrument there is a
    -- StrokeMap for.
    SSollu sollus -> realizeSollu (smapInstN smaps) sollus
    SInst1 strokes -> realizeInstrument strokes

realizeSollu :: Map Sollu stroke -> [Sollu] -> [Note stroke]
realizeSollu = undefined

realizeInstrument :: [stroke] -> [Note stroke]
realizeInstrument = undefined

{-
I want to have a Korvai stroke, but be able to put them all in a Db.

Maybe existential?  Korvai stroke is paired with Instrument stroke.

The usual way is a class.  That's just automatic Instrument.

How to put in a Db then?

I want to be able to say realize some instrument, and get either realized
strokes, or Nothing.

Use laziness, and store Map Instrument realized

Realization is either:
    Instrument stroke -> [Section Sollu] -> Maybe [Note stroke]
    Instrument stroke -> [Section stroke] -> [Note stroke]

Is there any problem with "realizing" on Korvai construction?
I no longer have sections, so if I want start, end, tags I have to
propagate those through too.

If I'm going to have a sum of the realized values, why not a sum of the
inputs?  Isn't that what KorvaiType was?  So what if I just extend KorvaiType
with Kendang?

No, not the same, because with the inputs I have to choose the realize to
apply, hence instFromSollu and instFromMridangam.

The other nice thing was to get rid of instFromMridangam.  But I think I just
hardcoded it in realizeInstrument.

I guess lazy sum puts the set of Instruments in the korvai constructor, so
there's no need to choose and maybe fail, just look at the ones in there.
Namely, can I get rid of instFromSollu and instFromMridangam?
Actually, can I get rid of Instrument entirely?

Probably not, because instToScore, and also I still want to collect the
ways to realize each instrument, not just hardcode in the korvai constructor.

The problem is now how do I select which realization I want?
Previously it was the Instrument, which would actually do the realization.

What if I put sollu type back into Korvai.

I come right back to the original solution:

    , instRealizeSollu :: StrokeMaps -> Section Sollu
        -> Section [Note stroke]
    , instRealize :: StrokeMaps -> Section stroke -> Section [Note stroke]

This is just less-factored

    , instFromSollu :: Realize.SolluMap stroke
        -> Realize.ToStrokes Solkattu.Sollu stroke
    , instFromMridangam ::
        Maybe (Realize.ToStrokes (Realize.Stroke Mridangam.Stroke) stroke)

In fact, I think smap is the only thing that varies?

Problem is still that formatInstrument takes a type argument.
But, where will I run into trouble with that?  Putting it in the Db at least.

Also I'm stuck with separate formatInstrument and formatSollu.
It's really (sollu -> stroke).

A Korvai needs to have the formatting packed in:

GKorvai
    (Korvai stroke)
    (Instrument stroke -> Text)
    (Instrument stroke -> Text)

But this is the same as just rendering... well, except I can provide the
instrument.  But these GKorvais are all the same.  Well, not quite, I have
separate ones for sollu and instrument.

This is just the same as the Realized thing from before... except now I have
a way to render from Instrument.

I can't construct the GKorvai inside Korvai though, because it uses Terminal.
I guess it can go in Dsl.Mridangam, Dsl.Solkattu.

But if I make all score Korvais into GKorvai, then all the modification
functions are a pain, they have to go through gKorvai.  Ok, how about
make them typed, but wrap before they go into the Db.

How?  Extract could expect a type based on the module name, and wrap in
a function.  I think that's the only way if Korvai is typed.  Otherwise,
I have to put the type inside Korvai, and that means Extracted.

What if I go back to KorvaiType, but put a forall in there for instrument?

I wind up with in realize:

realize :: Solkattu.Notation stroke => Instrument stroke -> Korvai
    -> [Either Error ([Flat stroke], [Realize.Warning])]
realize instrument korvai
    | stroke == Sollu && sections == TSollu = Realize.realizeSimpleStroke
    | sections == TSollu = Realize.realizeSollu (Realize.smapSolluMap smap)
    | sections == TInstrument = Realize.realizeStroke

-}

class Notation stroke

data Instruments f =
    IMridangam (f Mridangam.Stroke)
    | IKendang (f KendangTunggal.Stroke)

data Instrument stroke = Instrument {
    instStrokeMap :: StrokeMaps -> StrokeMap stroke
    -- Sollu can be converted to all instruments.
    , instFromSollu :: ToStrokes Sollu stroke
    , instDirect :: KorvaiType -> Maybe (ToStrokes stroke stroke)
    -- Instrument can be converted to instrument.
    -- , instFromStroke

    -- , instFromSollu :: Realize.SolluMap stroke
    --     -> Realize.ToStrokes Solkattu.Sollu stroke
    -- , instFromMridangam ::
    --     Maybe (Realize.ToStrokes (Realize.Stroke Mridangam.Stroke) stroke)
    }

-- realizeSection :: ToStrokes sollu stroke -> StrokeMap stroke
--     -> Section sollu -> Section [Note stroke]

-- type InstrumentName = Instruments (Const.Const ())

mridangam :: Instruments Instrument
mridangam = IMridangam $ Instrument
    { instStrokeMap = smapMridangam
    }

writeAll :: FilePath -> Korvai -> IO ()
writeAll = undefined

-- formatInstrument :: Instruments Instrument -> Korvai -> [Text]
-- formatInstrument instrument korvai = case (instrument, korvaiSections korvai) of
--     (_, ISollu (Sections sections)) ->
--         map (formatResults . realizeSection realizeStroke smap) sections
--         where smap = undefined

realize :: Instrument stroke -> Korvai -> [Section [Note stroke]]
realize instrument korvai = case korvaiSections korvai of
    TSollu sections -> undefined
    TInstrument sections -> undefined
    where
    smap = instStrokeMap instrument (korvaiStrokeMaps korvai)

-- realize :: Instrument stroke -> Korvai sollu -> [Note stroke]
-- realize instrument korvai
--   | sollu ~ Sollu = realizeSollu instrument
--   | stroke ~ sollu = realizeInstrument instrument
--   | otherwise = error

formatResults :: Section [Note stroke] -> Text
formatResults = undefined

data Korvai = Korvai {
    korvaiSections :: KorvaiType
    , korvaiStrokeMaps :: StrokeMaps
    }
-- newtype Sections stroke = Sections [Section stroke]

data KorvaiType =
    TSollu [Section Sollu]
    | TInstrument (Instruments Section)

-- -- This is awkward, I would prefer Map Instrument (StrokeMap stroke)
-- type StrokeMaps = [Instruments StrokeMap]
-- newtype StrokeMap stroke = StrokeMap (Map Sollu stroke)

data StrokeMaps = StrokeMaps {
    smapMridangam :: StrokeMap Mridangam.Stroke
    , smapKendang :: StrokeMap KendangTunggal.Stroke
    }
type StrokeMap stroke = Map Sollu stroke

mridangamKorvai :: [Section Mridangam.Stroke] -> Korvai
mridangamKorvai sections = Korvai
    { korvaiSections = undefined -- IMridangam $ Sections sections
    , korvaiStrokeMaps = StrokeMaps mempty mempty
        -- { smapMridangam = Right $ mempty { Realize.smapPatternMap = pmap }
    }

solluKorvai :: StrokeMaps -> [Section Sollu] -> Korvai
solluKorvai smaps sections = Korvai
    { korvaiSections = TSollu sections
    , korvaiStrokeMaps = smaps
    }

data Section a = Section {
    sectionSequence :: a
    , sectionTags :: Tags.Tags
    } deriving (Functor)

realizeSection :: ToStrokes sollu stroke -> StrokeMap stroke
    -> Section sollu -> Section [Note stroke]
realizeSection _toStrokes _smap _section = undefined

data Note stroke = Note
    deriving (Show)

data Sollu = Sollu deriving (Eq, Ord)
instance Notation Sollu
instance Notation Mridangam.Stroke
instance Notation KendangTunggal.Stroke

-- originally: Realize.realizeSollu
solluToStrokes :: StrokeMap stroke -> ToStrokes Sollu stroke
solluToStrokes = undefined

realizeStroke :: ToStrokes stroke stroke
realizeStroke = undefined

data ToStrokes sollu stroke = ToStrokes
