{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
module Solkattu.Korvai3 where

import qualified Data.Text as Text

import qualified Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Tags as Tags

import           Global

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

writeAll :: FilePath -> Korvai stroke -> IO ()
writeAll = undefined

data GKorvai = forall stroke. Notation stroke => GKorvai {
    gKorvai :: Korvai ()
    , gSollu :: Maybe (Instrument stroke -> Text)
    , gInstrument :: Instrument stroke -> Text
    }

solluKorvai :: Korvai Sollu -> GKorvai
solluKorvai k = GKorvai
    { gKorvai = const () <$> k
    , gSollu = Just $ \inst -> formatSollu inst k
    , gInstrument = \inst -> formatInstrument inst k
    }

instrumentKorvai :: Notation stroke => Korvai stroke -> GKorvai
instrumentKorvai k = GKorvai
    { gKorvai = const () <$> k
    , gSollu = Nothing
    , gInstrument = \inst -> formatInstrument inst k
    }

korvaiM :: Korvai Mridangam.Stroke
korvaiM = undefined

-- korvaiM_ = GKorvai korvaiM (flip formatSollu

{-
    If stroke ~ Sollu, then they don't have to match, and I use realizeSollu.
    Otherwise, use realizeInstrument.
    Maybe have a class that maps (sollu, stroke) -> Maybe Stroke?
    That's what I had in instrument, but it needs a

    realizeInstrument vs. realizeSollu is the thing that varies.
    Can I put it in Instrument?
-}
formatInstrument :: Notation stroke => Instrument stroke -> Korvai stroke
    -> Text
formatInstrument instrument korvai =
    Text.unlines $ map formatResults $
        map (realizeSection realizeStroke smap) $
        korvaiSections korvai
    where
    smap = instStrokeMap instrument (korvaiStrokeMaps korvai)

formatSollu :: Notation stroke => Instrument stroke -> Korvai Sollu -> Text
formatSollu instrument korvai =
    Text.unlines $ map formatResults $
        map (realizeSection (solluToStrokes smap) smap) $
        korvaiSections korvai
    where
    smap = instStrokeMap instrument (korvaiStrokeMaps korvai)

formatResults :: Notation stroke => Section [Note stroke] -> Text
formatResults = undefined

data Korvai stroke = Korvai {
    korvaiSections :: [Section stroke]
    , korvaiStrokeMaps :: StrokeMaps
    } deriving (Functor)

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

data StrokeMaps = StrokeMaps {
    smapMridangam :: StrokeMap Mridangam.Stroke
    , smapKendang :: StrokeMap KendangTunggal.Stroke
    }

type StrokeMap stroke = Map Sollu stroke

data Instrument stroke = Instrument {
    instStrokeMap :: StrokeMaps -> Map Sollu stroke
    , instRealizeSollu :: StrokeMaps -> Section Sollu
        -> Section [Note stroke]
    , instRealize :: StrokeMaps -> Section stroke -> Section [Note stroke]
    }

mridangam :: Instrument Mridangam.Stroke
mridangam = Instrument
    { instStrokeMap = smap
    , instRealizeSollu = \smaps ->
        realizeSection (solluToStrokes (smap smaps)) (smap smaps)
    , instRealize = \smaps -> realizeSection realizeStroke (smap smaps)
    }
    where
    smap = smapMridangam

kendang :: Instrument KendangTunggal.Stroke
kendang = Instrument
    { instStrokeMap = smap
    , instRealizeSollu = \smaps ->
        realizeSection (solluToStrokes (smap smaps)) (smap smaps)
    , instRealize = \smaps -> realizeSection realizeStroke (smap smaps)
    }
    where
    smap = smapKendang

-- originally: Realize.realizeSollu
solluToStrokes :: StrokeMap stroke -> ToStrokes Sollu stroke
solluToStrokes = undefined

realizeStroke :: ToStrokes stroke stroke
realizeStroke = undefined

data ToStrokes sollu stroke = ToStrokes
