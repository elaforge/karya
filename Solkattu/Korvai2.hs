{-# LANGUAGE LiberalTypeSynonyms #-}
module Solkattu.Korvai2 where

import qualified Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Tags as Tags

import Global

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

-}

-- This needs to select somehow.
formatInstrument :: Instrument stroke -> Korvai -> Text
formatInstrument instrument korvai = undefined

data Korvai = Korvai {
    korvaiRealized :: [Realization]
    }

type Realization = Instruments RealizedSections

data Instruments f =
    IKonnakol (f Sollu)
    | IMridangam (f Mridangam.Stroke)
    | IKendang (f KendangTunggal.Stroke)

newtype RealizedSections stroke =
    RealizedSections [Section [Note (Realize.Stroke stroke)]]

-- mridangamKorvai :: Tala.Tala -> Realize.PatternMap Mridangam.Stroke
--     -> [Section (Realize.Stroke Mridangam.Stroke)]
--     -> KorvaiT (Realize.Stroke Mridangam.Stroke)
mridangamKorvai :: [Section (Realize.Stroke Mridangam.Stroke)] -> Korvai
mridangamKorvai = instrumentKorvai mridangam

kendang1Korvai :: [Section (Realize.Stroke KendangTunggal.Stroke)] -> Korvai
kendang1Korvai = instrumentKorvai kendang

instrumentKorvai :: Instrument stroke -> [Section (Realize.Stroke stroke)]
    -> Korvai
instrumentKorvai instrument sections = Korvai
    { korvaiRealized = [realizeInstrument smaps instrument sections]
    }
    where
    smaps = StrokeMaps mempty mempty

solluKorvai :: StrokeMaps -> [Section Sollu] -> Korvai
solluKorvai smaps sections = Korvai
    { korvaiRealized =
        [ realizeKonnakol sections
        , realizeSollu smaps mridangam sections
        , realizeSollu smaps kendang sections
        ]
    }

data Section a = Section {
    sectionSequence :: a
    , sectionTags :: Tags.Tags
    }

realizeKonnakol :: [Section Sollu] -> Realization
realizeKonnakol =
    IKonnakol . RealizedSections
        . map (realizeSection Realize.realizeSimpleStroke smap)
    where
    smap = mempty
    -- , instStrokeMap = const $ Right $
    --     mempty { Realize.smapPatternMap = Konnakol.defaultPatterns }

realizeSollu :: StrokeMaps -> Instrument stroke -> [Section Sollu]
    -> Realization
realizeSollu smaps instrument =
    instRealization instrument . RealizedSections
        . map (realizeSection toStrokes smap)
    where
    toStrokes = undefined -- Realize.realizeSollu undefined -- from smap
    smap = instStrokeMap instrument smaps

-- realizeSollu :: SolluMap stroke -> ToStrokes Solkattu.Sollu stroke

realizeInstrument :: StrokeMaps -> Instrument stroke
    -> [Section (Realize.Stroke stroke)] -> Realization
realizeInstrument smaps instrument =
    instRealization instrument . RealizedSections
        . map (realizeSection Realize.realizeStroke smap)
    where
    smap = instStrokeMap instrument smaps

-- realizeStroke :: ToStrokes (Stroke stroke) stroke

-- realizeSection :: (Ord sollu, Pretty sollu, Solkattu.Notation stroke)
--     => Realize.ToStrokes sollu stroke -> Realize.StrokeMap stroke -> Tala.Tala
--     -> Section sollu -> Either Error ([Flat stroke], [Realize.Warning])

realizeSection :: Realize.ToStrokes sollu stroke -> StrokeMap stroke
    -> Section sollu -> Section [Note (Realize.Stroke stroke)]
realizeSection toStrokes smap section = undefined

data Note stroke = Note
    deriving (Show)

data Sollu = Sollu deriving (Eq, Ord)
-- data MridangamStroke
-- data KendangStroke

data StrokeMaps = StrokeMaps {
    smapMridangam :: StrokeMap Mridangam.Stroke
    , smapKendang :: StrokeMap KendangTunggal.Stroke
    }

type StrokeMap stroke = Map Sollu stroke

data Instrument stroke = Instrument {
    instStrokeMap :: StrokeMaps -> Map Sollu stroke
    , instRealization :: RealizedSections stroke -> Realization
    }

mridangam :: Instrument Mridangam.Stroke
mridangam = Instrument
    { instStrokeMap = smapMridangam
    -- , instRealization = Mridangam
    }

kendang :: Instrument KendangTunggal.Stroke
kendang = Instrument
    { instStrokeMap = smapKendang
    -- , instRealization = Kendang
    }
