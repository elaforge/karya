module Solkattu.KorvaiRealized where

import qualified Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Tags as Tags

import Global


formatM :: Korvai -> Text
formatM = formatInstrument instMridangam

formatInstrument :: (Instruments RealizedSections -> RealizedSections stroke)
    -> Korvai -> Text
formatInstrument instrument korvai = case instrument (korvaiRealized korvai) of
    WrongInstrument -> "wrong instrument"
    RealizedSections sections -> renderNotes sections

renderNotes :: [Section [Note stroke]] -> Text
renderNotes = undefined

data Korvai = Korvai {
    korvaiRealized :: Instruments RealizedSections
    }

data Instruments f = Instruments {
    instKonnakol :: f Sollu
    , instMridangam :: f Mridangam.Stroke
    , instKendang :: f KendangTunggal.Stroke
    }

data RealizedSections stroke =
    RealizedSections [Section [Note (Realize.Stroke stroke)]]
    | WrongInstrument

type StrokeMaps = Instruments StrokeMap
type StrokeMap = Map Sollu

-- This is no good because it's only all of them for sollu, not for instrument.
-- I still want to look it up though, so maybe it's just all Either.

emptyRealized :: Instruments RealizedSections
emptyRealized = Instruments
    { instKonnakol = WrongInstrument
    , instMridangam = WrongInstrument
    , instKendang = WrongInstrument
    }

mridangamKorvai :: [Section (Realize.Stroke Mridangam.Stroke)] -> Korvai
mridangamKorvai sections = Korvai
    { korvaiRealized = emptyRealized
        { instMridangam = realizeInstrument sections }
    }
kendangKorvai :: [Section (Realize.Stroke KendangTunggal.Stroke)] -> Korvai
kendangKorvai sections = Korvai
    { korvaiRealized = emptyRealized
        { instKendang = realizeInstrument sections }
    }

realizeInstrument :: [Section (Realize.Stroke stroke)]
    -> RealizedSections stroke
realizeInstrument = RealizedSections . map (realizeSection toStrokes smap)
    where
    toStrokes = Realize.realizeStroke
    smap = undefined

solluKorvai :: StrokeMaps -> [Section Sollu] -> Korvai
solluKorvai smaps sections = Korvai
    { korvaiRealized = Instruments
        { instKonnakol = realizeKonnakol sections
        , instMridangam = realizeSollu (instMridangam smaps) sections
        , instKendang = realizeSollu (instKendang smaps) sections
        }
    }

realizeKonnakol :: [Section Sollu] -> RealizedSections Sollu
realizeKonnakol = RealizedSections . map (realizeSection toStrokes smap)
    where
    toStrokes = Realize.realizeSimpleStroke
    smap = undefined
    -- , instStrokeMap = const $ Right $
    --     mempty { Realize.smapPatternMap = Konnakol.defaultPatterns }

realizeSollu :: StrokeMap stroke -> [Section Sollu] -> RealizedSections stroke
realizeSollu smap = RealizedSections . map (realizeSection toStrokes smap)
    where toStrokes = undefined

realizeSection :: Realize.ToStrokes sollu stroke -> StrokeMap stroke
    -> Section sollu -> Section [Note (Realize.Stroke stroke)]
realizeSection toStrokes smap section = undefined

data Section a = Section {
    sectionSequence :: a
    , sectionTags :: Tags.Tags
    } deriving (Show)

data Note stroke = Note
    deriving (Show)

data Sollu = Sollu deriving (Eq, Ord)
