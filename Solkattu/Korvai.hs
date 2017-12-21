-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ExistentialQuantification #-}
-- | Tie together generic Solkattu and specific instruments into a single
-- 'Korvai'.
module Solkattu.Korvai where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Time.Calendar as Calendar

import qualified Util.CallStack as CallStack
import qualified Util.Map
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Derive.Expr as Expr
import qualified Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Solkattu.Instrument.Konnakol as Konnakol
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Instrument.Reyong as Reyong
import qualified Solkattu.Instrument.Sargam as Sargam
import qualified Solkattu.Instrument.ToScore as ToScore
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Sequence as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tala as Tala

import Global


type Sequence = SequenceT Solkattu.Sollu
type SequenceT sollu = [S.Note Solkattu.Group (Solkattu.Note sollu)]

type Error = Text

mapSollu :: (a -> b) -> SequenceT a -> SequenceT b
mapSollu f = map $ \n -> case n of
    S.Note note -> S.Note (f <$> note)
    S.TempoChange change notes -> S.TempoChange change (mapSollu f notes)
    S.Group g notes -> S.Group g (mapSollu f notes)

-- * korvai

data Korvai = Korvai {
    korvaiSequences :: !KorvaiType
    , korvaiStrokeMaps :: !StrokeMaps
    , korvaiTala :: !Tala.Tala
    , korvaiMetadata :: !Metadata
    } deriving (Eq, Show)

data KorvaiType =
    Sollu [SequenceT Solkattu.Sollu]
    | Mridangam [SequenceT (Realize.Stroke Mridangam.Stroke)]
    deriving (Show, Eq)

solluSequence (Sollu seq) = Just seq
solluSequence _ = Nothing

instance Pretty KorvaiType where
    pretty (Sollu a) = pretty a
    pretty (Mridangam a) = pretty a

instance Pretty Korvai where
    format (Korvai sequence strokeMaps tala metadata) = Pretty.record "Korvai"
        [ ("sequence", Pretty.format sequence)
        , ("strokeMaps", Pretty.format strokeMaps)
        , ("tala", Pretty.format tala)
        , ("metadata", Pretty.format metadata)
        ]

korvai :: Tala.Tala -> StrokeMaps -> [Sequence] -> Korvai
korvai tala strokeMaps sequences = inferMetadata $ Korvai
    { korvaiSequences = Sollu sequences
    , korvaiStrokeMaps = strokeMaps
    , korvaiTala = tala
    , korvaiMetadata = mempty
    }

mridangamKorvai :: Tala.Tala -> Realize.Patterns Mridangam.Stroke
    -> [SequenceT (Realize.Stroke Mridangam.Stroke)]
    -> Korvai
mridangamKorvai tala pmap sequences = inferMetadata $ Korvai
    { korvaiSequences = Mridangam sequences
    , korvaiStrokeMaps = mempty
        { instMridangam = Realize.Instrument
            { instStrokeMap = mempty
            , instPatterns = pmap
            }
        }
    , korvaiTala = tala
    , korvaiMetadata = mempty
    }

-- | Tie together everything describing how to realize a single instrument.
data Instrument stroke = Instrument {
    instFromSollu :: Realize.StrokeMap stroke
        -> Realize.GetStroke Solkattu.Sollu stroke
    , instFromMridangam ::
        Maybe (Realize.GetStroke (Realize.Stroke Mridangam.Stroke) stroke)
    , instFromStrokes :: StrokeMaps -> Realize.Instrument stroke
    -- | Modify strokes after 'realize'.  Use with 'strokeTechnique'.
    , instPostprocess :: [Flat stroke] -> [Flat stroke]
    , instToScore :: ToScore.ToScore stroke
    }

defaultInstrument :: Expr.ToExpr (Realize.Stroke stroke) => Instrument stroke
defaultInstrument = Instrument
    { instFromSollu = Realize.realizeSollu
    , instFromMridangam = Nothing
    , instFromStrokes = const mempty
    , instPostprocess = id
    , instToScore = ToScore.toScore
    }

mridangam :: Instrument Mridangam.Stroke
mridangam = defaultInstrument
    { instFromMridangam = Just Realize.realizeStroke
    , instPostprocess = Mridangam.postprocess
    , instFromStrokes = instMridangam
    }

konnakol :: Instrument Solkattu.Sollu
konnakol = defaultInstrument
    { instFromSollu = const Realize.realizeSimpleStroke
    , instFromStrokes = const $ Realize.Instrument
        { instStrokeMap = mempty
        -- TODO to control the patterns, I could modify
        -- konnakol.getRealization
        , instPatterns = Konnakol.defaultPatterns
        }
    }

kendangTunggal :: Instrument KendangTunggal.Stroke
kendangTunggal =
    defaultInstrument { instFromStrokes = instKendangTunggal }

reyong :: Instrument Reyong.Stroke
reyong = defaultInstrument { instFromStrokes = instReyong }

sargam :: Instrument Sargam.Stroke
sargam = defaultInstrument
    { instFromStrokes = instSargam
    , instToScore = Sargam.toScore
    }

-- | An existential type to capture the Pretty instance.
data GInstrument =
    forall stroke. Solkattu.Notation stroke => GInstrument (Instrument stroke)

instruments :: Map Text GInstrument
instruments = Map.fromList
    [ ("mridangam", GInstrument mridangam)
    , ("konnakol", GInstrument konnakol)
    , ("kendangTunggal", GInstrument kendangTunggal)
    , ("reyong", GInstrument reyong)
    , ("sargam", GInstrument sargam)
    ]

-- * realize

-- | Fully realized notes.
type Flat stroke =
    S.Flat (Realize.Group (Realize.Stroke stroke)) (Realize.Note stroke)

-- | Realize a Korvai on a particular instrument.
realize :: Solkattu.Notation stroke => Instrument stroke -> Bool -> Korvai
    -> [Either Error ([Flat stroke], Error)]
realize instrument realizePatterns korvai = case korvaiSequences korvai of
    Sollu seqs -> map (realize1 (instFromSollu instrument smap)) seqs
    Mridangam seqs -> case instFromMridangam instrument of
        Nothing -> [Left "no sequence, wrong instrument type"]
        Just realizeNote -> map (realize1 realizeNote) seqs
    where
    realize1 realizeNote =
        fmap (first (instPostprocess instrument))
        . realizeInstrument realizePatterns realizeNote inst tala
    smap = Realize.instStrokeMap inst
    tala = korvaiTala korvai
    inst = instFromStrokes instrument (korvaiStrokeMaps korvai)

realizeInstrument :: (Pretty sollu, Solkattu.Notation stroke)
    => Bool -> Realize.GetStroke sollu stroke
    -> Realize.Instrument stroke -> Tala.Tala -> SequenceT sollu
    -> Either Error ([Flat stroke], Error)
realizeInstrument realizePatterns getStroke inst tala sequence = do
    realized <- Realize.formatError $
        Realize.realize pattern getStroke $ flatten sequence
    let alignError = Realize.verifyAlignment tala $ S.tempoNotes realized
    return (realized, maybe "" (\(i, msg) -> showt i <> ": " <> msg) alignError)
    -- TODO maybe put a carat in the output where the error index is
    where
    pattern
        | realizePatterns = Realize.realizePattern (Realize.instPatterns inst)
        | otherwise = Realize.keepPattern

flatten :: [S.Note g (Solkattu.Note sollu)] -> [S.Flat g (Solkattu.Note sollu)]
flatten = Solkattu.cancelKarvai . S.flatten

-- TODO broken by KorvaiType, fix this
-- vary :: (Sequence -> [Sequence]) -> Korvai -> Korvai
-- vary modify korvai = korvai
--     { korvaiSequences = concatMap modify (korvaiSequences korvai) }

-- * Metadata

-- | Attach some metadata to a Korvai.
data Metadata = Metadata {
    _date :: !(Maybe Calendar.Day)
    -- | This is lazy because it might have a 'Solkattu.Exception' in it!
    , _tags :: Tags
    , _location :: !Location
    } deriving (Eq, Show)

-- | (module, lineNumber, variableName)
type Location = (Text, Int, Text)

instance Monoid Metadata where
    mempty = Metadata Nothing mempty ("", 0, "")
    mappend (Metadata date1 tags1 loc1@(mod1, _, _))
            (Metadata date2 tags2 loc2) =
        Metadata (date1 <|> date2) (tags1 <> tags2)
            (if Text.null mod1 then loc2 else loc1)

instance Pretty Metadata where
    format (Metadata date tags loc) = Pretty.record "Metadata"
        [ ("date", Pretty.format date)
        , ("tags", Pretty.format tags)
        , ("location", Pretty.format loc)
        ]

newtype Tags = Tags (Map Text [Text])
    deriving (Eq, Show, Pretty)

instance Monoid Tags where
    mempty = Tags mempty
    mappend (Tags t1) (Tags t2) = Tags (Util.Map.mappend t1 t2)

date :: CallStack.Stack => Int -> Int -> Int -> Calendar.Day
date y m d
    | Num.inRange 2012 2020 y && Num.inRange 1 13 m && Num.inRange 1 32 d =
        Calendar.fromGregorian (fromIntegral y) m d
    | otherwise = errorStack $ "invalid date: " <> showt (y, m, d)

-- ** infer

inferMetadata :: Korvai -> Korvai
inferMetadata korvai =
    withMetadata (mempty { _tags = inferTags korvai }) korvai

inferTags :: Korvai -> Tags
inferTags korvai = Tags $ Util.Map.multimap $ concat
    [ [("tala", Tala._name tala)]
    , map (("avartanams",) . pretty . (/aksharas)
        . Solkattu.durationOf S.defaultTempo) seqs
    , map ("nadai",) (map showt nadais)
    , map ("speed",) (map showt speeds)
    , map ("instrument",) [name | (name, True) <- instruments]
    ]
    where
    tala = korvaiTala korvai
    aksharas = fromIntegral (sum (Tala.tala_aksharas tala))

    seqs = case korvaiSequences korvai of
        Sollu seqs -> map (mapSollu (const ())) seqs
        Mridangam seqs -> map (mapSollu (const ())) seqs
    tempos = map (map fst . S.tempoNotes . flatten) seqs
    nadais = Seq.unique_sort $ concatMap (map S._nadai) tempos
    speeds = Seq.unique_sort $ concatMap (map S._speed) tempos

    instruments =
        [ ("mridangam", hasInstrument korvai instMridangam)
        , ("kendangTunggal", hasInstrument korvai instKendangTunggal)
        , ("reyong", hasInstrument korvai instReyong)
        , ("sargam", hasInstrument korvai instSargam)
        ]
    hasInstrument korvai get = get (korvaiStrokeMaps korvai) /= mempty

withMetadata :: Metadata -> Korvai -> Korvai
withMetadata meta korvai =
    korvai { korvaiMetadata = meta <> korvaiMetadata korvai }

-- * types

data StrokeMaps = StrokeMaps {
    instMridangam :: Realize.Instrument Mridangam.Stroke
    , instKendangTunggal :: Realize.Instrument KendangTunggal.Stroke
    , instReyong :: Realize.Instrument Reyong.Stroke
    , instSargam :: Realize.Instrument Sargam.Stroke
    } deriving (Eq, Show)

instance Monoid StrokeMaps where
    mempty = StrokeMaps mempty mempty mempty mempty
    mappend (StrokeMaps a1 a2 a3 a4) (StrokeMaps b1 b2 b3 b4) =
        StrokeMaps (a1<>b1) (a2<>b2) (a3<>b3) (a4<>b4)

instance Pretty StrokeMaps where
    format (StrokeMaps mridangam kendangTunggal reyong sargam) =
        Pretty.record "StrokeMaps"
            [ ("mridangam", Pretty.format mridangam)
            , ("kendangTunggal", Pretty.format kendangTunggal)
            , ("reyong", Pretty.format reyong)
            , ("sargam", Pretty.format sargam)
            ]


-- * print score

printInstrument :: Solkattu.Notation stroke => Instrument stroke -> Bool
    -> Korvai -> IO ()
printInstrument instrument realizePatterns korvai =
    printResults Nothing korvai $ realize instrument realizePatterns korvai

printKonnakol :: Bool -> Korvai -> IO ()
printKonnakol realizePatterns korvai =
    printResults (Just 4) korvai $ realize konnakol realizePatterns korvai

writeKonnakolHtml :: Bool -> Korvai -> IO ()
writeKonnakolHtml realizePatterns korvai =
    case sequence (realize konnakol realizePatterns korvai) of
        Left err -> Text.IO.putStrLn $ "ERROR:\n" <> err
        Right results
            | any (not . Text.null) warnings -> mapM_ Text.IO.putStrLn warnings
            | otherwise -> do
                Realize.writeHtml "konnakol.html" (korvaiTala korvai) notes
                putStrLn "wrote konnakol.html"
            where (notes, warnings) = unzip results

printResults :: Solkattu.Notation stroke => Maybe Int -> Korvai
    -> [Either Error ([S.Flat g (Realize.Note stroke)], Error)]
    -> IO ()
printResults overrideStrokeWidth korvai = printList . map show1
    where
    show1 (Left err) = "ERROR:\n" <> err
    show1 (Right (notes, warning)) = TextUtil.joinWith "\n"
        (Realize.format overrideStrokeWidth width tala notes)
        warning
    tala = korvaiTala korvai

width :: Int
width = 78

printList :: [Text] -> IO ()
printList [] = return ()
printList [x] = Text.IO.putStrLn x
printList xs = mapM_ print1 (zip [1..] xs)
    where
    print1 (i, x) = do
        putStrLn $ "---- " <> show i
        Text.IO.putStrLn x
