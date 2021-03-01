-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | Tie together generic Solkattu and specific instruments into a single
-- 'Korvai'.
module Solkattu.Korvai where
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time.Calendar as Calendar

import qualified GHC.Generics as Generics

import qualified Util.Maps as Maps
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Styled as Styled

import qualified Derive.Expr as Expr
import qualified Solkattu.Instrument.KendangPasang as KendangPasang
import qualified Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Solkattu.Instrument.Konnakol as Konnakol
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Instrument.Reyong as Reyong
import qualified Solkattu.Instrument.Sargam as Sargam
import qualified Solkattu.Instrument.ToScore as ToScore
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tags as Tags
import qualified Solkattu.Tala as Tala

import           Global


type Sequence = SequenceT Solkattu.Sollu
type SequenceT sollu = [S.Note Solkattu.Group (Solkattu.Note sollu)]

type Error = Text

mapSollu :: (a -> b) -> SequenceT a -> SequenceT b
mapSollu f = map $ \case
    S.Note note -> S.Note (f <$> note)
    S.TempoChange change notes -> S.TempoChange change (mapSollu f notes)
    S.Group g notes -> S.Group g (mapSollu f notes)

-- * Score

data Score = Single !Korvai | Tani !Metadata ![Part Korvai]
    deriving (Show)
data Part k = K !k | Comment !Text
    deriving (Show, Functor)

tani :: [Part Korvai] -> Score
tani = Tani (mempty { _tags = Tags.withType "tani" })

mapScore :: (Korvai -> Korvai) -> Score -> Score
mapScore f = \case
    Single k -> Single (f k)
    Tani meta parts -> Tani meta (fmap (fmap f) parts)

scoreKorvais :: Score -> [Korvai]
scoreKorvais (Single k) = [k]
scoreKorvais (Tani _ parts) = [k | K k <- parts]

realizeScore :: (Korvai -> IO ()) -> Score -> IO ()
realizeScore realize = \case
    Single k -> realize k
    Tani _ parts -> forM_ parts $ \case
        -- TODO it would be nice to print the name, but it's only available
        -- through Db.  HasCallStack could get it... but I'd have to put it on
        -- every declaration.
        K korvai -> realize korvai
        Comment comment -> Styled.printLn $ Styled.bold comment

-- * korvai

data Korvai = Korvai {
    korvaiSections :: !KorvaiType
    , korvaiStrokeMaps :: !StrokeMaps
    , korvaiTala :: !Tala.Tala
    , korvaiMetadata :: !Metadata
    } deriving (Eq, Show, Generics.Generic)

-- | This is a hack so I can have both Solkattu.Sollu sequences and
-- instrument specific ones.  It induces a similar hack in 'Instrument',
-- 'instFromMridangam'.
--
-- This is really clumsy and doesn't scale, but I tried for weeks and came up
-- with 4 or 5 different approaches and none of them worked.
data KorvaiType =
    TSollu [Section (SequenceT Solkattu.Sollu)]
    | TMridangam [Section (SequenceT (Realize.Stroke Mridangam.Stroke))]
    | TKendangTunggal
        [Section (SequenceT (Realize.Stroke KendangTunggal.Stroke))]
    deriving (Show, Eq)

instance Pretty KorvaiType where
    pretty (TSollu a) = pretty a
    pretty (TMridangam a) = pretty a
    pretty (TKendangTunggal a) = pretty a

instance Pretty Korvai where
    format = Pretty.formatGCamel

korvai :: Tala.Tala -> StrokeMaps -> [Section (SequenceT Solkattu.Sollu)]
    -> Korvai
korvai tala strokeMaps sections = Korvai
    { korvaiSections = TSollu sections
    , korvaiStrokeMaps = strokeMaps
    , korvaiTala = tala
    , korvaiMetadata = mempty
    }

korvaiInstruments :: Korvai -> [(Text, GInstrument)]
korvaiInstruments korvai = filter (hasInstrument . snd) $ Map.toList instruments
    where
    hasInstrument (GInstrument inst) = case korvaiSections korvai of
        TSollu {} -> not (isEmpty inst)
        TMridangam {} -> Maybe.isJust (instFromMridangam inst)
        TKendangTunggal {} -> Maybe.isJust (instFromKendangTunggal inst)
    -- If the stroke map is broken, that at least means there is one.
    isEmpty inst = either (const False) Realize.isInstrumentEmpty $
        instStrokeMap inst (korvaiStrokeMaps korvai)

mridangamKorvai :: Tala.Tala -> Realize.PatternMap Mridangam.Stroke
    -> [Section (SequenceT (Realize.Stroke Mridangam.Stroke))] -> Korvai
mridangamKorvai tala pmap sections = Korvai
    { korvaiSections = TMridangam sections
    , korvaiStrokeMaps = mempty
        { smapMridangam = Right $ mempty { Realize.smapPatternMap = pmap }
        }
    , korvaiTala = tala
    , korvaiMetadata = mempty
    }

kendangTunggalKorvai :: Tala.Tala -> Realize.PatternMap KendangTunggal.Stroke
    -> [Section (SequenceT (Realize.Stroke KendangTunggal.Stroke))] -> Korvai
kendangTunggalKorvai tala pmap sections = Korvai
    { korvaiSections = TKendangTunggal sections
    , korvaiStrokeMaps = mempty
        { smapKendangTunggal = Right $ mempty { Realize.smapPatternMap = pmap }
        }
    , korvaiTala = tala
    , korvaiMetadata = mempty
    }

-- | Modify the korvai to extract a single Section.
index :: Int -> Korvai -> Korvai
index i = sliceSections i (i+1)

sliceSections :: Int -> Int -> Korvai -> Korvai
sliceSections start end korvai = case korvaiSections korvai of
    TSollu s -> korvai { korvaiSections = TSollu (get s) }
    TMridangam s -> korvai { korvaiSections = TMridangam (get s) }
    TKendangTunggal s -> korvai { korvaiSections = TKendangTunggal (get s) }
    where
    get xs
        | all (Num.inRange 0 (length xs)) [start, end] =
            take (start - end) $ drop start xs
        | otherwise = error $ "(start, end) " <> show (start, end)
            <> " out of range 0--" <> show (length xs)

-- * Section

data Section a = Section {
    sectionSequence :: a
    -- | Where the section should start.  0 means start on sam.
    , sectionStart :: !S.Duration
    -- | Expect the section to end at this time.  It can be negative, in which
    -- case it falls before sam.  Useful for eddupu.
    , sectionEnd :: !S.Duration
    -- | This is lazy because it might have a 'Solkattu.Exception' in it.  This
    -- is because 'inferSectionTags' has to evaluate the sequence.
    , sectionTags :: Tags.Tags
    } deriving (Eq, Show, Functor, Generics.Generic)

instance Pretty a => Pretty (Section a) where
    format = Pretty.formatGCamel

scoreSections :: Score -> [Section ()]
scoreSections = \case
    Single k -> genericSections k
    Tani _ parts -> flip concatMap parts $ \case
        K korvai -> genericSections korvai
        _ -> []

genericSections :: Korvai -> [Section ()]
genericSections korvai = case korvaiSections korvai of
    TSollu sections -> map (fmap (const ())) sections
    TMridangam sections -> map (fmap (const ())) sections
    TKendangTunggal sections -> map (fmap (const ())) sections

modifySections :: (Tags.Tags -> Tags.Tags) -> Korvai -> Korvai
modifySections modify korvai = korvai
    { korvaiSections = case korvaiSections korvai of
        TSollu sections -> TSollu $ map (modifySectionTags modify) sections
        TMridangam sections ->
            TMridangam $ map (modifySectionTags modify) sections
        TKendangTunggal sections ->
            TKendangTunggal $ map (modifySectionTags modify) sections
    }

addSectionTags :: Tags.Tags -> Section a -> Section a
addSectionTags tags = modifySectionTags (tags<>)

modifySectionTags :: (Tags.Tags -> Tags.Tags) -> Section a -> Section a
modifySectionTags modify section =
    section { sectionTags = modify (sectionTags section) }

section :: a -> Section a
section seq = Section
    { sectionSequence = seq
    , sectionStart = 0
    , sectionEnd = 0
    , sectionTags = mempty
    }

inferSections :: [SequenceT sollu] -> [Section (SequenceT sollu)]
inferSections seqs = case Seq.viewr (map section seqs) of
    Just (inits, last) ->
        map (addSectionTags (Tags.withType Tags.development)) inits
        ++ [addSectionTags (Tags.withType Tags.ending) last]
    Nothing -> []

-- * Instrument

-- | Tie together everything describing how to realize a single instrument.
data Instrument stroke = Instrument {
    instName :: Text
    -- | Realize a 'Sollu' 'KorvaiType'.
    , instFromSollu :: Realize.SolluMap stroke
        -> Realize.ToStrokes Solkattu.Sollu stroke
    -- | Realize a 'Mridangam' 'KorvaiType'.
    , instFromMridangam ::
        Maybe (Realize.ToStrokes (Realize.Stroke Mridangam.Stroke) stroke)
    , instFromKendangTunggal ::
        Maybe (Realize.ToStrokes (Realize.Stroke KendangTunggal.Stroke) stroke)
    -- | This can be a Left because it comes from one of the
    -- instrument-specific 'StrokeMaps' fields, which can be Left if
    -- 'Realize.strokeMap' verification failed.
    , instStrokeMap :: StrokeMaps -> Either Error (Realize.StrokeMap stroke)
    -- | Modify strokes after 'realize'.  Use with 'strokeTechnique'.
    , instPostprocess :: [Flat stroke] -> [Flat stroke]
    , instToScore :: ToScore.ToScore stroke
    }

defaultInstrument :: (Expr.ToExpr (Realize.Stroke stroke)) => Instrument stroke
defaultInstrument = Instrument
    { instName = ""
    , instFromSollu = Realize.realizeSollu
    , instFromMridangam = Nothing
    , instFromKendangTunggal = Nothing
    , instStrokeMap = const $ Right mempty
    , instPostprocess = id
    , instToScore = ToScore.toScore
    }

mridangam :: Instrument Mridangam.Stroke
mridangam = defaultInstrument
    { instName = "mridangam"
    , instFromMridangam = Just Realize.realizeStroke
    , instPostprocess = Mridangam.postprocess
    , instStrokeMap = smapMridangam
    }

konnakol :: Instrument Solkattu.Sollu
konnakol = defaultInstrument
    { instName = "konnakol"
    , instFromSollu = const Realize.realizeSimpleStroke
    , instStrokeMap = const $ Right $
        mempty { Realize.smapPatternMap = Konnakol.defaultPatterns }
    }

kendangTunggal :: Instrument KendangTunggal.Stroke
kendangTunggal = defaultInstrument
    { instName = "kendang tunggal"
    , instFromKendangTunggal = Just Realize.realizeStroke
    , instStrokeMap = smapKendangTunggal
    }

kendangPasang :: Instrument KendangPasang.Stroke
kendangPasang = defaultInstrument
    { instName = "kendang pasang"
    , instStrokeMap = smapKendangPasang
    }

reyong :: Instrument Reyong.Stroke
reyong = defaultInstrument
    { instName = "reyong"
    , instStrokeMap = smapReyong
    }

sargam :: Instrument Sargam.Stroke
sargam = defaultInstrument
    { instName = "sargam"
    , instStrokeMap = smapSargam
    , instToScore = Sargam.toScore
    }

-- | An existential type to capture the Notation instance.
data GInstrument =
    forall stroke. Solkattu.Notation stroke => GInstrument (Instrument stroke)

instruments :: Map Text GInstrument
instruments = Map.fromList $ Seq.key_on nameOf
    [ GInstrument mridangam
    , GInstrument konnakol
    , GInstrument kendangTunggal
    , GInstrument reyong
    , GInstrument sargam
    ]
    where nameOf (GInstrument inst) = instName inst


-- * realize

-- | Fully realized notes.
type Flat stroke =
    S.Flat (Realize.Group (Realize.Stroke stroke)) (Realize.Note stroke)

-- | Realize a Korvai on a particular instrument.
realize :: Solkattu.Notation stroke => Instrument stroke -> Korvai
    -> [Either Error ([Flat stroke], [Realize.Warning])]
realize instrument korvai =
    case instStrokeMap instrument (korvaiStrokeMaps korvai) of
        Left err -> [Left err]
        Right smap -> case korvaiSections korvai of
            TSollu sections -> map (realize1 toStrokes) sections
                where
                toStrokes = instFromSollu instrument (Realize.smapSolluMap smap)
            TMridangam sections -> case instFromMridangam instrument of
                Nothing -> [Left "no sequence, wrong instrument type"]
                Just toStrokes -> map (realize1 toStrokes) sections
            TKendangTunggal sections ->
                case instFromKendangTunggal instrument of
                    Nothing -> [Left "no sequence, wrong instrument type"]
                    Just toStrokes -> map (realize1 toStrokes) sections
            where
            realize1 toStrokes = fmap (first (instPostprocess instrument))
                . realizeSection toStrokes smap (korvaiTala korvai)

setSection :: Section x -> a -> Section a
setSection section a = const a <$> section

realizeSection :: (Ord sollu, Pretty sollu, Solkattu.Notation stroke)
    => Realize.ToStrokes sollu stroke -> Realize.StrokeMap stroke -> Tala.Tala
    -> Section (SequenceT sollu)
    -> Either Error ([Flat stroke], [Realize.Warning])
realizeSection toStrokes smap tala section = do
    realized <- Realize.formatError $ fst $
        Realize.realize smap toStrokes tala $ flatten $
        sectionSequence section
    let alignWarn = Realize.checkAlignment tala
            (sectionStart section) (sectionEnd section)
            (S.tempoNotes realized)
    (realized, durationWarns) <- return $ Realize.checkDuration realized
    startSpace <- spaces (inferNadai realized) (sectionStart section)
    return (startSpace ++ realized, maybe [] (:[]) alignWarn ++ durationWarns)

allMatchedSollus :: Instrument stroke -> Korvai
    -> Set (Realize.SolluMapKey Solkattu.Sollu)
allMatchedSollus instrument korvai = case korvaiSections korvai of
    TSollu sections -> mconcatMap
        (matchedSollus (instFromSollu instrument solluMap) (korvaiTala korvai))
        sections
    _ -> mempty
    where
    solluMap = either mempty Realize.smapSolluMap smap
    smap = instStrokeMap instrument (korvaiStrokeMaps korvai)

matchedSollus :: (Pretty sollu, Ord sollu)
    => Realize.ToStrokes sollu stroke -> Tala.Tala -> Section (SequenceT sollu)
    -> Set (Realize.SolluMapKey sollu)
matchedSollus toStrokes tala =
    snd . Realize.realize_ dummyPattern toStrokes tala . flatten
        . sectionSequence
    where
    -- Since I'm just looking for used sollus, I can just map all patterns to
    -- rests.  I probably don't have to bother to get the duration right, but
    -- why not.
    dummyPattern tempo (Solkattu.PatternM p) =
        Right $ replicate p (tempo, Realize.Space Solkattu.Rest)

inferNadai :: [Flat stroke] -> S.Nadai
inferNadai = S._nadai . maybe S.defaultTempo fst . Seq.head . S.tempoNotes

flatten :: [S.Note g (Solkattu.Note sollu)] -> [S.Flat g (Solkattu.Note sollu)]
flatten = Solkattu.cancelKarvai . S.flatten

-- | Generate enough 'Solkattu.Offset' spaces to align the score to the given
-- start Duration.
spaces :: S.Nadai -> S.Duration -> Either Error [S.Flat g (Realize.Note sollu)]
spaces nadai dur = do
    -- Cancel out the nadai.  So dur is now in s0 matras.
    let s0_matras = realToFrac dur * fromIntegral nadai
    speeds <- S.decomposeM s0_matras
    return $ map (\s -> S.FNote (speed s) space) speeds
    where
    space = Realize.Space Solkattu.Offset
    speed s = S.defaultTempo { S._speed = s, S._nadai = nadai }

-- * transform

-- TODO broken by KorvaiType, fix this
-- vary :: (Sequence -> [Sequence]) -> Korvai -> Korvai
-- vary modify korvai = korvai
--     { korvaiSections = concatMap modify (korvaiSections korvai) }

mapStrokeRest :: (Realize.Stroke a -> Maybe (Realize.Stroke b))
    -> [S.Flat g (Realize.Note a)] -> [S.Flat g (Realize.Note b)]
mapStrokeRest f = map convert
    where
    convert = \case
        S.FNote tempo a -> S.FNote tempo $
            fromMaybe (Realize.Space Solkattu.Rest) (Realize.mapStroke f a)
        S.FGroup tempo group notes ->
            S.FGroup tempo group (map convert notes)

-- * lint

-- | Show the shadowed strokes, except an ok set.  It's ok to shadow the
-- builtins.
lint :: Pretty stroke => Instrument stroke -> [Sequence] -> Korvai -> Text
lint inst defaultStrokes korvai =
    either (("stroke map: "<>) . (<>"\n")) lintSmap $
    instStrokeMap inst $ korvaiStrokeMaps korvai
    where
    lintSmap smap = Text.unlines $ filter (not . Text.null)
        [ if null shadowed then ""
            else Text.intercalate "\n" $ "shadowed:" : map prettyPair shadowed
        , if Set.null unmatched then ""
            else Text.intercalate "\n" $ "unmatched:"
                : map Realize.prettyKey (Set.toList unmatched)
        ]
        where
        shadowed = filter ((`Set.notMember` defaultKeys) . fst) $
            Realize.smapSolluShadows smap
        prettyPair (key, strokes) =
            Realize.prettyKey key <> ": " <> pretty strokes
        matched = allMatchedSollus inst korvai
        unmatched = Realize.smapKeys smap
            `Set.difference` matched
            `Set.difference` defaultKeys
    defaultKeys = Set.fromList $ map (second Maybe.catMaybes) $
        Either.rights $ map Realize.verifySolluKey defaultStrokes

-- * Metadata

-- | Attach some metadata to a Korvai.
data Metadata = Metadata {
    _date :: !(Maybe Calendar.Day)
    , _tags :: !Tags.Tags
    , _location :: !Location
    } deriving (Eq, Show, Generics.Generic)

-- | (module, lineNumber, variableName)
type Location = (Text, Int, Text)

instance Semigroup Metadata where
    (<>)    (Metadata date1 tags1 loc1@(mod1, _, _))
            (Metadata date2 tags2 loc2) =
        Metadata (date1 <|> date2) (tags1 <> tags2)
            (if Text.null mod1 then loc2 else loc1)
instance Monoid Metadata where
    mempty = Metadata Nothing mempty ("", 0, "")
    mappend = (<>)

instance Pretty Metadata where
    format = Pretty.formatG_

withKorvaiMetadata :: Metadata -> Korvai -> Korvai
withKorvaiMetadata meta korvai =
    korvai { korvaiMetadata = meta <> korvaiMetadata korvai }

modifyMetadata :: (Metadata -> Metadata) -> Score -> Score
modifyMetadata modify = \case
    Single k -> Single $ k { korvaiMetadata = modify (korvaiMetadata k) }
    Tani meta parts -> Tani (modify meta) parts

scoreMetadata :: Score -> Metadata
scoreMetadata = \case
    Single k -> korvaiMetadata k
    Tani meta _ -> meta

setLocation :: Location -> Score -> Score
setLocation loc = modifyMetadata $ \meta -> meta { _location = loc }

-- ** infer

inferMetadataS :: Score -> Score
inferMetadataS = mapScore inferMetadata

-- | This is called in "Solkattu.All", thanks to "Solkattu.ExtractKorvais".
--
-- It used to be called in the 'korvai' and 'mridangamKorvai' constructors, but
-- it was confusing how it wouldn't see modifications done after construction.
inferMetadata :: Korvai -> Korvai
inferMetadata = inferSections . inferKorvaiMetadata
    where
    inferSections korvai = case korvaiSections korvai of
        TSollu sections -> korvai
            { korvaiSections =
                TSollu $ map (addTags (korvaiTala korvai)) sections
            }
        TMridangam sections -> korvai
            { korvaiSections =
                TMridangam $ map (addTags (korvaiTala korvai)) sections
            }
        TKendangTunggal sections -> korvai
            { korvaiSections =
                TKendangTunggal $ map (addTags (korvaiTala korvai)) sections
            }
    addTags tala section =
        addSectionTags (inferSectionTags tala section) section

inferKorvaiMetadata :: Korvai -> Korvai
inferKorvaiMetadata korvai =
    withKorvaiMetadata (mempty { _tags = inferKorvaiTags korvai }) korvai

inferKorvaiTags :: Korvai -> Tags.Tags
inferKorvaiTags korvai = Tags.Tags $ Maps.multimap $ concat
    [ [ ("tala", Tala._name tala)
      , ("sections", showt sections)
      ]
    , map ("instrument",) instruments
    -- Default type=korvai if not given explicitly.
    , [ (Tags.type_, "korvai")
      | not $ Map.member Tags.type_ $
        Tags.untags (_tags (korvaiMetadata korvai))
      ]
    ]
    where
    tala = korvaiTala korvai
    sections = case korvaiSections korvai of
        TSollu xs -> length xs
        TMridangam xs -> length xs
        TKendangTunggal xs -> length xs
    instruments = map fst $ korvaiInstruments korvai

inferSectionTags :: Tala.Tala -> Section (SequenceT sollu) -> Tags.Tags
inferSectionTags tala section = Tags.Tags $ Map.fromList $
    [ ("avartanams", [pretty $ dur / talaAksharas])
    , ("nadai", map pretty nadais)
    , ("max_speed", [pretty $ maximum (0 : speeds)])
    , ("start", [pretty $ sectionStart section])
    , ("end", [pretty $ sectionEnd section])
    ]
    where
    seq = mapSollu (const ()) (sectionSequence section)
    talaAksharas = fromIntegral (Tala.tala_aksharas tala)
    dur = Solkattu.durationOf S.defaultTempo seq
    tempos = map fst $ S.tempoNotes $ flatten seq
    nadais = Seq.unique_sort $ map S._nadai tempos
    speeds = Seq.unique_sort $ map S._speed tempos


-- * types

data StrokeMaps = StrokeMaps {
    smapMridangam :: Either Error (Realize.StrokeMap Mridangam.Stroke)
    , smapKendangTunggal ::
        Either Error (Realize.StrokeMap KendangTunggal.Stroke)
    , smapKendangPasang ::
        Either Error (Realize.StrokeMap KendangPasang.Stroke)
    , smapReyong :: Either Error (Realize.StrokeMap Reyong.Stroke)
    , smapSargam :: Either Error (Realize.StrokeMap Sargam.Stroke)
    } deriving (Eq, Show, Generics.Generic)

instance Semigroup StrokeMaps where
    StrokeMaps a1 a2 a3 a4 a5 <> StrokeMaps b1 b2 b3 b4 b5 =
        StrokeMaps (merge a1 b1) (merge a2 b2) (merge a3 b3) (merge a4 b4)
            (merge a5 b5)
        where
        merge (Left err) _ = Left err
        merge _ (Left err) = Left err
        merge (Right a) (Right b) = Right (a<>b)

instance Monoid StrokeMaps where
    mempty = StrokeMaps
        (Right mempty) (Right mempty) (Right mempty) (Right mempty)
        (Right mempty)
    mappend = (<>)

instance Pretty StrokeMaps where
    format = Pretty.formatGCamel
