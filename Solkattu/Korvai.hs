-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
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

import qualified Util.Lists as Lists
import qualified Util.Maps as Maps
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Styled as Styled

import qualified Derive.Expr as Expr
import qualified Solkattu.Bol as Bol
import qualified Solkattu.Instrument.KendangPasang as KendangPasang
import qualified Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Solkattu.Instrument.Konnakol as Konnakol
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Instrument.Reyong as Reyong
import qualified Solkattu.Instrument.Sargam as Sargam
import qualified Solkattu.Instrument.Tabla as Tabla
import qualified Solkattu.Instrument.ToScore as ToScore
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tags as Tags
import qualified Solkattu.Tala as Tala
import qualified Solkattu.Talas as Talas

import           Global


type Sequence = SequenceT Solkattu.Sollu
type SequenceT sollu = SequenceG Solkattu.Group sollu
type SequenceG g sollu = S.Sequence g (Solkattu.Note sollu)

type Error = Text

mapSollu :: (a -> b) -> S.Sequence g (Solkattu.Note a)
    -> S.Sequence g (Solkattu.Note b)
mapSollu f = S.mapS $ \case
    S.Note note -> S.Note (f <$> note)
    S.TempoChange change notes -> S.TempoChange change (mapS notes)
    S.Group g notes -> S.Group g (mapS notes)
    where
    mapS = S.toList . mapSollu f . S.fromList

-- * Score

data Score = Single !Korvai | Tani !Metadata ![Part Korvai]
    deriving (Show)
data Part k = K !k | Comment !Text
    deriving (Show, Functor)

-- | Make a Tani Score, which is just a sequence of Korvais.
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
    korvaiSections :: !KorvaiSections
    , korvaiStrokeMaps :: !StrokeMaps
    , korvaiTala :: !Talas.Tala
    , korvaiMetadata :: !Metadata
    } deriving (Show, Generics.Generic)

instance Pretty Korvai where
    format = Pretty.formatGCamel

korvai :: Tala.Tala -> StrokeMaps -> [Section (SequenceT Solkattu.Sollu)]
    -> Korvai
korvai tala strokeMaps sections = Korvai
    { korvaiSections = KorvaiSections IKonnakol
        (fmap (fmap (fmap (fmap Realize.stroke))) sections)
        -- Wrap up in dummy Realize.Strokes, see 'Realize.realizeSollu'.
    , korvaiStrokeMaps = strokeMaps
    , korvaiTala = Talas.Carnatic tala
    , korvaiMetadata = mempty
    }

korvaiInstruments :: Korvai -> [GInstrument]
korvaiInstruments korvai = filter hasInstrument instruments
    where
    hasInstrument (GInstrument inst) =
        case getSections inst (korvaiSections korvai) of
            Just _ -> True
            Nothing -> case korvaiSections korvai of
                KorvaiSections IKonnakol _ -> not (isEmpty inst)
                _ -> False
    -- Even if the stroke map is broken, at least there is one.
    isEmpty :: Instrument stroke -> Bool
    isEmpty inst = either (const False) Realize.isInstrumentEmpty $
        getStrokeMap inst (korvaiStrokeMaps korvai)

mridangamKorvai :: Tala.Tala -> Realize.PatternMap Mridangam.Stroke
    -> [Section (SequenceT (Realize.Stroke Mridangam.Stroke))] -> Korvai
mridangamKorvai = instrumentKorvai IMridangam

kendangTunggalKorvai :: Tala.Tala -> Realize.PatternMap KendangTunggal.Stroke
    -> [Section (SequenceT (Realize.Stroke KendangTunggal.Stroke))] -> Korvai
kendangTunggalKorvai = instrumentKorvai IKendangTunggal

instrumentKorvai :: Instrument stroke -> Tala.Tala
    -> Realize.PatternMap stroke
    -> [Section (SequenceT (Realize.Stroke stroke))]
    -> Korvai
instrumentKorvai inst tala pmap sections = Korvai
    { korvaiSections = KorvaiSections inst sections
    , korvaiStrokeMaps =
        setStrokeMap inst $ Right $ mempty { Realize.smapPatternMap = pmap }
    , korvaiTala = Talas.Carnatic tala
    , korvaiMetadata = mempty
    }

tablaKorvai :: Talas.Tal
    -> [Section (SequenceT (Realize.Stroke Bol.Bol))] -> Korvai
tablaKorvai tala sections = Korvai
    { korvaiSections = KorvaiSections IBol sections
    , korvaiStrokeMaps = mempty
    , korvaiTala = Talas.Hindustani tala
    , korvaiMetadata = mempty
    }

-- | Modify the korvai to extract a single Section.
index :: Int -> Korvai -> Korvai
index i = slice i (i+1)

slice :: Int -> Int -> Korvai -> Korvai
slice start end korvai = case korvaiSections korvai of
    KorvaiSections inst sections -> korvai
        { korvaiSections = KorvaiSections inst $
            get (if end < 0 then length sections + end else end) sections
        }
    where
    get :: Int -> [a] -> [a]
    get end xs
        | all (Num.inRange 0 (length xs + 1)) [start, end] =
            take (end - start) $ drop start xs
        | otherwise = error $ "(start, end) " <> show (start, end)
            <> " out of range 0--" <> show (length xs)

-- ** Instrument

-- This seems like a dependent pair, Instrument lets me know what stroke is.
data KorvaiSections = forall stroke.
    KorvaiSections (Instrument stroke) (Sections stroke)

instance Show KorvaiSections where
    show (KorvaiSections inst _) = untxt $ instrumentName inst
instance Pretty KorvaiSections where
    pretty (KorvaiSections inst _) = instrumentName inst

type Sections stroke = [Section (SequenceT (Realize.Stroke stroke))]

-- TODO: can I use Data.Type.Equality :~: ?
getSections :: Instrument stroke -> KorvaiSections -> Maybe (Sections stroke)
getSections inst ktype = case (inst, ktype) of
    (IKonnakol, KorvaiSections IKonnakol sections) -> Just sections
    (IMridangam, KorvaiSections IMridangam sections) -> Just sections
    (IKendangTunggal, KorvaiSections IKendangTunggal sections) -> Just sections
    (IKendangPasang, KorvaiSections IKendangPasang sections) -> Just sections
    (IReyong, KorvaiSections IReyong sections) -> Just sections
    (ISargam, KorvaiSections ISargam sections) -> Just sections
    (IBol, KorvaiSections IBol sections) -> Just sections
    (ITabla, KorvaiSections ITabla sections) -> Just sections
    _ -> Nothing

data GInstrument = forall stroke.
    (Solkattu.Notation stroke, Ord stroke, Expr.ToExpr (Realize.Stroke stroke))
    => GInstrument (Instrument stroke)

instruments :: [GInstrument]
instruments =
    [ GInstrument IKonnakol
    , GInstrument IMridangam
    , GInstrument IKendangTunggal
    , GInstrument IReyong
    , GInstrument ISargam
    ]

ginstrumentName :: GInstrument -> Text
ginstrumentName (GInstrument inst) = instrumentName inst

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
    KorvaiSections _ sections -> map (fmap (const ())) sections

modifySections :: (Tags.Tags -> Tags.Tags) -> Korvai -> Korvai
modifySections modify korvai = korvai
    { korvaiSections = case korvaiSections korvai of
        KorvaiSections inst sections ->
            KorvaiSections inst $ map (modifySectionTags modify) sections
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
inferSections seqs = case Lists.unsnoc (map section seqs) of
    Just (inits, last) ->
        map (addSectionTags (Tags.withType Tags.development)) inits
        ++ [addSectionTags (Tags.withType Tags.ending) last]
    Nothing -> []

-- * Instrument

-- | Each instrument is matched up with a stroke type.
data Instrument stroke where
    IKonnakol :: Instrument Solkattu.Sollu
    IMridangam :: Instrument Mridangam.Stroke
    IKendangTunggal :: Instrument KendangTunggal.Stroke
    IKendangPasang :: Instrument KendangPasang.Stroke
    IReyong :: Instrument Reyong.Stroke
    ISargam :: Instrument Sargam.Stroke
    IBol :: Instrument Bol.Bol
    ITabla :: Instrument Tabla.Stroke

instrumentName :: Instrument stroke -> Text
instrumentName = \case
    IKonnakol -> "konnakol"
    IMridangam -> "mridangam"
    IKendangTunggal -> "kendang tunggal"
    IKendangPasang -> "kendang pasang"
    IReyong -> "reyong"
    ISargam -> "sargam"
    IBol -> "bol"
    ITabla -> "tabla"

getStrokeMap :: Instrument stroke -> StrokeMaps
    -> StrokeMap Solkattu.Sollu stroke
getStrokeMap inst smap = case inst of
    IKonnakol -> Right $ mempty
        { Realize.smapPatternMap = Konnakol.defaultPatterns }
    IMridangam -> smapMridangam smap
    IKendangTunggal -> smapKendangTunggal smap
    IKendangPasang -> smapKendangPasang smap
    IReyong -> smapReyong smap
    ISargam -> smapSargam smap
    IBol -> Right mempty -- like IKonnakol except no patterns
    ITabla -> Left "tabla should have had a hardcoded stroke map"

setStrokeMap :: Instrument stroke -> StrokeMap Solkattu.Sollu stroke
    -> StrokeMaps
setStrokeMap inst smap = case inst of
    IKonnakol -> mempty
    IMridangam -> mempty { smapMridangam = smap }
    IKendangTunggal -> mempty { smapKendangTunggal = smap }
    IKendangPasang -> mempty { smapKendangPasang = smap }
    IReyong -> mempty { smapReyong = smap }
    ISargam -> mempty { smapSargam = smap }
    IBol -> mempty
    ITabla -> mempty

instPostprocess :: Instrument stroke -> [Flat stroke] -> [Flat stroke]
instPostprocess = \case
    IMridangam -> Mridangam.postprocess
    _ -> id

instToScore :: Expr.ToExpr (Realize.Stroke stroke) => Instrument stroke
    -> ToScore.ToScore stroke
instToScore = \case
    ISargam -> Sargam.toScore
    _ -> ToScore.toScore


-- * realize

-- | Fully realized notes.
type Flat stroke =
    S.Flat (Realize.Group (Realize.Stroke stroke)) (Realize.Note stroke)

type Realized stroke = ([Flat stroke], [Realize.Warning])

-- | Realize a Korvai on a particular instrument.
realize :: forall stroke. (Solkattu.Notation stroke, Ord stroke)
    => Instrument stroke -> Korvai -> [Either Error (Realized stroke)]
realize inst korvai = case getStrokeMap inst (korvaiStrokeMaps korvai) of
    Left err -> [Left err]
    Right smap -> case getSections inst (korvaiSections korvai) of
        -- An instrument Korvai can be realized to the same instrument.
        Just sections ->
            map (realizeSection tala Realize.realizeStroke smap postproc)
                sections
        Nothing -> case korvaiSections korvai of
            -- IKonnakol korvai can be realized to any instrument.
            KorvaiSections IKonnakol sections ->
                map (realizeSection tala toStrokes smap postproc) sections
                where
                toStrokes = Realize.realizeSollu (Realize.smapSolluMap smap)
            -- IBol can be realized to ITabla.
            -- TODO doesn't work yet, how to restrict to stroke ~ Tabla.Stroke?
            -- KorvaiSections IBol sections ->
            --     map (realizeSection tala toStrokes smap postproc) sections
            --     where
            --     toStrokes = Realize.realizeSollu Bol.bolMap
            KorvaiSections kinst _ -> (:[]) $ Left $ "can't realize "
                <> instrumentName kinst <> " as " <> instrumentName inst
    where
    tala = korvaiTala korvai
    postproc = instPostprocess inst

realizeSection :: (Ord sollu, Pretty sollu, Solkattu.Notation stroke)
    => Talas.Tala
    -> Realize.ToStrokes sollu stroke
    -> Realize.StrokeMap Solkattu.Sollu stroke
    -> ([Flat stroke] -> [Flat stroke])
    -> Section (SequenceT sollu)
    -> Either Error (Realized stroke)
realizeSection tala toStrokes smap postproc section = do
    realized <- Realize.formatError $ fst $
        Realize.realize smap toStrokes (Talas.aksharas tala) $ flatten $
        sectionSequence section
    let alignWarn = checkAlignment realized
    (realized, durationWarns) <- return $ Realize.checkDuration realized
    startSpace <- spaces (inferNadai realized) (sectionStart section)
    return
        ( postproc $ startSpace ++ realized
        , maybe [] (:[]) alignWarn ++ durationWarns
        )
    where
    checkAlignment realized
        | tala == Talas.Carnatic Tala.any_beats = Nothing
        | otherwise = Realize.checkAlignment
            (Talas.aksharas tala)
            (sectionStart section) (sectionEnd section)
            (S.tempoNotes realized)

allMatchedSollus :: Instrument stroke -> Korvai
    -> Set (Realize.SolluMapKey Solkattu.Sollu)
allMatchedSollus instrument korvai = case korvaiSections korvai of
    KorvaiSections IKonnakol sections -> Set.map strip $
        mconcatMap (matchedSollus toStrokes talaAksharas) sections
    _ -> mempty
    where
    talaAksharas = Talas.aksharas (korvaiTala korvai)
    -- For uniformity with instruments, IKonnakol also maps from
    -- (Realize.Stroke Sollu) even though I don't use emphasis.  So shim it
    -- back to plain Sollus.
    strip (tag, sollus) = (tag, map Realize._stroke sollus)
    toStrokes = Realize.realizeSollu solluMap
    solluMap = either mempty Realize.smapSolluMap smap
    smap = getStrokeMap instrument (korvaiStrokeMaps korvai)

matchedSollus :: (Pretty sollu, Ord sollu) => Realize.ToStrokes sollu stroke
    -> Tala.Akshara -> Section (SequenceT sollu)
    -> Set (Realize.SolluMapKey sollu)
matchedSollus toStrokes talaAksharas =
    snd . Realize.realize_ dummyPattern toStrokes talaAksharas . flatten
        . sectionSequence
    where
    -- Since I'm just looking for used sollus, I can just map all patterns to
    -- rests.  I probably don't have to bother to get the duration right, but
    -- why not.
    dummyPattern tempo (Solkattu.PatternM p) =
        Right $ replicate p (tempo, Realize.Space Solkattu.Rest)

inferNadai :: [Flat stroke] -> S.Nadai
inferNadai = S._nadai . maybe S.defaultTempo fst . Seq.head . S.tempoNotes

flatten :: SequenceG g sollu -> [S.Flat g (Solkattu.Note sollu)]
flatten = Solkattu.cancelKarvai . S.flatten . S.toList

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

-- TODO broken by KorvaiSections, fix this
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
    getStrokeMap inst $ korvaiStrokeMaps korvai
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
        Either.rights $ map Realize.verifySolluKey (map S.toList defaultStrokes)

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
inferMetadata = modifySections . inferKorvaiMetadata
    where
    modifySections korvai = korvai
        { korvaiSections = case korvaiSections korvai of
            KorvaiSections inst sections ->
                KorvaiSections inst $ map (add korvai) sections
        }
    add korvai section = addSectionTags
        (inferSectionTags (korvaiTala korvai) section) section

inferKorvaiMetadata :: Korvai -> Korvai
inferKorvaiMetadata korvai =
    withKorvaiMetadata (mempty { _tags = inferKorvaiTags korvai }) korvai

inferKorvaiTags :: Korvai -> Tags.Tags
inferKorvaiTags korvai = Tags.Tags $ Maps.multimap $ concat
    [ [ ("tala", Talas.name tala)
      , ("sections", showt sections)
      , ("avartanams", pretty avartanams)
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
        KorvaiSections _ sections -> length sections
    instruments = map ginstrumentName $ korvaiInstruments korvai
    avartanams = Num.sum $ case korvaiSections korvai of
        KorvaiSections _ sections -> map (sectionAvartanams tala) sections

inferSectionTags :: Talas.Tala -> Section (SequenceT sollu) -> Tags.Tags
inferSectionTags tala section = Tags.Tags $ Map.fromList $
    [ ("avartanams", [pretty $ sectionAvartanams tala section])
    , ("nadai", map pretty nadais)
    , ("max_speed", [pretty $ maximum (0 : speeds)])
    , ("start", [pretty $ sectionStart section])
    , ("end", [pretty $ sectionEnd section])
    ]
    where
    seq = mapSollu (const ()) (sectionSequence section)
    tempos = map fst $ S.tempoNotes $ flatten seq
    nadais = Seq.unique_sort $ map S._nadai tempos
    speeds = Seq.unique_sort $ map S._speed tempos

sectionAvartanams :: Talas.Tala -> Section (SequenceT sollu) -> Int
sectionAvartanams tala section = floor $ dur / talaAksharas
    -- Take the floor because there may be a final note as supported by
    -- Realize.checkAlignment.
    where
    talaAksharas = fromIntegral (Talas.aksharas tala)
    seq = mapSollu (const ()) (sectionSequence section)
    dur = Solkattu.durationOf S.defaultTempo seq


-- * types

-- | This can be a Left because it comes from one of the instrument-specific
-- 'StrokeMaps' fields, which can be Left if 'Realize.strokeMap' verification
-- failed.
type StrokeMap sollu stroke = Either Error (Realize.StrokeMap sollu stroke)

data StrokeMaps = StrokeMaps {
    smapMridangam        :: StrokeMap Solkattu.Sollu Mridangam.Stroke
    , smapKendangTunggal :: StrokeMap Solkattu.Sollu KendangTunggal.Stroke
    , smapKendangPasang  :: StrokeMap Solkattu.Sollu KendangPasang.Stroke
    , smapReyong         :: StrokeMap Solkattu.Sollu Reyong.Stroke
    , smapSargam         :: StrokeMap Solkattu.Sollu Sargam.Stroke
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
