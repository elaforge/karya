-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Realize abstract solkattu 'S.Note's to concrete instrument-dependent
-- 'Note's.
module Solkattu.Realize where
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad.Writer as Writer
import qualified Control.Monad.Writer.CPS as Writer.CPS

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Map
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil
import qualified Util.UF as UF

import qualified Derive.Expr as Expr
import qualified Derive.Symbols as Symbols
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tala as Tala

import Global


type Error = Text

-- | The group is () because I don't need groups in the stroke map keys.
type SNote stroke = S.Note () (Note stroke)

-- * Note

-- | The 'Solkattu.Sollu's have been reduced to concrete strokes.
data Note stroke =
    Note !(Stroke stroke)
    | Space !Solkattu.Space
    -- | A pattern that has been made abstract.  This is a group that has been
    -- abstracted away.  That means it can have a name, but also it doesn't
    -- have to have an integral matra duration.  Since Abstract comes from
    -- Notes, the abstract duration is a series of 1-matra Abstracts, where
    -- each Note used to be.
    --
    -- These are created at the Format level, not here.
    | Abstract !Solkattu.Meta
    -- | This is 'Solkattu.Alignment'.  It shouldn't be here, but since I now
    -- drop groups in realize via 'stripGroups', I have to do
    -- 'verifyAlignment' on the output of 'realize', which means I need to
    -- preserve the Alignments.
    | Alignment !Tala.Akshara
    deriving (Eq, Show, Functor)

instance DeepSeq.NFData (Note stroke) where
    rnf _ = ()

data Stroke stroke = Stroke {
    _emphasis :: !Emphasis
    , _stroke :: !stroke
    } deriving (Eq, Ord, Show, Functor)

toExpr :: Expr.ToExpr a => Stroke a -> Expr.Expr Expr.MiniVal
toExpr (Stroke emphasis stroke) = case emphasis of
    Normal -> Expr.to_expr stroke
    Light -> Expr.with Symbols.weak stroke
    Heavy -> Expr.with Symbols.accent stroke

instance Solkattu.Notation stroke => Solkattu.Notation (Stroke stroke) where
    notation (Stroke emphasis stroke) = case emphasis of
        -- This makes the output ambiguous since some strokes are already
        -- capitalized.  TODO use bold or something
        Heavy -> Text.toUpper $ Solkattu.notation stroke
        Normal -> Solkattu.notation stroke
        Light -> Solkattu.notation stroke
    notationHtml (Stroke emphasis stroke) =
        emphasize $ Solkattu.notationHtml stroke
        where
        emphasize = case emphasis of
            Light -> Doc.tag_attrs "font" [("color", "gray")] . Just
            Normal -> id
            Heavy -> Doc.tag_attrs "font" [("color", "darkred")] . Just

instance Pretty stroke => Pretty (Stroke stroke) where
    pretty (Stroke emphasis stroke) = (<> pretty stroke) $ case emphasis of
        Heavy -> "hv "
        Normal -> ""
        Light -> "lt "

noteOf :: Note a -> Maybe (Stroke a)
noteOf (Note stroke) = Just stroke
noteOf _ = Nothing

strokeOf :: Note a -> Maybe a
strokeOf = fmap _stroke . noteOf

stroke :: stroke -> Stroke stroke
stroke = Stroke Normal

rest :: SNote stroke
rest = S.Note (Space Solkattu.Rest)

strokeToSequence :: stroke -> [S.Note g (Solkattu.Note (Stroke stroke))]
strokeToSequence = (:[]) . S.Note . Solkattu.Note . Solkattu.note . stroke

-- There's no general ToCall instance for Stroke because individual instruments
-- may have special cases.

-- | The emphasis will be propagated to the underlying stroke.
data Emphasis = Light | Normal | Heavy deriving (Eq, Ord, Show)

instance Semigroup Emphasis where (<>) = max
instance Monoid Emphasis where
    mempty = Normal
    mappend = (<>)

instance Pretty Emphasis where
    pretty Light = "^"
    pretty Normal = ""
    pretty Heavy = "v"

instance S.HasMatras (Note stroke) where
    matrasOf n = case n of
        Note {} -> 1
        Space {} -> 1
        Abstract meta -> fromMaybe 1 $ Solkattu._matras meta
        Alignment {} -> 0
    hasSustain n = case n of
        Note {} -> False
        Space {} -> True
        Abstract {} -> True
        Alignment {} -> False

instance Solkattu.Notation stroke => Solkattu.Notation (Note stroke) where
    notation = \case
        Space Solkattu.Rest -> "_"
        Space Solkattu.Offset -> " "
        Note s -> Solkattu.notation s
        Abstract meta -> metaNotation meta
        Alignment _ -> "" -- this should be filtered out prior to render
    extension = \case
        Space Solkattu.Rest -> ' '
        Abstract meta -> case Solkattu._type meta of
            Solkattu.GSarvaT -> '='
            _ -> '-'
        _ -> ' '
    notationHtml n = case n of
        Note s -> Solkattu.notationHtml s
        _ -> Doc.html $ Solkattu.notation n

metaNotation :: Solkattu.Meta -> Text
metaNotation (Solkattu.Meta _ (Just name) _) = name
metaNotation (Solkattu.Meta matras Nothing gtype) = case gtype of
    Solkattu.GSarvaT -> "="
    _ -> maybe "" showt matras <> typeName gtype

typeName :: Solkattu.GroupType -> Text
typeName = \case
    Solkattu.GTheme -> ""
    Solkattu.GFiller -> "f"
    Solkattu.GPattern -> "p"
    Solkattu.GExplicitPattern -> "p"
    Solkattu.GSarvaT -> "sarva"

-- | Used to replace two rests.
doubleRest :: Char
doubleRest = '‗' -- DOUBLE LOW LINE U+2017

instance Pretty stroke => Pretty (Note stroke) where
    pretty n = case n of
        Space Solkattu.Rest -> "_"
        Space Solkattu.Offset -> "."
        Note s -> pretty s
        Abstract a -> pretty a
        Alignment n -> "@" <> showt n

noteDuration :: S.HasMatras a => S.Tempo -> a -> S.Duration
noteDuration tempo = (* S.matraDuration tempo) . fromIntegral . S.matrasOf

-- * verifyAlignment

-- | Verify that the notes start and end at sam, and the given Alignments
-- fall where expected.
verifyAlignment :: Tala.Tala -> S.Duration -> S.Duration
    -> [(S.Tempo, Note stroke)] -> Maybe (Int, Error)
    -- ^ (index where the error occured, error)
verifyAlignment tala startOn endOn notes
    | tala == Tala.any_beats = Nothing
    | otherwise = msum (map verify (zip [0..] states)) <|> checkEnd
    where
    (finalState, states) = S.tempoToState tala startOn notes
    -- Either finalState one is at 0, or the last non-rest note is.
    checkEnd
        | atEnd finalState || maybe False atEnd finalNote = Nothing
        | otherwise = Just
            ( length states
            , "should end on sam" <> endMsg
                <> ", actually ends on " <> S.showPosition finalState
                <> ", or sam - " <> pretty left
            )
        where
        endMsg
            | endOn == 0 = ""
            | endOn > 0 = "+" <> pretty endOn
            | otherwise = pretty endOn
        finalNote = fst <$> List.find (not . isSpace . snd) (reverse states)
        left = fromIntegral (Tala.tala_aksharas tala)
            - S.stateMatraPosition finalState
    verify (i, (state, Alignment akshara))
        | atAkshara akshara state = Nothing
        | otherwise = Just (i, "expected akshara " <> showt akshara
            <> ", but at " <> S.showPosition state)
    verify _ = Nothing
    isSpace (Space _) = True
    isSpace _ = False
    atEnd state
        | endOn >= 0 = akshara == endOn
        | otherwise = akshara - fromIntegral (Tala.tala_aksharas tala) == endOn
        where akshara = S.stateMatraPosition state
    atAkshara akshara state =
        S.stateAkshara state == akshara && S.stateMatra state == 0

-- * StrokeMap

{- | Sollu to instrument stroke mapping.

    I considered integrating both strokes and patterns into SolluMap, but
    I kind of like how the types for 'SolluMap' and 'PatternMap' can be more
    specific.  Namely, SolluMap has only strokes and rests, because it gets
    substituted for sollus, regardless of their rhythm, while PatternMap can
    have tempo changes since they always substitute a single Solkattu.Note.
    If I ever have a use for e.g. (taka.p5, ...) then I could reconsider.
-}
data StrokeMap stroke = StrokeMap {
    smapSolluMap :: SolluMap stroke
    -- | Shadowed SolluMapKeys, saved here to warn about them later.
    , smapSolluShadows ::
        [(SolluMapKey Solkattu.Sollu, [Maybe (Stroke stroke)])]
    , smapPatternMap :: PatternMap stroke
    } deriving (Eq, Show)

isInstrumentEmpty :: StrokeMap stroke -> Bool
isInstrumentEmpty (StrokeMap (SolluMap solluMap) _ (PatternMap patternMap)) =
    Map.null solluMap && Map.null patternMap

smapKeys :: StrokeMap stroke -> Set (SolluMapKey Solkattu.Sollu)
smapKeys smap = Map.keysSet m
    where SolluMap m = smapSolluMap smap

instance Semigroup (StrokeMap stroke) where
    StrokeMap a1 b1 c1 <> StrokeMap a2 b2 c2 =
        StrokeMap (a1<>a2) (b1<>b2) (c1<>c2)
instance Monoid (StrokeMap stroke) where
    mempty = StrokeMap mempty mempty mempty
    mappend = (<>)

instance Pretty stroke => Pretty (StrokeMap stroke) where
    format (StrokeMap solluMap solluShadows patternMap) =
        Pretty.record "StrokeMap"
            [ ("solluMap", Pretty.format solluMap)
            , ("solluShadows", Pretty.format solluShadows)
            , ("patternMap", Pretty.format patternMap)
            ]

-- | Verify a list of pairs stroke map and put them in an 'StrokeMap'.
strokeMap :: Pretty stroke => PatternMap stroke
    -> [ ( [S.Note g (Solkattu.Note Solkattu.Sollu)]
         , [S.Note g (Solkattu.Note (Stroke stroke))]
         )
       ]
    -> Either Error (StrokeMap stroke)
strokeMap pmap strokes = do
    strokes <- mapM (traverse solkattuToRealize) strokes
    (smap, solluShadows) <- solluMap strokes
    return $ StrokeMap
        { smapSolluMap = smap
        , smapSolluShadows = solluShadows
        , smapPatternMap = pmap
        }

-- | Stroke maps use 'Solkattu.Notes', so they can use the same language in
-- "Solkattu.Dsl".  But since they don't go through a realization step
-- (being used to implement the realization step for sollus), I can directly
-- map them to 'Realize.Note's before storing them in 'StrokeMap'.
solkattuToRealize :: [S.Note g (Solkattu.Note (Stroke stroke))]
    -> Either Error [S.Note () (Note stroke)]
solkattuToRealize = mapM $ traverse convert . S.mapGroup (const ())
    where
    convert = \case
        Solkattu.Note n -> Right $ Note (Solkattu._sollu n)
        Solkattu.Space a -> Right $ Space a
        Solkattu.Pattern p -> Left $ "can't convert pattern: " <> pretty p
        Solkattu.Alignment a -> Right $ Alignment a

-- ** PatternMap

-- | This maps a 'Pattern' of a certain duration to a realization.  The
-- 'S.Matra's should the same duration as the the list in the default tempo.
-- This is enforced in the constructor 'patternMap'.
newtype PatternMap stroke = PatternMap (Map Solkattu.Pattern [SNote stroke])
    deriving (Eq, Show, Pretty, Semigroup, Monoid)

-- | Make a PatternMap while checking that the durations match.  Analogous to
-- 'solluMap'.
patternMap :: [(Solkattu.Pattern, [SNote stroke])]
    -> Either Error (PatternMap stroke)
patternMap pairs
    | null errors = Right $ PatternMap $ Map.fromList pairs
    | otherwise = Left $ Text.intercalate "; " errors
    where
    errors = mapMaybe check pairs
    check (p, notes)
        | notesMatras /= fromIntegral (S.matrasOf p) =
            Just $ "pattern matras " <> pretty (S.matrasOf p)
                <> " /= realization matras " <> pretty notesMatras
                <> " for " <> showt p
        | otherwise = Nothing
        where
        notesMatras = notesDuration / S.matraDuration S.defaultTempo
        notesDuration = sum $ map (S.durationOf S.defaultTempo) notes

lookupPattern :: Solkattu.Pattern -> PatternMap stroke -> Maybe [SNote stroke]
lookupPattern p (PatternMap pmap) = Map.lookup p pmap

-- ** SolluMap

-- | Sollus and Strokes should be the same length.  This is enforced in the
-- constructor 'solluMap'.  Nothing is a rest, which means a sollu can map
-- to silence, which actually happens in practice.
newtype SolluMap stroke =
    SolluMap (Map (SolluMapKey Solkattu.Sollu) [Maybe (Stroke stroke)])
    deriving (Eq, Show, Pretty, Semigroup, Monoid)
type SolluMapKey sollu = (Maybe Solkattu.Tag, [sollu])

prettyKey :: Pretty sollu => SolluMapKey sollu -> Text
prettyKey (tag, sollus) = maybe ""  ((<>"^") . pretty) tag <> pretty sollus

-- | Directly construct a SolluMap from strokes.
simpleSolluMap :: [([Solkattu.Sollu], [Maybe stroke])] -> SolluMap stroke
simpleSolluMap = SolluMap .  fmap (fmap (fmap stroke)) . Map.fromList
    . map (first (Nothing,))

-- | Verify and costruct a SolluMap from a list of pairs.  Later pairs win over
-- earlier ones.
solluMap :: Pretty stroke =>
    [([S.Note g (Solkattu.Note Solkattu.Sollu)], [SNote stroke])]
    -> Either Error (SolluMap stroke,
        [(SolluMapKey Solkattu.Sollu, [Maybe (Stroke stroke)])])
solluMap =
    fmap (first SolluMap . Util.Map.unique . reverse) . mapM verifySolluMap

-- | A sollu can map to a rest stroke: tang.ga, where ga is silent Or
-- taka.tarikita played N_ktpk.  But I don't think a rest sollu can map to
-- a stroke, and in fact it won't work since I look up by sollus only.
verifySolluMap :: Pretty stroke
    => ([S.Note g (Solkattu.Note Solkattu.Sollu)], [SNote stroke])
    -> Either Error (SolluMapKey Solkattu.Sollu, [Maybe (Stroke stroke)])
verifySolluMap (sollus, strokes) = do
    (tag, mbSollus) <- verifySolluKey sollus

    let pSollus = Text.intercalate "." . map (maybe "__" pretty)
        pStrokes = mconcat . map pretty
    let throw = Left . (pref<>)
            where pref = pSollus mbSollus <> " -> " <> pStrokes strokes <> ": "
    -- TODO enable strictCheck somehow.  Maybe on by default, and disable via
    -- tag on strokes?
    let strictCheck = False
    strokes <- forM (zipTails mbSollus strokes) $ \case
        (Just sollu : _, S.Note (Space {}) : _)
            | strictCheck ->
                throw $ "sollu '" <> pretty sollu <> "' given rest stroke"
            | otherwise -> return $ Just Nothing
        (Nothing : _, S.Note (Note stroke) : _) ->
            throw $ "rest sollu given non-rest stroke '" <> pretty stroke <> "'"
        (Nothing : _, S.Note (Space {}) : _) -> return Nothing
        (Just _ : _, S.Note (Note stroke) : _) -> return $ Just $ Just stroke
        (sollus, []) ->
            throw $ "more sollus than strokes at " <> pSollus sollus
        ([], strokes) ->
            throw $ "more strokes than sollus at " <> pStrokes strokes
        (sollu : _, stroke : _) ->
            throw $ "should have plain sollus and strokes: "
                <> pretty (sollu, stroke)
    return ((tag, Maybe.catMaybes mbSollus), Maybe.catMaybes strokes)

zipTails :: [a] -> [b] -> [([a], [b])]
zipTails as bs = filter (\(as, bs) -> not (null as && null bs)) $
    zip (List.tails as) (List.tails bs)

verifySolluKey :: [S.Note g (Solkattu.Note Solkattu.Sollu)]
    -> Either Error (SolluMapKey (Maybe Solkattu.Sollu))
verifySolluKey sollus_ = do
    let sollus = map (S.mapGroup (const ())) sollus_
    let throw = Left . ((pretty sollus <> ": ")<>)
    (tags, sollus) <- fmap (unzip . Maybe.catMaybes) $
        -- Allow but ignore TempoChanges.  This makes it convenient to use
        -- a sequence like 'nakataka = su (na.ka.ta.ka)' in both notation
        -- and the stroke map.
        forM (S.notes sollus) $ \case
            Solkattu.Note note ->
                Right $ Just (Solkattu._tag note, Just $ Solkattu._sollu note)
            Solkattu.Space {} -> Right $ Just (Nothing, Nothing)
            s -> throw $ "should only have plain sollus: " <> pretty s
    -- TODO warn if there are inconsistent tags?
    return (Seq.head (Maybe.catMaybes tags), sollus)


-- * realize

type RealizePattern tempo stroke =
    tempo -> Solkattu.Pattern -> Either Error [(tempo, Note stroke)]

realizePattern :: PatternMap stroke -> RealizePattern S.Tempo stroke
realizePattern pmap tempo pattern = case lookupPattern pattern pmap of
    Nothing -> Left $ "no pattern for " <> pretty pattern
    Just notes -> Right $ S.tempoNotes $ S.flattenWith tempo notes

-- | This is the realized version of 'Solkattu.Group'.  I retain the dropped
-- strokes so "Solkattu.Technique" can use them.
data Group stroke =
    GReduction !(Reduction stroke)
    | GMeta !Solkattu.Meta
    deriving (Eq, Ord, Show)

data Reduction stroke = Reduction {
    _dropped :: ![stroke]
    , _side :: !Solkattu.Side
    } deriving (Eq, Ord, Show)

instance Pretty stroke => Pretty (Group stroke) where
    pretty (GReduction r) = pretty r
    pretty (GMeta m) = pretty m

instance Pretty stroke => Pretty (Reduction stroke) where
    pretty (Reduction dropped side) = pretty (dropped, side)

type Realized stroke = S.Flat (Group (Stroke stroke)) (Note stroke)

realize :: (Pretty sollu, Ord sollu)
    => StrokeMap stroke -> ToStrokes sollu stroke
    -> [S.Flat Solkattu.Group (Solkattu.Note sollu)]
    -> (UF.UntilFail Error (Realized stroke), Set (SolluMapKey sollu))
realize smap = realize_ (realizePattern (smapPatternMap smap))
    -- TODO just pass PatternMap, since I don't parameterize anymore
    -- well, except Korvai.matchedSollus

realize_ :: (Pretty sollu, Ord sollu)
    => RealizePattern S.Tempo stroke -> ToStrokes sollu stroke
    -> [S.Flat Solkattu.Group (Solkattu.Note sollu)]
    -> (UF.UntilFail Error (Realized stroke), Set (SolluMapKey sollu))
realize_ realizePattern toStrokes =
    Writer.CPS.runWriter
        . fmap (UF.concatMap convertGroups) . UF.processM realize1
    where
    realize1 note@(S.FNote tempo n) notes = case n of
        Solkattu.Alignment n -> return $ (,notes) $
            UF.singleton $ S.FNote tempo (Alignment n)
        Solkattu.Space space -> return $ (,notes) $
            UF.singleton $ S.FNote tempo (Space space)
        Solkattu.Pattern p@(Solkattu.PatternM matras) ->
            return $ (,notes) $ case realizePattern tempo p of
                Left err -> UF.Fail err
                Right tempoNotes -> UF.singleton $ S.FGroup tempo group $
                    map (uncurry S.FNote) tempoNotes
            where
            group = Solkattu.GMeta $ Solkattu.Meta
                { _matras = Just matras
                , _name = Nothing
                , _type = Solkattu.GPattern
                }
        Solkattu.Note {} -> case findSequence toStrokes (note : notes) of
            Left err -> return (UF.Fail err, notes)
            Right (matched, (strokes, remain)) -> do
                Writer.tell $ Set.singleton matched
                return (UF.fromList strokes, remain)
    realize1 (S.FGroup tempo
            (Solkattu.GMeta m@(Solkattu.Meta (Just matras) _ Solkattu.GSarvaT))
                children)
            notes
        | null children = return
            ( UF.singleton $ S.FGroup tempo (Solkattu.GMeta m)
                [S.FNote tempo (Abstract m)]
                -- The metadata is already in the group, but ToScore at least
                -- relies on an explicit Abstract since it flattens out groups.
            , notes
            )
        -- TODO use monad Either?
        | otherwise = (, notes) <$> case findSequence toStrokes children of
            Left err -> return $ UF.Fail $ "sarva: " <> err
            Right (matched, (strokes, left)) -> case mapM getRest left of
                Nothing -> return $ UF.Fail $
                    "sarva: incomplete match: " <> pretty matched
                    <> ", left: " <> pretty (S.flattenedNotes left)
                -- Trailing rests are ok, as long as I include them in the
                -- output.
                Just rests ->
                    case splitStrokes dur (cycle (strokes ++ rests)) of
                        Left err -> return $ UF.Fail $ "sarva: " <> err
                        Right (_left, (strokes, _)) -> do
                            Writer.tell $ Set.singleton matched
                            -- I keep this as a group so format can highlight
                            -- it.  Even though I realized the sarva, I might
                            -- as well leave the duration on.
                            return $ UF.singleton $
                                S.FGroup tempo (Solkattu.GMeta m) strokes
        where dur = S.matraDuration tempo * fromIntegral matras
    realize1 (S.FGroup tempo group children) notes = do
        rest <- UF.toList <$> UF.processM realize1 children
        return $ (, notes) $ case rest of
            -- I drop the group, so there will be extra notes in the
            -- output.  But if I emit a partial group, then
            -- convertGroup may get an error, which will conceal the
            -- real one here.
            (children, Just err) -> UF.fromListFail children err
            (children, Nothing) ->
                UF.singleton $ S.FGroup tempo group children
    getRest (S.FNote tempo (Solkattu.Space space)) =
        Just $ S.FNote tempo (Space space)
    getRest _ = Nothing

formatError :: Solkattu.Notation a => UF.UntilFail Error (S.Flat g a)
    -> Either Error [S.Flat g a]
formatError = format . UF.toList
    where
    format (result, Nothing) = Right result
    format (pre, Just err) = Left $
        TextUtil.joinWith "\n" (errorNotation (S.flattenedNotes pre)) err
    errorNotation = Text.unwords . map (justifyLeft 2 ' ' . Solkattu.notation)

{- | Given a group like

    > [S.FGroup (Solkatttu.Reduction 1 Before) [a, b], c]

    collect dropped strokes into a Realize.'Reduction':

    > [S.FGroup (Reduction [a] Before) [b], c]
-}
convertGroups :: S.Flat Solkattu.Group (Note stroke)
    -> UF.UntilFail Error (S.Flat (Group (Stroke stroke)) (Note stroke))
convertGroups (S.FNote tempo note) = UF.singleton $ S.FNote tempo note
convertGroups (S.FGroup tempo g children) =
    case UF.toList $ UF.concatMap convertGroups (UF.fromList children) of
        -- The convertGroup is unlikely to succeed if the children aren't
        -- complete, and even if it did it would probably be confusing.
        -- So flatten this group and append an error.
        (children, Just err) -> UF.fromListFail children err
        (children, Nothing) -> convertGroup tempo g children
    where
    convertGroup tempo (Solkattu.GReduction (Solkattu.Reduction split side))
            children =
        case splitStrokes (S.fmatraDuration tempo split) children of
            Left err -> UF.Fail err
            Right (left, (pre, post))
                | left > 0 -> UF.Fail $ "group split too long, duration left: "
                    <> pretty left
                | otherwise -> UF.singleton $ S.FGroup tempo group kept
                    where
                    group = GReduction $ Reduction
                        { _dropped = mapMaybe noteOf $ S.flattenedNotes dropped
                        , _side = side
                        }
                    (kept, dropped) = case side of
                        Solkattu.Before -> (post, pre)
                        Solkattu.After -> (pre, post)
    convertGroup tempo (Solkattu.GMeta meta) children =
        UF.singleton $ S.FGroup tempo (GMeta meta) children

splitStrokes :: S.Duration -> [S.Flat g (Note stroke)]
    -> Either Error
        ( S.Duration
        , ([S.Flat g (Note stroke)], [S.Flat g (Note stroke)])
        )
splitStrokes dur [] = Right (dur, ([], []))
splitStrokes dur notes | dur <= 0 = Right (dur, ([], notes))
splitStrokes dur (note : notes) = case note of
    -- TODO I could check for SarvaGroup here.  It means I have sarva nested
    -- inside a group, which I guess I should shorten.  I'll wait until I'm
    -- more sure about my approach to sarva.
    S.FGroup tempo g children -> case splitStrokes dur children of
        Left err -> Left err
        Right (left, (_pre, [])) -> add (note:) $ splitStrokes left notes
        Right (left, (pre, post)) -> Right
            ( left
            , ( [S.FGroup tempo g pre]
              , S.FGroup tempo g post : notes
              )
            )
    S.FNote tempo n
        | noteDur <= dur -> add (note:) $ splitStrokes (dur - noteDur) notes
        | otherwise -> case n of
            Note _ -> Left "can't split a stroke"
            Abstract a -> Left $ "can't split Abstract " <> pretty a
            Alignment _ -> Left $ "not reached: alignment should have 0 dur"
            Space space -> do
                let make = fmap (map (uncurry S.FNote)) . makeSpace tempo space
                pre <- make dur
                post <- make (dur - noteDur)
                return (0, (pre, post ++ notes))
        where
        noteDur = noteDuration tempo n
    where
    add = fmap . second . first

-- | Try to produce Spaces of the given Duration.  Based on Notation.spaceD.
makeSpace :: S.Tempo -> Solkattu.Space -> S.Duration
    -> Either Error [(S.Tempo, Note stroke)]
makeSpace tempo space dur = map make <$> S.decompose s0_matras
    where
    make speed = (tempo { S._speed = speed }, Space space)
    s0_matras = dur * fromIntegral (S._nadai tempo)

-- | Find the longest matching sequence and return the match and unconsumed
-- notes.
findSequence :: Pretty sollu => ToStrokes sollu stroke
    -> [S.Flat g (Solkattu.Note sollu)]
    -> Either Error
        ( SolluMapKey sollu
        , ( [S.Flat g (Note stroke)]
          , [S.Flat g (Solkattu.Note sollu)]
          )
        )
findSequence toStrokes notes =
    case bestMatch tag sollus toStrokes of
        Nothing -> Left $ "sequence not found: " <> pretty sollus
        Just (matched, strokes) ->
            Right (matched, replaceSollus strokes notes)
    where
    -- Collect only sollus and rests.  This stops at a group boundary.
    pre = fst $ Seq.span_while noteOf notes
    sollus = map Solkattu._sollu $ mapMaybe snd pre
    noteOf (S.FNote tempo n) = (tempo,) <$> case n of
        Solkattu.Note n -> Just $ Just n
        Solkattu.Space {} -> Just Nothing
        Solkattu.Alignment {} -> Just Nothing
        Solkattu.Pattern {} -> Nothing
    noteOf (S.FGroup {}) = Nothing
    tag = Solkattu._tag =<< Seq.head (mapMaybe snd pre)

-- | Match each stroke to a Sollu, copying over Rests without consuming
-- a stroke.
replaceSollus :: [Maybe (Stroke stroke)] -- ^ Nothing means a rest
    -> [S.Flat g (Solkattu.Note sollu)]
    -> ([S.Flat g (Note stroke)], [S.Flat g (Solkattu.Note sollu)])
replaceSollus [] ns = ([], ns)
    -- I should be out of strokes before I get here, so this shouldn't happen.
    -- I could pass [(S.Tempo, Solkattu.Note sollu)], but then I have to
    -- recreate a S.FGroup for everything in the tail, which is potentially
    -- quadratic.
replaceSollus (_ : _) ns@(S.FGroup {} : _) = ([], ns)
replaceSollus (stroke : strokes) (S.FNote tempo n : ns) = case n of
    Solkattu.Note _ ->
        first (S.FNote tempo rnote :) $ replaceSollus strokes ns
        where rnote = maybe (Space Solkattu.Rest) Note stroke
    Solkattu.Space space -> first (S.FNote tempo (Space space) :) next
    Solkattu.Alignment {} -> next
    -- This shouldn't happen because Seq.spanWhile noteOf should have
    -- stopped when it saw this.
    Solkattu.Pattern {} -> next
    where
    -- Continue without consuming this stroke.
    next = replaceSollus (stroke : strokes) ns
replaceSollus (_:_) [] = ([], [])
    -- This shouldn't happen because strokes from the SolluMap should be
    -- the same length as the RealizedNotes used to find them.

-- ** ToStrokes

-- | Find strokes for a sequence of sollus.
data ToStrokes sollu stroke = ToStrokes {
    -- | The longest [sollu] key in the whole SolluMap, so I know when to give
    -- up looking for the longest prefix.
    _longestKey :: !Int
    , _getStrokes :: Maybe Solkattu.Tag -> [sollu]
        -> Maybe [Maybe (Stroke stroke)]
    }

realizeStroke :: ToStrokes (Stroke stroke) stroke
realizeStroke = ToStrokes
    { _longestKey = 100
    , _getStrokes = const $ Just . map Just
    }

-- | If the sollu and stroke are the same, I can just copy the sollu.  This is
-- for "monomorphic" single instrument scores, such as for mridangam.
realizeSimpleStroke :: ToStrokes stroke stroke
realizeSimpleStroke = ToStrokes
    -- realize for GSarva expects an exact match, so to make it happy I can
    -- convert as many as I'm given.  TODO grody hack
    { _longestKey = 100
    , _getStrokes = const $ Just . map (Just . stroke)
    }

realizeSollu :: SolluMap stroke -> ToStrokes Solkattu.Sollu stroke
realizeSollu (SolluMap smap) = ToStrokes
    { _longestKey =
        fromMaybe 0 $ Seq.maximum (map (length . snd) (Map.keys smap))
    , _getStrokes = \tag sollus -> Map.lookup (tag, sollus) smap
    }

-- | Convert sollus to strokes.
bestMatch :: Maybe Solkattu.Tag -> [sollu] -> ToStrokes sollu stroke
    -> Maybe (SolluMapKey sollu, [Maybe (Stroke stroke)])
    -- ^ Nothing means no match, [Nothing] is a rest
bestMatch tag sollus toStrokes =
    -- Try with the specific tag, otherwise fall back to no tag.
    Seq.head (find tag prefixes) <|> Seq.head (find Nothing prefixes)
    where
    find tag = mapMaybe (\s -> ((tag, s),) <$> _getStrokes toStrokes tag s)
    prefixes = reverse $ drop 1 $ List.inits $
        take (_longestKey toStrokes) sollus


-- * text util

justifyLeft :: Int -> Char -> Text -> Text
justifyLeft n c text
    | len >= n = text
    | otherwise = text <> Text.replicate (n - len) (Text.singleton c)
    where len = textLength text

textLength :: Text -> Int
textLength = sum . map len . untxt
    where
    -- Combining characters don't contribute to the width.  I'm sure it's way
    -- more complicated than this, but for the moment this seems to work.
    len c
        | Char.isMark c = 0
        | otherwise = 1
