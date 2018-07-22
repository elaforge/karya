-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Realize abstract solkattu 'S.Note's to concrete instrument-dependent
-- 'Note's.
module Solkattu.Realize where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil
import qualified Util.UF as UF

import qualified Derive.Expr as Expr
import qualified Derive.Symbols as Symbols
import qualified Solkattu.Sequence as S
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
    | Pattern !Solkattu.Pattern
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
        Pattern p -> S.matrasOf p
        Alignment {} -> 0
    hasSustain n = case n of
        Note {} -> False
        Space {} -> True
        Pattern {} -> True
        Alignment {} -> False

instance Solkattu.Notation stroke => Solkattu.Notation (Note stroke) where
    notation = \case
        Space Solkattu.Rest -> "_"
        Space Solkattu.Sarva -> "="
        Space Solkattu.Offset -> " "
        Note s -> Solkattu.notation s
        Pattern p -> Solkattu.notation p
        Alignment _ -> "" -- this should be filtered out prior to render
    extension = \case
        Space Solkattu.Rest -> ' '
        Space Solkattu.Sarva -> '='
        Pattern p -> Solkattu.extension p
        _ -> ' '
    notationHtml n = case n of
        Note s -> Solkattu.notationHtml s
        Pattern p -> Solkattu.notationHtml p
        _ -> Doc.html $ Solkattu.notation n

-- | Used to replace two rests.
doubleRest :: Char
doubleRest = '‗' -- DOUBLE LOW LINE U+2017

instance Pretty stroke => Pretty (Note stroke) where
    pretty n = case n of
        Space Solkattu.Rest -> "_"
        Space Solkattu.Sarva -> "="
        Space Solkattu.Offset -> "."
        Note s -> pretty s
        Pattern p -> pretty p
        Alignment n -> "@" <> showt n

noteDuration :: S.Tempo -> Note stroke -> S.Duration
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

-- * Patterns

-- | This maps a 'Pattern' of a certain duration to a realization.  The
-- 'S.Matra's should the same duration as the the list in the default tempo.
-- This is enforced in the constructor 'patterns'.
newtype Patterns stroke = Patterns (Map Solkattu.Pattern [SNote stroke])
    deriving (Eq, Show, Pretty, Semigroup, Monoid)

-- | Make a Patterns while checking that the durations match.
patterns :: [(Solkattu.Pattern, [SNote stroke])]
    -> Either Error (Patterns stroke)
patterns pairs
    | null errors = Right $ Patterns $ Map.fromList pairs
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

lookupPattern :: Solkattu.Pattern -> Patterns stroke -> Maybe [SNote stroke]
lookupPattern p (Patterns pmap) = Map.lookup p pmap

mapPatterns :: ([SNote stroke] -> [SNote stroke]) -> Patterns stroke
    -> Patterns stroke
mapPatterns f (Patterns p) = Patterns (f <$> p)

-- ** StrokeMap

-- | Sollus and Strokes should be the same length.  This is enforced in the
-- constructor 'strokeMap'.  Nothing is a rest, which applies to longer
-- sequences like dinga.
newtype StrokeMap stroke = StrokeMap
    (Map (Maybe Solkattu.Tag, [Solkattu.Sollu]) [Maybe (Stroke stroke)])
    deriving (Eq, Show, Pretty, Semigroup, Monoid)

-- | Directly construct a StrokeMap from strokes.
simpleStrokeMap :: [([Solkattu.Sollu], [Maybe stroke])] -> StrokeMap stroke
simpleStrokeMap = StrokeMap .  fmap (fmap (fmap stroke)) . Map.fromList
    . map (first (Nothing,))

strokeMap :: Pretty stroke =>
    [([S.Note g (Solkattu.Note Solkattu.Sollu)], [SNote stroke])]
    -> Either Error (StrokeMap stroke)
strokeMap =
    fmap (StrokeMap . Map.fromList)
        . mapM (verify . first (map (S.mapGroup (const ()))))
    where
    verify (sollus, strokes) = do
        let throw = Left
                . (("stroke map " <> pretty sollus <> " to " <> pretty strokes
                    <> ": ") <>)
        (tags, sollus) <- fmap (unzip . Maybe.catMaybes) $
            -- Allow but ignore TempoChanges.  This makes it convenient to use
            -- a sequence like 'nakataka = su (na.ka.ta.ka)' in both notation
            -- and the stroke map.
            forM (S.notes sollus) $ \case
                Solkattu.Note note ->
                    Right $ Just (Solkattu._tag note, Solkattu._sollu note)
                Solkattu.Space {} -> Right Nothing
                s -> throw $ "should only have plain sollus: " <> pretty s
        strokes <- forM strokes $ \case
            S.Note (Note s) -> Right (Just s)
            S.Note (Space {}) -> Right Nothing
            s -> throw $ "should have plain strokes: " <> pretty s
        unless (length sollus == length strokes) $
            throw $ "sollus and strokes have differing lengths after removing\
                \ rests: sollus " <> showt (length sollus)
                <> " /= strokes " <> showt (length strokes)
        -- TODO warn if there are inconsistent tags?
        return ((Seq.head (Maybe.catMaybes tags), sollus), strokes)

-- ** Instrument

-- | Sollu to instrument stroke mapping.
data Instrument stroke = Instrument {
    instStrokeMap :: StrokeMap stroke
    , instPatterns :: Patterns stroke
    } deriving (Eq, Show)

isInstrumentEmpty :: Instrument stroke -> Bool
isInstrumentEmpty (Instrument (StrokeMap strokeMap) (Patterns patterns)) =
    Map.null strokeMap && Map.null patterns

instance Semigroup (Instrument stroke) where
    Instrument a1 b1 <> Instrument a2 b2 = Instrument (a1<>a2) (b1<>b2)
instance Monoid (Instrument stroke) where
    mempty = Instrument mempty mempty
    mappend = (<>)

instance Pretty stroke => Pretty (Instrument stroke) where
    format (Instrument strokeMap patterns) = Pretty.record "Instrument"
        [ ("strokeMap", Pretty.format strokeMap)
        , ("patterns", Pretty.format patterns)
        ]

instrument :: Pretty stroke =>
    [([S.Note g (Solkattu.Note Solkattu.Sollu)], [SNote stroke])]
    -> Patterns stroke -> Either Error (Instrument stroke)
instrument strokes patterns = do
    smap <- strokeMap strokes
    return $ Instrument
        { instStrokeMap = smap
        , instPatterns = patterns
        }

-- * realize

type RealizePattern tempo stroke =
    tempo -> Solkattu.Pattern -> Either Error [(tempo, Note stroke)]

-- | Don't realize Patterns, just pass them through.
keepPattern :: RealizePattern tempo stroke
keepPattern tempo pattern = Right [(tempo, Pattern pattern)]

realizePattern :: Patterns stroke -> RealizePattern S.Tempo stroke
realizePattern pmap tempo pattern = case lookupPattern pattern pmap of
    Nothing -> Left $ "no pattern for " <> pretty pattern
    Just notes -> Right $ S.tempoNotes $ S.flattenWith tempo notes

-- | This is the realized version of 'Solkattu.Group'.  I retain the dropped
-- strokes so "Solkattu.Technique" can use them.
data Group stroke = Group { _dropped :: ![stroke], _side :: !Solkattu.Side }
    deriving (Eq, Ord, Show)

instance Pretty stroke => Pretty (Group stroke) where
    pretty (Group dropped side) = pretty (dropped, side)

type Realized stroke = S.Flat (Group (Stroke stroke)) (Note stroke)

realize :: Pretty sollu => RealizePattern S.Tempo stroke
    -> GetStroke sollu stroke -> [S.Flat Solkattu.Group (Solkattu.Note sollu)]
    -> UF.UntilFail Error (Realized stroke)
realize realizePattern getStroke =
    UF.concatMap convertGroups . UF.process realize1
    where
    realize1 note@(S.FNote tempo n) notes = case n of
        Solkattu.Alignment n -> (,notes) $
            UF.singleton $ S.FNote tempo (Alignment n)
        Solkattu.Space space -> (,notes) $
            UF.singleton $ S.FNote tempo (Space space)
        Solkattu.Pattern p -> (,notes) $ case realizePattern tempo p of
            Left err -> UF.Fail err
            Right tempoNotes -> UF.fromList $ map (uncurry S.FNote) tempoNotes
        Solkattu.Note {} -> case findSequence getStroke (note : notes) of
            Left err -> (UF.Fail err, notes)
            Right (realized, remain) -> (UF.fromList realized, remain)
    realize1 (S.FGroup tempo g children) notes =
        (,notes) $ case UF.toList $ UF.process realize1 children of
            -- I drop the group, so there will be extra notes in the output.
            -- But if I emit a partial group, then convertGroup may get an
            -- error, which will conceal the real one here.
            (children, Just err) -> UF.fromListFail children err
            (children, Nothing) -> UF.singleton $ S.FGroup tempo g children

formatError :: Solkattu.Notation a => UF.UntilFail Error (S.Flat g a)
    -> Either Error [S.Flat g a]
formatError = format . UF.toList
    where
    format (result, Nothing) = Right result
    format (pre, Just err) = Left $
        TextUtil.joinWith "\n" (errorNotation (S.flattenedNotes pre)) err
    errorNotation = Text.unwords . map (justifyLeft 2 ' ' . Solkattu.notation)

{- | Given a group like

    > [S.FGroup (Solkatttu.Group 1 Before) [a, b], c]

    collect dropped strokes into a 'Group':

    > [S.FGroup (Group [a] Before) [b], c]
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
    convertGroup tempo (Solkattu.Group split side) children =
        case splitStrokes (S.fmatraDuration tempo split) children of
            Left err -> UF.Fail err
            Right (left, (pre, post))
                | left > 0 -> UF.Fail $ "group split too long, duration left: "
                    <> pretty left
                | otherwise -> UF.singleton $ S.FGroup tempo group kept
                    where
                    group = Group
                        { _dropped = mapMaybe noteOf $ S.flattenedNotes dropped
                        , _side = side
                        }
                    (kept, dropped) = case side of
                        Solkattu.Before -> (post, pre)
                        Solkattu.After -> (pre, post)

splitStrokes :: S.Duration -> [S.Flat g (Note stroke)]
    -> Either Error
        ( S.Duration
        , ([S.Flat g (Note stroke)], [S.Flat g (Note stroke)])
        )
splitStrokes dur [] = Right (dur, ([], []))
splitStrokes dur notes | dur <= 0 = Right (dur, ([], notes))
splitStrokes dur (note : notes) = case note of
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
            Pattern p -> Left $ "can't split a pattern: " <> pretty p
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
findSequence :: Pretty sollu => GetStroke sollu stroke
    -> [S.Flat g (Solkattu.Note sollu)]
    -> Either Error
        ([S.Flat g (Note stroke)], [S.Flat g (Solkattu.Note sollu)])
findSequence getStroke notes =
    case bestMatch tag sollus getStroke of
        Nothing -> Left $ "sequence not found: " <> pretty sollus
        Just strokes -> Right $ replaceSollus strokes notes
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
replaceSollus :: [Maybe (Stroke stroke)]
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
    -- This shouldn't happen because Seq.spanWhile isSollu should have
    -- stopped when it saw this.
    Solkattu.Pattern {} -> next
    where
    -- Continue without consuming this stroke.
    next = replaceSollus (stroke : strokes) ns
replaceSollus (_:_) [] = ([], [])
    -- This shouldn't happen because strokes from the StrokeMap should be
    -- the same length as the RealizedNotes used to find them.

-- ** GetStroke

-- | Int is the longest [sollu] key, so I know when to give up looking for the
-- longest prefix.
type GetStroke sollu stroke =
    (Int, Maybe Solkattu.Tag -> [sollu] -> Maybe [Maybe (Stroke stroke)])

realizeStroke :: GetStroke (Stroke stroke) stroke
realizeStroke = (1, const $ Just . map Just)

-- | If the sollu and stroke are the same, I can just copy the sollu.  This is
-- for "monomorphic" single instrument scores, such as for mridangam.
realizeSimpleStroke :: GetStroke stroke stroke
realizeSimpleStroke = (1, const $ Just . map (Just . stroke))

realizeSollu :: StrokeMap stroke -> GetStroke Solkattu.Sollu stroke
realizeSollu (StrokeMap smap) =
    ( fromMaybe 0 $ Seq.maximum (map (length . snd) (Map.keys smap))
    , \tag sollus -> Map.lookup (tag, sollus) smap
    )

bestMatch :: Maybe Solkattu.Tag -> [sollu] -> GetStroke sollu stroke
    -> Maybe [Maybe (Stroke stroke)]
bestMatch tag sollus (longestKey, getStroke) =
    -- Try with the specific tag, otherwise fall back to no tag.
    Seq.head (find tag prefixes) <|> Seq.head (find Nothing prefixes)
    where
    find tag = mapMaybe (\s -> getStroke tag s)
    prefixes = reverse $ drop 1 $ List.inits $ take longestKey sollus

exactMatch :: Maybe Solkattu.Tag -> [sollu] -> GetStroke sollu stroke
    -> Maybe [Maybe (Stroke stroke)]
exactMatch tag sollus (_, getStroke) =
    getStroke tag sollus <|> getStroke Nothing sollus


-- TODO duplicated in Format

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
