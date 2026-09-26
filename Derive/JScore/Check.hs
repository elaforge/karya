-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
{-# LANGUAGE CPP #-}
module Derive.JScore.Check (
    -- * transform
    Transform
    , convertLaras
    -- * check
    , CheckM
    , Bias(..)
    -- * integrate
    , Event
    , forIntegrate
    , iramaDivisor
    , instrumentMultiplier
    , instrumentOctave
    , standardNames
    -- * Meta
    , Meta(..)
    , collectMetas
    , makeMeta
    -- * format
    , Block
    , formatScore
#ifdef TESTING
    , inferOctave
    , resolveTokens
    , stretchEvent
#endif
) where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.Logger as Logger
import qualified Util.Num as Num

import qualified Derive.JScore.T as T

import           Global


-- * transform

type Transform = T.Pitch -> T.Pitch

convertLaras :: T.Laras -> T.Laras -> Maybe Transform
convertLaras a b = case (a, b) of
    _ | a == b -> Just id
    (T.PelogLima, T.PelogBarang) -> Just oneToSeven
    (T.PelogBarang, T.PelogLima) -> Just sevenToOne
    (T.SlendroManyura, T.PelogBarang) -> Just oneToSeven
    (T.PelogBarang, T.SlendroManyura) -> Just sevenToOne

    -- slendro-manyura -> pelog-barang -> pelog-lima
    (T.SlendroManyura, T.PelogLima) -> Just id
    (T.PelogLima, T.SlendroManyura) -> Just id

    -- TODO pelog-nem to lima by transposing down one, not sure.
    -- can I go lima to num by going back up one?
    (T.PelogNem, T.PelogLima) -> Just $ T.addPc T.PelogLima (-1)
    -- TODO maybe?
    (T.SlendroManyura, T.SlendroSanga) -> Just $ T.addPc T.SlendroSanga (-1)
    (T.SlendroSanga, T.PelogNem) -> Just id -- TODO maybe?
    _ -> Nothing
    where
    oneToSeven p@(T.Pitch _ T.P1) = T.addPcAbs (-1) p
    oneToSeven p = p
    sevenToOne p@(T.Pitch _ T.P7) = T.addPcAbs 1 p
    sevenToOne p = p

-- * check

type CheckM a = Logger.Logger T.Error a

warn :: T.Pos -> Text -> CheckM ()
warn pos msg = Logger.log (T.Error pos msg)

data Bias = BiasStart | BiasEnd
    deriving (Show, Eq)

-- * integrate

type Event = (T.Time, T.Note T.Pitch T.Time)

forIntegrate :: Bias -> T.ParsedScore
    -> CheckM [(Meta, T.Block T.Pitch [[Event]])]
forIntegrate bias score = mapMaybeM block (collectMetas score)
    where
    block (pos, (Nothing, _)) = do
        warn pos "skipping, incomplete meta"
        pure Nothing
    block (_, (Just meta, block)) = do
        tracks <- mapM (resolveTokens meta bias . T.trackTokens) $
            case T.blockTracks block of
                Nothing -> []
                Just (T.Tracks tracks) -> tracks
        let blockGatra = resolveGatraPitch (T.blockGatra block)
        pure $ Just (meta, block { T.blockGatra, T.blockTracks = tracks })

resolveTokens :: Meta -> Bias -> [T.ParsedToken] -> CheckM [Event]
resolveTokens (Meta { mIrama, mInstrument }) bias =
    fmap (adjust . resolveDurations bias) . normalizeBarlines bias
        . resolvePitch
    where
    adjust = map (second addOct . stretchEvent stretch)
    addOct n = n { T.notePitch = T.addOct oct (T.notePitch n) }
    -- 1 bar is normalized to 1t.  irama_bars is how many of them per gatra.
    -- Then *4 so 1 gatra is 4t.
    stretch = 4 * recip (fromIntegral (iramaBars mIrama))
    oct = instrumentOctave mInstrument

stretchEvent :: T.Time -> Event -> Event
stretchEvent stretch (start, note) =
    ( stretch * start
    , note { T.noteDuration = stretch * T.noteDuration note }
    )

-- | How many bars are conventionally written for 1 gatra in the given irama.
iramaBars :: T.Irama -> Int
iramaBars = \case
    T.Lancar -> 2
    T.Tanggung -> 2
    T.Dadi -> 4
    T.Wiled -> 4
    T.Rangkep -> 8

-- | How many notes per gatra for peking in the given irama.
iramaDivisor :: T.Irama -> Int
iramaDivisor = \case
    T.Lancar -> 4
    T.Tanggung -> 8
    T.Dadi -> 16
    T.Wiled -> 32
    T.Rangkep -> 64

-- TODO
-- .3.2 puthut-semedi, barung has 4*8 = 32, but basic rhythm may be 4*4 = 16
-- panerus has 4*8 = 32 which is half.

-- | How many notes for the instrument relative to peking.
instrumentMultiplier :: T.Instrument -> Int
instrumentMultiplier = \case
    T.GenderBarung -> 1
    T.GenderPanerus -> 2
    T.Siter -> 2

-- | Absolute octave of the center where T.Octave = 0.
instrumentOctave :: T.Instrument -> T.Octave
instrumentOctave = \case
    T.GenderBarung -> 3
    T.GenderPanerus -> 4
    T.Siter -> 4

-- * Meta

data Meta = Meta
    { mLaras :: T.Laras
    , mIrama :: T.Irama
    , mInstrument :: T.Instrument
    } deriving (Show, Eq)

instance Pretty Meta where
    pretty (Meta laras irama instrument) =
        "Meta:" <> Text.intercalate ","
            [showt laras, showt irama, showt instrument]

collectMetas :: T.Score (T.Block pitch tracks)
    -> [(T.Pos, (Maybe Meta, T.Block pitch tracks))]
collectMetas (T.Score tops) = go [] tops
    where
    go metas = \case
        [] -> []
        (_, T.ToplevelMeta meta) : toplevels -> go (meta : metas) toplevels
        (pos, T.BlockDefinition block) : toplevels ->
            (pos, (makeMeta metas, block)) : go metas toplevels

makeMeta :: [T.Meta] -> Maybe Meta
makeMeta metas = Meta
    <$> Lists.head [a | T.Laras a <- metas]
    <*> Lists.head [a | T.Irama a <- metas]
    <*> Lists.head [a | T.Instrument a <- metas]

-- * format

type Block = T.Block T.Pitch [[Token]]
type Token = T.Token T.Pos (T.Note T.Pitch ()) T.Rest

-- TODO: format_xyz is "check for formatting", not actually format.  Find
-- less confusing name.

-- | This takes the parsed score to a somewhat more normalized version.  So not
-- the same as converting to tracklang, because I don't need actual times and
-- durations.  Also I have to 'normalize_hands' to make sure they are at the
-- same "zoom", but it's only since I don't want to convert from times back to
-- notes.
formatScore :: T.ParsedScore -> CheckM (T.Score Block)
formatScore score = T.Score <$> mapM format (collectMetas score)
    where
    format (pos, (mbMeta, block)) = do
        -- Must 'resolve_pitch' before 'infer_chord'.
        block <- formatBlock block
        block <- normalizeName mbMeta pos block
        let mbInst = mInstrument <$> mbMeta
        let mbLaras = mLaras <$> mbMeta
        whenJust mbInst $ \inst -> checkRange inst block
        whenJust ((,) <$> mbInst <*> mbLaras) $ \(inst, laras) ->
            checkPitch inst laras block
        pure (pos, T.BlockDefinition block)

formatBlock :: T.ParsedBlock -> CheckM Block
formatBlock block = do
    blockTracks <- case T.blockTracks block of
        -- TODO or I could do resolve_names first?  Still may fail to resolve.
        Nothing -> pure []
        Just tracks -> formatTracks tracks
    let blockGatra = resolveGatraPitch (T.blockGatra block)
    pure $ block { T.blockGatra, T.blockTracks }

formatTracks :: T.Tracks -> CheckM [[Token]]
formatTracks (T.Tracks tracks) = case map T.trackTokens tracks of
    [rights, lefts] -> do
        lefts <- formatTokens bias lefts
        rights <- formatTokens bias rights
        (lefts, rights) <- normalizeHands bias lefts rights
        checkHands lefts rights
        pure [rights, lefts]
    -- TODO error for two handed instruments?
    tracks -> mapM (formatTokens bias) tracks
    where
    bias = BiasStart

formatTokens :: Bias -> [T.ParsedToken]
    -> CheckM [T.Token T.Pos (T.Note T.Pitch ()) T.Rest]
formatTokens bias = normalizeBarlines bias . resolvePitch
    -- This doesn't do 'resolve_durations', because I want the original rests.

-- * normalize names

-- | Un-abbreviate standard cengkok names.
normalizeName :: Maybe Meta -> T.Pos
    -> T.Block pitch [[T.Token pos (T.Note T.Pitch dur) rest]]
    -> CheckM (T.Block pitch [[T.Token pos (T.Note T.Pitch dur) rest]])
normalizeName mbMeta pos block = do
    names <- case T.blockNames block of
        [] -> pure ["seleh"]
        name : names -> case Map.lookup name fromAbbr of
            Just name -> pure $ name : names
            Nothing -> do
                unless (isValidName name) $
                    warn pos $ "unknown name: " <> name
                pure $ name : names
    pure $ block { T.blockNames = names, T.blockInferred = inferred }
    where
    inferred
        | inst == Just T.GenderBarung =
            case inferChord laras (T.blockTracks block) of
                Nothing -> []
                Just (chord, pc) ->
                    [ Text.toLower $ showt chord
                    , Text.singleton (T.pcChar pc)
                    ]
        | otherwise = []
    laras = maybe T.PelogLima mLaras mbMeta
    inst = mInstrument <$> mbMeta
    fromAbbr = Map.fromList
        [ (abbr, name)
        | (name, Just abbr) <- Map.toList standardNames
        ]

data Chord = Kempyung | Gembyang
    deriving (Eq, Show)

inferChord :: T.Laras -> [[T.Token pos (T.Note T.Pitch dur) rest]]
    -> Maybe (Chord, T.PitchClass)
inferChord laras tracks = case map finalPitch tracks of
    [Just high, Just low]
        | T.pitchPc high == T.pitchPc low
        && T.pitchOctave high == T.pitchOctave low + 1 ->
            Just (Gembyang, T.pitchPc low)
        | T.addPc laras 3 low == high -> Just (Kempyung, T.pitchPc low)
    _ -> Nothing

finalPitch :: [T.Token pos (T.Note pitch dur) rest] -> Maybe pitch
finalPitch =
    msum . map pitchOf . take 3 . filter (not . isBarline) . reverse
    -- Only consider a few final notes, sometimes a final chord is offset,
    -- but not by much.
    where
    isBarline (T.TBarline {}) = True
    isBarline _ = False
    pitchOf (T.TNote _ note) = Just $ T.notePitch note
    pitchOf _ = Nothing

-- | Names like gantung-3 are also valid.
isValidName :: Text -> Bool
isValidName name =
    Set.member name names
        || Text.all Char.isDigit post && Set.member (Text.dropEnd 1 pre) names
    where
    names = Map.keysSet standardNames
    (pre, post) = Text.breakOnEnd "-" name

standardNames :: Map Text (Maybe Text)
standardNames = Map.fromList $ map (second Just)
    [ ("ayu-kuning", "ak")

    , ("debyang-debyung", "dd")
    , ("dualolo", "dll")
    , ("gelut", "g")
    , ("jarik-kawung", "jk")
    , ("kacaryan", "kc")
    , ("kutuk-kuning", "kk")
    -- For gender barung I can infer kempyung, but not other instruments.
    , ("kutuk-kuning-kempyung", "kkp")
    , ("puthut", "p") -- 2 gatra pattern puthut gelut
    , ("puthut-semedi", "ps")
    , ("tumurun", "tm")
    ] ++ map (, Nothing)
    [ "buko"
    , "duduk"
    , "gantung"
    , "rambatan"

    -- The Balungan article on gender panerus thinks these are 2 gatra
    -- patterns.
    , "ayu", "kuning"
    , "debyang", "debyung"
    , "duduk-a", "duduk-b"
    , "kacaryan-a", "kacaryan-b"
    , "rujak", "rujakan"
    , "ora", "butuh"
    ]

-- * resolve_pitch

resolvePitch
    :: [T.Token pos (T.Note T.ParsedPitch dur) rest]
    -> [T.Token pos (T.Note T.Pitch dur) rest]
resolvePitch = snd . List.mapAccumL resolve (0, Nothing)
    where
    resolve prev = \case
        T.TBarline pos -> (prev, T.TBarline pos)
        T.TRest pos rest -> (prev, T.TRest pos rest)
        T.TNote pos note ->
            ( (oct, Just pc)
            , T.TNote pos $ note { T.notePitch = pitch }
            )
            where
            pitch@(T.Pitch oct pc) = inferOctave prev (T.notePitch note)

-- | No octave inferring, the octave is exactly as notated.  Maybe this is more
-- useful for balungan, which more often stays within the central register?  Or
-- not?
resolveGatraPitchSimple :: T.Gatra T.ParsedPitch -> T.Gatra T.Pitch
resolveGatraPitchSimple (T.Gatra p1 p2 p3 p4) =
    T.Gatra (r p1) (r p2) (r p3) (r p4)
    where
    r (T.Balungan p annot) = T.Balungan (convert <$> p) annot
    convert (T.ParsedPitch oct pc) = T.Pitch oct pc

resolveGatraPitch :: T.Gatra T.ParsedPitch -> T.Gatra T.Pitch
resolveGatraPitch (T.Gatra p1 p2 p3 p4) =
    case snd $ List.mapAccumL resolve (0, Nothing) [p1, p2, p3, p4] of
        [p1, p2, p3, p4] -> T.Gatra p1 p2 p3 p4
        -- TODO use Traversable StateT instead of mapAccumL?
        _ -> error "expected exactly 4"
    where
    resolve prev (T.Balungan Nothing annot) = (prev, T.Balungan Nothing annot)
    resolve prev (T.Balungan (Just p) annot) =
        ((oct, Just pc), T.Balungan (Just pitch) annot)
        where pitch@(T.Pitch oct pc) = inferOctave prev p

-- Initial prev_oct is based on the instrument.
-- Similar to Check.infer_octave, but uses simpler 'Pitch' instead of
-- 'Perform.Pitch.Pitch'.
inferOctave :: (T.Octave, Maybe T.PitchClass) -> T.ParsedPitch -> T.Pitch
inferOctave (prevOct, Nothing) (T.ParsedPitch relOct pc) =
    T.Pitch (prevOct + relOct) pc
inferOctave (prevOct, Just prevPc) (T.ParsedPitch relOct pc) =
    case compare relOct 0 of
        -- If distances are equal, favor downward motion.  But since PitchClass
        -- has an odd number, this never happens.  If I omit P4, it could
        -- though, but then I should also omit P1 or P7.
        EQ
            | prevPc == pc -> T.Pitch prevOct pc
            | otherwise -> Lists.minOn (abs . T.pitchDiff prev) above below
        GT -> T.addOct (relOct-1) above
        LT -> T.addOct (relOct+1) below
    where
    prev = T.Pitch prevOct prevPc
    -- If I'm moving up, then I don't need to increment the octave to be above.
    above = T.Pitch (if pc > prevPc then prevOct else prevOct+1) pc
    below = T.Pitch (if pc < prevPc then prevOct else prevOct-1) pc

-- * resolve_durations

-- | Put absolute T.Time on starts and durations.  Normalized such that
-- one bar is 1.
resolveDurations :: Bias -> [T.Token pos (T.Note pitch dur) T.Rest]
    -> [(T.Time, T.Note pitch T.Time)]
resolveDurations bias =
    Maybe.catMaybes . snd . List.mapAccumL resolveRests 0 . Lists.zipNexts
        . concatMap resolve . Lists.split isBarline
    where
    resolve bar = mapMaybe (addDur len) bar
        where len = length bar
    addDur len = \case
        T.TBarline {} -> Nothing
        T.TRest _ rest -> Just (dur, Left rest)
        T.TNote _ note -> Just (dur, Right note)
        where dur = T.Time (1 / fromIntegral len)
    isBarline (T.TBarline {}) = True
    isBarline _ = False
    -- If BiasEnd, each note goes from the *end* of its duration until the
    -- start of the next note, or non-sustaining rest.  This gives them the
    -- end-bias.
    resolveRests time ((dur, Left _), _) = (time + dur, Nothing)
    resolveRests time ((dur, Right n), nexts) =
        ( time + dur
        , Just $ case bias of
            BiasEnd -> (time + dur, n { T.noteDuration = sustain })
            BiasStart -> (time, n { T.noteDuration = dur + sustain })
        )
        where
        -- The final note will have 0 dur, which I can look for and set.
        -- If the final note is a sustain rest, then use its duration, not 0.
        -- TODO Not sure if this is useful, maybe it should be sustain duration
        -- plus the default end dur?  Maybe I should take a default end-note
        -- duration?
        sustain = Num.sum $ case bias of
            BiasEnd -> untilNote nexts
            BiasStart -> map fst $ takeWhile isSustain nexts
    isSustain (_, Left (T.Rest { restSustain })) = restSustain
    isSustain _ = False
    untilNote [] = []
    untilNote ((dur, n) : ns) = case n of
        Left (T.Rest { restSustain }) ->
            dur : if restSustain then untilNote ns else []
        Right (T.Note {}) -> [dur]

-- | Verify bar durations, infer rests if necessary.  After this, all bars
-- should be a power of 2.
normalizeBarlines :: Bias -> [T.Token T.Pos (T.Note pitch T.HasSpace) T.Rest]
    -> CheckM [T.Token T.Pos (T.Note pitch ()) T.Rest]
normalizeBarlines bias = mapBars $ \case
    [] -> pure []
    bar@(t : _)
        | powerOf2 (length bar) -> pure $ map strip bar
        | powerOf2 (length inferred) -> pure $ map strip inferred
        | otherwise -> do
            warn (T.tokenPos t) $
                "bar not a power of 2: " <> showt (length bar)
                <> ", with inferred rests: " <> showt (length inferred)
            pure $ map strip bar
        where
        inferred = inferRests bias bar
        strip = T.mapNote (\n -> n { T.noteDuration = () })

-- | The T.Tokens given to the function will not contain T.TBarline.
-- I could put it in the type, but it seems too much bother.
mapBars :: Monad m => ([T.Token pos n1 r1] -> m [T.Token pos n2 r2])
    -> [T.Token pos n1 r1] -> m [T.Token pos n2 r2]
mapBars f tokens = concat . add <$> mapM f (pre : map snd posts)
    where
    add [] = []
    add (t:ts) = t : zipWith (:) bars ts
    bars = map (T.TBarline . fst) posts
    (pre, posts) = Lists.splitWith isBarline tokens
    isBarline (T.TBarline pos) = Just pos
    isBarline _ = Nothing

powerOf2 :: Int -> Bool
powerOf2 n = snd (properFraction (logBase 2 (fromIntegral n))) == 0

-- | If there aren't enough notes in the bar, try inferring a rest before
-- every odd group of notes followed by space.
inferRests :: Bias
    -> [T.Token T.Pos (T.Note pitch T.HasSpace) T.Rest]
    -> [T.Token T.Pos (T.Note pitch T.HasSpace) T.Rest]
inferRests bias = concatMap infer . Lists.splitAfter hasSpace
    where
    infer tokens
        | even (length tokens) = tokens
        | bias == BiasEnd = extraRest : tokens
        | otherwise = tokens ++ [extraRest]
    extraRest = T.TRest T.fakePos $ T.Rest True T.HasSpace
    hasSpace = \case
        T.TBarline {} -> True
        T.TNote _ note -> T.noteDuration note == T.HasSpace
        T.TRest _ rest -> T.restSpace rest == T.HasSpace

-- | Split on barlines, zipPadded, map across them.
normalizeHands :: Bias
    -> [T.Token T.Pos note T.Rest] -> [T.Token T.Pos note T.Rest]
    -> CheckM ([T.Token T.Pos note T.Rest], [T.Token T.Pos note T.Rest])
normalizeHands bias lefts rights =
    fmap (bimap joinBars joinBars . unzip) $ mapM normalize $
    Lists.zipPadded (splitBars lefts) (splitBars rights)
    where
    normalize = \case
        Lists.First (pos, lefts) -> do
            warn pos "left hand with no right hand"
            pure ((pos, lefts), (pos, []))
        Lists.Second (pos, rights) -> do
            warn pos "right hand with no left hand"
            pure ((pos, []), (pos, rights))
        Lists.Both (pos0, lefts) (pos1, rights) ->
            case (log2 (length lefts), log2 (length rights)) of
                (Nothing, _) -> do
                    warn pos0 "not power of 2"
                    pure ((pos0, lefts), (pos1, rights))
                (_, Nothing) -> do
                    warn pos1 "not power of 2"
                    pure ((pos0, lefts), (pos1, rights))
                (Just d1, Just d2)
                    | delta >= 0 ->
                        pure ((pos0, lefts), (pos1, expand pos1 delta rights))
                    | otherwise ->
                        pure ((pos0, expand pos0 delta lefts), (pos1, rights))
                    where delta = d1 - d2
    expand pos delta = case bias of
        BiasEnd -> concatMap (\n -> replicate (delta^2) (rest pos) ++ [n])
        BiasStart -> concatMap (: replicate (delta^2) (rest pos))
    rest pos = T.TRest pos (T.Rest True T.NoSpace)

log2 :: Int -> Maybe Int
log2 n
    | frac == 0 = Just i
    | otherwise = Nothing
    where (i, frac) = properFraction $ logBase 2 (fromIntegral n)

splitBars :: [T.Token pos note rest] -> [(pos, [T.Token pos note rest])]
splitBars [] = []
splitBars tokens@(t0 : _) = (T.tokenPos t0, group0) : groups
    where
    (group0, groups) = Lists.splitWith isBarline tokens
    isBarline (T.TBarline pos) = Just pos
    isBarline _ = Nothing

joinBars :: [(pos, [T.Token pos note rest])] -> [T.Token pos note rest]
joinBars [] = []
joinBars ((_, g0) : gs) = concat $ g0 : map join gs
    where join (pos, tokens) = T.TBarline pos : tokens

-- * check pitches

instrumentRange :: T.Instrument -> (T.Pitch, T.Pitch)
instrumentRange = \case
    T.GenderBarung -> (T.Pitch (-2) T.P6, T.Pitch 1 T.P3)
    T.GenderPanerus -> (T.Pitch (-2) T.P6, T.Pitch 1 T.P3)
    T.Siter -> (T.Pitch (-2) T.P2, T.Pitch 1 T.P3)

has1 :: T.Laras -> Bool
has1 = (/= T.PelogBarang)

has4 :: T.Instrument -> Bool
has4 = \case
    _ -> False -- if I have slenthem or saron or something I'll have True

has7 :: T.Laras -> Bool
has7 = (== T.PelogBarang)

checkRange :: T.Instrument -> T.Block pitch [[Token]] -> CheckM ()
checkRange inst = mapM_ (mapM_ check) . T.blockTracks
    where
    check = \case
        T.TNote pos note ->
            unless (low <= p && p <= high) $
                warn pos $ "out of range for " <> showt inst
                    <> ": " <> pretty p
            where p = T.notePitch note
        _ -> pure ()
    (low, high) = instrumentRange inst

checkPitch :: T.Instrument -> T.Laras -> T.Block pitch [[Token]] -> CheckM ()
checkPitch inst laras = mapM_ (mapM_ check) . T.blockTracks
    where
    check = \case
        T.TNote pos note -> do
            when (not (has4 inst) && pc == T.P4) $
                warn pos $ showt inst <> " doesn't have 4"
            when (not (has1 laras) && pc == T.P1) $
                warn pos $ showt laras <> " doesn't have 1"
            when (not (has7 laras) && pc == T.P7) $
                warn pos $ showt laras <> " doesn't have 7"
            where pc = T.pitchPc $ T.notePitch note
        _ -> pure ()

checkHands :: [Token] -> [Token] -> CheckM ()
checkHands lefts rights = mapM_ check $ Lists.zipPadded lefts rights
    where
    check = \case
        Lists.Both (T.TNote pos left) (T.TNote _ right)
            | pl >= pr ->
                warn pos $ "left hand >= right: " <> pretty pl <> " >= "
                    <> pretty pr
            | otherwise -> pure ()
            where
            pl = T.notePitch left
            pr = T.notePitch right
        _ -> pure ()
