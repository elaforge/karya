-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Calls for reyong and trompong techniques.
--
-- >   1        2     3        4
-- >  /-------\/----\/-------\/----\--\
-- > 4e 4u 4a 5i 5o 5e 5u 5a 6i 6o 6e 6u
-- > 3  5  6  1  2  3  5  6  1  2  3  5
module Derive.C.Bali.Reyong (
    library
    , cek
#ifdef TESTING
    , module Derive.C.Bali.Reyong
#endif
) where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Num as Num

import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Bali.Gangsa as Gangsa
import qualified Derive.C.Bali.Gender as Gender
import qualified Derive.C.Post.Postproc as Postproc
import qualified Derive.Call as Call
import qualified Derive.Call.GraceUtil as GraceUtil
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.StaticMacro as StaticMacro
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Flags as Flags
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.Types as Types

import           Global
import           Types


{-
    Kotekan uses separate calls for the cycle and the \"pickup\" preparation.
    It would be nicer to do it automatically, but sometimes pickups appear in
    irregular places, so I still need a manual option.

    The pickup is actually a transitition, since it may be different depending
    on source and destination pitches.

       3<------2<------1<------1
    +++3<--3+++2<--2+++1<------1
    3353535322323232112121212121
-}

module_ :: Module.Module
module_ = "bali" <> "reyong"

library :: Library.Library
library = mconcat
    [ Library.generators
        [ ("kilit", realizePattern norotPatterns)
        , (">kilit", realizePattern norotPreparePatterns)
        -- kilit is probably redundant now that I have norot.
        , ("norot", cNorot Nothing)
        , ("nt", cNorot Nothing)
        , ("nt>", cNorot (Just True))
        , ("nt-", cNorot (Just False))
        , ("k//", cKotekanRegular False (Just "-12-12-1") (Just Call.Down))
        , ("k\\\\", cKotekanRegular False (Just "-21-21-21") (Just Call.Up))
        , ("k_\\", realizePattern $ reyongPattern "-44-43-4" "-11-1-21")
        , ("k//\\\\", realizePattern $
            reyongPattern "-4-34-3-43-434-3" "-12-12-21-21-12-")
        , ("k", cKotekanRegular False Nothing Nothing)
        , ("k^", cKotekanRegular True Nothing Nothing)
        , ("t", cTumpuk)
        , ("a", cTumpukAuto)
        , ("o", cByong)
        , (":", cPitches [Pitch.pitch 4 2, Pitch.pitch 5 0])
        , ("/", articulation "cek-loose" ((:[]) . posCek) (cek <> Attrs.open))
        , ("//", articulation "cek-loose"
            (replicate 2 . posCek) (cek <> Attrs.open))
        , ("X", articulation "cek" ((:[]) . posCek) cek)
        , ("XX", articulation "cek" (replicate 2 . posCek) cek)
        , ("O", articulation "byong" posByong mempty)
        , ("-", articulation "byut-loose" posByong (Attrs.mute <> Attrs.open))
        , ("+", articulation "byut" posByong Attrs.mute)

        , ("n1", cSolkattuNote [0])
        , ("n2", cSolkattuNote [1])
        , ("n3", cSolkattuNote [2])
        , ("n4", cSolkattuNote [3])
        , ("n14", cSolkattuNote [0, 3])
        ]
    , Library.generators $ Gender.ngoretVariations cNgoret
    , Library.transformers
        [ ("infer-damp", cInferDamp)
        , ("hand-damp", cHandDamp)
        , ("cancel-kotekan", cCancelKotekan)
        , ("realize-ngoret", Derive.setModule module_ Gender.cRealizeNgoret)
        , ("realize-reyong", cRealizeReyong)
        , ("realize-trompong", cRealizeTrompong)
        , ("vv", cLowerOctaveNote)
        ]
    , Library.both
        [ ("lv", Make.attributedNote module_ undamped)
        , ("upper", cUpper)
        ]
    ]
    where articulation = makeArticulation reyongPositions

cek :: Attrs.Attributes
cek = Attrs.attr "cek"

reyongPattern :: [Char] -> [Char] -> Pattern
reyongPattern above below = makePattern $ parseKotekan above below

cNgoret :: Sig.Parser (Maybe Pitch.Transpose) -> Derive.Generator Derive.Note
cNgoret = Gender.ngoret module_ False
    (pure $ Typecheck.RealTimeFunction $ const (DeriveT.RealDuration 0))

voicesEnv :: Sig.Parser [Voice]
voicesEnv = Sig.environ "voices" Sig.Both ([] :: [Sig.Dummy])
    "Only emit notes for these positions, from 1 to 4. Empty means all of them."

-- * tumpuk

cTumpuk :: Derive.Generator Derive.Note
cTumpuk = Derive.generator module_ "tumpuk" Tags.inst "Pile up notes together."
    $ Sig.call ((,,)
    <$> Sig.required "notes"
        ("Articulations, from " <> Doc.literal (txt (Map.keys articulations))
        <> ", pitches from `edcba0123456789`, or a space for a rest.")
    <*> Sig.defaulted "dur" (0.1 :: Double) "Duration of each note."
    <*> placeEnv
    ) $ \(notes, dur, place) -> Sub.inverting $ \args -> do
        notes <- Derive.requireRight id $ parseTumpuk (untxt notes)
        tumpuk args place dur notes

tumpuk :: Derive.PassedArgs Score.Event -> Typecheck.Normalized -> RealTime
    -> [TumpukNote] -> Derive.NoteDeriver
tumpuk args place dur notes = do
    (start, end) <- Args.realRange args
    prev <- traverse Derive.real $ Args.prevStart args
    pitch <- Call.getPitch start
    realizeTumpuk prev start end place (Args.prevEventPitch args) pitch dur
        notes

placeEnv :: Sig.Parser Typecheck.Normalized
placeEnv = Sig.environ "place" Sig.Both (1 :: Double)
    "At 0, grace notes fall before their base note.  At 1, grace notes fall on\
    \ the base note, and the base note is delayed."

-- | Dyn 0 means a rest.
type TumpukNote = (TumpukPitch, Attrs.Attributes, Dyn)
data TumpukPitch = Transpose Pitch.Step | Prev deriving (Eq, Show)
type Dyn = Signal.Y

realizeTumpuk :: Maybe RealTime -> RealTime -> RealTime
    -> Typecheck.Normalized -> Maybe PSignal.Pitch -> PSignal.Pitch
    -> RealTime -> [TumpukNote] -> Derive.NoteDeriver
realizeTumpuk prev eventStart eventEnd place prevPitch eventPitch dur
        notes =
    mconcatMap realize $ filter ((>0) . noteDyn . fst) $ zip notes extents
    where
    extents = GraceUtil.fitGraceDurs place
        prev eventStart eventEnd (length notes) dur
    noteDyn (_, _, dyn) = dyn
    realize ((tpitch, attrs, dyn), (realStart, dur)) = do
        start <- Derive.score realStart
        end <- Derive.score (realStart + dur)
        pitch <- case tpitch of
            Transpose steps -> pure $ Pitches.transposeD steps eventPitch
            Prev -> Derive.require "no prev pitch" prevPitch
        Derive.place start (end-start) $ Call.multiplyDynamic dyn $
            Call.addAttributes attrs $ Call.pitchedNote pitch

parseTumpuk :: [Char] -> Either Text [TumpukNote]
parseTumpuk =
    fmap (Maybe.catMaybes . snd) . Lists.mapAccumLM parse (Transpose 0)
    where
    parse pitch c = case Map.lookup c articulations of
        Just (attrs, dyn) -> Right (pitch, Just (pitch, attrs, dyn))
        Nothing -> case Num.readDigit c <|> lookup c negative of
            Just steps -> Right (Transpose steps, Nothing)
            Nothing
                | c == 'p' -> Right (Prev, Nothing)
                | otherwise -> Left $ "unknown articulation: " <> showt c
    negative = [('a', -1), ('b', -2), ('c', -3), ('d', -4), ('e', -5)]

-- | These more or less correspond to the group articulations:
--
-- > /   X   -   +   o
-- >         m-  nx  .o
articulations :: Map Char (Attrs.Attributes, Dyn)
articulations = Map.fromList
    [ (' ', (mempty, 0))
    , ('.', (mempty, 0.75))
    , ('o', (mempty, 1))
    , ('m', (Attrs.mute <> Attrs.open, 0.75))
    , ('-', (Attrs.mute <> Attrs.open, 1))
    , ('n', (Attrs.mute, 0.75))
    -- I would use +, but if you start with one it parses as an attr.
    , ('x', (Attrs.mute, 1))
    ]

cTumpukAuto :: Derive.Generator Derive.Note
cTumpukAuto = Derive.generator module_ "tumpuk-auto" Tags.inst
    "A variant of `tumpuk` that randomly picks a pattern."
    $ Sig.call placeEnv $ \place -> Sub.inverting $ \args -> do
        randoms <- Call.randoms
        let rnd1 : rnd2 : _ = randoms -- randoms is infinite
        eventDur <- Args.realDuration args
        (notes, dur) <- Derive.requireRight id $
            selectPattern eventDur rnd1 rnd2
        tumpuk args place dur notes

-- | If dur is long, use the slow end of the patterns.  If dur is short, try to
-- fit the pattern in the first 2/3, dropping patterns if they exceed that at
-- their fastest.
selectPattern :: RealTime -> Double -> Double
    -> Either Text ([TumpukNote], RealTime)
selectPattern dur rnd1 rnd2 = justErr err $ select fitsWell <|> select fits
    where
    select ps = case NonEmpty.nonEmpty ps of
        Nothing -> Nothing
        Just ps -> Just $ pickDur (Call.pick ps rnd1)
    pickDur (p, (slow, fast)) = (p, dur)
        where
        dur = Num.scale (Num.clamp slow fast minSlow) fast
            (RealTime.seconds rnd2)
        minSlow = desiredDur / fromIntegral (length p)
    err = "no patterns fit duration " <> pretty dur
    -- Try to take up only this much dur with tumpuk notes, but take up to
    -- the full duration if nothing fits.
    desiredDur = dur * (2/3)
    fitsWell = filter ((<= desiredDur) . patternDur) tumpukPatterns
    fits = filter ((<= dur) . patternDur) tumpukPatterns
    patternDur (p, (_, fast)) = fromIntegral (length p) * fast

-- | Pattern and usable time range.
-- TODO probably I can vary -=, nx, and .o, or just randomize the dyn for
-- non-final notes.
tumpukPatterns :: [([TumpukNote], (RealTime, RealTime))]
tumpukPatterns = expectRight $ mapM (firstA parseTumpuk)
    [ ("p-0o", (1/10, 1/14))
    , ("p-0.o", (1/12, 1/18))
    , ("p.0.o", (1/14, 1/18))
    , ("1.0.o", (1/12, 1/18))
    , ("p.0.pm0o", (1/18, 1/20))
    ]
    where
    expectRight = either (errorStack . ("tumpuk_patterns: "<>)) id
    firstA f (a, c) = (,) <$> f a <*> pure c

-- * c_byong

cByong :: Derive.Generator Derive.Note
cByong = Derive.generator module_ "byong" Tags.inst
    "Play the byong notes, but only for the current voice, and following\
    \ normal damping rules."
    $ Sig.call0 $ Sub.inverting $ \args -> do
        voice <- Derive.getVal EnvKey.voice
        position <- Derive.require ("unknown position: " <> showt voice) $
            Lists.at reyongPositions (voice - 1 :: Int)
        (_, showPitch, _) <- Call.getPitchFunctions
        let realize = Call.place args
                . realizeNote showPitch voice (Args.start args)
        mconcatMap (realize . (, mempty)) (posByong position)

cPitches :: [Pitch.Pitch] -> Derive.Generator Derive.Note
cPitches pitches = Derive.generator module_ "pitches" Tags.inst
    ("Play notes for each pitch: " <> ShowVal.doc pitches
        <> ". Really only for `4e` and `5i` for the penyorog.")
    $ Sig.call0 $ Sub.inverting $ \args ->
        mconcatMap (Call.place args . realize (Args.start args)) pitches
    where
    realize start pitch =
        Call.transposedPitchedNote =<< Call.evalPitch_ start pitch

-- * cancel

cCancelKotekan :: Derive.Transformer Derive.Note
cCancelKotekan = Derive.transformer module_ "cancel-kotekan" Tags.postproc
    "This is like the `cancel` call, except it understands flags set by\
    \ kotekan, and cancels based on reyong voice." $
    Postproc.makeCancel Gangsa.cancelStrongFinal Post.voiceKey

-- * kilitan

cNorot :: Maybe Bool -> Derive.Generator Derive.Note
cNorot defaultPrepare =
    Derive.generator module_ "norot" Tags.inst "Reyong norot."
    $ Sig.call ((,,,)
    <$> Sig.defaulted "prepare" defaultPrepare
        "Whether or not to prepare for the next pitch. If Nothing, infer based\
        \ on the next note."
    <*> Gangsa.durEnv <*> Gangsa.inferInitialFinalEnv <*> voicesEnv
    ) $ \(prepare, noteDur, initialFinal, voices) -> Sub.inverting $
    \args -> do
        (pitch, showPitch) <- getParsedPitch args
        nextPitch <- inferPrepare args prepare
        let orientation = Args.orientation args
        let (sustainParams, prepareParams) = Gangsa.prepareSustain
                (Maybe.isJust nextPitch) noteDur initialFinal
                orientation (Args.range args)
        sustain <- case sustainParams of
            Nothing -> pure mempty
            Just (initialFinal, range) ->
                realizePositions
                    (realizeNotes range orientation initialFinal
                        showPitch Gangsa.Repeat noteDur)
                    voices norotPatterns pitch
        prepare <- case (,) <$> nextPitch <*> prepareParams of
            Nothing -> pure Nothing
            Just (next, (initialFinal, range)) ->
                Just <$> realizePositions
                    (realizeNotes range orientation initialFinal
                        showPitch Gangsa.Once noteDur)
                    voices norotPreparePatterns next
        maybe sustain (sustain<>) prepare

realizePositions :: ((Voice, note) -> Derive.NoteDeriver)
    -> [Voice] -> Map Pitch.PitchClass [note]
    -> Pitch.Pitch -> Derive.Deriver Derive.NoteDeriver
realizePositions realize voices patterns pitch = do
    positions <- Derive.require
        ("no pattern for pitch: " <> pretty pitch)
        (Map.lookup (Pitch.pitchPc pitch) patterns)
    pure $ mconcatMap realize (filterVoices voices positions)

-- | Kilitan is implemented as a set of patterns indexed by an absolute pitch
-- degree.  The patterns are similar to kotekan, except with absolute pitches,
-- and without a polos \/ sangsih division.
realizePattern :: Pattern -> Derive.Generator Derive.Note
realizePattern pattern =
    Derive.generator module_ "reyong" Tags.inst "Emit reyong pattern."
    $ Sig.call ((,,)
    <$> Gangsa.durEnv <*> Gangsa.inferInitialFinalEnv <*> voicesEnv)
    $ \(dur, initialFinal, voices) -> Sub.inverting $ \args -> do
        (pitch, showPitch) <- getParsedPitch args
        positions <- Derive.require ("no pattern for pitch: " <> pretty pitch)
            (Map.lookup (Pitch.pitchPc pitch) pattern)
        mconcatMap
            (realizeNotesArgs args initialFinal showPitch Gangsa.Repeat dur)
            (filterVoices voices positions)

-- | Select the given Voice indices.
filterVoices :: [Voice] -> [a] -> [(Voice, a)]
filterVoices voices positions
    | null voices = zip [1..] positions
    | otherwise = filter ((`elem` voices) . fst) (zip [1..] positions)

-- * kotekan

cKotekanRegular :: Bool -> Maybe Text -> Maybe Call.UpDown
    -> Derive.Generator Derive.Note
cKotekanRegular inverted maybeKernel maybeDir =
    Derive.generator module_ "kotekan" Tags.inst
    ("Render a kotekan pattern from a kernel representing the polos.\
    \ The sangsih is inferred. This can emit notes at both the beginning and\
    \ end of the event, so use `cancel-kotekan` to cancel the extras.")
    $ Sig.call ((,,,,)
    <$> maybe
        (Sig.defaultedEnv "kernel" Sig.Both ("k-12-1-21" :: Text) kernelDoc)
        pure maybeKernel
    <*> maybe
        (Sig.defaulted "dir" Call.Up
            "Inferred part is above or below the explicit one.")
        pure maybeDir
    <*> Gangsa.durEnv <*> Gangsa.inferInitialFinalEnv <*> voicesEnv
    ) $ \(kernelS, dir, dur, initialFinal, voices) -> Sub.inverting $
    \args -> do
        kernel <- Derive.requireRight id $ Gangsa.makeKernel (untxt kernelS)
        (pitch, showPitch) <- getParsedPitch args
        pattern <- Derive.require "empty pattern" $
            kernelToPattern dir
                (if inverted then Gangsa.invert kernel else kernel)
        let positions = kotekanPattern perOctave
                (map posCek reyongPositions) pattern (Pitch.pitchPc pitch)
        mconcatMap
            (realizeNotesArgs args initialFinal showPitch Gangsa.Repeat dur)
            (filterVoices voices positions)
    where perOctave = 5

kernelDoc :: Doc.Doc
kernelDoc = "Transposition steps for the part that ends on the destination\
    \ pitch. It should consist of `-`, `1`, and `2`. You can start with `k` to\
    \ avoid needing quotes. Starting with `k` will also require the length to\
    \ be a multiple of 4."

realizeNotesArgs :: Derive.PassedArgs a -> (Maybe Bool, Bool)
    -> (Pitch.Pitch -> Maybe Pitch.Note) -> Gangsa.Repeat -> ScoreTime
    -> (Voice, [[Note]]) -> Derive.NoteDeriver
realizeNotesArgs args initialFinal =
    realizeNotes (Args.range args) (Args.orientation args)
        (Gangsa.inferInitial args initialFinal)

realizeNotes :: (ScoreTime, ScoreTime) -> Types.Orientation -> (Bool, Bool)
    -> (Pitch.Pitch -> Maybe Pitch.Note) -> Gangsa.Repeat -> ScoreTime
    -> (Voice, [Chord]) -> Derive.NoteDeriver
realizeNotes (start, end) orientation initialFinal showPitch repeat dur
        (voice, position) =
    Gangsa.realizeNotes (realizeNote showPitch voice start) $
        Gangsa.realizePattern repeat orientation initialFinal start end dur
            (const position)

kernelToPattern :: Call.UpDown -> Gangsa.Kernel -> Maybe KotekanPattern
kernelToPattern direction kernel = do
    -- Reyong doesn't really have polos and sangsih, so here polos is the
    -- one that has the destination.
    let polos = map (fmap (+offset) . toSteps) kernel
        sangsih = map (fmap (+offset) . inferSangsih) kernel
    sangsihLast <- msum (reverse sangsih)
    polosLast <- msum (reverse polos)
    pure $ case direction of
        Call.Up -> KotekanPattern
            { kotekanAbove = (sangsih, sangsihLast)
            , kotekanBelow = (polos, polosLast)
            }
        Call.Down -> KotekanPattern
            { kotekanAbove = (polos, polosLast)
            , kotekanBelow = (sangsih, sangsihLast)
            }
    where
    -- I want the final note to end on the destination, whether it be polos or
    -- sangsih.  So shift everything to make that happen.  If it's Gap then I
    -- have no idea, leave it alone.
    offset = negate $ fromMaybe 0 $ case Lists.last kernel of
        Just Gangsa.Gap -> inferSangsih Gangsa.Gap
        Just a -> toSteps a
        Nothing -> Nothing
    toSteps a = case a of
        Gangsa.Gap -> Nothing
        Gangsa.Rest -> Nothing
        Gangsa.Low -> Just 0
        Gangsa.High -> Just 1
    inferSangsih a = case direction of
        Call.Up -> case a of
            Gangsa.Gap -> Nothing
            Gangsa.Rest -> Just 2
            Gangsa.Low -> Just 3
            Gangsa.High -> Nothing
        Call.Down -> case a of
            Gangsa.Gap -> Nothing
            Gangsa.Rest -> Just (-1)
            Gangsa.Low -> Nothing
            Gangsa.High -> Just (-2)

-- | Like 'Gangsa.get_pitch', but get the symbolic pitch.
getParsedPitch :: Derive.PassedArgs a
    -> Derive.Deriver (Pitch.Pitch, Pitch.Pitch -> Maybe Pitch.Note)
getParsedPitch args = do
    (parsePitch, showPitch, _) <- Call.getPitchFunctions
    pitch <- Call.getParsedPitch parsePitch =<< Args.realStart args
    pure (pitch, showPitch)

-- | Like 'Gangsa.infer_prepare', except return a parsed Pitch.Pitch.
inferPrepare :: Derive.PassedArgs a -> Maybe Bool
    -> Derive.Deriver (Maybe Pitch.Pitch)
inferPrepare args prepare = do
    (parsePitch, _, _) <- Call.getPitchFunctions
    justm (Gangsa.inferPrepare args prepare) $ \_ ->
        maybe (pure Nothing) (Args.lookupParsedPitchAt parsePitch)
            (Args.nextStart args)

-- * articulation

makeArticulation :: [Position] -> Derive.CallName
    -> (Position -> [Pitch.Pitch]) -> Attrs.Attributes
    -> Derive.Generator Derive.Note
makeArticulation positions name getNotes attrs =
    Derive.generator module_ name Tags.inst
    "Reyong articulation. The doubled variants emit two notes, and rely on\
    \ start time randomization so they're not exactly simultaneous." $
    Sig.call voicesEnv $ \voices -> Sub.inverting $ \args -> do
        (_, showPitch, _) <- Call.getPitchFunctions
        mconcat $ map (realize showPitch args) (filterVoices voices positions)
    where
    realize showPitch args (voice, position) = hands $ map
        (Call.place args . realizeNote showPitch voice (Args.start args)) $
        map (\p -> (p, attrs)) (getNotes position)
    hands [n] = n
    hands [n1, n2] = hand Call.L n1 <> hand Call.R n2
        where hand = Derive.withVal EnvKey.hand . ShowVal.showVal
    hands ns = mconcat ns

type Voice = Int

realizeNote :: (Pitch.Pitch -> Maybe Pitch.Note) -> Voice -> ScoreTime
    -> Note -> Derive.NoteDeriver
realizeNote showPitch voice start (pitch, attrs) =
    -- TODO I could maybe get rid of all the show_pitch args by using
    -- Call.eval_pitch_, but would that be inefficient?
    Call.addAttributes attrs $ Derive.withVal EnvKey.voice voice $
        Call.transposedPitchedNote =<< Call.evalPitch showPitch start pitch


-- * kotekan

data KotekanPattern = KotekanPattern
    -- | Pair relative steps with the final note's distance from the
    -- destination pitch.  This is used to find this line's distance from
    -- a given position.
    { kotekanAbove :: !([Maybe Pitch.Step], Pitch.Step)
    , kotekanBelow :: !([Maybe Pitch.Step], Pitch.Step)
    } deriving (Show)

{- What about the high ding?

    I still want to do high ding automatically, but it depends on the speed.
    How about leave it out for now, and add it later.

    "44-34-3- 43-434-3" "-12-12-2 1-21-12(3)"
    Says to play the (3) if someone else isn't playing it, e.g. if it's
    being played on the top position.

    Or I could write both and filter out duplicate notes in favor of the
    lower position.  But that makes above and below have the same 'last',
    where the polos part should be considered closer.
-}

type Pattern = Map Pitch.PitchClass [[Chord]]
type Chord = [Note]
type Note = (Pitch.Pitch, Attrs.Attributes)

-- | Figure out a pattern for each note of the scale.
makePattern :: KotekanPattern -> Pattern
makePattern pattern = Map.fromList
    [ (pc, kotekanPattern perOctave (map posCek reyongPositions) pattern pc)
    | pc <- pcs
    ]
    where
    perOctave = 5
    pcs = [0 .. perOctave - 1]

kotekanPattern :: Pitch.PitchClass -> [Pitch.Pitch] -> KotekanPattern
    -> Pitch.PitchClass -> [[Chord]]
kotekanPattern perOctave centers pattern pc =
    map (map convert) $ assignPositions pattern perOctave pc centers
    where convert = maybe [] (\p -> [(p, mempty)])

assignPositions :: KotekanPattern -> Pitch.PitchClass -> Pitch.PitchClass
    -> [Pitch.Pitch] -> [[Maybe Pitch.Pitch]]
assignPositions (KotekanPattern above below) perOctave destination =
    map extract . assignClosest distance absolute
    where
    -- Kotekan at theoretical positions in every octave.
    absolute = concatMap (\dest -> map (transpose dest) [below, above])
        [Pitch.Pitch oct (Pitch.Degree destination 0) | oct <- [0..]]
    transpose pitch (steps, last) =
        (map (fmap (add pitch)) steps, add pitch last)
    add pitch steps = Pitch.addPc perOctave steps pitch
    extract ((steps, _), _) = steps
    distance (_, last) center = abs $ Pitch.diffPc perOctave last center

-- | Find the value from the first list that is closest to the first element of
-- the first list, and then zip them up.
assignClosest :: Ord key => (a -> b -> key) -> [a] -> [b] -> [(a, b)]
assignClosest distance = go
    where
    go [] _ = []
    go _ [] = []
    go (x1 : xs@(x2:_)) (y:ys)
        | distance x1 y < distance x2 y = (x1, y) : zip xs ys
        | otherwise = go xs (y:ys)
    go [x] (y:_) = [(x, y)]

parseKotekan :: [Char] -> [Char] -> KotekanPattern
parseKotekan above below = KotekanPattern
    { kotekanAbove = (map (fmap (subtract dest)) abovep, aboveLast - dest)
    , kotekanBelow = (map (fmap (subtract dest)) belowp, belowLast - dest)
    }
    where
    (abovep, belowp) = (parseRelative above, parseRelative below)
    Just aboveLast = msum (reverse abovep)
    Just belowLast = msum (reverse belowp)
    Just dest = msum $ mapMaybe Lists.last [belowp, abovep]

parseRelative :: [Char] -> [Maybe Pitch.Step]
parseRelative = map parse1 . filter (/=' ')
    where
    parse1 '-' = Nothing
    parse1 c = Just (digit c)
    digit c = fromMaybe
        (errorStack $ "Reyong.parse_kotekan: not a digit: " <> showt c)
        (Num.readDigit c)

-- ** absolute

-- | Map a char to the notes it represents for a certain position.  Each
-- position has different set of notes available.  This is just to interpret a
-- mini-notation for each position.
type NoteTable = Map Char Chord

-- | Saih lima pitch degree.
data Degree = I | O | E | U | A deriving (Eq, Ord, Enum, Show)
instance Pretty Degree where pretty = showt

toPc :: Degree -> Pitch.PitchClass
toPc = fromEnum

parseAbsolute :: NoteTable -> [Char] -> [Chord]
parseAbsolute table = map $ \c -> fromMaybe
    (errorStack $ "parse_absolute: not in table: " <> showt c)
    (Map.lookup c table)

parseNote :: NoteTable -> Char -> Note
parseNote table = extract . head . parseAbsolute table . (:[])
    where
    extract [x] = x
    extract xs = errorStack $ "parse_note: expected only one: " <> pretty xs

{-
    1 penyorog        3 ponggang
             2 pengenter    4 pemetit
    |------|---       |---|---
             |------|---    |---------|
    4e 4u 4a 5i 5o 5e 5u 5a 6i 6o 6e 6u 6a 7i
-}
norotPreparePatterns :: Map Pitch.PitchClass [[Chord]]
norotPreparePatterns = Map.fromList $ zip [0..] $ map parse byDegree
    where
    parse = zipWith parseAbsolute (map posTable reyongPositions)
    byDegree =
        -- 1       2       3       4
        [ ["eua-", "Iioi", "Uua-", "Iioi"] -- i
        , ["ua:-", "Ooeo", "uai-", "Ooeo"] -- o
        , ["Eeue", "Eeie", "uau-", "Eeue"] -- e
        , ["Uuau", "ioe-", "Uuau", "ioeu"] -- u
        , ["Aaea", "Eeie", "Aa-a", "Eeie"] -- a
        ]

norotPatterns :: Map Pitch.PitchClass [[Chord]]
norotPatterns = Map.fromList $ zip [0..] $ map parse byDegree
    where
    parse = zipWith parseAbsolute (map posTable reyongPositions)
    byDegree =
        -- 1       2       3       4
        [ ["aua-", "oioi", "aua-", "oioi"] -- i
               -- i oeio
        , [":-:-", "eoeo", "iai-", "eoeo"] -- o
        , ["ueue", "ieie", "u-u-", "ueue"] -- e
        , ["auau", "eoe-", "auau", "eueu"] -- u
        , ["eaea", "ieie", "-a-a", "ieie"] -- a
        ]

-- | Map letters to chords, starting from the given octave and Degree.  Capital
-- letters get 'Attrs.mute'.
noteTable :: Pitch.Octave -> Degree -> NoteTable
noteTable octave start = Map.fromList $
    ('-', []) : take (length notes) (drop (toPc start) pitches)
        ++ take (length notes) muted
    where
    pitches =
        [ (char, [(Pitch.Pitch oct (Pitch.Degree pc 0), mempty)])
        | oct <- [octave..], (pc, char) <- zip [0..] notes
        ]
    muted = [(Char.toUpper char, mute notes) | (char, notes) <- pitches]
    mute = map (fmap (<>Attrs.mute))
    notes = "ioeua"

data Position = Position
    { posCek :: !Pitch.Pitch
    , posByong :: ![Pitch.Pitch]
    , posTable :: !NoteTable
    } deriving (Eq, Show)

reyongPositions :: [Position]
reyongPositions = [position1, position2, position3, position4]

position1 :: Position
position1 = makePosition ptable 'u' "ea"
    where
    ptable = Map.insert ':' (map (parseNote table) "ei") table
        where table = noteTable 4 E

position2 :: Position
position2 = makePosition (noteTable 5 I) 'o' "ie"

position3 :: Position
position3 = makePosition (noteTable 5 U) 'a' "ui"

position4 :: Position
position4 = makePosition (noteTable 6 I) 'e' "ou"

makePosition :: NoteTable -> Char -> [Char] -> Position
makePosition table cek byong = Position
    { posCek = parse cek
    , posByong = map parse byong
    , posTable = table
    }
    where parse = fst . parseNote table

-- * damping

cHandDamp :: Derive.Transformer Derive.Note
cHandDamp = Derive.transformer module_ "hand-damp" Tags.postproc
    "Damping when the parts are already divided by hand."
    $ Sig.callt ((,)
        <$> Sig.required "insts" "Apply damping to these instruments."
        <*> Sig.defaulted "dur" (0.15 :: RealTime)
            "There must be at least this much time between the end of a note\
            \ and the start of the next to damp."
    ) $ \(insts, dur) _args deriver ->
        handDamp (Set.fromList insts) dur <$> deriver

handDamp :: Set ScoreT.Instrument -> (RealTime -> RealTime)
    -> Stream.Stream Score.Event -> Stream.Stream Score.Event
handDamp dampedInsts durAt =
    Post.emapAsc_ infer . Post.nextBy Post.handKey
    where
    infer (event, _)
        | Score.eventInstrument event `Set.notMember` dampedInsts = [event]
    infer (event, Just next) | tooClose event next = [event]
    infer (event, _) = damp event
    damp event = [event, makeDamp 0 event]
    tooClose event next =
        (Score.eventStart next - Score.eventEnd event)
            <= durAt (Score.eventEnd event)

cInferDamp :: Derive.Transformer Derive.Note
cInferDamp = Derive.transformer module_ "infer-damp" Tags.postproc
    ("Add damping for reyong parts based on a simulation of the hand technique.\
    \ The " <> ShowVal.doc damped <> " attribute will force a damp, while "
    <> ShowVal.doc undamped <> " will prevent damping. The latter can cause a\
    \ previously undamped note to become damped because the hand is now freed\
    \ up.\
    \\nThe output is additional notes with `+mute` and zero duration at note\
    \ end.")
    $ Sig.callt ((,,)
        <$> Sig.required "insts" "Apply damping to these instruments."
        <*> Sig.defaulted "dur" (0.15 :: RealTime)
            "This is how fast the player is able to damp. A note is only damped\
            \ if there is a hand available which has this much time to move\
            \ into position for the damp stroke, and then move into position\
            \ for its next note afterwards."
        <*> Sig.defaulted "early" (0.025 :: RealTime)
            "Damp this much before the next note, if it would be simultaneous\
            \ with the next start."
    ) $ \(insts, dur, early) _args deriver ->
        -- infer_damp preserves order, so Post.apply is safe.  TODO Is there a
        -- way to express this statically?
        Post.applyM
            (Derive.runLogs . inferDampVoices (Set.fromList insts) dur early)
            =<< deriver

-- | Multiply this by 'Controls.dynamic' for the dynamic of +mute notes created
-- by infer-damp.
dampControl :: ScoreT.Control
dampControl = "damp"

-- | Divide notes into voices.  Assign each note to a hand.  The end of each
-- note needs a free hand to damp.  That can be the same hand if the next note
-- with that hand is a sufficiently long time from now, or the opposite hand if
-- it is not too busy.
inferDampVoices :: Set ScoreT.Instrument
    -> (RealTime -> RealTime) -- ^ duration required to damp
    -> (RealTime -> RealTime) -> [Score.Event] -> Log.LogId [Score.Event]
inferDampVoices dampedInsts durAt earlyAt events = do
    unless (null skipped) $
        Log.warn $ "skipped events without pitch: "
            <> Score.shortEvents skipped
    pure damped
    where
    (skipped, damped) = bimap concat (Post.mergeAsc . map Post.mergeAsc)
        . unzip . map inferVoice . Lists.keyedGroupSort Post.voiceKey $
        events
    inferVoice ((inst, _voice), events)
        | inst `Set.notMember` dampedInsts = ([], [events])
        | otherwise =
            (skipped, zipWith inferEvent damps (Lists.zipNext events))
        where (damps, skipped) = inferDamp durAt events
    inferEvent (hand, damped) (event, next) =
        map (Post.addEnviron EnvKey.hand hand) $ if damped
            then [Score.duration (subtract early) event, makeDamp early event]
            else [event]
        where
        early = case next of
            Just n | Score.eventEnd event >= Score.eventStart n ->
                earlyAt (Score.eventStart event)
            _ -> 0

-- | Create a damped note at the end of the given note.
makeDamp :: RealTime -> Score.Event -> Score.Event
makeDamp early event =
    Score.addAttributes Attrs.mute $ Score.setDynamic damp $
    event
        { Score.eventStart = Score.eventEnd event - early
        , Score.eventDuration = 0
        }
    where
    damp = maybe 0.35 ScoreT.valOf
        (Score.controlAt (Score.eventEnd event) dampControl event)

inferDamp :: (RealTime -> RealTime) -> [Score.Event]
    -> ([(Call.Hand, Bool)], [Score.Event])
    -- ^ (True if corresponding input event should be damped, skipped)
inferDamp durAt =
    first (snd . List.mapAccumL infer (0, 0) . Lists.zipNexts) . assignHands
    where
    infer prev ((hand, event), nexts) = (handsState, (hand, damp))
        where
        damp = Score.hasAttribute damped event
            || (couldDamp event
                && (sameHandCanDamp || otherHandCanDamp))
        -- The same hand can damp if its next strike is sufficiently distant.
        sameHandCanDamp = enoughTime (next hand)
        -- The other hand can damp if it has enough time from its previous
        -- strike to move, and the current hand has moved out of the way by
        -- changing pitches.
        -- TODO maybe doesn't need a full dur from prev strike?
        otherHandCanDamp = and
            [ (now - prevStrike (Call.otherHand hand)) >= dur
            , enoughTime (next (Call.otherHand hand))
            , maybe True ((/= Score.initialNn event) . Score.initialNn . snd)
                (next hand)
            ]
        now = Score.eventEnd event
        prevStrike Call.L = fst prev
        prevStrike Call.R = snd prev
        handsState
            | damp = case hand of
                Call.L -> (now, snd prev)
                Call.R -> (fst prev, now)
            | otherwise = prev
        next hand = Lists.head $ filter ((==hand) . fst) nexts
        enoughTime = maybe True
            ((>=dur) . subtract now . Score.eventStart . snd)
        dur = durAt (Score.eventStart event)

-- | True for events which could get an inferred damp.
couldDamp :: Score.Event -> Bool
couldDamp event =
    Score.eventDuration event > 0
        && not (any (Attrs.contain attrs) [undamped, cek, Attrs.mute])
    where attrs = Score.eventAttributes event

damped :: Attrs.Attributes
damped = Attrs.attr "damped"

undamped :: Attrs.Attributes
undamped = Attrs.attr "undamped"

-- | Assign hands based on the direction of the pitches.  This is a bit
-- simplistic but hopefully works well enough.
assignHands :: [Score.Event] -> ([(Call.Hand, Score.Event)], [Score.Event])
assignHands =
    first (snd . List.mapAccumL assign (Call.L, 999))
        . Lists.partitionOn (\e -> (,e) <$> Score.initialNn e)
    where
    assign (prevHand, prevPitch) (pitch, event) =
        ((hand, pitch), (hand, event))
        where
        hand
            | pitch == prevPitch = prevHand
            | pitch > prevPitch = Call.R
            | otherwise = Call.L

-- * patterns

baris :: [([Char], [Char])]
baris =
    [ ( "-e-oe-eo-eo-oe-oeo-eo-oe-oe-eo-e"
      , "-ai-aia-ia-i-ai-a-ia-i-ai-aia-ia"
      )
    ]

-- * realize-reyong

cRealizeReyong :: Derive.Transformer Derive.Note
cRealizeReyong = StaticMacro.check "c_realize_reyong" $
    StaticMacro.transformer module_ "realize-reyong" Tags.postproc doc
        -- infer-damp relies on having the ngoret pitches and will get confused
        -- by uncalled notes, so it has to go after those.
        [ StaticMacro.Call cInferDamp [StaticMacro.Var, StaticMacro.Var]
        , StaticMacro.Call cCancelKotekan [StaticMacro.Var]
        , StaticMacro.Call Gender.cRealizeNgoret []
        ]
    where doc = "Combine the reyong realize calls in the right order."

cRealizeTrompong :: Derive.Transformer Derive.Note
cRealizeTrompong = StaticMacro.check "c_realize_trompong" $
    StaticMacro.transformer module_ "realize-trompong" Tags.postproc doc
        -- infer-damp relies on having the ngoret pitches and will get confused
        -- by uncalled notes, so it has to go after those.
        [ StaticMacro.Call cHandDamp [StaticMacro.Var, StaticMacro.Var]
        , StaticMacro.Call Gender.cRealizeNgoret []
        ]
    where doc = "Combine the reyong realize calls in the right order."

-- * octave transposition

cLowerOctaveNote :: Derive.Transformer Derive.Note
cLowerOctaveNote = Derive.transformer module_ "lower-octave-note"
    (Tags.postproc <> Tags.underInvert)
    "Double a note with a single note one octave down, and add\
    \ 'Derive.Flags.infer_duration'."
    $ Sig.call0t $ Sub.underInvert $ \args deriver -> do
        start <- Args.realStart args
        pitch <- Call.getPitch start
        let note = Call.addFlags Flags.inferDuration $
                Derive.at (Args.start args) $ Call.pitchedNote $
                PSignal.addControl Controls.octave (-1) pitch
        -- The transposed goes second so realize-ngoret infers to the upper
        -- pitch rather than the lower one.  This is kind of subtle and
        -- unsatisfying, but works.
        deriver <> note

cUpper :: Library.Calls Derive.Note
cUpper = Make.transformNotes module_ "upper" Tags.inst
    ("Double a part with `" <> Doc.pretty Controls.octave
        <> "=+1` and `" <> Doc.pretty EnvKey.voice
        <> "=2`. If reyong subtracks have `v=+1` and `v=+2` respectively,\
        \ they'll wind up with the right voices.")
    (pure ()) $ \() deriver ->
        deriver <> Call.addConstant Controls.octave 1
            (Derive.withVal EnvKey.voice (2 :: Int) deriver)

-- * solkattu

solkattuModule :: Module.Module
solkattuModule = module_ <> "solkattu"

-- | These calls are used by the 'Derive.Solkattu.Instrument.Reyong'
-- realization.
cSolkattuNote :: [Pitch.Step] -> Derive.Generator Derive.Note
cSolkattuNote steps = Derive.generator solkattuModule "solkattu-note"
    Tags.inst "A pitched note, as generated by reyong solkattu."
    $ Sig.call0 $ Sub.inverting $ \args ->
        mconcatMap (realize (Args.start args) (Args.next args)) steps
    where
    realize start next step =
        Derive.place start (next - start) $
        Call.addConstant Controls.diatonic (fromIntegral step)
        Call.note
