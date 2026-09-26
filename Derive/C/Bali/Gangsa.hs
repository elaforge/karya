-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{- | Calls for gangsa techniques.  Gangsa come in polos and sangsih pairs,
    and play either kotekan patterns or play unison or parallel parts.

    Kotekan patterns have a number of features in common.  They are all
    transpositions from a base pitch.  Rhythmically, they consist of notes with
    a constant duration, that line up at the end of an event's range, and the
    last duration is negative (i.e. implicit, depending on the next note).
    They use polos and sangsih and may switch patterns when the a kotekan speed
    threshold is passed.  Notes are also possibly muted.

    There are a number of ways this can be extended:

    - Use any attribute instead of just mute.
    - More instruments than just polos and sangsih.
    - Multiple kotekan thresholds.

    The first two are supported at the 'KotekanNote' level of abstraction.  For
    the others, I have to either directly use 'Note's or create a new
    abstraction:

    - Variable durations.
-}
module Derive.C.Bali.Gangsa where
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import           GHC.Stack (HasCallStack)

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty

import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Bali.Gender as Gender
import qualified Derive.C.Post.Postproc as Postproc
import qualified Derive.Call as Call
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.StaticMacro as StaticMacro
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Flags as Flags
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.Event as Event
import qualified Ui.Types as Types

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.generators
        [ ("norot", cNorot False Nothing)
        -- Alias for norot.  It's separate so I can rebind this locally.
        , ("nt", cNorot False Nothing)
        , ("nt-", cNorot False (Just False))
        , ("nt<", cNorot True Nothing)
        , ("nt<-", cNorot True (Just False))
        , ("gnorot", cGenderNorot)
        , ("k_\\", cKotekanIrregular Pat $ irregularPattern $
            IrregularPattern
            { irPolos              = "-11-1321"
            , irSangsih4           = "-44-43-4"
            , irPolosNgotek       = "-11-1-21"
            , irSangsihNgotek3    = "3-32-32-"
            , irSangsihNgotek4    = "-44-43-4"
            })
        , ("k-\\", cKotekanIrregular Pat $ irregularPattern $
            IrregularPattern
            { irPolos              = "211-1321"
            , irSangsih4           = "-44-43-4"
            , irPolosNgotek       = "211-1-21"
            , irSangsihNgotek3    = "3-32-32-"
            , irSangsihNgotek4    = "-44-43-4"
            })
        , ("k//\\\\", cKotekanIrregular Pat $ irregularPattern $
            IrregularPattern
            { irPolos              = "-123123213213123"
            , irSangsih4           = "-423423243243423"
            , irPolosNgotek       = "-12-12-21-21-12-"
            , irSangsihNgotek3    = "3-23-232-32-3-23"
            , irSangsihNgotek4    = "-4-34-3-43-434-3"
            })
        -- There are two ways to play k\\, either 21321321 or 31321321.  The
        -- first one is irregular since sangsih starts on 2 but there's no
        -- unison polos.
        , ("k\\\\", cKotekanIrregular Telu $ irregularPattern $
            IrregularPattern
            { irPolos              = "21321321"
            , irSangsih4           = "24324324"
            , irPolosNgotek       = "-1-21-21"
            , irSangsihNgotek3    = "2-32-32-"
            , irSangsihNgotek4    = "-43-43-4"
            })
        , ("k//", cKotekanIrregular Telu $ irregularPattern $
            IrregularPattern
            { irPolos              = "23123123"
            , irSangsih4           = "20120120"
            , irPolosNgotek       = "-3-23-23"
            , irSangsihNgotek3    = "2-12-12-"
            , irSangsihNgotek4    = "-01-01-0"
            })
        , ("k\\\\2", cKotekanRegular False (Just "-1-21-21") Telu)
        , ("k//2",   cKotekanRegular False (Just "-2-12-12") Telu)
        -- This is k// but with sangsih above.
        -- TODO maybe a more natural way to express this would be to make k//
        -- understand sangsih=u?  But then I also need sangsih=d for k\\,
        -- irregular_pattern support for sangsih direction, and they both become
        -- irregular.
        , ("k//^",   cKotekanRegular False (Just "2-12-12-") Telu)

        , ("kotekan", cKotekanKernel)
        , ("k", cKotekanRegular False Nothing Telu)
        , ("k^", cKotekanRegular True Nothing Telu)
        , ("ke", cKotekanExplicit)
        ]
    , Library.generators $ Gender.ngoretVariations cNgoret
    , Library.transformers
        [ ("i+", Make.environVal module_ "i+" "initial" True
            "Kotekan calls will emit a note on the initial beat.")
        , ("i-", Make.environVal module_ "i-" "initial" False
            "Kotekan calls won't emit a note on the initial beat.")
        , ("f-", Make.environVal module_ "f-" "final" False
            "Kotekan calls won't emit a final note at the end time.")
        , ("unison", cUnison)
        , ("noltol", cNoltol)
        , ("realize-gangsa", cRealizeGangsa)
        , ("realize-noltol", cRealizeNoltol)
        , ("realize-ngoret", Derive.setModule module_ Gender.cRealizeNgoret)
        , ("cancel-pasang", cCancelPasang)
        ]
    , Library.both
        [ ("nyog", cNyogcag)
        , ("kempyung", cKempyung)
        , ("k+", cKempyung) -- short version for single notes
        , ("p+", cDeriveWith "p+" True False)
        , ("s+", cDeriveWith "s+" False True)
        , ("ps+", cDeriveWith "ps+" True True)
        ]
    ]

module_ :: Module.Module
module_ = "bali" <> "gangsa"

cDeriveWith :: Derive.CallName -> Bool -> Bool -> Library.Calls Derive.Note
cDeriveWith name withPolos withSangsih =
    Make.transformNotes module_ name Tags.inst
    "Derive the note with polos, sangsih, or both." pasangEnv $
    \pasang deriver -> mconcat $ concat
        [ [Derive.withInstrument (polos pasang) deriver | withPolos]
        , [Derive.withInstrument (sangsih pasang) deriver | withSangsih]
        ]

-- * instrument postproc

-- | Variable mute for gangsa.  Intended for the 'Cmd.Cmd.inst_postproc' field.
-- This interprets 'Controls.mute' and turns it into either a @%mod@ control or
-- @mute_attr@.
mutePostproc :: Attrs.Attributes -> Score.Event -> (Score.Event, [Log.Msg])
mutePostproc muteAttr event = (,[]) $
    case Score.controlAt (Score.eventStart event) Controls.mute event of
        Nothing -> setMod 0 event
        Just tval
            | mute >= threshold -> Score.addAttributes muteAttr event
            | mute <= 0 -> setMod 0 event
            -- The mod control goes from 1 (least muted) to 0 (most muted).
            -- Bias mod towards the higher values, since the most audible
            -- partial mutes are from .75--1.
            | otherwise -> setMod (1 - mute**2) $ Score.setDuration 0 event
            where
            mute = ScoreT.valOf tval
    where
    setMod = Score.setControl Controls.mod . ScoreT.untyped . Signal.constant
    -- Use the mute_attr above this threshold.
    threshold = 0.85

-- * ngoret

cNgoret :: Sig.Parser (Maybe Pitch.Transpose) -> Derive.Generator Derive.Note
cNgoret = Gender.ngoret module_ False $
    Sig.defaulted "damp" (0.15 :: RealTime)
    "Time that the grace note overlaps with this one. So the total\
    \ duration is time+damp, though it will be clipped to the\
    \ end of the current note."

-- * patterns

-- | There are 4 ways to realize a kotekan:
--
-- 1. Undivided.  Since it's undivided it could be unison or kempyung.
-- 2. Slow but divided.  Play all the notes, but sangsih and polos are kempyung
-- on the outer notes.
-- 3, 4. Ngotek, in telu and pat versions.
data KotekanPattern = KotekanPattern
    { kotekanTelu :: ![Maybe Pitch.Step]
    , kotekanPat :: ![Maybe Pitch.Step]
    , kotekanInterlockTelu :: !(Pasang [Maybe Pitch.Step])
    , kotekanInterlockPat :: !(Pasang [Maybe Pitch.Step])
    } deriving (Eq, Show)

instance Pretty KotekanPattern where
    format (KotekanPattern telu pat itelu ipat) = Pretty.record "KotekanPattern"
        [ ("telu", Pretty.format telu)
        , ("pat", Pretty.format pat)
        , ("interlock_telu", Pretty.format itelu)
        , ("interlock_pat", Pretty.format ipat)
        ]

data Pasang a = Pasang { polos :: a, sangsih :: a } deriving (Eq, Show)

instance Pretty a => Pretty (Pasang a) where
    format (Pasang polos sangsih) = Pretty.record "Pasang"
        [ ("polos", Pretty.format polos)
        , ("sangsih", Pretty.format sangsih)
        ]

data Realization a = Realization
    { interlocking :: a
    , nonInterlocking :: a
    } deriving (Eq, Show)

instance Pretty a => Pretty (Realization a) where
    format (Realization inter nonInter) = Pretty.record "Realization"
        [ ("interlocking", Pretty.format inter)
        , ("non_interlocking", Pretty.format nonInter)
        ]

data IrregularPattern = IrregularPattern
    { irPolos :: [Char]
    , irSangsih4 :: [Char]
    , irPolosNgotek :: [Char]
    , irSangsihNgotek3 :: [Char]
    , irSangsihNgotek4 :: [Char]
    } deriving (Show)

irregularPattern :: HasCallStack => IrregularPattern -> KotekanPattern
irregularPattern (IrregularPattern {..}) = KotekanPattern
    { kotekanTelu = parse1 irPolos
    , kotekanPat = parse1 irSangsih4
    , kotekanInterlockTelu = Pasang
        { polos = parse1 irPolosNgotek, sangsih = parse1 irSangsihNgotek3 }
    , kotekanInterlockPat = Pasang
        { polos = parse1 irPolosNgotek, sangsih = parse1 irSangsihNgotek4 }
    }
    where
    -- TODO the HasCallStack doesn't actually work because all these
    -- functions would have to have it too.
    parse1 = parsePattern destination . check
    check ns
        | length ns == length irPolos = ns
        | otherwise = errorStack $ "not same length as polos: " <> showt ns
    destination = fromMaybe (errorStack "no final pitch") $
        Lists.last $ Maybe.catMaybes $ parsePattern 0 irPolos

parsePattern :: HasCallStack => Pitch.Step -> [Char] -> [Maybe Pitch.Step]
parsePattern destination = map (fmap (subtract destination) . parse1)
    where
    parse1 '-' = Nothing
    parse1 c = Just $ fromMaybe (errorStack $ "not a digit: " <> showt c) $
        Num.readDigit c

kotekanPattern :: KotekanPattern -> KotekanStyle -> Pasang ScoreT.Instrument
    -> Cycle
kotekanPattern pattern style pasang = Realization
    { interlocking = realize (interlocking realization)
    , nonInterlocking = realize (nonInterlocking realization)
    }
    where
    realization = patternSteps style pasang pattern
    realize = map (map (uncurry kotekanNote))

patternSteps :: KotekanStyle -> Pasang ScoreT.Instrument -> KotekanPattern
    -> Realization [[(Maybe ScoreT.Instrument, Pitch.Step)]]
patternSteps style pasang (KotekanPattern telu pat itelu ipat) = Realization
    { interlocking = case style of
        Telu -> interlocking itelu
        Pat -> interlocking ipat
    , nonInterlocking = case style of
        Telu -> map (realize Nothing) telu
        Pat -> interlocking (Pasang { polos = telu, sangsih = pat })
    }
    where
    realize inst n = maybe [] ((:[]) . (inst,)) n
    interlocking part =
        [ realize (Just (polos pasang)) p ++ realize (Just (sangsih pasang)) s
        | (p, s) <- zip (polos part) (sangsih part)
        ]

-- ** norot

-- | Initially I implemented this as a postproc, but it now seems to me that
-- it would be more convenient as a generator.  In any case, as a postproc it
-- gets really complicated.
cNorot :: Bool -> Maybe Bool -> Derive.Generator Derive.Note
cNorot startPrepare prepare =
    Derive.generator module_ "norot" Tags.inst
    "Emit the basic norot pattern. Normally it will prepare for the next\
    \ pitch if it touches the next note, the `nt-` variant will suppress that.\
    \ The `nt<` variant will also emit a preparation at the note's start."
    $ Sig.call ((,,,,,)
    <$> Sig.defaulted "style" Default "Norot style."
    <*> durEnv <*> kotekanEnv <*> instrumentTopEnv <*> pasangEnv
    <*> inferInitialFinalEnv
    ) $ \(style, noteDur, kotekan, instTop, pasang, (initial, final))
    -> Sub.inverting $ \args -> do
        nextPitch <- inferPrepare args prepare
        curPitch <- Derive.pitchAt =<< Args.realStart args
        scale <- Call.getScale
        underThreshold <- underThresholdFunction kotekan noteDur
        let getSteps = norotSteps scale instTop style
        let sustainCycle = gangsaNorot style pasang . getSteps
            prepareCycle = gangsaNorotPrepare style pasang . getSteps
        let initialFinal =
                ( fromMaybe (Args.orientation args == Types.Positive) initial
                , final
                )
        norot startPrepare sustainCycle prepareCycle underThreshold
            curPitch nextPitch noteDur initialFinal (Args.range args)

norot :: Bool -> (PSignal.Transposed -> Cycle) -> (PSignal.Transposed -> Cycle)
    -> (ScoreTime -> Bool) -> Maybe PSignal.Pitch -> Maybe PSignal.Pitch
    -> ScoreTime -> (Bool, Bool) -> (ScoreTime, ScoreTime)
    -> Derive.NoteDeriver
norot startPrepare sustainCycle prepareCycle underThreshold
        curPitch nextPitch
        noteDur initialFinal (start, end) = do
    realStart <- Derive.real start
    cycles <- norotSequence startPrepare sustainCycle prepareCycle
        curPitch nextPitch realStart
    let notes = applyInitialFinal start end initialFinal $
            realizeNorot underThreshold noteDur start end cycles
    realizeNotes id (concat notes)

applyInitialFinal :: ScoreTime -> ScoreTime -> (Bool, Bool) -> [[Note a]]
    -> [[Note a]]
applyInitialFinal start end (initial, final) =
    Lists.mapLast modifyFinal
    . (if initial then id else dropWhile (any ((<=start) . noteStart)))
    where
    modifyFinal notes
        | final && any ((>=end) . noteStart) notes =
            map (addFlag (Flags.inferDuration <> finalFlag)) notes
        | otherwise = []

-- | Realize the output of 'norot_sequence'.
realizeNorot :: (ScoreTime -> Bool) -> ScoreTime -> ScoreTime -> ScoreTime
    -> (Maybe PitchedCycle, Maybe PitchedCycle, Maybe PitchedCycle)
    -> [[Note Derive.NoteDeriver]]
realizeNorot underThreshold noteDur initialStart exactEnd
        (prepareThis, sustain, prepareNext) =
    map realize . trim $ concat
        -- This is the initial note, which may be dropped.
        [ onJust sustain $ \(PitchedCycle pitch cycle) ->
            -- There should never be an empty cycle, but might as well be safe.
            onJust (Lists.last (getCycle cycle initialStart)) $ \notes ->
                [(pitch, (initialT, notes))]
        , onJust prepareThis $ \(PitchedCycle pitch cycle) ->
            oneCycle pitch cycle thisT
        , onJust sustain $ \(PitchedCycle pitch cycle) ->
            map (pitch,) $ cycles (getCycle cycle) $
                Lists.range' sustainT nextT noteDur
        , onJust prepareNext $ \(PitchedCycle pitch cycle) ->
            oneCycle pitch cycle nextT
        ]
        -- TODO should I throw an error if I wanted a pitch but couldn't get
        -- it?  I could make a NoteDeriver that throws when evaluated.
    {- i = initial, t = prepare_this, s = sustain, n = prepare_next
        0 1 2 3 4
        1 3 3 4 3
        i n-----n                s = (1, 1), n = (1, 5)

        0 1 2 3 4
        1 2 1 2 1                s = (1, 5)
        i s-----s

        0 1 2 3 4 5 6 7 8
        1 1 1 2 1 3 3 4 3
        i t-----t n-----n        t = (1, 5), s = (5, 5), n = (5, 9)

        0 1 2 3 4 5 6 7 8 9 a
        1 1 1 2 1 2 1 3 3 4 3
        i t-----t s-s n-----n    t = (1, 5), s = (5, 7), n = (7, 11)

        0 1 2 3 4 5 6 7 8 9 a b c d e f 10
        1 1 1 2 1 3 3 4 3 3 3 4 3 4 3 4 3
        |-nt< ----------->|-nt< -------->
        i t-----t n-----n t-----t s-----s
    -}
    where
    onJust val f = maybe [] f val
    -- This is just sequencing the 4 sections, where sustain is stretchy, but
    -- it's complicated because they align to the end.  I'd lay them out
    -- forwards and then shift back, but the times need to be accurate for
    -- get_cycle.
    initialT = min initialStart (thisT - noteDur)
    thisT = min start nextT
    sustainT = start + if Maybe.isJust prepareThis then prepDur else 0
    nextT = end - if Maybe.isJust prepareNext then prepDur else 0
    -- Negative orientation means the logical start and end are shifted forward
    -- by one step.
    start = initialStart + noteDur
    end = exactEnd + noteDur

    trim = takeWhile ((<=exactEnd) . fst . snd)
        . dropWhile ((<initialStart) . fst . snd)
    oneCycle pitch cycle start = map (pitch,) $
        zip (Lists.range_ start noteDur) (getCycle cycle start)
    getCycle cycle t
        | underThreshold t = interlocking cycle
        | otherwise = nonInterlocking cycle
    prepDur = noteDur * 4

    realize :: (PSignal.Pitch, (ScoreTime, [KotekanNote]))
        -> [Note Derive.NoteDeriver]
    realize (pitch, (t, chord)) = map (makeNote t pitch) chord
    makeNote t pitch note = Note
        { noteStart = t
        , noteDuration = noteDur
        , noteFlags = mempty
        , noteData = realizeNote pitch note
        }
    realizeNote pitch (KotekanNote inst steps muted) =
        maybe id Derive.withInstrument inst $
        -- TODO the kind of muting should be configurable.  Or, rather I should
        -- dispatch to a zero dur note call, which will pick up whatever form
        -- of mute is configured.
        -- TODO I'm using 'm' for that now, right?
        (if muted then Call.addAttributes Attrs.mute else id) $
        Call.pitchedNote (Pitches.transposeD steps pitch)

-- | Figure out the appropriate cycles for each norot phase.  There are
-- 3 phases: an optional preparation for the current pitch, a variable length
-- sustain, and an optional preparation for the next pitch.
norotSequence :: Bool
    -> (PSignal.Transposed -> Cycle) -> (PSignal.Transposed -> Cycle)
    -> Maybe PSignal.Pitch -> Maybe PSignal.Pitch -> RealTime
    -> Derive.Deriver (Maybe PitchedCycle, Maybe PitchedCycle,
        Maybe PitchedCycle)
norotSequence startPrepare sustainCycle prepareCycle curPitch nextPitch
        start = do
    -- It's ok for there to be no current pitch, because the sustain might
    -- not be played at all.  But if there's no pitch at all it's probably
    -- better to throw an error than silently emit no notes.
    when (all Maybe.isNothing [curPitch, nextPitch]) $
        Derive.throw "no current pitch and no next pitch"
    prepareThis <- case (startPrepare, curPitch) of
        (True, Just pitch) -> do
            pitchT <- Derive.resolvePitch start pitch
            pure $ Just $ PitchedCycle pitch (prepareCycle pitchT)
        _ -> pure Nothing
    sustain <- case curPitch of
        Nothing -> pure Nothing
        Just pitch -> do
            pitchT <- Derive.resolvePitch start pitch
            pure $ Just $ PitchedCycle pitch (sustainCycle pitchT)
    prepareNext <- case nextPitch of
        Nothing -> pure Nothing
        Just next -> do
            nextT <- Derive.resolvePitch start next
            pure $ Just $ PitchedCycle next (prepareCycle nextT)
    pure (prepareThis, sustain, prepareNext)

data PitchedCycle = PitchedCycle !PSignal.Pitch !Cycle

-- | Figure out parameters for the sustain and prepare phases.
-- Why is this SO COMPLICATED.
--
-- TODO this is still used by Reyong.  If I can simplify reyong norot too
-- then I can get rid of it.
prepareSustain :: Bool -> ScoreTime -> (Maybe Bool, Bool)
    -> Types.Orientation -> (ScoreTime, ScoreTime)
    -> (Maybe ((Bool, Bool), (ScoreTime, ScoreTime)),
        Maybe ((Bool, Bool), (ScoreTime, ScoreTime)))
prepareSustain hasPrepare noteDur (maybeInitial, final) orient
        (start, end) =
    (sustain, prepare)
    where
    sustain
        | hasSustain =
            Just ((initial, if hasPrepare then False else final),
                (start, sustainEnd))
        | otherwise = Nothing
        where
        initial = fromMaybe (orient == Types.Positive) maybeInitial
        sustainEnd = end - if hasPrepare then prepareDur else 0
    prepare
        | hasPrepare =
            Just ((True, final), (end - prepareDur, end))
        | otherwise = Nothing
    dur = end - start
    -- False if all the time is taken up by the prepare.
    -- Default to no initial if this is immediately going into a prepare.  This
    -- is so I can use a 'nt>' for just prepare but line it up on the beat.
    -- I don't actually need this if I expect a plain 'nt>' to be negative.
    hasSustain = not hasPrepare
        || (dur > prepareDur1
            || dur > prepareDur && maybeInitial == Just True)
    prepareDur = noteDur * 3
    prepareDur1 = noteDur * 4

inferPrepare :: Derive.PassedArgs a -> Maybe Bool
    -- ^ True to prepare, False to not, Nothing to prepare if this note touches
    -- the next one.
    -> Derive.Deriver (Maybe PSignal.Pitch)
inferPrepare _ (Just False) = pure Nothing
inferPrepare args (Just True) = Args.lookupNextPitch args
inferPrepare args Nothing
    | Args.nextStart args /= Just (Event.max (Args.event args)) =
        pure Nothing
    | otherwise = Args.lookupNextPitch args

gangsaNorot :: NorotStyle -> Pasang ScoreT.Instrument
    -> Pasang (Pitch.Step, Pitch.Step) -> Cycle
gangsaNorot style pasang steps = Realization
    { interlocking = map (:[]) [s (fst pstep), p (snd pstep)]
    , nonInterlocking = case style of
        Default -> map ((:[]) . both) [fst pstep, snd pstep]
        Diamond ->
            [ [p (fst pstep), s (fst sstep)]
            , [p (snd pstep), s (snd sstep)]
            ]
    }
    where
    both = kotekanNote Nothing
    p = kotekanNote (Just (polos pasang))
    s = kotekanNote (Just (sangsih pasang))
    pstep = polos steps
    sstep = sangsih steps

gangsaNorotPrepare :: NorotStyle -> Pasang ScoreT.Instrument
    -> Pasang (Pitch.Step, Pitch.Step) -> Cycle
gangsaNorotPrepare style pasang steps = Realization
    { interlocking =
        [ [p p2, s p2]
        , [p p2, s p2]
        , [s p1]
        , [p p2]
        ]
    , nonInterlocking = case style of
        Default -> map (:[]) [mutedNote (both p2), both p2, both p1, both p2]
        Diamond ->
            [ map mutedNote [p p2, s s2]
            , [p p2, s s2]
            , [p p1, s s1]
            , [p p2, s s2]
            ]
    }
    where
    both = kotekanNote Nothing
    p = kotekanNote (Just (polos pasang))
    s = kotekanNote (Just (sangsih pasang))
    (p1, p2) = polos steps
    (s1, s2) = sangsih steps

norotSteps :: Scale.Scale -> Maybe Pitch.Pitch -> NorotStyle
    -> PSignal.Transposed
    -- ^ this is to figure out if the sangsih part will be in range
    -> Pasang (Pitch.Step, Pitch.Step)
norotSteps scale instTop style pitch
    | outOfRange 1 = Pasang { polos = (-1, 0), sangsih = (-1, 0) }
    | otherwise = case style of
        Diamond -> Pasang { polos = (1, 0), sangsih = (-1, 0) }
        -- Sangsih is only used if non-interlocking and using Diamond style.
        -- So the snd pair should be ignored.
        Default -> Pasang { polos = (1, 0), sangsih = (1, 0) }
    where
    outOfRange steps = noteTooHigh scale instTop $
        Pitches.transposeD steps pitch

cGenderNorot :: Derive.Generator Derive.Note
cGenderNorot = Derive.generator module_ "gender-norot" Tags.inst
    "Gender-style norot."
    $ Sig.call ((,,,)
    <$> durEnv <*> kotekanEnv <*> pasangEnv <*> inferInitialFinalEnv)
    $ \(dur, kotekan, pasang, initialFinal) -> Sub.inverting $ \args -> do
        pitch <- getPitch args
        underThreshold <- underThresholdFunction kotekan dur
        realizeKotekanPatternArgs args initialFinal
            dur pitch underThreshold Repeat (genderNorot pasang)

genderNorot :: Pasang ScoreT.Instrument -> Cycle
genderNorot pasang = Realization
    { interlocking = [[s 1], [p 0], [s 1], [p 0]]
    , nonInterlocking =
        [ [p (-1), s 1]
        , [p (-2), s 0]
        , [p (-1), s 1]
        , if includeUnison then [p 0, s 0] else [s 0]
        ]
    }
    where
    includeUnison = True -- TODO chance based on signal
    p = kotekanNote (Just (polos pasang))
    s = kotekanNote (Just (sangsih pasang))

-- * kotekan

kotekanDoc :: Doc.Doc
kotekanDoc =
    "Kotekan calls perform a pattern with `inst-polos` and `inst-sangsih`.\
    \ They line up at the end of the event but may also emit a note at the\
    \ start of the event, so use `cancel-pasang` to cancel the extra notes.\
    \ Ngubeng kotekan is naturally suited to positive duration, while majalan\
    \ is suited to negative duration."

cKotekanIrregular :: KotekanStyle -> KotekanPattern
    -> Derive.Generator Derive.Note
cKotekanIrregular defaultStyle pattern =
    Derive.generator module_ "kotekan" Tags.inst
    ("Render a kotekan pattern where both polos and sangsih are explicitly\
    \ specified. This is for irregular patterns.\n" <> kotekanDoc)
    $ Sig.call ((,,,,)
    <$> styleArg defaultStyle
    <*> durEnv <*> kotekanEnv <*> pasangEnv <*> inferInitialFinalEnv
    ) $ \(style, dur, kotekan, pasang, initialFinal) ->
    Sub.inverting $ \args -> do
        pitch <- getPitch args
        underThreshold <- underThresholdFunction kotekan dur
        realizeKotekanPatternArgs args initialFinal
            dur pitch underThreshold Repeat
            (kotekanPattern pattern style pasang)

-- ** regular

cKotekanKernel :: Derive.Generator Derive.Note
cKotekanKernel =
    Derive.generator module_ "kotekan" Tags.inst
    ("Render a kotekan pattern from a kernel. The sangsih part is inferred.\n"
        <> kotekanDoc)
    $ Sig.call ((,,,,,,,,)
    <$> Sig.defaulted "rotation" (0 :: Double)
        "Rotate kernel to make a different pattern."
    <*> styleArg Telu
    <*> Sig.defaultedEnv "sangsih" Sig.Both Call.Up
        "Whether sangsih is above or below polos."
    <*> Sig.environ "invert" Sig.Prefixed False "Flip the pattern upside down."
    <*> Sig.requiredEnviron "kernel" Sig.Both kernelDoc
    <*> durEnv <*> kotekanEnv <*> pasangEnv <*> inferInitialFinalEnv
    ) $ \(rotation, style, sangsihAbove, inverted, kernelS, dur, kotekan,
        pasang, initialFinal) ->
    Sub.inverting $ \args -> do
        kernel <- Derive.requireRight id $ makeKernel (untxt kernelS)
        pitch <- getPitch args
        underThreshold <- underThresholdFunction kotekan dur
        let cycle = realizeKernel sangsihAbove style pasang
                ((if inverted then invert else id) (rotate rotation kernel))
        realizeKotekanPatternArgs args initialFinal dur pitch
            underThreshold Repeat cycle

-- | For regular kotekan, the sangsih can be automatically derived from the
-- polos.
cKotekanRegular :: Bool -> Maybe Text -> KotekanStyle
    -> Derive.Generator Derive.Note
cKotekanRegular inverted maybeKernel defaultStyle =
    Derive.generator module_ "kotekan" Tags.inst
    ("Render a kotekan pattern from a kernel representing the polos.\
    \ The sangsih is inferred.\n" <> kotekanDoc)
    $ Sig.call ((,,,,,,)
    <$> maybe
        (Sig.defaultedEnv "kernel" Sig.Both ("k-12-1-21" :: Text) kernelDoc)
        pure maybeKernel
    <*> styleArg defaultStyle
    <*> Sig.defaultedEnv "sangsih" Sig.Both (Nothing :: Maybe Sig.Dummy)
        "Whether sangsih is above or below polos. If not given, sangsih will\
        \ be above if the polos ends on a low note or rest, below otherwise."
    <*> durEnv <*> kotekanEnv <*> pasangEnv <*> inferInitialFinalEnv
    ) $ \(kernelS, style, sangsihDir, dur, kotekan, pasang, initialFinal) ->
    Sub.inverting $ \args -> do
        kernel <- Derive.requireRight id $ makeKernel (untxt kernelS)
        let sangsihAbove = fromMaybe (inferSangsih inverted kernel)
                sangsihDir
        pitch <- getPitch args
        underThreshold <- underThresholdFunction kotekan dur
        let cycle = realizeKernel sangsihAbove style pasang
                (if inverted then invert kernel else kernel)
        realizeKotekanPatternArgs args initialFinal
            dur pitch underThreshold Repeat cycle
    where
    inferSangsih inverted kernel = (if inverted then Call.invert else id) $
        case Lists.last kernel of
            Just High -> Call.Down
            _ -> Call.Up

cKotekanExplicit :: Derive.Generator Derive.Note
cKotekanExplicit =
    Derive.generator module_ "kotekan" Tags.inst
    "Render a kotekan pattern from explicit polos and sangsih parts."
    $ Sig.call ((,,,)
    <$> Sig.required "polos" "Polos part."
    <*> Sig.required "sangsih" "Sangsih part."
    <*> durEnv <*> pasangEnv
    ) $ \(polosS, sangsihS, dur, pasang) -> Sub.inverting $ \args -> do
        let (expected, frac) = properFraction (Args.duration args / dur)
        when (frac /= 0) $ Derive.throw $ "event " <> showt (Args.duration args)
            <> " not evenly divisble by kotekan dur " <> showt dur
        polosSteps <- parse "polos" expected polosS
        sangsihSteps <- parse "sangsih" expected sangsihS
        pitch <- getPitch args
        let realize = realizeExplicit (Args.range args) dur pitch
        realize polosSteps (polos pasang)
            <> realize sangsihSteps (sangsih pasang)
    where
    parse name expected part_
        | Text.length part /= expected =
            Derive.throw $ name <> ": expected length of " <> showt expected
                <> " but was " <> showt (Text.length part)
        | otherwise = Derive.requireRight ((part <> ":")<>) $
            mapM parse1 (untxt part)
        where part = Text.dropWhile (=='k') part_
    parse1 '-' = Right Nothing
    parse1 c = maybe (Left $ "expected digit or '-': " <> showt c)
        (Right . Just) (Num.readDigit c)

realizeExplicit :: (ScoreTime, ScoreTime) -> ScoreTime -> PSignal.Pitch
    -> [Maybe Pitch.Step] -> ScoreT.Instrument -> Derive.NoteDeriver
realizeExplicit (start, end) dur pitch notes inst = mconcat
    [ Derive.place t dur (note t transpose)
    | (t, Just transpose) <- zip (tail (Lists.range_ start dur)) notes
    ]
    where
    note t transpose =
        (if t >= end then Call.addFlags (Flags.inferDuration <> finalFlag)
            else id) $
        Derive.withInstrument inst $
        Call.pitchedNote (Pitches.transposeD transpose pitch)

kernelDoc :: Doc.Doc
kernelDoc = "Polos part in transposition steps.\
    \ This will be normalized to end on the destination pitch.\
    \ It should consist of `-`, `1`, and `2`. You can start with `k` to\
    \ avoid needing quotes. Starting with `k` will also require the length to\
    \ be a multiple of 4."

realizeKernel :: Call.UpDown -> KotekanStyle
    -> Pasang ScoreT.Instrument -> Kernel -> Cycle
realizeKernel sangsihAbove style pasang kernel =
    endOnZero $ kernelToPattern kernel sangsihAbove style pasang

-- *** implementation

realizeKotekanPatternArgs :: Derive.PassedArgs a -> (Maybe Bool, Bool)
    -> ScoreTime -> PSignal.Pitch -> (ScoreTime -> Bool) -> Repeat -> Cycle
    -> Derive.NoteDeriver
realizeKotekanPatternArgs args initialFinal =
    realizeKotekanPattern (inferInitial args initialFinal)
        (Args.range args) (Args.orientation args)

-- | Take a Cycle, which is an abstract description of a pattern via
-- 'KotekanNote's, to real notes in a NoteDeriver.
realizeKotekanPattern :: (Bool, Bool) -- ^ include (initial, final)
    -> (ScoreTime, ScoreTime) -> Types.Orientation -> ScoreTime -> PSignal.Pitch
    -> (ScoreTime -> Bool) -> Repeat -> Cycle -> Derive.NoteDeriver
realizeKotekanPattern initialFinal (start, end) orientation dur pitch
        underThreshold repeat cycle =
    realizeNotes realize $
        realizePattern repeat orientation initialFinal start end dur getCycle
    where
    getCycle t
        | underThreshold t = interlocking cycle
        | otherwise = nonInterlocking cycle
    realize (KotekanNote inst steps muted) =
        maybe id Derive.withInstrument inst $
        -- TODO the kind of muting should be configurable.  Or, rather I should
        -- dispatch to a zero dur note call, which will pick up whatever form
        -- of mute is configured.
        -- TODO I'm using 'm' for that now, right?
        (if muted then Call.addAttributes Attrs.mute else id) $
        Call.pitchedNote (Pitches.transposeD steps pitch)
    -- TODO It should no longer be necessary to strip flags from
    -- 'Call.pitched_note', because "" only puts flags on if the event is
    -- at the end of the track, and that shouldn't happen for these.  Still,
    -- Call.pitched_note should use a lower level note call that doesn't do
    -- things like that.

type Kernel = [Atom]
data Atom = Gap -- ^ a gap in the kotekan pattern
    | Rest -- ^ a rest will be filled in by the other part
    | Low | High
    deriving (Eq, Ord, Show)

instance Pretty Atom where
    format = Pretty.char . toChar
    formatList cs =
        "make_kernel \"" <> Pretty.text (txt (map toChar cs)) <> "\""

makeKernel :: [Char] -> Either Text Kernel
makeKernel ('k':cs)
    | length cs `mod` 4 /= 0 =
        Left $ "kernel's length " <> showt (length cs)
            <> " is not a multiple of 4: " <> showt cs
    | otherwise = mapM fromChar cs
makeKernel cs = mapM fromChar cs

fromChar :: Char -> Either Text Atom
fromChar c = case c of
    '_' -> Right Gap
    '-' -> Right Rest
    '1' -> Right Low
    '2' -> Right High
    _ -> Left $ "kernel must be one of `_-12`, but got " <> showt c

toChar :: Atom -> Char
toChar c = case c of
    Gap -> '_'
    Rest -> '-'
    Low -> '1'
    High -> '2'

-- | Make both parts end on zero by subtracting the pitch of the final
-- non-interlocking note.
endOnZero :: Cycle -> Cycle
endOnZero realization = Realization
    { interlocking = add (-steps) (interlocking realization)
    , nonInterlocking = add (-steps) (nonInterlocking realization)
    }
    where
    add steps = map $ map $ \note ->
        note { noteSteps = steps + noteSteps note }
    steps = fromMaybe 0 $ do
        final : _ <- Lists.last (nonInterlocking realization)
        pure $ noteSteps final

kernelToPattern :: Kernel -> Call.UpDown -> KotekanStyle
    -> Pasang ScoreT.Instrument -> Cycle
kernelToPattern kernel sangsihAbove kotekanStyle pasang = Realization
    { interlocking = map interlock kernel
    , nonInterlocking = map nonInterlock kernel
    }
    where
    interlock atom = case (sangsihAbove, kotekanStyle) of
        (Call.Up, Telu) -> case atom of
            Gap -> []
            Rest -> [s 2]
            Low -> [p 0]
            High -> [p 1, s 1]
        (Call.Up, Pat) -> case atom of
            Gap -> []
            Rest -> [s 2]
            Low -> [p 0, s 3]
            High -> [p 1]
        (Call.Down, Telu) -> case atom of
            Gap -> []
            Rest -> [s (-1)]
            Low -> [p 0, s 0]
            High -> [p 1]
        (Call.Down, Pat) -> case atom of
            Gap -> []
            Rest -> [s (-1)]
            Low -> [p 0]
            High -> [p 1, s (-2)]
    nonInterlock atom = case (sangsihAbove, kotekanStyle) of
        (Call.Up, Telu) -> case atom of
            Gap -> []
            Rest -> [both 2]
            Low -> [both 0]
            High -> [both 1]
        (Call.Up, Pat) -> case atom of
            Gap -> []
            Rest -> [p 2, s 2]
            Low -> [p 0, s 3]
            High -> [p 1, s 1]
        (Call.Down, Telu) -> case atom of
            Gap -> []
            Rest -> [both (-1)]
            Low -> [both 0]
            High -> [both 1]
        (Call.Down, Pat) -> case atom of
            Gap -> []
            Rest -> [p (-1), s (-1)]
            Low -> [p 0, s 0]
            High -> [p 1, s (-2)]
    p = kotekanNote (Just (polos pasang))
    s = kotekanNote (Just (sangsih pasang))
    both = kotekanNote Nothing

rotate :: Int -> [a] -> [a]
rotate n xs = cycle (rotations xs) !! n

rotations :: [a] -> [[a]]
rotations xs = xs : go xs (reverse xs)
    where
    go [] _ = []
    go _ [_] = []
    go _ [] = []
    go xs (z:zs) = p : go p zs
        where p = take len (z : xs)
    len = length xs

invert :: Kernel -> Kernel
invert = map $ \case
    Gap -> Gap
    Rest -> Rest
    High -> Low
    Low -> High

-- *** all kernels

-- | Find a kernel as a rotation or inversion of one of the standard ones.
findKernel :: Kernel -> Maybe (Kernel, Bool, Int)
findKernel kernel = lookup kernel variants
    where
    variants =
        [ (variant, (kernel, inverted, rotation))
        | kernel <- allKernels
        , (variant, (inverted, rotation)) <- variations kernel
        ]
    allKernels = [kernel_12_1_21, kernel_1_21_21, kernel_2_21_21]
    Right kernel_12_1_21 = makeKernel "-12-1-21"
    Right kernel_1_21_21 = makeKernel "-1-21-21"
    Right kernel_2_21_21 = makeKernel "-2-21-21"

    variations :: Kernel -> [(Kernel, (Bool, Int))]
    variations kernel_ = Lists.uniqueOn fst
        [ (variant, (inverted, rotate))
        | (inverted, kernel) <- [(False, kernel_), (True, invert kernel_)]
        , (rotate, variant) <- zip [0..] (rotations kernel)
        ]

-- ** implementation

data Repeat = Repeat | Once deriving (Show)
instance Pretty Repeat where pretty = showt

-- | (interlocking pattern, non-interlocking pattern)
--
-- Each list represents coincident notes.  [] is a rest.
type Cycle = Realization [[KotekanNote]]

data Note a = Note
    { noteStart :: !ScoreTime
    , noteDuration :: !ScoreTime
    -- | Used for 'final_flag'.
    , noteFlags :: !Flags.Flags
    , noteData :: !a
    } deriving (Functor, Show)

instance Pretty a => Pretty (Note a) where
    format (Note start dur flags d) = Pretty.format (start, dur, flags, d)

addFlag :: Flags.Flags -> Note a -> Note a
addFlag flag n = n { noteFlags = flag <> noteFlags n }

-- | High level description of a note.  This goes into Note before it becomes
-- a Derive.NoteDeriver.
data KotekanNote = KotekanNote
    -- | If Nothing, retain the instrument in scope.  Presumably it will be
    -- later split into polos and sangsih by a @unison@ or @kempyung@ call.
    { noteInstrument :: !(Maybe ScoreT.Instrument)
    , noteSteps :: !Pitch.Step
    , noteMuted :: !Bool
    } deriving (Show)

instance Pretty KotekanNote where
    format (KotekanNote inst steps muted) =
        Pretty.format (inst, steps, if muted then "+mute" else "+open" :: Text)

kotekanNote :: Maybe ScoreT.Instrument -> Pitch.Step -> KotekanNote
kotekanNote inst steps = KotekanNote
    { noteInstrument = inst
    , noteSteps = steps
    , noteMuted = False
    }

mutedNote :: KotekanNote -> KotekanNote
mutedNote note = note { noteMuted = True }

underThresholdFunction :: (RealTime -> RealTime) -> ScoreTime
    -> Derive.Deriver (ScoreTime -> Bool) -- ^ say if a note at this time
    -- with the given duration would be under the kotekan threshold
underThresholdFunction kotekan dur = do
    toReal <- Derive.realFunction
    pure $ \t ->
        let real = toReal t
        in toReal (t+dur) - real < kotekan real

-- | Repeatedly call a cycle generating function to create notes.  The result
-- will presumably be passed to 'realize_notes' to convert the notes into
-- NoteDerivers.
realizePattern :: Repeat -- ^ Once will just call get_cycle at the start
    -- time.  Repeat will start the cycle at t+1 because t is the initial, so
    -- it's the end of the cycle.
    -> Types.Orientation -- ^ align to start or end
    -> (Bool, Bool)
    -> ScoreTime -> ScoreTime -> ScoreTime
    -> (ScoreTime -> [[a]]) -- ^ Get one cycle of notes, starting at the time.
    -> [Note a]
realizePattern repeat orientation (initial, final) start end dur getCycle =
    case repeat of
        Once -> concatMap realize $
            (if orientation == Types.Positive then zip else zipEnd)
                (Lists.range start end dur) (getCycle start)
        Repeat -> concatMap realize pairs
    where
    pairs = case orientation of
        Types.Positive -> cycles wrapped ts
        Types.Negative -> cyclesEnd getCycle ts
        where ts = Lists.range start end dur
    -- Since cycles are end-weighted, I have to get the end of a cycle if an
    -- initial note is wanted.
    wrapped t
        | t == start = maybe [] (:[]) (Lists.last ns)
        | otherwise = ns
        where ns = getCycle t
    realize (t, chord)
        | t >= end = if final
            then map (addFlag (Flags.inferDuration <> finalFlag)) ns
            else []
        | t == start = if initial then ns else []
        | otherwise = ns
        where ns = map (Note t dur mempty) chord

-- | Pair each @t@ with an @a@, asking the function for more @a@s as needed.
cycles :: (t -> [a]) -> [t] -> [(t, a)]
cycles getCycle = go
    where
    go [] = []
    go (t:ts) = case rest of
        Left ts -> pairs ++ go ts
        Right _ -> pairs
        where (pairs, rest) = Lists.zipRemainder (t:ts) (getCycle t)

-- | This is like 'cycles', but the last cycle is aligned to the end of the
-- @t@s, chopping off the front of the cycle if necessary.
cyclesEnd :: (t -> [a]) -> [t] -> [(t, a)]
cyclesEnd getCycle = shift . go
    where
    shift (pairs, restNs) = zip ts (drop (length restNs) ns ++ restNs)
        where (ts, ns) = unzip pairs
    go [] = ([], [])
    go (t:ts) = case rest of
        Left ts -> first (pairs++) (go ts)
        Right ns -> (pairs, ns)
        where (pairs, rest) = Lists.zipRemainder (t:ts) (getCycle t)

-- | Like 'zip', but two sequences are aligned at at their ends, instead of
-- their starts.
zipEnd :: [a] -> [b] -> [(a, b)]
zipEnd xs ys = reverse (zip (reverse xs) (reverse ys))

-- | Turn Notes into a NoteDeriver.
realizeNotes :: (a -> Derive.NoteDeriver) -> [Note a] -> Derive.NoteDeriver
realizeNotes realize = mconcatMap $ \(Note start dur flags note) ->
    Derive.place start dur $ Call.addFlags flags $ realize note

-- | Style for non-interlocking norot.  Interlocking norot is always the upper
-- neighbor (or lower on the top key).
data NorotStyle
    -- | Norot is emitted as the current instrument, which should be converted
    -- into kempyung or unison by a postproc.
    = Default
    -- | Norot in the diamond pattern, where sangsih goes down.
    | Diamond
    deriving (Bounded, Eq, Enum, Show)

instance ShowVal.ShowVal NorotStyle
instance Typecheck.Typecheck NorotStyle
instance Typecheck.ToVal NorotStyle

data KotekanStyle = Telu | Pat deriving (Bounded, Eq, Enum, Show)
instance ShowVal.ShowVal KotekanStyle
instance Typecheck.Typecheck KotekanStyle
instance Typecheck.ToVal KotekanStyle

-- * postproc

cUnison :: Derive.Transformer Derive.Note
cUnison = Derive.transformer module_ "unison" Tags.postproc
    "Split part into unison polos and sangsih. Emit only polos if\
    \ `only=polos` and only sangsih if `only=sangsih`."
    $ Sig.callt pasangEnv $ \pasang _args deriver -> do
        inst <- Call.getInstrument
        pasang <- Pasang <$> Derive.getInstrument (polos pasang)
            <*> Derive.getInstrument (sangsih pasang)
        Post.emapAsc_ (unison inst pasang) <$> deriver
    where
    unison inst pasang event
        | Score.eventInstrument event == inst = [set polos, set sangsih]
        | otherwise = [event]
        where
        msg = "unison from " <> pretty inst
        set role = Score.addLog msg $ Post.setInstrument (role pasang) event

-- | I could do this in two different ways:  Eval normally, then eval with
-- +kempyung, and make instrument note call understand it.  Or, postproc,
-- transpose, and check if the nn is above a limit.  The first one would let
-- the instrument choose how it wants to interpret +kempyung while letting this
-- call remain generic, but let's face it, it only really means one thing.  The
-- second seems a little simpler since it doesn't need a cooperating note call.
--
-- So postproc it is.
cKempyung :: Library.Calls Derive.Note
cKempyung = Make.transformNotes module_ "kempyung" Tags.postproc
    "Split part into kempyung, with `polos-inst` below and `sangsih-inst`\
    \ above. If the sangsih would go out of range, it's forced into unison."
    ((,)
    <$> instrumentTopEnv <*> pasangEnv
    ) $ \(maybeTop, pasang) deriver -> do
        pasangInst <- Call.getInstrument
        pasang <- Pasang <$> Derive.getInstrument (polos pasang)
            <*> Derive.getInstrument (sangsih pasang)
        scale <- Call.getScale
        let tooHigh = pitchTooHigh scale maybeTop
        Post.emapAsc_ (kempyung tooHigh pasangInst pasang) <$> deriver
    where
    kempyung tooHigh pasangInst pasang event
        | Score.eventInstrument event == pasangInst =
            [ set ("low kempyung from " <> pretty pasangInst) polos
            , transpose tooHigh $
                set ("high kempyung from " <> pretty pasangInst) sangsih
            ]
        | otherwise = [event]
        where
        set msg role =
            Score.addLog msg $ Post.setInstrument (role pasang) event
    transpose tooHigh event
        | tooHigh transposed = event
        | otherwise = transposed
        where
        transposed = event
            { Score.eventPitch =
                -- TODO it's not really linear, but these pitches should be
                -- constant anyway.
                PSignal.mapYLinear (Pitches.transpose (Pitch.Diatonic 3))
                    (Score.eventPitch event)
            }

cNyogcag :: Library.Calls Derive.Note
cNyogcag = Make.transformNotes module_ "nyog" Tags.postproc
    "Nyog cag style. Split a single part into polos and sangsih parts by\
    \ assigning polos and sangsih to alternating notes."
    pasangEnv $ \pasang deriver -> do
        inst <- Call.getInstrument
        pasang <- Pasang <$> Derive.getInstrument (polos pasang)
            <*> Derive.getInstrument (sangsih pasang)
        snd . Post.emapAsc (nyogcag inst pasang) True <$> deriver

nyogcag :: ScoreT.Instrument -> Pasang (ScoreT.Instrument, Derive.Instrument)
    -> Bool -> Score.Event -> (Bool, [Score.Event])
nyogcag pasangInst pasang isPolos event =
    ( nextIsPolos
    , if eventInst == pasangInst then [withInst event] else [event]
    )
    where
    nextIsPolos
        | eventInst == pasangInst = not isPolos
        | eventInst == fst (polos pasang) = False
        | eventInst == fst (sangsih pasang) = True
        | otherwise = isPolos
    eventInst = Score.eventInstrument event
    withInst =
        Post.setInstrument (if isPolos then polos pasang else sangsih pasang)

-- * realize calls

cRealizeGangsa :: Derive.Transformer Derive.Note
cRealizeGangsa = StaticMacro.check "c_realize_gangsa" $
    StaticMacro.transformer module_ "realize-gangsa" Tags.postproc doc
        [ StaticMacro.Call cRealizeNoltol []
        , StaticMacro.Call cCancelPasang [StaticMacro.Var]
        , StaticMacro.Call Gender.cRealizeNgoret []
        ]
    where doc = "Combine the gangsa realize calls in the right order."

-- | (noltol-time, kotekan-dur, damp-dyn)
type NoltolArg = (RealTime, RealTime, Signal.Y)

noltolArg :: Text
noltolArg = "noltol"

cNoltol :: Derive.Transformer Derive.Note
cNoltol = Derive.transformer module_ "noltol" Tags.delayed
    "Play the transformed notes in noltol style. If the space between\
    \ notes of the same (instrument, hand) is above a threshold,\
    \ end the note with a `+mute`d copy of itself. This only happens if\
    \ the duration of the note is at or below the `kotekan-dur`."
    $ Sig.callt ((,,)
    <$> Sig.defaulted "time" (0.1 :: Double)
        "Play noltol if the time available exceeds this threshold."
    <*> Sig.defaulted "damp-dyn" (0.65 :: Double)
        "Damped notes are multiplied by this dyn."
    <*> durEnv
    ) $ \(threshold, dampDyn, maxDur) args deriver -> do
        maxDur <- Call.realDuration (Args.start args) maxDur
        events <- deriver
        let times = Post.realTimeControl threshold events
        pure $ Post.emap1_ (put dampDyn maxDur) $ Stream.zip times events
        where
        put dampDyn maxDur (threshold, event) =
            Score.putArg noltolArg
                ((threshold, maxDur, dampDyn) :: NoltolArg) event

cRealizeNoltol :: Derive.Transformer Score.Event
cRealizeNoltol = Derive.transformer module_ "realize-noltol"
    Tags.realizeDelayed "Perform the annotations added by `noltol`."
    $ Sig.call0t $ \_args deriver -> realizeNoltolCall =<< deriver

realizeNoltolCall :: Stream.Stream Score.Event -> Derive.NoteDeriver
realizeNoltolCall =
    Post.emapS_ fst realize . Post.nextBy Score.eventInstrument
    where
    realize (event, next) = do
        (event, maybeArg) <- Derive.requireRight id $
            Score.takeArg noltolArg event
        case maybeArg of
            Nothing -> pure $ Stream.fromEvent event
            Just arg -> realizeNoltol arg event next

-- | If the next note of the same instrument is below a threshold, the note's
-- off time is replaced with a +mute.
realizeNoltol :: NoltolArg -> Score.Event -> Maybe Score.Event
    -> Derive.NoteDeriver
realizeNoltol (threshold, maxDur, dampDyn) event next =
    pure (Stream.fromEvent event) <> muted
    where
    muted
        | shouldNoltol = do
            start <- Derive.score (Score.eventEnd event)
            pitch <- Derive.require "no pitch" $
                Score.pitchAt (Score.eventStart event) event
            -- I used to copy the note and apply +mute, but this is low level
            -- and wouldn't take the instrument's zero-dur config.  Also it
            -- meant that integration would come out with +mute.
            Derive.withInstrument (Score.eventInstrument event) $
                Call.withPitch pitch $
                Call.multiplyDynamic dampDyn $
                Derive.place start 0 Call.note
        | otherwise = mempty
    shouldNoltol =
        Score.eventDuration event RealTime.<= maxDur
        && maybe True ((>= threshold) . space) next
    space next = Score.eventStart next - Score.eventEnd event

-- ** cancel-pasang

cCancelPasang :: Derive.Transformer Derive.Note
cCancelPasang = Derive.transformer module_ "cancel-pasang" Tags.postproc
    "This is like the `cancel` call, except it also knows how to cancel out\
    \ pasang instruments such that adjacent kotekan calls can have initial and\
    \ final notes, but won't get doubled notes."
    $ Postproc.makeCancel cancelStrongFinal pasangKey

-- | Kotekan ends with a final, which cancels normal, but loses to strong.
-- This is like 'Postproc.cancel_strong_weak', except it adds 'final_flag',
-- so I can have the end of a kotekan override, but still be overidden
-- with an explicit strong note.
cancelStrongFinal :: [Score.Event] -> Either Text [Score.Event]
cancelStrongFinal events
    | not (null strongs) = merge strongs (finals ++ rest)
    | not (null finals) = merge finals rest
    | not (null normals) = merge normals weaks
    | otherwise = Right weaks
    where
    (strongs, finals, rest) = Lists.partition2
        (Score.hasFlags Flags.strong) (Score.hasFlags finalFlag) events
    (weaks, normals) = List.partition (Score.hasFlags Flags.weak) events
    merge strongs weaks =
        Right [Postproc.inferDurationMerged strong weaks | strong <- strongs]

-- | Match any of polos, sangsih, and pasang to each other.  Since polos and
-- sangsih together are considered one voice, a sangsih start is note end for
-- a polos note.
pasangKey :: Postproc.Key
    (Either ScoreT.Instrument (ScoreT.Instrument, ScoreT.Instrument),
        Maybe Text)
pasangKey e = (inst, get EnvKey.hand)
    where
    inst = case (get instPolos, get instSangsih) of
        (Just p, Just s) -> Right (p, s)
        _ -> Left (Score.eventInstrument e)
    get k = Env.maybeVal k (Score.eventEnviron e)

-- * implementation

-- | Get pitch for a kotekan call.
getPitch :: Derive.PassedArgs a -> Derive.Deriver PSignal.Pitch
getPitch = Call.getPitchHere

styleArg :: KotekanStyle -> Sig.Parser KotekanStyle
styleArg deflt = Sig.defaultedEnv "style" Sig.Both deflt "Kotekan style."

durEnv :: Sig.Parser ScoreTime
durEnv = Sig.environQuoted "kotekan-dur" Sig.Unprefixed
    (DeriveT.quoted "ts" [DeriveT.str "s"]) "Duration of derived notes."

kotekanEnv :: Sig.Parser (RealTime -> RealTime)
kotekanEnv = fmap (RealTime.seconds .) $
    Sig.environ "kotekan" Sig.Unprefixed (0.15 :: RealTime)
        "If note durations are below this, divide the parts between polos and\
        \ sangsih."

inferInitialFinalEnv :: Sig.Parser (Maybe Bool, Bool)
inferInitialFinalEnv = (,)
    <$> Sig.environ "initial" Sig.Unprefixed (Nothing :: Maybe Sig.Dummy)
        "If true, include an initial note, which is the same as the final note.\
        \ This is suitable for the start of a sequence of kotekan calls.\
        \ If not given, infer false for negative duration, true for positive."
    <*> Sig.environ "final" Sig.Unprefixed True
        "If true, include the final note, at the event end."

inferInitial :: Derive.PassedArgs a -> (Maybe Bool, Bool) -> (Bool, Bool)
inferInitial args =
    first $ fromMaybe (not $ Event.isNegative (Args.event args))

initialFinalEnv :: Sig.Parser (Bool, Bool)
initialFinalEnv = (,)
    <$> Sig.environ "initial" Sig.Unprefixed True
        "If true, include an initial note, which is the same as the final note.\
        \ This is suitable for the start of a sequence of kotekan calls.\
        \ If not given, infer false for negative duration, true for positive."
    <*> Sig.environ "final" Sig.Unprefixed True
        "If true, include the final note, at the event end."

instrumentTopEnv :: Sig.Parser (Maybe Pitch.Pitch)
instrumentTopEnv = Sig.environKey EnvKey.instrumentTop
    (Nothing :: Maybe Sig.Dummy)
    "Top pitch this instrument can play. Normally the instrument sets\
    \ it via the instrument environ."

noteTooHigh :: Scale.Scale -> Maybe Pitch.Pitch -> PSignal.Transposed -> Bool
noteTooHigh scale maybeTop pitchv = fromMaybe False $ do
    top <- maybeTop
    note <- either (const Nothing) Just $ PSignal.pitchNote pitchv
    pitch <- either (const Nothing) Just $ Scale.scaleRead scale mempty note
    pure $ pitch > top

pitchTooHigh :: Scale.Scale -> Maybe Pitch.Pitch -> Score.Event -> Bool
pitchTooHigh scale maybeTop =
    maybe False (noteTooHigh scale maybeTop) . Score.initialPitch

pasangEnv :: Sig.Parser (Pasang ScoreT.Instrument)
pasangEnv = Pasang
    <$> Sig.requiredEnviron (Derive.ArgName instPolos)
        Sig.Unprefixed "Polos instrument."
    <*> Sig.requiredEnviron (Derive.ArgName instSangsih)
        Sig.Unprefixed "Sangsih instrument."

instPolos :: Env.Key
instPolos = "inst-polos"

instSangsih :: Env.Key
instSangsih = "inst-sangsih"

finalFlag :: Flags.Flags
finalFlag = Flags.flag "final"
