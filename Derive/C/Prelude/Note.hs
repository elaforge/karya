-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Basic calls for note tracks.
module Derive.C.Prelude.Note (
    library
    , cNote
    , transformedNote, transformedNoteName
    , noteCall
    , Config(..), useAttributes, noDurationAttributes
    , GenerateNote
    , defaultNote, defaultNoteIntegrate
    , noteFlags
    , adjustDuration
    , minDuration
) where
import qualified Data.Either as Either
import qualified Data.Map as Map

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.NoteUtil as NoteUtil
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.Flags as Flags
import qualified Derive.Library as Library
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream
import qualified Derive.Symbols as Symbols

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.generators
        [ (Symbols.nullNote, cNote)
        , (Symbols.defaultNote, cNote)
        ]
    , Library.transformers [(Symbols.noteTrack, cNoteTrack)]
    , Library.both [("attr", cWithAttributes)]
    ]

-- | This is mostly useful for tests when I need some call that makes a note
-- and takes arguments.
cWithAttributes :: Library.Calls Derive.Note
cWithAttributes = Make.transformNotes Module.prelude "note" mempty
    "A note with attributes or instrument."
    (Sig.many "attr" "Set instrument or add attributes.") $
    \instAttrs deriver -> do
        let (insts, attrs) = Either.partitionEithers instAttrs
            inst = Lists.last insts
        maybe id Derive.withInstrument inst $
            Call.addAttributes (mconcat attrs) deriver

-- * note

cNote :: Derive.Generator Derive.Note
cNote = noteCall "note" "" mempty $ defaultNote useAttributes

-- | Create a standard note call with a transformer applied.
transformedNote :: Doc.Doc -> Tags.Tags
    -> (Derive.NoteArgs -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Derive.Generator Derive.Note
transformedNote = transformedNoteName "note"

-- | 'transformed_note' but set the name.  Since 'note_call' doesn't take any
-- args, this is just for documentation in the stack.
transformedNoteName :: Derive.CallName -> Doc.Doc -> Tags.Tags
    -> (Derive.NoteArgs -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Derive.Generator Derive.Note
transformedNoteName name prependDoc tags transform =
    noteCall name prependDoc tags $ \args ->
        transform args (defaultNote useAttributes args)

-- | Create a note call, configuring it with the actual note generating
-- function.  The generator is called with the usual note arguments, and
-- receives the usual instrument and attribute transform.
noteCall :: Derive.CallName -> Doc.Doc -> Tags.Tags -> GenerateNote
    -> Derive.Generator Derive.Note
noteCall name prependDoc tags generate =
    Derive.generator Module.prelude name tags prepended $
    Sig.call0 $ Sub.inverting $ applyInstrumentControls . generate
    where
    prepended
        | prependDoc == "" = generatorDoc
        | otherwise = "Modified note call: " <> prependDoc <> "\n"
            <> generatorDoc
    generatorDoc =
        "The note call is the default note generator, and will emit a single\
        \ score event. Usually this is bound to the null call, \"\", which is\
        \ therefore the most syntactically convenient note generator.\
        \\nThis should probably remain bound to "
        <> ShowVal.doc Symbols.noteTrack <> " symbol, which is used\
        \ internally by many other calls when they want a plain note. This is\
        \ so you can bind \"\" to one of those other calls without getting\
        \ recursion."
    -- Some history: long ago, you could call the note with >inst to set the
    -- instrument, and +a to set attributes.  The intention was an easy
    -- notation to write a single part that alternated between multiple
    -- instruments, in the way that trackers would often do it.  But eventually
    -- I generalized tracks to be able to contain arbitrary calls, and the note
    -- became the "" call, which couldn't take arguments.  Later I got +a
    -- syntax back courtesty of lookup calls, but the >inst literal syntax is
    -- gone now, and in any case I tend to have one instrument per-track due to
    -- wanting to scope transformations per-instrument.  But maybe some day
    -- I'll bring back >inst syntax as a lookup call.

-- | Apply the 'Instrument.Common.config_controls' field.  It happens in the
-- note call to make sure it happens only once per note.
applyInstrumentControls :: Derive.Deriver a -> Derive.Deriver a
applyInstrumentControls deriver = Call.lookupInstrument >>= \case
    Nothing -> deriver
    Just inst -> do
        (_inst, deriveInst) <- Derive.getInstrument inst
        let controls = ScoreT.untyped . Signal.constant <$>
                Derive.instControls deriveInst
        Derive.withMergedControls (Map.toList controls) deriver

cNoteTrack :: Derive.Transformer Derive.Note
cNoteTrack = Derive.transformer Module.prelude "note-track" mempty
    ("This is the implicit call at the top of every note track. The first\
    \ argument is the instrument named after the note track's `>`.\
    \ If there is a note transformer of the same name as the\
    \ instrument, starting with `>`, it will be called after setting the\
    \ instrument. This way, you can set instrument-specific variables or\
    \ transformations.")
    $ Sig.callt (Sig.defaultedEnv "inst" Derive.None
        (Nothing :: Maybe ScoreT.Instrument)
        "Set this instrument and run the transformer, if it exists."
    ) $ \inst args deriver -> noteTrack (Derive.passedCtx args) inst deriver

noteTrack :: Derive.Context Derive.Note -> Maybe ScoreT.Instrument
    -> Derive.NoteDeriver -> Derive.NoteDeriver
noteTrack ctx inst deriver = do
    let sym = Expr.Symbol $ ">" <> maybe "" ScoreT.instrumentName inst
    maybeCall <- Derive.lookupCall sym
    let transform = maybe id (callTransformer ctx) maybeCall
    maybe id Derive.withInstrument inst $ transform deriver

callTransformer :: Derive.Context d -> Derive.Transformer d
    -> Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d)
callTransformer ctx call deriver =
    Internal.withStackCall (Derive.callName call) $
        Derive.callFunc call passed deriver
    where
    passed = Derive.PassedArgs
        { Derive.passedVals = []
        , Derive.passedCallName = Derive.callName call
        , Derive.passedCtx = ctx
        }

-- ** generate

-- | Generate a single note.  This is intended to be used as the lowest level
-- null call for some instrument.
type GenerateNote = Derive.NoteArgs -> Derive.NoteDeriver

data Config = Config
    -- | Note duration is affected by 'Attrs.staccato'.
    { configStaccato :: !Bool
    -- | Note duration can depend on 'Controls.sustain' and
    -- 'Controls.sustain_abs'.
    , configSustain :: !Bool
    } deriving (Show)

useAttributes :: Config
useAttributes = Config
    { configStaccato = True
    , configSustain = True
    }

-- | Don't observe any of the duration affecting attributes.
noDurationAttributes :: Config
noDurationAttributes = Config False False

-- | The actual note generator.
defaultNote :: Config -> GenerateNote
defaultNote config args = defaultNoteIntegrate integrate config args
    where
    -- Score.event_text can be set explicitly to affect the integrated note.
    -- But, I hardcode to "" by default.  I could try to have calls such as
    -- "m" or "+attr" put that into the text, but ultimately I think integrate
    -- should infer that from the Score.Event itself if at all.  Otherwise,
    -- I don't see how things like combined attributes or attributes over
    -- multiple notes could possible be correct.
    integrate = ""

-- | Like 'default_note', except allow to set 'Derive.Score.event_integrate'.
{-# SCC defaultNoteIntegrate #-}
defaultNoteIntegrate :: Text -> Config -> GenerateNote
defaultNoteIntegrate integrate config args = do
    start <- Args.realStart args
    end <- Args.realEnd args
    dyn <- Internal.getDynamic id
    -- Add flags to get the arrival-note postproc to figure out the duration.
    -- Details in "Derive.Call.Post.ArrivalNote".
    let flags = noteFlags (start == end) (Derive.stateStack dyn)
            (Derive.stateEnviron dyn)
    cvmap <- Derive.controlsAt start
    let attrs = either (const mempty) id $
            Env.getVal EnvKey.attributes (Derive.stateEnviron dyn)
    let adjustedEnd = durationAttributes config cvmap attrs start end
    Stream.fromEvent <$> NoteUtil.makeEventControlVals
        cvmap args dyn start (adjustedEnd - start) integrate flags

noteFlags :: Bool -> Stack.Stack -> Env.Environ -> Flags.Flags
noteFlags zeroDur stack environ
    -- An event at TrackTime 0 never gets an inferred duration.
    -- Otherwise, I couldn't write single note calls for percussion.
    | inferDur && trackStart = mempty
    | inferDur = Flags.inferDuration <> Flags.strong
    | otherwise = mempty
    where
    -- Note that I can't use Args.duration or Args.range_on_track, because
    -- this may be invoked via e.g. Call.note, which fakes up an event with
    -- range (0, 1), and sets the duration via the warp.
    inferDur = blockEnd && zeroDur
    trackStart = start == Just 0
    blockEnd = start == Env.maybeVal EnvKey.blockEnd environ
    -- This is the start time on the track, which, due to slicing, is not
    -- necessarily the same as Args.start.
    start = fst <$>
        Lists.head (mapMaybe Stack.regionOf (Stack.innermost stack))

-- ** adjust start and duration

adjustDuration :: RealTime -> RealTime -> RealTime -> RealTime -> RealTime
adjustDuration curPos curDur nextPos nextDur
        -- Departing notes are not changed.
    | curDur > 0 = curDur
        -- Arriving followed by arriving with a rest in between extends to
        -- the arrival of the rest.
    | nextDur <= 0 && rest > 0 = rest
        -- Arriving followed by arriving with no rest, or an arriving note
        -- followed by a departing note will sound until the next note.
    | otherwise = nextPos - curPos
    where rest = nextPos + nextDur - curPos

-- | This keeps a negative sustain_abs from making note duration negative.
minDuration :: RealTime
minDuration = 1 / 64

{- | Interpret attributes and controls that effect the note's duration.

    This is actually somewhat complicated.  Instead of all the
    duration-affecting controls all applying together, notes fit into distinct
    categories:

    - Zero-duration notes ignore all this.

    - Staccato notes divide their duration by 2.

    - Normal notes multiply 'Controls.sustain' and add 'Controls.duration_abs',
    which could be negative.  They clip at a minimum duration to keep from
    going negative.
-}
durationAttributes :: Config -> ScoreT.ControlValMap -> Attrs.Attributes
    -> RealTime -> RealTime -> RealTime -- ^ new end
durationAttributes config controls attrs start end
    | start >= end = end -- don't mess with 0 dur or negative notes
    | Just setDur <- Map.lookup Controls.sustainSet controls, useSustain =
        start + max minDuration (RealTime.seconds setDur)
    | otherwise = start + max minDuration (dur * sustain + sustainAbs)
    where
    has = Attrs.contain attrs
    dur = end - start
    staccato = configStaccato config && has Attrs.staccato
    sustain = if staccato then sustain_ * 0.5 else sustain_
    sustain_ = if useSustain then lookupTime 1 Controls.sustain else 1
    sustainAbs = if staccato || not useSustain then 0
        else lookupTime 0 Controls.sustainAbs
    useSustain = configSustain config
    lookupTime deflt control = maybe deflt RealTime.seconds
        (Map.lookup control controls)
