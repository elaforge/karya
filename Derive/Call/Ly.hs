-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for calls to cooperate with the lilypond backend.
module Derive.Call.Ly where
import qualified Data.Map as Map

import qualified Util.Lists as Lists
import qualified Util.Texts as Texts

import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Sub as Sub
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.Flags as Flags
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Types as Types

import           Global
import           Types


-- * utils for ly calls

whenLilypond :: Derive.Deriver a -- ^ Run if this is a lilypond derive.
    -> Derive.Deriver a -- ^ Run if this is a normal derive.
    -> Derive.Deriver a
whenLilypond lily = whenLilypondConfig (const lily)

whenLilypondConfig :: (Types.Config -> Derive.Deriver a)
    -- ^ Run if this is a lilypond derive.
    -> Derive.Deriver a -- ^ Run if this is a normal derive.
    -> Derive.Deriver a
whenLilypondConfig lily notLily =
    maybe notLily lily =<< Derive.lookupLilypondConfig

-- | Only emit the deriver if I'm in lilypond mode.
onlyLilypond :: Derive.NoteDeriver -> Derive.NoteDeriver
onlyLilypond deriver = ifM Derive.isLilypondMode deriver mempty

-- | When in lilypond mode, generate a note with the given Code.
noteCode :: Code -> Derive.PassedArgs d -> Derive.NoteDeriver
    -> Derive.NoteDeriver
noteCode code args = whenLilypond $ addFirst code $ Call.placedNote args

-- ** transformer

-- | Add the Code to all the events.
addAll :: Code -> Derive.NoteDeriver -> Derive.NoteDeriver
addAll code = fmap $ Post.emap1_ (addNoteCode code)

-- | Add Code to the first event.
addFirst :: Code -> Derive.NoteDeriver -> Derive.NoteDeriver
addFirst code deriver =
    Stream.first (not . isCode) (addNoteCode code) <$> deriver

prepend :: Position Constants.CodePosition
prepend = Position $
    Constants.CodePosition Constants.Chord Constants.Prepend Constants.First

append, noteAppend :: Constants.Distribution -> Position Constants.CodePosition
append = Position . Constants.CodePosition Constants.Chord Constants.Append
noteAppend = Position . Constants.CodePosition Constants.Note Constants.Append

-- ** note parent

-- | Replace a note parent with one that derives its sub-events as-is
-- and adds lilypond code to them.
notesCode :: Code -> Derive.PassedArgs d
    -> Derive.NoteDeriver -> Derive.NoteDeriver
notesCode code = notesWith (addAll code)

-- | Like 'notes_code', but only apply the code to the first event, not all of
-- them.
firstNoteCode :: Code -> Derive.PassedArgs d
    -> Derive.NoteDeriver -> Derive.NoteDeriver
firstNoteCode code args = whenLilypond $
    addFirst code $ Sub.deriveSubs args

-- | This is like 'notes_code', but the first event in each track gets the
-- start code, and the last event in each track gets the end code.
notesAround :: Code -> Code -> Derive.PassedArgs d
    -> Derive.NoteDeriver -> Derive.NoteDeriver
notesAround start end args =
    whenLilypond $ mconcatMap around =<< Sub.subEvents args
    where
    around notes = firstLast
        (addNoteCode start) (addNoteCode end) <$> Sub.derive notes

-- | Like 'notes_around', but for use when you already know you're in lilypond
-- mode.
notesAroundLy :: Code -> Code -> Derive.PassedArgs d -> Derive.NoteDeriver
notesAroundLy start end = mconcatMap around <=< Sub.subEvents
    where
    around notes = firstLast
        (addNoteCode start) (addNoteCode end) <$> Sub.derive notes

-- | Like 'notes_around', but when I'm not in lilypond mode just derive the
-- sub events unchanged.
codeAround :: FreeCode -> FreeCode -> Derive.PassedArgs d -> Derive.NoteDeriver
codeAround start end args = whenLilypond
    (code0 (Args.start args) start
        <> Sub.deriveSubs args <> code0 (Args.end args) end)
    (Sub.deriveSubs args)

-- | Transform and evaluate the sub events.
notesWith :: (Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Derive.PassedArgs d
    -> Derive.NoteDeriver -> Derive.NoteDeriver
notesWith f args = whenLilypond $
    Sub.derive . map (fmap f) . concat =<< Sub.subEvents args

-- | Apply a function to the first and last Events, which are not 'is_code0'.
firstLast :: (Score.Event -> Score.Event) -> (Score.Event -> Score.Event)
    -> Stream.Stream Score.Event -> Stream.Stream Score.Event
firstLast = Stream.firstLast (not . isCode)

-- ** code

-- | Either prepend or append some code to a lilypond note.
type Code = (Position Constants.CodePosition, Ly)
type FreeCode = (Position Constants.FreeCodePosition, Ly)
data Position pos = Position !pos
    -- | Create a note with the given env value set to the Ly code.  This is
    -- for directives to the lilypond performer, not to lilypond itself.
    -- E.g. 'Constants.v_subdivision'.
    | SetEnviron !Env.Key
    deriving (Eq, Show)

instance Pretty pos => Pretty (Position pos) where
    pretty (SetEnviron key) = "SetEnviron " <> pretty key
    pretty (Position p) = pretty p

instance Typecheck.Typecheck (Position Constants.CodePosition) where
    fromVal = Typecheck.fromValSymbol codePositionNames
    toType Proxy = Typecheck.toTypeSymbol (Map.keys codePositionNames)

codePositionNames :: Map Text (Position Constants.CodePosition)
codePositionNames =
    Map.fromList $ Lists.keyOn ShowVal.showVal $
        map Position Constants.allPositions

instance Typecheck.ToVal (Position Constants.CodePosition) where
    toVal = DeriveT.VStr . Expr.Str . ShowVal.showVal

instance ShowVal.ShowVal (Position Constants.CodePosition) where
    showVal = Texts.dropPrefix "ly-" . keyOf
        where
        keyOf (SetEnviron k) = k
        keyOf (Position pos) = Constants.positionKey pos

-- | Fragment of Lilypond code.
type Ly = Text

-- | A lilypond \"note\", which is just a chunk of text.
type Note = Ly

-- | Like 'code', but for 0 duration code fragments, and can either put them
-- before or after notes that occur at the same time.
code0 :: ScoreTime -> FreeCode -> Derive.NoteDeriver
code0 start code = do
    rstart <- Derive.real start
    Post.emap1_ (code0Event rstart code) <$> Derive.place start 0 Call.note

-- | Make a code0 event directly.  Inherit instrument and environ from an
-- existing note.  Otherwise, the lilypond backend doesn't know how to group
-- the code event.
--
-- TODO aka free_code, maybe rename it?
code0Event :: RealTime -> FreeCode -> Score.Event -> Score.Event
code0Event start (pos, code) =
    -- I don't use Score.move_event because I don't care about signals.
    (\e -> e { Score.eventStart = start })
    . Score.addFlags Flags.lyCode
    . Score.modifyEnviron modify
    where
    modify = case pos of
        SetEnviron key -> Env.insertVal key code
        Position pos -> Constants.withFreeCode pos code

-- | Derive with the 'Constants.ly_global' instrument.
global :: Derive.Deriver a -> Derive.Deriver a
global = Derive.withValRaw EnvKey.instrument Constants.lyGlobal

-- | Test if an event is a 0 duration lilypond code event.
isCode0 :: Score.Event -> Bool
isCode0 event = Score.eventDuration event == 0
    && Flags.has (Score.eventFlags event) Flags.lyCode

isCode :: Score.Event -> Bool
isCode event = Flags.has (Score.eventFlags event) Flags.lyCode

-- *** low level

-- | Add lilypond code to a note.  Skip 'is_code' events, since those aren't
-- actual notes.
addNoteCode :: Code -> Score.Event -> Score.Event
addNoteCode (pos, code) event
    | isCode event = event
    | otherwise = (`Score.modifyEnviron` event) $ case pos of
        SetEnviron key -> Env.insertVal key code
        Position pos -> Constants.withCode pos code

-- ** convert

-- | Round the RealTime to the nearest NoteDuration.
noteDuration :: Types.Config -> RealTime -> Types.NoteDuration
noteDuration config = Types.timeToNoteDur . toTime config

-- | Like 'note_duration', but only succeeds if the RealTime is exactly
-- a NoteDuration.
isNoteDuration :: Types.Config -> RealTime -> Maybe Types.NoteDuration
isNoteDuration config = Types.isNoteDur . toTime config

isDuration :: Types.Config -> RealTime -> Maybe Types.Duration
isDuration config t = case isNoteDuration config t of
    Just (Types.NoteDuration dur False) -> Just dur
    _ -> Nothing

notePitch :: Derive.NoteDeriver -> Derive.Deriver Note
notePitch deriver = do
    events <- deriver
    event <- require "had no event" $ Lists.head (Stream.eventsOf events)
    env <- Derive.getEnviron
    pitchToLily env
        =<< require "note had no pitch" (Score.initialPitch event)
    -- Wow, there are a lot of ways to fail.
    where
    require = Derive.require . (prefix <>)
    prefix = "Ly.note_pitch: "

pitchToLily :: Env.Environ -> PSignal.Transposed -> Derive.Deriver Note
pitchToLily env = fmap Types.toLily
    . Derive.requireRight ("Ly.pitch_to_lily: "<>) . Convert.pitchToLily env

toTime :: Types.Config -> RealTime -> Types.Time
toTime = Types.realToTime . Types.configQuarterDuration
