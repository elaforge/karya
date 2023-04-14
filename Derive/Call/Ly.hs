-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for calls to cooperate with the lilypond backend.
module Derive.Call.Ly where
import qualified Data.Map as Map

import qualified Util.Lists as Lists
import qualified Util.Seq as Seq
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

when_lilypond :: Derive.Deriver a -- ^ Run if this is a lilypond derive.
    -> Derive.Deriver a -- ^ Run if this is a normal derive.
    -> Derive.Deriver a
when_lilypond lily = when_lilypond_config (const lily)

when_lilypond_config :: (Types.Config -> Derive.Deriver a)
    -- ^ Run if this is a lilypond derive.
    -> Derive.Deriver a -- ^ Run if this is a normal derive.
    -> Derive.Deriver a
when_lilypond_config lily not_lily =
    maybe not_lily lily =<< Derive.lookup_lilypond_config

-- | Only emit the deriver if I'm in lilypond mode.
only_lilypond :: Derive.NoteDeriver -> Derive.NoteDeriver
only_lilypond deriver = ifM Derive.is_lilypond_mode deriver mempty

-- | When in lilypond mode, generate a note with the given Code.
note_code :: Code -> Derive.PassedArgs d -> Derive.NoteDeriver
    -> Derive.NoteDeriver
note_code code args = when_lilypond $ add_first code $ Call.placed_note args

-- ** transformer

-- | Add the Code to all the events.
add_all :: Code -> Derive.NoteDeriver -> Derive.NoteDeriver
add_all code = fmap $ Post.emap1_ (add_note_code code)

-- | Add Code to the first event.
add_first :: Code -> Derive.NoteDeriver -> Derive.NoteDeriver
add_first code deriver =
    Stream.first (not . is_code) (add_note_code code) <$> deriver

prepend :: Position Constants.CodePosition
prepend = Position $
    Constants.CodePosition Constants.Chord Constants.Prepend Constants.First

append, note_append :: Constants.Distribution -> Position Constants.CodePosition
append = Position . Constants.CodePosition Constants.Chord Constants.Append
note_append = Position . Constants.CodePosition Constants.Note Constants.Append

-- ** note parent

-- | Replace a note parent with one that derives its sub-events as-is
-- and adds lilypond code to them.
notes_code :: Code -> Derive.PassedArgs d
    -> Derive.NoteDeriver -> Derive.NoteDeriver
notes_code code = notes_with (add_all code)

-- | Like 'notes_code', but only apply the code to the first event, not all of
-- them.
first_note_code :: Code -> Derive.PassedArgs d
    -> Derive.NoteDeriver -> Derive.NoteDeriver
first_note_code code args = when_lilypond $
    add_first code $ Sub.derive_subs args

-- | This is like 'notes_code', but the first event in each track gets the
-- start code, and the last event in each track gets the end code.
notes_around :: Code -> Code -> Derive.PassedArgs d
    -> Derive.NoteDeriver -> Derive.NoteDeriver
notes_around start end args =
    when_lilypond $ mconcatMap around =<< Sub.sub_events args
    where
    around notes = first_last
        (add_note_code start) (add_note_code end) <$> Sub.derive notes

-- | Like 'notes_around', but for use when you already know you're in lilypond
-- mode.
notes_around_ly :: Code -> Code -> Derive.PassedArgs d -> Derive.NoteDeriver
notes_around_ly start end = mconcatMap around <=< Sub.sub_events
    where
    around notes = first_last
        (add_note_code start) (add_note_code end) <$> Sub.derive notes

-- | Like 'notes_around', but when I'm not in lilypond mode just derive the
-- sub events unchanged.
code_around :: FreeCode -> FreeCode -> Derive.PassedArgs d -> Derive.NoteDeriver
code_around start end args = when_lilypond
    (code0 (Args.start args) start
        <> Sub.derive_subs args <> code0 (Args.end args) end)
    (Sub.derive_subs args)

-- | Transform and evaluate the sub events.
notes_with :: (Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Derive.PassedArgs d
    -> Derive.NoteDeriver -> Derive.NoteDeriver
notes_with f args = when_lilypond $
    Sub.derive . map (fmap f) . concat =<< Sub.sub_events args

-- | Apply a function to the first and last Events, which are not 'is_code0'.
first_last :: (Score.Event -> Score.Event) -> (Score.Event -> Score.Event)
    -> Stream.Stream Score.Event -> Stream.Stream Score.Event
first_last = Stream.first_last (not . is_code)

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
    from_val = Typecheck.from_val_symbol code_position_names
    to_type Proxy = Typecheck.to_type_symbol (Map.keys code_position_names)

code_position_names :: Map Text (Position Constants.CodePosition)
code_position_names =
    Map.fromList $ Seq.key_on ShowVal.show_val $
        map Position Constants.all_positions

instance Typecheck.ToVal (Position Constants.CodePosition) where
    to_val = DeriveT.VStr . Expr.Str . ShowVal.show_val

instance ShowVal.ShowVal (Position Constants.CodePosition) where
    show_val = Texts.dropPrefix "ly-" . key_of
        where
        key_of (SetEnviron k) = k
        key_of (Position pos) = Constants.position_key pos

-- | Fragment of Lilypond code.
type Ly = Text

-- | A lilypond \"note\", which is just a chunk of text.
type Note = Ly

-- | Like 'code', but for 0 duration code fragments, and can either put them
-- before or after notes that occur at the same time.
code0 :: ScoreTime -> FreeCode -> Derive.NoteDeriver
code0 start code = do
    rstart <- Derive.real start
    Post.emap1_ (code0_event rstart code) <$> Derive.place start 0 Call.note

-- | Make a code0 event directly.  Inherit instrument and environ from an
-- existing note.  Otherwise, the lilypond backend doesn't know how to group
-- the code event.
--
-- TODO aka free_code, maybe rename it?
code0_event :: RealTime -> FreeCode -> Score.Event -> Score.Event
code0_event start (pos, code) =
    -- I don't use Score.move_event because I don't care about signals.
    (\e -> e { Score.event_start = start })
    . Score.add_flags Flags.ly_code
    . Score.modify_environ modify
    where
    modify = case pos of
        SetEnviron key -> Env.insert_val key code
        Position pos -> Constants.with_free_code pos code

-- | Derive with the 'Constants.ly_global' instrument.
global :: Derive.Deriver a -> Derive.Deriver a
global = Derive.with_val_raw EnvKey.instrument Constants.ly_global

-- | Test if an event is a 0 duration lilypond code event.
is_code0 :: Score.Event -> Bool
is_code0 event = Score.event_duration event == 0
    && Flags.has (Score.event_flags event) Flags.ly_code

is_code :: Score.Event -> Bool
is_code event = Flags.has (Score.event_flags event) Flags.ly_code

-- *** low level

-- | Add lilypond code to a note.  Skip 'is_code' events, since those aren't
-- actual notes.
add_note_code :: Code -> Score.Event -> Score.Event
add_note_code (pos, code) event
    | is_code event = event
    | otherwise = (`Score.modify_environ` event) $ case pos of
        SetEnviron key -> Env.insert_val key code
        Position pos -> Constants.with_code pos code

-- ** convert

-- | Round the RealTime to the nearest NoteDuration.
note_duration :: Types.Config -> RealTime -> Types.NoteDuration
note_duration config = Types.time_to_note_dur . to_time config

-- | Like 'note_duration', but only succeeds if the RealTime is exactly
-- a NoteDuration.
is_note_duration :: Types.Config -> RealTime -> Maybe Types.NoteDuration
is_note_duration config = Types.is_note_dur . to_time config

is_duration :: Types.Config -> RealTime -> Maybe Types.Duration
is_duration config t = case is_note_duration config t of
    Just (Types.NoteDuration dur False) -> Just dur
    _ -> Nothing

note_pitch :: Derive.NoteDeriver -> Derive.Deriver Note
note_pitch deriver = do
    events <- deriver
    event <- require "had no event" $ Lists.head (Stream.events_of events)
    env <- Derive.get_environ
    pitch_to_lily env
        =<< require "note had no pitch" (Score.initial_pitch event)
    -- Wow, there are a lot of ways to fail.
    where
    require = Derive.require . (prefix <>)
    prefix = "Ly.note_pitch: "

pitch_to_lily :: Env.Environ -> PSignal.Transposed -> Derive.Deriver Note
pitch_to_lily env = fmap Types.to_lily
    . Derive.require_right ("Ly.pitch_to_lily: "<>) . Convert.pitch_to_lily env

to_time :: Types.Config -> RealTime -> Types.Time
to_time = Types.real_to_time . Types.config_quarter_duration
