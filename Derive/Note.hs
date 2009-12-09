{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- Control.Monad
{- | Derive events on a note track.  This is the Derive equivalent of
    "Cmd.NoteTrack", but has a different name to avoid clashes.

    Note tracks have a \"function call\" abstraction mechanism.  An event in
    one block is expanded into the events derived from the block it names, and
    so on recursively.

    The sub-block is passed its parent's tempo map (along with all the other
    controls in the environment) to interpret as it will, so that it may,
    for example, set absolute tempo.  The generated events should begin with
    the given start time, but are not guaranteed to last for the given
    duration.

    The parse section parses note track events as manipulated by
    "Cmd.NoteTrack".

    Note track language:

    Notes are created from a combination of the event text and the track title.
    The track title is simply prepended to the contents of each event in that
    track.  There are several kinds of words:

    - A word starting with @--@ is a comment, and that word and every
    subsequent word is ignored.  At the least this is useful for tests to
    keep track of events.

    - Attributes are prepended by @+@, @-@, or @=@ are collected into a set,
    which is placed in the environment.  @+@ adds to the set, @-@ subtracts,
    and @=@ sets it.

    - Instruments are prepended by @>@ and the textually last one is placed in
    the environment, replacing what was previously there.

    - The call is the first word that remains after the above is stripped out.

    - Args are the words after the call.

    Args can be:

    [@ 1.23 @]              floating point number

    [@ letters @]           string

    [@ \@var @]              variable from the environment (TODO implement)

    [@ *note @]             note from a scale (TODO which one?)

    Usage:

        >>inst      t *4c- *4d- *4e-    triplet played with inst
        >>inst      g -2                grace from pitch-2 to pitch
        >>inst      ns/b1               subderive block "ns/b1", which
        >                               inherits >inst if it doesn't set its own
        >>inst1     >inst2 t a b        set >inst2 and call "t a b"
        >>inst      seed 123            random seed set to 123
        >>drums     snare               play "drums" inst with "snare" arg
        >>          ""                  inherit inst from calling block

    The call can be several things:

    [@ func @] @func@ will be looked up in a set of hardcoded derives, and if
    that fails, as block proj/func to be subderived.

    [@ns/block@ or @/block@] As the above, only it's always a block.

    Edit:

    - In raw mode, note inserts the relevant degree, backspace deletes a char
    or a degree.

    - In note mode, a note replaces the last 'note' event after splitting ';',
    or adds one if there isn't one already.  Backspace removes the last note
    event.  If it was the only note expression, remove the event.

    - In method mode, it edits the last note event or adds one, just like note
    mode.
-}
module Derive.Note where
import Control.Monad
import qualified Control.Monad.Identity as Identity
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Text.ParserCombinators.Parsec as P

import Util.Control ((#>>))
import qualified Util.Log as Log
import qualified Util.Parse as Parse
import Util.Seq as Seq

import Ui
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Derive.Control as Control
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal


type NoteParser m = Track.PosEvent -> Derive.DeriveT m [Parsed]

data Parsed = Parsed
    { parsed_call :: String
    , parsed_args :: [CallArg]
    , parsed_inst :: Maybe Score.Instrument
    , parsed_attrs :: Score.Attributes
    } deriving (Eq, Show)

data CallArg = Number Double | String String | Note Pitch.Note
    deriving (Eq, Show)

-- * note track

d_note_track :: (Monad m) => Derive.TrackDeriver m
d_note_track track_id = do
    track <- Derive.get_track track_id
    let pos_events = Track.event_list (Track.track_events track)
    pos_parsed <- parse_events (Track.track_title track) pos_events
    derive_notes pos_parsed

-- ** parse

-- | Convert each Event.Event into a Parsed, stripping out the rest of
-- the Event except start and dur.
parse_events :: (Monad m) => String -> [Track.PosEvent]
    -> Derive.DeriveT m [(Track.PosEvent, Parsed)]
parse_events track_title pos_events = do
    inst <- fmap Derive.state_instrument Derive.get
    attrs <- fmap Derive.state_attributes Derive.get
    maybe_parsed <- mapM
        (Derive.catch_warn . (parse_event inst attrs track_title))
        pos_events
    return [(pos_event, parsed)
        | (pos_event, Just parsed) <- zip pos_events maybe_parsed]

parse_event :: Monad m => Maybe Score.Instrument -> Score.Attributes
    -> String -> Track.PosEvent -> Derive.DeriveT m Parsed
parse_event inst attrs track_title (pos, event) = do
    let (parsed, errs) =
            parse_note inst attrs track_title (Event.event_text event)
    if (null errs)
        then return parsed
        else Derive.with_stack_pos pos (Event.event_duration event) $
            Derive.throw $ "parsing: " ++ Seq.join "; " errs

parse_note :: Maybe Score.Instrument -> Score.Attributes -> String -> Text.Text
    -> (Parsed, [String])
parse_note env_inst env_attrs title event = (Parsed call args inst attrs, errs)
    where
    -- It may be worth it to try to keep this packed, but the rest of the code
    -- is String so I'll punt on it now.
    event_words = map Text.unpack (Text.words event)
    -- I have to thread word numbers through to have nice error msgs.
    ws0 = describe "title" (words title) ++ describe "event" event_words
    describe desc xs =
        [(desc ++ " word " ++ show i, x) | (i, x) <- zip [0..] xs]
    (inst, attrs, ws1) = preprocess_words env_inst env_attrs ws0
    (call, args, errs) = parse_args ws1

preprocess_words :: Maybe Score.Instrument -> Score.Attributes
    -> [(String, String)]
    -> (Maybe Score.Instrument, Score.Attributes, [(String, String)])
preprocess_words env_inst env_attrs ws = (inst, attrs, reverse ws2_rev)
    where
    -- foldl is most convenient for left-to-right set processing but it gets
    -- the output backwards.
    (attrs, ws1_rev) = List.foldl' parse_attr (env_attrs, []) (strip_comment ws)
    (inst, ws2_rev) = List.foldl' parse_inst (env_inst, []) (reverse ws1_rev)
    parse_attr (attrs, rest) desc_word = case snd desc_word of
        '+':attr -> (Set.insert attr attrs, rest)
        '-':attr -> (Set.delete attr attrs, rest)
        "=" -> (Set.empty, rest)
        '=':attr -> (Set.singleton attr, rest)
        _ -> (attrs, desc_word:rest)
    parse_inst (inst, rest) desc_word = case snd desc_word of
        -- Bare '>' is probably a track title, and I don't want that to
        -- override the inst from the environment.
        ">" -> (inst, rest)
        '>':inst_name -> (Just (Score.Instrument inst_name), rest)
        _ -> (inst, desc_word:rest)

strip_comment [] = []
strip_comment (w@(_, word):ws)
    | "--" `List.isPrefixOf` word = []
    | otherwise = w : strip_comment ws

parse_args :: [(String, String)] -> (String, [CallArg], [String])
parse_args ws = (call, args, errs1 ++ errs2)
    where
    (errs1, args) = Seq.partition_either $ map parse_arg (drop 1 ws)
    call = if null ws then "" else snd (head ws)
    -- I can lift this restriction if convenient, but it seems like it may
    -- catch some typos.
    errs2 = case call of
        (c:_) | not (Char.isLetter c) ->
            ["call " ++ show call ++ " starts with non-letter " ++ show c]
        _ -> []

parse_arg :: (String, String) -> Either String CallArg
parse_arg (desc, "") = Left $ desc ++ ": empty word, this shouldn't happen!"
parse_arg (desc, word@(c:rest))
    | Char.isDigit c || c == '.' = case Parse.float word of
        Just val -> Right (Number val)
        Nothing -> Left $ desc ++ ": can't parse number " ++ show word
    | c == '*' = Right (Note (Pitch.Note rest))
    | otherwise = Right (String word)

-- ** derive

derive_notes :: (Monad m) => [(Track.PosEvent, Parsed)]
    -> Derive.DeriveT m [Score.Event]
derive_notes = fmap (trim_pitches . concat) . mapM note
    where
    note ((pos, event), parsed) = Derive.with_stack_pos
        pos (Event.event_duration event) (derive_note pos event parsed)

derive_note :: (Monad m) => TrackPos -> Event.Event -> Parsed
    -> Derive.DeriveT m [Score.Event]
derive_note pos event (Parsed call args inst attrs) = do
    -- TODO when signals are lazy this will be inefficient.  I need to come
    -- up with a way to guarantee such accesses are increasing and let me gc
    -- the head.
    start <- Derive.local_to_global pos
    end <- Derive.local_to_global (pos + Event.event_duration event)
    -- Log.debug $ show (Event.event_string event) ++ ": local global "
    --     ++ show ((pos, start), (pos + Event.event_duration event, end))
    -- Derive.Warp wp stretch shift <- fmap Derive.state_warp Derive.get
    -- Log.debug $ "warp " ++ (show wp)
    st <- Derive.get

    if null call
        then do
            unless (null args) $
                -- Null call but non-null args shouldn't happen.
                Derive.warn ("ignored trailing args: " ++ show args)
            return [Score.Event start (end-start) (Event.event_text event)
                (Derive.state_controls st) (Derive.state_pitch st)
                (Derive.state_stack st) inst attrs]
        -- d_call will set shift and stretch which is in local time, so pass
        -- local rather than global.
        -- TODO look in namespace for calls other than subderive
        -- TODO pass args
        else Derive.d_sub_derive [] $ Derive.with_instrument inst attrs $
            d_call pos (Event.event_duration event) call

-- | In a note track, the pitch signal for each note ends when the next note
-- begins.  Otherwise, it looks like each note changes pitch when the next note
-- begins.  Of course, the note is already \"done\" at this point, but the
-- decay time means it may still be audible.
trim_pitches :: [Score.Event] -> [Score.Event]
trim_pitches events = map trim_event (Seq.zip_next events)
    where
    trim_event (event, Nothing) = event
    trim_event (event, Just next) =
        event { Score.event_pitch = PitchSignal.truncate p psig }
        where
        psig = Score.event_pitch event
        p = Score.event_start next

d_call :: TrackPos -> TrackPos -> String -> Derive.EventDeriver
d_call start dur ident = do
    -- TODO also I'll want to support generic calls
    default_ns <- fmap (State.state_project . Derive.state_ui) Derive.get
    let block_id = Types.BlockId (make_id default_ns ident)
    stack <- fmap Derive.state_stack Derive.get
    -- Since there is no branching, any recursion will be endless.
    when (block_id `elem` [bid | (bid, _, _) <- stack]) $
        Derive.throw $ "recursive block derivation: " ++ show block_id
    -- Stretch call to fit in duration, based on the block length.
    -- An alternate approach would be no stretch, but just clip, but I'm
    -- not sure how to indicate that kind of derivation.
    -- This is actually the only thing that requires that block_id name a real
    -- block.
    ui_state <- fmap Derive.state_ui Derive.get
    block_dur <- either (Derive.throw . ("d_call: "++) . show)
        return (State.eval ui_state (get_block_dur block_id))
    if block_dur > TrackPos 0
        then Derive.d_at start (Derive.d_stretch (dur/block_dur)
            (Derive.d_block block_id))
        else do
            Derive.warn $ "block with zero duration: " ++ show block_id
            return []

-- | Sub-derived blocks are stretched according to their length, and this
-- function defines the length of a block.  'event_end' seems the most
-- intuitive, but then you can't make blocks with trailing space.  You can
-- work around it though by appending a comment dummy event.
get_block_dur :: (State.UiStateMonad m) => BlockId -> m TrackPos
get_block_dur = State.event_end


-- | Make an Id from a string, relative to the current ns if it doesn't already
-- have one.
--
-- TODO move this to a more generic place since LanguageCmds may want it to?

make_id :: String -> String -> Id.Id
make_id default_ns ident_str = Id.id ns ident
    where
    (w0, w1) = break (=='/') ident_str
    (ns, ident) = if null w1 then (default_ns, w0) else (w0, drop 1 w1)


-- * old crap kept alive by Cmd.NoteTrack

-- | (method, note)
type NoteTokens = (String, String)

tokenize_note :: String -> Either String NoteTokens
tokenize_note text = case Seq.split_commas text of
    [] -> Right ("", "")
    [w0] -> Right ("", w0)
    [w0, w1] -> Right (w0, w1)
    ws -> Left $ "too many words in note: " ++ show ws

untokenize_note :: NoteTokens -> String
untokenize_note toks = case toks of
    ("", "") -> ""
    ("", note) -> note
    (meth, note) -> meth ++ "," ++ note
