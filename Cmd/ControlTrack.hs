-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
module Cmd.ControlTrack (
    cmd_val_edit, cmd_tempo_val_edit
    , cmd_method_edit
    -- * edit
    , val_edit_at
    , modify_val
    -- * Partial
    , Partial(..)
    , parse, unparse
    , parse_general, unparse_general
#ifdef TESTING
    , module Cmd.ControlTrack
#endif
) where
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection

import qualified Derive.DeriveT as DeriveT
import qualified Derive.Parse as Parse
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Key as Key
import qualified Ui.Ui as Ui

import           Global
import           Types


{- | Accept keystrokes and modify the val field of the event.  Also accept
    'InputNote.NoteOn' or 'InputNote.Control' msgs and enter a value based on
    their velocity or value, respectively.  So you can use a MIDI knob to set
    arbitrary control values.

    Since control vals are typically normalized between 0 and 1, this accepts
    hexadecimal higits and modifies the event text with 'modify_hex'.  However,
    not all tracks are normalized, so this only happens if 'infer_normalized'
    thinks that it's normalized.

    The @'@ key will enter a @'@ call, which repeats the last value.  This is
    useful to extend a constant pitch value to the desired breakpoint.
-}
cmd_val_edit :: Cmd.M m => Msg.Msg -> m Cmd.Status
cmd_val_edit msg = suppress "control track val edit" $ do
    EditUtil.fallthrough msg
    (_, _, track_id, _) <- Selection.get_insert
    ifM (infer_normalized track_id)
        (edit_normalized msg) (edit_non_normalized msg)
    return Cmd.Done
    where suppress = Cmd.suppress_history Cmd.ValEdit

-- | Editing a tempo track is just like editing a normal control track, except
-- that it doesn't do the hex entry thing.
cmd_tempo_val_edit :: Cmd.M m => Msg.Msg -> m Cmd.Status
cmd_tempo_val_edit msg = suppress "tempo track val edit" $ do
    EditUtil.fallthrough msg
    edit_non_normalized msg
    return Cmd.Done
    where suppress = Cmd.suppress_history Cmd.ValEdit

-- | A track is assumed to be normalized if its first event has a @`0x`@ in it.
-- If the track has no first event, then it defaults to normalized.
--
-- TODO This is kind of bogus since it's just the first event and it just looks
-- for a substring.  A better check would be to see if the event being edited
-- can have a normalized number extracted from it, and fall back on this only
-- if there is no existing event.
infer_normalized :: Ui.M m => TrackId -> m Bool
infer_normalized =
    -- Don't get fooled by the ' call, which is fairly common.
    fmap (maybe True normal . Seq.head . dropWhile (=="'") . map Event.text
        . Events.ascending)
    . Ui.get_events
    where
    normal event = any (`Text.isInfixOf` event) normalized_prefixes

normalized_prefixes :: [Text]
normalized_prefixes = ["`0x`", "0x"]

edit_non_normalized :: Cmd.M m => Msg.Msg -> m ()
edit_non_normalized msg = case msg of
    (EditUtil.num_key -> Just key) -> modify_event (modify_num key)
    (Msg.key_down -> Just (Key.Char '\'')) -> EditUtil.soft_insert "'"
    _ -> Cmd.abort

edit_normalized :: Cmd.M m => Msg.Msg -> m ()
edit_normalized msg = case msg of
    (EditUtil.hex_key -> Just key) -> modify_event (modify_hex key)
    (Msg.key_down -> Just (Key.Char '\'')) -> EditUtil.soft_insert "'"
    -- AsciiKbd won't have an interesting velocity.
    Msg.InputNote (InputNote.NoteOn _ (Pitch.Input Pitch.PianoKbd _ _) vel) ->
        insert_val False vel
    Msg.InputNote (InputNote.Control _ _ val) -> insert_val True val
    _ -> Cmd.abort
    where
    insert_val control_input val = do
        pos <- EditUtil.get_pos
        val_edit_at pos val
        -- Never advance for control input, because there are usually a lot
        -- of those at once.
        whenM (andM [return (not control_input),
                Cmd.gets (Cmd.state_advance . Cmd.state_edit)])
            Selection.advance

modify_num :: EditUtil.Key -> Modify
modify_num key partial =
    case EditUtil.modify_text_key [] key (_val partial) of
        Nothing -> (Nothing, Text.null $ _val partial)
        Just new_val -> (Just $ partial { _val = new_val }, False)

{- | This is tricky because the editing mode is different depending on whether
    the val is hex or not.

    If it's hex or null, expect higits and rotate them into the value, always
    staying in the form @`0x`##@.  If it's not hex, act like
    'cmd_tempo_val_edit'.

    The one difference is that 'cmd_val_edit' catches all alphanum keys since it
    is expecting a-f, and will then ignore them if they are other letters, while
    'cmd_tempo_val_edit' only catches the keys it will use, passing the rest
    through.  It's already confusing enough which keys are caught by which
    editing mode, it would be even worse if it also depended on text of the
    event being edited.  TODO perhaps I should go further and catch alphanum
    for the tempo track too, for consistency.
-}
modify_hex :: EditUtil.Key -> Modify
modify_hex key partial
    | Just new_val <- update_hex (_val partial) key = case new_val of
        Nothing -> (Nothing, True)
        Just val -> (Just $ partial { _val = val }, False)
    | EditUtil.is_num_key key = modify_num key partial
    | otherwise = (Just partial, False)

-- | Nothing if the val is not a hex number, Just Nothing if it was but the key
-- was Backspace, and Just Just if it should get a new value.
update_hex :: Text -> EditUtil.Key -> Maybe (Maybe Text)
update_hex val_ key
    | Text.null val = case key of
        EditUtil.Backspace -> Just Nothing
        EditUtil.Key c
            | higit c -> Just $ Just $ ShowVal.hex_prefix <> Text.pack ['0', c]
            | otherwise -> Nothing
    | Just c2 <- parse_val val = case key of
        EditUtil.Backspace -> Just Nothing
        EditUtil.Key c
            | c == '-' -> Just $ Just $ if negative then val else "-" <> val
            | higit c -> Just $ Just $ prefix <> Text.pack [c2, c]
            -- The field is hex, but this wasn't a higit, so ignore it.
            | otherwise -> Just (Just val)
    | otherwise = Nothing -- not hex at all
    where
    prefix = (if negative then "-" else "") <> ShowVal.hex_prefix
    negative = "-" `Text.isPrefixOf` val_
    val = if negative then Text.drop 1 val_ else val_
    higit c = '0' <= c && c <= '9' || 'a' <= c && c <= 'f'
    parse_val t = case Text.unpack <$> strip t of
        Just [c1, c2] | higit c1 && higit c2 -> Just c2
        _ -> Nothing
        where strip t = msum $ map (($t) . Text.stripPrefix) normalized_prefixes

cmd_method_edit :: Cmd.M m => Msg.Msg -> m Cmd.Status
cmd_method_edit msg =
    Cmd.suppress_history Cmd.MethodEdit "control track method edit" $ do
    EditUtil.fallthrough msg
    case msg of
        (EditUtil.method_key -> Just key) -> modify_event $ \partial ->
            ( Just $ partial
                { _method = fromMaybe "" $
                    EditUtil.modify_text_key [] key (_method partial)
                }
            , False
            )
        _ -> Cmd.abort
    return Cmd.Done


-- * implementation

val_edit_at :: Cmd.M m => EditUtil.Pos -> Signal.Y -> m ()
val_edit_at pos val = modify_event_at pos $ \partial ->
    (Just $ partial { _val = ShowVal.show_hex_val val }, False)

-- | old -> (new, advance?)
type Modify = Partial -> (Maybe Partial, Bool)

modify_event :: Cmd.M m => Modify -> m ()
modify_event f = do
    pos <- EditUtil.get_pos
    modify_event_at pos f

modify_event_at :: Cmd.M m => EditUtil.Pos -> Modify -> m ()
modify_event_at pos f = EditUtil.modify_event_at pos True False
    (first (fmap unparse) . f . parse . fromMaybe "")

-- | Try to figure out where the note part is in event text and modify that
-- with the given function.
--
-- If the val was hex, keep it hex.
modify_val :: (Signal.Y -> Signal.Y) -> Text -> Maybe Text
    -- ^ Nothing if I couldn't parse out a VNum.
modify_val f text = case Parse.parse_val (_val partial) of
    Right (DeriveT.VNum n) -> Just $ unparse $
        partial { _val = show_val (f <$> n) }
    _ -> Nothing
    where
    partial = parse text
    show_val num
        | ScoreT.Typed ScoreT.Untyped n <- num,
            ShowVal.is_hex_val (_val partial) = ShowVal.show_hex_val n
        | otherwise = ShowVal.show_val (DeriveT.VNum num)

-- * Partial

{- | Partially-parse event text into method, val, and args.  Method is actually
    the call, val is the first argument to the calll, and args are the
    remaining arguments.  Control calls have a convention where the first
    argument is the value to set.  I separate it out so I can replace just that
    value while leaving any arguments intact.  E.g., exponential interpolation
    might look like @e 0 3@, where 0 is the destination and 3 is the exponent.
    Or @e (4c) 3@ in the case of pitches.  If I press a MIDI key I want to
    replace just the @4c@.

    The "method" terminology dates from back before calls existed.  Nowadays
    it's just a call, but for that matter so are numeric literals, so I need
    something to differentiate @1@ from @i 1@.
-}
data Partial = Partial {
    _transform :: [[Text]]
    , _method :: Text
    , _val :: Text
    , _args :: [Text]
    , _comment :: Text
    } deriving (Show, Eq)

-- | Try to figure out the call part of the expression and split it from the
-- rest.
--
-- I use a trailing space to tell the difference between a method and a val.
parse :: Text -> Partial
parse = parse_general (\method val args -> (method, val, args))

parse_general :: (Text -> Text -> [Text] -> (Text, Text, [Text]))
    -> Text -> Partial
parse_general split_expr = make . Seq.viewr . Parse.split_pipeline
    where
    make Nothing = Partial [] "" "" [] ""
    make (Just (transform, expr)) = Partial
        { _transform = transform
        , _method = Text.strip method
        , _val = Text.strip val
        , _args = args
        , _comment = comment
        }
        where
        (expr2, comment) = case Seq.viewr expr of
            Just (expr, comment) | "--" `Text.isPrefixOf` comment ->
                (expr, comment)
            _ -> (expr, "")
        (method, val, args) = case expr2 of
            method : val : args -> split_expr method val args
            [arg]
                | " " `Text.isSuffixOf` arg -> (arg, "", [])
                | otherwise -> ("", arg, [])
            [] -> ("", "", [])

unparse_general :: (Text -> Text -> [Text] -> [Text]) -> Partial -> Text
unparse_general join_expr (Partial transform method val args comment) =
    Parse.join_pipeline $ transform ++ [Seq.map_init (<>" ") (expr ++ comments)]
    where
    comments = if Text.null comment then [] else [comment]
    expr = join_expr method val args

unparse :: Partial -> Text
unparse = unparse_general join_expr
    where
    join_expr method val args
        | Text.null method && Text.null val = args
        | Text.null method = val : args
        | otherwise = [method, Text.unwords (val : args)]
