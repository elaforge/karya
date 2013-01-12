{-# LANGUAGE ViewPatterns #-}
module Cmd.ControlTrack where
import qualified Data.ByteString.Char8 as ByteString.Char8

import Util.Control
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection

import qualified Derive.ParseBs as ParseBs
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal


cmd_raw_edit :: Cmd.Cmd
cmd_raw_edit = Cmd.suppress_history Cmd.RawEdit "control track raw edit"
    . EditUtil.raw_edit True

-- | Accept keystrokes and modify the val field of the event.  Also accept
-- 'InputNote.NoteOn' or 'InputNote.Control' msgs and enter a value based on
-- their velocity or value, respectively.
cmd_val_edit :: Cmd.Cmd
cmd_val_edit msg = suppress "control track val edit" $ do
    EditUtil.fallthrough msg
    case msg of
        (EditUtil.alphanum_key -> Just key) -> modify_event (modify_hex key)
        (Msg.key_down -> Just (Key.Char '\'')) -> EditUtil.soft_insert "'"
        Msg.InputNote (InputNote.NoteOn _ _ vel) -> insert_val False vel
        Msg.InputNote (InputNote.Control _ _ val) -> insert_val True val
        _ -> Cmd.abort
    return Cmd.Done
    where
    suppress = Cmd.suppress_history Cmd.ValEdit
    insert_val control_input val = do
        pos <- Selection.get_insert_pos
        val_edit_at pos val
        -- Never advance for control input, because there are usually a lot
        -- of those at once.
        whenM (andM [return (not control_input),
                Cmd.gets (Cmd.state_advance . Cmd.state_edit)])
            Selection.advance

cmd_tempo_val_edit :: Cmd.Cmd
cmd_tempo_val_edit msg = suppress "tempo track val edit" $ do
    EditUtil.fallthrough msg
    case msg of
        (EditUtil.num_key -> Just key) -> modify_event (modify_num key)
        (Msg.key_down -> Just (Key.Char '\'')) -> EditUtil.soft_insert "'"
        _ -> Cmd.abort
    return Cmd.Done
    where suppress = Cmd.suppress_history Cmd.ValEdit

modify_num :: Key.Key -> Modify
modify_num key event =
    case EditUtil.modify_text_key [] key (event_val event) of
        Nothing -> (Nothing, null (event_val event))
        Just new_val -> (Just $ event { event_val = new_val }, False)

-- | This is tricky because the editing mode is different depending on whether
-- the val is hex or not.
--
-- If it's hex or null, expect higits and rotate them into the value, always
-- staying in the form `0x`##.  If it's not hex, act like 'cmd_tempo_val_edit'.
--
-- The one difference is that 'cmd_val_edit' catches all alphanum keys since it
-- is expecting a-f, and will then ignore them if they are other letters, while
-- 'cmd_tempo_val_edit' only catches the keys it will use, passing the rest
-- through.  It's already confusing enough which keys are caught by which
-- editing mode, it would be even worse if it also depended on text of the
-- event being editing.  TODO perhaps I should go further and catch alphanum
-- for the tempo track too, for consistency.
modify_hex :: Key.Key -> Modify
modify_hex key event
    | Just new_val <- update_hex (event_val event) key = case new_val of
        Nothing -> (Nothing, True)
        Just val -> (Just $ event { event_val = val }, False)
    | EditUtil.is_num_key key = modify_num key event
    | otherwise = (Just event, False)

-- | Nothing if the val is not a hex number, Just Nothing if it was but the key
-- was Backspace, and Just Just if it should get a new value.
update_hex :: String -> Key.Key -> Maybe (Maybe String)
update_hex val key
    | null val = case key of
        Key.Backspace -> Just Nothing
        Key.Char c | higit c -> Just $ Just $ ShowVal.hex_prefix ++ ['0', c]
        _ -> Nothing
    | Just c2 <- parse_val val = case key of
        Key.Backspace -> Just Nothing
        Key.Char c | higit c -> Just $ Just $ ShowVal.hex_prefix ++ [c2, c]
        -- The field is hex, but this wasn't a higit, so ignore it.
        _ -> Just $ Just val
    | otherwise = Nothing -- not hex at all
    where
    higit c = '0' <= c && c <= '9' || 'a' <= c && c <= 'f'
    parse_val ['`', '0', 'x', '`', c1, c2]
        | higit c1 && higit c2 = Just c2
    parse_val _ = Nothing

cmd_method_edit :: Cmd.Cmd
cmd_method_edit msg =
    Cmd.suppress_history Cmd.MethodEdit "control track method edit" $ do
    EditUtil.fallthrough msg
    case msg of
        (EditUtil.method_key -> Just key) -> modify_event $ \event ->
            (Just $ event { event_method = fromMaybe "" $
                    EditUtil.modify_text_key [] key (event_method event) },
                False)
        _ -> Cmd.abort
    return Cmd.Done


-- * implementation

val_edit_at :: (Cmd.M m) => State.Pos -> Signal.Y -> m ()
val_edit_at pos val = modify_event_at pos $ \event ->
    (Just $ event { event_val = ShowVal.show_hex_val val }, False)

data Event = Event {
    event_method :: String
    , event_val :: String
    , event_args :: String
    } deriving (Eq, Show)

-- | old_event -> (new_event, advance?)
type Modify = Event -> (Maybe Event, Bool)

modify_event :: (Cmd.M m) => Modify -> m ()
modify_event f = do
    pos <- Selection.get_insert_pos
    modify_event_at pos f

modify_event_at :: (Cmd.M m) => State.Pos -> Modify -> m ()
modify_event_at pos f = EditUtil.modify_event_at pos True True
    (first (fmap unparse) . f . parse . fromMaybe "")

-- | Try to figure out the call part of the expression and split it from the
-- rest.
--
-- I use a trailing space to tell the difference between a method and a val.
--
-- > "x"        -> Event { method = "", val = x, args = "" }
-- > "x "       -> Event { method = x, val = "", args = "" }
-- > "x y"      -> Event { method = x, val = y, args = "" }
-- > "x y z"    -> Event { method = x, val = y, args = z }
--
-- The val itself can't have args, because it will then be mistaken for the
-- method.  E.g. given @.5 0@, the @0@ will be considered the val while @.5@ is
-- the method.  This isn't a problem for control calls, which are just numbers
-- and don't take arguments.
--
-- TODO The event is already bytestring, why don't I just directly give it to
-- lex1?
parse :: String -> Event
parse s
    | null post = Event "" pre ""
    | post == " " = Event pre "" ""
    | otherwise = split_args pre (drop 1 post)
    where (pre, post) = break (==' ') s

split_args :: String -> String -> Event
split_args method rest = case ParseBs.lex1 (ParseBs.from_string rest) of
    Nothing -> Event method rest ""
    Just (w, ws) -> Event method (ParseBs.to_string (rstrip w))
        (ParseBs.to_string ws)
    where rstrip = fst . ByteString.Char8.spanEnd (==' ')

unparse :: Event -> String
unparse (Event method val args)
    | null method && null val = ""
    | null method = val -- No method means no args, see comment on 'parse'.
    | otherwise = unwords $ method : val : if null args then [] else [args]

-- | Try to figure out where the note part is in event text and modify that
-- with the given function.
modify_val :: (Signal.Y -> Signal.Y) -> String -> Maybe String
    -- ^ Nothing if I couldn't parse out a VNum.
modify_val f text = case ParseBs.parse_val (event_val event) of
        Right (TrackLang.VNum n) -> Just $ unparse $ event
            { event_val = TrackLang.show_val $ TrackLang.VNum (f <$> n) }
        _ -> Nothing
    where event = parse text
