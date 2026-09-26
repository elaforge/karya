-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
module Cmd.ControlTrack (
    cmdValEdit, cmdTempoValEdit
    , cmdMethodEdit
    -- * edit
    , valEditAt
    , modifyVal
    -- * Partial
    , Partial(..)
    , parse, unparse
    , parseGeneral, unparseGeneral
#ifdef TESTING
    , module Cmd.ControlTrack
#endif
) where
import qualified Data.Text as Text

import qualified Util.Lists as Lists
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
cmdValEdit :: Cmd.M m => Msg.Msg -> m Cmd.Status
cmdValEdit msg = suppress "control track val edit" $ do
    EditUtil.fallthrough EditUtil.WantBackspace msg
    (_, _, trackId, _) <- Selection.getInsert
    ifM (inferNormalized trackId)
        (editNormalized msg) (editNonNormalized msg)
    pure Cmd.Done
    where suppress = Cmd.suppressHistory Cmd.ValEdit

-- | Editing a tempo track is just like editing a normal control track, except
-- that it doesn't do the hex entry thing.
cmdTempoValEdit :: Cmd.M m => Msg.Msg -> m Cmd.Status
cmdTempoValEdit msg = suppress "tempo track val edit" $ do
    EditUtil.fallthrough EditUtil.WantBackspace msg
    editNonNormalized msg
    pure Cmd.Done
    where suppress = Cmd.suppressHistory Cmd.ValEdit

-- | A track is assumed to be normalized if its first event has a @`0x`@ in it.
-- If the track has no first event, then it defaults to normalized.
--
-- TODO This is kind of bogus since it's just the first event and it just looks
-- for a substring.  A better check would be to see if the event being edited
-- can have a normalized number extracted from it, and fall back on this only
-- if there is no existing event.
inferNormalized :: Ui.M m => TrackId -> m Bool
inferNormalized =
    -- Don't get fooled by the ' call, which is fairly common.
    fmap (maybe True normal . Lists.head . dropWhile (=="'") . map Event.text
        . Events.ascending)
    . Ui.getEvents
    where
    normal event = any (`Text.isInfixOf` event) normalizedPrefixes

normalizedPrefixes :: [Text]
normalizedPrefixes = ["`0x`", "0x"]

editNonNormalized :: Cmd.M m => Msg.Msg -> m ()
editNonNormalized = \case
    (EditUtil.numKey -> Just key) -> modifyEvent (modifyNum key)
    (Msg.keyDown -> Just (Key.Char '\'')) -> EditUtil.softInsert "'"
    _ -> Cmd.abort

editNormalized :: Cmd.M m => Msg.Msg -> m ()
editNormalized = \case
    (EditUtil.hexKey -> Just key) -> modifyEvent (modifyHex key)
    (Msg.keyDown -> Just (Key.Char '\'')) -> EditUtil.softInsert "'"
    -- AsciiKbd won't have an interesting velocity.
    Msg.InputNote (InputNote.NoteOn _ (Pitch.Input Pitch.PianoKbd _ _) vel) ->
        insertVal False vel
    Msg.InputNote (InputNote.Control _ _ val) -> insertVal True val
    _ -> Cmd.abort
    where
    insertVal controlInput val = do
        pos <- EditUtil.getPos
        valEditAt pos val
        -- Never advance for control input, because there are usually a lot
        -- of those at once.
        whenM (andM [pure (not controlInput),
                Cmd.gets (Cmd.stateAdvance . Cmd.stateEdit)])
            Selection.advance

modifyNum :: EditUtil.Key -> Modify
modifyNum key partial =
    case EditUtil.modifyTextKey [] key (_val partial) of
        Nothing -> (Nothing, Text.null $ _val partial)
        Just newVal -> (Just $ partial { _val = newVal }, False)

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
modifyHex :: EditUtil.Key -> Modify
modifyHex key partial
    | Just newVal <- updateHex (_val partial) key = case newVal of
        Nothing -> (Nothing, True)
        Just val -> (Just $ partial { _val = val }, False)
    | EditUtil.isNumKey key = modifyNum key partial
    | otherwise = (Just partial, False)

-- | Nothing if the val is not a hex number, Just Nothing if it was but the key
-- was Backspace, and Just Just if it should get a new value.
updateHex :: Text -> EditUtil.Key -> Maybe (Maybe Text)
updateHex val_ key
    | Text.null val = case key of
        EditUtil.Backspace -> Just Nothing
        EditUtil.Key c
            | higit c -> Just $ Just $ ShowVal.hexPrefix <> Text.pack ['0', c]
            | otherwise -> Nothing
    | Just c2 <- parseVal val = case key of
        EditUtil.Backspace -> Just Nothing
        EditUtil.Key c
            | c == '-' -> Just $ Just $ if negative then val else "-" <> val
            | higit c -> Just $ Just $ prefix <> Text.pack [c2, c]
            -- The field is hex, but this wasn't a higit, so ignore it.
            | otherwise -> Just (Just val)
    | otherwise = Nothing -- not hex at all
    where
    prefix = (if negative then "-" else "") <> ShowVal.hexPrefix
    negative = "-" `Text.isPrefixOf` val_
    val = if negative then Text.drop 1 val_ else val_
    higit c = '0' <= c && c <= '9' || 'a' <= c && c <= 'f'
    parseVal t = case Text.unpack <$> strip t of
        Just [c1, c2] | higit c1 && higit c2 -> Just c2
        _ -> Nothing
        where strip t = msum $ map (($t) . Text.stripPrefix) normalizedPrefixes

cmdMethodEdit :: Cmd.M m => Msg.Msg -> m Cmd.Status
cmdMethodEdit msg =
    Cmd.suppressHistory Cmd.MethodEdit "control track method edit" $ do
    EditUtil.fallthrough EditUtil.WantBackspace msg
    case msg of
        (EditUtil.methodKey -> Just key) -> modifyEvent $ \partial ->
            ( Just $ partial
                { _method = fromMaybe "" $
                    EditUtil.modifyTextKey [] key (_method partial)
                }
            , False
            )
        _ -> Cmd.abort
    pure Cmd.Done


-- * implementation

valEditAt :: Cmd.M m => EditUtil.Pos -> Signal.Y -> m ()
valEditAt pos val = modifyEventAt pos $ \partial ->
    (Just $ partial { _val = ShowVal.showHexVal val }, False)

-- | old -> (new, advance?)
type Modify = Partial -> (Maybe Partial, Bool)

modifyEvent :: Cmd.M m => Modify -> m ()
modifyEvent f = do
    pos <- EditUtil.getPos
    modifyEventAt pos f

modifyEventAt :: Cmd.M m => EditUtil.Pos -> Modify -> m ()
modifyEventAt pos f = EditUtil.modifyEventAt pos True False
    (first (fmap unparse) . f . parse . fromMaybe "")

-- | Try to figure out where the note part is in event text and modify that
-- with the given function.
--
-- If the val was hex, keep it hex.
modifyVal :: (Signal.Y -> Signal.Y) -> Text -> Maybe Text
    -- ^ Nothing if I couldn't parse out a constant number.
modifyVal f text = case Parse.parseVal (_val partial) of
    Right val | Just num <- DeriveT.constantVal val ->
        Just $ unparse $ partial { _val = showVal (f <$> num) }
    _ -> Nothing
    where
    partial = parse text
    showVal num
        | ScoreT.Typed ScoreT.Untyped n <- num
        , ShowVal.isHexVal (_val partial) = ShowVal.showHexVal n
        | otherwise = ShowVal.showVal num

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
data Partial = Partial
    { _transform :: [[Text]]
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
parse = parseGeneral (\method val args -> (method, val, args))

parseGeneral :: (Text -> Text -> [Text] -> (Text, Text, [Text]))
    -> Text -> Partial
parseGeneral splitExpr = make . Lists.unsnoc . Parse.splitPipeline
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
        (expr2, comment) = case Lists.unsnoc expr of
            Just (expr, comment) | "--" `Text.isPrefixOf` comment ->
                (expr, comment)
            _ -> (expr, "")
        (method, val, args) = case expr2 of
            method : val : args -> splitExpr method val args
            [arg]
                | " " `Text.isSuffixOf` arg -> (arg, "", [])
                | otherwise -> ("", arg, [])
            [] -> ("", "", [])

unparseGeneral :: (Text -> Text -> [Text] -> [Text]) -> Partial -> Text
unparseGeneral joinExpr (Partial transform method val args comment) =
    Parse.joinPipeline $
        transform ++ [Lists.mapInit (<>" ") (expr ++ comments)]
    where
    comments = if Text.null comment then [] else [comment]
    expr = joinExpr method val args

unparse :: Partial -> Text
unparse = unparseGeneral joinExpr
    where
    joinExpr method val args
        | Text.null method && Text.null val = args
        | Text.null method = val : args
        | otherwise = [method, Text.unwords (val : args)]
