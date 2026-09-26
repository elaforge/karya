-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{- | Cmds to edit a pitch track, which is a special kind of control track.

    This module creates the pitches that are later parsed by Derive.Control.
-}
module Cmd.PitchTrack (
    cmdValEdit, cmdMethodEdit
    , valEditAt, methodEditAt
    , cmdRecordNoteStatus
    -- * edits
    , transposeSelection
    , transpose
    , cycleEnharmonics
    , pitches
    , pitchTracks
#ifdef TESTING
    , module Cmd.PitchTrack
#endif
) where
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.ControlTrack as ControlTrack
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.InputNote as InputNote
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection

import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.Expr as Expr
import qualified Derive.Parse as Parse
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Scale as Scale
import qualified Derive.Symbols as Symbols

import qualified Perform.Pitch as Pitch
import qualified Ui.Key as Key

import           Global


-- * entry

-- | Val edit turns 'Msg.InputNote's into the appropriate scale degree call
-- for the scale in scope.
--
-- Like control tracks, @'@ will add a @'@ call, which repeats the last value.
-- This is useful to extend a constant pitch value to the desired breakpoint.
cmdValEdit :: Cmd.M m => Msg.Msg -> m Cmd.Status
cmdValEdit msg = Cmd.suppressHistory Cmd.ValEdit "pitch track val edit" $ do
    EditUtil.fallthrough EditUtil.NoBackspace msg
    case msg of
        Msg.InputNote (InputNote.NoteOn _ input _) -> do
            pos <- EditUtil.getPos
            note <- EditUtil.inputToNote input
            valEditAt pos note
            whenM (Cmd.gets (Cmd.stateAdvance . Cmd.stateEdit))
                Selection.advance
        Msg.InputNote (InputNote.PitchChange _ input) -> do
            pos <- EditUtil.getPos
            note <- EditUtil.inputToNote input
            valEditAt pos note
        (Msg.keyDown -> Just (Key.Char '\'')) -> EditUtil.softInsert "'"
        _ -> Cmd.abort
    pure Cmd.Done

-- | Method edit directs keystrokes to the (optional) call around the pitch
-- call.  Pitches by themselves simply set a constant pitch by default, but
-- a call can create an interpolated curve, or a trill, or anything really.
cmdMethodEdit :: Cmd.M m => Msg.Msg -> m Cmd.Status
cmdMethodEdit msg = Cmd.suppressHistory Cmd.MethodEdit
        "pitch track method edit" $ do
    EditUtil.fallthrough EditUtil.WantBackspace msg
    key <- Cmd.abortUnless $ EditUtil.methodKey msg
    pos <- EditUtil.getPos
    methodEditAt pos key
    pure Cmd.Done

valEditAt :: Cmd.M m => EditUtil.Pos -> Pitch.Note -> m ()
valEditAt pos note = modifyEventAt pos $ \partial ->
    (Just $ partial { ControlTrack._val = Pitch.noteText note }, False)

methodEditAt :: Cmd.M m => EditUtil.Pos -> EditUtil.Key -> m ()
methodEditAt pos key = modifyEventAt pos $ \partial ->
    ( Just $ partial
        { ControlTrack._method = fromMaybe "" $
            EditUtil.modifyTextKey [] key (ControlTrack._method partial)
        }
    , False
    )

-- | Record the last note entered.
cmdRecordNoteStatus :: Cmd.M m => Msg.Msg -> m Cmd.Status
cmdRecordNoteStatus msg = do
    case msg of
        Msg.InputNote (InputNote.NoteOn _ input _) -> do
            note <- EditUtil.inputToNote input
            Cmd.setStatus Config.statusNote $ Just $ Pitch.noteText note
        _ -> pure ()
    pure Cmd.Continue

-- * implementation

-- | old -> (new, advance?)
type Modify = ControlTrack.Partial -> (Maybe ControlTrack.Partial, Bool)

modifyEventAt :: Cmd.M m => EditUtil.Pos -> Modify -> m ()
modifyEventAt pos f = EditUtil.modifyEventAt pos True True
    (first (fmap unparse) . f . parse . fromMaybe "")

parse :: Text -> ControlTrack.Partial
parse = ControlTrack.parseGeneral splitExpr
    where
    splitExpr method val args
        -- Uses parens to disambiguate between call and val vs. val
        -- with args.
        | "(" `Text.isPrefixOf` val = (method, val, args)
        | otherwise = ("", method, val : args)

-- | Since pitches are calls, they need to lose or gain parens when they move
-- to or from toplevel call position.  This is one reason parse and unparse
-- are not exact inverses.
unparse :: ControlTrack.Partial -> Text
unparse = ControlTrack.unparseGeneral joinExpr
    where
    joinExpr method val args
        | Text.null method && Text.null val = args
        -- If the method is gone, the note no longer needs its parens.
        | Text.null method = stripParens val : args
        | otherwise = [method, addParens (Text.unwords (val : args))]
    stripParens t
        | "(" `Text.isPrefixOf` t && ")" `Text.isSuffixOf` t =
            Text.drop 1 $ Text.take (Text.length t - 1) t
        | otherwise = t
    -- If there's a method and it doesn't already have parens, it'll need them.
    addParens val
        | Text.null val || "(" `Text.isPrefixOf` val = val
        | otherwise = "(" <> val <> ")"

-- | Try to figure out where the pitch call part is in event text and modify
-- that with the given function.  The function can signal failure by returning
-- Left.
--
-- This is a bit of a heuristic because by design a pitch is a normal call and
-- there's no syntactic way to tell where the pitches are in an expression.  If
-- the text is a call with a val call as its first argument, that's considered
-- the pitch call.  Otherwise, if the text is just a call, that's the pitch
-- call.  Otherwise the text is unchanged.
--
-- This works with the convention that pitch calls take the \"base\" pitch as
-- their first argument.
modifyNote :: (Pitch.Note -> Either Text Pitch.Note) -> Text
    -> Either Text Text
modifyNote f = modifyExpr $ \noteStr -> case Text.uncons noteStr of
    Just ('(', rest) ->
        let (note, post) = Text.break (`elem` (" )" :: [Char])) rest
        in Text.cons '(' . (<>post) . Pitch.noteText <$> f (Pitch.Note note)
    _ -> Pitch.noteText <$> f (Pitch.Note noteStr)

-- | Modify the note expression, e.g. in @i (a b c)@ it would be @(a b c)@,
-- including the parens.
modifyExpr :: (Text -> Either Text Text) -> Text -> Either Text Text
modifyExpr f text = case Parse.parseExpr text of
    Left _ -> Right text
    Right expr -> case expr of
        Expr.Call sym (Expr.ValCall _ : _) :| []
            | sym /= Symbols.equal ->
                let (pre, within) = Text.break (=='(') text
                    (note, post) = break1 (==')') within
                in (\n -> pre <> n <> post) <$> f note
        Expr.Call sym _ :| []
            | sym /= Symbols.equal ->
                let (pre, post) = Text.break (==' ') text
                in (<>post) <$> f pre
        _ -> Right text

    where
    break1 f t = case Text.findIndex f t of
        Just i -> Text.splitAt (i+1) t
        Nothing -> (t, "")


-- * edits

-- | Function that modifies the pitch of an event on a pitch track, or a Left
-- if the operation failed.
type ModifyPitch =
    Scale.Scale -> Env.Environ -> Pitch.Note -> Either Text Pitch.Note

transposeSelection :: Cmd.M m => Scale.Transposition -> Pitch.Octave
    -> Pitch.Step -> m ()
transposeSelection transposition oct steps =
    pitches $ transpose transposition oct steps

transpose :: Scale.Transposition -> Pitch.Octave -> Pitch.Step -> ModifyPitch
transpose transposition octaves steps = \scale env note ->
    case Scale.transpose transposition scale env octaves steps note of
        -- Leave non-pitches alone.
        Left DeriveT.UnparseableNote {} -> Right note
        Left err -> Left (pretty err)
        Right note2 -> Right note2

cycleEnharmonics :: ModifyPitch
cycleEnharmonics scale env note = first pretty $ do
    enharmonics <- Scale.scaleEnharmonics scale env note
    pure $ fromMaybe note (Lists.head enharmonics)

pitches :: Cmd.M m => ModifyPitch -> m ()
pitches = ModifyEvents.selection . pitchTracks

-- | Apply a ModifyPitch to only pitch tracks.
pitchTracks :: Cmd.M m => ModifyPitch -> ModifyEvents.Track m
pitchTracks f = ModifyEvents.tracksNamed ParseTitle.isPitchTrack $
        \blockId trackId events -> do
    scale <- Perf.getScale (blockId, Just trackId)
    env <- Perf.getEnviron (blockId, Just trackId)
    let modify = modifyNote (f scale env)
    ModifyEvents.failableText modify blockId trackId events
