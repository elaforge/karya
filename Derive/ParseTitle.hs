-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ViewPatterns #-}
-- | Utilities to deal with block and track titles.
--
-- This module is used by both Cmd and Derive since Cmd also wants to know
-- track types for track specific cmds.
--
-- Note track titles are just tracklang expressions, so no extra code is
-- needed.  Control tracks titles are just a hardcoded list of special cases,
-- though they are parsed as tracklang Vals.
module Derive.ParseTitle where
import qualified Data.Attoparsec.Text as A
import Data.Attoparsec.Text ((<?>))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

import qualified Util.ParseText as ParseText
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Expr as Expr
import qualified Derive.Parse as Parse
import Derive.Parse (lexeme)
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Symbols as Symbols

import qualified Perform.Pitch as Pitch
import Global


-- * blocks

-- | A block title is a normal expression, applied as a transform.
parse_block :: Text -> Either Text BaseTypes.Expr
parse_block = Parse.parse_expr

-- * tracks

data Type = TempoTrack | ControlTrack | PitchTrack | NoteTrack
    deriving (Eq, Show)

instance Pretty Type where pretty = showt

track_type :: Text -> Type
track_type title
    | is_note_track title = NoteTrack
    | is_pitch_track title = PitchTrack
    | is_tempo_track title = TempoTrack
    | otherwise = ControlTrack

-- ** note track

parse_note_track :: Text -> Either Text (Score.Instrument, Maybe TrackCall)
parse_note_track = ParseText.parse p_note_track

-- > >inst !track-call
p_note_track :: A.Parser (Score.Instrument, Maybe TrackCall)
p_note_track = (,)
    <$> (A.char '>' *> (Score.Instrument <$> Parse.p_identifier True ""))
    <*> ParseText.optional p_track_call

-- ** control track

-- | Parse the first part of the control track title.  This is special syntax,
-- and is not the usual call plus list of argument values.
parse_control_type :: Text -> Either Text ControlType
parse_control_type = fmap fst . parse_control_title

parse_control_title :: Text -> Either Text (ControlType, [BaseTypes.Call])
parse_control_title = ParseText.parse p_control_title

data ControlType =
    -- | Tempo track with an optional modifying symbol.
    Tempo (Maybe Expr.Symbol)
    -- | Pitch track that sets a ScaleId (unless it's 'Pitch.empty_scale'),
    -- and sets the given pitch signal.
    | Pitch Pitch.ScaleId (Either TrackCall Score.PControl) (Maybe Merge)
    -- | Control track with an optional combining operator.
    | Control (Either TrackCall (Score.Typed Score.Control)) (Maybe Merge)
    deriving (Eq, Show)

type Merge = Expr.Symbol
type TrackCall = Expr.Symbol

instance ShowVal.ShowVal ControlType where
    show_val = control_type_to_title

p_control_title :: A.Parser (ControlType, [BaseTypes.Call])
p_control_title = do
    ctype <- p_control_type
    expr <- A.option [] (Parse.p_pipe >> NonEmpty.toList <$> Parse.p_expr True)
    return (ctype, expr)

p_control_type :: A.Parser ControlType
p_control_type = p_tempo <|> p_pitch <|> p_control

p_tempo :: A.Parser ControlType
p_tempo = Tempo <$>
    (lexeme (A.string "tempo") *> ParseText.optional (lexeme p_merge))

-- | *twelve (#name | !track-call) merge
p_pitch :: A.Parser ControlType
p_pitch = Pitch
    <$> lexeme p_scale_id
    <*> (lexeme $ (Left <$> p_track_call)
        <|> (Right <$> A.option Score.default_pitch (lexeme Parse.p_pcontrol)))
    <*> ParseText.optional (lexeme p_merge)

-- | (!track-call | % | control:typ) merge
p_control :: A.Parser ControlType
p_control = Control
    <$> (lexeme $ A.choice
        [ Left <$> p_track_call
        , A.char '%'
            *> pure (Right $ Score.untyped $ Score.unchecked_control "")
        , Right <$> p_typed_control
        ])
    <*> ParseText.optional (lexeme p_merge)

p_typed_control :: A.Parser (Score.Typed Score.Control)
p_typed_control = (flip Score.Typed)
    <$> (Score.unchecked_control <$> Parse.p_identifier False ":")
    <*> A.option Score.Untyped p_type_annotation

p_type_annotation :: A.Parser Score.Type
p_type_annotation = do
    A.char ':'
    typ <- Parse.p_identifier False ""
    case Score.code_to_type typ of
        Nothing -> fail $ "unknown type: " <> show typ
        Just typ -> return typ

-- ** unparse

control_type_to_title :: ControlType -> Text
control_type_to_title ctype = Text.unwords $ case ctype of
    Tempo sym -> "tempo" : maybe_sym sym
    Pitch (Pitch.ScaleId scale_id) pcontrol merge ->
        "*" <> scale_id
        : either ((:[]) . show_tcall) show_pcontrol pcontrol
        ++ show_merge merge
    Control c merge -> either show_tcall control_to_title c : show_merge merge
    where
    maybe_sym = maybe [] ((:[]) . Expr.unsym)
    show_merge = maybe [] ((:[]) . ShowVal.show_val)
    show_tcall = ("!"<>) . ShowVal.show_val
    show_pcontrol pcontrol = [ShowVal.show_val pcontrol | pcontrol /= ""]

-- | This is different from ShowVal (Typed Control) because the control doesn't
-- need a % in the control track title.
control_to_title :: Score.Typed Score.Control -> Text
control_to_title (Score.Typed typ c)
    | c == "" = "%"
    | otherwise = Score.control_name c
        <> if typ == Score.Untyped then ""
            else ":" <> Score.type_to_code typ

-- ** parse util

p_merge :: A.Parser Merge
p_merge = Expr.Symbol <$> Parse.p_identifier False ""

p_track_call :: A.Parser Expr.Symbol
p_track_call = A.char '!' *> Parse.p_symbol True

-- | This is special syntax that's only allowed in control track titles.
p_scale_id :: A.Parser Pitch.ScaleId
p_scale_id = do
    A.char '*'
    Pitch.ScaleId <$> A.option "" (Parse.p_identifier True "")
    <?> "scale id"

-- * util

-- | Convert a track title to its control.
title_to_control :: Text -> Maybe Score.Control
title_to_control title = case parse_control_type title of
    Right (Control (Right control) _) -> Just (Score.typed_val control)
    _ -> Nothing

-- | A pitch track is also considered a control track.
is_control_track :: Text -> Bool
is_control_track = not . is_note_track

-- | This is like 'is_control_track' but doesn't include pitch tracks.
is_signal_track :: Text -> Bool
is_signal_track title =
    is_control_track title && case parse_control_type title of
        Right (Control {}) -> True
        _ -> False

is_tempo_track :: Text -> Bool
is_tempo_track title = case parse_control_type title of
    Right (Tempo {}) -> True
    _ -> False

-- ** note

-- | Parse a note track like @>inst@ as @note-track inst@.  Other than
-- this, note track titles are normal expressions.
parse_note :: Text -> Either Text BaseTypes.Expr
parse_note title = case Text.uncons title of
    Just ('>', rest) -> Parse.parse_expr (prefix <> rest)
        where prefix = Expr.unsym Symbols.note_track <> " "
    _ -> Left $ "note track title should start with >: " <> showt title

unparse_note :: BaseTypes.Expr -> Text
unparse_note = strip . ShowVal.show_val
    where
    strip t = maybe t ((">"<>) . Text.stripStart) $
        Text.stripPrefix (Expr.unsym Symbols.note_track) t

-- | Convert a track title into its instrument.
title_to_instrument :: Text -> Maybe Score.Instrument
title_to_instrument title = case parse_note_track title of
    Right (inst, _) -> Just inst
    _ -> Nothing

-- | Convert from an instrument to the title of its instrument track.
instrument_to_title :: Score.Instrument -> Text
instrument_to_title (Score.Instrument a) = ">" <> a

is_note_track :: Text -> Bool
is_note_track = (">" `Text.isPrefixOf`)

strip_expr :: Text -> Text
strip_expr = Text.stripEnd . Text.takeWhile (/='|')

note_track :: Text
note_track = ">"

-- ** pitch

title_to_scale :: Text -> Maybe Pitch.ScaleId
title_to_scale title = case parse_control_type title of
    Right (Pitch scale_id _ _) -> Just scale_id
    _ -> Nothing

scale_to_title :: Pitch.ScaleId -> Text
scale_to_title scale_id =
    ShowVal.show_val (Pitch scale_id (Right Score.default_pitch) Nothing)

is_pitch_track :: Text -> Bool
is_pitch_track = ("*" `Text.isPrefixOf`)
    -- Previously it was 'Maybe.isJust . title_to_scale', but this is called
    -- a lot during slicing so efficiency matters.

pitch_track :: Text
pitch_track = "*"
