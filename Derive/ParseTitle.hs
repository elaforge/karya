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
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Util.Pretty as Pretty
import qualified Derive.Controls as Controls
import qualified Derive.Parse as Parse
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import Global


-- * blocks

-- | A block title is a normal expression, applied as a transform.
parse_block :: Text -> Either Text TrackLang.Expr
parse_block = Parse.parse_expr

-- * tracks

data Type = TempoTrack | ControlTrack | PitchTrack | NoteTrack
    deriving (Eq, Show)

instance Pretty.Pretty Type where pretty = showt

track_type :: Text -> Type
track_type title
    | is_note_track title = NoteTrack
    | is_pitch_track title = PitchTrack
    | is_tempo_track title = TempoTrack
    | otherwise = ControlTrack

-- ** control

data ControlType =
    -- | Control track with an optional combining operator.
    Control (Maybe TrackLang.CallId) (Score.Typed Score.Control)
    -- | Pitch track that sets a ScaleId (unless it's 'Pitch.empty_scale'),
    -- and sets the given pitch signal.
    | Pitch (Maybe TrackLang.CallId) Pitch.ScaleId Score.PControl
    -- | Tempo track with an optional modifying symbol.
    | Tempo (Maybe TrackLang.Symbol)
    deriving (Show)

instance Pretty.Pretty ControlType where
    pretty = unparse_control

parse_control :: Text -> Either Text ControlType
parse_control = fmap fst . parse_control_expr

parse_control_expr :: Text -> Either Text (ControlType, [TrackLang.Call])
parse_control_expr title = do
    (vals, expr) <- Parse.parse_control_title title
    ctrack <- parse_control_vals vals
    return (ctrack, expr)

-- TODO this parsing is all very ad-hoc, and there's no guarantee that it won't
-- overlap, or that 'unparse_control_vals' is really an overlap.
parse_control_vals :: [TrackLang.Val] -> Either Text ControlType
parse_control_vals vals = case vals of
    --  *twelve -> default pitch track in twelve
    [scale -> Just scale_id] ->
        Right $ Pitch Nothing scale_id Score.default_pitch
    --  *twelve merge
    [scale -> Just scale_id, TrackLang.VSymbol merge] ->
        Right $ Pitch (Just merge) scale_id Score.default_pitch
    --  *twelve # -> default pitch track in twelve
    --  *twelve #name -> named pitch track
    [scale -> Just scale_id, pitch_control_of -> Just cont] ->
        Right $ Pitch Nothing scale_id cont
    --  *twelve #name merge
    [scale -> Just scale_id, pitch_control_of -> Just cont,
            TrackLang.VSymbol merge] ->
        Right $ Pitch (Just merge) scale_id cont
    -- "tempo"
    [TrackLang.VSymbol (TrackLang.Symbol "tempo")] -> Right $ Tempo Nothing
    [TrackLang.VSymbol (TrackLang.Symbol "tempo"), TrackLang.VSymbol sym] ->
        Right $ Tempo (Just sym)
    -- control
    --
    -- It would be more regular to require \"%control\" and \"add %control\"
    -- for control tracks, but it looks nicer without the extra noise.
    [TrackLang.VSymbol control] ->
        Control Nothing <$> parse_control_type control
    -- add control -> relative control
    [TrackLang.VSymbol merge, TrackLang.VSymbol control] ->
        Control (Just merge) <$> parse_control_type control
    -- % -> default control
    -- It might be more regular to allow anything after %, but I'm a fan of
    -- only one way to do it, so only allow it for "".
    --
    -- The empty scale * defaults to the current scale id, because there's no
    -- real meaning to an empty scale id.  However, Controls.null is a valid
    -- control like any other and is simply used as a special name by the
    -- control block call hack in "Derive.Call.Block".
    [TrackLang.VControlRef (TrackLang.LiteralControl control)]
        | control == Controls.null ->
            Right $ Control Nothing (Score.untyped Controls.null)
    -- add % -> relative default control
    [TrackLang.VSymbol merge, TrackLang.VControlRef
            (TrackLang.LiteralControl control)] | control == Controls.null ->
        Right $ Control (Just merge) (Score.untyped Controls.null)
    _ -> Left $ "control track must be one of [\"tempo\", control,\
        \ op control, %, op %, *scale, *scale #name, op #, op #name],\
        \ got: " <> Text.unwords (map TrackLang.show_val vals)
    where
    scale (TrackLang.VSymbol (TrackLang.Symbol sym)) =
        case Text.uncons sym of
            Just ('*', scale_id) -> Just (Pitch.ScaleId scale_id)
            _ -> Nothing
    scale _ = Nothing

    pitch_control_of :: TrackLang.Val -> Maybe Score.PControl
    pitch_control_of (TrackLang.VPControlRef (TrackLang.LiteralControl c)) =
        Just c
    pitch_control_of _ = Nothing

parse_control_type :: TrackLang.Symbol
    -> Either Text (Score.Typed Score.Control)
parse_control_type (TrackLang.Symbol name) = case Text.uncons post of
    Just (':', t) -> do
        control <- Score.control pre
        typ <- maybe (Left $ "unknown type on control track: " <> showt t)
            Right $ Score.code_to_type (untxt t)
        return $ Score.Typed typ control
    _ -> Score.untyped <$> Score.control name
    where (pre, post) = Text.break (==':') name

unparse_typed :: Score.Typed Score.Control -> Text
unparse_typed (Score.Typed typ c) =
    Score.control_name c <> if Text.null code then "" else ":" <> code
    where code = Score.type_to_code typ

unparse_control_expr :: ControlType -> [TrackLang.Call] -> Text
unparse_control_expr ctype calls
    | Text.null call_expr = unparse_control ctype
    | otherwise = unparse_control ctype <> " | " <> call_expr
    where call_expr = maybe "" ShowVal.show_val $ NonEmpty.nonEmpty calls

unparse_control :: ControlType -> Text
unparse_control = Text.unwords . map TrackLang.show_val . unparse_control_vals

unparse_control_vals :: ControlType -> [TrackLang.Val]
unparse_control_vals ctype = case ctype of
    Control merge control -> maybe_sym merge ++ [control_val control]
    Pitch merge (Pitch.ScaleId scale_id) pcontrol -> concat
        [ [TrackLang.VSymbol (TrackLang.Symbol (Text.cons '*' scale_id))]
        , if pcontrol == Score.default_pitch then []
            else [TrackLang.VPControlRef $ TrackLang.LiteralControl pcontrol]
        , maybe_sym merge
        ]
    Tempo sym -> TrackLang.VSymbol "tempo" : maybe_sym sym
    where
    maybe_sym = maybe [] ((:[]) . TrackLang.VSymbol)
    control_val c
        | c == Score.untyped Controls.null = empty_control
        | otherwise = TrackLang.VSymbol $ TrackLang.Symbol (unparse_typed c)
    empty_control = TrackLang.VControlRef $
        TrackLang.LiteralControl Controls.null

-- | Convert a track title to its control.
title_to_control :: Text -> Maybe Score.Control
title_to_control title = case parse_control title of
    Right (Control _ cont) -> Just (Score.typed_val cont)
    _ -> Nothing

control_to_title :: Score.Control -> Text
control_to_title = Score.control_name

-- | A pitch track is also considered a control track.
is_control_track :: Text -> Bool
is_control_track = not . is_note_track

-- | This is like 'is_control_track' but doesn't include pitch tracks.
is_signal_track :: Text -> Bool
is_signal_track title = is_control_track title && case parse_control title of
    Right (Control {}) -> True
    _ -> False

is_tempo_track :: Text -> Bool
is_tempo_track title = case parse_control title of
    Right (Tempo {}) -> True
    _ -> False

-- ** note

-- | Parse a note track like @>inst@ as @note-track >inst@.  Other than
-- this, note track titles are normal expressions.
parse_note :: Text -> Either Text TrackLang.Expr
parse_note = Parse.parse_expr . (prefix <>)
    where prefix = TrackLang.unsym note_track_symbol <> " "

unparse_note :: TrackLang.Expr -> Text
unparse_note = strip . ShowVal.show_val
    where
    strip t = fromMaybe t $ Text.stripPrefix prefix t
    prefix = TrackLang.unsym note_track_symbol <> " "

note_track_symbol :: TrackLang.Symbol
note_track_symbol = "note-track"

-- | Convert a track title into its instrument.
--
-- This is a hack because the track title is actually code and I'm trying to
-- pick an instrument out without executing it.
title_to_instrument :: Text -> Maybe Score.Instrument
title_to_instrument title = case Text.uncons title of
    Just ('>', name) -> Just $ Score.Instrument $ strip_expr name
    _ -> Nothing

-- | Convert from an instrument to the title of its instrument track.
instrument_to_title :: Score.Instrument -> Text
instrument_to_title = TrackLang.show_val

is_note_track :: Text -> Bool
is_note_track = Maybe.isJust . title_to_instrument

strip_expr :: Text -> Text
strip_expr = Text.stripEnd . Text.takeWhile (/='|')

-- ** pitch

title_to_scale :: Text -> Maybe Pitch.ScaleId
title_to_scale title = case parse_control title of
    Right (Pitch _ scale_id _) -> Just scale_id
    _ -> Nothing

scale_to_title :: Pitch.ScaleId -> Text
scale_to_title scale_id =
    unparse_control (Pitch Nothing scale_id Score.default_pitch)

is_pitch_track :: Text -> Bool
is_pitch_track = ("*" `Text.isPrefixOf`)
    -- Previously it was 'Maybe.isJust . title_to_scale', but this is called
    -- a lot during slicing so efficiency matters.
