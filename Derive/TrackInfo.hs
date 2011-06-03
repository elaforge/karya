{-# LANGUAGE ViewPatterns #-}
-- | Utilities to deal with track titles.
--
-- Like Derive.Schema, this module is used by both Cmd and Derive.
--
-- Note track titles are just tracklang expressions, so no extra code is
-- needed.  Control tracks titles are rather more complicated, and Cmds in
-- addition to the Schema need to agree on how they are parsed.
module Derive.TrackInfo where
import qualified Data.ByteString.Char8 as B
import qualified Data.Maybe as Maybe

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Derive.ParseBs as Parse
import qualified Derive.Scale.Relative as Relative
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


-- * track info

data ControlType =
    -- | > control
    -- > + control
    Control (Maybe TrackLang.CallId) Score.Control
    -- | > *scale
    -- > *scale pitch_control
    -- > + *
    -- > + * pitch_control
    | Pitch PitchType (Maybe Score.Control)
    -- | > tempo
    | Tempo
    deriving (Show)

data PitchType =
    PitchRelative TrackLang.CallId | PitchAbsolute (Maybe Pitch.ScaleId)
    deriving (Show)

parse_control :: String -> Either String ControlType
parse_control = fmap fst . parse_control_expr

parse_control_expr :: String -> Either String (ControlType, TrackLang.Expr)
parse_control_expr title = do
    (vals, expr) <- Parse.parse_control_title (B.pack title)
    ctrack <- parse_control_vals vals
    return (ctrack, expr)

-- | It would be more regular to require \"%cont\" and \"add %cont\" for
-- control tracks, but it looks nicer without the extra noise.
parse_control_vals :: [TrackLang.Val] -> Either String ControlType
parse_control_vals vals = case vals of
        --  *twelve -> default pitch track in twelve
        [TrackLang.VScaleId scale] ->
            Right $ Pitch (PitchAbsolute (scale_of scale)) Nothing
        --  *twelve # -> default ptich track in twelve
        --  *twelve #name -> named pitch track
        [TrackLang.VScaleId scale, pitch_control -> Just cont] ->
            Right $ Pitch (PitchAbsolute (scale_of scale)) cont
        -- add # -> relative pitch for default
        -- add #name -> relative pitch for named track
        [TrackLang.VSymbol call, pitch_control -> Just cont] ->
            Right $ Pitch (PitchRelative call) cont
        -- "tempo"
        [TrackLang.VSymbol (TrackLang.Symbol "tempo")] -> Right Tempo
        -- control
        [TrackLang.VSymbol control] ->
            Right $ Control Nothing (control_of control)
        -- add control
        [TrackLang.VSymbol call, TrackLang.VSymbol control] ->
            Right $ Control (Just call) (control_of control)
        _ -> Left $ "control track must be one of [\"tempo\", control, "
            ++ "op control, " ++ "*scale, *scale #name, op #, op #name], "
            ++ "got: " ++ Pretty.pretty vals
    where
    scale_of (Pitch.ScaleId "") = Nothing
    scale_of scale = Just scale
    control_of (TrackLang.Symbol control) = Score.Control control

    pitch_control :: TrackLang.Val -> Maybe (Maybe Score.Control)
    pitch_control (TrackLang.VPitchControl
            (TrackLang.Control cont@(Score.Control name)))
        | null name = Just Nothing
        | otherwise = Just (Just cont)
    pitch_control _ = Nothing

unparse_control :: ControlType -> String
unparse_control = Seq.join " " . map Pretty.pretty . unparse_control_vals

unparse_control_vals :: ControlType -> [TrackLang.Val]
unparse_control_vals ctype = case ctype of
        Control call (Score.Control control) -> case call of
            Nothing -> [sym control]
            Just op -> [TrackLang.VSymbol op, sym control]
        Pitch ptype name ->
            let pname = maybe [] (\(Score.Control c) -> [sym c]) name
            in case ptype of
                PitchRelative call ->
                    [TrackLang.VSymbol call, sym "*"] ++ pname
                PitchAbsolute (Just (Pitch.ScaleId scale_id)) ->
                    sym ('*':scale_id) : pname
                PitchAbsolute Nothing -> sym "*" : pname
        Tempo -> [sym "tempo"]
    where
    sym = TrackLang.VSymbol . TrackLang.Symbol

-- ** util

strip_expr :: String -> String
strip_expr = Seq.rstrip . takeWhile (/='|')

scale_to_title :: Pitch.ScaleId -> String
scale_to_title scale_id =
    unparse_control (Pitch (PitchAbsolute (Just scale_id)) Nothing)

title_to_scale :: String -> Maybe Pitch.ScaleId
title_to_scale title = case parse_control title of
    Right (Pitch (PitchAbsolute (Just scale_id)) _) -> Just scale_id
    Right (Pitch (PitchRelative _) _) -> Just Relative.scale_id
    _ -> Nothing

-- | Convert a track title into its instrument.
--
-- This is a hack because the track title is actually code and I'm trying to
-- pick an instrument out without executing it.
title_to_instrument :: String -> Maybe Score.Instrument
title_to_instrument ('>':name) = Just (Score.Instrument (strip_expr name))
title_to_instrument _ = Nothing

-- | Convert from an instrument to the title of its instrument track.
instrument_to_title :: Score.Instrument -> String
instrument_to_title = Pretty.pretty . TrackLang.VInstrument

title_is_relative :: String -> Bool
title_is_relative = either (const False) is_relative . parse_control

is_relative :: ControlType -> Bool
is_relative (Control (Just _) _) = True
is_relative (Pitch (PitchRelative _) _) = True
is_relative _ = False

is_note_track :: String -> Bool
is_note_track = Maybe.isJust . title_to_instrument

is_control_track :: String -> Bool
is_control_track = not . is_note_track

is_tempo_track :: String -> Bool
is_tempo_track = (=="tempo")
