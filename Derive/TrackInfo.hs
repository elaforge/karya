{-# LANGUAGE ViewPatterns #-}
-- | Utilities to deal with track titles.
--
-- This module is used by both Cmd and Derive since Cmd also wants to know
-- track types for track specific cmds.
--
-- Note track titles are just tracklang expressions, so no extra code is
-- needed.  Control tracks titles are just a hardcoded list of special cases,
-- though they are parsed as tracklang Vals.
module Derive.TrackInfo where
import qualified Data.ByteString.Char8 as B
import qualified Data.Maybe as Maybe

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Derive.ParseBs as Parse
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


-- * track info

data ControlType =
    Control (Maybe TrackLang.CallId) Score.Control
    -- | Control is Nothing for default scale.
    | Pitch Pitch.ScaleId (Maybe Score.Control)
    | Tempo
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
        [TrackLang.VScaleId scale_id] ->
            Right $ Pitch scale_id Nothing
        --  *twelve # -> default pitch track in twelve
        --  *twelve #name -> named pitch track
        [TrackLang.VScaleId scale_id, pitch_control -> Just cont] ->
            Right $ Pitch scale_id cont
        -- "tempo"
        [TrackLang.VSymbol (TrackLang.Symbol "tempo")] -> Right Tempo
        -- control
        [TrackLang.VSymbol control] ->
            Right $ Control Nothing (control_of control)
        -- add control -> relative control
        [TrackLang.VSymbol call, TrackLang.VSymbol control] ->
            Right $ Control (Just call) (control_of control)
        -- % -> default control
        -- It might be more regular to allow anything after %, but I'm a fan
        -- of only one way to do it, so only allow it for "".
        --
        -- The empty scale * defaults to the current scale id, because there's
        -- no real meaning to an empty scale id.  However, Score.c_null is
        -- a valid control like any other and is simply used as a special
        -- name by the control block call hack in "Derive.Call.Block".
        [TrackLang.VControl (TrackLang.LiteralControl (Score.Control ""))] ->
            Right $ Control Nothing Score.c_null
        -- add % -> relative default control
        [TrackLang.VSymbol call, TrackLang.VControl
                (TrackLang.LiteralControl (Score.Control ""))] ->
            Right $ Control (Just call) Score.c_null
        _ -> Left $ "control track must be one of [\"tempo\", control, "
            ++ "op control, %, op %, *scale, *scale #name, op #, op #name], "
            ++ "got: " ++ Pretty.pretty vals
    where
    control_of (TrackLang.Symbol control) = Score.Control control

    pitch_control :: TrackLang.Val -> Maybe (Maybe Score.Control)
    pitch_control (TrackLang.VPitchControl
            (TrackLang.LiteralControl cont@(Score.Control name)))
        | null name = Just Nothing
        | otherwise = Just (Just cont)
    pitch_control _ = Nothing

unparse_control :: ControlType -> String
unparse_control = Seq.join " " . map Pretty.pretty . unparse_control_vals

unparse_control_vals :: ControlType -> [TrackLang.Val]
unparse_control_vals ctype = case ctype of
        Control call control ->
            let cont = sym (if control == Score.c_null then "%"
                    else uncontrol control)
            in case call of
                Nothing -> [cont]
                Just op -> [TrackLang.VSymbol op, cont]
        Pitch (Pitch.ScaleId scale_id) name -> sym ('*':scale_id)
            : maybe [] ((:[]) . sym . ('#':) . uncontrol) name
        Tempo -> [sym "tempo"]
    where
    sym = TrackLang.VSymbol . TrackLang.Symbol
    uncontrol (Score.Control c) = c

-- ** util

strip_expr :: String -> String
strip_expr = Seq.rstrip . takeWhile (/='|')

scale_to_title :: Pitch.ScaleId -> String
scale_to_title scale_id = unparse_control (Pitch scale_id Nothing)

title_to_scale :: String -> Maybe Pitch.ScaleId
title_to_scale title = case parse_control title of
    Right (Pitch scale_id _) -> Just scale_id
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

is_note_track :: String -> Bool
is_note_track = Maybe.isJust . title_to_instrument

is_control_track :: String -> Bool
is_control_track = not . is_note_track

is_tempo_track :: String -> Bool
is_tempo_track = (=="tempo")

-- | Technically a pitch track is also a control track.  This is only true
-- for a non-pitch control track.
is_signal_track :: String -> Bool
is_signal_track title = is_control_track title && case parse_control title of
    Right (Control {}) -> True
    _ -> False
