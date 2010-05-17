{-# LANGUAGE PatternGuards #-}
-- | Utilities to deal with track titles.
--
-- Note track titles are just tracklang expressions, so no extra code is
-- needed.  Control tracks titles are rather more complicated, and Cmds in
-- addition to the Schema need to agree on how they are parsed.
--
-- TODO The name is no longer very accurate.
module Derive.Schema.Default where
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Tree as Tree
import qualified Text.Printf as Printf
import qualified Util.Seq as Seq
import qualified Util.Tree
import qualified Util.Pretty as Pretty

import Ui
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang
import qualified Perform.Pitch as Pitch
import qualified Perform.Midi.Instrument as Instrument
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Info as Info


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
    (expr, vals) <- TrackLang.parse_control_track title
    ctrack <- parse_control_vals vals
    return (ctrack, expr)

parse_control_vals :: [TrackLang.Val] -> Either String ControlType
parse_control_vals vals = case vals of
        [TrackLang.VSymbol (TrackLang.Symbol "tempo")] -> Right Tempo
        [TrackLang.VSymbol control] ->
            Right $ Control Nothing (control_of control)
        [TrackLang.VSymbol call, TrackLang.VSymbol control] ->
            Right $ Control (Just call) (control_of control)
        [TrackLang.VNote note] ->
            Right $ Pitch (PitchAbsolute (scale_of note)) Nothing
        [TrackLang.VNote note, TrackLang.VSymbol control] ->
            Right $ Pitch (PitchAbsolute (scale_of note))
                (Just (control_of control))
        TrackLang.VSymbol call : TrackLang.VNote note : rest
            | not (null (Pitch.note_text note)) ->
                Left "relative pitch track can't have a scale"
            | [] <- rest ->
                Right $ Pitch (PitchRelative call) Nothing
            | [TrackLang.VSymbol control] <- rest ->
                Right $ Pitch (PitchRelative call) (Just (control_of control))
        _ -> Left $ "args must be one of [\"tempo\", control, op control, "
            ++ "*scale, *scale pitch_control, op *, op * pitch_control]"
    where
    scale_of (Pitch.Note "") = Nothing
    scale_of (Pitch.Note text) = Just (Pitch.ScaleId text)
    control_of (TrackLang.Symbol control) = Score.Control control

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
                PitchRelative call -> [TrackLang.VSymbol call, note ""] ++ pname
                PitchAbsolute (Just (Pitch.ScaleId scale_id)) ->
                    [note scale_id] ++ pname
                PitchAbsolute Nothing -> [note ""] ++ pname
        Tempo -> [sym "tempo"]
    where
    sym = TrackLang.VSymbol . TrackLang.Symbol
    note = TrackLang.VNote . Pitch.Note

-- ** util

scale_to_title :: Pitch.ScaleId -> String
scale_to_title scale_id =
    unparse_control (Pitch (PitchAbsolute (Just scale_id)) Nothing)

title_to_scale :: String -> Maybe Pitch.ScaleId
title_to_scale title = case parse_control title of
    Right (Pitch (PitchAbsolute (Just scale_id)) _) -> Just scale_id
    _ -> Nothing

-- | Convert from an instrument to the title of its instrument track.
instrument_to_title :: Score.Instrument -> String
instrument_to_title = Pretty.pretty . TrackLang.VInstrument

title_is_relative :: String -> Bool
title_is_relative = either (const False) is_relative . parse_control

is_relative :: ControlType -> Bool
is_relative (Control (Just _) _) = True
is_relative (Pitch (PitchRelative _) _) = True
is_relative _ = False

-- | Note tracks are defined as tracks without children.  But if I'm trying to
-- figure out a skeleton in the first place I need to guess which one is the
-- note track.
looks_like_note_track :: String -> Bool
looks_like_note_track ('>':_) = True
looks_like_note_track _ = False

is_tempo_track :: String -> Bool
is_tempo_track = (=="tempo")

-- * set_inst_status

-- | Stick some handy info about the current instrument into the status.
--
-- This should be run whenever the track focus changes, or tracks are expanded
-- or collapsed.
--
-- This assumes the current schema is the default schema.  TODO look this up in
-- the schema.
set_inst_status :: (Monad m) => BlockId -> TrackNum -> Cmd.CmdT m ()
set_inst_status block_id tracknum = do
    ttree <- State.get_track_tree block_id
    -- This may be run loading a new state when there is no focused block, so
    -- be careful to not abort the cmd.
    maybe_block_id <- Cmd.lookup_focused_block
    case maybe_block_id of
        Just block_id -> do
            status <- get_track_status block_id ttree tracknum
            Cmd.set_global_status "inst" status
        Nothing -> return ()

-- | Looks like:
-- title (tracknum): inst_name, allocation, [control tracks]
-- fm8/inst1 at 1: fm8:0,1,2, [vel {collapse 2}, pedal {expand 3}]
get_track_status :: (Monad m) => BlockId -> State.TrackTree -> TrackNum
    -> Cmd.CmdT m String
get_track_status block_id ttree tracknum = case note_track_of ttree tracknum of
    Just (inst, note_tracknum) -> do
        let controls = control_tracks_of ttree note_tracknum
        track_descs <- show_track_status block_id controls
        midi_config <- State.get_midi_config
        let addrs = Map.findWithDefault [] inst
                (Instrument.config_alloc midi_config)
        let title = instrument_to_title inst
        return $ Printf.printf "%s at %d: %s, [%s]" title note_tracknum
            (Info.show_addrs addrs) (Seq.join ", " track_descs)
    Nothing -> return $ "track " ++ show tracknum ++ ": no inst"


find_track :: TrackNum -> State.TrackTree -> Maybe (Tree.Tree State.TrackInfo)
find_track tracknum = Util.Tree.find ((==tracknum) . State.track_tracknum)

control_tracks_of :: State.TrackTree -> TrackNum -> [State.TrackInfo]
control_tracks_of ttree tracknum = case paths_of ttree tracknum of
        Nothing -> []
        Just (_, parents, _) -> controls parents
    where
    controls = filter (is_control . State.track_title)
    is_control title = not (is_tempo_track title)
    -- TODO a "control of" should be defined as the "single parents", every
    -- parent that has only one child

-- | Given a tracknum, find the note track below it.  Since there may
-- be multiple ones, pick the first one.
--
-- Search down the children for a instrument title, then get the first note
-- track (tree bottom) underneath that.
--
-- This assumes the instrument is specified in a track below, not inherited
-- from the environment.  Inheriting the instrument seems hard to do, but may
-- be possible.  Maybe I can sove this problem later.
note_track_of :: State.TrackTree -> TrackNum
    -> Maybe (Score.Instrument, TrackNum)
note_track_of ttree tracknum = case find_track tracknum ttree of
        Nothing -> Nothing
        Just tree -> find_inst tree
    where
    find_inst tree@(Tree.Node track children) = case inst_of track of
        Nothing -> msum (map find_inst children)
        Just inst -> Just
            (inst, State.track_tracknum (Util.Tree.first_leaf tree))
    inst_of = title_to_instrument . State.track_title

-- | Convert a track title into its instrument.
--
-- This is a hack because the track title is actually code and I'm trying to
-- pick an instrument out without executing it.
--
-- TODO this will break on anything complicated like @>inst | xyz@.
title_to_instrument :: String -> Maybe Score.Instrument
title_to_instrument ('>':name) = Just (Score.Instrument name)
title_to_instrument _ = Nothing

-- | Looks like: [vel {collapse 2}, pedal {expand 3}]
show_track_status :: (State.UiStateMonad m) => BlockId -> [State.TrackInfo]
    -> m [String]
show_track_status block_id status = forM status $ \info -> do
    let title = State.track_title info
        tracknum = State.track_tracknum info
    btrack <- State.block_track_at block_id tracknum
    let cmd_text = case fmap Block.track_flags btrack of
            Nothing -> "?"
            Just flags
                | Block.Collapse `elem` flags -> "expand"
                | otherwise -> "collapse"
    return $ Printf.printf "%s {%s %d}" (Info.str title) cmd_text tracknum

paths_of :: State.TrackTree -> TrackNum
    -> Maybe (State.TrackInfo, [State.TrackInfo], [State.TrackInfo])
paths_of track_tree tracknum =
    List.find ((==tracknum) . State.track_tracknum . (\(a, _, _) -> a))
        (Util.Tree.paths track_tree)
