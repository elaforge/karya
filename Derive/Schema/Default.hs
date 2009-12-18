-- | Definitions for the default schema.
--
-- Most are in "Derive.Schema" but a few are broken out so others can import
-- them.  Technically this is incorrect because general commands shouldn't
-- depend on the schema, but 'set_inst_status' wants to do some
-- track parsing to set global status.  This will be a problem if I ever
-- have other schemas, but if I have those I can solve the problem then.
--
-- set_inst_status can't be in the schema because it wants to run when
-- non-schema cmds run, like set selection and track collapse.
--
-- I originally assumed I would have many different schemas, but now that
-- the skeleton is explicit and I have a possible notation to pipe a track
-- through a function, I'm not entirely sure if it's worth keeping the schema
-- concept.  Even if I do eliminate schemas, there is still a division between
-- general and track-specific commands, and the latter can't import the former,
-- to avoid circular imports.
module Derive.Schema.Default where
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Text.Printf as Printf
import qualified Util.Seq as Seq
import qualified Util.Tree

import Ui
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import qualified Perform.Midi.Instrument as Instrument
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Info as Info


paths_of :: State.TrackTree -> TrackNum
    -> Maybe (State.TrackInfo, [State.TrackInfo], [State.TrackInfo])
paths_of track_tree tracknum =
    List.find ((==tracknum) . State.track_tracknum . (\(a, _, _) -> a))
        (Util.Tree.paths track_tree)

-- | The type of a track is derived from its title.
is_tempo_track, is_pitch_track, is_note_track :: String -> Bool
is_tempo_track = (=="tempo")
is_pitch_track title = case parse_control_title title of
    (_, Right _) -> True
    _ -> False
is_note_track = (">" `List.isPrefixOf`)

-- | True if this track is a relative control or pitch track.
is_relative_track :: String -> Bool
is_relative_track title = case parse_control_title title of
    (Just _, _) -> True
    _ -> False

pitch_track_prefix = "*"

inst_of_track = Score.Instrument . Seq.strip . drop 1

-- | Convert a track title into its instrument.  This could be per-schema, but
-- I'm going to hardcode it for now and assume all schemas will do the same
-- thing.
title_to_instrument :: String -> Maybe Score.Instrument
title_to_instrument name
    | is_note_track name = Just $ inst_of_track name
    | otherwise = Nothing

-- | Convert from an instrument to the title of its instrument track.
instrument_to_title :: Score.Instrument -> String
instrument_to_title (Score.Instrument inst) = '>' : inst

title_to_scale :: String -> Maybe Pitch.ScaleId
title_to_scale title = either (const Nothing) Just
    (snd (parse_control_title title))

scale_to_title :: Pitch.ScaleId -> String
scale_to_title scale_id = unparse_control_title Nothing (Right scale_id)

-- | The fst element is Just ControlOp for a relative track.
parse_control_title :: String
    -> (Maybe String, Either Score.Control Pitch.ScaleId)
parse_control_title title
    | ',' `elem` title = (Just (Seq.strip pre), parse (drop 1 post))
    | otherwise = (Nothing, parse title)
    where
    (pre, post) = break (==',') title
    parse title =
        maybe (Left (Score.Control (Seq.strip title))) Right (to_pitch title)
    to_pitch title
        | pitch_track_prefix `List.isPrefixOf` s =
            Just (Pitch.ScaleId (drop 1 s))
        | otherwise = Nothing
        where s = Seq.strip title

unparse_control_title :: Maybe String -> Either Score.Control Pitch.ScaleId
    -> String
unparse_control_title maybe_op cont = case cont of
    Left (Score.Control s) -> pref s
    Right (Pitch.ScaleId s) -> pref (pitch_track_prefix ++ s)
    where pref = maybe id (\op t -> op ++ ", " ++ t) maybe_op


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

control_tracks_of :: State.TrackTree -> TrackNum -> [State.TrackInfo]
control_tracks_of ttree tracknum = case paths_of ttree tracknum of
        Nothing -> []
        Just (_, parents, _) -> controls parents
    where
    controls = filter (is_control . State.track_title)
    is_control title = not (is_tempo_track title || is_note_track title)

note_track_of :: State.TrackTree -> TrackNum
    -> Maybe (Score.Instrument, TrackNum)
note_track_of ttree tracknum = case paths_of ttree tracknum of
        Nothing -> Nothing
        Just (track, parents, children) ->
            find_inst (track : parents ++ children)
    where
    find_inst = msum . map inst_of
    inst_of info = case title_to_instrument (State.track_title info) of
        Nothing -> Nothing
        Just inst -> Just (inst, State.track_tracknum info)

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
