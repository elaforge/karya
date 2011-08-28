{- | Quick hack to print out the binary save files.

Later there should be an undump mode that converts text to the binary format.
-}
module App.Dump where
import qualified Data.Array.IArray as IArray
import qualified Data.Map as Map
import qualified System.Environment as Environment
import qualified System.Exit
import Text.Printf

import qualified Util.PPrint as PPrint
import qualified Util.Seq as Seq
import qualified Ui.Events as Events
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Serialize as Serialize
import qualified Cmd.Simple as Simple


default_abbrs = [abbreviate_tracks, abbreviate_rulers]

abbreviate_tracks = "track"
abbreviate_rulers = "ruler"

-- Haskell report says you're supposed to be able to put a newline between
-- the whacks, but now it suddenly doesn't work?
usage = "dump [ -complete ] save_file\n"
    ++ "\t-complete - Don't abbreviate the rulers and tracks"

main :: IO ()
main = do
    args <- Environment.getArgs
    (abbrs, fn) <- case args of
        ["-complete", fn] -> return ([], fn)
        [fn] -> return (default_abbrs, fn)
        _ -> fail_with usage
    either_state <- Serialize.unserialize fn
    Serialize.SaveState ui_state date <- case either_state of
        Left exc -> fail_with $ "Error reading " ++ show fn ++ ": " ++ show exc
        Right state -> return state
    printf "SaveState {\nsave_date = %s,\nui_state = \n" (show date)
    pprint_ui_state abbrs ui_state
    putStrLn "}"

fail_with msg = do
    putStrLn msg
    System.Exit.exitWith (System.Exit.ExitFailure 1)

pprint_ui_state abbr (State.State views blocks tracks rulers config) = do
    put_field "state_views" (PPrint.pshow views)
    put_field "state_blocks" (PPrint.pshow blocks)
    put_field "state_tracks" $ if abbreviate_tracks `elem` abbr
        then pshow_map (Map.map abbr_track tracks)
        else PPrint.pshow tracks
    put_field "state_ruler" $ if abbreviate_rulers `elem` abbr
        then pshow_map (Map.map abbr_ruler rulers)
        else PPrint.pshow (Map.map abbr_ruler rulers)
    pprint_config config

pprint_config (State.Config ns dir root midi defaults) = do
    put_field "namespace" ns
    put_field "project_dir" dir
    put_field "root" (show root)
    put_field "midi_config" (PPrint.pshow midi)
    pprint_defaults defaults

pprint_defaults (State.Default scale inst tempo) = do
    put_field "scale" (show scale)
    put_field "inst" (show inst)
    put_field "tempo" (show tempo)

pshow_map fm = "Map.fromList [\n"
    ++ Seq.join ",\n" (map show_assoc (Map.assocs fm))
    ++ "]\n"
    where
    show_assoc (k, v) = "(" ++ show k ++ ", " ++ v ++ ")"

put_field :: String -> String -> IO ()
put_field name val = do
    putStrLn $ "\n-- * " ++ name
    mapM_ putStr [name, " = ", val, ",\n"]

abbr_track track = "track_events =\n" ++ PPrint.pshow (map Simple.event events)
    where events = Events.ascending (Track.track_events track)

abbr_ruler ruler = "ruler_marklists = "
    ++ concatMap abbr_marklist (Ruler.ruler_marklists ruler)

abbr_marklist (name, mlist) =
    "(" ++ show name ++ ", " ++ show_len mlist ++ ")"
    where
    show_len (Ruler.Marklist marray) =
        "<marks:" ++ show (snd (IArray.bounds marray)) ++ ">"
