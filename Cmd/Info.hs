-- | Cmds that display some info for the user.
--
-- These will be frequently be called from the language, but may also be used
-- by built in cmds.  Since they are intended for human consumption, many
-- of them return strings.
module Cmd.Info where
import qualified Data.Set as Set
import qualified Util.Seq as Seq
import qualified Util.Map as Map

import qualified Midi.Midi as Midi

import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb


inst_info :: (Monad m) => Score.Instrument -> Cmd.CmdT m String
inst_info inst = do
    maybe_info <- Cmd.lookup_instrument_info inst
    midi_config <- State.get_midi_config
    let show_info = show_instrument_info
            (Map.get [] inst (Instrument.config_alloc midi_config))
    return $ show_inst inst ++ ": " ++ maybe "<not found>" show_info maybe_info

show_instrument_info :: [Instrument.Addr] -> MidiDb.Info -> String
show_instrument_info addrs (MidiDb.Info _synth patch) = unlines
    [ "keyswitches: " ++ show_keyswitch_map (Instrument.patch_keyswitches patch)
    , "addrs: " ++ show_addrs addrs
    ]

-- | Looks like: "wdev1:[0,1,2]; wdev2:[0]"
show_addrs :: [Instrument.Addr] -> String
show_addrs addrs = show_list2
    [ Midi.un_write_device wdev ++ ":" ++ Seq.join "," (map (show . snd) addrs)
    | (wdev, addrs) <- Seq.keyed_group_with fst addrs]

show_inst :: Score.Instrument -> String
show_inst (Score.Instrument name) = '>' : name

show_keyswitch_map :: Instrument.KeyswitchMap -> String
show_keyswitch_map (Instrument.KeyswitchMap attr_ks) = show_list $
    map (('+':) . Seq.join "+" . Set.elems . fst) attr_ks

show_list, show_list2 :: [String] -> String
show_list [] = "[]"
show_list xs = Seq.join ", " xs

show_list2 [] = "[]"
show_list2 xs = Seq.join "; " xs

str :: String -> String
str "" = '"' : '"' : ""
str s = s
