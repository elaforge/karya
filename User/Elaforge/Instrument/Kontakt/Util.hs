-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for kontakt.
module User.Elaforge.Instrument.Kontakt.Util where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector.Unboxed as Vector

import qualified Util.Map
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Midi.Midi as Midi
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Perform.Midi.Patch as Patch
import Global


write :: FilePath -> Either Text Text -> IO ()
write fname = either errorIO $ \t -> do
    putStrLn $ "write " <> fname
    Text.IO.writeFile fname t

-- * tuning_ksp

-- | Create a script in Kontakt's hilariously incompetent KSP language to
-- retune a 12TET patch to the given scale.
tuning_ksp :: Maybe Patch.AttributeMap -> Patch.Scale -> Either Text Text
tuning_ksp attr_map scale = interpolate values tuning_template
    where
    values = Map.fromList
        [ ("TITLE", showt (Patch.scale_name scale))
        , ("PITCHES", ksp_array 6 pitches)
        ]
    -- -1 marks unmapped pitches, since it's visually distinct from 0.
    pitches = map (maybe (-1) (round . (*millicent)))
        (Patch.scale_offsets attr_map scale)
    millicent = 1000 * 100 -- silly scent willy sent

tuning_template :: Text
tuning_template =
    -- To ignore notes that don't have a tuning, I could set another %set array
    -- with 0 or 1, and do ignore_event($EVENT_ID).
    "on init\n\
    \    set_script_title(*TITLE*)\n\
    \    declare %Pitches[128] := *PITCHES*\n\
    \end on\n\
    \\n\
    \on note\n\
    \    change_tune($EVENT_ID, %Pitches[$EVENT_NOTE], 0)\n\
    \end on\n"

-- * drum_mute_ksp

-- | Create KSP to handle sample stopping.  Each drum Note has a Group, and
-- each Group can stop a set of other groups from sounding.
--
-- Kontakt has a built-in mechanism, but as usual it gets it wrong.  The
-- built-in mechanism lets you assign notes to a group, and limit voices in the
-- group, which means that two of the same strokes in a row will mute each
-- other.
drum_mute_ksp :: Text -> CUtil.PitchedNotes -> [(Drums.Group, [Drums.Group])]
    -- ^ each Group along with a set of Groups that it stops
    -> Either Text Text
drum_mute_ksp instrument notes stop_groups = do
    stop_group_ids <- make_stop_groups stop_groups groups
    let values = Map.fromList
            [ ("INSTRUMENT", instrument)
            , ("MAX_GROUPS", showt (length groups))
            , ("MAX_KEYSWITCHES", showt max_keyswitches)
            , ("PITCH_TO_GROUP", ksp_array 8 pitch_to_group)
            , ("PITCH_TO_KEYSWITCH", ksp_array 8 pitch_to_keyswitch)
            , ("STOP_GROUPS", ksp_array 8 stop_group_ids)
            ]
    interpolate values drum_mute_template
    where
    (pitch_to_keyswitch, pitch_to_group, groups, max_keyswitches) =
        drum_mute_values notes

drum_mute_values :: CUtil.PitchedNotes -> ([Int], [Int], [Drums.Group], Int)
drum_mute_values notes =
    (pitch_to_keyswitch, pitch_to_group, groups, length keyswitch_notes)
    where
    pitch_to_group = Vector.toList $ mconcat $ map make keyswitch_notes
    make ks_notes = midi_pitch_array none
        [ ((Midi.from_key s, Midi.from_key e), group_id group)
        | (group, (s, e)) <- ks_notes
        ]
    pitch_to_keyswitch = Vector.toList $ Vector.replicate 128 none
        Vector.// zip (map Midi.from_key keyswitches) [0..]
    (keyswitches, keyswitch_notes) = unzip $ Map.toAscList keyswitch_to_notes
    -- If this instrument doesn't use keyswitches (it could use
    -- 'Patch.ControlSwitch'es), then they all wind up with 0.  I need
    -- at least one keyswitch so pitch_to_group isn't empty, and pretending
    -- there's one at 0 is fine since it never changes.  This is fine, assuming
    -- that overlapping groups all belong to the same stop gorup.
    keyswitch_to_notes = Util.Map.multimap
        [ (ks_of keyswitch, (Drums._group note, (low, high)))
        | (note, (keyswitch, low, high, _)) <- notes
        ]
    ks_of (Patch.Keyswitch ks : _) = ks
    ks_of _ = 0
    groups = Seq.drop_dups id (List.sort (map (Drums._group . fst) notes))
    group_to_id = Map.fromList $ zip groups [0..]
    group_id g = Map.findWithDefault none g group_to_id

-- | Used in KSP for a nothing value.
none :: Int
none = -1

make_stop_groups :: [(Drums.Group, [Drums.Group])] -> [Drums.Group]
    -> Either Text [Int]
make_stop_groups stop_groups groups = do
    mapM_ (get . fst) stop_groups -- ensure all groups in stop_groups are known
    concatMapM realize groups
    where
    ngroups = length groups
    realize group = do
        stops <- mapM get stops
        return $ take ngroups $ stops ++ repeat none
        where stops = fromMaybe [] $ lookup group stop_groups
    get g = justErr ("no group: " <> showt g) $ List.elemIndex g groups

midi_pitch_array :: Vector.Unbox a => a -> [((Int, Int), a)] -> Vector.Vector a
midi_pitch_array deflt ranges =
    Vector.replicate 128 deflt Vector.// concatMap expand ranges
    where expand ((s, e), v) = [(i, v) | i <- Seq.range' s e 1]

drum_mute_template :: Text
drum_mute_template =
    "on init\n\
    \    set_script_title(\"drum-mute for *INSTRUMENT*\")\n\
    \\n\
    \    {- immutable, CamelCase -}\n\
    \    declare const $None := -1\n\
    \    declare const $FadeTimeUs := 100 * 1000\n\
    \    {- map pitch note number to group, or $None to apply no processing -}\n\
    \    declare const $MaxGroups := *MAX_GROUPS*\n\
    \    declare const $MaxKeyswitches := *MAX_KEYSWITCHES*\n\
    \    {- remember this many sounding notes -}\n\
    \    declare const $MaxVoices := 4\n\
    \    declare %PitchToGroup[128 * $MaxKeyswitches] := *PITCH_TO_GROUP*\n\
    \    {- keyswitch numbers for keyswitch notes, -1 for the rest -}\n\
    \    declare %PitchToKeyswitch[128] := *PITCH_TO_KEYSWITCH*\n\
    \    {- map a group to the other groups it should stop -}\n\
    \    declare %StopGroups[$MaxGroups * $MaxGroups] := *STOP_GROUPS*\n\
    \\n\
    \    {- mutable, lower_under -}\n\
    \    {- map a group to sounding events in that event -}\n\
    \    declare %sounding_groups[$MaxGroups * $MaxVoices]\n\
    \    {- current active keyswitch -}\n\
    \    declare $keyswitch\n\
    \\n\
    \    {- scratch -}\n\
    \    declare $addr\n\
    \    declare $event\n\
    \    declare $group\n\
    \    declare $i\n\
    \    declare $j\n\
    \    declare $stop_group\n\
    \    declare $to\n\
    \\n\
    \    $i := 0\n\
    \    while ($i < num_elements(%sounding_groups))\n\
    \        %sounding_groups[$i] := $None\n\
    \        $i := $i + 1\n\
    \    end while\n\
    \\n\
    \    $keyswitch := 0\n\
    \end on\n\
    \\n\
    \on note\n\
    \    if (%PitchToKeyswitch[$EVENT_NOTE] # $None)\n\
    \        $keyswitch := %PitchToKeyswitch[$EVENT_NOTE]\n\
    \    end if\n\
    \\n\
    \    $group := %PitchToGroup[$EVENT_NOTE + $keyswitch * 128]\n\
    \    if ($group = $None)\n\
    \        exit\n\
    \    end if\n\
    \\n\
    \    $addr := $group * $MaxVoices\n\
    \    {- turn off all sounding notes stopped by this group -}\n\
    \    $i := 0\n\
    \    while ($i < $MaxGroups)\n\
    \        $stop_group := %StopGroups[$group * $MaxGroups + $i]\n\
    \        if ($stop_group # $None)\n\
    \            $j := 0\n\
    \            while ($j < $MaxVoices)\n\
    \                $event := %sounding_groups[$stop_group * $MaxVoices + $j]\n\
    \                if ($event # $None)\n\
    \                    fade_out($event, $FadeTimeUs, 1)\n\
    \                    %sounding_groups[$stop_group * $MaxVoices + $j] := $None\n\
    \                end if\n\
    \                $j := $j + 1\n\
    \            end while\n\
    \        end if\n\
    \        $i := $i + 1\n\
    \    end while\n\
    \\n\
    \    {- put this note into %sounding_groups -}\n\
    \    {- if I've run out of voices, turn off the last note -}\n\
    \    if (%sounding_groups[$addr + $MaxVoices - 1] # $None)\n\
    \        fade_out(%sounding_groups[$addr + $MaxVoices - 1], $FadeTimeUs, 1)\n\
    \    end if\n\
    \\n\
    \    {- move notes up, and insert a new one at 0 -}\n\
    \    $i := $MaxVoices - 1\n\
    \    while ($i > 0)\n\
    \        %sounding_groups[$addr + $i] := %sounding_groups[$addr + $i - 1]\n\
    \        $i := $i - 1\n\
    \    end while\n\
    \    %sounding_groups[$addr] := $EVENT_ID\n\
    \end on\n"


-- * util

interpolate :: Map Text Text -> Text -> Either Text Text
interpolate values =
    TextUtil.mapDelimitedM False '*' $ \v ->
        justErr ("no value for " <> showt v) $ Map.lookup v values

ksp_array :: Int -> [Int] -> Text
ksp_array chunk_size =
    ("( ...\n"<>) . (<>")") . Text.intercalate ", ...\n" . map ((indent<>)
        . Text.intercalate ", ") . Seq.chunked chunk_size . map showt
    where indent = Text.replicate 8 " "
