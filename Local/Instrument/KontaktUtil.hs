-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE QuasiQuotes #-}
-- | Utilities for kontakt.
module Local.Instrument.KontaktUtil where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector.Unboxed as Vector

import qualified Util.Map
import qualified Util.MultiString as MultiString
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil
import qualified Midi.Midi as Midi
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Perform.Midi.Instrument as Instrument
import Global


write :: FilePath -> Either Text Text -> IO ()
write fname = either (errorIO . untxt) $ \t -> do
    putStrLn $ "write " <> fname
    Text.IO.writeFile fname t

-- * tuning_ksp

-- | Create a script in Kontakt's hilariously incompetent KSP language to
-- retune a 12TET patch to the given scale.
tuning_ksp :: Instrument.PatchScale -> Either Text Text
tuning_ksp (Instrument.PatchScale name scale) =
    interpolate values tuning_template
    where
    values = Map.fromList
        [ ("TITLE", showt name)
        , ("PITCHES", ksp_array pitches)
        ]
    pitches = map (uncurry from_nn) (zip [0..] (Vector.toList scale))
    from_nn key nn
        | nn == 0 = 0
        | otherwise = round ((nn - fromIntegral key) * millicent)
    millicent :: Double
    millicent = 1000 * 100 -- silly scent willy sent

tuning_template :: Text
tuning_template = [MultiString.s|
on init
    set_script_title(*TITLE*)
    declare %Pitches[128] := *PITCHES*
end on

on note
    change_tune($EVENT_ID, %Pitches[$EVENT_NOTE], 0)
end on
|]
-- To ignore notes that don't have a tuning, I could set another %set array
-- with 0 or 1, and do ignore_event($EVENT_ID).

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
            , ("PITCH_TO_GROUP", ksp_array pitch_to_group)
            , ("PITCH_TO_KEYSWITCH", ksp_array pitch_to_keyswitch)
            , ("STOP_GROUPS", ksp_array stop_group_ids)
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
        Vector.// zip (map Midi.from_key (Maybe.catMaybes keyswitches)) [0..]
    (keyswitches, keyswitch_notes) = unzip $ Map.toAscList keyswitch_to_notes
    keyswitch_to_notes = Util.Map.multimap
        [ (ks, (Drums.note_group note, (low, high)))
        | (note, (ks, low, high, _)) <- notes
        ]
    groups = Seq.drop_dups id (List.sort (map (Drums.note_group . fst) notes))
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
    get g = maybe (Left $ "no group: " <> showt g) Right $
        List.elemIndex g groups

midi_pitch_array :: Vector.Unbox a => a -> [((Int, Int), a)] -> Vector.Vector a
midi_pitch_array deflt ranges =
    Vector.replicate 128 deflt Vector.// concatMap expand ranges
    where expand ((s, e), v) = [(i, v) | i <- Seq.range' s e 1]

drum_mute_template :: Text
drum_mute_template = [MultiString.s|
on init
    set_script_title("drum-mute for *INSTRUMENT*")

    {- constant, CamelCase -}
    declare const $None := -1
    declare const $FadeTimeUs := 100 * 1000
    {- map pitch note number to group, or $None to apply no processing -}
    declare const $MaxGroups := *MAX_GROUPS*
    declare const $MaxKeyswitches := *MAX_KEYSWITCHES*
    {- remember this many sounding notes -}
    declare const $MaxVoices := 4
    declare %PitchToGroup[128 * $MaxKeyswitches] := *PITCH_TO_GROUP*
    declare %PitchToKeyswitch[128] := *PITCH_TO_KEYSWITCH*
    {- map a group to the other groups it should stop -}
    declare %StopGroups[$MaxGroups * $MaxGroups] := *STOP_GROUPS*

    {- mutable, lower_under -}
    {- map a group to sounding events in that event -}
    declare %sounding_groups[$MaxGroups * $MaxVoices]
    {- current active keyswitch -}
    declare $keyswitch

    {- scratch -}
    declare $addr
    declare $event
    declare $group
    declare $i
    declare $j
    declare $stop_group
    declare $to

    $i := 0
    while ($i < num_elements(%sounding_groups))
        %sounding_groups[$i] := $None
        $i := $i + 1
    end while

    $keyswitch := 0
end on

on note
    if (%PitchToKeyswitch[$EVENT_NOTE] # $None)
        $keyswitch := %PitchToKeyswitch[$EVENT_NOTE]
    end if

    $group := %PitchToGroup[$EVENT_NOTE + $keyswitch * 128]
    if ($group = $None)
        exit
    end if

    $addr := $group * $MaxVoices
    {- turn off all sounding notes stopped by this group -}
    $i := 0
    while ($i < $MaxGroups)
        $stop_group := %StopGroups[$group * $MaxGroups + $i]
        if ($stop_group # $None)
            $j := 0
            while ($j < $MaxVoices)
                $event := %sounding_groups[$stop_group * $MaxVoices + $j]
                if ($event # $None)
                    fade_out($event, $FadeTimeUs, 1)
                    %sounding_groups[$stop_group * $MaxVoices + $j] := $None
                end if
                $j := $j + 1
            end while
        end if
        $i := $i + 1
    end while

    {- put this note into %sounding_groups -}
    {- if I've run out of voices, turn off the last note -}
    if (%sounding_groups[$addr + $MaxVoices - 1] # $None)
        fade_out(%sounding_groups[$addr + $MaxVoices - 1], $FadeTimeUs, 1)
    end if

    {- move notes up, and insert a new one at 0 -}
    $i := $MaxVoices - 1
    while ($i > 0)
        %sounding_groups[$addr + $i] := %sounding_groups[$addr + $i - 1]
        $i := $i - 1
    end while
    %sounding_groups[$addr] := $EVENT_ID
end on
|]


-- * util

interpolate :: Map.Map Text Text -> Text -> Either Text Text
interpolate values =
    TextUtil.mapDelimitedM False '*' $ \v ->
        maybe (Left $ "no value for " <> showt v) Right $ Map.lookup v values

ksp_array :: [Int] -> Text
ksp_array =
    ("( ...\n"<>) . (<>")") . Text.intercalate ", ...\n" . map ((indent<>)
        . Text.intercalate ", ") . Seq.chunked 8 . map showt
    where indent = Text.replicate 8 " "
