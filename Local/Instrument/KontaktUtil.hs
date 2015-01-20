-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE QuasiQuotes #-}
-- | Utilities for kontakt.
module Local.Instrument.KontaktUtil where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as Vector

import qualified Util.MultiString as MultiString
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil
import qualified Midi.Midi as Midi
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Perform.Midi.Instrument as Instrument
import Global


-- * tuning_ksp

-- | Create a script in Kontakt's hilariously incompetent KSP language to
-- retune a 12TET patch to the given scale.
tuning_ksp :: Instrument.PatchScale -> Text
tuning_ksp (Instrument.PatchScale name scale) =
    "on init\n\
    \    set_script_title(" <> showt name <> ")\n" <> pitch_table scale
    <> "end on\n\
    \\n\
    \on note\n\
    \    change_tune($EVENT_ID, %pitches[$EVENT_NOTE], 0)\n\
    \end on\n"
    -- To ignore notes that don't have a tuning, I could set another %set array
    -- with 0 or one, and do ignore_event($EVENT_ID).

pitch_table :: Vector.Vector Double -> Text
pitch_table scale = Text.unlines $ map ("    "<>) $
    "declare %pitches[" <> showt (Vector.length scale) <> "]"
    : ["%pitches[" <> showt key <> "] := " <> from_nn key nn |
        (key, nn) <- zip [0..] (Vector.toList scale)]
    where
    from_nn key nn
        | nn == 0 = "0"
        | otherwise = showt (round ((nn - fromIntegral key) * millicent) :: Int)
    millicent = 1000 * 100 -- silly scent willy sent


-- * drum_mute_ksp

drum_mute_ksp :: Text -> CUtil.PitchedNotes -> [(Drums.Group, [Drums.Group])]
    -> Either Text Text
drum_mute_ksp instrument notes stop_groups = do
    stop_group_ids <- make_stop_groups stop_groups groups
    let values = Map.fromList
            [ ("INSTRUMENT", instrument)
            , ("PITCH_TO_GROUP", ksp_array pitch_to_group)
            , ("MAX_GROUPS", showt (length groups))
            , ("STOP_GROUPS", ksp_array stop_group_ids)
            ]
    interpolate values drum_mute_template
    where
    groups = Seq.drop_dups id (List.sort (map fst ranges))
    group_to_id = Map.fromList $ zip groups [0..]
    group_id g = Map.findWithDefault (-1) g group_to_id
    pitch_to_group = Vector.toList $ midi_pitch_array (-1)
        [ ((Midi.from_key s, Midi.from_key e), group_id group)
        | (group, (s, e)) <- ranges
        ]
    ranges =
        [ (Drums.note_group note, (low, high))
        | (note, (_, low, high, _)) <- notes
        ]

make_stop_groups :: [(Drums.Group, [Drums.Group])] -> [Drums.Group]
    -> Either Text [Int]
make_stop_groups stop_groups groups = do
    mapM_ (get . fst) stop_groups -- ensure all groups in stop_groups are known
    concatMapM realize groups
    where
    ngroups = length groups
    realize group = do
        stops <- mapM get stops
        return $ take ngroups $ stops ++ repeat (-1)
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
    declare const $FadeTimeUs := 100 * 1000
    {- map pitch note number to group, or -1 to apply no processing -}
    declare %PitchToGroup[128] := *PITCH_TO_GROUP*
    declare const $MaxGroups := *MAX_GROUPS*
    {- remember this many sounding notes -}
    declare const $MaxVoices := 4
    {- map a group to the other groups it should stop -}
    declare %StopGroups[$MaxGroups * $MaxGroups] := *STOP_GROUPS*

    {- mutable, lower_under -}
    {- map a group to sounding events in that event -}
    declare %sounding_groups[$MaxGroups * $MaxVoices]

    declare $addr
    declare $event
    declare $group
    declare $i
    declare $j
    declare $stop_group
    declare $to

    $i := 0
    while ($i < $MaxGroups * $MaxGroups)
        %sounding_groups[$i] := -1
        $i := $i + 1
    end while
end on

on note
    $group := %PitchToGroup[$EVENT_NOTE]
    if ($group = 0)
        exit
    end if

    $addr := $group * $MaxVoices
    {- turn off all sounding notes stopped by this group -}
    $i := 0
    while ($i < $MaxGroups)
        $stop_group := %StopGroups[$group * $MaxGroups + $i]
        if ($stop_group # -1)
            $j := 0
            while ($j < $MaxVoices)
                $event := %sounding_groups[$stop_group * $MaxVoices + $j]
                if ($event # -1)
                    fade_out($event, $FadeTimeUs, 1)
                    %sounding_groups[$stop_group * $MaxVoices + $j] := 0
                end if
                $j := $j + 1
            end while
        end if
        $i := $i + 1
    end while

    {- put this note into %sounding_groups -}
    {- if I've run out of voices, turn off the last note -}
    if (%sounding_groups[$addr + $MaxVoices - 1] # -1)
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
