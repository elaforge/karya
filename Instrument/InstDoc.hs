-- Copyright 2026 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Format documentation for instruments.
module Instrument.InstDoc (info_of) where
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy

import qualified Util.Doc as Doc
import qualified Util.Format as Format
import qualified Util.Lists as Lists
import qualified Util.Texts as Texts

import qualified Cmd.CallDoc as CallDoc
import qualified Cmd.Cmd as Cmd
import qualified Derive.Derive as Derive
import qualified Derive.ScoreT as ScoreT
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT
import qualified Instrument.Tag as Tag

import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Midi.Control as Midi.Control
import qualified Perform.Midi.Patch as Midi.Patch
import qualified Perform.Sc.Patch as Sc.Patch

import           Global


info_of :: InstT.Qualified -> Text -> Cmd.Inst -> [Tag.Tag] -> Text
info_of qualified synth_doc (Inst.Inst backend common) tags = mconcat
    [ Text.intercalate " -- " $ filter (not . Text.null)
        [ synth_name
        , if Text.null inst_name then "*" else inst_name
        , synth_doc
        , Inst.backend_name backend
        ]
    , "\n\n"
    , body
    ]
    where
    InstT.Qualified synth_name inst_name = qualified
    body = format_fields $ common_fields tags common ++ backend_fields
    backend_fields = case backend of
        Inst.Dummy msg -> [("dummy msg", msg)]
        Inst.Midi inst -> midi_fields inst_name inst
        Inst.Im patch -> im_patch_fields patch
        Inst.Sc patch -> sc_patch_fields patch

format_fields :: [(Text, Text)] -> Text
format_fields = Text.unlines . filter (not . Text.null) . map field

field :: (Text, Text) -> Text
field (title, raw_text)
    | Text.null text = ""
    | Text.length text < 40 && not ("\n" `Text.isInfixOf` text) =
        title <> ": " <> text <> "\n"
    | otherwise = "\t" <> title <> ":\n" <> text <> "\n"
    where text = Text.strip raw_text

common_fields :: [Tag.Tag] -> Common.Common Cmd.InstrumentCode -> [(Text, Text)]
common_fields tags common =
    [ ("Environ", if env == mempty then "" else pretty env)
    , ("Flags", Text.intercalate ", " $ map showt $ Set.toList flags)
    , ("Call map", if Map.null call_map then "" else pretty call_map)
    -- code
    , ("Cmds", show_cmds code)
    , ("Note generators", show_calls CallDoc.GeneratorCall gen)
    , ("Note transformers", show_calls CallDoc.TransformerCall trans)
    , ("Track calls", show_calls CallDoc.TrackCall track)
    , ("Val calls", show_calls CallDoc.ValCall val)
    -- info
    , ("Doc", doc)
    , ("Tags", show_tags tags)
    -- TODO lost the patch_file field
    ]
    where
    Derive.Scopes gen trans track val = Cmd.inst_calls code
    show_calls ctype = show_call_bindings . CallDoc.entries ctype
        . CallDoc.call_map_to_entries . CallDoc.call_map_doc
    Common.Common
        { common_code = code
        , common_environ = env
        , common_doc = Doc.Doc doc
        , common_flags = flags
        , common_call_map = call_map
        } = common

show_cmds :: Cmd.InstrumentCode -> Text
show_cmds code = Text.unlines $ concat
    [ map show_handler (Cmd.inst_cmds code)
    , maybe [] (const ["[custom thru]"]) $ Cmd.inst_thru code
    ]

show_handler :: Cmd.Handler m -> Text
show_handler = \case
    Cmd.Handler (Just note_entry) cmd ->
        Cmd.cmd_name cmd <> ": " <> case note_entry of
            Cmd.WithoutOctave m -> list $ Map.elems m
            Cmd.WithOctave m -> list $ concatMap Map.elems $ Map.elems m
        where
        list xs = "["
            <> Text.unwords (Lists.unique (filter (not . Text.null) xs))
            <> "]"
    Cmd.Handler Nothing cmd -> Cmd.cmd_name cmd
    Cmd.Keymap keymap -> pretty $ map Cmd.cmd_name $ Map.elems keymap

show_tags :: [(Text, Text)] -> Text
show_tags tags =
    Text.unwords [quote k <> "=" <> quote v | (k, v) <- Lists.sortOn fst tags]

quote :: Text -> Text
quote s
    | Text.any Char.isSpace s = "\"" <> s <> "\""
    | otherwise = s

show_call_bindings :: [CallDoc.CallBindings] -> Text
show_call_bindings = Lazy.toStrict . Format.render "\t" 10000
    . Format.paragraphs . map (CallDoc.call_bindings_text False)
    -- Let fltk do the wrapping.  Of course it doesn't know how the indentation
    -- works, so wrapped lines don't get indented, but it doesn't look that
    -- bad.

-- ** midi

midi_fields :: InstT.Name -> Midi.Patch.Patch -> [(Text, Text)]
midi_fields name patch =
    -- important properties
    [ ("Flags", Text.intercalate ", " $ map showt $ Set.toList $
        fromMaybe mempty flags)
    , ("Controls", show_control_map control_map)
    , ("Control defaults", pretty control_defaults)
    -- implementation details
    , ("Attribute map", show_attribute_map attr_map)
    , ("Mode map", show_mode_map mode_map)
    , ("Pitchbend range", pretty pb_range)
    , ("Decay", if decay == Nothing then "" else pretty decay)
    , ("Scale", maybe "" pretty scale)
    , ("Initialization", show_initialize initialize)
    , ("Original name", if name == orig_name then "" else showt orig_name)
    ]
    where
    Midi.Patch.Patch
        { patch_name = orig_name
        , patch_control_map = control_map
        , patch_initialize = initialize
        , patch_attribute_map = attr_map
        , patch_mode_map = mode_map
        , patch_defaults = settings
        } = patch
    Midi.Patch.Settings flags scale decay pb_range control_defaults = settings

show_attribute_map :: Midi.Patch.AttributeMap -> Text
show_attribute_map (Common.AttributeMap table) =
    -- TODO browser uses a variable width font, so this doesn't actually line
    -- up.  I would have to either use fixed width, or have explicit columns
    -- via html.
    Text.unlines $ Texts.columns 2 $
        map fmt (Lists.sortOn (low_key . snd) table)
    where
    -- If this instrument uses a keymap, it's easier to read the attribute map
    -- if I put it in keymap order.
    low_key (_, Just (Midi.Patch.UnpitchedKeymap k)) = Just k
    low_key (_, Just (Midi.Patch.PitchedKeymap k _ _)) = Just k
    low_key (_, Nothing) = Nothing
    fmt (attrs, (keyswitches, mb_keymap)) =
        [ pretty attrs
        , pretty keyswitches <> maybe "" ((" "<>) . pretty) mb_keymap
        ]

show_mode_map :: Midi.Patch.ModeMap -> Text
show_mode_map (Midi.Patch.ModeMap table) = Text.unlines
    [ key <> ": " <> Text.intercalate ", "
        [ pretty val <> "=" <> pretty ks
        | (val, ks) <- Map.toList modes
        ] <> " [default: " <> pretty deflt <> "]"
    | (key, (deflt, modes)) <- Map.toAscList table
    ]

show_control_map :: Midi.Control.ControlMap -> Text
show_control_map cmap = Text.intercalate ", "
    [ ScoreT.control_name cont <> " (" <> showt num <> ")"
    | (cont, num) <- Map.toList cmap
    ]

show_initialize :: Midi.Patch.InitializePatch -> Text
show_initialize = \case
    Midi.Patch.NoInitialization -> ""
    Midi.Patch.InitializeMessage msg -> "Message: " <> msg
    Midi.Patch.InitializeMidi msgs -> Text.unlines (map pretty msgs)

-- ** im

im_patch_fields :: Im.Patch.Patch -> [(Text, Text)]
im_patch_fields (Im.Patch.Patch controls attr_map elements) =
    [ ("Attributes", Text.intercalate ", " $ map pretty $
        Common.mapped_attributes attr_map)
    , ("Controls", Text.unlines $ Texts.columns 2
        [[pretty control, doc] | (control, doc) <- Map.toAscList controls])
    , ("Elements", Text.unwords (Set.toList elements))
    ]

-- ** sc

sc_patch_fields :: Sc.Patch.Patch -> [(Text, Text)]
sc_patch_fields (Sc.Patch.Patch _name _filename controls) =
    [ ("Controls", Text.unlines
        [ pretty control <> "\t" <> showt id
        | (control, id) <- Map.toAscList controls
        ])
    ]
