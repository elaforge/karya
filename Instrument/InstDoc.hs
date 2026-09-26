-- Copyright 2026 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Format documentation for instruments.
module Instrument.InstDoc (infoOf) where
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


infoOf :: InstT.Qualified -> Text -> Cmd.Inst -> [Tag.Tag] -> Text
infoOf qualified synthDoc (Inst.Inst backend common) tags = mconcat
    [ Text.intercalate " -- " $ filter (not . Text.null)
        [ synthName
        , if Text.null instName then "*" else instName
        , synthDoc
        , Inst.backendName backend
        ]
    , "\n\n"
    , body
    ]
    where
    InstT.Qualified synthName instName = qualified
    body = formatFields $ commonFields tags common ++ backendFields
    backendFields = case backend of
        Inst.Dummy msg -> [("dummy msg", msg)]
        Inst.Midi inst -> midiFields instName inst
        Inst.Im patch -> imPatchFields patch
        Inst.Sc patch -> scPatchFields patch

formatFields :: [(Text, Text)] -> Text
formatFields = Text.unlines . filter (not . Text.null) . map field

field :: (Text, Text) -> Text
field (title, rawText)
    | Text.null text = ""
    | Text.length text < 40 && not ("\n" `Text.isInfixOf` text) =
        title <> ": " <> text <> "\n"
    | otherwise = "\t" <> title <> ":\n" <> text <> "\n"
    where text = Text.strip rawText

commonFields :: [Tag.Tag] -> Common.Common Cmd.InstrumentCode -> [(Text, Text)]
commonFields tags common =
    [ ("Environ", if env == mempty then "" else pretty env)
    , ("Flags", Text.intercalate ", " $ map showt $ Set.toList flags)
    , ("Call map", if Map.null callMap then "" else pretty callMap)
    -- code
    , ("Cmds", showCmds code)
    , ("Note generators", showCalls CallDoc.GeneratorCall gen)
    , ("Note transformers", showCalls CallDoc.TransformerCall trans)
    , ("Track calls", showCalls CallDoc.TrackCall track)
    , ("Val calls", showCalls CallDoc.ValCall val)
    -- info
    , ("Doc", doc)
    , ("Tags", showTags tags)
    -- TODO lost the patch_file field
    ]
    where
    Derive.Scopes gen trans track val = Cmd.instCalls code
    showCalls ctype = showCallBindings . CallDoc.entries ctype
        . CallDoc.callMapToEntries . CallDoc.callMapDoc
    Common.Common
        { commonCode = code
        , commonEnviron = env
        , commonDoc = Doc.Doc doc
        , commonFlags = flags
        , commonCallMap = callMap
        } = common

showCmds :: Cmd.InstrumentCode -> Text
showCmds code = Text.unlines $ concat
    [ map showHandler (Cmd.instCmds code)
    , maybe [] (const ["[custom thru]"]) $ Cmd.instThru code
    ]

showHandler :: Cmd.Handler m -> Text
showHandler = \case
    Cmd.Handler (Just noteEntry) cmd ->
        Cmd.cmdName cmd <> ": " <> case noteEntry of
            Cmd.WithoutOctave m -> list $ Map.elems m
            Cmd.WithOctave m -> list $ concatMap Map.elems $ Map.elems m
        where
        list xs = "["
            <> Text.unwords (Lists.unique (filter (not . Text.null) xs))
            <> "]"
    Cmd.Handler Nothing cmd -> Cmd.cmdName cmd
    Cmd.Keymap keymap -> pretty $ map Cmd.cmdName $ Map.elems keymap

showTags :: [(Text, Text)] -> Text
showTags tags =
    Text.unwords [quote k <> "=" <> quote v | (k, v) <- Lists.sortOn fst tags]

quote :: Text -> Text
quote s
    | Text.any Char.isSpace s = "\"" <> s <> "\""
    | otherwise = s

showCallBindings :: [CallDoc.CallBindings] -> Text
showCallBindings = Lazy.toStrict . Format.render "\t" 10000
    . Format.paragraphs . map (CallDoc.callBindingsText False)
    -- Let fltk do the wrapping.  Of course it doesn't know how the indentation
    -- works, so wrapped lines don't get indented, but it doesn't look that
    -- bad.

-- ** midi

midiFields :: InstT.Name -> Midi.Patch.Patch -> [(Text, Text)]
midiFields name patch =
    -- important properties
    [ ("Flags", Text.intercalate ", " $ map showt $ Set.toList $
        fromMaybe mempty flags)
    , ("Controls", showControlMap controlMap)
    , ("Control defaults", pretty controlDefaults)
    -- implementation details
    , ("Attribute map", showAttributeMap attrMap)
    , ("Mode map", showModeMap modeMap)
    , ("Pitchbend range", pretty pbRange)
    , ("Decay", if decay == Nothing then "" else pretty decay)
    , ("Scale", maybe "" pretty scale)
    , ("Initialization", showInitialize initialize)
    , ("Original name", if name == origName then "" else showt origName)
    ]
    where
    Midi.Patch.Patch
        { patchName = origName
        , patchControlMap = controlMap
        , patchInitialize = initialize
        , patchAttributeMap = attrMap
        , patchModeMap = modeMap
        , patchDefaults = settings
        } = patch
    Midi.Patch.Settings flags scale decay pbRange controlDefaults = settings

showAttributeMap :: Midi.Patch.AttributeMap -> Text
showAttributeMap (Common.AttributeMap table) =
    -- TODO browser uses a variable width font, so this doesn't actually line
    -- up.  I would have to either use fixed width, or have explicit columns
    -- via html.
    Text.unlines $ Texts.columns 2 $
        map fmt (Lists.sortOn (lowKey . snd) table)
    where
    -- If this instrument uses a keymap, it's easier to read the attribute map
    -- if I put it in keymap order.
    lowKey (_, Just (Midi.Patch.UnpitchedKeymap k)) = Just k
    lowKey (_, Just (Midi.Patch.PitchedKeymap k _ _)) = Just k
    lowKey (_, Nothing) = Nothing
    fmt (attrs, (keyswitches, mbKeymap)) =
        [ pretty attrs
        , pretty keyswitches <> maybe "" ((" "<>) . pretty) mbKeymap
        ]

showModeMap :: Midi.Patch.ModeMap -> Text
showModeMap (Midi.Patch.ModeMap table) = Text.unlines
    [ key <> ": " <> Text.intercalate ", "
        [ pretty val <> "=" <> pretty ks
        | (val, ks) <- Map.toList modes
        ] <> " [default: " <> pretty deflt <> "]"
    | (key, (deflt, modes)) <- Map.toAscList table
    ]

showControlMap :: Midi.Control.ControlMap -> Text
showControlMap cmap = Text.intercalate ", "
    [ ScoreT.controlName cont <> " (" <> showt num <> ")"
    | (cont, num) <- Map.toList cmap
    ]

showInitialize :: Midi.Patch.InitializePatch -> Text
showInitialize = \case
    Midi.Patch.NoInitialization -> ""
    Midi.Patch.InitializeMessage msg -> "Message: " <> msg
    Midi.Patch.InitializeMidi msgs -> Text.unlines (map pretty msgs)

-- ** im

imPatchFields :: Im.Patch.Patch -> [(Text, Text)]
imPatchFields (Im.Patch.Patch controls attrMap elements) =
    [ ("Attributes", Text.intercalate ", " $ map pretty $
        Common.mappedAttributes attrMap)
    , ("Controls", Text.unlines $ Texts.columns 2
        [[pretty control, doc] | (control, doc) <- Map.toAscList controls])
    , ("Elements", Text.unwords (Set.toList elements))
    ]

-- ** sc

scPatchFields :: Sc.Patch.Patch -> [(Text, Text)]
scPatchFields (Sc.Patch.Patch _name _filename controls) =
    [ ("Controls", Text.unlines
        [ pretty control <> "\t" <> showt id
        | (control, id) <- Map.toAscList controls
        ])
    ]
