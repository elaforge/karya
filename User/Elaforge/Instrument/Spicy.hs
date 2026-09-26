-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Spicy guitar, free at http://www.spicyguitar.com/
module User.Elaforge.Instrument.Spicy where
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Prelude.Note as Note
import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.ScoreT as ScoreT

import qualified Instrument.InstT as InstT
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Ui.UiConfig as UiConfig

import           Global


synthName :: InstT.SynthName
synthName = "spicy"

synth :: MidiInst.Synth
synth = MidiInst.synth synthName "Spicy Guitar, http://www.spicyguitar.com" $
    MidiInst.synthControls controls patches

patches :: [MidiInst.Patch]
patches = (:[]) $
    MidiInst.code #= MidiInst.nullCall noteCall $
    MidiInst.makePatch $
    MidiInst.addFlag Patch.HoldKeyswitch $
    Patch.attributeMap #= Patch.singleKeyswitches keyswitches $
    Patch.patch (-3, 3) Patch.defaultName


-- | WARNING: changing these while playing tends to crash the VST.
controls :: [(Midi.Control, ScoreT.Control)]
controls =
    [ (20, "position") -- 0 for bridge, 1 for middle
    , (21, "finger") -- 0 for finger plucking, 1 for pick
    , (22, "inharmonicity")
    , (23, "twang")
    , (24, "color")
    , (25, "impedance")
    , (26, "vibrato") -- speed of vibrato
    , (27, "mute") -- amount of palm mute effect
    , (28, "harm")
    ]

keyswitches :: [(Attrs.Attributes, Midi.Key)]
keyswitches =
    [ (Attrs.legato, Key.b2)
    , (Attrs.mute, Key.c3)
    , (Attrs.harm, Key.cs3)
    ]

noteCall :: Derive.Generator Derive.Note
noteCall = Note.transformedNote
    ("If given a string-name attribute in " <> attrsDoc <> ", suffix the"
        <> " instrument with the string name.  When combined with the proper"
        <> " midi config, this will redirect the note to the proper channel"
        <> " for that string.") mempty
    (const transform)
    where
    attrsDoc = Doc.Doc $ Text.intercalate ", " ["`" <> a <> "`" | a <- strings]
    transform deriver = do
        attrs <- Call.getAttributes
        inst <- Call.lookupInstrument
        let string = Lists.head
                [ string
                | attr <- Attrs.toList attrs, string <- strings
                , attr == string
                ]
        case (inst, string) of
            (Just inst, Just string) ->
                Derive.withInstrument (stringInst inst string) deriver
            _ -> deriver
    stringInst inst string =
        ScoreT.Instrument $ ScoreT.instrumentName inst <> "-" <> string

strings :: [Text]
strings = ["e1", "a", "d", "g", "b", "e2"]

-- | Create the proper midi config to work with the string attrs used by
-- 'note_call'.
allocations :: Text -> InstT.Name -> UiConfig.Allocations
allocations devName name = UiConfig.midiAllocations $
    inst name 0 : [inst (name <> "-" <> string) chan
        | (string, chan) <- zip strings [1..]]
    where
    inst name chan =
        ( ScoreT.Instrument name
        , (InstT.Qualified synthName name, MidiInst.config1 dev chan)
        )
    dev = Midi.writeDevice devName
