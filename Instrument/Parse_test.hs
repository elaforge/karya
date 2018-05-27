-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Instrument.Parse_test where
import qualified Text.Parsec as Parsec

import Util.Test
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Instrument.Common as Common
import qualified Instrument.InstTypes as InstTypes
import qualified Instrument.Parse as Parse

import Global


test_parse_annotations = do
    let f = bimap show (map extract)
            .  Parsec.runParser Parse.p_annotation_file () "test"
        extract (qualified, annots) =
            (InstTypes.show_qualified qualified, annots)
    equal (f "s/1 there\n") $ Right [("s/1", [("there", "")])]
    equal (f "s/1\n") $ Right [("s/1", [])]
    equal (f "s/1 a=b c=d\n") $
        Right [("s/1", [("a", "b"), ("c", "d")])]
    equal (f "s/1 a=b c=d # comment\n") $
        Right [("s/1", [("a", "b"), ("c", "d")])]
    equal (f "# empty\n") $ Right []
    left_like (f "bad inst\n") "unexpected \" \""

test_parse_patch_file = do
    let parse f = extract f
            . Parsec.runParser Parse.p_patch_file Parse.empty_state "test"
        extract f = bimap show (map f)

    let e_init (patch, _) = case Patch.patch_initialize patch of
            Patch.InitializeMidi msgs -> [m | Midi.ChannelMessage _ m <- msgs]
            init -> error $ "unexpected init: " ++ show init
        e_tags = Common.common_tags . snd

    let cc = Midi.ControlChange
    equal (parse e_init patch_file) $ Right
        [ [cc 0 0, cc 32 0, Midi.ProgramChange 0]
        , [cc 0 0, cc 32 0, Midi.ProgramChange 1]
        , [cc 0 0, cc 32 1, Midi.ProgramChange 0]
        , [cc 0 0, cc 32 1, Midi.ProgramChange 1]
        ]
    equal (parse e_tags patch_file) $ Right $
        replicate 3 [("category", "boring")] ++ [[("category", "interesting")]]

    equal (parse e_tags "p1, tag\np2, tag2=b\n") $ Right
        [[("tag", "")], [("tag2", "b")]]
    left_like (parse e_tags "p0\np1, bad_tag=blah") "unexpected \"_\""
    left_like (parse e_tags "p, tag=") "unexpected end of input"


patch_file :: Text
patch_file =
    "# some synth\n\
    \\n\
    \*bank 0\n\
    \Patch 1, category=boring\n\
    \Patch 2\n\
    \\n\
    \*bank 1\n\
    \Patch 1/0\n\
    \Patch 1/1, category=interesting\n"
