-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Instrument.Parse_test where
import qualified Util.ParseM as ParseM
import qualified Instrument.Common as Common
import qualified Instrument.InstT as InstT
import qualified Instrument.Parse as Parse

import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch

import           Global
import           Util.Test


test_parse_annotations :: Test
test_parse_annotations = do
    let f = second (map extract)
            . ParseM.parseS () "" Parse.pAnnotationFile
        extract (qualified, annots) =
            (InstT.showQualified qualified, annots)
    equal (f "s/1 there\n") $ Right [("s/1", [("there", "")])]
    equal (f "s/1\n") $ Right [("s/1", [])]
    equal (f "s/1 a=b c=d\n") $
        Right [("s/1", [("a", "b"), ("c", "d")])]
    equal (f "s/1 a=b c=d # comment\n") $
        Right [("s/1", [("a", "b"), ("c", "d")])]
    equal (f "# empty\n") $ Right []
    leftLike (f "bad inst\n") "unexpected 'b'"

test_parse_patch_file :: Test
test_parse_patch_file = do
    let parse ex = extract ex
            . ParseM.parseS Parse.emptyState "" Parse.pPatchFile
        extract ex = second (map ex)

    let eInit (patch, _) = case Patch.patchInitialize patch of
            Patch.InitializeMidi msgs -> [m | Midi.ChannelMessage _ m <- msgs]
            init -> error $ "unexpected init: " ++ show init
        eTags = Common.commonTags . snd

    let cc = Midi.ControlChange
    rightEqual (parse eInit patchFile)
        [ [cc 0 0, cc 32 0, Midi.ProgramChange 0]
        , [cc 0 0, cc 32 0, Midi.ProgramChange 1]
        , [cc 0 0, cc 32 1, Midi.ProgramChange 0]
        , [cc 0 0, cc 32 1, Midi.ProgramChange 1]
        ]
    equal (parse eTags patchFile) $ Right $
        replicate 3 [("category", "boring")] ++ [[("category", "interesting")]]

    equal (parse eTags "p1, tag\np2, tag2=b\n") $ Right
        [[("tag", "")], [("tag2", "b")]]
    leftLike (parse eTags "p0\np1, bad_tag=blah") "unexpected '_'"
    leftLike (parse eTags "p, tag=") "unexpected end of input"


patchFile :: Text
patchFile =
    "# some synth\n\
    \\n\
    \*bank 0\n\
    \Patch 1, category=boring\n\
    \Patch 2\n\
    \\n\
    \*bank 1\n\
    \Patch 1/0\n\
    \Patch 1/1, category=interesting\n"
