-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Ruler.RulerUtil_test where
import qualified Data.Map as Map

import qualified Util.Lists as Lists
import qualified Cmd.Create as Create
import qualified Cmd.Ruler.RulerUtil as RulerUtil
import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.Meter.Mark as Mark
import qualified Ui.Ruler as Ruler
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_modify :: Test
test_modify = do
    let f withRuler scope =
            extract $ snd $ run make $
                RulerUtil.modify scope UiTest.defaultBlockId
                    (const $ Right $ mkruler ["X", "Y"])
            where make = if withRuler then makeSections else makeNoRuler 2
        extract st = (trackRulers st, extractRulers st)
        xy = ["X", "Y"]
    -- Both are modified.
    equal (f True RulerUtil.Block)
        (["s1", "s1", "s2", "s2"], [("s1", xy), ("s2", xy)])
    -- Just set one.
    equal (f True (RulerUtil.Tracks [1]))
        (["s1", "s1", "s2", "s2"], [("s1", xy), ("s2", ["s2"])])
    -- This winds up being the same.
    equal (f True (RulerUtil.Section 3))
        (["s1", "s1", "s2", "s2"], [("s1", ["s1"]), ("s2", xy)])
    -- Modify with no ruler.
    equal (f False RulerUtil.Block) (["b1", "b1", "b1"], [("b1", xy)])
    equal (f False (RulerUtil.Tracks [1]))
        (["-no-ruler-", "b1", "-no-ruler-"], [("b1", xy)])
    -- Section encompasses the whole block.
    equal (f False (RulerUtil.Section 1))
        (["b1", "b1", "b1"], [("b1", xy)])

test_local :: Test
test_local = do
    let f scope = extract $ snd $ run makeSections $
            RulerUtil.local scope UiTest.defaultBlockId
                (const $ Right $ mkruler ["X", "Y"])
        extract st = (trackRulers st, extractRulers st)
        xy = ["X", "Y"]
        unmodified n = ("s" <> showt n, ["s" <> showt n])
    -- They're unique to the block, so modify.
    equal (f RulerUtil.Block)
        (["s1", "s1", "s2", "s2"], [("s1", xy), ("s2", xy)])
    -- Modify single track.
    equal (f (RulerUtil.Tracks [1]))
        (["s1", "b1", "s2", "s2"], [("b1", xy), unmodified 1, unmodified 2])
    -- But modify both and no copy is made.
    equal (f (RulerUtil.Tracks [0, 1]))
        (["s1", "s1", "s2", "s2"], [("s1", xy), unmodified 2])
    -- Section winds up being the same.
    equal (f (RulerUtil.Section 1))
        (["s1", "s1", "s2", "s2"], [("s1", xy), unmodified 2])

run :: Ui.StateId z -> Ui.StateId a -> (a, Ui.State)
run make action = UiTest.run Ui.empty (make >> action)

-- | Make a block with two ruler sections.
-- TODO I also want to make sure only the section ruler id is set.
makeSections :: Ui.M m => m ()
makeSections = do
    Ui.setNamespace UiTest.test_ns
    r1 <- Create.ruler "s1" (mkruler ["s1"])
    t1 <- mktrack 1
    r2 <- Create.ruler "s2" (mkruler ["s2"])
    t2 <- mktrack 3
    UiTest.createBlock (Id.unpackId blockId) ""
        [ Block.RId r1, Block.TId t1 r1
        , Block.RId r2, Block.TId t2 r2
        ]
    Ui.setSkeleton blockId =<< UiTest.parseSkeleton blockId
    where
    blockId = UiTest.defaultBlockId
    mktrack n = Ui.createTrack
        (Id.unpackId (UiTest.mkTidBlock blockId n))
        (UiTest.emptyTrack ">")

makeNoRuler :: Ui.M m => Int -> m ()
makeNoRuler ntracks =
    void $ UiTest.mkblockRulerId Ui.noRuler
        UiTest.defaultBlockId "" (replicate ntracks (">", []))

trackRulers :: Ui.State -> [Text]
trackRulers = map Id.identName . Block.rulerIdsOf
    . map Block.tracklikeId . Block.blockTracks . head . Map.elems
    . Ui.stateBlocks

extractRulers :: Ui.State -> [(Text, [Mark.Label])]
extractRulers =
    map (bimap Id.identName extract) . Map.toList . Ui.stateRulers
    where extract = map (snd . snd) . UiTest.rulerMarks

mkruler :: [Mark.Label] -> Ruler.Ruler
mkruler labels = UiTest.mkrulerMarks
    [ (t, (0, label))
    | (t, label) <- zip (Lists.range_ 0 1) labels
    ]
