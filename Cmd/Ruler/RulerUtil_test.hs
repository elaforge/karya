-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Ruler.RulerUtil_test where
import qualified Data.Map as Map

import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import qualified Cmd.Create as Create
import qualified Cmd.Ruler.RulerUtil as RulerUtil
import Global


test_modify = do
    let f with_ruler scope =
            extract $ snd $ run make $
                RulerUtil.modify scope UiTest.default_block_id
                    (const $ Right $ mk_ruler ["X", "Y"])
            where make = if with_ruler then make_sections else make_no_ruler 2
        extract st = (track_rulers st, extract_rulers st)
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

test_local = do
    let f scope = extract $ snd $ run make_sections $
            RulerUtil.local scope UiTest.default_block_id
                (const $ Right $ mk_ruler ["X", "Y"])
        extract st = (track_rulers st, extract_rulers st)
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
make_sections :: Ui.M m => m ()
make_sections = do
    Ui.set_namespace UiTest.test_ns
    r1 <- Create.ruler "s1" (mk_ruler ["s1"])
    t1 <- mktrack 1
    r2 <- Create.ruler "s2" (mk_ruler ["s2"])
    t2 <- mktrack 3
    UiTest.create_block (Id.unpack_id block_id) ""
        [ Block.RId r1, Block.TId t1 r1
        , Block.RId r2, Block.TId t2 r2
        ]
    Ui.set_skeleton block_id =<< UiTest.parse_skeleton block_id
    where
    block_id = UiTest.default_block_id
    mktrack n = Ui.create_track
        (Id.unpack_id (UiTest.mk_tid_block block_id n))
        (UiTest.empty_track ">")

make_no_ruler :: Ui.M m => Int -> m ()
make_no_ruler ntracks =
    void $ UiTest.mkblock_ruler Ui.no_ruler
        UiTest.default_block_id "" (replicate ntracks (">", []))

run_marks :: Int -> Maybe [Mark] -> Ui.StateId a -> (a, Ui.State)
run_marks ntracks marks action = UiTest.run Ui.empty $ do
    case marks of
        Nothing -> UiTest.mkblock_ruler Ui.no_ruler
            UiTest.default_block_id "" tracks
        Just marks ->
            UiTest.mkblock_marklist (mk_marklist marks)
                UiTest.default_block_id "" tracks
    action
    where tracks = replicate ntracks (">", [])

type Mark = Text

track_rulers :: Ui.State -> [Text]
track_rulers = map Id.ident_name . Block.ruler_ids_of
    . map Block.tracklike_id . Block.block_tracks . head . Map.elems
    . Ui.state_blocks

extract_rulers :: Ui.State -> [(Text, [Text])]
extract_rulers =
    map (Id.ident_name *** extract) . Map.toList . Ui.state_rulers
    where
    extract ruler = map (Ruler.mark_name . snd) (Ruler.ascending 0 mlist)
        where (_config, mlist) = Ruler.get_meter ruler

extract_mark :: Ruler.PosMark -> Mark
extract_mark (_, m) = Ruler.mark_name m

mk_ruler :: [Mark] -> Ruler.Ruler
mk_ruler = Ruler.meter_ruler Ruler.default_config . mk_marklist

mk_marklist :: [Mark] -> Ruler.Marklist
mk_marklist marks = Ruler.marklist
    [(t, Ruler.null_mark { Ruler.mark_name = label }) | (t, label)
        <- zip (Seq.range_ 0 1) marks]
