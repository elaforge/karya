module Derive.Call.Integrate_test where
import qualified Data.Map as Map

import Util.Control
import Util.Test
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Cmd.Create as Create
import qualified Cmd.Integrate
import qualified Derive.Call.Integrate as Integrate
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale.All as Scale.All
import qualified Derive.Score as Score

import Types


test_integrate = do
    let f = first (map extract) . integrate
        integrate = Integrate.integrate lookup Nothing . map DeriveTest.mkevent
        lookup k = Map.lookup k Scale.All.scales
        extract (Integrate.Track title events) =
            (title, map UiTest.extract_event events)
        inst = Score.Instrument "inst"
    -- pprint (integrate [(0, 1, "a", [], inst)])
    equal (f [(0, 1, "a", [], inst)])
        ([(">inst", [(0, 1, "a")]), ("*twelve", [(0, 0, "4c")])], [])

test_integrate_block = do
    let simple = [("i", [(0, 1, "4c")], [])]
    equal (integrate1 id simple) $ Right
        [(">i", [(0, 1, "")]), ("*twelve", [(0, 0, "4c")])]
    -- Default tempo is cancelled out.
    equal (integrate1 (State.config#State.default_#State.tempo #= 2) simple) $
        Right [(">i", [(0, 1, "")]), ("*twelve", [(0, 0, "4c")])]


integrate1 :: (State.State -> State.State) -> [UiTest.NoteSpec]
    -> Either [String] [UiTest.TrackSpec]
integrate1 modify_ui tracks = extract <$> integrate_blocks modify_ui
    [("top", concatMap UiTest.note_spec tracks)]
    where
    -- dyn picks up the default dyn, so it's not interesting.
    extract = filter ((/="dyn") . fst) . fst

integrate_blocks :: (State.State -> State.State) -> [UiTest.BlockSpec]
    -> Either [String] ([UiTest.TrackSpec], [Skeleton.Edge])
integrate_blocks modify_ui blocks
    | not (null errs) = Left errs
    | otherwise = Right (track_specs, skel)
    where
    (bid : _, ui_state) = second modify_ui $ UiTest.run State.empty $
        UiTest.mkblocks blocks
    (tracks, errs) = integrate ui_state bid $ DeriveTest.extract_events id $
        DeriveTest.derive_block ui_state bid
    (new_block_id, state) = UiTest.run State.empty $ create_block tracks
    ((_, track_specs), skel) = UiTest.block_to_spec new_block_id state

create_block :: (State.M m) => [Integrate.Track] -> m BlockId
create_block tracks = do
    block_id <- Create.block State.no_ruler
    Cmd.Integrate.fill_block block_id tracks
    return block_id

integrate :: State.State -> BlockId -> [Score.Event]
    -> ([Integrate.Track], [String])
integrate state block_id events = Integrate.integrate lookup Nothing unwarped
    where
    lookup k = Map.lookup k Scale.All.scales
    unwarped = UiTest.eval state $ Integrate.unwarp block_id events
