module Derive.Call.Integrate_test where
import qualified Data.Map as Map

import Util.Control
import Util.Test
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Cmd.Create as Create
import qualified Cmd.Integrate
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Integrate as Integrate
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Scale.All as Scale.All
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import Types


test_integrate = do
    let f = first (map extract) . integrate
        integrate = Integrate.integrate lookup_scale lookup_attrs Nothing
        lookup_attrs = const $ Map.fromList [(Attrs.plak, "plak")]
        extract (Integrate.Track title events) =
            (title, map UiTest.extract_event events)
        inst = Score.Instrument "inst"
        event = DeriveTest.mkevent
    equal (f [event (0, 1, "a", [], inst)])
        ([(">inst", [(0, 1, "")]), ("*twelve", [(0, 0, "4c")])], [])
    -- No pitch track, has a control track.
    equal (f [(event (0, 1, "a", [("dyn", [(0, 0), (2, 1)])], inst))
            { Score.event_pitch = Pitches.signal Twelve.scale [] }])
        ( [ (">inst", [(0, 1, "")])
          , ("dyn", [(0, 0, "`0x`00"), (2, 0, "`0x`ff")])
          ]
        , []
        )
    -- Attributes get mapped back to their call.
    equal (f [(event (0, 1, "a", [], inst))
            { Score.event_attributes = Attrs.plak }])
        ( [(">inst", [(0, 1, "plak")]), ("*twelve", [(0, 0, "4c")])]
        , []
        )

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
integrate state block_id events =
    Integrate.integrate lookup_scale (const mempty) Nothing unwarped
    where
    unwarped = UiTest.eval state $ Integrate.unwarp block_id events

lookup_scale :: Pitch.ScaleId -> Maybe Scale.Scale
lookup_scale = flip Map.lookup Scale.All.scales
