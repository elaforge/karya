-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Play_test where
import qualified Data.Vector as Vector

import qualified Util.Log as Log
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Instrument.InstT as InstT
import qualified Solkattu.Dsl.Mridangam as M
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Play as Play
import qualified Solkattu.Tala as Tala

import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_to_state :: Test
test_to_state = do
    equal (extract <$> to_state (M.k <> M.t <> M.__ <> M.d)) $ Right
        [ (">", [(0, 0, "k"), (0.25, 0, "t"), (0.75, 0, "d")])
        , (">metronome", [(0, 0, "")])
        , ("*", [(0, 0, "3p")])
        , ("dyn", [(0, 0, "1")])
        ]

test_derive_to_disk :: Test
test_derive_to_disk = do
    let ui_state = expect_right $ to_state (M.k <> M.t)
    prettyp (UiTest.dump_blocks ui_state)
    (events, logs) <- derive ui_state
    prettyp (map Log.msg_text logs)
    let extract e = (DeriveTest.e_start_dur e, DeriveTest.e_attributes e)
    events <- return $ filter ((==Play.inst_name) . Score.event_instrument)
        events
    equal (map extract events) [((0, 0), "+ki"), ((0.25, 0), "+ta")]

derive :: Ui.State -> IO ([Score.Event], [Log.Msg])
derive ui_state = do
    cmd_state <- Play.load_cmd_state
    -- pprint $ Inst.synth_names $ Cmd.config_instrument_db $
    --     Cmd.state_config cmd_state
    block_id <- either (errorIO . pretty) return $
        Ui.eval ui_state Ui.get_root_id
    return $ first Vector.toList $ Play.derive cmd_state ui_state block_id


extract :: Ui.State -> [UiTest.TrackSpec]
extract = snd . fst . snd . head . UiTest.dump_blocks

to_state :: M.Sequence -> Either Text Ui.State
to_state =
    Play.to_state Korvai.IMridangam
        (InstT.Qualified "sampler" "mridangam-d") "" 1
    . M.korvaiS1 tala4

tala4 :: Tala.Tala
tala4 = Tala.Tala "tala4" [Tala.O, Tala.O] 0
