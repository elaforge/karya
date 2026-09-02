-- Copyright 2026 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.Parse.AllocRecord_test where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Parse
import qualified Derive.Controls as Controls
import qualified Derive.Expr as Expr
import qualified Derive.Parse.AllocRecord as AllocRecord
import qualified Derive.Parse.Record as Record
import qualified Derive.REnv as REnv
import qualified Derive.ScoreT as ScoreT

import qualified Instrument.Common as Common
import qualified Instrument.InstT as InstT
import qualified Perform.Midi.Patch as Patch
import qualified Ui.UiConfig as UiConfig

import           Global
import           Util.Test


test_un_allocation :: Test
test_un_allocation = do
    let un = Record.un_record . AllocRecord.un_allocation
    equal (Text.unlines (un allocation)) alloc_expr

test_p_allocation :: Test
test_p_allocation = do
    let parse = AllocRecord.p_allocation alloc_qualified . Map.fromList
            <=< p_record
    -- pprint (p_record alloc_expr)
    right_equal (parse alloc_expr) allocation

alloc_qualified :: InstT.Qualified
alloc_qualified = InstT.Qualified "a" "b"

allocation :: UiConfig.Allocation
allocation = UiConfig.Allocation
    { alloc_qualified
    , alloc_config = Common.Config
        { config_environ = REnv.from_list
            [ ("expr", REnv.VQuoted $ Expr.generator $
                Expr.call "f" [num 1, str "a"])
            , ("num", num 42)
            , ("str", str "str")
            ]
        , config_controls = Map.fromList [(Controls.dynamic, 0.25)]
        , config_mute = False
        , config_solo = False
        }
    , alloc_backend = UiConfig.Midi $ Patch.Config
        { config_allocation = []
        , config_initialization = Just Patch.Tuning
        , config_settings = Patch.Settings
            { config_flags = Just $ Set.fromList
                [Patch.Pressure, Patch.HoldKeyswitch, Patch.ResumePlay]
            , config_scale = Nothing
            , config_decay = Just 0.5
            , config_pitch_bend_range = Just (-12, 12)
            , config_control_defaults = Just $
                Map.fromList [(ScoreT.Control "cc1", 42)]
            }
        }
    }

alloc_expr :: Text
alloc_expr =
    "{ env:\n\
    \    { expr: \"(f 1 a)\n\
    \    , num: 42\n\
    \    , str: str\n\
    \    }\n\
    \, controls: {dyn: .25}\n\
    \, initialization: Tuning\n\
    \, flags: [Pressure, HoldKeyswitch, ResumePlay]\n\
    \, decay: .5s\n\
    \, pb_range: [-12, 12]\n\
    \, control_defaults: {cc1: 42}\n\
    \}\n"

num :: Double -> REnv.Val
num = REnv.VNum . ScoreT.untyped

str :: Text -> REnv.Val
str = REnv.VStr . Expr.Str

p_record :: Text -> Either Text [(Text, Record.RVal)]
p_record = Util.Parse.parse Record.p_record
