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
    let un = Record.unRecord . AllocRecord.unAllocation
    equal (Text.unlines (un allocation)) allocExpr

test_p_allocation :: Test
test_p_allocation = do
    let parse = AllocRecord.pAllocation allocQualified . Map.fromList
            <=< pRecord
    -- pprint (p_record alloc_expr)
    rightEqual (parse allocExpr) allocation

allocQualified :: InstT.Qualified
allocQualified = InstT.Qualified "a" "b"

allocation :: UiConfig.Allocation
allocation = UiConfig.Allocation
    { allocQualified
    , allocConfig = Common.Config
        { configEnviron = REnv.fromList
            [ ("expr", REnv.VQuoted $ Expr.generator $
                Expr.call "f" [num 1, str "a"])
            , ("num", num 42)
            , ("str", str "str")
            ]
        , configControls = Map.fromList [(Controls.dynamic, 0.25)]
        , configMute = False
        , configSolo = False
        }
    , allocBackend = UiConfig.Midi $ Patch.Config
        { configAllocation = []
        , configInitialization = Just Patch.Tuning
        , configSettings = Patch.Settings
            { configFlags = Just $ Set.fromList
                [Patch.Pressure, Patch.HoldKeyswitch, Patch.ResumePlay]
            , configScale = Nothing
            , configDecay = Just 0.5
            , configPitchBendRange = Just (-12, 12)
            , configControlDefaults = Just $
                Map.fromList [(ScoreT.Control "cc1", 42)]
            }
        }
    }

allocExpr :: Text
allocExpr =
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

pRecord :: Text -> Either Text [(Text, Record.RVal)]
pRecord = Util.Parse.parse Record.pRecord
