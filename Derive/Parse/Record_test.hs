-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Parse.Record_test where
import qualified Util.ParseM as ParseM
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Expr as Expr
import qualified Derive.Parse.Record as Record
import qualified Derive.ScoreT as ScoreT
import           Derive.TestInstances () -- for Eq Val

import qualified Perform.Signal as Signal

import           Global
import           Util.Test


deriving instance Eq Record.RVal

test_val :: Test
test_val = do
    let f = parseRval
    rightEqual (f "[]") $ list []
    rightEqual (f "[ 12 ]") $ list [num 12]
    rightEqual (f "[1/2 , 'hi' ]") $ list [num 0.5, str "hi"]
    rightEqual (f "{ }") $ record []
    rightEqual (f "{ a : 13 }") $ record [("a", v $ num 13)]
    rightEqual (f "[ 1\n, -- blah\n 2]") $ list [num 1, num 2]
    rightEqual (f instVal) $ record
        [ ("initialization", v $ str "Tuning")
        , ( "settings"
          , record
            [ ( "flags"
              , list $ map str ["Pressure", "HoldKeyswitch", "ResumePlay"]
              )
            , ("scale", v $ str "legong umbang")
            , ("pitch_bend_range", list $ map num [-12, 12])
            , ("control_defaults", record [("cc1", v $ num 42)])
            ]
          )
        ]

instVal :: Text
instVal =
    "{ initialization: Tuning\n\
    \, settings:\n\
    \  { flags: [Pressure, HoldKeyswitch, ResumePlay]\n\
    \  , scale: 'legong umbang'\n\
    \  , pitch_bend_range: [-12, 12]\n\
    \  , control_defaults: {cc1: 42}\n\
    \  }\n\
    \}\n"

test_val_error :: Test
test_val_error = do
    let f = parseRval
    leftLike (f badVal) "5:23:" -- carat should be under /
    where
    badVal =
        "{ initialization: Tuning\n\
        \, settings:\n\
        \  { flags: [Pressure, HoldKeyswitch, ResumePlay]\n\
        \  , scale: 'legong umbang'\n\
        \  , pitch_bend_range: /\n\
        \  , control_defaults: {cc1: 42}\n\
        \  }\n\
        \}\n"

v :: DeriveT.Val -> Record.RVal
v = Record.Val

list :: [DeriveT.Val] -> Record.RVal
list = Record.Val . DeriveT.VList

num :: Signal.Y -> DeriveT.Val
num = DeriveT.VSignal . ScoreT.untyped . Signal.constant

str :: Text -> DeriveT.Val
str = DeriveT.VStr . Expr.Str

record :: [(Text, Record.RVal)] -> Record.RVal
record = Record.Record

test_roundtrip :: Test
test_roundtrip = do
    let roundtrip t = (parseRval (mconcat (Record.unRval t)), t)
    uncurry rightEqual (roundtrip $ v $ str "hi")
    uncurry rightEqual (roundtrip $ v $ str "hi ' there")
    uncurry rightEqual (roundtrip $ v $ num 4)
    uncurry rightEqual (roundtrip $ list [str "a b", num 0])
    uncurry rightEqual (roundtrip $ record [("a", list []), ("b", record [])])

test_roundtrip_text :: Test
test_roundtrip_text = do
    let roundtrip t = (mconcat . Record.unRval <$> parseRval t, Right t)
    uncurry equal (roundtrip "4")
    uncurry equal (roundtrip "[hi]")
    uncurry equal (roundtrip "['h i']")

parseRval :: Text -> Either Text Record.RVal
parseRval = ParseM.parse Record.pRval

parseRecord :: Text -> Either Text [(Text, Record.RVal)]
parseRecord = ParseM.parse Record.pRecord
