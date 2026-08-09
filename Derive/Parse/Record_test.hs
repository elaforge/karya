-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Parse.Record_test where
import qualified Data.Map as Map

import qualified Util.Parse
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
    let f = parse_rval
    right_equal (f "[]") $ list []
    right_equal (f "[ 12 ]") $ list [num 12]
    right_equal (f "[1/2 , 'hi' ]") $ list [num 0.5, str "hi"]
    right_equal (f "{ }") $ record []
    right_equal (f "{ a : 13 }") $ record [("a", v $ num 13)]
    right_equal (f "[ 1\n, -- blah\n 2]") $ list [num 1, num 2]
    right_equal (f inst_val) $ record
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

inst_val :: Text
inst_val =
    "{ initialization: Tuning\n\
    \, settings:\n\
    \  { flags: [Pressure, HoldKeyswitch, ResumePlay]\n\
    \  , scale: 'legong umbang'\n\
    \  , pitch_bend_range: [-12, 12]\n\
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
    let roundtrip t = (parse_rval (mconcat (Record.un_rval t)), t)
    uncurry right_equal (roundtrip $ v $ str "hi")
    uncurry right_equal (roundtrip $ v $ str "hi ' there")
    uncurry right_equal (roundtrip $ v $ num 4)
    uncurry right_equal (roundtrip $ list [str "a b", num 0])
    uncurry right_equal (roundtrip $ record [("a", list []), ("b", record [])])

test_roundtrip_text :: Test
test_roundtrip_text = do
    let roundtrip t = (mconcat . Record.un_rval <$> parse_rval t, Right t)
    uncurry equal (roundtrip "4")
    uncurry equal (roundtrip "[hi]")
    uncurry equal (roundtrip "['h i']")

parse_rval :: Text -> Either Text Record.RVal
parse_rval = Util.Parse.parse Record.p_rval

parse_record :: Text -> Either Text [(Text, Record.RVal)]
parse_record = Util.Parse.parse Record.p_record
