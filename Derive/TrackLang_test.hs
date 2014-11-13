-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.TrackLang_test where
import Util.Test
import qualified Derive.Environ as Environ
import qualified Derive.Parse as Parse
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang
import Derive.TrackLang (Type(..), NumType(..), NumValue(..))

import qualified Perform.Pitch as Pitch
import Global
import Types


test_map_symbol = do
    let f modify = ShowVal.show_val . TrackLang.map_symbol modify
            . expect_right "parse" . Parse.parse_expr
    -- Mostly this is testing that show_val is a proper inverse of
    -- Parse.parse_expr.
    equal (f (const "1") "23 23 '23' | 42") "1 23 '1' | 1"

test_types_match = do
    let f t1 v2 = TrackLang.types_match t1 (to_type v2)
    equal (f (TNum TScoreTime TAny) (0 :: RealTime)) False
    equal (f (TNum TScoreTime TAny) (0 :: ScoreTime)) True
    -- equal (f (TNum TScoreTime TAny) (TNum TScoreTime TNatural)
    -- TTime has subtypes.
    equal (f (TNum TTime TAny) (-1 :: RealTime)) True
    equal (f (TNum TDefaultReal TAny) (-1 :: ScoreTime)) True
    -- NumValue also has subtypes.
    equal (f (TNum TTime TPositive) (-1 :: RealTime)) False
    equal (f (TNum TTime TPositive) (1 :: RealTime)) True

    -- TTranspose has subtypes.
    equal (f (TNum TTranspose TAny) (1 :: RealTime)) False
    equal (f (TNum TTranspose TAny) (Pitch.Chromatic 1)) True
    equal (f (TNum TTranspose TAny) (Pitch.Diatonic 1)) True
    equal (f (TNum TTranspose TAny) (Pitch.nn 1)) True
    equal (f (TNum TTranspose TAny) (Pitch.Nn 1)) True

to_type :: TrackLang.Typecheck a => a -> Type
to_type = TrackLang.type_of . TrackLang.to_val

test_put_val = do
    let f key val = either Just (const Nothing) . TrackLang.put_val key val
    equal (f Environ.block_end (0 :: ScoreTime) mempty) Nothing
    equal (f Environ.block_end (0 :: RealTime) mempty)
        (Just (TNum TScoreTime TAny))
    -- Don't infer that just because someone put in a positive value that
    -- the type must be positive.
    let env = TrackLang.make_environ [("k", TrackLang.to_val (1 :: Int))]
    equal (f "k" (-1 :: Int) env) Nothing
