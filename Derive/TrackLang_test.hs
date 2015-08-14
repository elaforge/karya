-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.TrackLang_test where
import Util.Test
import qualified Derive.Environ as Environ
import qualified Derive.Parse as Parse
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang
import qualified Derive.ValType as ValType

import qualified Perform.Pitch as Pitch
import Global
import Types


test_typecheck = do
    let f :: TrackLang.Typecheck a => Text -> Either Text (Maybe a)
        f = fmap TrackLang.from_val . Parse.parse_val
    equal (f "1nn") (Right (Just (Pitch.NoteNumber 1)))
    equal (f "1nn") (Right (Just (Pitch.Nn 1)))
    equal (f "1") (Right (Just (TrackLang.DefaultDiatonic (Pitch.Diatonic 1))))
    equal (f "1") (Right (Just (Pitch.Chromatic 1)))
    equal (f "1c")
        (Right (Just (TrackLang.DefaultDiatonic (Pitch.Chromatic 1))))
    equal (f "1nn") (Right (Just (TrackLang.DefaultDiatonic (Pitch.Nn 1))))

test_map_symbol = do
    let f modify = ShowVal.show_val . TrackLang.map_symbol modify
            . expect_right "parse" . Parse.parse_expr
    -- Mostly this is testing that show_val is a proper inverse of
    -- Parse.parse_expr.
    equal (f (const "1") "23 23 '23' | 42") "1 23 '1' | 1"

test_put_val = do
    let f key val = either Just (const Nothing) . TrackLang.put_val key val
    equal (f Environ.block_end (0 :: ScoreTime) mempty) Nothing
    equal (f Environ.block_end (0 :: RealTime) mempty)
        (Just (ValType.TNum ValType.TScoreTime ValType.TAny))
    -- Don't infer that just because someone put in a positive value that
    -- the type must be positive.
    let env = TrackLang.make_environ [("k", TrackLang.to_val (1 :: Int))]
    equal (f "k" (-1 :: Int) env) Nothing
