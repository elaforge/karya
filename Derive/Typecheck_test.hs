-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Typecheck_test where
import Util.Test
import qualified Derive.Parse as Parse
import qualified Derive.Typecheck as Typecheck
import qualified Perform.Pitch as Pitch
import Global


test_typecheck = do
    let f :: Typecheck.Typecheck a => Text -> Either Text (Maybe a)
        f = fmap Typecheck.from_val_simple . Parse.parse_val
    equal (f "1nn") (Right (Just (Pitch.NoteNumber 1)))
    equal (f "1nn") (Right (Just (Pitch.Nn 1)))
    equal (f "1") (Right (Just (Typecheck.DefaultDiatonic (Pitch.Diatonic 1))))
    equal (f "1") (Right (Just (Pitch.Chromatic 1)))
    equal (f "1c")
        (Right (Just (Typecheck.DefaultDiatonic (Pitch.Chromatic 1))))
    equal (f "1nn") (Right (Just (Typecheck.DefaultDiatonic (Pitch.Nn 1))))
