module Derive.Scale.Twelve_test where

import Util.Test

import qualified Perform.Pitch as Pitch
import qualified Derive.Scale.Twelve as Twelve

n = Pitch.Note

test_note_to_nn = do
    let f = Twelve.note_to_nn . n
    equal (f "4c") (Just 60)
    equal (f "-1c") (Just 0)
    equal (f "-2b") Nothing
    equal (f "0c") (Just 12)
    equal (f "9g") (Just 127)
    equal (f "9g#") Nothing

test_transpose = do
    let f n note = Twelve.transpose n (Pitch.Note note)
    equal (f 0 "blah") (Left Pitch.NotInScale)
    equal (f 900 "4c") (Left Pitch.OutOfRange)
    equal (f 0 "4c") (Right (n "4c"))
    equal (f 1 "4c") (Right (n "4c#"))
    equal (f (-1) "4c") (Right (n "3b"))
    equal (f 12 "4c") (Right (n "5c"))
    equal (f 1.5 "4c") (Right (n "4c#+50"))
    equal (f 1.5 "4c+50") (Right (n "4d"))
    equal (f 1.5 "4c+50,10") (Right (n "4d,10"))
