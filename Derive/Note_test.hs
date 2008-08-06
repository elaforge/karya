module Derive.Note_test where
import qualified Text.ParserCombinators.Parsec as P

import Util.Test

import qualified Ui.TestSetup as TestSetup

import qualified Derive.Derive as Derive
import qualified Derive.Note as Note

import qualified Derive.Derive_test as Derive_test

import qualified Perform.Signal as Signal
import qualified Perform.Pitch as Pitch

import qualified Derive.Twelve as Twelve

-- * derivers

derive = Derive_test.derive

test_d_instrument_track = do
    let d tid = Note.d_note_track (Note.scale_parser scale) tid
    let (tids, state) = TestSetup.run_mkstate
            [ ("0", [(0, 10, "5a-"), (10, 10, "5b-"), (20, 10, "5c-")])
            , ("1", [(0, 10, ".1"), (10, 10, ".2"), (20, 10, ".4")])
            ]
    print tids
    let (val, tempo, inv_tempo, logs) = derive state (d (head tids))
    mapM_ pprint val

-- * parse

scale = Twelve.twelve_scale

test_note = do
    print $ P.parse (Note.p_pitch_segment scale) "" "5e-"

test_parse = do
    -- let parse = either (const Nothing) Just . P.parse (Note.p_note scale) ""
    let parse = Note.parse_note scale

    -- Not a scale, so it looks like a block.
    equal (parse "7q#") $ Right (Nothing, Just "7q#")
    -- But in this case the abc is already a block...
    equal (parse "7q# abc") (Left "expected scale degree")

    equal (parse "i 7c#") $ Right
        ( Just (Signal.Linear, Pitch.Pitch "7c#" (Pitch.NoteNumber 85.0))
        , Nothing)
    equal (parse "2.1e 7c#") $ Right
        ( Just (Signal.Exp 2.1, Pitch.Pitch "7c#" (Pitch.NoteNumber 85.0))
        , Nothing)

    equal (parse "i 7c# <block") $ Right
        ( Just (Signal.Linear, Pitch.Pitch "7c#" (Pitch.NoteNumber 85.0))
        , Just "<block")

    -- But if it has a method, it must match the scale:
    equal (parse "i block") (Left "expected scale degree")

    equal (parse "i 7q block") (Left "expected [method, scale]")
