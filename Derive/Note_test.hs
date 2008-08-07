module Derive.Note_test where

import Util.Test

import qualified Ui.TestSetup as TestSetup

import qualified Derive.Note as Note
import qualified Derive.Scale as Scale

import qualified Derive.Derive_test as Derive_test

import qualified Perform.Signal as Signal
import qualified Perform.Pitch as Pitch

import qualified Derive.Twelve as Twelve

-- * derivers

derive = Derive_test.derive

test_d_instrument_track = do
    let d tid = Note.d_note_track Scale.scale_map (Note.scale_parser scale) tid
    let (tids, state) = TestSetup.run_mkstate
            [ ("0", [(0, 10, "5a-"), (10, 10, "5b-"), (20, 10, "5c-")])
            , ("1", [(0, 10, ".1"), (10, 10, ".2"), (20, 10, ".4")])
            ]
    print tids
    let (val, _tempo, _inv_tempo, logs) = derive state (d (head tids))
    mapM_ pprint val

-- * parse

scale = Twelve.scale

test_parse = do
    -- let parse = either (const Nothing) Just . P.parse (Note.p_note scale) ""
    let parse = Note.parse_note scale
        sid = Pitch.scale_id scale

    -- Not a scale, so it looks like a block.
    equal (parse "7q#") $ Right (Nothing, Just "7q#")
    -- But in this case the abc is already a block...
    equal (parse "7q# abc") (Left "expected scale degree, got \"7q#\"")

    equal (parse "i 7c#") $ Right
        ( Just (Signal.Linear, Pitch.Pitch sid (Pitch.Note "7c#"))
        , Nothing)
    equal (parse "2.1e 7c#") $ Right
        ( Just (Signal.Exp 2.1, Pitch.Pitch sid (Pitch.Note "7c#"))
        , Nothing)

    equal (parse "i 7c# <block") $ Right
        ( Just (Signal.Linear, Pitch.Pitch sid (Pitch.Note "7c#"))
        , Just "<block")

    -- But if it has a method, it must match the scale:
    equal (parse "i block") (Left "expected scale degree, got \"block\"")

    equal (parse "i 7q block")
        (Left "expected [method, scale], got [\"i\",\"7q\"]")
