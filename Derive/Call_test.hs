module Derive.Call_test where
import qualified Data.Map as Map
import Util.Test
import qualified Util.Log as Log

import qualified Derive.CallSig as CallSig
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang
import qualified Derive.Score as Score
import qualified Derive.Call.CallTest as CallTest

import qualified Perform.Signal as Signal


test_val_call = do
    let extract = DeriveTest.extract e_event Log.msg_string
        e_event = fmap Signal.unsignal . Map.lookup (Score.Control "cont")
            . Score.event_controls
    let run evt = extract $ DeriveTest.derive_tracks_cmap cmap
            [(">", [(0, 1, "")]), ("cont", [(0, 0, evt)])]
    equal (run "foobar")
        (Right [Just []],
            ["cached generator / control: call not found: foobar"])
    equal (run "set 1")
        (Right [Just [(0, 1)]], [])
    equal (run "set (add1 1)")
        (Right [Just [(0, 2)]], [])
    equal (run "set (add1 (add1 1))")
        (Right [Just [(0, 3)]], [])
    let (res, logs) = run "set (add1 1 2)" in do
        equal res (Right [Just []])
        strings_like logs ["too many arguments"]

cmap = CallTest.add_val_call "add1" add_one CallTest.all_calls

add_one :: Derive.ValCall
add_one = Derive.ValCall "add" $ \args -> CallSig.call1 args
    (CallSig.required "v") $ \val -> return (TrackLang.VNum (val + 1))
