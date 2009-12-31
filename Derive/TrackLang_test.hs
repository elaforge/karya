module Derive.TrackLang_test where

import Util.Test

import qualified Derive.Score as Score
import Derive.TrackLang (Val(..), AttrMode(..))
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


test_extract = do
    let sig0 :: (TrackLang.Arg Double, TrackLang.Arg Double)
        sig0 = (TrackLang.Arg "mandatory" Nothing,
            TrackLang.Arg "optional" (Just 42))
        f args sig = map_left TrackLang.show_type_error
            (TrackLang.extract2 args sig)

    left_like (f [] sig0) "too few arguments"
    equal (f [VNum 1] sig0) (Right (1, 42))
    equal (f [VNum 1, VNum 2] sig0) (Right (1, 2))
    left_like (f [VMethod (TrackLang.Method "ho")] sig0)
        "0/mandatory: expected num"
    left_like (f [VNum 1, VNum 2, VNum 3] sig0)
        "too many arguments"
    left_like (f [VNum 1] (snd sig0, fst sig0))
        "required args can't follow an optional one"

test_parse = do
    let mkcall = VCall . TrackLang.CallId
        mkattr = VAttr . TrackLang.Attr
        mksig = VSignal . TrackLang.Signal
    let f = either (const Nothing) Just . TrackLang.parse
    equal (f "") (Just ([], []))
    equal (f "call *note >inst m'meth' .45 +attr -- comment") $
        Just ([mkcall "call", VNote (Pitch.Note "note"),
            VInstrument (Just (Score.Instrument "inst")),
            VMethod (TrackLang.Method "meth"),
            VNum 0.45, mkattr (Add, "attr")], [])
    equal (f "c1 | c2 | c3") $
        Just ([mkcall "c1"], [[mkcall "c2"], [mkcall "c3"]])
    equal (f "+a -b =c =") $
        Just ([mkattr (Add, "a"), mkattr (Remove, "b"), mkattr (Set, "c"),
            mkattr (Clear, "")],
            [])
    equal (f ">synth/inst > >a-b") $
        Just ([VInstrument (Just (Score.Instrument "synth/inst")),
            VInstrument Nothing,
            VInstrument (Just (Score.Instrument "a-b"))], [])

    equal (f "bad/ident") Nothing
    equal (f "+bad/attr") Nothing
    equal (f "+ foo") Nothing

    equal (f "%sig %sig2,.4") $
        Just ([mksig (Nothing, Just (Score.Control "sig")),
            mksig  (Just 0.4, Just (Score.Control "sig2"))], [])
