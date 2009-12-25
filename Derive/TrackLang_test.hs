module Derive.TrackLang_test where

import Util.Test

import qualified Derive.Score as Score
import Derive.TrackLang (Val(..), AttrMode(..), Arg(..), Type(..))
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


test_typecheck = do
    let f = TrackLang.typecheck
    strings_like (f [Arg TNum (Just (Num 42)), Arg TNum Nothing] [])
        ["required arg can't follow optional one"]
    strings_like (f [Arg TNum Nothing] [])
        ["too few arguments"]
    strings_like (f [] [Num 42])
        ["too many arguments"]
    strings_like (f [Arg TNum (Just (Call "c"))] [Num 42])
        ["expected type doesn't match default"]
    strings_like (f [Arg TNum (Just (Num 32))] [Call "c"])
        ["expected type doesn't match given"]
    strings_like (f [Arg TNum (Just (Num 32))] [])
        []
    strings_like (f [Arg TNum Nothing] [Num 42])
        []

test_parse = do
    let f = either (const Nothing) Just . TrackLang.parse
    equal (f "") (Just ([], []))
    equal (f "call *note >inst m'meth' .45 +attr -- comment") $
        Just ([Call "call", Note (Pitch.Note "note"),
            Instrument (Just (Score.Instrument "inst")), Method "meth",
            Num 0.45, Attr Add "attr"], [])
    equal (f "c1 | c2 | c3") $
        Just ([Call "c1"], [[Call "c2"], [Call "c3"]])
    equal (f "+a -b =c =") $
        Just ([Attr Add "a", Attr Remove "b", Attr Set "c", Attr Clear ""],
            [])
    equal (f ">synth/inst > >a-b") $
        Just ([Instrument (Just (Score.Instrument "synth/inst")),
            Instrument Nothing, Instrument (Just (Score.Instrument "a-b"))], [])

    equal (f "bad/ident") Nothing
    equal (f "+bad/attr") Nothing
    equal (f "+ foo") Nothing
