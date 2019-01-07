-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
{-# LANGUAGE TypeApplications #-}
module Derive.TScore.Parse_test where
import qualified GHC.Stack as Stack

import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.T as T
import qualified Ui.Id as Id

import           Global
import           Util.Test


test_score = do
    let f = second Parse.unparse . parse @T.Score Parse.parse
    let score =
            "%meter=adi\n\
            \block1 = %block1=directive \"block1 title\" [\n\
            \    \">inst1\" a\n\
            \    //\n\
            \    \">inst2\" b\n\
            \]\n\
            \block2 = [c]\n"
    right_equal (f score) $
        "%meter=adi\n\
        \block1 = %block1=directive \"block1 title\"\
        \ [ >inst1 a // >inst2 b ]\n\
        \block2 = [ c ]\n"

test_pos = do
    let f = fmap (\(T.Score defs) -> defs) . parse Parse.parse
    let score =
            "%meter=adi\n\
            \block1 = %block1=directive \"block1 title\" [\n\
            \    \">inst1\" a b\n\
            \]\n"
    let show_pos pos = putStr $ untxt $
            T.show_error score (T.Error (T.Pos pos) "some error")
    let Right defs = f score

    show_pos 0
    show_pos 11
    equal (map fst defs) $ map T.Pos [0, 11]
    let untracks (T.Tracks a) = a
    let tokens = concatMap T.track_tokens $ concat
            [ untracks $ T.block_tracks block
            | (_, T.BlockDefinition block) <- defs
            ]
    show_pos 68
    show_pos 70
    equal (map (\t -> (T.token_pos t, Parse.unparse t)) tokens)
        [(T.Pos 68, "a"), (T.Pos 70, "b")]


roundtrip :: forall a. (Stack.HasCallStack, Parse.Element a)
    => Proxy a -> Text -> IO Bool
roundtrip Proxy t =
    right_equal (second Parse.unparse (parse @a Parse.parse t)) t

test_parse = do
    roundtrip (Proxy @Id.BlockId) "block1"
    roundtrip (Proxy @Id.BlockId) "x/a"
    roundtrip (Proxy @T.Directive) "%a=b"

test_track = do
    let f = fmap (map strip_token . T.track_tokens) . parse Parse.parse
    let bar = T.TBarline no_pos . T.Barline
    let rest = T.TRest no_pos . T.Rest

    right_equal (f "| ||") [bar 1, bar 2]
    right_equal (f "a") [token "" no_oct "a" no_dur]
    right_equal (f "a -- hi") [token "" no_oct "a" no_dur]
    right_equal (f "_4 | _.")
        [ rest (T.Duration (Just 4) 0 False)
        , bar 1
        , rest (T.Duration Nothing 1 False)
        ]
    right_equal (f "a b/")
        [ token "" no_oct "a" no_dur
        , token "b" no_oct "" no_dur
        ]
    right_equal (f "> \"a b\"/") [token "a b" no_oct "" no_dur]
    right_equal (f "> \"a \"() b\"/") [token "a \"() b" no_oct "" no_dur]

test_token = do
    let f = fmap strip_token . parse Parse.parse
        dur i dots tie = T.NDuration (T.Duration i dots tie)
    left_like (f "") "unexpected end of input"
    right_equal (f "a") $ token "" no_oct "a" no_dur
    right_equal (f "+pizz/") $ token "+pizz" no_oct "" no_dur
    right_equal (f "a/'b1.~") $
        token "a" (T.Relative 1) "b" (dur (Just 1) 1 True)
    right_equal (f "a'/a#4") $
        token "a'" no_oct "a#" (dur (Just 4) 0 False)
    right_equal (f "a0") $ token "" no_oct "a" T.CallDuration
    right_equal (f "a/a0") $ token "a" no_oct "a" T.CallDuration
    right_equal (f "a/1") $ token "a" no_oct "" (dur (Just 1) 0 False)

test_token_roundtrip = do
    let p = Proxy @(T.Token T.Pitch T.NDuration T.Duration)
    roundtrip p "a"
    roundtrip p "+pizz/"
    roundtrip p "\"a b\"/"
    roundtrip p "a/'b1.~"

-- * implementation

strip_token :: T.Token pitch ndur rdur -> T.Token pitch ndur rdur
strip_token = \case
    T.TBarline _ a -> T.TBarline no_pos a
    T.TNote _ a -> T.TNote no_pos (a { T.note_pos = no_pos })
    T.TRest _ a -> T.TRest no_pos a

no_oct :: T.Octave
no_oct = T.Relative 0

no_dur :: T.NDuration
no_dur = T.NDuration $ T.Duration Nothing 0 False

no_pos :: T.Pos
no_pos = T.Pos 0

token :: Text -> T.Octave -> Text -> T.NDuration
    -> T.Token T.Pitch T.NDuration T.Duration
token call oct pitch dur = T.TNote no_pos $
    T.Note (T.Call call) (T.Pitch oct pitch) dur no_pos

parse :: Parse.Parser a -> Text -> Either String a
parse = Parse.parse_text
