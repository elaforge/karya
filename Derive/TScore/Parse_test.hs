-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
{-# LANGUAGE TypeApplications #-}
module Derive.TScore.Parse_test where
import qualified Data.Text as Text
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
            \    \">inst1\" a -- comment\n\
            \    // -- comment\n\
            \    \">inst2\" b\n\
            \]\n\
            \block2 = [c]\n"
    right_equal (f score) $
        "%meter=adi\n\
        \block1 = %block1=directive \"block1 title\"\
        \ [ >inst1 a // >inst2 b ]\n\
        \block2 = [ c ]\n"

test_p_whitespace = do
    let f = parse Parse.p_whitespace
    left_like (f "   a") "unexpected"
    right_equal (f "") ()
    right_equal (f "   ") ()
    right_equal (f " \n  \n") ()
    right_equal (f "-- hi\n") ()
    right_equal (f " -- hi\n") ()
    right_equal (f " -- hi\n   -- there") ()

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
    right_equal (Text.strip <$> second Parse.unparse (parse @a Parse.parse t)) t

test_parse = do
    roundtrip (Proxy @Id.BlockId) "block1"
    roundtrip (Proxy @Id.BlockId) "x/a"
    roundtrip (Proxy @T.Directive) "%a=b"
    let p = Proxy @T.Score
    roundtrip p "b = [ a ]"

test_track = do
    let f = fmap (map strip_token . T.track_tokens) . parse Parse.parse
    let bar = T.TBarline no_pos . T.Barline
    let rest = T.TRest no_pos . T.Rest
    right_equal (f "| ||") [bar 1, bar 2]
    right_equal (f "a") [token "" no_oct "a" no_dur]
    right_equal (f "a -- hi") [token "" no_oct "a" no_dur]
    right_equal (f "_4 | _.")
        [ rest (T.Duration (Just 4) Nothing 0 False)
        , bar 1
        , rest (T.Duration Nothing Nothing 1 False)
        ]
    right_equal (f "a b/")
        [ token "" no_oct "a" no_dur
        , token "b" no_oct "" no_dur
        ]
    right_equal (f "> \"a b\"/") [token "a b" no_oct "" no_dur]
    right_equal (f "> \"a \"() b\"/") [token "a \"() b" no_oct "" no_dur]

test_token = do
    let f = fmap strip_token . parse Parse.parse
        dur int1 int2 dots tie = T.NDuration (T.Duration int1 int2 dots tie)
    left_like (f "") "unexpected end of input"
    right_equal (f "a") $ token "" no_oct "a" no_dur
    right_equal (f "a/") $ token "a" no_oct "" no_dur
    right_equal (f "a.") $ token "" no_oct "a" (dur Nothing Nothing 1 False)
    right_equal (f "+pizz/") $ token "+pizz" no_oct "" no_dur
    right_equal (f "a/'b1.~") $
        token "a" (T.Relative 1) "b" (dur (Just 1) Nothing 1 True)
    right_equal (f "a'/a#4") $
        token "a'" no_oct "a#" (dur (Just 4) Nothing 0 False)
    right_equal (f "a0") $ token "" no_oct "a" T.CallDuration
    right_equal (f "a/a0") $ token "a" no_oct "a" T.CallDuration
    right_equal (f "a/1") $ token "a" no_oct "" (dur (Just 1) Nothing 0 False)
    right_equal (f "a1:2") $ token "" no_oct "a" (dur (Just 1) (Just 2) 0 False)
    right_equal (f "a:2") $ token "" no_oct "a" (dur Nothing (Just 2) 0 False)

test_token_roundtrip = do
    -- Lots of things can roundtrip but still not parse correctly, so this is
    -- not as good as 'test_token'.
    let p = Proxy @(T.Token T.Pitch T.NDuration T.Duration)
    roundtrip p "4a"
    roundtrip p "a."
    roundtrip p "a~"
    roundtrip p ",a"
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
no_dur = Parse.empty_duration

no_pos :: T.Pos
no_pos = T.Pos 0

token :: Text -> T.Octave -> Text -> T.NDuration
    -> T.Token T.Pitch T.NDuration T.Duration
token call oct pitch dur = T.TNote no_pos $ T.Note
    { note_call = T.Call call
    , note_pitch = T.Pitch oct pitch
    , note_zero_duration = False
    , note_duration = dur
    , note_pos = no_pos
    }

parse :: Parse.Parser a -> Text -> Either String a
parse = Parse.parse_text
