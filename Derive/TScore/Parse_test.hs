-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
{-# LANGUAGE TypeApplications #-}
module Derive.TScore.Parse_test where
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified GHC.Stack as Stack

import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.T as T
import qualified Derive.TScore.TScore as TScore

import qualified Instrument.InstT as InstT
import qualified Midi.Midi as Midi
import qualified Ui.Id as Id

import           Global
import           Util.Test


everything_score :: Text
everything_score =
    "-- Comments use double-dash.\n\
    \-- Toplevel directives apply to everything below them.\n\
    \-- The supported directives are in 'Check.parse_directive'.\n\
    \%meter=adi\n\
    \%instruments=''\n\
    \    >i1 a/b loop0 1 -- comments work in multi-line strings\n\
    \    >i2 c/d\n\
    \''\n\
    \%ky=''\n\
    \    x y z\n\
    \''\n\
    \-- Optional per-block directives, and quoted block title.\n\
    \block1 = %scale=sargam \"block1 title\" [\n\
    \    -- Tracks start with a track title, which should start with >. It\n\
    \    -- doesn't need quotes if there are no spaces, but if there are, the\n\
    \    -- quotes go after the >.  Ties (~) and dots (.) go at the end of\n\
    \    -- the note.\n\
    \    >inst1 4s4 r g m~ | m d n. s8 |\n\
    \        -- Pitches are by absolute octave, relative down , or up '\n\
    \        4s4 's ,s ''s |\n\
    \        -- Rests use _\n\
    \        _4. _8 s2 |\n\
    \    -- Tracks must be unique so deletes and moves can be detected.\n\
    \    -- You can put symbols after >, which will be used to make tracks\n\
    \    -- unique, but otherwise ignored.\n\
    \    >!inst1 s\n\
    \    -- Bare tie ~ to sustain previous note, bare dot . to repeat,\n\
    \    -- so this is like \"2s~ 2s | 2s 2s |\"\n\
    \    >\"inst2 | +pizz\" 2s ~ | . . |\n\
    \]\n\
    \\n\
    \-- Simple block with only one track, and one note.  kebab-case is ok.\n\
    \simple-block = [c]\n\
    \\n\
    \-- Wrapped tracks.\n\
    \wrapped = [\n\
    \    >i1 s r >i2 g m\n\
    \] [\n\
    \    >i1 p d >i2 n s\n\
    \]\n\
    \\n\
    \calls = [\n\
    \    -- Call is separated from <pitch><dur> with a /.  Pitch and dur are\n\
    \    -- optional:\n\
    \    simple-block/r2 simple-block/\n\
    \]\n\
    \\n\
    \sub-blocks = [\n\
    \    -- Any place a call can appear can also have a sub-block:\n\
    \    [ 4s1 r > 4g2 ]/1 |\n\
    \    -- The sub-block can get a transformer.  Quotes needed for spaces:\n\
    \    +pizz[s]/1 | \"+pizz | harm\"[g]/\n\
    \]\n"

test_score :: Test
test_score = do
    let f = second unparse . parse @T.Score
    let score =
            "%meter=adi\n\
            \block1 = %block1=directive \"block1 title\" [\n\
            \    >\"inst1\" a -- comment\n\
            \    -- comment\n\
            \    >inst2 b\n\
            \] [\n\
            \    >inst1 c >inst2 d\n\
            \]\n\
            \block2 = [c]\n"
    right_equal (f score) $
        "%meter=adi\n\
        \block1 = %block1=directive \"block1 title\"\
        \ [>inst1 a >inst2 b] [>inst1 c >inst2 d]\n\
        \block2 = [c]\n"

    -- Parse -> unparse -> parse -> unparse reaches a fixpoint.
    let normalized = f everything_score
    right_equal (const () <$> normalized) ()
    equal normalized (f =<< normalized)
    Text.IO.putStrLn $ either txt id normalized

test_default_call :: Test
test_default_call = do
    let f = fmap (\(T.Score defs) -> defs) . parse

    let score = "%default-call\nb = [b1/0 b2 b3]\n"
    right_equal (unparse . T.Score <$> f score) score
    right_equal (e_score_tokens <$> f score)
        [ tnote "b1" no_oct "" T.CallDuration
        , tnote "b2" no_oct "" no_dur
        , tnote "b3" no_oct "" no_dur
        ]

    let score = "b = %default-call [a > b]\n"
    right_equal (unparse . T.Score <$> f score) score
    right_equal (e_score_tokens <$> f score)
        [ tnote "a" no_oct "" no_dur
        , tnote "b" no_oct "" no_dur
        ]

    let score = "b = [b1/0 b2 b3]\n"
    right_equal (unparse . T.Score <$> f score) score
    right_equal (e_score_tokens <$> f score)
        [ tnote "b1" no_oct "" T.CallDuration
        , tnote "" no_oct "b" (idur 2)
        , tnote "" no_oct "b" (idur 3)
        ]

e_score_tokens :: [(pos, T.Toplevel)] -> [Parse.Token]
e_score_tokens defs = concat
    [ concatMap (map strip_pos . T.track_tokens) (e_tracks block)
    | T.BlockDefinition block <- map snd defs
    ]

test_p_whitespace :: Test
test_p_whitespace = do
    let f = Parse.parse_text Parse.p_whitespace
    left_like (f "   a") "unexpected"
    right_equal (f "") ()
    right_equal (f "   ") ()
    right_equal (f " \n  \n") ()
    right_equal (f "-- hi\n") ()
    right_equal (f " -- hi\n") ()
    right_equal (f " -- hi\n   -- there") ()

test_pos :: Test
test_pos = do
    let f = fmap (\(T.Score defs) -> defs) . parse
    let score =
            "%meter=adi\n\
            \block1 = %block1=directive \"block1 title\" [\n\
            \    >inst1 a b\n\
            \]\n"
    let show_pos pos = putStr $ untxt $
            T.show_error score (T.Error (T.Pos pos) "some error")
    let Right defs = f score

    show_pos 0
    show_pos 11
    equal (map fst defs) $ map T.Pos [0, 11]
    let tokens = concatMap T.track_tokens $ concat
            [ e_tracks block
            | (_, T.BlockDefinition block) <- defs
            ]
    show_pos 66
    show_pos 68
    equal (map (\t -> (T.token_pos t, unparse t)) tokens)
        [(T.Pos 66, "a"), (T.Pos 68, "b")]
    let note_of (T.TNote _ note) = Just note
        note_of _ = Nothing
    show_pos 66
    show_pos 68
    equal (map T.note_pos $ mapMaybe note_of tokens) [T.Pos 66, T.Pos 68]

test_roundtrip :: Test
test_roundtrip = do
    roundtrip (Proxy @Id.BlockId) "block1"
    roundtrip (Proxy @Id.BlockId) "x/a"
    roundtrip (Proxy @T.Directive) "%a=b"
    let p = Proxy @T.Score
    roundtrip p "b = [a]"
    roundtrip p "b = [[x y]/]"
    roundtrip p "b = [>hi a[x y]/4]"
    roundtrip p "b = [>hi \"a b\"[x y]/]"
    let p = Proxy @(T.Track T.Call)
    roundtrip p ">a b"
    roundtrip p ">!a b"
    roundtrip p ">!\"a b\" q"

test_tracks_wrapped :: Test
test_tracks_wrapped = do
    let f = fmap extract . parse
        extract (T.WrappedTracks _ wrapped) =
            map (map strip_track . T.untracks) wrapped
    right_equal (f "[a > b] [c > d]")
        [ [ T.Track "" "" [] [pnote0 "a"] no_pos
          , T.Track "" ">" [] [pnote0 "b"] no_pos
          ]
        , [ T.Track "" "" [] [pnote0 "c"] no_pos
          , T.Track "" ">" [] [pnote0 "d"] no_pos
          ]
        ]
    -- Empty tracks are ok.
    right_equal (f "[>i1 a >i2 b] [>i1 >i2]")
        [ [ T.Track "" ">i1" [] [pnote0 "a"] no_pos
          , T.Track "" ">i2" [] [pnote0 "b"] no_pos
          ]
        , [T.Track "" ">i1" [] [] no_pos, T.Track "" ">i2" [] [] no_pos]
        ]

test_tracks :: Test
test_tracks = do
    let f = fmap (map strip_track . T.untracks) . parse @(T.Tracks T.Call)
    right_equal (f "[s]") [T.Track "" "" [] [pnote0 "s"] no_pos]
    right_equal (f "[s > r]")
        [ T.Track "" "" [] [pnote0 "s"] no_pos
        , T.Track "" ">" [] [pnote0 "r"] no_pos
        ]
    right_equal (f "[>\" | u\" s >\" | t\" r]")
        [ T.Track "" "> | u" [] [pnote0 "s"] no_pos
        , T.Track "" "> | t" [] [pnote0 "r"] no_pos
        ]

test_track :: Test
test_track = do
    let parse_track = fmap extract . parse
        extract (T.Track _ title _ tokens _) = (title, map strip_pos tokens)
    let bar = T.TBarline no_pos . T.Barline
    let rest = T.TRest no_pos . T.Rest
    let title = fmap fst . parse_track
        tokens = fmap snd . parse_track
    right_equal (title "> a") ">"
    right_equal (title ">inst a") ">inst"
    right_equal (title ">\"inst | trans\" a") ">inst | trans"
    right_equal (title ">\" | trans\" a") "> | trans"

    right_equal (tokens "| ||") [bar 1, bar 2]
    right_equal (tokens "a") [tnote "" no_oct "a" no_dur]
    right_equal (tokens "a -- hi") [tnote "" no_oct "a" no_dur]
    right_equal (tokens "_4 | _.")
        [ rest (T.Duration (Just 4) Nothing 0 False)
        , bar 1
        , rest (T.Duration Nothing Nothing 1 False)
        ]
    right_equal (tokens "a b/")
        [ tnote "" no_oct "a" no_dur
        , tnote "b" no_oct "" no_dur
        ]
    right_equal (tokens "> \"a b\"/") [tnote "a b" no_oct "" no_dur]
    right_equal (tokens "> \"a \"() b\"/") [tnote "a \"() b" no_oct "" no_dur]

    right_equal (tokens "a [b]/")
        [ tnote "" no_oct "a" no_dur
        , tnote (sub "" [tnote "" no_oct "b" no_dur]) no_oct "" no_dur
        ]
    right_equal (tokens "a\\ [b]/")
        [ tnote (sub "a" [tnote "" no_oct "b" no_dur]) no_oct "" no_dur
        ]

test_track_key :: Test
test_track_key = do
    let f = fmap extract . parse @(T.Track T.Call)
        extract t = (T.track_key t, T.track_title t)
    right_equal (f ">a b") ("", ">a")
    right_equal (f ">!@#a b") ("!@#", ">a")

test_track_directives :: Test
test_track_directives = do
    let f = fmap extract . parse @(T.Track T.Call)
        extract (T.Track _ title dirs tokens _) =
            (title, map e_directive dirs, map strip_pos tokens)
    right_equal (f "> %a=b c") (">", [("a", Just "b")], [pnote0 "c"])
    right_equal (f "> %a %b=c d")
        (">", [("a", Nothing), ("b", Just "c")], [pnote0 "d"])

test_token :: Test
test_token = do
    let f = fmap strip_pos . parse
    left_like (f "") "unexpected end of input"
    right_equal (f "a") $ pnote "a" no_dur
    right_equal (f "a/") $ tnote "a" no_oct "" no_dur
    right_equal (f "a.") $ pnote "a" (dur Nothing Nothing 1 False)
    right_equal (f "+pizz/") $ tnote "+pizz" no_oct "" no_dur
    right_equal (f "a/'b1.~") $
        tnote "a" (T.Relative 1) "b" (dur (Just 1) Nothing 1 True)
    right_equal (f "a'/a#4") $
        tnote "a'" no_oct "a#" (dur (Just 4) Nothing 0 False)
    right_equal (f "a0") $ pnote "a" T.CallDuration
    right_equal (f "a/a0") $ tnote "a" no_oct "a" T.CallDuration
    right_equal (f "a/1") $ tnote "a" no_oct "" (idur 1)
    right_equal (f "a1:2") $ pnote "a" (dur (Just 1) (Just 2) 0 False)
    right_equal (f "a:2") $ pnote "a" (dur Nothing (Just 2) 0 False)
    right_equal (f "a*") $
        set_zero_dur $ pnote "a" (dur Nothing Nothing 0 False)
    -- These are treated specially by Check, but are normal notes according to
    -- the  parser.
    right_equal (f ".") $ tnote "" no_oct "" (dur Nothing Nothing 1 False)
    right_equal (f "~") $ tnote "" no_oct "" (dur Nothing Nothing 0 True)
    right_equal (f "^") $ T.TNote no_pos $
        Parse.empty_note { T.note_pitch = T.CopyFrom }

test_token_sub_block :: Test
test_token_sub_block = do
    let f = fmap strip_pos . parse
        pitch p dur = tnote "" no_oct p dur

    right_equal (f "[a]/2") $ tnote
        (sub "" [pitch "a" no_dur])
        no_oct "" (idur 2)
    right_equal (f "[a > b2]/") $ tnote
        (subs "" [[("", [pitch "a" no_dur]), (">", [pitch "b" (idur 2)])]])
        no_oct "" no_dur
    right_equal (f "[[x]/]/") $ tnote
        (sub "" [tnote (sub "" [pitch "x" no_dur]) no_oct "" no_dur])
        no_oct "" no_dur
    right_equal (f "a[b]/") $
        tnote (sub "a" [pitch "b" no_dur]) no_oct "" no_dur
    right_equal (f "\"x y\"[b]/") $
        tnote (sub "x y" [pitch "b" no_dur]) no_oct "" no_dur
    right_equal (f "a[b][c]/") $ tnote
        (subs "a" [[("", [pitch "b" no_dur])], [("", [pitch "c" no_dur])]])
        no_oct "" no_dur

    right_equal (f "a[b]\\ -- hi\n  [c]/") $ tnote
        (subs "a" [[("", [pitch "b" no_dur])], [("", [pitch "c" no_dur])]])
        no_oct "" no_dur
    right_equal (f "a\\ [b]\\ [c]/") $ tnote
        (subs "a" [[("", [pitch "b" no_dur])], [("", [pitch "c" no_dur])]])
        no_oct "" no_dur

test_token_roundtrip :: Test
test_token_roundtrip = do
    -- Lots of things can roundtrip but still not parse correctly, so this is
    -- not as good as 'test_token'.
    let p = Proxy @Parse.Token
    roundtrip p "4a"
    roundtrip p "a."
    roundtrip p "a~"
    roundtrip p ",a"
    roundtrip p "+pizz/"
    roundtrip p "\"a b\"/"
    roundtrip p "a/'b1.~"
    roundtrip p "a*4:2"
    roundtrip p "a[b]/"
    roundtrip p "a[b][c]/"
    roundtrip p "^"
    roundtrip p "^8"

test_p_multi_string :: Test
test_p_multi_string = do
    let f = Parse.parse_text Parse.p_multi_string
    right_equal (f "''hi''") "hi"
    right_equal (f "''hi'the're''") "hi'the're"
    right_equal (f "''\n\
        \  hi\n\
        \  there\n\
        \''") "hi\nthere"

test_parse_allocation :: Test
test_parse_allocation = do
    let f = Parse.parse_allocation
    right_equal (f ">i syn/p") $
        T.Allocation "i" (InstT.Qualified "syn" "p") T.Im
    let loop1 = Midi.write_device "loop1"
    right_equal (f ">i syn/ loop1 1 2") $
        T.Allocation "i" (InstT.Qualified "syn" "") $
        T.Midi [(loop1, 0), (loop1, 1)]
    left_like (f ">i syn/ loop1 0") "should be in range"
    left_like (f ">i syn/ loop1") "unexpected"
    left_like (f ">i syn/ loop1 x") "unexpected"

-- * implementation

roundtrip :: forall a. (Stack.HasCallStack, Parse.Element a)
    => Proxy a -> Text -> Test
roundtrip Proxy t =
    right_equal (Text.strip <$> second unparse (parse @a t)) t

strip_pos :: T.Token T.Call pitch ndur rdur -> T.Token T.Call pitch ndur rdur
strip_pos = \case
    T.TBarline _ a -> T.TBarline no_pos a
    T.TNote _ a -> T.TNote no_pos (strip_note a)
    T.TRest _ a -> T.TRest no_pos a
    where
    strip_note note = note
        { T.note_call = case T.note_call note of
            T.SubBlock prefix subs -> T.SubBlock prefix (map strip_tracks subs)
            call -> call
        , T.note_pos = no_pos
        }
    strip_tracks (T.Tracks tracks) = T.Tracks $ map strip_track tracks

e_tracks :: T.Block T.WrappedTracks -> [T.Track T.Call]
e_tracks = T.untracks . expect_right . TScore.unwrap_tracks . T.block_tracks

strip_track :: T.Track T.Call -> T.Track T.Call
strip_track track = track
    { T.track_tokens = map strip_pos (T.track_tokens track)
    , T.track_pos = no_pos
    }

e_directive :: T.Directive -> (Text, Maybe Text)
e_directive (T.Directive _ k v) = (k, v)

no_oct :: T.Octave
no_oct = T.Relative 0

no_dur :: T.NDuration
no_dur = Parse.empty_nduration

dur :: Maybe Int -> Maybe Int -> Int -> Bool -> T.NDuration
dur int1 int2 dots tie = T.NDuration (T.Duration int1 int2 dots tie)

idur :: Int -> T.NDuration
idur int1 = dur (Just int1) Nothing 0 False

no_pos :: T.Pos
no_pos = T.Pos 0

tracks :: [(Text, [Parse.Token])] -> T.Tracks T.Call
tracks ts = T.Tracks [T.Track "" title [] tokens no_pos | (title, tokens) <- ts]

tnote :: T.Call -> T.Octave -> Text -> T.NDuration -> Parse.Token
tnote call oct pitch dur = T.TNote no_pos $ T.Note
    { note_call = call
    , note_pitch = T.NPitch $ T.Pitch oct pitch
    , note_zero_duration = False
    , note_duration = dur
    , note_pos = no_pos
    }

pnote :: Text -> T.NDuration -> Parse.Token
pnote p dur = tnote "" no_oct p dur

pnote0 :: Text -> Parse.Token
pnote0 p = tnote "" no_oct p no_dur

sub :: T.CallText -> [Parse.Token] -> T.Call
sub prefix t = subs prefix [[("", t)]]

subs :: T.CallText -> [[(Text, [Parse.Token])]] -> T.Call
subs prefix ts = T.SubBlock prefix (map tracks ts)

set_zero_dur :: T.Token call pitch ndur rdur -> T.Token call pitch ndur rdur
set_zero_dur = \case
    T.TNote pos note -> T.TNote pos $ note { T.note_zero_duration = True }
    tnote -> tnote

parse :: Parse.Element a => Text -> Either String a
parse = Parse.parse_text (Parse.parse Parse.default_config)

unparse :: Parse.Element a => a -> Text
unparse = Parse.unparse Parse.default_config
