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

import qualified Ui.Id as Id

import           Global
import           Util.Test


everythingScore :: Text
everythingScore =
    "-- Comments use double-dash.\n\
    \-- Toplevel directives apply to everything below them.\n\
    \-- The supported directives are in 'Check.parse_directive'.\n\
    \%meter=adi\n\
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
    rightEqual (f score) $
        "%meter = adi\n\
        \block1 = %block1=directive \"block1 title\"\
        \ [>inst1 a >inst2 b] [>inst1 c >inst2 d]\n\
        \block2 = [c]\n"

    -- Parse -> unparse -> parse -> unparse reaches a fixpoint.
    let normalized = f everythingScore
    rightEqual (const () <$> normalized) ()
    equal normalized (f =<< normalized)
    Text.IO.putStrLn $ either txt id normalized

test_default_call :: Test
test_default_call = do
    let f = fmap (\(T.Score defs) -> defs) . parse

    let score = "%default-call\nb = [b1/0 b2 b3]\n"
    rightEqual (unparse . T.Score <$> f score) score
    rightEqual (eScoreTokens <$> f score)
        [ tnote "b1" noOct "" T.CallDuration
        , tnote "b2" noOct "" noDur
        , tnote "b3" noOct "" noDur
        ]

    let score = "b = %default-call [a > b]\n"
    rightEqual (unparse . T.Score <$> f score) score
    rightEqual (eScoreTokens <$> f score)
        [ tnote "a" noOct "" noDur
        , tnote "b" noOct "" noDur
        ]

    let score = "b = [b1/0 b2 b3]\n"
    rightEqual (unparse . T.Score <$> f score) score
    rightEqual (eScoreTokens <$> f score)
        [ tnote "b1" noOct "" T.CallDuration
        , tnote "" noOct "b" (idur 2)
        , tnote "" noOct "b" (idur 3)
        ]

eScoreTokens :: [(pos, T.Toplevel)] -> [Parse.Token]
eScoreTokens defs = concat
    [ concatMap (map stripPos . T.trackTokens) (eTracks block)
    | T.BlockDefinition block <- map snd defs
    ]

test_p_whitespace :: Test
test_p_whitespace = do
    let f = Parse.parseText Parse.pWhitespace
    leftLike (f "   a") "unexpected"
    rightEqual (f "") ()
    rightEqual (f "   ") ()
    rightEqual (f " \n  \n") ()
    rightEqual (f "-- hi\n") ()
    rightEqual (f " -- hi\n") ()
    rightEqual (f " -- hi\n   -- there") ()

test_pos :: Test
test_pos = do
    let f = fmap (\(T.Score defs) -> defs) . parse
    let score =
            "%meter=adi\n\
            \block1 = %block1=directive \"block1 title\" [\n\
            \    >inst1 a b\n\
            \]\n"
    let showPos pos = putStr $ untxt $
            T.showError score (T.Error (T.Pos pos) "some error")
    let Right defs = f score

    showPos 0
    showPos 11
    equal (map fst defs) $ map T.Pos [0, 11]
    let tokens = concatMap T.trackTokens $ concat
            [ eTracks block
            | (_, T.BlockDefinition block) <- defs
            ]
    showPos 66
    showPos 68
    equal (map (\t -> (T.tokenPos t, unparse t)) tokens)
        [(T.Pos 66, "a"), (T.Pos 68, "b")]
    let noteOf (T.TNote _ note) = Just note
        noteOf _ = Nothing
    showPos 66
    showPos 68
    equal (map T.notePos $ mapMaybe noteOf tokens) [T.Pos 66, T.Pos 68]

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
            map (map stripTrack . T.untracks) wrapped
    rightEqual (f "[a > b] [c > d]")
        [ [ T.Track "" "" [] [pnote0 "a"] noPos
          , T.Track "" ">" [] [pnote0 "b"] noPos
          ]
        , [ T.Track "" "" [] [pnote0 "c"] noPos
          , T.Track "" ">" [] [pnote0 "d"] noPos
          ]
        ]
    -- Empty tracks are ok.
    rightEqual (f "[>i1 a >i2 b] [>i1 >i2]")
        [ [ T.Track "" ">i1" [] [pnote0 "a"] noPos
          , T.Track "" ">i2" [] [pnote0 "b"] noPos
          ]
        , [T.Track "" ">i1" [] [] noPos, T.Track "" ">i2" [] [] noPos]
        ]

test_tracks :: Test
test_tracks = do
    let f = fmap (map stripTrack . T.untracks) . parse @(T.Tracks T.Call)
    rightEqual (f "[s]") [T.Track "" "" [] [pnote0 "s"] noPos]
    rightEqual (f "[s > r]")
        [ T.Track "" "" [] [pnote0 "s"] noPos
        , T.Track "" ">" [] [pnote0 "r"] noPos
        ]
    rightEqual (f "[>\" | u\" s >\" | t\" r]")
        [ T.Track "" "> | u" [] [pnote0 "s"] noPos
        , T.Track "" "> | t" [] [pnote0 "r"] noPos
        ]

test_track :: Test
test_track = do
    let parseTrack = fmap extract . parse
        extract (T.Track _ title _ tokens _) = (title, map stripPos tokens)
    let bar = T.TBarline noPos . T.Barline
    let rest = T.TRest noPos . T.Rest
    let title = fmap fst . parseTrack
        tokens = fmap snd . parseTrack
    rightEqual (title "> a") ">"
    rightEqual (title ">inst a") ">inst"
    rightEqual (title ">\"inst | trans\" a") ">inst | trans"
    rightEqual (title ">\" | trans\" a") "> | trans"

    rightEqual (tokens "| ||") [bar 1, bar 2]
    rightEqual (tokens "a") [tnote "" noOct "a" noDur]
    rightEqual (tokens "a -- hi") [tnote "" noOct "a" noDur]
    rightEqual (tokens "_4 | _.")
        [ rest (T.Duration (Just 4) Nothing 0 False)
        , bar 1
        , rest (T.Duration Nothing Nothing 1 False)
        ]
    rightEqual (tokens "a b/")
        [ tnote "" noOct "a" noDur
        , tnote "b" noOct "" noDur
        ]
    rightEqual (tokens "> \"a b\"/") [tnote "a b" noOct "" noDur]
    rightEqual (tokens "> \"a \"() b\"/") [tnote "a \"() b" noOct "" noDur]

    rightEqual (tokens "a [b]/")
        [ tnote "" noOct "a" noDur
        , tnote (sub "" [tnote "" noOct "b" noDur]) noOct "" noDur
        ]
    rightEqual (tokens "a\\ [b]/")
        [ tnote (sub "a" [tnote "" noOct "b" noDur]) noOct "" noDur
        ]

test_track_key :: Test
test_track_key = do
    let f = fmap extract . parse @(T.Track T.Call)
        extract t = (T.trackKey t, T.trackTitle t)
    rightEqual (f ">a b") ("", ">a")
    rightEqual (f ">!@#a b") ("!@#", ">a")

test_track_directives :: Test
test_track_directives = do
    let f = fmap extract . parse @(T.Track T.Call)
        extract (T.Track _ title dirs tokens _) =
            (title, map eDirective dirs, map stripPos tokens)
    rightEqual (f "> %a=b c") (">", [("a", Just "b")], [pnote0 "c"])
    rightEqual (f "> %a %b=c d")
        (">", [("a", Nothing), ("b", Just "c")], [pnote0 "d"])

test_token :: Test
test_token = do
    let f = fmap stripPos . parse
    leftLike (f "") "unexpected end of input"
    rightEqual (f "a") $ pnote "a" noDur
    rightEqual (f "a/") $ tnote "a" noOct "" noDur
    rightEqual (f "a.") $ pnote "a" (dur Nothing Nothing 1 False)
    rightEqual (f "+pizz/") $ tnote "+pizz" noOct "" noDur
    rightEqual (f "a/'b1.~") $
        tnote "a" (T.Relative 1) "b" (dur (Just 1) Nothing 1 True)
    rightEqual (f "a'/a#4") $
        tnote "a'" noOct "a#" (dur (Just 4) Nothing 0 False)
    rightEqual (f "a0") $ pnote "a" T.CallDuration
    rightEqual (f "a/a0") $ tnote "a" noOct "a" T.CallDuration
    rightEqual (f "a/1") $ tnote "a" noOct "" (idur 1)
    rightEqual (f "a1:2") $ pnote "a" (dur (Just 1) (Just 2) 0 False)
    rightEqual (f "a:2") $ pnote "a" (dur Nothing (Just 2) 0 False)
    rightEqual (f "a*") $
        setZeroDur $ pnote "a" (dur Nothing Nothing 0 False)
    -- These are treated specially by Check, but are normal notes according to
    -- the  parser.
    rightEqual (f ".") $ tnote "" noOct "" (dur Nothing Nothing 1 False)
    rightEqual (f "~") $ tnote "" noOct "" (dur Nothing Nothing 0 True)
    rightEqual (f "^") $ T.TNote noPos $
        Parse.emptyNote { T.notePitch = T.CopyFrom }

test_token_sub_block :: Test
test_token_sub_block = do
    let f = fmap stripPos . parse
        pitch p dur = tnote "" noOct p dur

    rightEqual (f "[a]/2") $ tnote
        (sub "" [pitch "a" noDur])
        noOct "" (idur 2)
    rightEqual (f "[a > b2]/") $ tnote
        (subs "" [[("", [pitch "a" noDur]), (">", [pitch "b" (idur 2)])]])
        noOct "" noDur
    rightEqual (f "[[x]/]/") $ tnote
        (sub "" [tnote (sub "" [pitch "x" noDur]) noOct "" noDur])
        noOct "" noDur
    rightEqual (f "a[b]/") $
        tnote (sub "a" [pitch "b" noDur]) noOct "" noDur
    rightEqual (f "\"x y\"[b]/") $
        tnote (sub "x y" [pitch "b" noDur]) noOct "" noDur
    rightEqual (f "a[b][c]/") $ tnote
        (subs "a" [[("", [pitch "b" noDur])], [("", [pitch "c" noDur])]])
        noOct "" noDur

    rightEqual (f "a[b]\\ -- hi\n  [c]/") $ tnote
        (subs "a" [[("", [pitch "b" noDur])], [("", [pitch "c" noDur])]])
        noOct "" noDur
    rightEqual (f "a\\ [b]\\ [c]/") $ tnote
        (subs "a" [[("", [pitch "b" noDur])], [("", [pitch "c" noDur])]])
        noOct "" noDur

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
    let f = Parse.parseText Parse.pMultiString
    rightEqual (f "''hi''") "hi"
    rightEqual (f "''hi'the're''") "hi'the're"
    rightEqual (f "''\n\
        \  hi\n\
        \  there\n\
        \''") "hi\nthere"

-- * implementation

roundtrip :: forall a. (Stack.HasCallStack, Parse.Element a)
    => Proxy a -> Text -> Test
roundtrip Proxy t =
    rightEqual (Text.strip <$> second unparse (parse @a t)) t

stripPos :: T.Token T.Call pitch ndur rdur -> T.Token T.Call pitch ndur rdur
stripPos = \case
    T.TBarline _ a -> T.TBarline noPos a
    T.TNote _ a -> T.TNote noPos (stripNote a)
    T.TRest _ a -> T.TRest noPos a
    where
    stripNote note = note
        { T.noteCall = case T.noteCall note of
            T.SubBlock prefix subs -> T.SubBlock prefix (map stripTracks subs)
            call -> call
        , T.notePos = noPos
        }
    stripTracks (T.Tracks tracks) = T.Tracks $ map stripTrack tracks

eTracks :: T.Block T.WrappedTracks -> [T.Track T.Call]
eTracks = T.untracks . expectRight . TScore.unwrapTracks . T.blockTracks

stripTrack :: T.Track T.Call -> T.Track T.Call
stripTrack track = track
    { T.trackTokens = map stripPos (T.trackTokens track)
    , T.trackPos = noPos
    }

eDirective :: T.Directive -> (Text, Maybe Text)
eDirective (T.Directive _ k v) = (k, v)

noOct :: T.Octave
noOct = T.Relative 0

noDur :: T.NDuration
noDur = Parse.emptyNduration

dur :: Maybe Int -> Maybe Int -> Int -> Bool -> T.NDuration
dur int1 int2 dots tie = T.NDuration (T.Duration int1 int2 dots tie)

idur :: Int -> T.NDuration
idur int1 = dur (Just int1) Nothing 0 False

noPos :: T.Pos
noPos = T.Pos 0

tracks :: [(Text, [Parse.Token])] -> T.Tracks T.Call
tracks ts = T.Tracks [T.Track "" title [] tokens noPos | (title, tokens) <- ts]

tnote :: T.Call -> T.Octave -> Text -> T.NDuration -> Parse.Token
tnote call oct pitch dur = T.TNote noPos $ T.Note
    { noteCall = call
    , notePitch = T.NPitch $ T.Pitch oct pitch
    , noteZeroDuration = False
    , noteDuration = dur
    , notePos = noPos
    }

pnote :: Text -> T.NDuration -> Parse.Token
pnote p dur = tnote "" noOct p dur

pnote0 :: Text -> Parse.Token
pnote0 p = tnote "" noOct p noDur

sub :: T.CallText -> [Parse.Token] -> T.Call
sub prefix t = subs prefix [[("", t)]]

subs :: T.CallText -> [[(Text, [Parse.Token])]] -> T.Call
subs prefix ts = T.SubBlock prefix (map tracks ts)

setZeroDur :: T.Token call pitch ndur rdur -> T.Token call pitch ndur rdur
setZeroDur = \case
    T.TNote pos note -> T.TNote pos $ note { T.noteZeroDuration = True }
    tnote -> tnote

parse :: Parse.Element a => Text -> Either String a
parse = Parse.parseText (Parse.parse Parse.defaultConfig)

unparse :: Parse.Element a => a -> Text
unparse = Parse.unparse Parse.defaultConfig
