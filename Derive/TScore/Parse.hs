-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Parse the tscore language:

    > %scale=sargam
    > root = %default-call [melody/0 melody]
    > melody = %dur=mult [
    >     >inst1 | a1 | b2 [ c~ c // e f ]/ |
    >     //
    >     >inst2 | p1 | m |
    > ]
-}
module Derive.TScore.Parse where
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Void as Void

import qualified Util.Control as Control
import qualified Util.P as P
import           Util.P ((<?>))
import qualified Util.Parse as Parse
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Derive.TScore.T as T
import qualified Ui.Id as Id

import           Global


-- | Parsing config.  Parsed 'T.Directive's can affect further parsing, which
-- is not very nice, but convenient for concise notation.
data Config = Config {
    -- | If true, notes with no call get the pitch as their call.
    _default_call :: !Bool
    } deriving (Show, Eq)

default_config :: Config
default_config = Config False

-- * parse

parse_score :: Text -> Either String T.Score
parse_score = parse_text (parse default_config)

parse_text :: Parser a -> Text -> Either String a
parse_text p = first P.errorBundlePretty . P.parse (p <* P.eof) ""

type Parser a = P.Parsec Void.Void Text a

get_pos :: Parser T.Pos
get_pos = T.Pos . P.stateOffset <$> P.getParserState

class Element a where
    parse :: Config -> Parser a
    unparse :: Config -> a -> Text

instance Element a => Element (T.Pos, a) where
    parse config = (,) <$> get_pos <*> parse config
    unparse config = unparse config . snd

instance Element T.Score where
    parse config = fmap T.Score $ do
        p_whitespace
        Control.loop1 config $ \loop config ->
            P.optional (lexeme (parse config)) >>= \case
                Nothing -> return []
                Just toplevel -> (toplevel:) <$>
                    loop (update_config (directives_of (snd toplevel)) config)
    unparse config (T.Score toplevels) =
        Text.unlines $ snd $ List.mapAccumL un config toplevels
        where
        un config toplevel =
            ( update_config (directives_of (snd toplevel)) config
            , unparse config toplevel
            )

update_config :: [T.Directive] -> Config -> Config
update_config directives config
    | default_call_set directives = config { _default_call = True }
    | otherwise = config

default_call_set :: [T.Directive] -> Bool
default_call_set directives =
    maybe False (maybe True (/="f")) $
    Seq.last [val | T.Directive name val <- directives, name == default_call]

default_call :: Text
default_call = "default-call"

directives_of :: T.Toplevel -> [T.Directive]
directives_of (T.ToplevelDirective d) = [d]
directives_of _ = []

instance Element T.Toplevel where
    parse config = T.ToplevelDirective <$> parse config
        <|> T.BlockDefinition <$> parse config
    unparse config (T.ToplevelDirective a) = unparse config a
    unparse config (T.BlockDefinition a) = unparse config a

-- > ns/block = %directive "block title" [ ... ]
instance Element (T.Block T.WrappedTracks) where
    parse config = do
        bid <- lexeme (parse config)
        keyword "="
        directives <- P.many (lexeme (parse config))
        title <- P.option "" (lexeme p_string)
        tracks <- parse (update_config directives config)
        return $ T.Block bid directives title tracks
    unparse config (T.Block bid directives title tracks) =
        Text.unwords $ filter (not . Text.null) $ concat
            [ [unparse config bid, "="]
            , map (unparse config) directives
            , [if Text.null title then "" else un_string title]
            , [unparse (update_config directives config) tracks]
            ]

instance Element Id.BlockId where
    parse _ = do
        a <- P.takeWhile1 Id.is_id_char
        mb <- P.optional $ P.try $ P.char '/' *> (P.takeWhile1 Id.is_id_char)
        let bid = maybe (Id.id default_namespace a)
                (\b -> Id.id (Id.namespace a) b) mb
        maybe (fail $ "invalid BlockId: " <> prettys bid) return (Id.make bid)
        <?> "BlockId"
    unparse _ = Id.show_short default_namespace . Id.unpack_id

default_namespace :: Id.Namespace
default_namespace = Id.namespace "tscore"

instance Element T.WrappedTracks where
    parse config =
        T.WrappedTracks <$> get_pos
            <*> (lexeme "[" *> (unwrap <$> wrapped) <* lexeme "]")
        where
        -- I can't do this the straightforward nested way because // is a
        -- prefix of ///.  Even if I match // not followed by /, it still
        -- doesn't work because it just matches the // prefix and tries to
        -- match the trailing / as a note.  I'm sure there's a way to do it
        -- with enough backtracking, but it seemed simpler to parse a flat list
        -- with different separators.
        unwrap = map (T.Tracks . Either.rights)
            -- The head should be safe because this comes from a SepList, so
            -- the elements are alternating, and there are only two Left vals.
            -- I could keep it typesafe by staying in SepList, but it seems
            -- really annoying to write the split.
            . map (map head_e . Seq.split1 (Left "//"))
            . Seq.split1 (Left "///")
            . to_list
        wrapped = sepBy2 (parse config) (lexeme ("///" <|> "//"))
        head_e [] = error "alternating list wasn't alternating"
        head_e (x:_) = x
    unparse config (T.WrappedTracks _ wrapped) =
        -- This winds up looking pretty ugly, but I can't really fix it without
        -- a fancy formatting or exact print.
        "[" <> Text.intercalate "\n    ///\n" (map tracks wrapped) <> "]"
        where
        tracks (T.Tracks ts) = Text.intercalate " // " (map (unparse config) ts)

data SepList sep a = Nil | Sep a [(sep, a)]
    deriving (Show)

to_list :: SepList sep a -> [Either sep a]
to_list Nil = []
to_list (Sep a xs) = Right a : concat [[Left sep, Right a] | (sep, a) <- xs]

-- | Like 'P.sepBy', but return the separators.
sepBy2 :: Parser a -> Parser sep -> Parser (SepList sep a)
sepBy2 p sep = Sep <$> p <*> P.many ((,) <$> sep <*> p) <|> pure Nil

instance Element (T.Tracks T.Call) where
    parse config = fmap T.Tracks $ keyword "[" *> tracks <* keyword "]"
        where
        tracks = P.sepBy1 (lexeme (parse config)) (keyword "//")
    unparse config (T.Tracks tracks) =
        "[" <> Text.intercalate " // " (map (unparse config) tracks) <> "]"

instance Element (T.Track T.Call) where
    parse config = T.Track
        <$> P.option "" (lexeme p_title) <*> P.some (lexeme (parse config))
        where
        p_title = P.char '>' *> ((">"<>) <$> P.takeWhile Id.is_id_char)
            <|> p_space_title
        p_space_title = do
            t <- p_string
            unless (">" `Text.isPrefixOf` t) $
                fail $ "note track should start with >: " <> untxt t
            return t
    unparse config (T.Track title tokens) =
        TextUtil.join2 un_title (Text.unwords (map (unparse config) tokens))
        where
        un_title
            | " " `Text.isInfixOf` title = un_string title
            | otherwise = title

instance Element T.Directive where
    parse _ = do
        P.char '%'
        T.Directive
            <$> P.takeWhile1 (not_in "=")
            <*> P.optional (P.char '=' *> P.takeWhile1 (not_in ""))
    unparse _ (T.Directive lhs rhs) = "%" <> lhs <> maybe "" ("="<>) rhs

instance Element (T.Token T.Call T.Pitch T.NDuration T.Duration) where
    parse config = T.TBarline <$> get_pos <*> parse config
        <|> T.TRest <$> get_pos <*> parse config
        <|> T.TNote <$> get_pos <*> parse config
    unparse config (T.TBarline _ bar) = unparse config bar
    unparse config (T.TNote _ note) = unparse config note
    unparse config (T.TRest _ rest) = unparse config rest

instance Pretty (T.Token T.Call T.Pitch T.NDuration T.Duration) where
    pretty = unparse default_config

instance Pretty (T.Token T.CallText T.Pitch T.NDuration T.Duration) where
    pretty = \case
        T.TNote _ note -> pretty note
        T.TBarline _ bar -> pretty bar
        T.TRest _ rest -> pretty rest

-- ** barline

instance Element T.Barline where
    parse _ = T.Barline <$> (Text.length <$> P.takeWhile1 (=='|'))
        <|> (P.char ';' *> pure T.AssertCoincident)
    unparse _ (T.Barline n)
        | n >= 1 = Text.replicate n "|"
        | otherwise = error $ "Barline <= 0: " <> show n
    unparse _ T.AssertCoincident = ";"

instance Pretty T.Barline where pretty = unparse default_config

-- ** Note

-- | Parse a note with a letter pitch.
--
-- > <call><oct><pitch><zero-dur><dur><dots><tie>
-- > call/  4    s      *         4    .     ~
-- >        ,    s                :1
--
-- oct is optional, but oct without pitch is not so useful, so pitch >=1 char.
--
-- > a a2 call/a2
-- > a2.
-- > a~ a2~
-- > "call with spaces"/
instance Element (T.Note T.Call T.Pitch T.NDuration) where
    parse config
        -- I need this P.try because / is an empty note and I need to backtrack
        -- when it fails, to parse //.
        | _default_call config = P.try $ do
            pos <- get_pos
            call <- P.optional $ parse config
            (pitch, zero_dur, dur) <-
                P.option (empty_pitch, False, empty_nduration) $ do
                    P.char '/'
                    (,,) <$> P.option empty_pitch (P.try (parse config))
                         <*> p_zero_dur
                         <*> parse config
            make_note pos call pitch zero_dur dur
        | otherwise = do
            pos <- get_pos
            call <- P.optional $ P.try $ parse config <* P.char '/'
            -- I need a try, because if it starts with a number it could be
            -- an octave, or a duration.
            pitch <- P.option empty_pitch $ P.try (parse config)
            zero_dur <- p_zero_dur
            dur <- parse config
            make_note pos call pitch zero_dur dur
        where
        p_zero_dur = P.option False (P.char '*' *> pure True)
        make_note pos call pitch zero_dur dur = do
            let note = T.Note
                    { note_call = fromMaybe (T.Call "") call
                    , note_pitch = pitch
                    , note_zero_duration = zero_dur
                    , note_duration = dur
                    , note_pos = pos
                    }
            -- If I allow "" as a note, I can't get P.many of them.
            guard (note { T.note_pos = T.Pos 0 } /= empty_note)
            return note
    unparse config (T.Note call pitch zero_dur dur _pos)
        | _default_call config =
            unparse config call
            <> if (pitch, zero_dur, dur) == empty then "" else mconcat
                [ "/"
                , unparse config pitch
                , if zero_dur then "*" else ""
                , unparse config dur
                ]
        | otherwise = mconcat
            [ if call == T.Call "" then "" else unparse config call <> "/"
            , unparse config pitch
            , if zero_dur then "*" else ""
            , unparse config dur
            ]
        where
        empty = (empty_pitch, False, empty_nduration)

-- | This is the output from Check.check.
instance Pretty (T.Note T.CallText (Maybe Text) T.Time) where
    pretty (T.Note call pitch _zero_dur dur _pos) = mconcat
        [ if call == "" then "" else call <> "/"
        , fromMaybe "" pitch, pretty dur
        ]

instance Pretty (T.Note T.CallText T.Pitch T.NDuration) where
    pretty (T.Note call pitch _zero_dur dur _pos) = mconcat
        [ if call == "" then "" else call <> "/"
        , unparse default_config pitch, unparse default_config dur
        ]

empty_note :: T.Note T.Call T.Pitch T.NDuration
empty_note = T.Note
    { note_call = T.Call ""
    , note_pitch = empty_pitch
    , note_zero_duration = False
    , note_duration = empty_nduration
    , note_pos = T.Pos 0
    }

empty_nduration :: T.NDuration
empty_nduration = T.NDuration empty_duration

empty_duration :: T.Duration
empty_duration = T.Duration
    { dur_int1 = Nothing
    , dur_int2 = Nothing
    , dur_dots = 0
    , dur_tie = False
    }

empty_pitch :: T.Pitch
empty_pitch = T.Pitch (T.Relative 0) ""

-- | This note is treated specially by the Check layer, to repeat of the
-- previous note.
dot_note :: T.Note T.CallText T.Pitch T.NDuration
dot_note = empty_note
    { T.note_call = ""
    , T.note_duration = T.NDuration $ empty_duration { T.dur_dots = 1 }
    }

-- | This note is treated specially by the Check layer, to repeat of the
-- previous note, plus put a tie on the previous note.
tie_note :: T.Note T.CallText T.Pitch T.NDuration
tie_note = empty_note
    { T.note_call = ""
    , T.note_duration = T.NDuration $ empty_duration { T.dur_tie = True }
    }

-- |
-- > plain-word
-- > "ns/block"
-- > "word with spaces"
-- > "with embedded "() quote"
-- > [sub // block]
-- > plain-word[sub // block]
-- > "spaces word"[sub]
instance Element T.Call where
    parse config =
        uncurry T.SubBlock <$> P.try (p_subblock config)
        <|> T.Call <$> p_string
        <|> T.Call <$> P.takeWhile1 call_char
        <?> "call"
    unparse _ (T.Call call)
        | Text.any (`elem` [' ', '/']) call = "\"" <> call <> "\""
        | otherwise = call
    unparse config (T.SubBlock prefix tracks) = mconcat $
        (if Text.null prefix then "" else unparse config (T.Call prefix))
        : map (unparse config) tracks

instance Pretty T.Call where pretty = unparse default_config

p_subblock :: Config -> Parser (Text, [T.Tracks T.Call])
p_subblock config = (,)
    <$> lexeme_b (p_string <|> P.takeWhile1 call_char <|> pure "")
    <*> P.some (lexeme_b (parse config))

p_string :: Parser Text
p_string = fmap mconcat $ P.between (P.char '"') (P.char '"') $
    P.many $ P.try (P.string "\"(") <|> (Text.singleton <$> P.satisfy (/='"'))

un_string :: Text -> Text
un_string str = "\"" <> str <> "\""

instance Element (T.Rest T.Duration) where
    -- I could possibly forbid ~ tie for rests, but I don't see why
    parse config = T.Rest <$> (P.char '_' *> parse config)
    unparse config (T.Rest dur) = "_" <> unparse config dur

instance Pretty (T.Rest T.Duration) where
    pretty = unparse default_config

call_char :: Char -> Bool
call_char = not_in
    [ '/'
    , '\\'
    , '['
    , ']' -- so a Note inside a SubBlock doesn't eat the ]
    ]

pitch_char :: Char -> Bool
pitch_char c = not_in exclude c && not (Char.isDigit c)
    where
    -- This breaks modularity because I have to just know all the syntax that
    -- could come after, which is Duration, end of Tracks, Rest.  But the more
    -- I can get into Pitch, the more I can get into Call without needing ""s.
    exclude =
        [ '~', '.' -- pitch is followed by T.NDuration
        , ':' -- T.Duration
        , '/' -- Don't mistake the T.Tracks separator for a note.
        , ']' -- end of T.Tracks
        , '*' -- zero duration marker
        , '\\' -- skip whitespace
        ]

-- ** Pitch

instance Element T.Pitch where
    parse config = T.Pitch <$>
        parse config <*> P.takeWhile1 pitch_char <?> "pitch"
    unparse config (T.Pitch octave call) = unparse config octave <> call

instance Pretty T.Pitch where pretty = unparse default_config

instance Element T.Octave where
    parse _ =
        T.Absolute <$> Parse.p_int <|> T.Relative <$> p_relative <?> "octave"
        where
        p_relative = Text.foldl' (\n c -> n + if c == ',' then -1 else 1) 0 <$>
            P.takeWhile (`elem` (",'" :: String))
    unparse _ (T.Absolute oct) = showt oct
    unparse _ (T.Relative n)
        | n >= 0 = Text.replicate n "'"
        | otherwise = Text.replicate (-n) ","

instance Pretty T.Octave where pretty = unparse default_config

-- ** Duration

instance Element T.NDuration where
    parse config =
        P.char '0' *> pure T.CallDuration <|> T.NDuration <$> parse config
    unparse _ T.CallDuration = "0"
    unparse config (T.NDuration a) = unparse config a

instance Element T.Duration where
    parse _ = T.Duration
        <$> P.optional Parse.p_nat
        <*> P.optional (P.char ':' *> Parse.p_nat)
        <*> (Text.length <$> P.takeWhile (=='.'))
        <*> P.option False (P.char '~' *> pure True)
        <?> "duration"
    unparse _ (T.Duration int1 int2 dots tie) = mconcat
        [ maybe "" showt int1
        , maybe "" ((":"<>) . showt) int2
        , Text.replicate dots "."
        , if tie then "~" else ""
        ]

-- ** util

p_whitespace :: Parser ()
p_whitespace = void $ P.many $ P.space1 <|> p_comment
    where
    p_comment = do
        P.string "--"
        P.takeWhile (/='\n')
        P.option () (void $ P.char '\n')

lexeme :: Parser a -> Parser a
lexeme = (<* p_whitespace)

-- | Use a backslash to allow whitespace.  If I had a separate tokenizer, maybe
-- I could implement this as a general-purpose glue together tokens thing, but
-- at least for the moment I only want to do this in a specific spot.
lexeme_b :: Parser a -> Parser a
lexeme_b = (<* P.option () (P.char '\\' *> p_whitespace))

keyword :: Text -> Parser ()
keyword str = void $ lexeme (P.string str)

not_in :: [Char] -> Char -> Bool
not_in cs = \c -> not (Char.isSpace c) && c `notElem` cs
