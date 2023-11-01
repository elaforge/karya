-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Parse the tscore language.
module Derive.TScore.Parse (
    -- * Parse
    parse_score
    , parse_allocation, unparse_allocations
    , default_call, default_namespace
    , show_block, show_block_track
    -- * Note
    , dot_note, tie_note
    -- * util
    , strip_comment
    , p_whitespace_
#ifdef TESTING
    , module Derive.TScore.Parse
#endif
) where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Void as Void

import qualified Util.Control as Control
import qualified Util.P as P
import           Util.P ((<?>))
import qualified Util.Parse as Parse

import qualified Derive.Parse.Instruments as Instruments
import qualified Derive.TScore.T as T
import qualified Ui.Id as Id

import           Global
import           Types


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
default_call_set directives = not $ null
    [() | T.Directive _ name _ <- directives, name == default_call]

default_call :: Text
default_call = "default-call"

directives_of :: T.Toplevel -> [T.Directive]
directives_of (T.ToplevelDirective d) = [d]
directives_of _ = []

instance Element T.Toplevel where
    parse config =
        T.ToplevelDirective . un <$> parse config
        <|> T.BlockDefinition <$> parse config
        where un (ToplevelDirective d) = d
    unparse config = \case
        T.ToplevelDirective a -> unparse config (ToplevelDirective a)
        T.BlockDefinition a -> unparse config a

-- > ns/block = %directive "block title" [ ... ]
instance Element (T.Block T.WrappedTracks) where
    parse config = do
        block_id <- lexeme (parse config)
        keyword "="
        block_directives <- P.many (lexeme (parse config))
        block_title <- P.option "" (lexeme p_string)
        block_tracks <- parse (update_config block_directives config)
        return $ T.Block
            { block_id, block_directives, block_title, block_tracks }
    unparse config (T.Block
            { block_id, block_directives, block_title, block_tracks }) =
        Text.unwords $ filter (not . Text.null) $ concat
            [ [unparse config block_id, "="]
            , map (unparse config) block_directives
            , [if Text.null block_title then "" else un_string block_title]
            , [unparse (update_config block_directives config) block_tracks]
            ]

instance Element Id.BlockId where
    parse _ = do
        a <- P.takeWhile1 Id.is_id_char
        mb <- P.optional $ P.try $ P.char '/' *> (P.takeWhile1 Id.is_id_char)
        let bid = maybe (Id.id default_namespace a)
                (\b -> Id.id (Id.namespace a) b) mb
        maybe (fail $ "invalid BlockId: " <> prettys bid) return (Id.make bid)
        <?> "BlockId"
    unparse _ = show_block

show_block :: Id.BlockId -> Text
show_block = Id.show_short default_namespace . Id.unpack_id

show_block_track :: Id.BlockId -> TrackNum -> Text
show_block_track block_id tracknum =
    show_block block_id <> ":" <> showt tracknum

default_namespace :: Id.Namespace
default_namespace = Id.namespace "tscore"

-- | Tracks are bounded with [ ], and multiple Tracks can be simply written
-- in sequence, like [ a b ] [ c d ], which then get unwrapped as [ a+c c+d ].
instance Element T.WrappedTracks where
    parse config = T.WrappedTracks <$> get_pos
        <*> P.some (lexeme (parse config))
    unparse config (T.WrappedTracks _ wrapped) =
        Text.unwords $ map (unparse config) wrapped

-- | Tracks are surrounded by [ ].  Each track is delimited by a word starting
-- with >.
instance Element (T.Tracks T.Call) where
    parse config = fmap T.Tracks $ lexeme "[" *> tracks <* lexeme "]"
        where
        tracks = P.sepBy1 (lexeme (parse config)) $
            P.lookAhead $ lexeme (P.char '>' *> pure () <|> "\">" *> pure ())
    unparse config (T.Tracks tracks) =
        "[" <> Text.unwords (map (unparse config) tracks) <> "]"

instance Element (T.Track T.Call) where
    parse config = do
        track_pos <- get_pos
        (track_key, track_title) <- P.option ("", "") $ lexeme $ (,)
            <$> (P.char '>' *> P.takeWhile is_key_char)
            <*> fmap (">"<>) (p_string <|> P.takeWhile Id.is_id_char)
        track_directives <- P.many $ lexeme $ parse config
        track_tokens <- P.many $ lexeme $ parse config
        return $ T.Track
            { track_key, track_title, track_directives, track_tokens
            , track_pos
            }
    unparse config (T.Track
            { track_key, track_title, track_directives, track_tokens }) =
        Text.unwords $ filter (not . Text.null) $ concat
            -- See 'T.track_title' for why this is so complicated.
            [ (:[]) $ if track_key == "" && track_title == "" then ""
                else ">" <> track_key <> if " " `Text.isInfixOf` track_title
                    then un_string (Text.drop 1 track_title)
                    else Text.drop 1 track_title
            , map (unparse config) track_directives
            , map (unparse config) track_tokens
            ]

is_key_char :: Char -> Bool
is_key_char c = c `elem` ("!@#$%^&*" :: [Char])
    -- Technically this could be `c /='"' && not (Id.is_id_char c)`, but I'll be
    -- more restrictive for now.

-- | Apply different parsing rules to a 'T.ToplevelDirective'.
newtype ToplevelDirective = ToplevelDirective T.Directive
    deriving (Eq, Show)

-- | At the toplevel, I need a % to disambiguate from 'T.BlockDefinition'.
-- But, I can have spaces around =.
instance Element ToplevelDirective where
    parse _ = ToplevelDirective <$> p_directive True
    unparse _ (ToplevelDirective d) = unparse_directive True d

-- | Block level directives don't allow spaces around = even though I don't
-- think they would cause a problem.  Previously I thought I could omit %s,
-- but it turns out to make it ambiguous with notes, especially for
-- track_directives.
instance Element T.Directive where
    parse _ = p_directive False
    unparse _ = unparse_directive False

p_directive :: Bool -> Parser T.Directive
p_directive spaces = do
    pos <- get_pos
    P.char '%'
    key <- P.takeWhile1 Id.is_id_char
    when spaces p_whitespace
    val <- P.optional $ do
        P.char '='
        when spaces p_whitespace
        p_multi_string <|> p_word
    return $ T.Directive pos key val

unparse_directive :: Bool -> T.Directive -> Text
unparse_directive spaces (T.Directive _ lhs rhs) =
    mconcat ["%", lhs, maybe "" ((equal<>) . unparse_directive_value) rhs]
    where equal = if spaces then " = " else "="

unparse_directive_value :: Text -> Text
unparse_directive_value val
    | Text.any Char.isSpace val = "''\n" <> val <> "\n''"
    | otherwise = val

p_multi_string :: Parser Text
p_multi_string = "''" *> contents <* "''"
    where
    contents = dedent . Text.intercalate "'" <$>
        ((:) <$> content <*> P.many chunk)
    content = P.takeWhile1 (/='\'')
    -- TODO this is inefficient, surely there's a way to takeWhile but look at
    -- the next char?
    chunk = P.try $ "'" *> P.takeWhile1 (/='\'')

dedent :: Text -> Text
dedent t = Text.strip $ case Text.lines (Text.dropWhile (=='\n') t) of
    [] -> ""
    x : xs -> Text.unlines (map (strip indent) (x:xs))
        where indent = Text.takeWhile Char.isSpace x
    where strip pref s = fromMaybe s $ Text.stripPrefix pref s

parse_allocation :: Text -> Either String Instruments.Allocation
parse_allocation = parse_text Instruments.p_allocation

unparse_allocations :: [Instruments.Allocation] -> Text
unparse_allocations =
    Text.unlines . Instruments.unparse_allocations . map ((, "") . Just)

type Token = T.Token T.Call (T.NPitch T.Pitch) T.NDuration T.Duration

instance Element Token where
    parse config =
        T.TBarline <$> get_pos <*> parse config
        <|> T.TRest <$> get_pos <*> parse config
        <|> T.TNote <$> get_pos <*> parse config
    unparse config (T.TBarline _ bar) = unparse config bar
    unparse config (T.TNote _ note) = unparse config note
    unparse config (T.TRest _ rest) = unparse config rest

instance Pretty Token where
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

-- * Note

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
instance Element (T.Note T.Call (T.NPitch T.Pitch) T.NDuration) where
    parse config
        -- I need this P.try because / is an empty note and I need to backtrack
        -- when it fails, to parse //.
        | _default_call config = P.try $ do
            pos <- get_pos
            call <- P.optional $ parse config
            (pitch, zero_dur, dur) <-
                P.option (empty_npitch, False, empty_nduration) $ do
                    P.char '/'
                    (,,) <$> P.option empty_npitch (P.try (parse config))
                         <*> p_zero_dur
                         <*> parse config
            make_note pos call pitch zero_dur dur
        | otherwise = do
            pos <- get_pos
            call <- P.optional $ P.try $ parse config <* P.char '/'
            -- I need a try, because if it starts with a number it could be
            -- an octave, or a duration.
            pitch <- P.option empty_npitch $ P.try (parse config)
            zero_dur <- p_zero_dur
            dur <- parse config
            make_note pos call pitch zero_dur dur
        where
        p_zero_dur = P.option False (P.char '*' *> pure True)
        make_note note_pos call pitch note_zero_duration note_duration = do
            -- If I allow "" as a note, I can't get P.many of them.
            guard (note { T.note_pos = T.Pos 0 } /= empty_note)
            return note
            where
            note = T.Note
                { note_call = fromMaybe (T.Call "") call
                , note_pitch = pitch
                , note_zero_duration
                , note_duration
                , note_pos
                }
    unparse config (T.Note
            { note_call, note_pitch, note_zero_duration, note_duration })
        | _default_call config =
            unparse config note_call
            <> if (note_pitch, note_zero_duration, note_duration) == empty
                then "" else mconcat
                [ "/"
                , unparse config note_pitch
                , if note_zero_duration then "*" else ""
                , unparse config note_duration
                ]
        | otherwise = mconcat
            [ if note_call == T.Call "" then ""
                else unparse config note_call <> "/"
            , unparse config note_pitch
            , if note_zero_duration then "*" else ""
            , unparse config note_duration
            ]
        where
        empty = (empty_npitch, False, empty_nduration)

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

instance Element (T.NPitch T.Pitch) where
    parse config =
        P.char '^' *> pure T.CopyFrom <|> T.NPitch <$> parse config
    unparse config = \case
        T.CopyFrom -> "^"
        T.NPitch pitch -> unparse config pitch

empty_note :: T.Note T.Call (T.NPitch T.Pitch) T.NDuration
empty_note = T.Note
    { note_call = T.Call ""
    , note_pitch = empty_npitch
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

empty_npitch :: T.NPitch T.Pitch
empty_npitch = T.NPitch empty_pitch

empty_pitch :: T.Pitch
empty_pitch = T.Pitch (T.Relative 0) ""

-- | This note is treated specially by the Check layer, to repeat of the
-- previous note.
dot_note :: T.Note T.CallText (T.NPitch T.Pitch) T.NDuration
dot_note = empty_note
    { T.note_call = ""
    , T.note_duration = T.NDuration $ empty_duration { T.dur_dots = 1 }
    }

-- | This note is treated specially by the Check layer, to repeat of the
-- previous note, plus put a tie on the previous note.
tie_note :: T.Note T.CallText (T.NPitch T.Pitch) T.NDuration
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
    , '>' -- so I can use > to separate tracks when default-call is on
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
        , ']' -- end of T.Tracks
        , '*' -- zero duration marker
        , '\\' -- skip whitespace
        , '>' -- this separates tracks
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
    unparse config = \case
        T.CallDuration -> "0"
        T.NDuration a -> unparse config a

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

-- * util

p_word :: Parser Text
p_word = P.takeWhile1 (not_in "")

p_whitespace :: Parser ()
p_whitespace = P.skipMany p_space

-- | 'p_whitespace', but True if there was any space to skip.
p_whitespace_ :: Parser Bool
p_whitespace_ =
    (maybe False (\() -> True) <$> P.optional p_space) <* (P.skipMany p_space)

p_space :: Parser ()
p_space = P.space1 <|> p_comment
    where
    p_comment = do
        P.string "--"
        P.skipWhile (/='\n')
        P.option () (void $ P.char '\n')

strip_comment :: Text -> Text
strip_comment = head . Text.splitOn "--"

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
