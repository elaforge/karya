-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{- | Parsing support for the "instrument:" section of the ky file.

    This is an awkward hybrid.  Firstly, the name is inconsistent, internally
    they are called "allocation" because "instrument" usually corresponds to
    'ScoreT.Instrument', while the allocation is the thing tying instrument
    to patch.  But at the UI level I find "allocation" too vague and use
    "instrument" for both concepts.

    But the main thing is that Allocation corresponds only to a subset of
    the actual 'Ui.UiConfig.Allocation'.  The real source of truth is
    UiConfig.Allocation.  So the ky equivalent has to merge in any changes
    that may have happened to the config (done in Parse.Ky.merge_instruments),
    which means automatically updating source, which is fiddly as usual.
-}
module Derive.Parse.Instruments (
    Allocation(..)
    , Config(..), empty_config
    , Backend(..)
    -- , get_ky
    -- , alloc_to_record
    , instrument_section
    -- , update_ui
    -- * parse
    , parse_instruments
    , p_instruments
    , un_instruments
    , p_alloc_line
    , unparse_allocations
    , spaces
) where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.Maps as Maps
import qualified Util.Num as Num
import qualified Util.P as P
import qualified Util.Parse as Parse
import qualified Util.Texts as Texts

import qualified Derive.Parse.AllocRecord as AllocRecord
import qualified Derive.Parse.Record as Record
import qualified Derive.ScoreT as ScoreT

import qualified Instrument.Common as Common
import qualified Instrument.InstT as InstT
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Midi.Patch
import qualified Perform.Midi.Patch as Patch
import qualified Ui.UiConfig as UiConfig

import           Global


type Parser a = P.Parser a
type Error = Text

-- | The subset of 'Ui.UiConfig.Allocation' on the header line.
data Allocation = Allocation
    { alloc_name :: ScoreT.Instrument
    , alloc_qualified :: InstT.Qualified
    , alloc_config :: Config
    , alloc_backend :: Backend
    } deriving (Eq, Show)

-- | Subset of 'Instrument.Common.Config'.
data Config = Config
    { config_mute :: Bool
    , config_solo :: Bool
    } deriving (Eq, Show)

empty_config :: Config
empty_config = Config False False

data Backend = Midi Midi.WriteDevice [Midi.Channel] | Dummy | Im | Sc
    deriving (Eq, Show)

show_backend :: Backend -> Text
show_backend = \case
    Midi {} -> "midi"
    Dummy -> "dummy"
    Im -> "im"
    Sc -> "sc"

-- * instruments

instrument_section :: Text
instrument_section = "instrument"

un_instruments :: UiConfig.Allocations -> Either Error Text
un_instruments (UiConfig.Allocations allocs) = do
    lines <- concatMapM (uncurry un_instrument) (Map.toList allocs)
    pure $ Text.unlines lines

un_instrument :: ScoreT.Instrument -> UiConfig.Allocation
    -> Either Error [Text]
un_instrument inst alloc = do
    alloc_line <- split inst alloc
    let line = Text.unwords $ un_alloc_line alloc_line
    let record = AllocRecord.un_allocation alloc
    pure $ line : if null record then []
        -- Indent so they can fold up.
        else map ("    " <>) $ Record.un_record record

parse_instruments :: Text -> Either Error UiConfig.Allocations
parse_instruments = Parse.parse p_instruments

p_instruments :: Parser UiConfig.Allocations
p_instruments = do
    spaces
    (allocs, dups) <- Maps.unique2 <$> P.many (lexeme p_instrument)
    -- TODO can I get megaparsec to interpret the SourcePos?
    unless (null dups) $
        fail $ "duplicate instrument names: " <> unlines (map show_dup dups)
    pure $ UiConfig.Allocations $ snd <$> allocs
    where
    show_dup (inst, posAllocs) = prettys inst <> " at "
        <> List.intercalate ", " (map (P.sourcePosPretty . fst) posAllocs)

p_instrument :: Parser (ScoreT.Instrument, (P.SourcePos, UiConfig.Allocation))
p_instrument = do
    pos <- P.getSourcePos
    Allocation name qualified config backend <- p_alloc_line
    record <- P.option mempty Record.p_record
    ui_alloc <- either (fail . untxt) pure $
        AllocRecord.p_allocation qualified record
    either (fail . untxt) (pure . (name,) . (pos,)) $
        merge config backend ui_alloc

merge :: Config -> Backend -> UiConfig.Allocation
    -> Either Error UiConfig.Allocation
merge config backend ui_alloc = do
    let alloc_backend = UiConfig.alloc_backend ui_alloc
    -- AllocRecord only parses Midi backends, because no other backend
    -- has config fields.  So it's either Midi or Unknown.
    let record = case alloc_backend of
            UiConfig.Midi config
                | config == Patch.config [] -> Nothing
                | otherwise -> Just config
            _ -> Nothing
    -- The actual backend, line_backend, and record backend must all match,
    -- though for non-midi there are no extra fields, so actual backend
    -- determines it.
    alloc_backend <- case (backend, record) of
        (Midi wdev chans, Just config) ->
            pure $ UiConfig.Midi $ config
                { Patch.config_allocation = convert wdev chans
                }
        (Midi wdev chans, Nothing) ->
            pure $ UiConfig.Midi $ Patch.config (convert wdev chans)
        (Dummy, Nothing) -> pure $ UiConfig.Dummy ""
        (Im, Nothing) -> pure  UiConfig.Im
        (Sc, Nothing) -> pure UiConfig.Sc
        (_, Just _) -> Left $ "got midi fields for " <> show_backend backend
    case alloc_backend of
        UiConfig.Midi config | null config.config_allocation ->
            Left "midi alloc with no channels"
        _ -> pure ()
    pure $ ui_alloc
        { UiConfig.alloc_backend
        , UiConfig.alloc_config = (UiConfig.alloc_config ui_alloc)
            { Common.config_mute, Common.config_solo }
        }
    where
    convert wdev chans = [((wdev, chan), Nothing) | chan <- chans]
    Config { config_mute, config_solo } = config

-- | Invert merge, split an Allocation off from UiConfig.Allocation.
split :: ScoreT.Instrument -> UiConfig.Allocation -> Either Error Allocation
split inst alloc = do
    alloc_backend <- case alloc.alloc_backend of
        UiConfig.Midi config -> ui_midi config
        UiConfig.Dummy {} -> pure Dummy
        UiConfig.Im -> pure Im
        UiConfig.Sc -> pure Sc
    pure $ Allocation
        { alloc_name = inst
        , alloc_qualified = alloc.alloc_qualified
        , alloc_config = Config
            { config_mute = alloc.alloc_config.config_mute
            , config_solo = alloc.alloc_config.config_solo
            }
        , alloc_backend
        }
    where
    ui_midi :: Midi.Patch.Config -> Either Error Backend
    ui_midi config =
        case Lists.groupFst $ map fst $ Midi.Patch.config_allocation config of
            [(wdev, chans)] -> Right $ Midi wdev chans
            allocs -> Left $ "midi config too complicated for: " <> showt allocs

-- * parse / unparse

p_alloc_line :: Parser Allocation
p_alloc_line = Allocation
    <$> (lexeme $ ">" *> (ScoreT.Instrument <$> p_word ""))
    <*> lexeme (InstT.parse_qualified <$> p_word "/")
    <*> lexeme p_config
    <*> p_backend

type Comment = Text

unparse_allocations :: [(Maybe Allocation, Comment)] -> [Text]
unparse_allocations allocs = Texts.columnsSome 1
    [ maybe (Left cmt) (Right . (++cmts) . un_alloc_line) mb_alloc
    | (mb_alloc, cmt) <- allocs
    , let cmts = filter (/="") [cmt]
    ]

un_alloc_line :: Allocation -> [Text]
un_alloc_line (Allocation name qualified config backend) =
    [ ">" <> ScoreT.instrument_name name
    , InstT.show_qualified qualified
    , un_config config
    ] ++ filter (/= "") [un_backend backend]

p_config :: Parser Config
p_config = (P.<?> "[ms]") $ P.between "[" "]" $ do
    config_mute <- False <$ "m" <|> True <$ "M"
    config_solo <- False <$ "s" <|> True <$ "S"
    pure $ Config { config_mute, config_solo }

un_config :: Config -> Text
un_config (Config mute solo) =
    "[" <> (if mute then "M" else "m") <> (if solo then "S" else "s") <> "]"

p_backend :: Parser Backend
p_backend =
    lexeme (Dummy <$ "dummy" <|> Im <$ "im" <|> Sc <$ "sc") <|> p_midi
    where
    p_midi = Midi <$> lexeme (Midi.write_device <$> p_word "")
        <*> (Lists.uniqueSort . concat <$> P.some (lexeme p_chan))
                P.<?> "midi_dev chan.."
    p_chan = do
        chans <- p_range
        forM_ chans $ \chan -> unless (Num.inRange 1 17 chan) $
            fail $ "midi channel should be in range 1--16: " <> show chan
        pure $ map (fromIntegral . subtract 1) chans

p_range :: Parser [Int]
p_range = do
    (low, mb_high) <- (,) <$> Parse.p_nat <*> P.optional (".." *> Parse.p_nat)
    pure $ case mb_high of
        Nothing -> [low]
        Just high
            | low >= high ->
                fail $ "backwards: " <> show low <> ".." <> show high
            | otherwise -> [low .. high]

un_backend :: Backend -> Text
un_backend = \case
    Midi wdev chans ->
        Text.unwords $ Midi.write_device_text wdev
            : abbr_ranges (map (+1) chans)
    Dummy -> "dummy"
    Im -> "im"
    Sc -> "sc"

abbr_ranges :: (Show a, Eq a, Num a) => [a] -> [Text]
abbr_ranges = map fmt . Lists.splitBetween (\x y -> x+1 /= y)
    where
    fmt [x] = showt x
    fmt [] = ""
    fmt xs@(x:_) = showt x <> ".." <> showt (last xs)

-- * util

p_word :: [Char] -> Parser Text
p_word extra = P.takeWhile1 $ \c -> any ($c)
    [ Char.isAsciiLower, Char.isAsciiUpper, Char.isDigit
    , (`elem` ("-" :: [Char]))
    , (`elem` extra)
    ]

spaces :: Parser ()
spaces = P.skipMany $
    ("--" *> P.skipWhile (/='\n') *> P.skipWhile (=='\n'))
    <|> P.skipSome (P.satisfy $ \c -> c == ' ' || c == '\n')

lexeme :: Parser a -> Parser a
lexeme = (<* spaces)
