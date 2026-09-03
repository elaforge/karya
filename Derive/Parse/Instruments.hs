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
    , instrument_section
    , to_backend
    -- * parse
    , p_instruments
    , un_instruments
    , un_scale
    , p_alloc_line
    , un_alloc_line
    , spaces
) where
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as Vector.Unboxed

import qualified Util.Lists as Lists
import qualified Util.Maps as Maps
import qualified Util.Num as Num
import qualified Util.P as P
import qualified Util.Parse as Parse

import qualified Derive.DeriveT as DeriveT
import qualified Derive.Expr as Expr
import qualified Derive.Parse.AllocRecord as AllocRecord
import qualified Derive.Parse.Record as Record
import qualified Derive.REnv as REnv
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

type LookupInst = InstT.Qualified -> Maybe REnv.Environ

un_instruments :: LookupInst -> UiConfig.Allocations -> Either Error Text
un_instruments lookup_inst (UiConfig.Allocations allocs) = do
    lines <- concatMapM (uncurry (un_instrument lookup_inst))
        (Map.toList allocs)
    pure $ Text.unlines $ lines ++ map un_scale scales
    where
    scales = Lists.unique $ mapMaybe (scale_of . UiConfig.alloc_backend)
        (Map.elems allocs)
    scale_of = \case
        UiConfig.Midi config -> Patch.settings#Patch.scale #$ config
        _ -> Nothing

un_instrument :: LookupInst -> ScoreT.Instrument -> UiConfig.Allocation
    -> Either Error [Text]
un_instrument lookup_inst inst alloc = do
    alloc_line <- split inst alloc
    let line = Text.unwords $ un_alloc_line alloc_line
    let record = AllocRecord.un_allocation alloc
    -- Indent so they can fold up.
    pure $ (line :) $ map ("    " <>) $
        (if null record then [] else Record.un_record record)
        ++ maybe [] inherited_environ (lookup_inst (alloc_qualified alloc_line))

inherited_environ :: REnv.Environ -> [Text]
inherited_environ env
    | REnv.null env = []
    | otherwise = map ("-- "<>) $ Record.un_rval $ AllocRecord.un_environ env

p_instruments :: Parser UiConfig.Allocations
p_instruments = do
    (env, records) <- first Map.fromList . Either.partitionEithers <$>
        p_definitions
    insts <- mapM (substituteM env) records
    let (allocs, dups) = Maps.unique2 insts
    dups <- mapM (traverse (mapM (firstM Parse.offsetToSourcePos))) dups
    unless (null dups) $
        fail $ "duplicate instrument names: " <> unlines (map show_dup dups)
    pure $ UiConfig.Allocations $ snd <$> allocs
    where
    show_dup (inst, posAllocs) = prettys inst <> " at "
        <> List.intercalate ", " (map (P.sourcePosPretty . fst) posAllocs)

type Record = Map Text Record.RVal

substituteM :: Record -> (Parse.Offset, (Allocation, Record))
    -> Parser (ScoreT.Instrument, (Parse.Offset, UiConfig.Allocation))
substituteM env (offset, (alloc_line, record)) = do
    record <- either die pure $ substitute env record
    ui_alloc <- either die pure $
        AllocRecord.p_allocation qualified record
    either die (pure . (name,) . (offset,)) $
        merge config backend ui_alloc
    where
    die = Parse.failAt offset . untxt
    Allocation name qualified config backend = alloc_line

-- | Resolve variables parsed by p_equal.  So far it's just for scale.
substitute :: Record -> Record -> Either Error Record
substitute env record = case Map.lookup "scale" record of
    Just (Record.Val (DeriveT.VStr (Expr.Str name))) ->
        case Map.lookup name env of
            Nothing -> Left $ "no assignment for: " <> name
            Just nns -> Right $ Map.insert "scale" (make name nns) record
    _ -> Right record
    where
    make name nns = Record.Record
        -- I think name is now unnecessary, but Patch.Scale has it, so let's
        -- keep it for now.
        [ ("name", Record.Val (DeriveT.str name))
        , ("key_to_nn", nns)
        ]

-- | Create the p_equal that 'substitute' replaces.
un_scale :: Patch.Scale -> Text
un_scale scale =
    -- TODO Historically scale_name had spaces.  They should no longer
    -- have them, but this is so old scores still load.
    un_equal (Text.replace " " "-" scale.scale_name) $
    Record.list $ map DeriveT.num $
    Vector.Unboxed.toList scale.scale_key_to_nn

p_definitions :: Parser
    [Either (Symbol, Record.RVal) (Parse.Offset, (Allocation, Record))]
p_definitions = do
    spaces
    P.many $ lexeme $ (Left <$> p_equal <|> Right <$> p_record)

p_record :: Parser (Parse.Offset, (Allocation, Record))
p_record = do
    offset <- P.getOffset
    alloc <- p_alloc_line
    record <- P.option mempty Record.p_record
    pure (offset, (alloc, Map.fromList record))

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
            pure $ UiConfig.Midi $
                config { Patch.config_allocation = convert wdev chans }
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
        UiConfig.Midi config -> first (prefix<>) $ to_backend config
            where prefix = pretty inst <> ": " <> pretty alloc <> ": "
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

to_backend :: Midi.Patch.Config -> Either Error Backend
to_backend config =
    case Lists.groupFst $ map fst $ Midi.Patch.config_allocation config of
        [(wdev, chans)] -> Right $ Midi wdev chans
        -- TODO I should no longer allow these, but if any are left, they
        -- should be converted to Dummy.
        [] -> Right Dummy
        allocs -> Left $ "midi config too complicated for: " <> showt allocs

-- * parse / unparse

p_alloc_line :: Parser Allocation
p_alloc_line = Allocation
    <$> (lexeme $ ">" *> (ScoreT.Instrument <$> p_word ""))
    <*> lexeme (InstT.parse_qualified <$> p_word "/")
    <*> lexeme p_config
    <*> p_backend

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

-- ** p_equal

type Symbol = Text

p_equal :: Parser (Symbol, Record.RVal)
p_equal = do
    lhs <- p_symbol
    spaces >> "=" >> spaces
    val <- Record.p_rval
    pure (lhs, val)

un_equal :: Symbol -> Record.RVal -> Text
un_equal sym val = Text.unwords $ [sym, "="] ++ Record.un_rval val

-- * util

p_word :: [Char] -> Parser Text
p_word extra = P.takeWhile1 $ \c -> any ($c)
    [ Char.isAsciiLower, Char.isAsciiUpper, Char.isDigit
    , (`elem` ("-" :: [Char]))
    , (`elem` extra)
    ]

p_symbol :: Parser Symbol
p_symbol = P.takeWhile $ \c -> Char.isAsciiLower c || c == '-'

spaces :: Parser ()
spaces = P.skipMany $
    ("--" *> P.skipWhile (/='\n') *> P.skipWhile (=='\n'))
    <|> P.skipSome (P.satisfy $ \c -> c == ' ' || c == '\n')

lexeme :: Parser a -> Parser a
lexeme = (<* spaces)

firstM :: Applicative f => (a -> f c) -> (a, b) -> f (c, b)
firstM f (a, b) = (,) <$> f a <*> pure b
