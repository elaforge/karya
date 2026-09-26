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
    , Config(..), emptyConfig
    , Backend(..)
    , instrumentSection
    , toBackend
    -- * parse
    , pInstruments
    , unInstruments
    , unScale
    , pAllocLine
    , unAllocLine
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
    { allocName :: ScoreT.Instrument
    , allocQualified :: InstT.Qualified
    , allocConfig :: Config
    , allocBackend :: Backend
    } deriving (Eq, Show)

-- | Subset of 'Instrument.Common.Config'.
data Config = Config
    { configMute :: Bool
    , configSolo :: Bool
    } deriving (Eq, Show)

emptyConfig :: Config
emptyConfig = Config False False

data Backend = Midi Midi.WriteDevice [Midi.Channel] | Dummy | Im | Sc
    deriving (Eq, Show)

showBackend :: Backend -> Text
showBackend = \case
    Midi {} -> "midi"
    Dummy -> "dummy"
    Im -> "im"
    Sc -> "sc"

-- * instruments

instrumentSection :: Text
instrumentSection = "instrument"

type LookupInst = InstT.Qualified -> Maybe REnv.Environ

unInstruments :: LookupInst -> UiConfig.Allocations -> Either Error Text
unInstruments lookupInst (UiConfig.Allocations allocs) = do
    lines <- concatMapM (uncurry (unInstrument lookupInst))
        (Map.toList allocs)
    pure $ Text.unlines $ lines ++ map unScale scales
    where
    scales = Lists.unique $ mapMaybe (scaleOf . UiConfig.allocBackend)
        (Map.elems allocs)
    scaleOf = \case
        UiConfig.Midi config -> Patch.settings#Patch.scale #$ config
        _ -> Nothing

unInstrument :: LookupInst -> ScoreT.Instrument -> UiConfig.Allocation
    -> Either Error [Text]
unInstrument lookupInst inst alloc = do
    allocLine <- split inst alloc
    let line = Text.unwords $ unAllocLine allocLine
    let record = AllocRecord.unAllocation alloc
    -- Indent so they can fold up.
    pure $ (line :) $ map ("    " <>) $
        (if null record then [] else Record.unRecord record)
        ++ maybe [] inheritedEnviron (lookupInst (allocQualified allocLine))

inheritedEnviron :: REnv.Environ -> [Text]
inheritedEnviron env
    | REnv.null env = []
    | otherwise = map ("-- "<>) $ Record.unRval $ AllocRecord.unEnviron env

pInstruments :: Parser UiConfig.Allocations
pInstruments = do
    (env, records) <- first Map.fromList . Either.partitionEithers <$>
        pDefinitions
    insts <- mapM (substituteM env) records
    let (allocs, dups) = Maps.unique2 insts
    dups <- mapM (traverse (mapM (firstM Parse.offsetToSourcePos))) dups
    unless (null dups) $
        fail $ "duplicate instrument names: " <> unlines (map showDup dups)
    pure $ UiConfig.Allocations $ snd <$> allocs
    where
    showDup (inst, posAllocs) = prettys inst <> " at "
        <> List.intercalate ", " (map (P.sourcePosPretty . fst) posAllocs)

type Record = Map Text Record.RVal

substituteM :: Record -> (Parse.Offset, (Allocation, Record))
    -> Parser (ScoreT.Instrument, (Parse.Offset, UiConfig.Allocation))
substituteM env (offset, (allocLine, record)) = do
    record <- either die pure $ substitute env record
    uiAlloc <- either die pure $
        AllocRecord.pAllocation qualified record
    either die (pure . (name,) . (offset,)) $
        merge config backend uiAlloc
    where
    die = Parse.failAt offset . untxt
    Allocation name qualified config backend = allocLine

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
unScale :: Patch.Scale -> Text
unScale scale =
    -- TODO Historically scale_name had spaces.  They should no longer
    -- have them, but this is so old scores still load.
    unEqual (Text.replace " " "-" scale.scaleName) $
    Record.list $ map DeriveT.num $
    Vector.Unboxed.toList scale.scaleKeyToNn

pDefinitions :: Parser
    [Either (Symbol, Record.RVal) (Parse.Offset, (Allocation, Record))]
pDefinitions = do
    spaces
    P.many $ lexeme $ (Left <$> pEqual <|> Right <$> pRecord)

pRecord :: Parser (Parse.Offset, (Allocation, Record))
pRecord = do
    offset <- P.getOffset
    alloc <- pAllocLine
    record <- P.option mempty Record.pRecord
    pure (offset, (alloc, Map.fromList record))

merge :: Config -> Backend -> UiConfig.Allocation
    -> Either Error UiConfig.Allocation
merge config backend uiAlloc = do
    let allocBackend = UiConfig.allocBackend uiAlloc
    -- AllocRecord only parses Midi backends, because no other backend
    -- has config fields.  So it's either Midi or Unknown.
    let record = case allocBackend of
            UiConfig.Midi config
                | config == Patch.config [] -> Nothing
                | otherwise -> Just config
            _ -> Nothing
    -- The actual backend, line_backend, and record backend must all match,
    -- though for non-midi there are no extra fields, so actual backend
    -- determines it.
    allocBackend <- case (backend, record) of
        (Midi wdev chans, Just config) ->
            pure $ UiConfig.Midi $
                config { Patch.configAllocation = convert wdev chans }
        (Midi wdev chans, Nothing) ->
            pure $ UiConfig.Midi $ Patch.config (convert wdev chans)
        (Dummy, Nothing) -> pure $ UiConfig.Dummy ""
        (Im, Nothing) -> pure  UiConfig.Im
        (Sc, Nothing) -> pure UiConfig.Sc
        (_, Just _) -> Left $ "got midi fields for " <> showBackend backend
    case allocBackend of
        UiConfig.Midi config | null config.configAllocation ->
            Left "midi alloc with no channels"
        _ -> pure ()
    pure $ uiAlloc
        { UiConfig.allocBackend
        , UiConfig.allocConfig = (UiConfig.allocConfig uiAlloc)
            { Common.configMute, Common.configSolo }
        }
    where
    convert wdev chans = [((wdev, chan), Nothing) | chan <- chans]
    Config { configMute, configSolo } = config

-- | Invert merge, split an Allocation off from UiConfig.Allocation.
split :: ScoreT.Instrument -> UiConfig.Allocation -> Either Error Allocation
split inst alloc = do
    allocBackend <- case alloc.allocBackend of
        UiConfig.Midi config -> first (prefix<>) $ toBackend config
            where prefix = pretty inst <> ": " <> pretty alloc <> ": "
        UiConfig.Dummy {} -> pure Dummy
        UiConfig.Im -> pure Im
        UiConfig.Sc -> pure Sc
    pure $ Allocation
        { allocName = inst
        , allocQualified = alloc.allocQualified
        , allocConfig = Config
            { configMute = alloc.allocConfig.configMute
            , configSolo = alloc.allocConfig.configSolo
            }
        , allocBackend
        }

toBackend :: Midi.Patch.Config -> Either Error Backend
toBackend config =
    case Lists.groupFst $ map fst $ Midi.Patch.configAllocation config of
        [(wdev, chans)] -> Right $ Midi wdev chans
        -- TODO I should no longer allow these, but if any are left, they
        -- should be converted to Dummy.
        [] -> Right Dummy
        allocs -> Left $ "midi config too complicated for: " <> showt allocs

-- * parse / unparse

pAllocLine :: Parser Allocation
pAllocLine = Allocation
    <$> (lexeme $ ">" *> (ScoreT.Instrument <$> pWord ""))
    <*> lexeme (InstT.parseQualified <$> pWord "/")
    <*> lexeme pConfig
    <*> pBackend

unAllocLine :: Allocation -> [Text]
unAllocLine (Allocation name qualified config backend) =
    [ ">" <> ScoreT.instrumentName name
    , InstT.showQualified qualified
    , unConfig config
    ] ++ filter (/= "") [unBackend backend]

pConfig :: Parser Config
pConfig = (P.<?> "[ms]") $ P.between "[" "]" $ do
    configMute <- False <$ "m" <|> True <$ "M"
    configSolo <- False <$ "s" <|> True <$ "S"
    pure $ Config { configMute, configSolo }

unConfig :: Config -> Text
unConfig (Config mute solo) =
    "[" <> (if mute then "M" else "m") <> (if solo then "S" else "s") <> "]"

pBackend :: Parser Backend
pBackend =
    lexeme (Dummy <$ "dummy" <|> Im <$ "im" <|> Sc <$ "sc") <|> pMidi
    where
    pMidi = Midi <$> lexeme (Midi.writeDevice <$> pWord "")
        <*> (Lists.uniqueSort . concat <$> P.some (lexeme pChan))
                P.<?> "midi_dev chan.."
    pChan = do
        chans <- pRange
        forM_ chans $ \chan -> unless (Num.inRange 1 17 chan) $
            fail $ "midi channel should be in range 1--16: " <> show chan
        pure $ map (fromIntegral . subtract 1) chans

pRange :: Parser [Int]
pRange = do
    (low, mbHigh) <- (,) <$> Parse.pNat <*> P.optional (".." *> Parse.pNat)
    pure $ case mbHigh of
        Nothing -> [low]
        Just high
            | low >= high ->
                fail $ "backwards: " <> show low <> ".." <> show high
            | otherwise -> [low .. high]

unBackend :: Backend -> Text
unBackend = \case
    Midi wdev chans ->
        Text.unwords $ Midi.writeDeviceText wdev
            : abbrRanges (map (+1) chans)
    Dummy -> "dummy"
    Im -> "im"
    Sc -> "sc"

abbrRanges :: (Show a, Eq a, Num a) => [a] -> [Text]
abbrRanges = map fmt . Lists.splitBetween (\x y -> x+1 /= y)
    where
    fmt [x] = showt x
    fmt [] = ""
    fmt xs@(x:_) = showt x <> ".." <> showt (last xs)

-- ** p_equal

type Symbol = Text

pEqual :: Parser (Symbol, Record.RVal)
pEqual = do
    lhs <- pSymbol
    spaces >> "=" >> spaces
    val <- Record.pRval
    pure (lhs, val)

unEqual :: Symbol -> Record.RVal -> Text
unEqual sym val = Text.unwords $ [sym, "="] ++ Record.unRval val

-- * util

pWord :: [Char] -> Parser Text
pWord extra = P.takeWhile1 $ \c -> any ($c)
    [ Char.isAsciiLower, Char.isAsciiUpper, Char.isDigit
    , (`elem` ("-" :: [Char]))
    , (`elem` extra)
    ]

pSymbol :: Parser Symbol
pSymbol = P.takeWhile $ \c -> Char.isAsciiLower c || c == '-'

spaces :: Parser ()
spaces = P.skipMany $
    ("--" *> P.skipWhile (/='\n') *> P.skipWhile (=='\n'))
    <|> P.skipSome (P.satisfy $ \c -> c == ' ' || c == '\n')

lexeme :: Parser a -> Parser a
lexeme = (<* spaces)

firstM :: Applicative f => (a -> f c) -> (a, b) -> f (c, b)
firstM f (a, b) = (,) <$> f a <*> pure b
