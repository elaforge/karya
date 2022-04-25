-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.Parse.Instruments (
    Allocation(..)
    , Config(..), empty_config
    , Backend(..)
    , get_ky
    , instrument_section
    , update_ui
    -- * parse
    , p_allocation
    , unparse_allocations
) where
import qualified Data.Attoparsec.Text as A
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Maps as Maps
import qualified Util.Num as Num
import qualified Util.ParseText as ParseText
import qualified Util.Seq as Seq
import qualified Util.Texts as Texts

import qualified Derive.ScoreT as ScoreT
import qualified Instrument.Common as Common
import qualified Instrument.InstT as InstT
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Midi.Patch
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global


type Parser a = A.Parser a
type Error = Text

-- | This is a simplified subset of 'Ui.UiConfig.Allocation'.
data Allocation = Allocation {
    alloc_name :: !ScoreT.Instrument
    , alloc_qualified :: !InstT.Qualified
    , alloc_config :: !Config
    , alloc_backend :: !Backend
    } deriving (Eq, Show)

-- | Subset of 'Instrument.Common.Config'.
data Config = Config {
    config_mute :: !Bool
    , config_solo :: !Bool
    } deriving (Eq, Show)

empty_config :: Config
empty_config = Config False False

data Backend = Midi Midi.WriteDevice [Midi.Channel] | NonMidi
    deriving (Eq, Show)

-- * instruments

get_ky :: Ui.M m => m Text
get_ky = do
    ky <- Ui.config#UiConfig.ky <#> Ui.get
    if instrument `elem` Text.lines ky then return ky
        else do
            allocs <- Ui.config#UiConfig.allocations <#> Ui.get
            allocs <- Ui.require_right id $ mapM (uncurry from_ui) $
                Map.toList $ UiConfig.unallocations allocs
            return $ Texts.join2 "\n\n" ky $
                instrument <> "\n" <> unparse_allocations allocs
    where
    instrument = instrument_section <> ":"

instrument_section :: Text
instrument_section = "instrument"

from_ui :: ScoreT.Instrument -> UiConfig.Allocation -> Either Error Allocation
from_ui inst alloc = do
    backend <- case UiConfig.alloc_backend alloc of
        UiConfig.Midi config -> ui_midi config
        _ -> return NonMidi
    let cconfig = UiConfig.alloc_config alloc
    return $ Allocation
        { alloc_name = inst
        , alloc_qualified = UiConfig.alloc_qualified alloc
        , alloc_config = Config
            { config_mute = Common.config_mute cconfig
            , config_solo = Common.config_solo cconfig
            }
        , alloc_backend = backend
        }
    where
    ui_midi :: Midi.Patch.Config -> Either Error Backend
    ui_midi config =
        case Seq.group_fst $ map fst $ Midi.Patch.config_allocation config of
            [(wdev, chans)] -> Right $ Midi wdev chans
            allocs -> Left $ "midi config too complicated for: " <> showt allocs

update_ui :: [Allocation] -> UiConfig.Allocations
    -> Either Error UiConfig.Allocations
update_ui allocs (UiConfig.Allocations olds) =
    fmap (UiConfig.Allocations . Map.fromList) $ mapMaybeM make $
        Maps.pairs (Map.fromList (Seq.key_on alloc_name allocs)) olds
    where
    make = \case
        (inst, Seq.Both alloc old) -> Just  . (inst,) <$> to_ui alloc (Just old)
        (inst, Seq.First alloc) -> Just . (inst,) <$> to_ui alloc Nothing
        (_, Seq.Second _) -> return Nothing

to_ui :: Allocation -> Maybe UiConfig.Allocation
    -> Either Error UiConfig.Allocation
to_ui (Allocation _name qualified (Config mute solo) backend) old = do
    ui_backend <- case (backend, UiConfig.alloc_backend <$> old) of
        (Midi wdev chans, Just (UiConfig.Midi config)) -> return $
            UiConfig.Midi $ config
                { Midi.Patch.config_allocation =
                    [((wdev, chan), Nothing) | chan <- chans]
                }
        (Midi wdev chans, Nothing) -> return $
            UiConfig.Midi $ Midi.Patch.config
                [((wdev, chan), Nothing) | chan <- chans]
        (Midi {}, Just backend) ->
            Left $ "tried to turn into midi: " <> UiConfig.backend_name backend
        (NonMidi, Just (UiConfig.Midi {})) -> Left "midi inst with no channels"
        (NonMidi, Just backend) -> return backend
        (NonMidi, Nothing) -> Left "TODO should infer backend"
    return $ UiConfig.Allocation
        { alloc_qualified = qualified
        , alloc_config = (maybe Common.empty_config UiConfig.alloc_config old)
            { Common.config_mute = mute
            , Common.config_solo = solo
            }
        , alloc_backend = ui_backend
        }

-- * parse / unparse

p_allocation :: Parser Allocation
p_allocation = Allocation
    <$> (lexeme $ ">" *> (ScoreT.Instrument <$> p_word ""))
    <*> lexeme (InstT.parse_qualified <$> p_word "/")
    <*> lexeme p_config
    <*> p_backend

unparse_allocations :: [Allocation] -> Text
unparse_allocations = Text.unlines . Texts.columns 1 . map un_allocation

un_allocation :: Allocation -> [Text]
un_allocation (Allocation name qualified config backend) =
    [ ">" <> ScoreT.instrument_name name
    , InstT.show_qualified qualified
    , un_config config
    , un_backend backend
    ]

p_config :: Parser Config
p_config = A.option empty_config $ ParseText.between (A.char '[') (A.char ']') $
    check . untxt =<< A.takeWhile (/=']')
    where
    check cs
        | all ((`elem` ['m', 's']) . Char.toLower) cs =
            return $ Config
                { config_mute = 'M' `elem` cs
                , config_solo = 'S' `elem` cs
                }
        | otherwise = fail $ "flags must be [MSms]: " <> show cs

un_config :: Config -> Text
un_config (Config mute solo) =
    "[" <> (if mute then "M" else "m") <> (if solo then "S" else "s") <> "]"

p_backend :: Parser Backend
p_backend =
    Midi <$> lexeme (Midi.write_device <$> p_word "")
        <*> A.many1 (lexeme p_chan)
    <|> pure NonMidi
    where
    p_chan = do
        chan <- ParseText.p_nat
        if Num.inRange 1 17 chan then return $ fromIntegral (chan - 1)
            else fail $ "midi channel should be in range 1--16: " <> show chan

un_backend :: Backend -> Text
un_backend = \case
    Midi wdev chans ->
        Text.unwords $ Midi.write_device_text wdev : map (showt . (+1)) chans
    NonMidi -> ""

-- * util

p_word :: [Char] -> Parser Text
p_word extra = A.takeWhile1 $ \c -> any ($c)
    [ Char.isAsciiLower, Char.isAsciiUpper, Char.isDigit
    , (`elem` ("-" :: [Char]))
    , (`elem` extra)
    ]

spaces :: Parser ()
spaces = A.skipMany $
    ("--" *> A.skipWhile (/='\n') *> A.skipWhile (=='\n'))
    <|> A.skipMany1 (A.satisfy $ \c -> c == ' ' || c == '\n')

lexeme :: Parser a -> Parser a
lexeme = (<* spaces)
