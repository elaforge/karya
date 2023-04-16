-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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

    A more traditional and simpler way would be to make the ky source be the
    source of truth, but that would mean having it serialize all the fields.  I
    got pretty far down this route, but it got pretty complicated because
    there's a whole new expression language and serialization layer, and I
    become unsure if it was really a good idea.

    So we do complicated merging for now.
-}
module Derive.Parse.Instruments (
    Allocation(..)
    , Config(..), empty_config
    , Backend(..)
    , from_ui
    , instrument_section
    , update_ui
    -- * parse
    , p_allocation
    , unparse_allocations
) where
import qualified Data.Char as Char
import qualified Data.Text as Text

import qualified Util.Maps as Maps
import qualified Util.Num as Num
import qualified Util.P as P
import qualified Util.Parse as Parse
import qualified Util.Lists as Lists
import qualified Util.Texts as Texts

import qualified Derive.ScoreT as ScoreT
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT

import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Midi.Patch
import qualified Ui.UiConfig as UiConfig

import           Global


type Parser a = P.Parser a
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
        case Lists.groupFst $ map fst $ Midi.Patch.config_allocation config of
            [(wdev, chans)] -> Right $ Midi wdev chans
            allocs -> Left $ "midi config too complicated for: " <> showt allocs

type LookupBackend = InstT.Qualified -> Maybe Inst.Backend

-- | Merge the Allocations parsed from the instrument section into the
-- Ui level config.
update_ui :: LookupBackend -> [Allocation]
    -> UiConfig.Allocations -> Either Error UiConfig.Allocations
update_ui lookup_backend allocs (UiConfig.Allocations olds) = do
    allocs <- check $ Maps.unique2 $ Lists.keyOn alloc_name allocs
    inst_allocs <- mapMaybeM inherit $ Maps.pairs allocs olds
    add_allocations lookup_backend mempty inst_allocs
    where
    check (m, []) = return m
    check (_, dups) = Left $ "duplicate names: "
        <> Text.unwords (map (ScoreT.instrument_name . fst) dups)
    -- Since a parsed Allocation doesn't have all possible data, inherit
    -- the rest from an already existing allocation with this name.
    inherit = \case
        (inst, Lists.Both alloc old) ->
            Just  . (inst,) <$> to_ui lookup_backend alloc (Just old)
        (inst, Lists.First alloc) ->
            Just . (inst,) <$> to_ui lookup_backend alloc Nothing
        (_, Lists.Second _) -> return Nothing

add_allocations :: LookupBackend
    -> UiConfig.Allocations -> [(ScoreT.Instrument, UiConfig.Allocation)]
    -> Either Error UiConfig.Allocations
add_allocations lookup_backend = foldM add
    where
    add allocs (inst, alloc) = do
        let qual = UiConfig.alloc_qualified alloc
        backend <- tryJust ("patch not found: " <> pretty qual) $
            lookup_backend qual
        UiConfig.allocate backend inst alloc allocs

to_ui :: LookupBackend -> Allocation -> Maybe UiConfig.Allocation
    -> Either Error UiConfig.Allocation
to_ui lookup_backend (Allocation _name qual (Config mute solo) backend) old = do
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
        (NonMidi, Nothing) -> case lookup_backend qual of
            Nothing -> Left $ "patch not found: " <> pretty qual
            Just backend -> return $ UiConfig.convert_backend backend
    return $ UiConfig.Allocation
        { alloc_qualified = qual
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

type Comment = Text

unparse_allocations :: [(Maybe Allocation, Comment)] -> [Text]
unparse_allocations allocs = Texts.columnsSome 1
    [ maybe (Left cmt) (Right . (++cmts) . un_allocation) mb_alloc
    | (mb_alloc, cmt) <- allocs
    , let cmts = filter (/="") [cmt]
    ]

un_allocation :: Allocation -> [Text]
un_allocation (Allocation name qualified config backend) =
    [ ">" <> ScoreT.instrument_name name
    , InstT.show_qualified qualified
    , un_config config
    , un_backend backend
    ]

p_config :: Parser Config
p_config = P.option empty_config $ P.between (P.char '[') (P.char ']') $
    check . untxt =<< P.takeWhile (/=']')
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
        <*> P.some (lexeme p_chan)
    <|> pure NonMidi
    where
    p_chan = do
        chan <- Parse.p_nat
        if Num.inRange 1 17 chan then return $ fromIntegral (chan - 1)
            else fail $ "midi channel should be in range 1--16: " <> show chan

un_backend :: Backend -> Text
un_backend = \case
    Midi wdev chans ->
        Text.unwords $ Midi.write_device_text wdev : map (showt . (+1)) chans
    NonMidi -> ""

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
