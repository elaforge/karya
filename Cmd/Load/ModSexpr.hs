-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Support for loading mods in sexpr format, as written by mod_to_sexpr.c.
module Cmd.Load.ModSexpr (load) where
import qualified Data.Bits as Bits
import           Data.Bits ((.&.))
import qualified Data.Char as Char
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import qualified Util.ParseM as ParseM
import qualified Cmd.Load.ModT as ModT
import qualified Derive.ScoreT as ScoreT
import qualified Perform.Pitch as Pitch

import           Global


type Error = Text

load :: FilePath -> IO (Either Error ModT.Module)
load fname = (toModule <=< parse) <$> Text.IO.readFile fname

toModule :: Val -> Either Error ModT.Module
toModule val = do
    mod <- toMap val
    meta <- toMap . VList =<< get "metadata" mod
    bpm <- num =<< one =<< get "bpm" meta
    spd <- num =<< one =<< get "spd" meta

    insts <- get "instruments" mod
    insts <- forM insts $ \case
        VList [VNum n, VString inst] -> Right (n, mkinst inst)
        _ -> Left "expected (num inst) pair"

    order <- get "order" mod
    order <- mapM num $
        filter (/= VString "+") $ takeWhile (/= VString "-") order

    patterns <- get "patterns" mod
    blocks <- forM patterns $ \pattern -> do
        tracks <- list pattern
        (lens, tracks) <- unzip <$> mapM (getTrack <=< list) tracks
        pure $ ModT.Block (maximum (0:lens)) tracks
    pure $ ModT.Module
        { _instruments = IntMap.fromList insts
        , _defaultTempo = ModT.Tempo bpm spd
        , _blockOrder = Map.singleton "score" order
        , _blocks = blocks
        }

    where
    mkinst n = ModT.Instrument (ScoreT.Instrument (Text.strip n)) Nothing

getTrack :: [Val] -> Either Error (Int, ModT.Track)
getTrack (VSymbol "track" : VNum rows : notes) = do
    lines <- mapM (toLine <=< list) notes
    pure (rows, ModT.makeTrack lines)
getTrack vals = Left $ "expected track, got " <> pretty (map valType vals)

-- (row pitch inst vol (fx1 fx1param) (fx2 fx2param))
toLine :: [Val] -> Either Error (Int, ModT.Line)
toLine [idx, pitch, inst, vol, fx1, fx2] = do
    idx <- num idx
    pitch <- num pitch
    inst <- num inst
    vol <- num vol
    fxs <- mapMaybeM toCmd [fx1, fx2]
    pure $ (idx,) $ ModT.Line
        { _pitch = if 0 < pitch && pitch < 128
            -- c4 in schism tracker shows up as 49, instead of 60.
            then Just $ Pitch.nn $ pitch + 11
            else Nothing
        , _instrument = inst
        , _commands = concat
            [ [ModT.Volume $ fromIntegral (vol - 1) / 64 | vol > 0]
            , fxs
            -- In IT 130 is ^^^, which is CutNote.
            , [ModT.CutNote | pitch == 130]
            ]
        }
toLine val = Left $ "expected note, got " <> pretty (map valType val)

toCmd :: Val -> Either Error (Maybe ModT.Command)
toCmd (VList [VString fx, VNum arg])
    | fx == "" = Right Nothing
    | otherwise = Right $ Just (fxToCmd fx arg)
toCmd val = Left $ "expected fx, got " <> valType val

fxToCmd :: Text -> Int -> ModT.Command
fxToCmd fx arg = case fx of
    "volslide" -> case split4 arg of
        (0xf, n) -> ModT.VolumeSlide $ - fromIntegral n / 10
        (n, 0xf) -> ModT.VolumeSlide $ fromIntegral n / 10
        (0, n) -> ModT.VolumeSlide $ - fromIntegral n
        (n, 0) -> ModT.VolumeSlide $ fromIntegral n
        _ -> unknown
    "it_break" -> ModT.CutBlock
    _ -> unknown
    where
    unknown = ModT.Command fx (fromIntegral arg)

split4 :: Int -> (Int, Int)
split4 word = (Bits.shiftR word 4 .&. 0xf, word .&. 0xf)

toMap :: Val -> Either Error (Map Text [Val])
toMap xs = do
    xs <- list xs
    fmap Map.fromList $ forM xs $ \case
        VList (VSymbol sym : rest) -> Right (sym, rest)
        val -> Left $ "key, got " <> valType val

get :: Text -> Map Text a -> Either Error a
get k = maybe (Left $ "no key: " <> k) Right . Map.lookup k

one :: [Val] -> Either Error Val
one [x] = Right x
one xs = Left $ "expected one val, got " <> showt (length xs)

list :: Val -> Either Error [Val]
list = expect "list" $ \case
    VList a -> Just a
    _ -> Nothing

num :: Val -> Either Error Int
num = expect "num" $ \case
    VNum a -> Just a
    _ -> Nothing

expect :: Text -> (Val -> Maybe a) -> Val -> Either Error a
expect name get val =
    maybe (Left $ "expected " <> name <> ", got " <> valType val) Right $
        get val

-- * parse

data Val = VList [Val] | VSymbol !Text | VString !Text | VNum !Int
    deriving (Eq, Show)

valType :: Val -> Text
valType = \case
    VList {} -> "list"
    VSymbol {} -> "symbol"
    VString {} -> "string"
    VNum {} -> "num"

type Parser a = ParseM.Parser a

parse :: Text -> Either Error Val
parse = ParseM.parse pVal

pVal :: Parser Val
pVal = lexeme $ pList <|> pNum <|> pString <|> pSymbol

pList :: Parser Val
pList = fmap VList $
    lexeme (P.char '(') *> P.many pVal <* lexeme (P.char ')')

pSymbol :: Parser Val
pSymbol = VSymbol <$> P.takeWhile1P Nothing (\c -> 'a' <= c && c <= 'z')

pNum :: Parser Val
pNum = VNum <$> ParseM.pInt

pString :: Parser Val
pString = fmap VString $
    P.char '"' *> P.takeWhileP Nothing (\c -> c /= '"') <* P.char '"'

lexeme :: Parser a -> Parser a
lexeme p = p <* space

space :: Parser ()
space = P.takeWhileP Nothing Char.isSpace
    *> P.option () (P.char ';' *> P.takeWhileP Nothing (/='\n') *> space)
