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

import qualified Util.Parse as Parse
import qualified Cmd.Load.ModT as ModT
import qualified Derive.ScoreT as ScoreT
import qualified Perform.Pitch as Pitch

import           Global


type Error = Text

load :: FilePath -> IO (Either Error ModT.Module)
load fname = (to_module <=< parse) <$> Text.IO.readFile fname

to_module :: Val -> Either Error ModT.Module
to_module val = do
    mod <- to_map val
    meta <- to_map . VList =<< get "metadata" mod
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
        (lens, tracks) <- unzip <$> mapM (get_track <=< list) tracks
        return $ ModT.Block tracks (maximum (0:lens))
    return $ ModT.Module
        { _instruments = IntMap.fromList insts
        , _default_tempo = ModT.Tempo bpm spd
        , _block_order = Map.singleton "default" order
        , _blocks = blocks
        }

    where
    mkinst n = ModT.Instrument (ScoreT.Instrument (Text.strip n)) Nothing

get_track :: [Val] -> Either Error (Int, ModT.Track)
get_track (VSymbol "track" : VNum rows : notes) = do
    lines <- mapM (to_line <=< list) notes
    return (rows, ModT.make_track lines)
get_track vals = Left $ "expected track, got " <> pretty (map val_type vals)

-- (row nn instrument vol (fx1 fx1param) (fx2 fx2param))
to_line :: [Val] -> Either Error (Int, ModT.Line)
to_line [idx, nn, inst, vol, fx1, fx2] = do
    idx <- num idx
    pitch <- (\n -> if n == 0 then Nothing else Just (Pitch.nn n)) <$> num nn
    inst <- num inst
    vol <- num vol
    fxs <- mapMaybeM to_cmd [fx1, fx2]
    let cmds = [ModT.Volume $ fromIntegral (vol - 1) / 64 | vol > 0] ++ fxs
    return (idx, ModT.Line pitch inst cmds)
to_line val = Left $ "expected note, got " <> pretty (map val_type val)

to_cmd :: Val -> Either Error (Maybe ModT.Command)
to_cmd (VList [VString fx, VNum arg])
    | fx == "" = Right Nothing
    | otherwise = Right $ Just (fx_to_cmd fx arg)
to_cmd val = Left $ "expected fx, got " <> val_type val

fx_to_cmd :: Text -> Int -> ModT.Command
fx_to_cmd fx arg = case fx of
    "volslide" -> case split4 arg of
        (0xf, n) -> ModT.VolumeSlideFine (-n)
        (n, 0xf) -> ModT.VolumeSlideFine (n)
        (0, n) -> ModT.VolumeSlide (-n)
        (n, 0) -> ModT.VolumeSlide n
        _ -> unknown
    "it_break" -> ModT.CutBlock
    _ -> unknown
    where
    unknown = ModT.Command fx (fromIntegral arg)

split4 :: Int -> (Int, Int)
split4 word = (Bits.shiftR word 4 .&. 0xf, word .&. 0xf)

to_map :: Val -> Either Error (Map Text [Val])
to_map xs = do
    xs <- list xs
    fmap Map.fromList $ forM xs $ \case
        VList (VSymbol sym : rest) -> Right (sym, rest)
        val -> Left $ "key, got " <> val_type val

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
    maybe (Left $ "expected " <> name <> ", got " <> val_type val) Right $
        get val

-- * parse

data Val = VList [Val] | VSymbol !Text | VString !Text | VNum !Int
    deriving (Eq, Show)

val_type :: Val -> Text
val_type = \case
    VList {} -> "list"
    VSymbol {} -> "symbol"
    VString {} -> "string"
    VNum {} -> "num"

type Parser a = Parse.Parser a

parse :: Text -> Either Error Val
parse = Parse.parse p_val

p_val :: Parser Val
p_val = lexeme $ p_list <|> p_num <|> p_string <|> p_symbol

p_list :: Parser Val
p_list = fmap VList $
    lexeme (P.char '(') *> P.many p_val <* lexeme (P.char ')')

p_symbol :: Parser Val
p_symbol = VSymbol <$> P.takeWhile1P Nothing (\c -> 'a' <= c && c <= 'z')

p_num :: Parser Val
p_num = VNum <$> Parse.p_int

p_string :: Parser Val
p_string = fmap VString $
    P.char '"' *> P.takeWhileP Nothing (\c -> c /= '"') <* P.char '"'

lexeme :: Parser a -> Parser a
lexeme p = p <* space

space :: Parser ()
space = P.takeWhileP Nothing Char.isSpace
    *> P.option () (P.char ';' *> P.takeWhileP Nothing (/='\n') *> space)
