import Data.Char
import qualified System.FilePath as FilePath

main = interact (unlines . map process . lines)

process (' ':line) = ' ':line
-- foo.o: x/y.cc
process line = FilePath.takeDirectory ccfile ++ "/" ++ line
    where
    (ofile, rest) = break (==':') line
    ccfile = (takeWhile (not.isSpace) . dropWhile isSpace . tail) rest
