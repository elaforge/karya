-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Hash 'Note.Note's to skip rerendering when possible.
module Synth.Faust.Hash where
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64.URL as Base64.URL
import qualified Data.ByteString.Char8 as ByteString.Char8

import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize
import qualified Synth.Shared.Note as Note
import Synth.Lib.Global


fingerprint :: Serialize.Serialize a => a -> ByteString.ByteString
fingerprint = fst . ByteString.Char8.spanEnd (=='=') . Base64.URL.encode
    . Serialize.encode

overlapping :: RealTime -> RealTime -> [Note.Note] -> [Note.Hash]
overlapping start size =
    map (mconcat . map fst) . groupOverlapping start size . Seq.key_on Note.hash
    -- Pair each Note with its Hash, then group Notes and combine the Hashes.
    -- This ensures I only compute each Hash a maximum of once.

groupOverlapping :: RealTime -> RealTime -> [(a, Note.Note)]
    -> [[(a, Note.Note)]]
groupOverlapping start size = go (Seq.range_ start size)
    -- Use Seq.range_ instead of successive addition to avoid accumulating
    -- error.  Size should integral, but let's just be careful.
    where
    go (t1 : ts@(t2 : _)) notes
        | null notes = []
        | null overlapping && null rest = []
        | otherwise = overlapping : go ts rest
        where (overlapping, rest) = splitOverlapping t1 t2 notes
    go _ _ = []

{-
    0   1   2   3   4   5   6   7   8
    +---
        +-------------------
            +---
                    +---
-}
splitOverlapping :: RealTime -> RealTime -> [(a, Note.Note)]
    -> ([(a, Note.Note)], [(a, Note.Note)])
splitOverlapping start end notes = (overlapping, overlapping ++ rest)
    where
    overlapping = filter (not . (<=start) . Note.end . snd) here
    (here, rest) = span ((<end) . Note.start . snd) $
        dropWhile ((<=start) . Note.end . snd) notes
