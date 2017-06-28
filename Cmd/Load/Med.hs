-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Parse MED output to ModTypes.Mod.
module Cmd.Load.Med where
import qualified Control.Exception as Exception
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Writer.Strict as Writer

import qualified Data.Set as Set
import qualified Unmed2.Amiga as Amiga
import qualified Unmed2.MED as MED
import qualified Unmed2.MEDBlock as MEDBlock
import qualified Unmed2.MEDInstrument as MEDInstrument

import qualified Util.Seq as Seq
import qualified Cmd.Load.ModTypes as M
import qualified Derive.ScoreTypes as ScoreTypes
import Global


load :: FilePath -> IO (M.Module, Set Log)
load fn = Exception.bracket (Amiga.loadMEM fn) Amiga.freeMEM $ \mem ->
    convert <$> MED.peek mem

type M a = Writer.WriterT (Set Log) Identity.Identity a
data Log = IgnoredCommand !Int !Int
    deriving (Eq, Ord, Show)

convert :: MED.MED -> (M.Module, (Set Log))
convert med = Writer.runWriter $ do
    blocks <- mapM block (MED.blocks med)
    return $ M.Module
        { _instruments = map instrument (MED.instrs med)
        , _default_tempo = M.Tempo 33 6 -- TODO
        , _blocks = blocks
        , _block_order =
            [(name, indices) | MED.PlaySeq name indices <- MED.playseqs med]
        }

instrument :: MEDInstrument.MEDInstrument -> M.Instrument
instrument inst = M.Instrument
    { _instrument_name = maybe (ScoreTypes.Instrument "none")
        (ScoreTypes.Instrument . txt) (MEDInstrument.name inst)
    , _volume = MEDInstrument.svol inst
    }

block :: MEDBlock.MEDBlock -> M M.Block
block b = do
    tracks <- mapM track $ Seq.rotate2 $ map snd $ MEDBlock.seqdata b
    return $ M.Block
        { _tracks = tracks
        , _block_length = MEDBlock.lines b
        }

track :: [Maybe (MEDBlock.Note, MEDBlock.Inst, [(MEDBlock.Cmd, MEDBlock.Val)])]
    -> M [M.Line]
track = mapM (maybe (return empty) note)
    where empty = M.Line Nothing 0 []

note :: (MEDBlock.Note, MEDBlock.Inst, [(MEDBlock.Cmd, MEDBlock.Val)])
    -> M M.Line
note (pitch, inst, cmds) = do
    mapM_ (Writer.tell . Set.singleton . uncurry IgnoredCommand) unrecognized
    return $ M.Line
        { _pitch = M.pitch pitch
        , _instrument = inst
        , _commands = ok
        }
    where (ok, unrecognized) = M.commands cmds
