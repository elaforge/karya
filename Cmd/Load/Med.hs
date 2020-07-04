-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Parse MED output to ModT.Mod.
module Cmd.Load.Med where
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Sound.MED.Generic as MED
import qualified Sound.MED.Generic.Block as Block
import qualified Sound.MED.Generic.Instrument as Instrument
import qualified Sound.MED.Generic.PlaySeq as PlaySeq

import qualified Util.Seq as Seq
import qualified Cmd.Load.ModT as ModT
import qualified Derive.ScoreT as ScoreT

import           Global


load :: FilePath -> IO ModT.Module
load fn = convert <$> MED.load fn

convert :: MED.MED -> ModT.Module
convert med = ModT.Module
    { _instruments = IntMap.fromList $ zip [1..] $
        map instrument (MED.instrs med)
        -- TODO I guess MED instruments start at 1?
    , _default_tempo = ModT.Tempo 33 6 -- TODO
    , _blocks = map block (MED.blocks med)
    , _block_order = Map.fromList
        [ (txt name, indices)
        | PlaySeq.MEDPlaySeq name indices <- MED.playseqs med
        ]
    }

instrument :: Instrument.MEDInstrument -> ModT.Instrument
instrument inst = ModT.Instrument
    { _instrument_name = maybe "none" (ScoreT.Instrument . txt) $
        Instrument.name inst
    , _volume = ModT.volume <$> Instrument.svol inst
    }

block :: Block.MEDBlock -> ModT.Block
block b = ModT.Block
    { _tracks = map track $ Seq.rotate2 $ map snd $ Block.seqdata b
    , _block_length = Block.lines b
    }

track :: [Maybe (Block.Note, Block.Inst, [(Block.Cmd, Block.Val)])]
    -> ModT.Track
track = ModT.make_track . zip [0..] . map (maybe empty note)
    where empty = ModT.Line Nothing 0 []

note :: (Block.Note, Block.Inst, [(Block.Cmd, Block.Val)]) -> ModT.Line
note (pitch, inst, cmds) = ModT.Line
    { _pitch = ModT.pitch pitch
    , _instrument = inst
    , _commands = ModT.commands cmds
    }
