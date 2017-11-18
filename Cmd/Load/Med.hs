-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Parse MED output to ModTypes.Mod.
module Cmd.Load.Med where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Sound.MED.Generic as MED
import qualified Sound.MED.Generic.Block as Block
import qualified Sound.MED.Generic.Instrument as Instrument
import qualified Sound.MED.Generic.PlaySeq as PlaySeq

import qualified Util.Seq as Seq
import qualified Cmd.Load.ModTypes as M
import qualified Derive.ScoreTypes as ScoreTypes
import Global


load :: Map Text Text -> FilePath -> IO M.Module
load inst_map fn = convert inst_map <$> MED.load fn

convert :: Map Text Text -> MED.MED -> M.Module
convert inst_map med = M.Module
    { _instruments = map (instrument inst_map) (MED.instrs med)
    , _default_tempo = M.Tempo 33 6 -- TODO
    , _blocks = map block (MED.blocks med)
    , _block_order =
        [ (txt name, indices)
        | PlaySeq.MEDPlaySeq name indices <- MED.playseqs med
        ]
    }

instrument :: Map Text Text -> Instrument.MEDInstrument -> M.Instrument
instrument inst_map inst = M.Instrument
    { _instrument_name = maybe "none"
        (ScoreTypes.Instrument . find . txt) (Instrument.name inst)
    , _volume = M.volume <$> Instrument.svol inst
    }
    where
    find n = Map.findWithDefault n n inst_map

block :: Block.MEDBlock -> M.Block
block b = M.Block
    { _tracks = map track $ Seq.rotate2 $ map snd $ Block.seqdata b
    , _block_length = Block.lines b
    }

track :: [Maybe (Block.Note, Block.Inst, [(Block.Cmd, Block.Val)])] -> [M.Line]
track = default_zeroes . map (maybe empty note)
    where empty = M.Line Nothing 0 []

default_zeroes :: [M.Line] -> [M.Line]
default_zeroes = snd . List.mapAccumL set (0, mempty)
    where
    set (inst, prev_cmds) line =
        ((M._instrument line2, next_cmds), line2 { M._commands = cmds })
        where
        line2 = set_inst inst line
        (next_cmds, cmds) = set_cmds prev_cmds (M._commands line)
    set_cmds prev_cmds cmds =
        ( Map.fromList defaulted <> prev_cmds
        , parsed ++ map (uncurry M.Command) defaulted
        )
        where
        (unparsed, parsed) = Seq.partition_on is_unparsed cmds
        defaulted = map (set_default prev_cmds) unparsed
    set_default prev_cmds (cmd, val) =
        (cmd, if val /= 0 then val else Map.findWithDefault 0 cmd prev_cmds)
    is_unparsed (M.Command cmd val) = Just (cmd, val)
    is_unparsed _ = Nothing
    set_inst inst line
        | M._instrument line == 0 = line { M._instrument = inst }
        | otherwise = line

note :: (Block.Note, Block.Inst, [(Block.Cmd, Block.Val)]) -> M.Line
note (pitch, inst, cmds) = M.Line
    { _pitch = M.pitch pitch
    , _instrument = inst
    , _commands = M.commands cmds
    }
