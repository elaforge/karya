-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Constants for control change numbers.
module Midi.CC where
import qualified Data.Word as Word


type Control = Word.Word8

bank, bank_lsb :: Control
bank = 0x0
bank_lsb = 0x20

mod, mod_lsb :: Control
mod = 0x1
mod_lsb = 0x21

breath, breath_lsb :: Control
breath = 0x2
breath_lsb = 0x22

foot, foot_lsb :: Control
foot = 0x04
foot_lsb = 0x24

portamento_time, portamento_time_lsb :: Control
portamento_time = 0x5
portamento_time_lsb = 0x25

data_entry, data_entry_lsb :: Control
data_entry = 0x6
data_entry_lsb = 0x26

volume, volume_lsb :: Control
volume = 0x7
volume_lsb = 0x27

balance, balance_rpn :: Control
balance = 0x8
balance_rpn = 0x28

pan, pan_lsb :: Control
pan = 0x0a
pan_lsb = 0x2a

nrpn_lsb, nrpn_msb :: Control
nrpn_lsb = 0x62
nrpn_msb = 0x63

rpn_lsb, rpn_msb :: Control
rpn_lsb = 0x64
rpn_msb = 0x65
