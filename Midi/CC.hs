-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Constants for control change numbers.
module Midi.CC where
import qualified Data.Word as Word


type Control = Word.Word8

bank, bankLsb :: Control
bank = 0x0
bankLsb = 0x20

mod, modLsb :: Control
mod = 0x1
modLsb = 0x21

breath, breathLsb :: Control
breath = 0x2
breathLsb = 0x22

foot, footLsb :: Control
foot = 0x04
footLsb = 0x24

portamentoTime, portamentoTimeLsb :: Control
portamentoTime = 0x5
portamentoTimeLsb = 0x25

dataEntry, dataEntryLsb :: Control
dataEntry = 0x6
dataEntryLsb = 0x26

volume, volumeLsb :: Control
volume = 0x7
volumeLsb = 0x27

balance, balanceRpn :: Control
balance = 0x8
balanceRpn = 0x28

pan, panLsb :: Control
pan = 0x0a
panLsb = 0x2a

nrpnLsb, nrpnMsb :: Control
nrpnLsb = 0x62
nrpnMsb = 0x63

rpnLsb, rpnMsb :: Control
rpnLsb = 0x64
rpnMsb = 0x65

-- | My own convention: vibrato speed, when vibrato depth is controlled by
-- 'mod'.
vibSpeed :: Control
vibSpeed = 3

-- | There are spare CCs which are ripe for the mapping.
cc14, cc15, cc16, cc17, cc18, cc19, cc20, cc21, cc22, cc23, cc24 :: Control
cc14 : cc15 : cc16 : cc17 : cc18 : cc19 : cc20 : cc21 : cc22 : cc23 : cc24 : _
    = [14..]

cc25, cc26, cc27, cc28, cc29, cc30, cc31 :: Control
cc25 : cc26 : cc27 : cc28 : cc29 : cc30 : cc31 : _ = [25..]
