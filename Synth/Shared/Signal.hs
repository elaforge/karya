-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | 'Signal' implementation.
module Synth.Shared.Signal (
    module Synth.Shared.Signal, module Perform.Signal
) where
import Perform.Signal hiding (Signal, Tempo, Warp, Control, Display)
import qualified Perform.Signal as Signal

type Signal = Signal.Control
