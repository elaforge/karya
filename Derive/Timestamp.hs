{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
{- | Timetstamp is used by the various rendering subsystems as well as
Derive.Player.  It can't go in Player because of circular imports with
Midi.Midi.
-}
module Derive.Timestamp where

-- | An absolute timestamp, measured from some arbitrary starting position.
-- Since I use the default PortMidi timer, it's from the initialization of the
-- midi subsystem at the moment.
--
-- The resolution is milliseconds.
newtype Timestamp = Timestamp Integer deriving (Eq, Ord, Show, Num)
