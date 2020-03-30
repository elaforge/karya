-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Utilities for GHC's implicit call stacks feature.
module Util.CallStack where
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans as Trans
import qualified Data.Text as Text
import Data.Text (Text)
import qualified GHC.Stack as Stack

import qualified Data.Aeson as Aeson


-- | Add this to the context of a function to give stack-aware functions access
-- to its caller.
type Stack = Stack.HasCallStack

callStack :: Stack => Stack.CallStack
callStack = Stack.callStack

-- | Simplified stack with just the immediate caller.
data Caller = Caller !FilePath !Int | NoCaller deriving (Eq, Show, Read)

instance Aeson.ToJSON Caller where
    toJSON (Caller fname line) = Aeson.toJSON (fname, line)
    toJSON NoCaller = Aeson.Null
instance Aeson.FromJSON Caller where
    parseJSON val = case val of
        Aeson.Null -> return NoCaller
        _ -> uncurry Caller <$> Aeson.parseJSON val

caller :: Stack.CallStack -> Caller
caller stack = case reverse (Stack.getCallStack stack) of
    (_, srcloc) : _ ->
        Caller (strip (Stack.srcLocFile srcloc))
            (Stack.srcLocStartLine srcloc)
    [] -> NoCaller
    where
    strip ('.':'/':s) = s
    strip s = s

showCaller :: Caller -> Text
showCaller (Caller fname line) = Text.pack fname <> ":" <> Text.pack (show line)
showCaller NoCaller = "<no-caller>"

showStack :: Stack.CallStack -> Text
showStack = showCaller . caller

getStack :: Stack => Text
getStack = showStack Stack.callStack

-- | Just like 'error', except show the caller's location.
errorStack :: Stack => Text -> a
errorStack msg = error $ Text.unpack $ getStack <> ": " <> msg

-- | Like 'errorStack', except run in IO.
errorIO :: Stack => Trans.MonadIO m => Text -> m a
errorIO = Trans.liftIO . Exception.throwIO . Exception.ErrorCall
    . Text.unpack . ((getStack <> ": ") <>)

throw :: (Stack, Exception.Exception e) => (Text -> e) -> Text -> a
throw toExc msg = Exception.throw (toExc (getStack <> ": " <> msg))
