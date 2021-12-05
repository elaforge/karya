-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Utilities for GHC's implicit call stacks feature.
module Util.CallStack (
    Stack
    , Caller(..), caller, showCaller

    , errorStack
    , errorIO
    , getStack, getStack1
    , throw
) where
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans as Trans
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import           Data.Text (Text)

import qualified GHC.Stack as Stack


-- | Add this to the context of a function to give stack-aware functions access
-- to its caller.
--
-- TODO use Stack.HasCallStack instead?  Or not, I already have Derive.Stack
-- and GHC.Stack.HasCallStack is pretty verbose.
type Stack = Stack.HasCallStack

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

showFrame :: (String, Stack.SrcLoc) -> String
showFrame (name, srcloc) =
    name <> ": " <> strip (Stack.srcLocFile srcloc) <> ":"
        <> show (Stack.srcLocStartLine srcloc)
    where
    strip ('.':'/':s) = s
    strip s = s

showCaller :: Caller -> Text
showCaller (Caller fname line) = Text.pack fname <> ":" <> Text.pack (show line)
showCaller NoCaller = "<no-caller>"

-- | This is like 'Stack.prettyCallStack', except more compact.
getStack :: Stack => Text
getStack = Text.intercalate "; " $ map (Text.pack . showFrame) $ reverse $
    Stack.getCallStack Stack.callStack

-- | Get only the top of the stack.  More compact than 'getStack'.
getStack1 :: Stack => Text
getStack1 = showCaller $ caller $ Stack.callStack

-- | This is for internal errors, which are unexpected and should "never
-- happen."  It's basically like 'Exception.ErrorCall', except that it's
-- an app-specific type rather than a generic one.  I don't know if the
-- distinction will ever be relevant, but there it is.  At least it's
-- greppable.
--
-- I use 'error' in tests and non-core code, but try to use 'errorIO' or
-- 'errorStack' code run by App.Main.
newtype Error = Error Text
    deriving (Show)

instance Exception.Exception Error where
    displayException (Error msg) = Text.unpack msg

-- | Like 'error', use when forced.
errorStack :: Stack => Text -> a
errorStack = Exception.throw . Error . ((getStack <> ": ") <>)

-- | Throw an error in IO, with a stack.
errorIO :: Stack => Trans.MonadIO m => Text -> m a
errorIO = Trans.liftIO . Exception.throwIO . Error . ((getStack <> ": ") <>)

throw :: (Stack, Exception.Exception e) => (Text -> e) -> Text -> a
throw toExc msg = Exception.throw (toExc (getStack <> ": " <> msg))
