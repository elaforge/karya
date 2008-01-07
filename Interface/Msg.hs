{-
As much functionality as possible is implemented at the app level, not the C++
UI level.  Except some hardcoded actions like selections and zooming, keyboard
and mouse events are dropped into a block-specific event queue.  The app must
receive those events and make the appropriate API calls.  This is so I can
dynamically change mouse and keyboard mapping at the haskell level.
-}

module Interface.Msg where

data Msg = Msg Context Data

data Context = Context
    { ctx_block :: Maybe Block
    , ctx_track :: Maybe Track
    , ctx_event :: Maybe Event
    } -- You can get the selection by asking the Track or Event.

data Data = Mouse MouseMsg | Kbd KbdMsg | Midi MidiMsg deriving (Show)

data MouseMsg = MouseMsg
    { mouse_down :: Bool
    , mouse_button :: Int
    } deriving (Show)

data KbdMsg = KbdMsg Char deriving (Show)

data MidiMsg = MidiMsg deriving (Show) -- unimpl

receive :: UI Msg -- | block until a msg is received
received = undefined
