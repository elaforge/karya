{- | Receive events from the C++ UI layer, package them up nicely, and forward
    them on to the event handling mechanism (the responder).
-}
module Ui.UiMsg where
import Foreign
import Text.Printf

import qualified Util.Seq as Seq

import Ui
import qualified Ui.Types as Types
import qualified Ui.Key as Key


-- | Technically MsgClose and whatnot don't have ctx_track and ctx_pos, but
-- it's easier to give everyone Context.
-- These all derive Ord so they can go in Sets and Maps.
data UiMsg = UiMsg Context Msg
    deriving (Show)

data Context = Context
    { ctx_block :: Maybe ViewId
    -- | Index into block tracks.
    , ctx_track :: Maybe TrackNum
    , ctx_pos :: Maybe TrackPos
    } deriving (Show)

-- | Corresponds to UiMsg::MsgType enum.
data Msg = MsgEvent Data | UiUpdate UiUpdate
    | MsgClose
    deriving (Eq, Ord, Show)

-- | These are generated when the UI is manipulated directly and makes changes
-- to its own state.  They are like Ui.Update except in the opposide direction:
-- fltk telling haskell what changes occurred.

-- TODO include the arg vals so I don't have to call back into fltk
data UiUpdate =
    UpdateInput String
    | UpdateTrackScroll Types.Width
    | UpdateZoom Types.Zoom
    -- | Size of entire block window, and (visible_track, visible_time).
    | UpdateViewResize Types.Rect (Int, Int)
    | UpdateTrackWidth Types.Width
    deriving (Eq, Ord, Show)

-- TODO this makes partial selectors... would it be better to split this up?
data Data =
    Mouse
        { mouse_state :: MouseState
        , mouse_coords :: (Int, Int)
        -- | As per fltk, 0 is the first click, 1 is a double click, etc.
        , mouse_clicks :: Int
        , mouse_is_click :: Bool
        }
    | Kbd KbdState Key.Key
    | AuxMsg AuxMsg
    | Unhandled Int
    deriving (Eq, Ord, Show)

data AuxMsg = Enter | Leave | Focus | Unfocus | Shortcut | Deactivate
    | Activate | Hide | Show
    deriving (Eq, Ord, Show)

data MouseState = MouseMove | MouseDrag MouseButton
    | MouseDown MouseButton | MouseUp MouseButton
    deriving (Eq, Ord, Show)
data KbdState = KeyDown | KeyUp deriving (Eq, Ord, Show)

type MouseButton = Int

pretty_ui_msg :: UiMsg -> String
pretty_ui_msg (UiMsg ctx (MsgEvent mdata)) = case mdata of
    Mouse mstate coords clicks is_click ->
        printf "Mouse: %s %s %s click: %s %d" (show mstate) (show coords)
            (pretty_context ctx) (show is_click) clicks
    Kbd kstate key -> printf "Kbd: %s %s" (show kstate) (show key)
    AuxMsg msg -> printf "Aux: %s %s" (show msg) (pretty_context ctx)
    Unhandled x -> printf "Unhandled: %d" x
pretty_ui_msg (UiMsg ctx msg)
    = printf "Other Event: %s %s" (show msg) (pretty_context ctx)

pretty_context (Context block tracknum pos) = "{" ++ contents ++ "}"
    where
    contents = Seq.join " " (filter (not.null) [show_maybe "block" block,
        show_maybe "tracknum" tracknum, show_maybe "pos" pos])
    show_maybe _ Nothing = ""
    show_maybe desc (Just x) = desc ++ "=" ++ show x
