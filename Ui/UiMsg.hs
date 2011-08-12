{- | Receive events from the C++ UI layer, package them up nicely, and forward
    them on to the event handling mechanism (the responder).
-}
module Ui.UiMsg where
import Text.Printf

import qualified Util.Pretty as Pretty
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Types as Types
import qualified Ui.Key as Key


-- | Technically not all UiMsgs have a Context, but it's easier to give
-- everyone a Context since it can be all Nothing anyway.
data UiMsg = UiMsg Context Msg
    deriving (Show)

data Context = Context
    { ctx_focus :: Maybe ViewId
    -- | Index into block tracks.
    , ctx_track :: Maybe TrackNum
    , ctx_pos :: Maybe ScoreTime
    } deriving (Show)

-- | Corresponds to UiMsg::MsgType enum.
--
-- Each UiUpdate has a ViewId which is separate from the focus.  This is
-- because they can happen to an unfocused view, e.g. on OS X UpdateViewResize
-- and UpdateClose.
data Msg =
    MsgEvent MsgEvent
    | UiUpdate ViewId UiUpdate
    -- | Give screen dimensions: screen number, total screens, rect.  This
    -- is a UiUpdate but it doesn't have a ViewId.
    | UpdateScreenSize Int Int Rect.Rect
    deriving (Eq, Ord, Show)

-- | These are generated when the UI is manipulated directly and makes changes
-- to its own state.  They are like Ui.Update except in the opposide
-- direction: fltk telling haskell what changes occurred.
data UiUpdate =
    UpdateInput String
    | UpdateTrackScroll Types.Width
    | UpdateZoom Types.Zoom
    -- | Size of entire block window, and (visible_track, visible_time).
    | UpdateViewResize Rect.Rect (Int, Int)
    | UpdateTrackWidth Types.Width
    -- | The given view was closed.
    | UpdateClose
    deriving (Eq, Ord, Show)

-- | MsgType.msg_event, which is a fltk event.
data MsgEvent =
    -- | (state, modifiers, coords, clicks, is_click)
    -- As per fltk, 0 is the first click, 1 is a double click, etc.
    Mouse
        { mouse_state :: MouseState
        , mouse_modifiers :: [Key.Modifier]
        , mouse_coords :: (Int, Int)
        -- | As per fltk, 0 is the first click, 1 is a double click, etc.
        , mouse_clicks :: Int
        , mouse_is_click :: Bool
        }
    | Kbd KbdState [Key.Modifier] Key.Key
    | AuxMsg AuxMsg
    | Unhandled Int
    deriving (Eq, Ord, Show)
    -- The presence of [Key.Modifier] in Kbd and Mouse is ugly because it's
    -- only for cmd_record_keys.  All the rest should use Cmd.state_keys_down.
    -- TODO maybe this should move to MsgEvent so FOCUS can update the
    -- modifiers?  Doesn't matter as long as fltk doesn't support it.

-- | Most of these are unused, but are included here for completeness.
data AuxMsg = Enter | Leave | Focus | Unfocus | Shortcut | Deactivate
    | Activate | Hide | Show
    deriving (Eq, Ord, Show)

data MouseState = MouseMove | MouseDrag MouseButton
    | MouseDown MouseButton | MouseUp MouseButton
    deriving (Eq, Ord, Show)
data KbdState = KeyDown | KeyRepeat | KeyUp deriving (Eq, Ord, Show)

type MouseButton = Int

instance Pretty.Pretty UiMsg where
    pretty ui_msg = case ui_msg of
        UiMsg ctx (MsgEvent mdata) -> case mdata of
            Mouse mstate mods coords clicks is_click ->
                printf "Mouse: %s %s %s %s click: %s %d" (show mstate)
                    (show mods) (show coords) (Pretty.pretty ctx)
                    (show is_click) clicks
            Kbd kstate mods key -> printf "Kbd: %s %s %s %s" (show kstate)
                (show mods) (show key) (Pretty.pretty ctx)
            AuxMsg msg -> printf "Aux: %s %s" (show msg) (Pretty.pretty ctx)
            Unhandled x -> printf "Unhandled: %d" x
        UiMsg ctx msg ->
            printf "Other Event: %s %s" (show msg) (Pretty.pretty ctx)

instance Pretty.Pretty Context where
    pretty (Context focus tracknum pos) = "{" ++ contents ++ "}"
        where
        contents = Seq.join " " (filter (not.null)
            [show_maybe "focus" focus, show_maybe "tracknum" tracknum,
                pretty_maybe "pos" pos])
        show_maybe desc = maybe "" (\v -> desc ++ "=" ++ show v)
        pretty_maybe desc = maybe "" (\v -> desc ++ "=" ++ Pretty.pretty v)
