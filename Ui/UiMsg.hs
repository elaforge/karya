{- | Receive events from the C++ UI layer, package them up nicely, and forward
    them on to the event handling mechanism (the responder).
-}
module Ui.UiMsg where
import Text.Printf

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Types as Types
import qualified Ui.Key as Key


-- | Technically not all UiMsgs have a Context, but it's easier to give
-- everyone a Context since it can be all Nothing anyway.
data UiMsg = UiMsg Context Msg
    deriving (Show)

data Context = Context
    { ctx_block :: Maybe ViewId
    -- | Index into block tracks.
    , ctx_track :: Maybe TrackNum
    , ctx_pos :: Maybe ScoreTime
    } deriving (Show)

-- | Corresponds to UiMsg::MsgType enum.
data Msg = MsgEvent Data | UiUpdate UiUpdate | MsgClose
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
    -- | Give screen dimensions: screen number, total screens, rect
    | UpdateScreenSize Int Int Types.Rect
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

instance Pretty.Pretty UiMsg where
    pretty ui_msg = case ui_msg of
        UiMsg ctx (MsgEvent mdata) -> case mdata of
            Mouse mstate coords clicks is_click ->
                printf "Mouse: %s %s %s click: %s %d" (show mstate)
                    (show coords) (Pretty.pretty ctx) (show is_click) clicks
            Kbd kstate key -> printf "Kbd: %s %s" (show kstate) (show key)
            AuxMsg msg -> printf "Aux: %s %s" (show msg) (Pretty.pretty ctx)
            Unhandled x -> printf "Unhandled: %d" x
        UiMsg ctx msg ->
            printf "Other Event: %s %s" (show msg) (Pretty.pretty ctx)

instance Pretty.Pretty Context where
    pretty (Context block tracknum pos) = "{" ++ contents ++ "}"
        where
        contents = Seq.join " " (filter (not.null) [show_maybe "block" block,
            show_maybe "tracknum" tracknum, pretty_maybe "pos" pos])
        show_maybe desc = maybe "" (\v -> desc ++ "=" ++ show v)
        pretty_maybe desc = maybe "" (\v -> desc ++ "=" ++ Pretty.pretty v)
