{- | Receive events from the C++ UI layer, package them up nicely, and forward
    them on to the event handling mechanism (the responder).
-}
module Ui.UiMsg where
import Text.Printf

import qualified Util.Pretty as Pretty
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq

import qualified Ui.Key as Key
import qualified Ui.Types as Types
import Types


-- | Technically not all UiMsgs have a Context, but it's easier to give
-- everyone a Context since it can be all Nothing anyway.
data UiMsg = UiMsg Context Msg
    deriving (Show)

data Context = Context
    { ctx_focus :: Maybe ViewId
    -- | Index into block tracks.  The TrackNum will be one greater than the
    -- last track if the cursor has moved beyond it.
    , ctx_track :: Maybe (TrackNum, Track)
    } deriving (Show)

-- | Whether the context is on the track itself or the skeleton display above
-- the track.
--
-- There are a few cases where there is a track but no position and it
-- *doesn't* mean SkeletonDisplay, namely UpdateTrackWidth and UpdateInput.
-- However, those cases are handled in a specific place while Track goes on
-- to become a mouse Modifier, so I don't mind if the former is a little
-- awkward for the benefit of the latter.
data Track = Track ScoreTime | SkeletonDisplay | Divider
    deriving (Eq, Ord, Read, Show)
    -- (Eq, Ord, Read) needed because this is in Cmd.Modifier

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
    -- | Size of entire block window, and (track_padding, time_padding).
    | UpdateViewResize Rect.Rect (Int, Int)
    | UpdateTrackWidth Types.Width
    -- | The given view was closed.
    | UpdateClose
    deriving (Eq, Ord, Show)

-- | MsgType.msg_event, which is a fltk event.
data MsgEvent =
    Mouse
        { mouse_state :: MouseState
        , mouse_modifiers :: [Key.Modifier]
        , mouse_coords :: (Int, Int)
        -- | As per fltk, 0 is the first click, 1 is a double click, etc.
        , mouse_clicks :: Int
        , mouse_is_click :: Bool
        }
    -- | The Char is the text that this key wants to enter, if any.  They Key
    -- is just the keycap, without taking shift into account.
    | Kbd KbdState [Key.Modifier] Key.Key (Maybe Char)
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

data MouseState = MouseMove | MouseDrag Types.MouseButton
    | MouseDown Types.MouseButton | MouseUp Types.MouseButton
    deriving (Eq, Ord, Show)
data KbdState = KeyDown | KeyRepeat | KeyUp deriving (Eq, Ord, Show)

instance Pretty.Pretty UiMsg where
    pretty ui_msg = case ui_msg of
        UiMsg ctx (MsgEvent mdata) -> case mdata of
            Mouse mstate mods coords clicks is_click ->
                printf "Mouse: %s %s %s %s click: %s %d" (show mstate)
                    (show mods) (show coords) (Pretty.pretty ctx)
                    (show is_click) clicks
            Kbd kstate mods key text -> printf "Kbd: %s %s %s %s%s"
                (show kstate) (show mods)
                (show key) (maybe "" (\c -> '(':c:") ") text)
                (Pretty.pretty ctx)
            AuxMsg msg -> printf "Aux: %s %s" (show msg) (Pretty.pretty ctx)
            Unhandled x -> printf "Unhandled: %d" x
        UiMsg ctx msg ->
            printf "Other Event: %s %s" (show msg) (Pretty.pretty ctx)

instance Pretty.Pretty Context where
    pretty (Context focus track) = "{" ++ contents ++ "}"
        where
        contents = Seq.join " " $ filter (not.null)
            [show_maybe "focus" focus, maybe "" show_track track]
        show_track (tnum, track) =
            "track=" ++ show tnum ++ ":" ++ Pretty.pretty track
        show_maybe desc = maybe "" (\v -> desc ++ "=" ++ show v)

instance Pretty.Pretty Track where
    pretty (Track pos) = "track:" ++ Pretty.pretty pos
    pretty Divider = "div"
    pretty SkeletonDisplay = "skel"
