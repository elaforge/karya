-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Receive events from the C++ UI layer, package them up nicely, and forward
    them on to the event handling mechanism (the responder).
-}
module Ui.UiMsg where
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.Rect as Rect
import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.Key as Key
import qualified Ui.Types as Types

import           Global
import           Types


-- | Technically not all UiMsgs have a Context, but it's easier to give
-- everyone a Context since it can be all Nothing anyway.
data UiMsg = UiMsg Context Msg
    deriving (Show)

data Context = Context
    { ctx_focus :: Maybe ViewId
    -- | Index into block tracks.  The TrackNum will be one greater than the
    -- last track if the cursor has moved beyond it.
    , ctx_track :: Maybe (TrackNum, Track)
    -- | Indicates that the msg originated from the floating input.
    -- It should be an 'UpdateInput'.
    , ctx_floating_input :: !Bool
    } deriving (Show)

-- | Whether the context is on the track itself or the skeleton display above
-- the track.
--
-- There are a few cases where there is a track but no position and it
-- *doesn't* mean SkeletonDisplay, namely UpdateTrackWidth and UpdateInput.
-- However, those cases are handled in a specific place while Track goes on
-- to become a mouse Modifier, so I don't mind if the former is a little
-- awkward for the benefit of the latter.
data Track = Track !TrackTime | SkeletonDisplay | Divider
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
    -- | Nothing means the text didn't change, but a msg is being sent anyway
    -- since that's the only way I know the floating edit input has closed.
    UpdateInput !(Maybe Text.Text)
    | UpdateTrackScroll !Types.Width
    | UpdateTimeScroll !ScoreTime
    -- | Size of entire block window, and padding.
    | UpdateViewResize !Rect.Rect !Block.Padding
    -- | TrackWidth.width, TrackWidth.suggested_width
    | UpdateTrackWidth !Types.Width !Types.Width
    -- | The given view was closed.
    | UpdateClose
    deriving (Eq, Ord, Show)

-- | MsgType.msg_event, which is a fltk event.
data MsgEvent =
    Mouse !MouseEvent
    -- | The Char is the text that this key wants to enter, if any.  The Key is
    -- just the keycap, without taking shift or other modifiers into account.
    | Kbd KbdState [Key.Modifier] Key.Key (Maybe Char)
    | AuxMsg AuxMsg
    | Unhandled Int
    deriving (Eq, Ord, Show)
    -- The presence of [Key.Modifier] in Kbd and Mouse is ugly because it's
    -- only for cmd_record_keys.  All the rest should use Cmd.state_keys_down.
    -- TODO maybe this should move to MsgEvent so FOCUS can update the
    -- modifiers?  Doesn't matter as long as fltk doesn't support it.

data MouseEvent = MouseEvent
    { mouse_state :: !MouseState
    , mouse_modifiers :: ![Key.Modifier]
    , mouse_coords :: !(Int, Int)
    -- | As per fltk, 0 is the first click, 1 is a double click, etc.
    , mouse_clicks :: !Int
    , mouse_is_click :: !Bool
    } deriving (Eq, Ord, Show)

-- | Most of these are unused, but are included here for completeness.
data AuxMsg = Enter | Leave | Focus | Unfocus | Shortcut | Deactivate
    | Activate | Hide | Show
    deriving (Eq, Ord, Show)

data MouseState = MouseMove | MouseDrag Types.MouseButton
    | MouseDown Types.MouseButton | MouseUp Types.MouseButton
    deriving (Eq, Ord, Show)
data KbdState = KeyDown | KeyRepeat | KeyUp deriving (Eq, Ord, Show)

instance Pretty MouseState where pretty = showt
instance Pretty KbdState where pretty = showt

-- | Like 'pretty', but more compact.  Only show the most important bits.
show_short :: UiMsg -> Text
show_short = \case
    UiMsg _ctx (MsgEvent mdata) -> case mdata of
        Mouse (MouseEvent { mouse_state = state }) -> showt state
        Kbd kstate mods key _maybe_char -> mconcat $ concat
            [ map ((<>"+") . showt) mods
            , [pretty key]
            , case kstate of
                KeyDown -> []
                KeyUp -> ["(u)"]
                KeyRepeat -> ["(r)"]
            ]
        AuxMsg msg -> showt msg
        Unhandled x -> "Unhandled: " <> showt x
    UiMsg _ctx (UiUpdate vid update) -> Id.ident_text vid <> ":" <> word update
    UiMsg _ctx (UpdateScreenSize {}) -> "UpdateScreenSize"
    where
    word :: Show a => a -> Text
    word = fromMaybe "" . Lists.head . Text.words . showt

instance Pretty UiMsg where
    pretty ui_msg = case ui_msg of
        UiMsg ctx (MsgEvent mdata) -> case mdata of
            Mouse mouse -> Text.unwords [pretty mouse, pretty ctx]
            Kbd kstate mods key maybe_char -> Text.unwords
                [ "Kbd:", showt kstate, showt mods, showt key
                , maybe "" (\c -> "(" <> Text.singleton c <> ") ") maybe_char
                    <> pretty ctx
                ]
            AuxMsg msg -> Text.unwords ["Aux:", showt msg, pretty ctx]
            Unhandled x -> "Unhandled: " <> showt x
        UiMsg ctx msg -> Text.unwords [showt msg, pretty ctx]

instance Pretty MouseEvent where
    pretty (MouseEvent mstate mods coords clicks is_click) = Text.unwords
        [ "Mouse:", showt mstate, showt mods, showt coords
        , "click:", showt is_click, showt clicks
        ]

instance Pretty Context where
    pretty (Context focus track floating_input) = "{" <> contents <> "}"
        where
        contents = Text.unwords $ filter (not . Text.null)
            [ show_maybe "focus" focus
            , maybe "" show_track track
            , if floating_input then "floating_input" else ""
            ]
        show_track (tnum, track) =
            "track=" <> showt tnum <> ":" <> pretty track
        show_maybe desc = maybe "" (\v -> desc <> "=" <> showt v)

instance Pretty Track where
    pretty (Track pos) = "track:" <> pretty pos
    pretty Divider = "div"
    pretty SkeletonDisplay = "skel"
