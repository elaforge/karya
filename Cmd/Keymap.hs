-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ViewPatterns #-}
{- | Support for efficient keymaps.

    The sequece of Cmds which return Continue or Done is flexible, but probably
    inefficient in the presence of hundreds of commands.  In addition, it can't
    warn about Cmds that respond to overlapping Msgs, e.g. respond to the same
    key.

    Keymaps provide an efficient way to respond to a useful subset of Msgs,
    i.e.  those which are considered 'key down' type msgs.  The exact
    definition is in 'Bindable'.

    Keys are bound using 'SimpleMod's, which are higher level than the ones in
    "Ui.Key".  This provides allows some abstraction between they key bindings
    and which actual modifiers those imply, and allows the conflation of
    multiple modifiers.  In addition, Shift is stripped for 'Key.Char's, since
    the shift is already reflected in the character itself.  So while Shift
    may be included binding e.g. Key.Enter, it must not be included when
    binding letter keys.

    There are multiple ways to treat keys and shift.  I've tried out several
    variants, and for now I'm using the following, which is also what fltk
    uses:

    - Hitting a unshifted key should emit the letter it normally emits.  This
    might seem obvious, but when they keyboard is remapp

    - Hitting a shifted letter still emits a lowercase letter, but includes the
    Shift modifier.  So technically you can never bind a capital letter,
    because it never happens.  But as a convenience, if you bind a capital
    letter, the bind functions will lowercase it and add Shift to the
    modifiers.

    - Hitting a shifted digit is like letters: the digit and Shift are
    emitted, not @!@ or @#@ or whatever.  This is probably for the best because
    different keyboards put different things up there.

    - Unlike letters and digits, symbols emit the shifted symbol, along with
    shift.  So if @shift+[@ is mapped to @{@ then @shift+{@ will be emitted.
    If you bind something to Shift-[ then the cmd will never happen.
    This might be a bad idea or it might not, but it's what fltk does.

    - Special keys like return and backspace of course have no shifted variant,
    so there's nothing to worry about there.

    Previously I got capital letters !\@#$% symbols from fltk, and then did
    some processing to strip Shift when it was already implicit in the key.
    But I ran into trouble when I tried to bind shift-cmd-\/ because shift-\/
    was '?' but shift-cmd-\/ went back to being '\/' again.  It seemed simpler
    and more consistent to not handle Shift any differently from other
    modifiers.
-}
module Cmd.Keymap where
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Ui.Key as Key
import qualified Ui.Types as Types
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified App.Config as Config


-- * binding

-- | Bind a Key with no modifiers.
plain_key :: Cmd.M m => Key.Key -> String -> m a -> [Binding m]
plain_key = bind_key []

-- | Bind a Char with no modifiers.
plain_char :: Cmd.M m => Char -> String -> m a -> [Binding m]
plain_char = plain_key . Key.Char

shift_char :: Cmd.M m => Char -> String -> m a -> [Binding m]
shift_char = bind_key [Shift] . Key.Char

-- | Bind a Char with the PrimaryCommand.
command_char :: Cmd.M m => Char -> String -> m a -> [Binding m]
command_char = bind_key [PrimaryCommand] . Key.Char

-- | Bind a key with the given modifiers.
bind_key :: Cmd.M m => [SimpleMod] -> Key.Key -> String -> m a -> [Binding m]
bind_key smods key desc cmd = bind smods (Key False key) desc (const cmd)

-- | Bind a key with a Cmd that returns Status.
bind_key_status :: Cmd.M m => [SimpleMod] -> Key.Key -> String
    -> m Cmd.Status -> [Binding m]
bind_key_status smods key desc cmd =
    bind_status smods (Key False key) desc (const cmd)

-- | Like 'bind_key', but the binding will be retriggered on key repeat.
bind_repeatable :: Cmd.M m => [SimpleMod] -> Key.Key -> String -> m a
    -> [Binding m]
bind_repeatable smods key desc cmd =
    bind smods (Key True key) desc (const cmd)

-- | 'bind_click' passes the Msg to the cmd, since mouse cmds are more likely
-- to want the msg to find out where the click was.  @clicks@ is 1 for a single
-- click, 2 for a double click, etc.
bind_click :: Cmd.M m => [SimpleMod] -> Types.MouseButton -> MouseOn -> Int
    -> String -> (Msg.Msg -> m a) -> [Binding m]
bind_click smods btn on clicks desc cmd =
    bind smods (Click btn on (clicks-1)) desc cmd

-- | A 'bind_drag' binds both the click and the drag.  It's conceivable to have
-- click and drag bound to different commands, but I don't have any yet.
bind_drag :: Cmd.M m => [SimpleMod] -> Types.MouseButton -> MouseOn
    -> String -> (Msg.Msg -> m a) -> [Binding m]
bind_drag smods btn on desc cmd =
    bind smods (Click btn on 0) desc cmd ++ bind smods (Drag btn on) desc cmd

-- | Like 'bind_status' but the return value of the Cmd is ignored and assumed
-- to be 'Cmd.Done'.  Since the cmd has already been matched on the bound key
-- this is likely what it would have done anyway.
bind :: Cmd.M m => [SimpleMod] -> Bindable -> String
    -> (Msg.Msg -> m a) -> [Binding m]
bind smods bindable desc cmd =
    bind_status smods bindable desc ((>> return Cmd.Done) . cmd)

-- | This is the most general Binding constructor: bind any Bindable with any
-- modifiers, and don't assume the cmd returns Done.
--
-- A capital letter is shorthand for Shift + Char.toLower c.
bind_status :: Cmd.M m => [SimpleMod] -> Bindable -> String
    -> (Msg.Msg -> m Cmd.Status) -> [Binding m]
bind_status smods_ bindable_ desc cmd =
    [ (key_spec mods bind, cspec desc cmd)
    | bind <- expand_bindable bindable
    , mods <- expand_mods bindable smods
    ]
    where
    (smods, bindable) = case bindable_ of
        Key repeat (Key.Char c) | Char.isUpper c ->
            (Shift : smods_, Key repeat (Key.Char (Char.toLower c)))
        _ -> (smods_, bindable_)

-- ** util

-- | A binding that accepts a KeyRepeat should also accept a KeyDown.
expand_bindable :: Bindable -> [Bindable]
expand_bindable (Key True key) = [Key False key, Key True key]
expand_bindable b = [b]

expand_mods :: Bindable -> [SimpleMod] -> [[Cmd.Modifier]]
expand_mods bindable smods
    | null result = [[]]
    | otherwise = result
    where
    result = Seq.cartesian (map simple_to_mods (prefix ++ smods))
    -- You can't have a click or drag without having that button down!
    prefix = case bindable of
        Click n _ _ -> [Mouse n]
        Drag n _ -> [Mouse n]
        _ -> []

-- * CmdMap

-- | Create a CmdMap for efficient lookup and return warnings encountered
-- during construction.
make_cmd_map :: Monad m => [Binding m] -> (CmdMap m, [String])
make_cmd_map bindings = (Map.fromList bindings, warns)
    where
    warns = map warn (overlaps bindings)
    warn cmds = "cmds overlap, picking the last one: ["
        ++ Seq.join ", " cmds ++ "]"

-- | Create a cmd that dispatches into the given CmdMap.
--
-- To look up a cmd, the Msg is restricted to a 'Bindable'.  Then modifiers
-- that are allowed to overlap (such as keys) are stripped out of the mods and
-- the KeySpec is looked up in the keymap.
make_cmd :: Cmd.M m => CmdMap m -> Msg.Msg -> m Cmd.Status
make_cmd cmd_map msg = do
    bindable <- Cmd.abort_unless (msg_to_bindable msg)
    mods <- mods_down
    case Map.lookup (KeySpec mods bindable) cmd_map of
        Nothing -> return Cmd.Continue
        Just (CmdSpec name cmd) -> do
            Log.debug $ "running command " ++ show name
            Cmd.name name (cmd msg)


-- | The Msg contains the low level key information, but most commands should
-- probably use these higher level modifiers.  That way left and right shifts
-- work the same, and cmds can use Command as customary on the Mac and Control
-- as customary on linux.
--
-- Things you have to inspect the Msg directly for:
--
-- - differentiate ShiftL and ShiftR
--
-- - use option on the Mac
data SimpleMod =
    Shift
    -- | Primary command key: command on Mac, control on Linux and Windows.
    -- This should be used for core and global commands.
    | PrimaryCommand
    -- | Secondary comamnd key: control or option on Mac, alt on Linux and
    -- Windows.  I'm not sure what this should be used for, but it should be
    -- different than Mod1 stuff.  Maybe static config user-added commands.
    | SecondaryCommand
    -- | Having mouse here allows for mouse button chording.
    | Mouse Types.MouseButton
    deriving (Eq, Ord, Show)

-- | Sometimes you really want to bind to control, regardless of the platform.
really_control :: SimpleMod
really_control = case Config.platform of
    Config.Mac -> SecondaryCommand
    Config.Linux -> PrimaryCommand

-- * implementation

-- | TODO This is a hardcoded Mac layout, when I support other platforms
-- it'll have to be configurable.
simple_mod_map :: [(SimpleMod, [Key.Modifier])]
simple_mod_map = case Config.platform of
    Config.Mac ->
        [ (Shift, [Key.Shift])
        , (PrimaryCommand, [Key.Meta])
        -- Alt is the Mac's option key.
        , (SecondaryCommand, [Key.Control, Key.Alt])
        ]
    Config.Linux ->
        [ (Shift, [Key.Shift])
        , (PrimaryCommand, [Key.Control])
        , (SecondaryCommand, [Key.Alt])
        ]

simple_to_mods :: SimpleMod -> [Cmd.Modifier]
simple_to_mods (Mouse btn) = [Cmd.MouseMod btn Nothing]
simple_to_mods simple =
    maybe [] (map Cmd.KeyMod) (lookup simple simple_mod_map)

-- ** Binding

type Binding m = (KeySpec, CmdSpec m)

data KeySpec = KeySpec (Set.Set Cmd.Modifier) Bindable deriving (Eq, Ord, Show)

key_spec :: [Cmd.Modifier] -> Bindable -> KeySpec
key_spec mods bindable = KeySpec (Set.fromList mods) bindable

-- | Pair a Cmd with a descriptive string that can be used for logging, undo,
-- etc.
data CmdSpec m = CmdSpec String (Msg.Msg -> m Cmd.Status)

cspec :: String -> (Msg.Msg -> m Cmd.Status) -> CmdSpec m
cspec = CmdSpec

-- | Make a CmdSpec for a CmdM, i.e. a Cmd that doesn't take a Msg.
cspec_ :: String -> m Cmd.Status -> CmdSpec m
cspec_ desc cmd = CmdSpec desc (const cmd)

-- ** CmdMap

type CmdMap m = Map.Map KeySpec (CmdSpec m)

overlaps :: [Binding m] -> [[String]]
overlaps bindings =
    [map cmd_name grp | grp <- Seq.group_on fst bindings, length grp > 1]
    where
    cmd_name (kspec, CmdSpec name _) = pretty kspec ++ ": " ++ name

-- | Return the mods currently down, stripping out non-modifier keys and notes,
-- so that overlapping keys will still match.  Mouse mods are not filtered, so
-- each mouse chord can be bound individually.
mods_down :: Cmd.M m => m (Set.Set Cmd.Modifier)
mods_down = Set.fromList <$> fmap (filter is_mod . Map.keys) Cmd.keys_down
    where
    is_mod (Cmd.KeyMod {}) = True
    is_mod (Cmd.MidiMod {}) = False
    is_mod (Cmd.MouseMod {}) = True

msg_to_bindable :: Msg.Msg -> Maybe Bindable
msg_to_bindable msg = case msg of
    (get_key -> Just (is_repeat, key)) -> Just $ Key is_repeat key
    (Msg.mouse -> Just mouse) -> case UiMsg.mouse_state mouse of
        UiMsg.MouseDown btn -> Just $ Click btn on (UiMsg.mouse_clicks mouse)
        UiMsg.MouseDrag btn -> Just $ Drag btn on
        _ -> Nothing
    (Msg.midi -> Just (Midi.ChannelMessage chan (Midi.NoteOn key _))) ->
        Just $ Note chan key
    _ -> Nothing
    where
    on = maybe Elsewhere mouse_on (Msg.context msg)
    get_key msg = case Msg.key msg of
        Just (UiMsg.KeyDown, k) -> Just (False, k)
        Just (UiMsg.KeyRepeat, k) -> Just (True, k)
        _ -> Nothing

data Bindable =
    -- | Key IsRepeat Key
    Key Bool Key.Key
    -- | Click MouseButton Clicks
    | Click Types.MouseButton MouseOn Int
    | Drag Types.MouseButton MouseOn
    -- | Channel can be used to restrict bindings to a certain keyboard.  This
    -- should probably be something more abstract though, such as a device
    -- which can be set by the static config.
    | Note Midi.Channel Midi.Key
    deriving (Eq, Ord, Show, Read)

-- | Where a click or drag occurred.
data MouseOn = OnTrack | OnDivider | OnSkeleton | Elsewhere
    deriving (Eq, Ord, Show, Read)

mouse_on :: UiMsg.Context -> MouseOn
mouse_on = maybe Elsewhere on . UiMsg.ctx_track
    where
    on (_, UiMsg.Track {}) = OnTrack
    on (_, UiMsg.Divider) = OnDivider
    on (_, UiMsg.SkeletonDisplay) = OnSkeleton

instance Pretty.Pretty MouseOn where
    pretty OnTrack = "track"
    pretty OnDivider = "divider"
    pretty OnSkeleton = "skeleton"
    pretty Elsewhere = "elsewhere"


-- * pretty printing

instance Pretty.Pretty KeySpec where
    pretty (KeySpec mods bindable) =
        Seq.join2 " " (show_mods mods) (show_bindable True bindable)
        where show_mods = Seq.join " + " . map show_mod . Set.toList

show_mod :: Cmd.Modifier -> String
show_mod m = case m of
    -- TODO this is only true on OS X
    Cmd.KeyMod Key.Meta -> "cmd"
    Cmd.KeyMod Key.Control -> "ctrl"
    Cmd.KeyMod mod -> map Char.toLower (show mod)
    Cmd.MouseMod button _ -> "mouse " ++ show button
    Cmd.MidiMod chan key -> "midi " ++ show key ++ " chan " ++ show chan

instance Pretty.Pretty Bindable where
    pretty = show_bindable True

show_bindable :: Bool -> Bindable -> String
show_bindable show_repeatable b = case b of
    Key is_repeat key -> pretty key
        ++ if show_repeatable && is_repeat then " (repeatable)" else ""
    Click button on times -> click_times times ++ "click "
        ++ show button ++ " on " ++ pretty on
    Drag button on -> "drag " ++ show button ++ " on " ++ pretty on
    Note chan key -> "midi " ++ show key ++ " channel " ++ show chan
    where
    click_times 0 = ""
    click_times 1 = "double-"
    click_times 2 = "triple-"
    click_times n = show n ++ "-"


-- * key layout

-- | Map a physical key, written relative to USA qwerty layout, to whatever
-- character that key actually emits (if the layout is already USA qwerty then
-- it's id of course).  This is for layouts which should be done based on
-- physical key position, like piano-style keyboards.  It makes the
-- overlapping-ness of non-mapped keys hard to predict though.
--
-- Since it's intended to map literal key characters, there is no accomodation
-- for failure.  Really this should be done at compile time, so it's
-- conceptually a compile time error.
--
-- TODO isn't there some way I can get this at compile time?  template haskell?
physical_key :: Char -> Char
physical_key c =
    fromMaybe (error $ "Keymap.physical_key " ++ show c ++ " not found") $
        Map.lookup c hardcoded_kbd_layout


qwerty, qwerty_lower, qwerty_upper :: [Char]
qwerty = qwerty_lower ++ qwerty_upper
qwerty_lower
    = "1234567890-="
    ++ "qwertyuiop[]\\"
    ++ "asdfghjkl;'"
    ++ "zxcvbnm,./"
qwerty_upper
    = "!@#$%^&*()_+"
    ++ "QWERTYUIOP{}|"
    ++ "ASDFGHJKL:\""
    ++ "ZXCVBNM<>?"

-- | Not just dvorak, but my slightly modified version.
dvorak :: [Char]
dvorak = "1234567890-="
    ++ "',.pyfgcrl[]\\"
    ++ "aoeuidhtns/"
    ++ ";qjkxbmwvz"

    ++ "!@#$%^&*()_+"
    ++ "\"<>PYFGCRL{}|"
    ++ "AOEUIDHTNS?"
    ++ ":QJKXBMWVZ"

qwerty_to_dvorak :: Map.Map Char Char
qwerty_to_dvorak = Map.fromList (zip qwerty dvorak)

-- TODO presumably this should eventually be easier to change
hardcoded_kbd_layout :: Map.Map Char Char
hardcoded_kbd_layout = qwerty_to_dvorak
