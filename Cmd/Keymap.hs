-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Support for efficient keymaps.

    The sequence of Cmds which return Continue or Done is flexible, but
    probably inefficient in the presence of hundreds of commands.  In addition,
    it can't warn about Cmds that respond to overlapping Msgs, e.g. respond to
    the same key.

    Keymaps provide an efficient way to respond to a useful subset of Msgs,
    i.e.  those which are considered 'key down' type msgs.  The exact
    definition is in 'Bindable'.

    Keys are bound using 'SimpleMod's, which are higher level than the ones in
    "Ui.Key".  This provides allows some abstraction between they key bindings
    and which actual modifiers those imply, and allows the conflation of
    multiple modifiers.

    If you bind to a shifted key, it will be converted to Shift + unshifted by
    'KeyLayouts.to_unshifted'.  But if you want to bind to the same physical
    key with and without shift, then you should bind to the unshifted version
    and add Shift yourself.
-}
module Cmd.Keymap where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.KeyLayouts as KeyLayouts
import qualified Cmd.Msg as Msg

import qualified Local.KeyLayout
import qualified Ui.Key as Key
import qualified Ui.Types as Types

import           Global


-- * binding

type Binding m = (Cmd.KeySpec, Cmd.NamedCmd m)

-- | Bind a Key with no modifiers.
plain_key :: Cmd.M m => Key.Key -> Text -> m () -> [Binding m]
plain_key = bind_key []

-- | Bind a Char with no modifiers.
plain_char :: Cmd.M m => Char -> Text -> m () -> [Binding m]
plain_char = plain_key . Key.Char

shift_char :: Cmd.M m => Char -> Text -> m () -> [Binding m]
shift_char = bind_key [Shift] . Key.Char

-- | Bind a Char with the 'PrimaryCommand'.
command_char :: Cmd.M m => Char -> Text -> m () -> [Binding m]
command_char = bind_key [PrimaryCommand] . Key.Char

-- | Bind a Char with the 'SecondaryCommand'.
secondary_char :: Cmd.M m => Char -> Text -> m () -> [Binding m]
secondary_char = bind_key [SecondaryCommand] . Key.Char

-- | Bind a key with the given modifiers.
bind_key :: Cmd.M m => [SimpleMod] -> Key.Key -> Text -> m () -> [Binding m]
bind_key smods key desc cmd = bind smods (Cmd.Key False key) desc (const cmd)

-- | Bind a key with a Cmd that returns Status.
bind_key_status :: [SimpleMod] -> Key.Key -> Text -> m Cmd.Status -> [Binding m]
bind_key_status smods key desc cmd =
    bind_status smods (Cmd.Key False key) desc (const cmd)

-- | Like 'bind_key', but the binding will be retriggered on key repeat.
bind_repeatable :: Cmd.M m => [SimpleMod] -> Key.Key -> Text -> m ()
    -> [Binding m]
bind_repeatable smods key desc cmd =
    bind smods (Cmd.Key True key) desc (const cmd)

-- | 'bind_click' passes the Msg to the cmd, since mouse cmds are more likely
-- to want the msg to find out where the click was.  @clicks@ is 1 for a single
-- click, 2 for a double click, etc.
bind_click :: Cmd.M m => [SimpleMod] -> Types.MouseButton -> Cmd.MouseOn -> Int
    -> Text -> (Msg.Msg -> m ()) -> [Binding m]
bind_click smods btn on clicks desc cmd =
    bind smods (Cmd.Click btn on (clicks-1)) desc cmd

-- | A 'bind_drag' binds both the click and the drag.  It's conceivable to have
-- click and drag bound to different commands, but I don't have any yet.
bind_drag :: Cmd.M m => [SimpleMod] -> Types.MouseButton -> Cmd.MouseOn
    -> Text -> (Msg.Msg -> m ()) -> [Binding m]
bind_drag smods btn on desc cmd =
    bind smods (Cmd.Click btn on 0) desc cmd
    ++ bind smods (Cmd.Drag btn on) desc cmd

bind_release :: Cmd.M m => [SimpleMod] -> Types.MouseButton -> Cmd.MouseOn
    -> Text -> (Msg.Msg -> m ()) -> [Binding m]
bind_release smods btn on = bind smods (Cmd.Release btn on)

-- | Like 'bind_status' but the Cmd is expected to return (), which will become
-- 'Cmd.Done'.  Since the cmd has already been matched on the bound key this is
-- likely what it would have done anyway.
bind :: Cmd.M m => [SimpleMod] -> Cmd.Bindable -> Text
    -> (Msg.Msg -> m ()) -> [Binding m]
bind smods bindable desc cmd =
    bind_status smods bindable desc ((>> return Cmd.Done) . cmd)

-- | This is the most general Binding constructor: bind any Bindable with any
-- modifiers, and don't assume the cmd returns Done.
--
-- A capital letter is shorthand for Shift + Char.toLower c.
bind_status :: [SimpleMod] -> Cmd.Bindable -> Text -> (Msg.Msg -> m Cmd.Status)
    -> [Binding m]
bind_status smods_ bindable_ name cmd =
    [ ( key_spec (expand_mods bindable smods) bind
      , Cmd.NamedCmd name cmd
      )
    | bind <- expand_bindable bindable
    ]
    where
    (smods, bindable) = case bindable_ of
        Cmd.Key repeat (Key.Char c) ->
            case KeyLayouts.to_unshifted Local.KeyLayout.layout c of
                Just unshifted ->
                    -- Don't worry about a duplicate Shift, 'key_spec' makes
                    -- this a Set.
                    (Shift : smods_, Cmd.Key repeat (Key.Char unshifted))
                Nothing -> (smods_, bindable_)
        _ -> (smods_, bindable_)

-- * Handler

-- | Create a Keymap for efficient lookup and return warnings encountered
-- during construction.
make_keymap :: [Binding m] -> (Cmd.Keymap m, [Text])
make_keymap bindings = (Map.fromList bindings, warns)
    where
    warns = map warn (overlaps bindings)
    warn cmds = "cmds overlap, picking the last one: ["
        <> Text.intercalate ", " cmds <> "]"

-- ** SimpleMod

-- | The Msg contains the low level key information, but most commands should
-- probably use these higher level modifiers.  That way left and right shifts
-- work the same, and cmds can use Command as customary on the Mac and Control
-- as customary on linux.
data SimpleMod =
    Shift
    -- | Primary command key: command on Mac, control on Linux and Windows.
    -- This should be used for core and global commands.
    | PrimaryCommand
    -- | Secondary comamnd key: control or option on Mac, alt on Linux and
    -- Windows.  I'm not sure what this should be used for, but perhaps it
    -- can be for more specific event text modifications, while PrimaryCommand
    -- is for general purpose modifications.  Also, it should have
    -- non-primitive cmds, so if you override them locally you won't lose
    -- anything essential.
    | SecondaryCommand
    -- | Having mouse here allows for mouse button chording.
    | Mouse Types.MouseButton
    deriving (Eq, Ord, Show)

-- | Map a SimpleMod to the Key.Modifiers it implies.
simple_mod_map :: Map SimpleMod Key.Modifier
simple_mod_map = Map.fromList $ case Config.platform of
    Config.Mac ->
        [ (Shift, Key.Shift)
        , (PrimaryCommand, Key.Meta)
        , (SecondaryCommand, Key.Control)
        ]
    Config.Linux ->
        [ (Shift, Key.Shift)
        , (PrimaryCommand, Key.Control)
        , (SecondaryCommand, Key.Alt)
        ]

simple_to_mod :: SimpleMod -> Maybe Cmd.Modifier
simple_to_mod (Mouse btn) = Just $ Cmd.MouseMod btn Nothing
simple_to_mod simple = Cmd.KeyMod <$> Map.lookup simple simple_mod_map

key_spec :: [Cmd.Modifier] -> Cmd.Bindable -> Cmd.KeySpec
key_spec mods bindable = Cmd.KeySpec (Set.fromList mods) bindable

-- ** Bindable

overlaps :: [Binding m] -> [[Text]]
overlaps bindings =
    [map cmd_name grp | grp <- Seq.group_sort fst bindings, length grp > 1]
    where
    cmd_name (kspec, Cmd.NamedCmd name _) =
        pretty kspec <> ": " <> name

-- | A binding that accepts a KeyRepeat should also accept a KeyDown.
expand_bindable :: Cmd.Bindable -> [Cmd.Bindable]
expand_bindable (Cmd.Key True key) = [Cmd.Key False key, Cmd.Key True key]
expand_bindable b = [b]

expand_mods :: Cmd.Bindable -> [SimpleMod] -> [Cmd.Modifier]
expand_mods bindable smods = mapMaybe simple_to_mod (prefix ++ smods)
    where
    -- You can't have a click or drag without having that button down!
    prefix = case bindable of
        Cmd.Click n _ _ -> [Mouse n]
        Cmd.Drag n _ -> [Mouse n]
        _ -> []
