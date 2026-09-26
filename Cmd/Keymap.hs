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

import qualified Util.Lists as Lists
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
plainKey :: Cmd.M m => Key.Key -> Text -> m () -> [Binding m]
plainKey = bindKey []

-- | Bind a Char with no modifiers.
plainChar :: Cmd.M m => Char -> Text -> m () -> [Binding m]
plainChar = plainKey . Key.Char

shiftChar :: Cmd.M m => Char -> Text -> m () -> [Binding m]
shiftChar = bindKey [Shift] . Key.Char

-- | Bind a Char with the 'PrimaryCommand'.
commandChar :: Cmd.M m => Char -> Text -> m () -> [Binding m]
commandChar = bindKey [PrimaryCommand] . Key.Char

-- | Bind a Char with the 'SecondaryCommand'.
secondaryChar :: Cmd.M m => Char -> Text -> m () -> [Binding m]
secondaryChar = bindKey [SecondaryCommand] . Key.Char

-- | Bind a key with the given modifiers.
bindKey :: Cmd.M m => [SimpleMod] -> Key.Key -> Text -> m () -> [Binding m]
bindKey smods key desc cmd = bind smods (Cmd.Key False key) desc (const cmd)

-- | Bind a key with a Cmd that returns Status.
bindKeyStatus :: [SimpleMod] -> Key.Key -> Text -> m Cmd.Status -> [Binding m]
bindKeyStatus smods key desc cmd =
    bindStatus smods (Cmd.Key False key) desc (const cmd)

-- | Like 'bind_key', but the binding will be retriggered on key repeat.
bindRepeatable :: Cmd.M m => [SimpleMod] -> Key.Key -> Text -> m ()
    -> [Binding m]
bindRepeatable smods key desc cmd =
    bind smods (Cmd.Key True key) desc (const cmd)

-- | 'bind_click' passes the Msg to the cmd, since mouse cmds are more likely
-- to want the msg to find out where the click was.  @clicks@ is 1 for a single
-- click, 2 for a double click, etc.
bindClick :: Cmd.M m => [SimpleMod] -> Types.MouseButton -> Cmd.MouseOn -> Int
    -> Text -> (Msg.Msg -> m ()) -> [Binding m]
bindClick smods btn on clicks desc cmd =
    bind smods (Cmd.Click btn on (clicks-1)) desc cmd

-- | A 'bind_drag' binds both the click and the drag.  It's conceivable to have
-- click and drag bound to different commands, but I don't have any yet.
bindDrag :: Cmd.M m => [SimpleMod] -> Types.MouseButton -> Cmd.MouseOn
    -> Text -> (Msg.Msg -> m ()) -> [Binding m]
bindDrag smods btn on desc cmd =
    bind smods (Cmd.Click btn on 0) desc cmd
    ++ bind smods (Cmd.Drag btn on) desc cmd

bindRelease :: Cmd.M m => [SimpleMod] -> Types.MouseButton -> Cmd.MouseOn
    -> Text -> (Msg.Msg -> m ()) -> [Binding m]
bindRelease smods btn on = bind smods (Cmd.Release btn on)

-- | Like 'bind_status' but the Cmd is expected to return (), which will become
-- 'Cmd.Done'.  Since the cmd has already been matched on the bound key this is
-- likely what it would have done anyway.
bind :: Cmd.M m => [SimpleMod] -> Cmd.Bindable -> Text
    -> (Msg.Msg -> m ()) -> [Binding m]
bind smods bindable desc cmd =
    bindStatus smods bindable desc ((>> pure Cmd.Done) . cmd)

-- | This is the most general Binding constructor: bind any Bindable with any
-- modifiers, and don't assume the cmd returns Done.
--
-- A capital letter is shorthand for Shift + Char.toLower c.
bindStatus :: [SimpleMod] -> Cmd.Bindable -> Text -> (Msg.Msg -> m Cmd.Status)
    -> [Binding m]
bindStatus smods_ bindable_ name cmd =
    [ ( keySpec (expandMods bindable smods) bind
      , Cmd.NamedCmd name cmd
      )
    | bind <- expandBindable bindable
    ]
    where
    (smods, bindable) = case bindable_ of
        Cmd.Key repeat (Key.Char c) ->
            case KeyLayouts.toUnshifted Local.KeyLayout.layout c of
                Just unshifted ->
                    -- Don't worry about a duplicate Shift, 'key_spec' makes
                    -- this a Set.
                    (Shift : smods_, Cmd.Key repeat (Key.Char unshifted))
                Nothing -> (smods_, bindable_)
        _ -> (smods_, bindable_)

-- * Handler

-- | Create a Keymap for efficient lookup and return warnings encountered
-- during construction.
makeKeymap :: [Binding m] -> (Cmd.Keymap m, [Text])
makeKeymap bindings = (Map.fromList bindings, warns)
    where
    warns = map warn (overlaps bindings)
    warn cmds = "cmds overlap, picking the last one: ["
        <> Text.intercalate ", " cmds <> "]"

-- ** SimpleMod

-- | The Msg contains the low level key information, but most commands should
-- probably use these higher level modifiers.  That way left and right shifts
-- work the same, and cmds can use Command as customary on the Mac and Control
-- as customary on linux.
data SimpleMod
    = Shift
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
simpleModMap :: Map SimpleMod Key.Modifier
simpleModMap = Map.fromList $ case Config.platform of
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

simpleToMod :: SimpleMod -> Maybe Cmd.Modifier
simpleToMod (Mouse btn) = Just $ Cmd.MouseMod btn Nothing
simpleToMod simple = Cmd.KeyMod <$> Map.lookup simple simpleModMap

keySpec :: [Cmd.Modifier] -> Cmd.Bindable -> Cmd.KeySpec
keySpec mods bindable = Cmd.KeySpec (Set.fromList mods) bindable

-- ** Bindable

overlaps :: [Binding m] -> [[Text]]
overlaps bindings =
    [map cmdName grp | grp <- Lists.groupSort fst bindings, length grp > 1]
    where
    cmdName (kspec, Cmd.NamedCmd name _) =
        pretty kspec <> ": " <> name

-- | A binding that accepts a KeyRepeat should also accept a KeyDown.
expandBindable :: Cmd.Bindable -> [Cmd.Bindable]
expandBindable (Cmd.Key True key) = [Cmd.Key False key, Cmd.Key True key]
expandBindable b = [b]

expandMods :: Cmd.Bindable -> [SimpleMod] -> [Cmd.Modifier]
expandMods bindable smods = mapMaybe simpleToMod (prefix ++ smods)
    where
    -- You can't have a click or drag without having that button down!
    prefix = case bindable of
        Cmd.Click n _ _ -> [Mouse n]
        Cmd.Drag n _ -> [Mouse n]
        _ -> []
