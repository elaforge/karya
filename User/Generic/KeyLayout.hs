-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export the local 'KeyLayouts.Layout'.
--
-- This would be in "Local.Config" except that would cause a circular import.
-- "Cmd.Keymap" imports it, and Cmd.Keymap is a low level module, while
-- Local.Config is high-level because it imports the entire local config.
module User.Generic.KeyLayout (layout) where
import qualified Cmd.KeyLayouts as KeyLayouts


layout :: KeyLayouts.Layout
layout = KeyLayouts.qwerty
