-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Switch between the Git implementation (cmdline git) and Git2
-- implementation (libgit2 FFI binding, faster but more dangerous).
--
-- There's nothing keeping the APIs consistent except good intentions.
module Util.Git (module Util.Git.Git_hlibgit2) where
-- import Util.Git.Git
-- import Util.Git.Git2
import Util.Git.Git_hlibgit2
