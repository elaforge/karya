-- | Switch between the Git implementation (cmdline git) and Git2
-- implementation (libgit2 FFI binding, faster but more dangerous).
--
-- There's nothing keeping the APIs consistent except good intentions.
module Util.Git (module Util.Git.Git) where
import Util.Git.Git
-- import Util.Git.Git2
