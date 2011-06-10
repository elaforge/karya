{-| Re-export DeriveInternal and DeriveLib.  Derivers should probably be
    importing this module rather than the lower level ones.

    "Derive.DeriveInternal" contains an explanation of the split, and
    "Derive.DeriveLib" an overview of derivation in general.
-}
module Derive.Derive (
    module Derive.Deriver.Internal, module Derive.Deriver.Lib
) where
import Derive.Deriver.Internal
import Derive.Deriver.Lib
