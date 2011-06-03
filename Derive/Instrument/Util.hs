-- | Utility functions for writing instrument calls.
module Derive.Instrument.Util where
import qualified Util.Pretty as Pretty

import qualified Derive.Call as Call
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang


-- | Make a call that simply calls the default note call with the given attrs.
with_attrs :: Score.Attributes -> Derive.NoteCall
with_attrs attrs =
    Derive.stream_generator ("with_attrs " ++ Pretty.pretty attrs) $
    \args -> CallSig.call0 args $ Util.with_attrs (Score.attrs_union attrs) $
        Call.reapply args [TrackLang.call ""]
