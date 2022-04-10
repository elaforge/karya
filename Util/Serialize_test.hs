-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Serialize_test where
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding

import qualified Util.Serialize as Serialize
import Util.Test


test_serialize :: Test
test_serialize = do
    let s = "臭味相投" :: Text.Text
        b = Text.Encoding.encodeUtf8 s
    equal s (recode s)
    equal b (recode b)

recode :: Serialize.Serialize a => a -> a
recode = either error id . Serialize.decode . Serialize.encode
