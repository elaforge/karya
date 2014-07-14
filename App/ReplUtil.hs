-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions shared betwen "Cmd.Repl" or "Cmd.ReplGhc" and "App.Repl".
module App.ReplUtil (
    Response, Result(..), raw
    , encode_request, decode_request
    , encode_response, decode_response
) where
import Data.Monoid ((<>))
import qualified Control.DeepSeq as DeepSeq
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding

import qualified Util.PPrint as PPrint


type Response = (Result, [Log])
type Log = Text
data Result = Raw !Text | Format !Text deriving (Show)

instance DeepSeq.NFData Result where
    rnf (Raw _) = ()
    rnf (Format _) = ()

raw :: Text -> Response
raw txt = (Raw txt, [])

-- * request

encode_request :: Text -> ByteString.ByteString
encode_request = Encoding.encodeUtf8 . (<>"\n")

decode_request :: ByteString.ByteString -> Text
decode_request = Text.dropWhileEnd (=='\n') . Encoding.decodeUtf8

-- * response

encode_response :: Response -> ByteString.ByteString
encode_response (result, logs) = encode_record (result_s : logs)
    where
    result_s = case result of
        Raw t -> Text.cons '!' t
        Format t -> t

decode_response :: ByteString.ByteString -> Text
decode_response bytes =
    Text.strip $ Text.unlines $
        (if null logs then [] else "Logs:" : logs ++ [""]) ++ [result]
    where
    (result, logs) = case decode_record bytes of
        [] -> ("", [])
        result : logs -> (format1 result, logs)
    format1 result
        | result == "()" = ""
        | Just ('!', s) <- Text.uncons result = s
        | otherwise = Text.strip $ Text.pack $ PPrint.format_str $
            Text.unpack result

-- * implementation

encode_record :: [Text] -> ByteString.ByteString
encode_record fields = Encoding.encodeUtf8 $
    Text.intercalate "\0\0" (map (Text.replace "\n" "\0") fields)
    <> "\n"

decode_record :: ByteString.ByteString -> [Text]
decode_record = map (Text.replace "\0" "\n") . Text.splitOn "\0\0"
    . Text.dropWhileEnd (=='\n') . Encoding.decodeUtf8
