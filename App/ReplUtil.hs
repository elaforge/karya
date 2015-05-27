-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions shared betwen "Cmd.Repl" or "Cmd.ReplGhc" and "App.Repl".
module App.ReplUtil (
    Response, Result(..), raw
    , encode_request, decode_request
    , encode_response, decode_response, format_response
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.ByteString as ByteString
import Data.Monoid ((<>))
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

decode_response :: ByteString.ByteString -> (Text, [Text])
decode_response bytes = (Text.strip result, logs)
    where
    (result, logs) = case decode_record bytes of
        [] -> ("", [])
        result : logs -> (format1 result, logs)
    format1 result
        | result == "()" = ""
        | Just ('!', s) <- Text.uncons result = s
        | otherwise = Text.strip $ Text.pack $ PPrint.format_str $
            Text.unpack result

format_response :: (Text, [Text]) -> Text
format_response (response, logs_) = Text.strip $ Text.unlines $
    (if null logs then [] else "Logs:" : logs ++ [""]) ++ [response]
    where logs = abbreviate_logs (map Text.strip logs_)

abbreviate_logs :: [Text] -> [Text]
abbreviate_logs logs = loaded ++ filter (not . package_log) logs
    where
    loaded =
        ["Loaded " <> Text.pack (show packages) <> " packages" | packages > 0]
    packages = length $ filter ("Loading package" `Text.isPrefixOf`) logs
    package_log log =
        any (`Text.isPrefixOf` log) ["Loading package", "linking ...", "done."]

-- * implementation

encode_record :: [Text] -> ByteString.ByteString
encode_record fields = Encoding.encodeUtf8 $
    Text.intercalate "\0\0" (map (Text.replace "\n" "\0") fields)
    <> "\n"

decode_record :: ByteString.ByteString -> [Text]
decode_record = map (Text.replace "\0" "\n") . Text.splitOn "\0\0"
    . Text.dropWhileEnd (=='\n') . Encoding.decodeUtf8
