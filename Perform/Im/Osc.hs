module Perform.Im.Osc where
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Sound.OSC as OSC
import qualified Sound.OSC.Transport.FD as OSC.Transport.FD
import qualified Sound.OSC.Transport.FD.UDP as OSC.Transport.FD.UDP

import qualified Util.Num as Num
import qualified Synth.Shared.Config as Config


-- let m1 = message "/dumpOSC" [Int32 1]
-- let m2 = message "/g_new" [Int32 1]
-- withTransport t (\fd -> let f = sendMessage fd in f m1 >> f m2)


start = send $ play "../data/sampler/reyong/62-109-127-open+v1.flac" 1 1
stop1 = send stop

send :: OSC.Message -> IO ()
send msg =
    OSC.Transport.FD.withTransport open $ \osc ->
        OSC.Transport.FD.sendMessage osc msg
    where
    open = OSC.Transport.FD.UDP.openUDP "127.0.0.1" Config.oscPort


play :: FilePath -> Double -> Double -> OSC.Message
play fname ratio volume = OSC.message "/play"
    [ OSC.ASCII_String (ByteString.Char8.pack fname)
    , OSC.Float (Num.d2f ratio), OSC.Float (Num.d2f volume)
    ]

stop :: OSC.Message
stop :: OSC.Message = OSC.message "/stop" []
