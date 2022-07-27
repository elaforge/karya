module Derive.Deriver.DeriveM_criterion where
import qualified Criterion
import qualified Criterion.Main
import qualified System.Environment as Environment
import qualified Text.Read as Read

import qualified Derive.Deriver.DeriveMBench as DeriveMBench


-- Try to see if there is overhead in DeriveM.
-- Either I'm doing it wrong, or there is none.
main :: IO ()
main = Environment.getArgs >>= \case
    [n] | Just n <- Read.readMaybe n ->
        print (DeriveMBench.runRefCountdown (10^n))
    _ -> Criterion.Main.defaultMain
        [ Criterion.bgroup "countdown" $ do
            n <- ns
            return $ Criterion.bench (show n)
                (Criterion.nf DeriveMBench.runCountdown n)
        , Criterion.bgroup "reference" $ do
            n <- ns
            return $ Criterion.bench (show n)
                (Criterion.nf DeriveMBench.runRefCountdown n)
        ]
    where
    ns = [10^2, 10^3, 10^4, 10^5, 10^6]
