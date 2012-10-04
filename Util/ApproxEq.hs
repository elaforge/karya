-- | ApproxEq class for comparing floating point numbers.
module Util.ApproxEq (ApproxEq(approx_eq)) where


class ApproxEq a where
    approx_eq :: Double -> a -> a -> Bool

instance ApproxEq Float where
    approx_eq eta x y = abs (x - y) <= realToFrac eta
instance ApproxEq Double where
    approx_eq eta x y = abs (x - y) <= eta

instance ApproxEq Char where approx_eq _ = (==)
instance ApproxEq Int where approx_eq _ = (==)
instance ApproxEq Integer where approx_eq _ = (==)
instance ApproxEq Bool where approx_eq _ = (==)

instance (ApproxEq a) => ApproxEq [a] where
    approx_eq eta xs ys =
        length xs == length ys && and (zipWith (approx_eq eta) xs ys)

instance (ApproxEq a) => ApproxEq (Maybe a) where
    approx_eq eta (Just x) (Just y) = approx_eq eta x y
    approx_eq _ _ _ = False

instance (ApproxEq a, ApproxEq b) => ApproxEq (Either a b) where
    approx_eq eta (Right x) (Right y) = approx_eq eta x y
    approx_eq eta (Left x) (Left y) = approx_eq eta x y
    approx_eq _ _ _ = False

instance (ApproxEq a, ApproxEq b) => ApproxEq (a, b) where
    approx_eq eta (a, b) (a', b') =
        approx_eq eta a a' && approx_eq eta b b'
instance (ApproxEq a, ApproxEq b, ApproxEq c) => ApproxEq (a, b, c) where
    approx_eq eta (a, b, c) (a', b', c') =
        approx_eq eta a a' && approx_eq eta b b' && approx_eq eta c c'
instance (ApproxEq a, ApproxEq b, ApproxEq c, ApproxEq d) =>
        ApproxEq (a, b, c, d) where
    approx_eq eta (a, b, c, d) (a', b', c', d') =
        approx_eq eta a a' && approx_eq eta b b' && approx_eq eta c c'
            && approx_eq eta d d'
