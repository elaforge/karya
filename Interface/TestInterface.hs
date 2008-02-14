{-# LANGUAGE ForeignFunctionInterface #-}
module Interface.TestInterface where

import Foreign
-- import Foreign.C

-- import Interface.Types
import qualified Interface.Color as Color
import qualified Interface.Block as Block
import qualified Interface.Ruler as Ruler
import qualified Interface.Track as Track

main = test_ruler

test_block = do
    block <- Block.create block_config
    Block.set_title block "oh no"
    t <- Block.get_title block
    putStrLn t

test_ruler = do
    ruler1 <- Ruler.create Color.white [marklist]
    track1 <- Track.create

    block1 <- Block.create block_config
    block2 <- Block.create block_config

    Block.insert_track block1 0 (Block.R ruler1) 20
    Block.insert_track block1 1 (Block.T (track1, ruler1)) 60
    Block.insert_track block1 2 (Block.D Color.blue) 5
    print "done"
    where
    major = Ruler.Mark 1 3 (Color.Color 0.45 0.27 0 0) "" 0 0
    minor = Ruler.Mark 2 2 (Color.Color 1 0.39 0.2 0) "" 0 0
    marklist = Ruler.Marklist $ take 10 $ zip [10, 20 ..]
        (cycle [major, minor, minor, minor])

test_color = do
    alloca $ \colorp -> do
        poke colorp Color.red
        c_print_color colorp

block_config = Block.BlockModelConfig [Color.black] Color.white
    Color.blue Color.blue

foreign import ccall "print_color" c_print_color :: Ptr Color.Color -> IO ()
