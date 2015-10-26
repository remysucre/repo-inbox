{-# OPTIONS_GHC -fexcess-precision #-}
--
-- The Computer Language Shootout
-- http://shootout.alioth.debian.org/
--
-- Contributed by Spencer Janssen, Trevor McCort, Christophe Poucet and Don Stewart
--
-- Must be compiled with the -fexcess-precision flag as a pragma. GHC
-- currently doesn't recognise the -fexcess-precision flag on the command
-- line (!).
--
-- The following flags are suggested when compiling:
--
--      -O -fglasgow-exts -optc-march=pentium4
--      -fbang-patterns -funbox-strict-fields -optc-O2 -optc-mfpmath=sse -optc-msse2 
--

import System.Environment
import System.IO
import Foreign
import Foreign.Marshal.Array

main = do
    w <- getArgs >>= return . read . head
    let n      = w `div` 8
        m  = 2 / fromIntegral w
    putStrLn ("P4\n"++show w++" "++show w)
    p <- mallocArray0 n
    unfold n (next_x w m n) p (T 1 0 0 (-1))

unfold :: Int -> (T -> Maybe (Word8,T)) -> Ptr Word8 -> T -> IO ()
unfold i f ptr x0 | i `seq` ptr `seq` x0 `seq` False = undefined
unfold i f ptr x0 = loop x0
  where
    loop x = x `seq` go ptr 0 x

    go p n x | p `seq` n `seq` x `seq` False = undefined
    go p n x = case f x of
        Just (w,y) | n /= i -> poke p w >> go (p `plusPtr` 1) (n+1) y
        Nothing             -> hPutBuf stdout ptr i
        _                   -> hPutBuf stdout ptr i >> loop x
{-# NOINLINE unfold #-}

data T = T !Int !Int !Int !Double

next_x w iw bw _ | w `seq` iw `seq` bw `seq` False = undefined
next_x w iw bw (T bx x y ci)
    | y  == w   = Nothing
    | bx == bw  = Just (loop_x w x 8 iw ci 0, T 1 0    (y+1)   (iw+ci))
    | otherwise = Just (loop_x w x 8 iw ci 0, T (bx+1) (x+8) y ci)

loop_x w x n iw ci b | w `seq` x `seq` n `seq` iw `seq` ci `seq` b `seq` False = undefined
loop_x w x n iw ci b
    | x < w = if n == 0
                    then b
                    else loop_x w (x+1) (n-1) iw ci (b+b+v)
    | otherwise = b `shiftL` n
  where
    v = fractal 0 0 (fromIntegral x * iw - 1.5) ci 50

fractal :: Double -> Double -> Double -> Double -> Int -> Word8
fractal r i cr ci k | r `seq` i `seq` cr `seq` ci `seq` k `seq` False = undefined
fractal r i cr ci k
    | r2 + i2 > 4 = 0
    | k == 0      = 1
    | otherwise   = fractal (r2-i2+cr) ((r+r)*i+ci) cr ci (k-1)
  where
    (r2,i2) = (r*r,i*i)
