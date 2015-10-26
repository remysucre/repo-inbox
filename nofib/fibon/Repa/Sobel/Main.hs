{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

-- | Apply Sobel operators to an image.
import Data.Word
import Control.Monad
import System.Environment
import Data.Array.Repa 			as Repa
import Data.Array.Repa.IO.BMP
import Data.Array.Repa.IO.Timing
import Prelude				hiding (compare)

import Solver

-- Main routine ---------------------------------------------------------------
main 
 = do	args	<- getArgs
	case args of
	 [iterations, fileIn, fileOut]	
		-> run (read iterations) fileIn fileOut
	 _	-> putStrLn "Usage: sobel <iterations::Int> <fileIn.bmp> <fileOut.bmp>"


run iterations fileIn fileOut
 = do	inputImage 	<- liftM (force . either (error . show) id) 
			$ readImageFromBMP fileIn
	
	let greyImage	= toGreyScale inputImage
	greyImage `deepSeqArray` return ()
		
	(result, _tElapsed)
		<- time $ let 	(gX, gY)	= loop iterations greyImage
			  in	gX `deepSeqArray` gY `deepSeqArray` return (gX, gY)

	-- putStr $ prettyTime tElapsed
        putStrLn "Done"
	
	let (gX, gY)	= result
	let outImage	= force2 $ Repa.zipWith magnitude gX gY	

	outImage `seq` return ()

	-- TODO: The image normalization in this write fn eats up most of the runtime.
	writeMatrixToGreyscaleBMP fileOut outImage


loop :: Int -> Image -> (Image, Image)
loop n 
 = withManifest $ \img ->
   if n == 0
    then (img, img)
    else do 
	let gX	= gradientX img
	let gY	= gradientY img	
	if (n == 1) 
		then gX `deepSeqArray` gY `deepSeqArray` (gX, gY)
		else gX `deepSeqArray` gY `deepSeqArray` loop (n - 1) img


-- | Determine the squared magnitude of a vector.
magnitude :: Float -> Float -> Double
{-# INLINE magnitude #-}
magnitude x y
	= fromRational $ toRational $ sqrt (x * x + y * y)


-- | RGB to greyscale conversion.
toGreyScale :: Array DIM3 Word8 -> Image
{-# NOINLINE toGreyScale #-}
toGreyScale 
  = withManifest $ \arr ->
    arr `seq` force2 $ traverse arr
	(\(sh :. _) -> sh)
	(\get ix    -> rgbToLuminance 
				(get (ix :. 0))
				(get (ix :. 1))
				(get (ix :. 2)))


-- | Convert a RGB value to a luminance.
rgbToLuminance :: Word8 -> Word8 -> Word8 -> Float
{-# INLINE rgbToLuminance #-}
rgbToLuminance r g b 
	= fromIntegral r * 0.3
	+ fromIntegral g * 0.59
	+ fromIntegral b * 0.11
