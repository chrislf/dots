module Main where

import Graphics.Gloss
import Data.Function (on)

import Lib

windowSize :: Int
windowSize = 300

glossMain :: IO ()
glossMain =
  simulate
    (InWindow
      "Window Name"
      (windowSize, windowSize)
      (10, 10)
    )
    white
    1
    [initFire]
    (showArray . heatToArray)
    (\_ _ a -> fire a)


data LED = LED {
    colour :: Color
  , lit :: Bool
}

showLED :: LED -> Picture
showLED (LED colour lit) = Color colour l
  where 
    l = if lit
          then ThickCircle 5 10
          else Circle 10

type Array = [[LED]]

type Heat = [[Int]]

initFire = replicate 10 100

fire :: Heat -> Heat
fire orig = initFire : take 10 top
  where
    makeNewRow = map (subtract 10)
    top = map makeNewRow orig


heatToArray :: Heat -> Array
heatToArray = map (map toLED)
  where
    toLED n = LED (makeColor 1 ((100-fromIntegral n)/100) 0 1) True


showArray :: Array -> Picture
showArray xss = result
  where 
    width = length . head $ xss
    height = length xss
    halfWidth = width `fdiv` 2
    halfHeight = height `fdiv` 2
    wRange = [-halfWidth..halfWidth]
    hRange = [-halfHeight..halfHeight]

    makeRow r = mconcat $ zipWith (\x l -> translate (25*x) 0 (showLED l)) wRange r
    rows = map makeRow xss
    result = mconcat $ zipWith (\y p -> translate 0 (25*y) p) hRange rows

-- why can't we write Integral a, b?
fdiv :: Fractional c => Int -> Int -> c
fdiv = (/) `on` fromIntegral


main :: IO ()
main = glossMain
