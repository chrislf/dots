module Main where

import Graphics.Gloss
import Data.Function (on)

import Lib

windowSize :: Int
windowSize = 300

glossMain :: IO ()
--glossMain = display (InWindow "Window Name" (windowSize, windowSize) (10, 10)) white (showArray a)
glossMain = animate (InWindow "Window Name" (windowSize, windowSize) (10, 10)) white (\t -> showArray (animateArray t a))


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


rll = LED red True
gll = LED green True
bll = LED blue True

a = [
    [ rll, rll, rll, rll ]
  , [ gll, gll, gll, gll ]
  , [ bll, bll, bll, bll ]
  , [ rll, gll, bll, rll ]
  ]


animateArray :: Float -> Array -> Array
animateArray f = take (round f)

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
