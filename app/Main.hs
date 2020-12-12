module Main where

import Graphics.Gloss
import Data.Function (on)
import System.Random
import Data.List

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
    (HeatModel (mkStdGen 100) [initFire])
    (showArray . heatToArray . heatArray)
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

data Model = HeatModel {
    randomGenerator :: StdGen
  , heatArray :: Heat
}

type Heat = [[Int]]

initFire = replicate 10 100

fire :: Model -> Model
fire (HeatModel rg orig) = HeatModel newg (initFire : take 10 top)
  where
    --L.unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
    f :: (Int, StdGen) -> Maybe ((Int, StdGen), (Int, StdGen))
    f (_, g) =
      let (val, g') = randomR (1, 10) g
      in Just ((val, g'), (val, g'))

    stff = (take 10 $ unfoldr f (0, rg)) :: [(Int, StdGen)]
    aa = (map fst stff) :: [Int]
    newg = snd . last $ stff
    makeNewRow :: [Int] -> [Int]
    makeNewRow r = zipWith (\a x -> subtract (10*a) x) aa r
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
