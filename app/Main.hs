module Main where

import Graphics.Gloss

import Lib


glossMain :: IO ()
glossMain = display (InWindow "Window Name" (300, 300) (10, 10)) white pic




pic = mconcat [ translate 0 (25*fromIntegral n) pp | n <- [-5..5] ]
  where
    pp = mconcat [ Color (fc n) (translate (25*fromIntegral n) 0 p) | n <- [-5..5] ]
    p = ThickCircle 5 10
    fc x
      | abs (x `rem` 3) == 0 = red
      | abs (x `rem` 3) == 1 = green
      | abs (x `rem` 3) == 2 = blue
      | otherwise = error (show x)


main :: IO ()
main = glossMain
