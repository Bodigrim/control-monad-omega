{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Control.Monad.Omega
import Data.List (sort)
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "All"
  [ testProperty "pure" $ \(x :: Int) ->
    sort (runOmega (pure x)) ===
      sort (pure x)
  , testProperty "liftA2" $ \(xs :: [Int]) ys ->
    sort (runOmega (liftA2 (+) (each xs) (each ys))) ===
      sort (liftA2 (+) xs ys)
  , testProperty "(<|>)" $ \(xs :: [Int]) ys ->
    sort (runOmega (each xs <|> each ys)) ===
      sort (xs <|> ys)
  , testProperty "join" $ \(xss :: [[Int]]) ->
    sort (runOmega (join (each (map each xss)))) ===
      sort (join xss)

  , testProperty "liftA2 vs. join" $ \(xs :: [Int]) ys ->
    let f x y = x * 10 + y in
    runOmega (liftA2 f (each xs) (each ys)) ===
      runOmega (join (each (map (\y -> each (map (\x -> f x y) xs)) ys)))
  ]

