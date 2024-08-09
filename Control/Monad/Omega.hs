{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
----------------------------------------------
-- |
-- Module    : Control.Monad.Omega
-- Copyright : (c) Luke Palmer 2008
-- License   : Public Domain
--
-- Maintainer : Luke Palmer <lrpalmer@gmail.com>
-- Stability : experimental
-- Portability : portable
--
-- A monad for enumerating sets: like the list monad, but
-- impervious to infinite descent.
--
-- A depth-first search of a data structure can fail to give a full traversal
-- if it has an infinitely deep path.  Likewise, a breadth-first search of a
-- data structure can fall short if it has an infinitely branching node.
-- Omega addresses this problem by using a \"diagonal\" traversal
-- that gracefully dissolves such data.
--
-- So while @liftM2 (,) [0..] [0..]@ gets \"stuck\" generating tuples whose
-- first element is zero, @"runOmega" $ liftM2 (,) ("each" [0..]) ("each"
-- [0..])@ generates all pairs of naturals.
--
-- More precisely, if @x@ appears at a finite index in
-- @xs@, and @y@ appears at a finite index in @f x@,
-- then @y@ will appear at a finite index in @each xs >>= f@. 
--
-- This monad gets its name because it is a monad over sets of order type
-- omega.
--
-- Warning: Omega is only a monad when the results of @runOmega@ are
-- interpreted as a set; that is, a valid transformation according to the
-- monad laws may change the order of the results.  However, the same
-- set of results will always be reachable.  If you are using this as a monad, 
-- I recommend that you use the newer weighted-search package instead 
-- (it's also faster).
----------------------------------------------

module Control.Monad.Omega 
    (diagonal, Omega, runOmega, each) 
where

import qualified Control.Monad as Monad
import qualified Control.Applicative as Applicative
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable

import qualified Control.Monad.Fail as Fail
import Data.List (transpose, uncons)
import Data.Maybe (mapMaybe)

-- | This is the hinge algorithm of the Omega monad,
-- exposed because it can be useful on its own.  Joins 
-- a list of lists with the property that for every i j 
-- there is an n such that @xs !! i !! j == diagonal xs !! n@.
-- In particular, @n <= (max(i, j) + 1) ^ 2@.
diagonal :: [[a]] -> [a]
diagonal xss = concat $ go 0 [] xss
    where
        go :: Int -> [[a]] -> [[a]] -> [[a]]
        go !_ yss [] = transpose yss
        go n yss (zs : zss) = us : hs : go (n + 1) tss zss
            where
                (us, vs) = splitAtReversed n zs
                (hs, tss) = unzip $ mapMaybe uncons $ vs : yss

splitAtReversed :: Int -> [a] -> ([a], [a])
splitAtReversed = go []
    where
        go acc n xs
            | n <= 0 = (acc, xs)
        go acc _ [] = (acc, [])
        go acc n (x : xs) = go (x : acc) (n - 1) xs

newtype Omega a = Omega { runOmega :: [a] }

each :: [a] -> Omega a
each = Omega

instance Functor Omega where
    fmap f (Omega xs) = Omega (map f xs)

instance Monad Omega where
    return = pure
    Omega m >>= f = Omega $ diagonal $ map (runOmega . f) m

#if !(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
#endif

instance Fail.MonadFail Omega where
    fail _ = Omega []

instance Monad.MonadPlus Omega where
    mzero = Applicative.empty
    mplus = (Applicative.<|>)

instance Applicative.Applicative Omega where
    pure = Omega . (:[])
    liftA2 f (Omega as) (Omega bs) = Omega $ concat $ go [] [] as bs
        where
            go initXs initYs (x : xs) (y : ys) =
                map (`f` y) initXs : (f x y : map (f x) initYs) :
                    go (x : initXs) (y : initYs) xs ys
            go initXs initYs [] (y : ys) =
                map (`f` y) initXs : go initXs (y : initYs) [] ys
            go initXs initYs (x : xs) [] =
                map (f x) initYs : go (x : initXs) initYs xs []
            go _ _ [] [] = []

instance Applicative.Alternative Omega where
    empty = Omega []
    Omega xs <|> Omega ys = Omega $ interleave xs ys

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x : xs) ys = x : interleave ys xs

instance Foldable.Foldable Omega where
    foldMap f (Omega xs) = Foldable.foldMap f xs

instance Traversable.Traversable Omega where
    traverse f (Omega xs) = fmap Omega $ Traversable.traverse f xs
