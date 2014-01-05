{-# OPTIONS_GHC -O2 #-}

module Tools where

import Data.List
import Data.Array.Unboxed

primesListTo m = sieve 3 (array (3,m) [(i,odd i) | i<-[3..m]]
                        :: UArray Int Bool)
  where
    sieve p a
      | p*p > m   = 2 : [i | (i,True) <- assocs a]
      | a!p       = sieve (p+2) $ a//[(i,False) | i <- [p*p, p*p+2*p..m]]
      | otherwise = sieve (p+2) a

rotations xs = init (zipWith (++) (tails xs) (inits xs))
