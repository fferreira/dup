module Adler
  (
    adler
  , adler32_foldl
  ) where

import Data.Bits (shiftL, (.|.), (.&.))
import Data.Char (ord)
import Data.List (foldl')

adler = adler32_foldl

base = 65521

adler32_foldl xs = let (a, b) = foldl step (1, 0) xs
                   in (b `shiftL` 16) .|. a
    where step (a, b) x = let a' = a + (ord x .&. 0xff)
                          in (a' `mod` base, (a' + b) `mod` base)

