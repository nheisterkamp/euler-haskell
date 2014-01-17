module Data.Digits (digits, digitsRev, unDigits, prop_digitsRoundTrip) where

import Test.QuickCheck

-- | Returns the digits of a positive integer as a list, in reverse order.
--   This is slightly more efficient than in forward order.
digitsRev :: Integral n
    => n -- ^ The base to use.
    -> n -- ^ The number to convert to digit form.
    -> [n] -- ^ The digits of the number in list form, in reverse.
digitsRev base i = case i of
        0 -> []
        _ -> lastDigit : digitsRev base rest
    where (rest, lastDigit) = quotRem i base

-- | Returns the digits of a positive integer as a list.
digits :: Integral n
    => n -- ^ The base to use (typically 10).
    -> n -- ^ The number to convert to digit form.
    -> [n] -- ^ The digits of the number in list form.
digits base = reverse . digitsRev base

-- | Takes a list of digits, and converts them back into a positive integer.
unDigits :: Integral n
    => n -- ^ The base to use.
    -> [n] -- ^ The digits of the number in list form.
    -> n -- ^ The original number.
unDigits base = foldl (\ a b -> a * base + b) 0

-- | unDigits . digits should be the identity, in any base.
prop_digitsRoundTrip
    :: Integer -- ^ The integer to test.
    -> Integer -- ^ The base to use.
    -> Property
prop_digitsRoundTrip i b = i > 0 ==> b > 1 ==> i == (unDigits b . digits b) i
