-- Real World Haskell
-- Chapter 12: Barcode recognition

-- Continuation of Chapter 10: Parsing Binary data
-- Two kinds of barcodes: UPC-A or EAN-13 (US or Europe)
-- EAN-13 developed later. It is a superset of UPC-A
-- EAN-13: 13 digit, in 4 groups
-- 1st Group: 2 digits - Number system (Nationality of Manuf. or ISBN)
-- 2nd Group: 5 digits - Manufacturer Id (assigned by the country)
-- 3rd Group: 5 digits - Product Id (assigned by manufacturer)
-- 4th Group: 1 digit - check digit - to validate the scan

-- To calculate check digit:
-- Take the first 12 digits, multiply all even digits by 3. Add them together
-- Add the result with the sum of odd digits. Divide by 10. Find reminder.
-- Find the difference of 10 with the reminder. That's the check digit!
checkDigit :: (Integral a) => [a] -> a
checkDigit ds = 10 - (sum products `mod` 10)
    where products = mapEveryOther (*3) (reverse ds)
    
mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f, id])

-- Sequence of bits in the barcode
-- Leading Guard Sequence: 101
-- A group of six digits, 7-bit wide
-- Another Guard Sequence: 01010
-- A group of six more digits, 7-bit wide
-- Trailing Guard Sequence: 101

-- Separate encodings for the six digits on the left and on the right
-- Parity bits encode the left group and the 13th digit.

