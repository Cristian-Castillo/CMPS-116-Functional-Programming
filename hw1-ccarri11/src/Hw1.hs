{- 
	Developers:
	William Galvan Mendoza
	Cristian C. Castillo Jr
	====================================
	SID # 1576341
	Cruz ID: ccarri11
	TA Assistance: Austin, Devish
	====================================

	| CSE 116: Intro to Haskell Assignment.
     Do not change the skeleton code!

     You may only replace the `error "TBD:..."` parts.

     For this assignment, you may use any library function on integers
     but only the following library functions on lists:

     length
     (++)
     (==)

 -}

module Hw1 where

import Prelude  hiding (replicate, sum, reverse)


-- | Sum the elements of a list
--
-- >>> sumList [1, 2, 3, 4]
-- 10
--
-- >>> sumList [1, -2, 3, 5]
-- 7
--
-- >>> sumList [1, 3, 5, 7, 9, 11]
-- 36
-- Completed in LAB TA AUSTIN HELP OCT 15 12:19 PM
sumList :: [Int] -> Int
-- sumList xs , I split 
sumList [] = 0 -- define type Int array set = 0.. Base case if nothing return 0
sumList(x:xs) = sumList xs + x -- take the head sum the remaining integers xs 'elems' tail in the list


-- | `digitsOfInt n` should return `[]` if `n` is not positive,
--    and otherwise returns the list of digits of `n` in the
--    order in which they appear in `n`.
--
-- >>> digitsOfInt 3124
-- [3, 1, 2, 4]
--
-- >>> digitsOfInt 352663
-- [3, 5, 2, 6, 6, 3]

-- Received Tutoring help Devesh TA 1 4:00pm Oct 16, 2019 
-- 1. Establish base case aka edge condition(s)
-- 2. Subproblem
-- 3. Operation on sub problem
digitsOfInt :: Int -> [Int]
digitsOfInt n
	| n == 0 = [] -- edge condition, base case 
	| (n < 10) && (n > 0) = [n] -- singleton element, if only 1 digit integer return as singleton
	| otherwise =  digitsOfInt(n `div` 10) ++modMe:[] 
	where modMe = n `mod` 10

  -- NOTES: To Grader
  -- 3124 mod 10 = 4     n mod 10
  -- 3124 / 10 = 312     n div 10 

  -- 					  will be used for recursive, 
  -- 					  if we pass mod it will be set off by n < 10 condition
  --					  thus, pass in div to keep place holder of digits
  --					  then modMe is cons (like prepend) onto array each time
  -- 312 mod 10 = 2
  -- 312 / 10 = 31

		
-- | `digits n` returns the list of digits of `n`
--
-- >>> digits 31243
-- [3,1,2,4,3]
--
-- digits (-23422)
-- [2, 3, 4, 2, 2]

digits :: Int -> [Int] -- digits turns into positive num
digits n = digitsOfInt (abs n)


-- | From http://mathworld.wolfram.com/AdditivePersistence.html
--   Consider the process of taking a number, adding its digits,
--   then adding the digits of the number derived from it, etc.,
--   until the remaining number has only one digit.
--   The number of additions required to obtain a single digit
--   from a number n is called the additive persistence of n,
--   and the digit obtained is called the digital root of n.
--   For example, the sequence obtained from the starting number
--   9876 is 9876 -> 30 -> 3, so 9876 has
--   an additive persistence of 2 and
--   a digital root of 3.
--
-- NOTE: assume additivePersistence & digitalRoot are only called with positive numbers

-- >>> additivePersistence 9876
-- 2

-- >>> additivePersistence 99999
-- 2
-- Per Carl Lab 5:30pm Thur Oct 17, 2019 
-- Received tutoring and strategy on process on how to implement
-- a helper function that would help track the accumulator
helperFunc :: Int -> Int -> Int 
helperFunc x counter  
	| ( digitalRoot x == x ) = counter
	| otherwise = addOne
	where addOne =  helperFunc(digitalRoot x) ((counter+1)+1)

additivePersistence :: Int -> Int
additivePersistence n = 
		if(n == 0) 
		then 0 
		else count -- begin accumulator count
		where count = (helperFunc n 0)
		

-- | digitalRoot n is the digit obtained at the end of the sequence
--   computing the additivePersistence

-- >>> digitalRoot 9876
-- 3

-- >>> digitalRoot 99999
-- 9

digitalRoot :: Int -> Int
digitalRoot n 	
	| (n <= 0) = 0 -- So edge condition 
	| (n < 10 && n > 0) = n -- trap the digital root 
	| otherwise = accumulator 
	where accumulator = digitalRoot((sumList(digitsOfInt(n)))) 
	-- break n into a list, then add that list, and just recursively return 
	-- the digital root
	

-- | listReverse [x1,x2,...,xn] returns [xn,...,x2,x1]
--
-- >>> listReverse []
-- []
--
-- >>> listReverse [1,2,3,4]
-- [4,3,2,1]
--
-- >>> listReverse ["i", "want", "to", "ride", "my", "bicycle"]
-- ["bicycle", "my", "ride", "to", "want", "i"]
listReverse :: [a] -> [a]
listReverse [] = []  -- If we reverse an empty string you get empty string but nevertheless an empty list
listReverse (x:xs) = listReverse(xs) ++[x] -- head is now reverse tail, and reverse tail is now head

-- | In Haskell, a `String` is a simply a list of `Char`, that is:
--
-- >>> ['h', 'a', 's', 'k', 'e', 'l', 'l']
-- "haskell"
--
-- >>> palindrome "malayalam"
-- True
--
-- >>> palindrome "myxomatosis"
-- False

palindrome :: String -> Bool
palindrome w
	| (w == []) = False -- If we have no string, and since char return empty list
	| otherwise = if (w == listReverse(w)) -- Otherwise if (w= myStr) == listReverse(w) reversing the string
		then True						-- Then return
		else False

