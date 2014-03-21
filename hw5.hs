module Main where

	import qualified Data.List
	import qualified Data.MemoTrie as Memo
	import qualified Data.Maybe


	----Problem 1
	sumNumbersNotDivisableBy2or3 :: Integer
	sumNumbersNotDivisableBy2or3 = sum [x | x <- [1..1000], mod x 2 /= 0, mod x 3 /= 0] --sums a filtered list of nums 1 to 1000 and nums where num%2 is 0 and num%3 is 0 are taken out of the list


	--Problem 2
	calc :: (Enum c, Num a, Ord a) => a -> [c] -> [c]
	calc x change 
		| x >= 25 = calc (x - 25) ([succ . head $ change] ++ tail change)--recursivly calls calc if there is still a quarter left in x, with x decreased by 25 and the first item int the list increased by one
		| x >= 10 = calc (x - 10) ([head change] ++ [succ (change!!1)] ++ (tail . tail $ change))--recursivly calls calc for dime, x - 10 and second item in list incremented by one
		| x >= 5 = calc (x - 5) ((init . init $ change) ++ [succ (change!!2)] ++ [last change])--recursivly calls calc for nickle, x - 5 and third item in list incremented by one
		| x /= 0 = calc (x - 1) ((init change) ++ [succ . last $ change])--recursivly calls calc for penny, x - 1 and last item in list incremented by one
		| otherwise = change--all change is returned 
		
	change1 :: Integer -> [Integer]
	change1 0 = [0, 0, 0, 0]
	change1 x = calc x [0, 0, 0, 0] -- initalizes recursive call to calc


	--Problem 4
	isPalindrome :: Int -> Bool
	isPalindrome n
		|len < 1 = False --if there are no digits then returns false
		|len <= 2 = same --checks if the remaing digits are the same if only one if left, then it returns true. 
		|otherwise	= if same then isPalindrome (read(init . tail  $ s)::Int) else False -- checks if first and last are same. if so then recurse but without the first and last of list. else false
		where 
			s = show n --converts n to a string
			len = length s --gets lenght of s
			same = (head s) == (last s) --compares the first and last digits of the number

	multNumsInList :: [Int] -> [Int]
	multNumsInList [] = []--checks for empty list
	multNumsInList (h:t) = (map (*h) (h:t)) ++ (multNumsInList t)--maps the head of the list times the rest of the list plus 

	palindromeProducts :: [Int] -> [Int]
	palindromeProducts lst = filter isPalindrome addedLst --filters out non palindrome numbers 
		where addedLst = Data.List.nub . multNumsInList $ lst --calls multNumsInList on list and then removes all duplicates in the list. 


	--Problem 6 
	--requires installing Data.MemoTrie with cabal 
	incPos :: [Int] -> Int -> [Int] -> Int -> ([Int], Int)
	incPos lst remain change pos = (take (pos) change ++ [succ (change!!pos)] ++ drop (pos + 1) change, remain - (lst!!pos))--increases the coin count is position pos

	findMin :: [Int] -> ([Int], Int) -> ([Int], Int)
	findMin = Memo.memo2 findMin' --memoizes results from findMin'
		where
			findMin' _ (change, 0) = (change, 0)--checks if all change has been distributed
			findMin' lst (change, remain) 
				| remain < 0 = (lst, 0) --checks to make sure remainder isn't 0
				| (filter (< 0) change) /= [] = (map (*(-200)) lst,0)--makes negative amounts of a coin return vary large nums to make sure they are not picked
				|otherwise = incs!!pos --gets the coins used/ reminang change pair with the least number of coins 
				where 
					f1 x = incPos lst remain change x --creates function to iterate number of coin in pos x
					f2 x = findMin lst (f1 x)--calls find min on the new number of coins
					len = length change--the lenght of change(number of coins in system)
					incs = map f2 [0..(len-1)]--maps f2 to the position of each coin in the system
					lists = map fst incs --creates a list of lists of coin numbers. (takes out leftover change)
					s = map sum lists --finds the sum of each list of coin numbers
					mini = minimum s --finds the minimum number of coins used
					pos = Data.Maybe.fromJust (Data.List.elemIndex mini s) --gets the index of the list with the minimum number of coins used 

	change2 :: [Int] -> Int -> [Int]
	change2 lst ch = fst (findMin lst (map (*0) lst, ch))--returns the minimnum number of each type of coin to give correct change

