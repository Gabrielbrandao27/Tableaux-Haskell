-- My Haskell Learning File
-- Press CTRL + L to clear terminal


-- Straigth forward functions

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x*2


-- List comprehension
-- the part before the pipe is the output function

first10Double x y = [x*2 | x <- [x..y], x*2 >= 10] -- takes a range cycles through it, and if the selected number doubled is >= 10, it returns the doubled number


bang xs = [if x < 10 then "BOOM" else "BANG" | x <- xs, odd x] -- takes a list and includes only the odds. If the number is < 10 returns BOOM and if it's bigger BANG


length' xs = sum [1 | _ <- xs] -- creates a length function by replacing every number on the list with 1 and then adding then up. "_" takes any element, doesn't care which


nouns = ["hobo","frog","pope"]  
adjectives = ["lazy","grouchy","scheming"]  
concat' = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns] -- concatenates nouns and adjectives with every possible combination


concat2' x y = [y ++ " " ++ x | y <- y, x <- x] -- same function but with input from the user


removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] -- removes non Upper chars by checking if the char is contained in the range ['A'..'Z']


oddsOut xxs = [[x | x <- xs, even x] | xs <- xxs] -- takes a list of lists and takes all the odds out


trian r = [(a,b,c) | c <- [1..r], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2] -- checks all the triangles within the input range and returns the perfect ones


fact x = product [x | x <- [1..x]] -- simple factorial function that multiplies every element on the list


-- Creating functions with matching patterns

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky 7777777"
lucky x = "Nop..."

factorial :: (Integral a) => a -> a
factorial n = product [1..n]

factRecu :: (Integral a) => a -> a
factRecu 0 = 1
factRecu n = n * factRecu (n-1)

charName :: Char -> String
charName 'a' = "Alberto"
charName 'b' = "Brow"
charName 'c' = "Cone"
charName x = "deu ruim pai"

addVet :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVet (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)