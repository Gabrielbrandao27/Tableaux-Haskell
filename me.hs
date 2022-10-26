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

first :: (a, b, c) -> a
first (x,_,_) = x

second :: (a, b, c) -> b
second (_,y,_) = y

third :: (a, b, c) -> c
third (_,_,z) = z

addVet' :: (Num a) => (a, a, a) -> (a, a, a) -> (a, a, a)
addVet' a b = (first a + first b, second a + second b, third a + third b)

head' :: [a] -> a
head' [] = error "List is Empty"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x -- Could be tell [x]
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y -- Could be tell [x, y]
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length2 :: (Num b) => [a] -> b
length2 [] = 0
length2 (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= underweight = "Underweight"
    | bmi <= regular     = "Regular"
    | bmi <= overweight  = "Overweight"
    | otherwise     = "Obese"
    where bmi = weight / height ^ 2
          (underweight, regular, overweight) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

-- Other way to do maximum'
maximum2' :: (Ord a) => [a] -> a
maximum2' [] = error "Empty"
maximum2' [x] = x
maximum2' (x:xs) = max x (maximum2' xs)

-- building my minimum':
minimum' :: (Ord a) => [a] -> a
minimum' [] = error "Empty"
minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs) -- Success

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x: replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0  = []
take' _ []    = []
take' n (x:xs) = x : take' (n-1) xs