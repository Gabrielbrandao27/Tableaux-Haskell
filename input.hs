
getProgram :: IO ()
getProgram = do
  print "Digite a formula para ser verificada: "
  line <- getLine
  print line
  print (getFullValues line)

countOpenParens :: String -> Int
countOpenParens str = length $ filter (== '(') str

countCloseParens :: String -> Int
countCloseParens str = length $ filter (== ')') str

splitAtIndex :: Int -> [Char] -> [[Char]]
splitAtIndex = \n -> \xs -> [take n xs, [xs !! max 0 n], drop n (tail xs)]

-- variant of map that passes each element's index as a second argument to f
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0 ..]

splitProgramsFunc :: String -> String -> [String]
splitProgramsFunc str =
  head
    . ( filter (not . null)
          . mapInd
            ( \y l ->
                ( if (y == '>' || y == '^' || y == 'v') && (countOpenParens (take l str) == countCloseParens (take l str))
                    then splitAtIndex l str
                    else []
                )
            )
      )

splitByFormulaValue :: String -> Char -> [String]
splitByFormulaValue [] delim = [""]
splitByFormulaValue (c : cs) delim
  | c == delim = [c] : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = splitByFormulaValue cs delim

splitValue :: String -> [Char] -> [String]
splitValue str = head . (filter (not . null) . map (\y -> if y == 'V' || y == 'F' then splitByFormulaValue str y else []))

-- v((avb),(b^a))
-- v(v(a,b),^(b,a))
-- "Vb>(a^(bva))"
-- "F(avb)v(a^b)"

getSecondProgram :: [String] -> String
getSecondProgram list = list !! 1

returnSecondProgram :: String -> String
returnSecondProgram str = getSecondProgram (splitValue str str)

executeFunc :: String -> [String]
executeFunc str = splitProgramsFunc (returnSecondProgram str) (returnSecondProgram str)

insert :: String -> [String] -> [String]
insert str list = [val, fstList, sndList, thrList]
  where
    val = str
    fstList = head list
    sndList = head (tail list)
    thrList = last list

getFullValues :: String -> [String]
getFullValues str = insert (head (splitValue str str)) (executeFunc str)