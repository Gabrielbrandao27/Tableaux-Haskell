
main = do
    putStrLn "Digite sua formula: "
    formula <- getLine
    print ("formula = " ++ formula)

formula = "F(avb)v(a^b)"

ajeita :: String -> [String]
ajeita str = [value, prim, op, seg]
    where value = take 1 str
          prim = take 5 (tail str) 
          op = take 1 (tail (tail (tail (tail (tail (tail formula))))))
          seg = reverse (take 5 (reverse str))

regra :: [String] -> [String]
regra x
    | op == "v" = if val == "V" then ["V", prim, "V", seg, "/"]
        else ["F", prim, "F", seg, ";"]
    | op == "^" = if val == "V" then ["V", prim, "V", seg, ";"]
        else ["F", prim, "F", seg, "/"]
    | op == ">" = if val == "V" then ["F", prim, "V", seg, "/"]
        else ["V", prim, "F", seg, ";"]
    | op == "~" = if val == "V" then ["F", prim]
        else ["V", prim]
    | otherwise = ["Operador Errado"]
    where op = last (init x)
          val = head x
          prim = head (tail x)
          seg = last x

ajeitaMaisRegra :: String -> [String]
ajeitaMaisRegra formula = regra (ajeita formula)

resultx = ajeitaMaisRegra formula

stringToList :: String -> [String]
stringToList form = [form]

listFormula = stringToList formula

galho1 :: [String] -> [String] -> [String]
galho1 result form = [pai, filho]
    where filho = head result ++ head (tail result)
          pai = last form

galho2 :: [String] -> [String] -> [String]
galho2 result gal1  = [pai, filho]
    where filho = head (tail (tail result)) ++ last (init result)
          pai = last gal1

gal1 = galho1 resultx listFormula
gal2 = galho2 resultx gal1

ajeita2 :: [String] -> [String]
ajeita2 formula
    | length form >= 3 = [value, prim, op, seg]
    | otherwise = [value++prim2]
    where form = last formula
          value = take 1 (last formula)
          prim = take 1 (tail (tail (last formula)))
          prim2 = take 1 (tail (last formula))
          op = take 1 (tail (tail (tail (last formula))))
          seg = take 1 (tail (reverse (last formula)))

ajeita2MaisRegra :: [String] -> [String]
ajeita2MaisRegra formula = regra (ajeita2 formula)

resulty = ajeita2MaisRegra gal1
resultw = ajeita2MaisRegra gal2

gal11 = galho1 resulty gal1
gal12 = galho2 resulty gal11

gal21 = galho1 resultw gal12
gal22 = galho2 resultw gal12

galhos = [gal1, gal2, gal11, gal12, gal21, gal22]

pegaNo :: [String] -> String
pegaNo gal = last gal

pegaNo2 :: [String] -> String
pegaNo2 gal = head (ajeita2 gal)

no1 = formula
no2 = pegaNo gal1
no3 = pegaNo gal2
no4 = pegaNo2 gal11
no5 = pegaNo2 gal12
no6 = pegaNo2 gal21
no7 = pegaNo2 gal22

nos = [no1, no2, no3, no4, no5, no6, no7] -- ["F(avb)v(a^b)","F(avb)","F(a^b)","Fa","Fb","Fa","Fb"]

head' :: [String] -> String
head' [] = ""
head' (x:xs) = x

last' :: [String] -> String
last' [] = ""
last' (x:[]) = x
last' (x:xs) = last' xs

compara :: String -> String -> Bool
compara no1 no2 = if a == b then 
                    if c /= d then 
                        True
                    else 
                        False
                else 
                    False
    where a = take 1 (reverse no1)
          b = take 1 (reverse no2)
          c = take 1 (no1)
          d = take 1 (no2)

removeUlt :: [String] -> [String]
removeUlt nos = init nos

avancaUm :: [String] -> [String]
avancaUm nos = tail nos

contradicao :: [String] -> String -> String -> String
contradicao nos no1 no2
    | (nos == [] || aux1 == [] || aux2 == []) = "Nao ha contradicao"

    | no1 /= no2 = if compara no1 no2 then "Contradicao"
                        else contradicao aux1 (head' aux1) (last' aux1)
    | no1 == no2 = contradicao aux2 (head' aux2) (last' aux2)

    where aux1 = (removeUlt nos)
          aux2 = (avancaUm nos)


{-

compara :: String -> String -> Bool
compara no1 no2
    | no1 /= no2 = if a == b then 
                        if c /= d then 
                            True
                        else 
                            False
                    else 
                        False
    | otherwise = False
    where a = take 1 (reverse no1)
          b = take 1 (reverse no2)
          c = take 1 (no1)
          d = take 1 (no2)

contradicao :: [String] -> String -> String -> String
contradicao nos no1 no2
    | (nos == [] || aux1 == [] || aux2 == []) = "Nao ha contradicao"

    | no1 /= no2 = if compara no1 no2 then "Contradicao"
                        else contradicao aux1 (head' aux1) (last' aux1)
    | no1 == no2 = contradicao aux2 (head' aux2) (last' aux2)

    where aux1 = (removeUlt nos)
          aux2 = (avancaUm nos)

-----------------------------------------------------------------------------
indexOf :: (Eq a) => a -> [a] -> Int
indexOf n [] = -1
indexOf n (x : xs)
  | n == x = 0
  | otherwise = case n `indexOf` xs of
      -1 -> -1
      i -> i + 1

getIndexOf :: Char -> String -> String -> Int
getIndexOf c str = head . (filter (> -1) . map (\y -> if y == c then indexOf y str else -1))

countOpenParens :: String -> Int
countOpenParens str = length $ filter (== '(') str

countCloseParens :: String -> Int
countCloseParens str = length $ filter (== ')') str

getFirstPart :: Char -> String -> String -> String
getFirstPart c str = head . (filter (not . null) . map (\y -> if y == c then take (getIndexOf c str str) str else []))

splitAtIndex :: Int -> [Char] -> ([Char], [Char])
splitAtIndex = \n -> \xs -> (take n xs, drop (n + 1) xs)

splitProgramsFunc :: String -> String -> (String, String)
splitProgramsFunc str =
  head
    . ( filter (not . null . fst)
          . map
            ( \y ->
                ( if (y == '>' || y == '^' || y == 'v') && (countOpenParens (getFirstPart y str str) == countCloseParens (getFirstPart y str str))
                    then splitAtIndex (getIndexOf y str str) str
                    else ("", "")
                )
            )
      )

-}