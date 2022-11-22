
main = do
    putStrLn "Digite sua formula: "
    formula <- getLine
    print ("formula = " ++ formula)


------- Input -------

getProgram :: IO ()
getProgram = do
  print "Digite a formula para ser verificada: "
  line <- getLine
  print line
  print (ajeita line)

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

formula = "F(bva)^(b^a)"

ajeita :: String -> [String]
ajeita str = insert (head (splitValue str str)) (executeFunc str)

{-ajeita :: String -> [String]
ajeita str = [value, prim, op, seg]
    where value = take 1 str
          prim = take 5 (tail str) 
          op = take 1 (tail (tail (tail (tail (tail (tail formula))))))
          seg = reverse (take 5 (reverse str))-}

------- Tableaux -------

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

-- Faz o primeiro galho cujo pai é a fórmula
galho1 :: [String] -> [String] -> [String]
galho1 result form = [pai, filho]
    where filho = head result ++ head (tail result)
          pai = last form

-- Faz o segundo galho cujo pai é o primeiro galho
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

pegaNo :: [String] -> String
pegaNo gal = last gal

pegaNo2 :: [String] -> String
pegaNo2 gal = head (ajeita2 gal)

makeNo :: [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> [String]
makeNo gal1 gal2 gal11 gal12 gal21 gal22 = 
    [formula, 
    pegaNo (galho1 (ajeitaMaisRegra formula) (stringToList formula)), 
    pegaNo gal2, 
    pegaNo2 gal11, 
    pegaNo2 gal12,
    pegaNo2 gal21, 
    pegaNo2 gal22]

nos = makeNo gal1 gal2 gal11 gal12 gal21 gal22

{-head' :: [String] -> String
head' [] = ""
head' (x:xs) = x

last' :: [String] -> String
last' [] = ""
last' (x:[]) = x
last' (x:xs) = last' xs-}

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
                        else contradicao aux1 (head aux1) (last aux1)
    | no1 == no2 = contradicao aux2 (head aux2) (last aux2)

    where aux1 = (removeUlt nos)
          aux2 = (avancaUm nos)

results = [resultx, resulty, resultw]

arvRegra :: [String] -> String
arvRegra results
    | a == ";" = (x++y) ++ "\n" ++ (w++z)
    | a == "/" = (x++y) ++ " " ++ (w++z)
    where a = last results
          x = head results
          y = head (tail results)
          w = last (init (init results))
          z = last (init results)

avancaUmComposto :: [[String]] -> [[String]]
avancaUmComposto results = tail results

arvore :: [[String]] -> String
arvore [] = ""
arvore (x:xs) = arvRegra x ++ "\n" ++ arvore xs
arvore [x] = arvRegra x

bar :: IO()
bar = putStrLn (formula ++ "\n" ++ arvore results ++ "\n" ++ contradicao nos (head nos) (last nos))
