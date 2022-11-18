
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

{-listToString :: [String] -> String
listToString gal = head gal ++ last gal-}

no1 = formula
no2 = last gal1
no3 = last gal2
no4 = head (ajeita2 gal11)
no5 = head (ajeita2 gal12)
no6 = head (ajeita2 gal21)
no7 = head (ajeita2 gal22)

nos = [no1, no2, no3, no4, no5, no6, no7]

galhos = [gal1, gal2, gal11, gal12, gal21, gal22]

{-
compara :: String -> String -> Int -> Bool
compara no1 no2 n
    | n == 1 = if a == b then True 
        else False
    | otherwise = if c == d then True
        else False
    where a = take 1 (reverse no1)
          b = take 1 (reverse no2)
          c = take 1 (no1)
          d = take 1 (no2)
-}

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

remove :: [String] -> [String]
remove nos = init nos

avanca :: [String] -> [String]
avanca nos = tail nos

contradicao :: [String] -> String -> String -> String
contradicao nos no1 no2
    | no1 /= no2 = if compara no1 no2 then "Contradicao"
                        else contradicao (remove aux1) (head (remove aux1)) (last (remove aux1))
    | otherwise = contradicao (avanca aux2) (head (avanca aux2)) (last (avanca aux2))
    where aux1 = nos
          aux2 = nos

{-


contradicao :: [String] -> String -> String -> String
contradicao nos no1 no2
    | no1 /= no2 = if (compara no1 no2 1) then
                        if (compara no1 no2 2) then
                            contradicao (remove aux) no1 (last (remove aux))
                        else "Contradicao"
                    else contradicao  (remove aux) no1 (last (remove aux))
    | no1 == no2 = contradicao (avanca nos) (head nos) (last nos)
    | otherwise = "nao ha contradicao"
    where aux = nos





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
    | no1 /= no2 = if compara no1 no2 then "Contradicao"
                        else contradicao (remove aux1) (head (remove aux1)) (last (remove aux1))
    | otherwise = contradicao (avanca aux2) (head (avanca aux2)) (last (avanca aux2))
    where aux1 = nos
          aux2 = nos






EX:
["F(avb)v(a^b)","F(avb)","F(a^b)","Fa","Fb","Fa","Fb"] "F(avb)v(a^b)" "Fb"
    "F(avb)v(a^b)" /= "Fb"
        compara "F(avb)v(a^b)" "Fb" 1


-}