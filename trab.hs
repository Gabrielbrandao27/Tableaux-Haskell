
main = do
    putStrLn "Digite sua formula: "
    formula <- getLine
    print ("formula = " ++ formula)

formula = "F(avb)v(a^b)"
--formulaPrefix = "(v(v(a,b),^(a,b)))"

ajeita :: String -> [String]
ajeita str = [value, prim, op, seg]
    where value = take 1 str
          prim = take 5 (tail str) 
          op = take 1 (tail (tail (tail (tail (tail (tail formula))))))
          seg = reverse (take 5 (reverse str))

x = ajeita formula

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

resultx = regra x

galho1 :: [String] -> String -> [String]
galho1 result formula = [pai, filho]
    where filho = head result ++ head (tail result)
          pai = formula

galho2 :: [String] -> [String] -> [String]
galho2 result gal1  = [pai, filho]
    where filho = head (tail (tail result)) ++ last (init result)
          pai = last gal1

gal1 = galho1 resultx formula
gal2 = galho2 resultx gal1

ajeita2 :: [String] -> [String]
ajeita2 gal1 = [value, prim, op, seg]
    where value = take 1 (last gal1)
          prim = take 1 (tail (tail (last gal1)))
          op = take 1 (tail (tail (tail (last gal1))))
          seg = take 1 (tail (reverse (last gal1)))

y = ajeita2 gal1
w = ajeita2 gal2

resulty = regra y
resultw = regra w

{-arvore :: [String] -> [String] -> [String]
arvore result formula gal
    | x == ";" = p
    | x == "/" = s
    where x = last result
          p = gal
          s = gal-}