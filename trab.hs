
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
ajeita2 gal1
    | length form >= 3 = [value, prim, op, seg]
    | otherwise = [value++prim2]
    where form = last gal1
          value = take 1 (last gal1)
          prim = take 1 (tail (tail (last gal1)))
          prim2 = take 1 (tail (last gal1))
          op = take 1 (tail (tail (tail (last gal1))))
          seg = take 1 (tail (reverse (last gal1)))

y = ajeita2 gal1
w = ajeita2 gal2

resulty = regra y
resultw = regra w

gal11 = galho1 resulty gal1
gal12 = galho2 resulty gal1

gal21 = galho1 resultw gal12
gal22 = galho2 resultw gal12

u = ajeita2 gal11
z = ajeita2 gal12

m = ajeita2 gal21
n = ajeita2 gal22

{-arvore :: [String] -> [String] -> [String]
arvore result formula gal
    | x == ";" = p
    | x == "/" = s
    where x = last result
          p = gal
          s = gal-}