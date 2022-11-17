{-ajeita :: [Char] -> [String]
ajeita str = [val, p, op, s]
    where val = take 1 str
          p = take 5 (tail str) 
          op = take 1 (reverse (take 7 str))
          s = reverse (take 5 (reverse str))
-}

formula = "F(avb)~(a^b)"
formulaPrefix = "(v(v(a,b),^(a,b)))"

ajeita :: [Char] -> (Char, [Char], Char, [Char])
ajeita str = (value, prim, op, seg)
    where value = head str
          prim = take 5 (tail str) 
          op = last (take 7 str)
          seg = reverse (take 5 (reverse str))

x = ajeita formula

value :: (Char, [Char], Char, [Char]) -> Char
value (a,_,_,_) = a

prim :: (Char, [Char], Char, [Char]) -> [Char]
prim (_,b,_,_) = b

operador :: (Char, [Char], Char, [Char]) -> Char
operador (_,_,c,_) = c

seg :: (Char, [Char], Char, [Char]) -> [Char]
seg (_,_,_,d) = d

val = value x
p = prim x
op = operador x
s = seg x

regra :: Char -> [Char] -> Char -> [Char] -> [String]
regra val prim op seg
    | op == 'v' = if val == 'V' then ["V", prim, "V", seg, "/"]
        else ["F", prim, "F", seg, ";"]
    | op == '^' = if val == 'V' then ["V", prim, "V", seg, ";"]
        else ["F", prim, "F", seg, "/"]
    | op == '>' = if val == 'V' then ["F", prim, "V", seg, "/"]
        else ["V", prim, "F", seg, ";"]
    | op == '~' = if val == 'V' then ["F", prim]
        else ["V", prim]
    | otherwise = ["Operador Errado"]


input :: IO ()
input = do
    putStrLn ("Digite sua formula: ")
    entrada <- getLine
    print ("formula = " ++ entrada)
    