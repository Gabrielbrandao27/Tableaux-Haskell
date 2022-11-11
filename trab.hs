
formula = "(avb)v(a^b)"

ajeita :: String -> ([Char], Char, [Char])
ajeita str = (prim, op, seg)
    where prim = take 5 str
          op = last (take 6 str)
          seg = reverse (take 5 (reverse str))


operador :: ([Char], Char, [Char]) -> Char
operador (_,b,_) = b


{-regra :: String -> Char -> ((Char, [Char]), (Char, [Char]))
regra str op
    | op == 'v' = (('V', prim), ('V', seg))
    otherwise error "Empty"
    where prim = take 5 str
          seg = reverse (take 5 (reverse str))-}


input :: IO ()
input = do
    putStrLn ("Digite sua formula: ")
    entrada <- getLine
    print ("formula = " ++ entrada)
    