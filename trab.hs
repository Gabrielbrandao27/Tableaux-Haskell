--formula = ("aVb", "V", "a^b")
str = "(aVb)V(a^b)"

ajeita :: String -> ([Char], Char, [Char])
ajeita str = (prim, op, seg)
    where prim = take 5 str
          op = last (take 6 str)
          seg = reverse (take 5 (reverse str))



operador ::(a, b, c) -> b
operador (_,b,_) = b
