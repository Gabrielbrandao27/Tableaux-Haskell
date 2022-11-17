
main = do
    putStrLn "Digite sua formula: "
    formula <- getLine
    print ("formula = " ++ formula)
    x $ ajeita formula
    val $ value x
    p $ prim x
    op $ operador x
    s $ seg x
    result $ regra val p op s
    gal1 $ galho1 result formula
    gal2 $ galho2 result gal1


oi :: String -> [String]
oi formula = regra (ajeita formula)

tchau :: [String] -> [String]
tchau formula = regra (ajeita2 formula)


formula = "F(avb)v(a^b)"

x = ajeita formula -- ["F","(avb)","v","(a^b)"]

resultx = regra x -- ["F","(avb)","F","(a^b)",";"]

listFormula = stringToList formula

gal1 = galho1 resultx listFormula -- ["F(avb)v(a^b)","F(avb)"]
gal2 = galho2 resultx gal1 -- ["F(avb)","F(a^b)"]

y = ajeita2 gal1 -- ["F","a","v","b"]
w = ajeita2 gal2 -- ["F","a","^","b"]

resulty = regra y -- ["F","a","F","b",";"]
resultw = regra w -- ["F","a","F","b","/"]

gal11 = galho1 resulty gal1 -- ["F(avb)","Fa"]
gal12 = galho2 resulty gal11 -- ["Fa","Fb"]

gal21 = galho1 resultw gal12 -- ["Fb","Fa"]
gal22 = galho2 resultw gal12 -- ["Fb","Fb"]

u = ajeita2 gal11 -- ["Fa"]
z = ajeita2 gal12 -- ["Fb"]

m = ajeita2 gal21 -- ["Fa"]
n = ajeita2 gal22 -- ["Fb"]




--formula = "F(avb)v(a^b)"
--formulaPrefix = "(v(v(a,b),^(a,b)))"

ajeita :: [Char] -> (Char, [Char], Char, [Char])
ajeita str = (value, prim, op, seg)
    where value = head str
          prim = take 5 (tail str) 
          op = last (take 7 str)
          seg = reverse (take 5 (reverse str))

--x = ajeita formula

value :: (Char, [Char], Char, [Char]) -> Char
value (a,_,_,_) = a

prim :: (Char, [Char], Char, [Char]) -> [Char]
prim (_,b,_,_) = b

operador :: (Char, [Char], Char, [Char]) -> Char
operador (_,_,c,_) = c

seg :: (Char, [Char], Char, [Char]) -> [Char]
seg (_,_,_,d) = d

{-val = value x
p = prim x
op = operador x
s = seg x-}

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

--result = regra val p op s

galho1 :: [String] -> [Char] -> [String]
galho1 result formula = [pai, filho]
    where filho = head result ++ head (tail result)
          pai = formula

galho2 :: [String] -> [String] -> [String]
galho2 result gal1  = [pai, filho]
    where filho = head (tail (tail result)) ++ last (init result)
          pai = last gal1

--gal1 = galho1 result formula
--gal2 = galho2 result gal1


{-arvore :: [String] -> [String]
arvore result formula
    | x == ";" = galho1 result formula-}
