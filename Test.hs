-- LFLE01
inc = DecFuncao "inc" "x" (Soma (Ref "x") (Valor 1)) 


-- let x = 5 + 3 in x + 2
avaliar (Let "x" (Soma (Valor 5) (Valor 3)) (Soma (Ref "x") (Valor 2))) []

avaliar (substituicao "x" 3 (Aplicacao "inc" (Ref "x"))) [inc]

avaliar (Aplicacao "inc" sub1) [inc]

-- let inc n = n+1 in let x = 3 + 2 in inc x
avaliar (Let "x" (Soma (Valor 3) (Valor 2)) (Aplicacao "inc" (Ref "x"))) [inc]
 




-- LFLE02E

refs = [("x", 15), ("x", 2), ("y", 5)]