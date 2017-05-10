-- Considerando a linguagem LFLE02, implemente uma variação (LFLE02E) com
-- escopo estático (utilize o capítulo 5 do livro base da disciplina como referência).

module LFLE02E where 

type Id = String
type Nome = String
type Arg = String 

type Ambiente = [DecFuncao]

type Referencias = [(Id, Int)]

data DecFuncao = DecFuncao Nome Arg Expressao 

data Expressao = Valor Int
               | Soma Expressao Expressao
               | Subtracao Expressao Expressao 
               | Multiplicacao Expressao Expressao
               | Divisao Expressao Expressao 
               | Let Id Expressao Expressao       
               | Ref Id 
               | Aplicacao Nome Expressao
 deriving(Show, Eq)

avaliar :: Expressao -> Ambiente -> Referencias -> Int
avaliar (Valor n) _ _ = n
avaliar (Soma e d) amb ref =  avaliar e amb ref + avaliar d amb ref
avaliar (Subtracao e d) amb ref = avaliar e amb ref - avaliar d amb ref
avaliar (Multiplicacao e d) amb ref =  avaliar e amb ref * avaliar d amb ref 
avaliar (Divisao e d) amb ref = avaliar e amb ref `div` avaliar d amb ref
avaliar (Aplicacao nome exp) amb ref = avaliar corpo amb ref' 
 where
   (DecFuncao n arg corpo) = pesquisarFuncao nome amb
   valor = avaliar exp amb ref 
   ref'  = (arg, valor):ref
avaliar (Let subId expNomeada corpoExp) amb ref = avaliar corpoExp amb ref'
  where
    valor = avaliar expNomeada amb ref
    ref'  = (subId, valor):ref
avaliar (Ref var) amb ref = pesquisarValor var ref

pesquisarFuncao :: Nome -> Ambiente -> DecFuncao
pesquisarFuncao nome [] = error ("Funcao " ++ nome ++ " nao declarada")
pesquisarFuncao nome (dec@(DecFuncao n a e):xs)
 | nome == n = dec
 | otherwise = pesquisarFuncao nome xs 

pesquisarValor :: Nome -> Referencias -> Int
pesquisarValor n [] = error ("identificador " ++ n ++ " nao declarado")
pesquisarValor nome ((n, v):rs)
 | nome == n = v
 | otherwise = pesquisarValor nome rs
