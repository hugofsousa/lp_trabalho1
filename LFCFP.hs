-- Considerando a linguagem LFCF, implemente uma versao que posterga as substituicoes
-- usando um ambiente que realiza o mapeamento entre identificadores e
-- expressoes. Leia a Secao 6.4 do livro base da disciplina. O resultado deve ser a
-- linguagem LFCFP (P de substituicoes postergadas).

module LFCFP where 

-- | A linguagem LFCF suporta tanto 
-- expressoes identificadas (LET) quanto 
-- identificadores e funcoes de alta ordem
-- (com o mecanismo de expressoes lambda). 


type Id = String

data Expressao = Valor Int
               | Soma Expressao Expressao
               | Subtracao Expressao Expressao 
               | Multiplicacao Expressao Expressao
               | Divisao Expressao Expressao 
               | Let Id Expressao Expressao       
               | Ref Id
               | Lambda Id Expressao
               | Aplicacao Expressao Expressao   
 deriving(Show, Eq)

-- | O interpretador da linguagem LFCF
-- (funcao 'avaliar') precisa ser ajustada, uma vez que
-- o tipo de retorno nao pode ser simplesmente
-- um inteiro.
-- 
-- Isso fica evidente quando refletimos sobre o valor que
-- deveria ser retornado com a avaliacao de uma 
-- expressao lambda (\x -> x + 1). A resposta mais
-- adequada para isso eh: a funcao avaliar retorna uma
-- nova expressao, que corresponde ou a um Valor inteiro
-- ou a uma expressao Lambda. 

avaliar :: Expressao -> Expressao
avaliar (Valor n) = Valor n
avaliar (Soma e d) = avaliarExpBin e d (+)
avaliar (Subtracao e d) = avaliarExpBin e d (-) 
avaliar (Multiplicacao e d) = avaliarExpBin e d (*)
avaliar (Divisao e d) = avaliarExpBin e d div
avaliar (Let var expNomeada expCorpo) = avaliar (substituicao var (avaliar expNomeada) expCorpo)
avaliar (Ref var) = error "nao eh esperado avaliar uma referencia apos substituicao"
avaliar (Lambda argFormal corpo) = (Lambda argFormal corpo)
avaliar (Aplicacao exp1 exp2) = 
 let expLambda = avaliar exp1
 in case expLambda of
     (Lambda argFormal corpo) -> avaliar (substituicao argFormal (avaliar exp2) corpo)
     otherwise -> error $ "expressao " ++ show exp1 ++ " nao eh uma expressao lambda"
     
-- | Aplica o processo de substituicao nas expressoes. 
substituicao :: Id -> Expressao -> Expressao -> Expressao
substituicao var exp (Valor n) = Valor n
substituicao var exp (Soma e d) = Soma (substituicao var exp e) (substituicao var exp d)
substituicao var exp (Subtracao e d) = Subtracao (substituicao var exp e) (substituicao var exp d)
substituicao var exp (Multiplicacao e d) = Multiplicacao (substituicao var exp e) (substituicao var exp d)
substituicao var exp (Divisao e d) = Divisao (substituicao var exp e) (substituicao var exp d)
substituicao var exp (Lambda argFormal corpo) = Lambda argFormal (substituicao var exp corpo)
substituicao var exp (Aplicacao exp1 exp2) = Aplicacao (substituicao var exp exp1) (substituicao var exp exp2)
substituicao var exp (Let subId expNomeada corpo)
  | var == subId = (Let subId (substituicao var exp expNomeada) corpo)
  | otherwise = (Let subId (substituicao var exp expNomeada) (substituicao var exp corpo))
substituicao var exp (Ref ref)
  | var == ref = exp
  | otherwise = (Ref ref)
  
-- | Avalia uma expressao binaria.
avaliarExpBin :: Expressao -> Expressao -> (Int -> Int -> Int) -> Expressao
avaliarExpBin e d op = Valor (op ve vd)
 where
  (Valor ve) = avaliar e
  (Valor vd) = avaliar d
