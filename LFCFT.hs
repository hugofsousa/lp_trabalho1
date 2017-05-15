-- Com o suporte a funçoes de primeira classe, as expressçoes do tipo Let se tornam
-- açucar sintatico, uma vez que podem ser trivialmente reescritas em termos de
-- aplicacoes de expressoes lambda (considere o exemplo abaixo como uma ilustracao).
-- Revise a implementacao da linguagem LFCF para que ocorra uma
-- traducao entre expressoes do tipo Let em expressoes Lambda, para que a avaliacao 
-- de uma expressao Let corresponda a avaliacao da expressao Lambda
-- correspondente. O resultado deve ser a linguagem LFCFT (T de transformacao
-- de Let em expressoes Lambda)

-- let x = 10
--    in x + 1
--
-- R: (\x -> x + 1) 10

module LFCFT where 

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

avaliar :: Expressao -> Expressao
avaliar (Valor n) = Valor n
avaliar (Soma e d) = avaliarExpBin e d (+)
avaliar (Subtracao e d) = avaliarExpBin e d (-) 
avaliar (Multiplicacao e d) = avaliarExpBin e d (*)
avaliar (Divisao e d) = avaliarExpBin e d div
avaliar (Let var expNomeada expCorpo) = -- avaliar (substituicao var (avaliar expNomeada) expCorpo)
  avaliar (Aplicacao (Lambda var expCorpo) expNomeada)

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
