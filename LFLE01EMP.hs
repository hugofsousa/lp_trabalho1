module LFLE01 where 

-- A linguagem LFLE01 suporta tanto 
-- expressoes identificadas (LET) quanto 
-- identificadores e expressoes + funcoes. 
-- As funcoes aceitam apenas um argumento 
-- sem informacoes de tipo. 

type Id = String
type Nome = String
type Arg = String 

type Ambiente = [DecFuncao]
 
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

-- O interpretador da linguagem LLE eh 
-- basicamente um avaliador de expressoes, mas 
-- com suporte a substituicao. 

avaliar :: Expressao -> Ambiente -> Int
avaliar (Valor n) _ = n
avaliar (Soma e d) amb =  avaliar e amb + avaliar d amb
avaliar (Subtracao e d) amb = avaliar  e amb - avaliar d amb
avaliar (Multiplicacao e d) amb =  avaliar e amb * avaliar d amb 
avaliar (Divisao e d) amb = avaliar e amb `div` avaliar d amb

avaliar (Aplicacao nome exp) amb = 
  let (DecFuncao n arg corpo) = pesquisarFuncao nome amb
  in avaliar (substituicao arg (avaliar exp amb) corpo) amb

avaliar (Let subId expNomeada corpoExp) amb =
  avaliar (substituicao subId (avaliar expNomeada amb) corpoExp) amb
avaliar (Ref var) _ = error "avaliando uma variavel livre." 

pesquisarFuncao :: Nome -> Ambiente -> DecFuncao
pesquisarFuncao nome [] = error ("Funcao " ++ nome ++ " nao declarada")
pesquisarFuncao nome (dec@(DecFuncao n a e):xs)
 | nome == n = dec
 | otherwise = pesquisarFuncao nome xs 

-- pesquisarFuncao nome ambiente =
--  let res = [dec | dec@(DecFuncao n a e) <- ambiente, nome == n]
--  in  case res of
--    [dec] = dec
--    (d:ds) = error "duplicidade de declaracao de funcao"
--    otherwise = error "funcao nao existe"

     
substituicao :: Id -> Int -> Expressao -> Expressao
substituicao subId val (Valor n) = Valor n
substituicao subId val (Soma e d) = Soma (substituicao subId val e) (substituicao subId val d)
substituicao subId val (Subtracao e d) = Subtracao  (substituicao subId val e) (substituicao subId val d) 
substituicao subId val (Multiplicacao e d) = Multiplicacao  (substituicao subId val e) (substituicao subId val d)
substituicao subId val (Let boundId namedExp bodyExp) 
 | subId == boundId  = (Let boundId namedExp bodyExp)
 | otherwise = Let boundId namedExp (substituicao subId val bodyExp) 
substituicao subId val (Ref var) 
 | subId == var = (Valor val)
 | otherwise = (Ref var)
substituicao subId val (Aplicacao nome arg) = undefined

