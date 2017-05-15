-- Considerando a linguagem LFLE01E (implementada na primeira questão deste
-- projeto), implemente o suporte a funcões com um ou mais argumentos, gerando
-- a nova linguagems LFLE01EMP.

module LFLE01EMP where 

type Id = String
type Nome = String
type Arg = String 

type Args = [Arg]
type Expressoes = [Expressao]

type Ambiente = [DecFuncao]
 
data DecFuncao = DecFuncao Nome Args Expressao 

data Expressao = Valor Int
               | Soma Expressao Expressao
               | Subtracao Expressao Expressao 
               | Multiplicacao Expressao Expressao
               | Divisao Expressao Expressao 
               | Let Id Expressao Expressao       
               | Ref Id 
               | Aplicacao Nome Expressoes
 deriving(Show, Eq)

avaliar :: Expressao -> Ambiente -> Int
avaliar (Valor n) _ = n
avaliar (Soma e d) amb =  avaliar e amb + avaliar d amb
avaliar (Subtracao e d) amb = avaliar  e amb - avaliar d amb
avaliar (Multiplicacao e d) amb =  avaliar e amb * avaliar d amb 
avaliar (Divisao e d) amb = avaliar e amb `div` avaliar d amb

avaliar (Aplicacao nome (exp:exps)) amb = 
  let (DecFuncao n (arg:args) corpo) = pesquisarFuncao nome amb
  in avaliar (substituicaoLista (arg:args) (exp:exps) corpo amb) amb

avaliar (Let subId expNomeada corpoExp) amb =
  avaliar (substituicao subId (avaliar expNomeada amb) corpoExp) amb
avaliar (Ref var) _ = error "avaliando uma variavel livre." 

pesquisarFuncao :: Nome -> Ambiente -> DecFuncao
pesquisarFuncao nome [] = error ("Funcao " ++ nome ++ " nao declarada")
pesquisarFuncao nome (dec@(DecFuncao n a e):xs)
 | nome == n = dec
 | otherwise = pesquisarFuncao nome xs 


substituicaoLista [] [] corpo amb = corpo
substituicaoLista [] _ _ amb = error ("Numero de argumentos insuficientes")
substituicaoLista _ [] _ amb = error ("Numero de argumentos insuficientes")
substituicaoLista (arg:args) (exp:exps) corpo amb = 
  substituicaoLista args exps (substituicao arg (avaliar exp amb) corpo) amb

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

substituicao subId val (Aplicacao nome (exp:exps)) = 
  Aplicacao nome (substituicaoNasExpressoes subId val (exp:exps))

substituicaoNasExpressoes _ _ [] = []
substituicaoNasExpressoes subId val (exp:exps) =
  (substituicao subId val exp):(substituicaoNasExpressoes subId val exps)
