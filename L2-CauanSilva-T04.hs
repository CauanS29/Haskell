


type CadastroSUS = [Cidadao]
type CPF = Integer
type Nome = String
type Genero = Char
type Dia = Int
type Mes = Int
type Ano = Int
type Data = (Dia, Mes, Ano)
type DataNasc = Data
type Endereco = String
type Municipio = String
type Estado = String
type Telefone = String
type Email = String
type Cidadao = (CPF, Nome, Genero, DataNasc, Endereco, Municipio, Estado, Telefone, Email)



meuCadastro :: CadastroSUS
meuCadastro = [ (45821353713, "Gabriel Dantas", 'M', (06,03,2003), "Rua G, 303", "Aracaju", "Sergipe", "998231657", "GabrielDantas@live.com"),  
                (23467487628, "Laize Sena", 'F', (02,07,2002), "Rua L, 71", "Nova Soure", "Bahia", "999415688", "LaizeSena@gmail.com"),     
                (87623415721, "Guilherme Argolo", 'X', (10,02,2001), "Rua F, 24", "Ilheus", "Bahia", "998452317", "ArgoloGuilherme@hotmail.com"), 
                (67345890235, "Irvine Ferreira", 'M', (14,11,1999), "Rua B, 44", "Lagoa Doce", "Bahia", "999873876","FerreiraIrvine@gmail.com") ] 



filtrarCPF :: Cidadao -> CPF 
filtrarCPF (cpf, _, _, _, _, _, _, _, _) = cpf 


checaCPF :: CPF -> CadastroSUS -> Bool 
checaCPF cpf banco = or [cpf == filtrarCPF cidadao |cidadao <- banco ] 

 


-- a) Atualizar endereço 

atualizaEndSUS :: CPF -> CadastroSUS -> Endereco -> CadastroSUS 
atualizaEndSUS  _ [] _ = []  
atualizaEndSUS cpf (x:xs) endNovo  
                              | filtrarCPF x == cpf = (cpf, nome, genero, dataNasc, endNovo, municipio, estado, telefone, email) : atualizaEndSUS cpf xs endNovo  
                              | otherwise = x : atualizaEndSUS cpf xs endNovo 
                             where  (cpf, nome, genero, dataNasc, endereco, municipio, estado, telefone, email) =  x  				

-- b) Remover o cidadao da lista.  

removeSUS :: CPF -> CadastroSUS -> CadastroSUS 
removeSUS _ [] = [] 
removeSUS cpf (x:xs) 
                     | filtrarCPF x == cpf = xs 
					 | otherwise = x : removeSUS cpf xs 


-- c) geraListaMunicipioFaixas 

type IdadeInicial = Int
type IdadeFinal = Int
type FaixaIdade = (IdadeInicial, IdadeFinal)
type Quantidade = Int

filtrarMunicipio :: Cidadao -> Municipio
filtrarMunicipio (_, _, _, _, _, municipio, _, _, _) = municipio  


filtrarEstado :: Cidadao -> Estado
filtrarEstado (_, _, _, _, _, _, estado, _, _) = estado 


filtrarNascimento :: Cidadao -> DataNasc
filtrarNascimento (_, _, _, dataNasc, _, _, _, _, _) = dataNasc 

idade :: DataNasc -> Data ->  Int 
idade (dia, mes, ano) (da, ma, aa) 
                      | mes == ma && dia <= da = aa - ano   
                      | mes == ma && dia >= da = aa - ano 
                      | mes > ma =  aa  - ano 
                      | mes < ma =  aa  - ano  


gerarlistaMunicipiosFaixas :: CadastroSUS -> Municipio -> [FaixaIdade] -> Data -> [(FaixaIdade, Quantidade)]
gerarlistaMunicipiosFaixas [] _ _ _ = [] 
gerarlistaMunicipiosFaixas _ _ [] _ = [] 
gerarlistaMunicipiosFaixas (x:xs) municipio (v:vs) dat 
                                                        | filtrarMunicipio x == municipio = (v, cidadaosPorMunicipioIdade (x:xs) municipio v  dat) : gerarlistaMunicipiosFaixas (x:xs) municipio vs dat 
                                                        | otherwise = gerarlistaMunicipiosFaixas xs municipio (v:vs) dat  


cidadaosPorMunicipio :: CadastroSUS -> Municipio -> Quantidade 
cidadaosPorMunicipio [] municipio = 0 
cidadaosPorMunicipio (x:xs) municipio 
                                      | filtrarMunicipio x == municipio = 1 + cidadaosPorMunicipio xs municipio 
									  | otherwise = cidadaosPorMunicipio xs municipio 


cidadaosPorMunicipioIdade :: CadastroSUS -> Municipio-> FaixaIdade -> Data -> Quantidade 
cidadaosPorMunicipioIdade [] _ _ _ = 0 
cidadaosPorMunicipioIdade (x:xs) municipio (idInicial, idFinal) dat 
                                                          | filtrarMunicipio x == municipio && idade (filtrarNascimento x) dat >= idInicial && idade (filtrarNascimento x) dat <= idFinal = 1 + cidadaosPorMunicipioIdade xs municipio (idInicial, idFinal) dat 
														  | otherwise = cidadaosPorMunicipioIdade xs municipio (idInicial, idFinal) dat 



-- d) aplicaPrimDose 

type Vacinados = [Vacinado] 

type Vacina = String
type TipoDose = Int
type Dose = (Vacina, Data)
type Doses = [Dose]
type Vacinado = (CPF, Doses) 

bancoVacinados = [ (45821353713, [("AstraZeneca", (25,09,2021))]),
                   (23467487628, [("CoronaVac", (12,06,2021))]), 
				   (87623415721, [("Pfizer", (10,05,2021))]),
				   (67345890235, [("Janssen", (18,04,2021)),("Janssen", (18,04,2021))]) ]  



aplicaPrimDose :: CPF -> CadastroSUS -> FaixaIdade -> Municipio -> Vacina -> Data -> Vacinados -> Vacinados 
aplicaPrimDose _ [] _ _ _ _ _ = [] 
aplicaPrimDose _ _ _ _ _ _ [] = [] 
aplicaPrimDose cpf (x:xs) (idInicial, idFinal) municipio vacina dat (y:ys) 
                                                                           | tomouPrimeiraDose cpf (y:ys) = error "Primeira dose ja aplicada" 
                                                                           | checaCPF cpf (x:xs) == False = error "usuario nao cadastrado no sistema" 
																	       | checaIdade cpf (x:xs) (idInicial, idFinal) dat == False = error "Esse cidadao nao esta dentro da faixa" 
																	       | checaMuni cpf (x:xs)  municipio == False = error "Municipio incompativel, atualize os dados SUS" 
																	       | vacina == "Janssen" = (cpf, (replicate 2 (vacina, dat))) : bancoVacinados 
																	       | otherwise = (cpf, [(vacina, dat)]) : bancoVacinados 


checaMuni :: CPF -> CadastroSUS -> Municipio -> Bool  
checaMuni _ [] _ = False 
checaMuni cpf (x:xs) municipio 
                               | filtrarCPF x == cpf && filtrarMunicipio x == municipio = True 
							   | otherwise = checaMuni cpf xs municipio 


checaIdade :: CPF -> CadastroSUS -> FaixaIdade -> Data -> Bool  
checaIdade _ [] _ _ = False 
checaIdade cpf (x:xs) (idInicial, idFinal) dat 
                                               | filtrarCPF x == cpf  && idade (filtrarNascimento x) dat >= idInicial && idade (filtrarNascimento x) dat <= idFinal = True 
											   | otherwise = checaIdade cpf xs (idInicial, idFinal) dat  
 


tomouPrimeiraDose :: CPF -> Vacinados -> Bool 
tomouPrimeiraDose _ [] = False 
tomouPrimeiraDose cpf (x:xs) 
                             | filtrarCPFVacinados x == cpf = True  
							 | otherwise = tomouPrimeiraDose cpf xs 

filtrarCPFVacinados (cpf, _) = cpf   


-- e) quantidadeDoseMun 


quantidadeDoseMun :: Vacinados -> TipoDose -> Municipio -> CadastroSUS -> Quantidade
quantidadeDoseMun _ _ _ [] = 0 
quantidadeDoseMun (x:xs) dose municipio (y:ys) 
                                               | filtrarMunicipio y == municipio && checaNumDose (filtrarCPF y) bancoVacinados == dose = 1 + quantidadeDoseMun (x:xs) dose municipio ys
                                               | otherwise = quantidadeDoseMun (x:xs) dose municipio ys


checaNumDose :: CPF -> Vacinados -> Int
checaNumDose _ [] = 0
checaNumDose cpf1 ((cpf, doses):xs)
                       | cpf1 == cpf = length doses
                       | otherwise = checaNumDose cpf1 xs



--- f) quantidadeEstIdDose 


quantidadeEstIdDose :: Vacinados -> Estado -> FaixaIdade -> Data  -> TipoDose -> CadastroSUS -> Quantidade 
quantidadeEstIdDose _ _ _ _ _ [] = 0 
quantidadeEstIdDose (x:xs) estado (idInicial, idFinal) dat dose (y:ys) 

                                                                       | filtrarEstado y == estado && checaNumDose (filtrarCPF y) xs == dose && idade (filtrarNascimento y) dat >= idInicial && idade (filtrarNascimento y) dat <= idFinal = 1 + quantidadeEstIdDose (x:xs) estado (idInicial, idFinal) dat dose ys 
																	   | otherwise = quantidadeEstIdDose (x:xs) estado (idInicial, idFinal) dat dose ys  
																	   


 -- g) quantidadeEstVacDose 
 
 
quantidadeEstVacDose :: Vacinados -> Estado -> Vacina -> TipoDose -> CadastroSUS -> Quantidade
quantidadeEstVacDose _ _ _ _ [] = 0
quantidadeEstVacDose [] _ _ _ _ = 0
quantidadeEstVacDose xs estadoInfor vacina dose (y:ys)
    | estadoInfor == estado &&  nDose == dose &&  nVacina == vacina = 1 + quantidadeEstVacDose xs estado vacina dose ys
    | otherwise = quantidadeEstVacDose xs estado vacina dose ys
    where
      estado = filtrarEstado y
      nDose = checaNumDose (filtrarCPF y) xs
      nVacina = filtrarVac1 (filtrarCPF y) xs 
 
 
filtrarVac1 :: CPF -> Vacinados  -> Vacina 
filtrarVac1 _ []  = ""
filtrarVac1 cpf (x:xs) 
                        | filtrarCPFVacinados x == cpf = filtrarVac2 x
                        | otherwise = filtrarVac1 cpf xs 


filtrarVac2 :: Vacinado -> Vacina 
filtrarVac2 (_, [(vacina, _)]) = vacina 
filtrarVac2(_, [(vacina, _), (vacina2, _)]) = vacina
 
 
 
 
{- 2) Suponha que o gestor municipal queira calcular a próxima faixa de idade que será
vacinada de acordo com a quantidade de vacinas que chegaram ao município.
Uma dada idade só pode ser considerada na faixa se o número de doses que
chegaram for suficiente para vacinar todas as pessoas daquela idade. Se este não
for o caso, o município guardará as vacinas que restaram para a próxima fase de
vacinação. Por exemplo, suponha que todos os cidadãos de 25 anos já foram
vacinados e chegaram ao município 5000 doses de vacina para serem aplicadas
como primeira dose. Digamos que para as idades de 24, 23, 22 e 21 anos a
população a vacinar seja 1000, 1500, 2000, 2000, respectivamente. A sua função
deve retornar a faixa (24, 22), pois o montante dessas idades é de 4500 cidadãos.
As 500 doses restantes ficarão para a próxima fase de vacinação, quando
chegarem mais doses. A sua função deve considerar como entrada o cadastroSUS,
a última idade já vacinada, o município e a quantidade de vacinas que chegaram.
A saída deve ser a faixa de idade (maior idade, menor idade) para ser vacinadas
com as doses que chegaram-} 


type Idade =  Int 
type Populacao = Int
type PopMun = (Municipio, [(FaixaIdade, Populacao)])
type PopEstado = (Estado, [PopMun])
type PopPais = [PopEstado] 


cadastrodemografico :: PopPais 
cadastrodemografico = [  ("Sergipe",[("Aracaju",[((0,10),100),((11,20),100),((21,30),100),((31,40),100),((41,50),100),((51,60),100),((61,70),100),((71,80),100),((81,90),100),((91,100),100),((101,110),40),((111,120),20),((121,130),5)])]), 
                         ("Bahia",[("Nova Soure",[((0,10),100),((11,20),100),((21,30),100),((31,40),100),((41,50),80),((51,60),70),((61,70),40),((71,80),30),((81,90),20),((91,100),10),((101,110),3),((111,120),0),((121,130),1)]),
                         ("Ilheus",[((0,10),100),((11,20),100),((21,30),100),((31,40),100),((41,50),80),((51,60),60),((61,70),30),((71,80),25),((81,90),12),((91,100),3),((101,110),1),((111,120),1),((121,130),0)]), 
						 ("Lagoa Doce",[((0,10),100),((11,20),100),((21,30),120),((31,40),90),((41,50),70),((51,60),60),((61,70),40),((71,80),30),((81,90),15),((91,100),8),((101,110),2),((111,120),1),((121,130),0)])]) ] 
					  


quantidadeVacinas :: CadastroSUS -> Idade -> Municipio -> Quantidade -> Data -> FaixaIdade 
quantidadeVacinas _ _ _ 0 _  = (0, 0) 
quantidadeVacinas _ 0 _ _ _  = (0, 0) 
quantidadeVacinas (x:xs) idade municipio quantidade dat 
                                                    |  cidadaosPorMunicipioIdade (x:xs) municipio (idade - 1, idade - 1) dat <= quantidade = ( head auxiliar , last auxiliar ) 
													|  otherwise = (0, 0) 
												where 
                                                  auxiliar  = pegarIdade (x:xs) idade  municipio quantidade dat    


pegarIdade :: CadastroSUS -> Idade -> Municipio -> Quantidade -> Data  -> [Idade] 
pegarIdade _ _ _ 0 _ = [] 
pegarIdade _ 0 _ _ _ = [] 
pegarIdade (x:xs) idade  municipio quantidade dat 
                                                 | cidadaosPorMunicipioIdade (x:xs) municipio (idade -1 , idade -1) dat <= quantidade = (idade - 1) : pegarIdade (x:xs) (idade - 2) municipio (quantidade -  cidadaosPorMunicipioIdade (x:xs) municipio (idade - 1, idade - 1) dat) dat 
                                                 | otherwise = [] 
 


gerarlistafaixa :: Int -> Int -> [FaixaIdade] 
gerarlistafaixa _ 0 = [] 
gerarlistafaixa x y 
                      | x == y = [] 
                      | x == 0 = (x, x+10) : gerarlistafaixa (x + 10) y 
                      | x /= 0 = (x+1, x+10) : gerarlistafaixa (x + 10) y  


gerarlista130 :: [FaixaIdade] 
gerarlista130 = gerarlistafaixa 0 130 


{-3) Com base no cadastro de vacinados, o cadastro SUS e o cadastro demográfico,
elabore uma função que dado um estado e um tipo de dose, calcule o percentual
da população vacinada com a dose informada, em todas as faixas etárias de dez
anos. -} 



continhaPrincipal :: CadastroSUS -> Vacinados -> PopPais -> Estado -> TipoDose -> [FaixaIdade] -> Data-> [(FaixaIdade, Float)]         
continhaPrincipal xs ys zs  estado dose faixaIdades dat = zip gerarlista130 (continha xs ys zs estado dose faixaIdades dat )      



continha :: CadastroSUS -> Vacinados -> PopPais -> Estado -> TipoDose -> [FaixaIdade] -> Data-> [Float]
continha _ _ [] _ _ _ _ = []
continha _ [] _ _ _ _ _ = []
continha _ _ _ _ _ [] _ = []
continha (x:xs) (y:ys) (z:zs) estado dose (faixaIdade:faixaIdades) dat = (fromIntegral(quantidadeEstIdDose (y:ys) estado faixaIdade dat dose (x:xs))/fromIntegral (ajuda2 faixaIdade (ajuda z))) * 100 : continha (x:xs) (y:ys) (z:zs) estado dose faixaIdades dat 
                                                                                                                                               




ajuda :: PopEstado -> [(FaixaIdade, Quantidade)] 
ajuda popEstado = zip (gerarlista130) (sumPopEstado popEstado)



ajuda2 :: FaixaIdade -> [(FaixaIdade,Quantidade)] -> Quantidade
ajuda2 _ [] = 0
ajuda2 faixaIdade (x:xs) 
                         | fst x == faixaIdade = snd x
						 | otherwise = ajuda2 faixaIdade xs 




somaQuantidades :: PopMun -> [Int] 
somaQuantidades (_, [(_, q1), (_, q2), (_, q3), (_, q4), (_, q5), (_, q6), (_, q7), (_,q8), (_, q9), (_, q10), (_, q11), (_, q12), (_, q13)]) = [q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13] 



sumEstado :: [Int] -> [Int] -> [Int]
sumEstado (x:xs) [] = (x:xs) 
sumEstado [] (y:ys) = (y:ys) 
sumEstado [] [] = [] 
sumEstado (x:xs) (y:ys) = x + y : sumEstado xs ys  


sumPopEstado :: PopEstado -> [Int] 
sumPopEstado (estado, []) = [] 
sumPopEstado (estado, (x:xs)) = sumEstado (somaQuantidades x) (sumPopEstado (estado, xs)) 




-- 4) 












-- 6) consultas sobre quantidades de doses por estado e gerar a lista de faixas do estados 


quantidadeDoseEst :: Vacinados -> TipoDose -> Estado -> CadastroSUS -> Quantidade
quantidadeDoseEst _ _ _ [] = 0 
quantidadeDoseEst (x:xs) dose estado (y:ys) 
                                            | filtrarEstado y == estado && checaNumDose (filtrarCPF y) bancoVacinados == dose = 1 + quantidadeDoseMun (x:xs) dose estado ys
                                            | otherwise = quantidadeDoseMun (x:xs) dose estado ys



gerarlistaEstadoFaixas :: CadastroSUS -> Estado -> [FaixaIdade] -> Data -> [(FaixaIdade, Quantidade)]
gerarlistaEstadoFaixas [] _ _ _ = [] 
gerarlistaEstadoFaixas _ _ [] _ = [] 
gerarlistaEstadoFaixas (x:xs) estado (v:vs) dat 
                                                        | filtrarEstado x == estado = (v, cidadaosPorEstadoIdade (x:xs) estado v  dat) : gerarlistaEstadoFaixas (x:xs) estado vs dat 
                                                        | otherwise = gerarlistaEstadoFaixas xs estado (v:vs) dat  


cidadaosPorEstado :: CadastroSUS -> Estado -> Quantidade 
cidadaosPorEstado [] estado = 0 
cidadaosPorEstado (x:xs) estado 
                                      | filtrarEstado x == estado = 1 + cidadaosPorEstado xs estado 
									  | otherwise = cidadaosPorEstado  xs estado 


cidadaosPorEstadoIdade :: CadastroSUS -> Estado-> FaixaIdade -> Data -> Quantidade 
cidadaosPorEstadoIdade [] _ _ _ = 0 
cidadaosPorEstadoIdade (x:xs) estado (idInicial, idFinal) dat 
                                                          | filtrarEstado x == estado && idade (filtrarNascimento x) dat >= idInicial && idade (filtrarNascimento x) dat <= idFinal = 1 + cidadaosPorEstadoIdade xs estado (idInicial, idFinal) dat 
														  | otherwise = cidadaosPorEstadoIdade xs estado (idInicial, idFinal) dat 