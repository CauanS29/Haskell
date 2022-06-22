


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

-- a) Cadastramento de um cidadão no sistema. 

filtrarCPF :: Cidadao -> CPF 
filtrarCPF (cpf, _, _, _, _, _, _, _, _) = cpf 


checaCPF :: CPF -> CadastroSUS -> Bool 
checaCPF cpf banco = or [cpf == filtrarCPF cidadao |cidadao <- banco ] 

adicionaSUS :: Cidadao -> CadastroSUS -> CadastroSUS 
adicionaSUS cidadao banco 
               
              | checaCPF (filtrarCPF cidadao) banco = error "ja tem um cpf cadastrado" 
              | otherwise = cidadao : banco  

-- b) Atualizar telefone e endereco do usuario 

posicionarCidadao :: CadastroSUS -> [( Int, Cidadao)] 
posicionarCidadao banco = zip posicoes banco  
                   where 
                          posicoes = [ 1 ..( length banco) ] 


encontrarPosCidadao :: CPF -> CadastroSUS -> Int 
encontrarPosCidadao cpf banco 
            
                | posicao == [] = 0 
                | otherwise = head posicao

  where 
                      posicao = [ id | (id, cidadao) <- posicionarCidadao banco, filtrarCPF cidadao == cpf ] 


atualizaEndSUS :: CPF -> CadastroSUS -> Endereco -> CadastroSUS 
atualizaEndSUS cpf banco enderecoNovo  

          | checaCPF cpf banco = parte1 ++ novoDado ++ parte2 
          | otherwise = error  "o cidadao nao possui cpf cadastrado"

      where 
              posicao = encontrarPosCidadao cpf banco 
              novoDado = [(cpfBanco, nome, genero, dataNasc, enderecoNovo, municipio, estado, telefone, email) | (cpfBanco, nome, genero, dataNasc, endereco, municipio, estado, telefone, email) <- banco, cpf == cpfBanco ] 
              parte1 = take (posicao - 1) banco
              parte2 = drop posicao banco 
			  

atualizaNumSUS :: CPF -> CadastroSUS -> Endereco -> CadastroSUS 
atualizaNumSUS cpf banco telefoneNovo    

          | checaCPF cpf banco = parte1 ++ novoDado ++ parte2 
   
      where 
              posicao = encontrarPosCidadao cpf banco 
              novoDado = [(cpfBanco, nome, genero, dataNasc, endereco, municipio, estado, telefoneNovo, email) | (cpfBanco, nome, genero, dataNasc, endereco, municipio, estado, telefone, email) <- banco, cpf == cpfBanco ] 
              parte1 = take (posicao - 1) banco
              parte2 = drop posicao banco 
			  
			  

-- c) Remover o cidadao da lista.  

removeSUS :: CPF -> CadastroSUS -> CadastroSUS 
removeSUS cpf banco = [(cpfBanco, nome, genero, dataNasc, endereco, municipio, estado, telefone, email) | (cpfBanco, nome, genero, dataNasc, endereco, municipio, estado, telefone, email) <- banco, cpfBanco /= cpf ]



-- d) Funcoes de consulta 

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


cidadaosPorMunicipio :: CadastroSUS -> Municipio -> Quantidade 
cidadaosPorMunicipio banco municipio = length [ x | x <- banco, filtrarMunicipio x == municipio ] 


cidadaosPorEstado :: CadastroSUS -> Estado -> Quantidade
cidadaosPorEstado banco estado = length [ x | x <- banco, filtrarEstado x == estado ] 


cidadaosPorMunicipioIdade :: CadastroSUS -> Municipio-> FaixaIdade -> Data -> Quantidade
cidadaosPorMunicipioIdade banco municipio (inicial, final) dat  = length [ x | x <- banco, filtrarMunicipio x == municipio, inicial <= idade (filtrarNascimento x) dat  , final >= idade (filtrarNascimento x) dat] 

cidadaosPorEstadoIdade :: CadastroSUS -> Estado -> FaixaIdade -> Data -> Quantidade
cidadaosPorEstadoIdade banco estado (inicial, final) dat = length [ x | x <- banco, filtrarEstado x == estado, inicial <= idade  (filtrarNascimento x) dat, final >= idade  (filtrarNascimento x) dat ] 


-- e) Pode ser interessante também gerar uma lista da quantidade de cidadãos por faixasde idade para um dado município ou estado. 


listaMunicipioFaixas :: CadastroSUS -> Municipio -> [FaixaIdade] -> Data -> IO() 
listaMunicipioFaixas banco municipio faixaIdade dat = putStr ("Municipio : " ++ show municipio ++ "\nFAIXA DE IDADES               QUANTIDADES\n" 
                                                   ++ formatarLinhas ( gerarlistaMunicipiosFaixas banco municipio faixaIdade dat ) 
                                                   ++ formatarTotal ( gerarlistaEstadosFaixas banco municipio faixaIdade dat ) ) 


listaEstadoFaixas :: CadastroSUS -> Estado -> [FaixaIdade] -> Data -> IO() 
listaEstadoFaixas banco estado faixaIdade dat = putStr ("Estado : " ++ show estado ++ "\n FAIXA DE IDADES               QUANTIDADES\n" 
                                                      ++ formatarLinhas ( gerarlistaEstadosFaixas banco estado faixaIdade dat ) 
                                                      ++ formatarTotal  ( gerarlistaEstadosFaixas banco estado faixaIdade dat ) ) 


gerarlistaMunicipiosFaixas :: CadastroSUS -> Municipio -> [FaixaIdade] -> Data -> [(FaixaIdade, Quantidade)] 
gerarlistaMunicipiosFaixas banco municipio faixaIdade dat  = [((idadeI, idadeF), cidadaosPorMunicipioIdade banco municipio (idadeI, idadeF) dat )| (idadeI, idadeF) <- faixaIdade ] 


gerarlistaEstadosFaixas :: CadastroSUS -> Estado -> [FaixaIdade] -> Data ->  [(FaixaIdade, Quantidade)] 
gerarlistaEstadosFaixas banco estado faixaIdade dat  = [((idadeI, idadeF), cidadaosPorEstadoIdade banco estado (idadeI, idadeF) dat) | (idadeI, idadeF) <- faixaIdade ] 


-- f) A lista do município ou estado deve obedecer à formatação, descrita a seguir. O cabeçalho segue o formato abaixo, onde nome é o nome do município ou estado informado na função. 


type QuantidadeFormatada = String 

type LinhaFormatada = String 

type LinhasFormatadas = String 

type TotalFormatado = String 
 

formataQuant :: Quantidade -> QuantidadeFormatada 
formataQuant qtd = "                                 " ++ show qtd 


formataUmaLinha :: (FaixaIdade, Quantidade)-> LinhaFormatada 
formataUmaLinha ((idInicial, idFinal), qtd ) = show idInicial ++ " - " ++  show idFinal ++ formataQuant qtd  

formatarLinhas :: [(FaixaIdade, Quantidade)] -> LinhasFormatadas 
formatarLinhas faixa_qtd = concat [formataUmaLinha item ++ "\n" | item <-faixa_qtd] 

formatarTotal :: [(FaixaIdade,Quantidade)] -> TotalFormatado 
formatarTotal lista = "Total                   " ++ show (sum [ quantidade | (_, quantidade) <- lista ] ) 


-- g) Aplicação da primeira dose. 


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
 

aplicaPrimDose:: CPF -> CadastroSUS -> FaixaIdade -> Municipio -> Vacina -> Data -> Vacinados -> Vacinados 
aplicaPrimDose cpf cad (idadeI, idadeF) municipio vacina dat vacinados 
                                                                       | tomouPrimeiraDose cpf vacinados = error "Primeira dose ja aplicada" 
                                                                       | checaCPF cpf cad == False = error "usuario nao cadastrado no sistema" 
																	   | checaIdade cpf cad (idadeI, idadeF) dat == False = error "Esse cidadao nao esta dentro da faixa" 
																	   | checaMuni cpf cad  municipio == False = error "Municipio incompativel, atualize os dados SUS" 
																	   | vacina == "Janssen" = (cpf, (replicate 2 (vacina, dat))) : bancoVacinados 
																	   | otherwise = (cpf, [(vacina, dat)]) : bancoVacinados 


tomouPrimeiraDose :: CPF -> Vacinados -> Bool 
tomouPrimeiraDose cpf banco = or [ x == cpf | (x,_) <- banco ] 

checaIdade :: CPF -> CadastroSUS -> FaixaIdade -> Data -> Bool 
checaIdade cpf cad (idadeI, idadeF) dat  =  [ x | x <- cad, filtrarCPF x == cpf, idadeI <= idade dat (filtrarNascimento x ), idadeF >= idade dat (filtrarNascimento x )] /= [] 

checaMuni :: CPF -> CadastroSUS -> Municipio -> Bool 
checaMuni cpf cad municipio =  [ x | x <- cad, filtrarMunicipio x == municipio, filtrarCPF x == cpf ]  /= []



-- h) Aplicação Segunda Dose 

aplicaSegDose:: CPF -> Data -> Vacinados -> Vacinados 
aplicaSegDose cpf dat2 banco 
                            | checaVacinado cpf banco == False = error "Cidadao nao vacinado"
							| checaSegDose cpf banco == 2 = error "Cidadao ja tomou a segunda dose" 
							| checaData cpf dat2 banco  == False = error "Data incorreta" 
							| otherwise = parte1 ++ novoVacina ++ parte2
		 where 
              posicao = encontrarPosVacinado cpf banco 
              novoVacina = [( cpfB, [(vacina, dat1), (vacina, dat2)]) | (cpfB, [(vacina, dat1)]) <- banco, cpf == cpfB ] 
              parte1 = take (posicao - 1) banco
              parte2 = drop posicao banco	
							

checaVacinado :: CPF -> Vacinados -> Bool 
checaVacinado cpf banco = or [ x == cpf | (x,_) <- banco ] 

checaSegDose :: CPF -> Vacinados -> Int 
checaSegDose cpf banco = head [ length x | (cpfB, x) <- banco, cpf == cpfB ] 

checaData :: CPF -> Data -> Vacinados -> Bool 
checaData cpf (da, ma, aa) banco 
                                  | aa > head [ ano | (cpfB, [(_, (_, _, ano))]) <- banco, cpf == cpfB ] = True 
                                  | aa == head [ ano | (cpfB, [(_, (_, _, ano))]) <- banco, cpf == cpfB ] &&  ma > head [ mes | (cpfB, [(_, (_, mes, _))]) <- banco, cpf == cpfB ] = True 
								  | aa == head [ ano | (cpfB, [(_, (_, _, ano))]) <- banco, cpf == cpfB ] &&  ma == head [ mes | (cpfB, [(_, (_, mes, _))]) <- banco, cpf == cpfB ] && da > head [ dia | (cpfB, [(_, (dia, _, _))]) <- banco, cpf == cpfB] = True 
								  | otherwise = False  
								  
								  
posicionarVacinados :: Vacinados  -> [( Int, Vacinado)] 
posicionarVacinados banco = zip posicoes banco  
                   where 
                          posicoes = [ 1 ..( length banco) ] 


encontrarPosVacinado :: CPF -> Vacinados -> Int  
encontrarPosVacinado cpf banco 
            
                | posicao == [] = error "CPF nao possui vacina"  
                | otherwise = head posicao 
             where 
                 posicao = [ x |(x, (cpfB, [dose])) <- posicionarVacinados banco, cpf == cpfB ]  
					  
					  
					  


-- i) atualização no cadastro de vacinados 



atualizaVacina:: CPF -> TipoDose -> Vacina -> Vacinados -> Vacinados
atualizaVacina cpf tipo vacina banco
                                     | (checaVacinado cpf banco) == False = error "CPF nao encontrado no banco"
                                     | checaNumDose cpf banco < tipo = error "dose ainda não foi ministrada" 
                                     | checaNumDose cpf banco == 2 && tipo == 2 = inicio ++ novaVac2 ++ final
                                     | checaNumDose cpf banco == 2 && tipo == 1 = inicio ++ novaVac1 ++ final
                                     | checaNumDose cpf banco == 1 && tipo == 1 = inicio ++ vacUnica ++ final
                                     where 
                                            inicio = take (posiçao-1) banco
                                            vacUnica = [(cpfB, [(vacina, data1)]) | (cpfB, [(vacina1, data1)]) <- banco, cpfB == cpf] 
                                            novaVac1 = [(cpfB, [(vacina, data1),(vacina2, data2)]) | (cpfB, [(vacina1, data1), (vacina2, data2)]) <- banco, cpfB == cpf]
                                            novaVac2 = [(cpfB, [(vacina1, data1),(vacina, data2)]) | (cpfB, [(vacina1, data1), (vacina2, data2 )]) <- banco, cpfB == cpf]
                                            final = drop posiçao banco
                                            posiçao = encontrarPosVacinado cpf banco 


checaNumDose :: CPF -> Vacinados -> Int 
checaNumDose cpf banco = head [ length x | (cpfB, x) <- banco, cpf == cpfB ]  



-- j) Quantidade de pessoas no município/estado vacinadas com uma dada dose. 

quantidadeDoseMun :: Vacinados -> TipoDose -> Municipio -> CadastroSUS -> Quantidade
quantidadeDoseMun banco tipo municipio cad =  length [x | x <- cad, checaVacinado (filtrarCPF x) banco, checaNumDose (filtrarCPF x) banco == tipo, filtrarMunicipio x == municipio ] 


quantidadeDoseEst :: Vacinados -> TipoDose -> Estado -> CadastroSUS -> Quantidade
quantidadeDoseEst banco tipo estado cad =  length [x | x <- cad, checaVacinado (filtrarCPF x) banco, checaNumDose (filtrarCPF x) banco == tipo, filtrarEstado x == estado ] 


-- K) Quantidade de pessoas no município/estado vacinadas por faixa etária e por dose. 

quantidadeMunIdDose :: Vacinados -> Municipio -> FaixaIdade -> Data ->  TipoDose -> CadastroSUS -> Quantidade
quantidadeMunIdDose banco municipio (idadeI, idadeF) dat tipo cad = length [x | x <- cad, checaVacinado (filtrarCPF x) banco, checaNumDose (filtrarCPF x) banco == tipo, filtrarMunicipio x == municipio, idadeI <= idade  (filtrarNascimento x )dat , idadeF >= idade  (filtrarNascimento x ) dat ] 

quantidadeEstIdDose :: Vacinados -> Estado -> FaixaIdade -> Data  -> TipoDose -> CadastroSUS -> Quantidade
quantidadeEstIdDose banco estado (idadeI, idadeF) dat tipo cad = length [x | x <- cad, checaVacinado (filtrarCPF x) banco, checaNumDose (filtrarCPF x) banco == tipo, filtrarEstado x == estado, idade (filtrarNascimento x ) dat >= idadeI, idade  (filtrarNascimento x ) dat <= idadeF] 


-- l) Quantidade de pessoas no município/estado vacinadas por tipo de vacina e por dose. 

quantidadeMunVacDose :: Vacinados -> Municipio -> Vacina -> TipoDose -> CadastroSUS -> Quantidade 
quantidadeMunVacDose banco municipio vacina tipo cad = length [ x | x <- cad, checaVacinado (filtrarCPF x) banco, filtrarMunicipio x == municipio, filtrarVacina (filtrarCPF x) banco == vacina, checaNumDose (filtrarCPF x) banco  == tipo ] 


quantidadeEstVacDose :: Vacinados -> Estado -> Vacina -> TipoDose -> CadastroSUS -> Quantidade 
quantidadeEstVacDose banco estado vacina tipo cad = length [ x | x <- cad, checaVacinado (filtrarCPF x) banco, filtrarEstado x == estado, filtrarVacina (filtrarCPF x) banco == vacina, checaNumDose (filtrarCPF x) banco  == tipo ] 


filtrarVacina :: CPF -> Vacinados -> Vacina 
filtrarVacina cpf banco 
                       | checaNumDose cpf banco == 2 = head [ vacina2 | (cpfB, [( vacina1, data1 ), ( vacina2, data2 )]) <- banco, cpf == cpfB ] 
					   | checaNumDose cpf banco == 1 = head [ vacina1 | (cpfB, [( vacina1, data1 )]) <- banco, cpf == cpfB ]  
					   


-- M) Quantidade de pessoas atrasadas na segunda dose no município/estado, dentre os cidadãos que pertencem ao cadastro de vacinados. 



{-converterDias :: Data -> Int 
converterDias (dia1, mes1, ano1)
                                  |dia1 == 30 = 30 * mes1
                                  |dia1 < 30 = dia1 + (30 * mes1)
                                  |dia1 > 30 = 1 + (30 * mes1)  -} 


-- n) consulta e-mail no cadastroSUS e consulta genero e vacina e numero de dose no banco de vacinas  


verificaGmail :: CadastroSUS -> Quantidade 
verificaGmail cad = length [ email | (_, _, _, _, _, _, _, _, email) <- cad, filtrarGmail email == "gmail.com" ]  

filtrarGmail :: String -> String 
filtrarGmail email = drop ( length email - 9) email 

verificaGenVac ::  Vacinados -> Genero -> Vacina -> TipoDose -> CadastroSUS ->  Quantidade 
verificaGenVac banco genero vacina dose cad = length [cidadao | cidadao <- cad, checaNumDose (filtrarCPF cidadao)  banco >= dose, filtrarGen cidadao == genero, (cpf, _)<- banco, cpf == filtrarCPF cidadao, filtrarVacina (filtrarCPF cidadao)   banco == vacina   ] 


filtrarGen :: Cidadao -> Genero 
filtrarGen (_, _, genero, _, _, _, _, _, _) = genero  



-- O) relatorio de pessoas de determinados generos por vacina e dose 


listaGenVac :: Vacinados -> CadastroSUS -> Genero -> [Vacina] -> TipoDose ->  IO() 
listaGenVac banco cad genero vacinas dose = putStr ("Genero : " ++ show genero ++ "\nVACINAS               QUANTIDADES\n" 
                                                   ++ formatarLinhasVac ( gerarlistaGenVac banco genero vacinas dose cad  ) 
                                                   ++ formatarTotalVac ( gerarlistaGenVac banco genero vacinas dose cad  ) ) 


gerarlistaGenVac :: Vacinados -> Genero -> [Vacina] -> TipoDose -> CadastroSUS ->  [(Vacina, Quantidade)] 
gerarlistaGenVac banco genero vacinas dose cad  = [(vacina, verificaGenVac banco genero vacina dose cad ) | vacina <- vacinas  ]  


formataQuantVac :: Quantidade -> QuantidadeFormatada 
formataQuantVac qtd = "                                 " ++ show qtd 



formataUmaLinhaVac :: (Vacina, Quantidade)-> LinhaFormatada 
formataUmaLinhaVac (vacina, quantidade)  
                                          | vacina == "Pfizer" = show vacina ++ "                      " ++ show quantidade ++ "\n"
                                          | vacina == "AstraZeneca" = show vacina ++ "                 " ++ show quantidade ++ "\n"
                                          | vacina == "Janssen" = show vacina ++ "                     " ++ show quantidade ++ "\n"
                                          | vacina == "CoronaVac" = show vacina ++ "                   " ++ show quantidade ++ "\n" 
    

formatarLinhasVac :: [(Vacina, Quantidade)] -> LinhasFormatadas 
formatarLinhasVac vacina_Quantidades = concat [formataUmaLinhaVac vacina_Quantidade | vacina_Quantidade <- vacina_Quantidades ] 


formatarTotalVac :: [(vacina,Quantidade)] -> TotalFormatado 
formatarTotalVac lista = "Total                   " ++ show (sum [ quantidade | (_, quantidade) <- lista ] )  







