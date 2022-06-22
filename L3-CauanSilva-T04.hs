{- matricula : 202100073326 
  nome completo : Cauan Santos Silva -} 


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


-- a) Atualizar endereço 


atualizaEndSUS :: CPF -> CadastroSUS -> Endereco -> CadastroSUS 
atualizaEndSUS cpf xs endNovo = map (atualiza cpf endNovo) xs


atualiza :: CPF -> Endereco -> Cidadao -> Cidadao
atualiza cpff endNovo (cpf, nome, genero, dataNasc, endereco, municipio, estado , telefone, email)
                                                                                                   | cpf == cpff = (cpf, nome, genero, dataNasc, endNovo, municipio, estado , telefone, email)
                                                                                                   | otherwise = (cpf, nome, genero, dataNasc, endereco, municipio, estado , telefone, email)



-- b) Remover o cidadao da lista.  


removeSUS :: CPF -> CadastroSUS -> CadastroSUS
removeSUS cpf xs = filter (checaCPF cpf) xs 


checaCPF :: CPF -> Cidadao -> Bool 
checaCPF cpff (cpf, nome, genero, dataNasc, endereco, municipio, estado , telefone, email)  
                                                                                           | cpff /= cpf = True 
																						   | cpff ==  cpf = False 


-- c) geraListaMunicipioFaixas 

type IdadeInicial = Int
type IdadeFinal = Int
type FaixaIdade = (IdadeInicial, IdadeFinal)
type Quantidade = Int


filtrarMunicipio :: Cidadao -> Municipio
filtrarMunicipio (_, _, _, _, _, municipio, _, _, _) = municipio 


idade :: DataNasc -> Data ->  Int 
idade (dia, mes, ano) (da, ma, aa) 
                      | mes == ma && dia <= da = aa - ano   
                      | mes == ma && dia >= da = aa - ano 
                      | mes > ma =  aa  - ano 
                      | mes < ma =  aa  - ano  


gerarlistaMunicipiosFaixas :: CadastroSUS -> Municipio -> [FaixaIdade] -> Data -> [(FaixaIdade, Quantidade)] 
gerarlistaMunicipiosFaixas xs municipio zs dat  =  map auxiliar zs
    where auxiliar y= (y, cidadaosPorMunicipioIdade xs municipio y  dat) 



cidadaosPorMunicipioIdade :: CadastroSUS -> Municipio -> FaixaIdade -> Data -> Quantidade  
cidadaosPorMunicipioIdade xs municipioo (idInicial, idFinal) dat  =  length  (filter filtrarInd  xs )
                              where 
							        filtrarInd (cpf, nome, genero, dataNasc, endereco, municipio, estado , telefone, email) = municipio == municipioo &&  idade dataNasc dat >= idInicial && idade dataNasc dat <= idFinal
                               
																		
-- d) quantidadeDoseMun 

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
 
 
 
quantidadeDoseMun :: Vacinados -> TipoDose -> Municipio -> CadastroSUS -> Quantidade
quantidadeDoseMun xs dose municipioo ys = length ((filter auxiliar2) ys) 
                  where auxiliar2 (cpf, nome, genero, dataNasc, endereco, municipio, estado , telefone, email)  = municipio == municipioo && checaNumDose cpf xs == dose
				         

checaNumDose :: CPF -> Vacinados -> Int 
checaNumDose cpff xs = (sum (map d (filter cpf xs))) 
                 where d (cpf,doses) =  length doses
                       cpf x = cpff == fst x
				


-- e) quantidadeEstIdDose 





