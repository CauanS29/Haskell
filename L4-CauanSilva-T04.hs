


import Data.Char 


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


	

{-formatarTudo :: CPF -> IO () 
formatarTudo 0 = putStrLn " Pesquis Finzalizada " 
formatarTudo cpf = -}




formatarDoses :: CPF -> Vacinados -> String
formatarDoses cpf vacinados =  toUpperString $ concat $ map f xs
    where f x = formatarDose x ++ "\n" ++ replicate 7 ' ' 
          xs = getDoses cpf vacinados 

 
 
formatarDose :: Dose -> String
formatarDose (vacina,date) = vacina ++ ", " ++ dat date
    where 
     dat (d,m,a)= show d ++ "." ++ show m ++ "." ++ show a




getNome :: CPF -> CadastroSUS -> Nome
getNome _ [] = errorWithoutStackTrace "Cidadão não cadastrado no bando do SUS."
getNome cpf (cid:banco) = if cpff == cpf then filtrarNome cid else getNome cpf banco 
        where
                filtrarNome :: Cidadao -> Nome
                filtrarNome (_,nome, _, _, _, _, _, _, _) = nome 
                (cpff,_, _, _, _, _, _, _, _) = cid 
				
				
			
getDoses :: CPF -> Vacinados -> Doses 
getDoses _ [] = errorWithoutStackTrace " Cidadão não está cadastrado no banco de vacinados. " 
getDoses cpf vacinados = head (map snd ( filter f vacinados ) )
                    where   
                       f x = fst x == cpf 
					   
			
toUpperString :: String -> String
toUpperString = map toUpper

