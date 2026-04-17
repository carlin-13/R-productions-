

#carregar 
if(!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(readxl,janitor #fazer limpeza de dados
               ,dpylr,stringr, writexl)

#Dados 
gini_1985 <- read_excel("Dados brutos/renomados/gini_1985.xlsx")
gini_95_96 <- read_excel("Dados brutos/renomados/gini_95_96.xlsx")
gini_2006 <- read_excel("Dados brutos/renomados/gini_2006.xlsx")
gini_2017 <- read_excel("Dados brutos/renomados/gini_2017.xlsx")

#Padronização das bases
names(gini_1985)


#Empilhar todas
gini_long <- bind_rows(
  gini_1985 |>  (ano =1985),
  #colocar as outras e ela empilha todas elas uma embaixo da outra 
)
#Base_Brasil (tirar ele de forma sozinha)
gin_brasil <-  gini_long |>  
  filter(Nome_uf == "Brasil")

gini_brasil$gini <-  round(gini_brasil$gini,3) #só vai arredondar 


#Base_regioes
gini_regioes <- gini_long |>  
   filter(str_detect(Nome_uf,"região"))


#Base_estado 
gini_estados <- gini_long |>  
  filter(cod_mun = "0000",
         nome_uf !="Brasil",         #não quero o brasil
         !str_detect(nome_uf,"região"))#não quero a região 


gini_regioes$gini <-  round(gini_regioes$gini,3) 
         
gini_estados$gini <-  round(gini_estados$gini,3) 

#Base municipios 

gini_estados <- gini_long |>  
  filter(code_mun != "00000") 

gini_municipios$gini <-  round(gini_municipios$gini,3) 

#salvar em excel
write_xlsx(gini_estados,"dados.xlsx")
#escreve de cada um


#fazendo um mapa agora
#mostrando a variação do gini na terra 

if(!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(readxl,janitor #fazer limpeza de dados
               ,dplyr,stringr, sf #para trabalhar com mapas
               , geobr #importar os shapes(os mapas)
               ,ggplot2
               )

#carregar a base

load("dados") #objeto gini_estados ou outro 

#carregar shapefile das ufs (geobr)

#vou acrescentar na minha planilha o shape file de cada lugar 
ufs <-  read_state(year = 2020) |> 
  clean_names() |> 
  select(code_state,name_state,geom)

#limpar e preparar os dados

gini_estados <-  gini_estados |>  
  clean_names() |> 
  mutate(cod_uf = as.numeric(cod_uf))

#filtrar anos de interesse


gini_1985 <- gini_estados |>  
  filter(ano==1985) |> 
  select(cod_uf,gini_1985=gini)

#fazer com os outos iguais


#calcular variação absoluta do GINI

gini_variaçao <- 
  gini_2017 |>  
  left_join(gini_1985, by ="cod_uf") |> 
  mutate(variaçao = gini_2017 - gini_1985,
         situação_desigualdade = case_when (
           variacao > 0 ~"aumentou",
           variacao = 0 ~"sem variacao",
           variacao < 0 ~"diminuiu",
           TRUE ~NA_character_
         ))


#mapa com gradiente de cor 
ggplot(gini_mapa) +
  geom_sf(aes(fill= variacao),color=NA #se eu colocar esse ele aparece o contorno das fronteiras 
          ) +
  scale_fill_gradient2(nome = 
                         "",
                       low= "blue",mid ="green",high = "red",midpoint = 0, na.value = "grey") +
  labs(title = "",subtitle = "", caption = "") + theme_void( base_size = 16)

#dá pra fazer com todos os dfs

