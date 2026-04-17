
#Utilizar uma base dos filmes do MUBI e com isso explorar seu conteúdo e usar o Data Science para escolher filmes que são bons


#utilizo o load para carregar uma base que já está no meu projeto de R 
load("base de dados que já está disponível no meu projeto de R")

nrows("base") #consigo ver a qtde de linahs que tenho na base 
ncol("base") #consigo ver a qtde de colunas que tem na minha base 
names("base") #consiog ver os nomes nos cabeçalhos 




if(!require("pacman")) install.packages("pacman")
library(pacman)

pacman:: p_load(
  dplyr ,# Manipulação de Dados
  ggplot2, #produção de gráficos
  forcats #ordenar categorias(alvo da análise)
)


#Criação de Gráfiicos 
#função geom_text é a função para colocar os valores dentro do gráfico

"Planilha" |>  
  filter() |>
  slice() #faz um recorte dos dados que voce que r
|> 
  mutate(fct_reorder()) |>
  ggplot() + 
  geom_col() +
  geom_text() + coord_flip() + 
  theme_bw() + 
  labs()

#Agregações de Dados



movies_agg <- base |>  
  group_by() |> 
  summarise(n_casos=n()) |>  
  arrange(-n_casos)

head(movies_agg)

