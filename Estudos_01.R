🔎 1. Buscas Bibliográficas e Nuvem de Palavras (SciELO/CAPES)



if(!require("pacman")) install.packages("pacman")
library(pacman)

pacman:: p_load(stringr,  #manipulação de strings
                dplyr,    #manipulação de dados
                writexl,  #exportar para o excel
                ggplot2,  #Visualização de dados
                easyScieloPack, #Busca de artigos no SciELO, ele automatiza, ou seja é um webscrapper
                ggtext,    #Textos ricos em gráficos
                ggpubr,   #Publicações de gráficos bonitos
                ggrepel, #Evita sobreposição de labels
                arrow,   #Leitura e escrita ed dados em formato Parquet/feather
                purrr,#Programação funcional (map, walk, etc.)
                capesR,
                usethis#ajuda a recuperar as teses de dissertação da capes
)

##Scielo é um site para achar artigos cientificos, para achar dados sobre questões de aprendizado

## exemplo ##
#aplico a base scielo a função de procura dentro dessa base a questão mostra o que é para que ele ache todo material que tiver Supremo Tribunal Federal
base_scielo<-  search_scielo(
  "Supremo Tribunal Federal",
  n_max = 1000
)
#Vamos buscar pelo termo de interesse 
#mas limitando à busca para 2000 ele demora mais, essa busca vai demorar menos ou mais devido a quantidade de buscar procuradas

#Visualizar o objeto
View(base_scielo)
#lista as variáveis da bases de dados
names(base_scielo)

#Padronizar título para minusculas e verificar se contém termos sobre STF 
base_scielo <- base_scielo |>  
  mutate(
    titulo_min = tolower(title),  # útil que eu posso fazer buscas sem ter o caso sensitivo ou seja aquilo de procurar dados maiusculos minusculos ou maiusculos,
    tema = stringr:: str_detect(
      titulo_min,
      #PT + EN + ES + sigla + variações
      "supremo tribunal federal|\\bstf\\b|supremo tribunal brasileiro|corte suprema brasileira| corte constitucional brasileira|surpreme federal court"
    )
  )

view(base_scielo)

#depois de tudo fazer o filtro 
#esse data.frame vai ser feita com todos aquelas o qual o tema tem ave com o Supremo Tribunal Federal
df_tematica_scielo <-  base_scielo |>  
  filter(tema == "TRUE") |>  
  arrange(year) |>  
  dplyr:: select(-tema)

#salvar os resultados filtrados em um novo arquivo
write.xlsx(df_tematica_scielo, "datafobia1.xlsx")

## Vamos repetir o processo com os dados de CAPES

capes <-  download_capes_data(c(1987:2022))
#base consolidada
df_capes <-  map_dfr(
  capes,
  read_parquet
)

#repetir o título em letra minuscula para evitar falso positivo e falso negativo

df_capes$titulo_min <- tolower(df_capes$titulo)  #colocar tudo em minusculo e evita falso positivo e falso negativo 

df_capes$tema <- stringr:: #quando ele vier ver o tema lá na base df_capes na coluna tema, ele já vai direto procurar a string que contiver essses nomes e todos em minusculo devido a função tolower da função anterior
  str_detect(
  df_capes$titulo_min, "supremo tribunal federal|\\bstf\\b|supremo tribunal brasileiro|corte suprema brasileira| corte constitucional brasileira|surpreme federal court"
)

df_tematica_capes <-  df_capes |>  
  filter(tema == "TRUE") |>  
  arrange(ano_base) |>  
  dplyr:: select(-tema) #verifica se tem ou não esses termos referidos


write_xlsx(df_tematica_capes, "stf_capes_xlsx")

#agroa se tem tantos os stf que tem em capes quanto os que tem em scielo 


##Gráficos 

df_agg_ano_capes <-  df_tematica_capes |> 
  group(ano_base)  |>  #agrupar com essa função 
  summarise(n_casos())  #vai listar a quantidade de casos 

df_agg_ano_scielo <-  df_tematica_scielo |> 
  group(year)  |>  #agrupar com essa função 
  summarise(n_casos())  #vai listar a quantidade de casos 

sum(df_agg_ano_capes$casos)
sum(df_agg_ano_scielo$casos)

library(ggtext)  #faz gráficos super bonitos 

??ggtext

##Extra : NUVEM DE PALAVRAS




#PACOTES
if(!require("pacman")) install.packages("pacman")
library(pacman)

pacman:: p_load(tidyverse, 
                wordcloud2,
                tm   
)



#Dados referentes ao scielo e ao capes


scielo_corpus <-  VCorpus(VectorSource((df_tematica_scielo)))  # Vai transformar em corpus a partir do título do trabalho

#Limpeza da df

scielo_corpus <-  scielo_corpus |> 
  #tiro numeros
  tm_map(removeNumbers) |> 
  #tiro pontuações 
  tm_map(removePunctuation) |> 
  tm_map(content_transformer(tolower)) |> 
  tm_map(
    #removepalvras
    removeWords,c(stopwords("english"),stopwords("spanish"),stopwords("portuguese") 
                  )
    
  )

#transformação para um matriz
tdm <- TermDocumentMatrix(scielo_corpus) |>  as.matrix() #transforma em matrix 
#identificar as palavras
palavras <-  sort(rowSums(tdm),decreasing = T) #Ordena a frequencia que vai aparecer 
#Juntar a partir da frequência das palavras e quantas vezes se repetiu 
df <- data.frame(palavra = names(palavras), freq = palavras) #Coloca as palavras dentro da matrix

#Juntar a partir da frequência das palavras e quantas vezes se repetiu 

df$peso <-  df$freq/sum(df$freq)*100 # Peso relativo de cada palavra utilizada 
df$peso <-  round(df$peso, 2) #para arredondar para dois digitos essa multiplicação predecessora 
wordcloud2(df, 
           size= 0.3,shape= "star",
           rotateRatio = .5,
           ellipticity = .10,
           color = "pink") # jeito que vai ser feito a nuvem de palavras

