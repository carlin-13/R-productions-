# R-productions-
Aqui dentro contém minhas produções utilizando Rstudio 


## 📚 Laboratório de R: Projetos & Estudos (Datafobia)

Abaixo estão os scripts com base nas aulas do canal Datafobia, os quais apresentam desde testes de hipóteses até análises exploratórias usando Lei de Benford.
<details>
<summary><b> 📜 Estudos </b></summary>

<details style="margin-left: 20px;">
<summary>🔎 1. Buscas Bibliográficas e Nuvem de Palavras (SciELO/CAPES)</summary>

```r


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


```

</details>

<details>
<summary>🎬 2. Análise Exploratória: Como escolher bons filmes? (MUBI)</summary>

```r

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

```

</details>

<details>
<summary>🕷️ 3. Web Scrapping: Raspagem de Dados de Pacotes do CRAN</summary>

```r

#Raspagem de dados em Dados estáticos 


if(!require("pacman")) install.packages("pacman")
library(pacman)

pacman:: p_load(
  rvest, #pacote utilizado para a raspagem de dados 
  lubridate #manipulação de datas ,
  , dplyr, 
  stringr,
  ggrepel #colocar rótulos de forma mais elegante
  , ggplot2
  )


#url de interesse que eu quero puxar as informações

url <-  "https://cran.r-project.org/web/packages/available_packages_by_date.html"

#ler a url que vamos mandar par o R ler o contéudo do link

page <-  read_html(url)

#extração do conteúdo ou seja vamos extrair as informações do link proveniente 

packages_data <- page |>
  #Consigo extrair as informações da página para um objeto 
  html_node("table") |>  
  html_nodes("tr") |>  
  html_nodes("td") |> 
  html_text() |> 
  #transporto tudo para uma matriz
  matrix(ncol = 3, byrow = TRUE)

#conversão da matriz de dados = depois disso, vamos estruturar em um planilha de dados

pacotes_r  <-  as.data.frame(packages_data, stringsAsFactors = FALSE)
#coloco os nomes na minha matriz que tava com os nomes muito paia
colnames(pacotes_r) <-  c("data","nome_pacote","descricao")


#Como tnho variáve data eu posso pegar o ano, ele pega e cira uma coluna só com o ano 

pacotes_r$data <- as.Date(pacotes_r$data,format = "%Y-%m-%d")
pacotes_r$ano <- year(pacotes_r$data)



#Pra finalizar eu posso passar para salvar e salvar no formato de escel
#Sys date já utiliza a data atual

write_xlsx(pacotes_r,paste("df_pacotes_r", format(
  #Sys.Date coloca a data atual e não precisa ficar atualizando 
  Sys.Date(),"%d-%m-%Y")))

#Deixar tudo minusculo de lei, para não decorrer de ocorrer do caso de falso negativo e falso positivo 

pacotes_r$descricao <-  tolower(pacotes_r$descricao)

#Vou agora escolher um tema para com que eu consiga buscar e que falem com  survey 

#ele vai olhar se na frase de descrição existe ou não na coluna descricao a palavra cluster e se tiver TRUE e senão FALSE

#cria a variável tema e com o str_detect ele procura pra ver se tem ou não na coluna a string "cluster"
pacotes_r$tema <-  str_detect(pacotes_r$descricao,"cluster")


df_tematica <-  pacotes_r |>  
  filter( tema == "TRUE") |>  
  arrange(data) |>  
  dplyr::select(-tema)

#se tiver vedadeiro se não tiver é fake


#importa para onde voce quiser
write_xlsx(df_tematica, "C:..")



#outra forma é de fazer o Web Scrapping é 


#a estrutura é a mesma, só muda a questão do que vai ser utilizado para se achar
pacotes_r$time_series <- str_detect(pacotes_r$descricao,"time series")
#se tiver é verdadeiro
df_tematica <- pacotes_r |> 
filter(time_series == "TRUE") |>  
  arrange(data) |> 
  dplyr:: select(-tema,-time_series)  

write_xlsx(df_tematica,"C: ")

  
#a estrutura é a mesma, só muda a questão do que vai ser utilizado para se achar


pacotes_r$regression <- str_detect(pacotes_r$descricao,"regression")

#se tiver é verdadeiro
filter(regression == "TRUE") |>  
  arrange(data) |> 
  dplyr:: select(-tema,-regression)  

write_xlsx(df_tematica,"C: ")




#SCRIPT EXTRA 


#Pode se agregar e fazer um gráfico


df_tematica |>  
  group_by(ano) |> 
  summarise(n_pacotes=n()) |> 
  mutate(ano=as.numeric(ano)) |>   #transforma ano em numérico
  ggplot(aes(ano,n_pacotes)) + 
  geom_line(alpha = .05, 
            size = 3., col = "blue") +
  geom_point(size = 25, alpha = .25, col = "blue") +
  scale_x_continuous(breaks = seq(2008,2025,by=1)) + 
  theme_bw() +
  labs( x="",
        y="",
        title = "Número de pacotes para conteúdo Time Series no CRAN",
        subtitle = "(2011 - 2025)",
        caption = "Fonte: Eu" + 
          geom_text(aes(label = n_pacotes),size = 10) +
          theme(
            legend.title = element_blank(),
            text = element_text(size =30),
            asis.text.x = element_text(angle=45, hjust = 1)
            
          )
  )  
```

</details>

<details>
<summary>👤 4. Identificação de Gênero a partir do Nome (IBGE/genderBR)</summary>

```r

#poderia usar o pacote pacmnan também 
install.packages("genderBR")
library(genderBR)

#crio objetos para cria a base 
nome <-  (c("PEDRO","AUGUSTO","AMANDA","KLEITON","GEOVANA"))
idade <-  (c(18,19,20,30,25))

#faz um data frame desses dois objetos
bse <- as.data.frame(cbind(nome,idade))
#dá uma olhada
View(bse)
#cria uma nova coluna com o genero dos nomes
#como que ele sabe o sexo? esse pacote busca lá na base do IBGE os nomes e ve se é home ou muié
#ele busca lá e por meio da probabilidae ele joga

bse$sexo  <-  get_gender(bse$nome)
#e agora na nossa base ele aparece com o sexo 
View(bse)

#posso ter nome é sobrenome e não muda nada 

nome2 <- c("tauane matias", "caio batista")

nome3 <-  get_gender(nome2)
                        
#por fim ele so ve pela probabilidade e não entra tão a fundo no nome


#se for procurar por nomes mais estranhos ele vai pela probabilidade como por exemplo 
# COM "T" ELE DÁ A PROBABILIDADE E COM "F" ELE DÁ QUAL É O SEXO DE FATO 
  get_gender("João", prob = F)
  
  
```
</details>

<details>
<summary>♈ 5. Manipulação de Datas: Cálculo do Signo do Zodíaco</summary>

```r

  if(!require("pacman")) install.packages("pacman")
  library(pacman)
  
  pacman:: p_load(
    readxl, #para ler arquivos externos
    DescTools ,# a função que faz o calculo do signo tá nela
    dplyr,
    lubridate,
    ggplot2,
    forcats  #ordenar as categorias
    ,rio 
    )
  #fiz desse jeito mais fácil
  base_signos <- read_excel("Teste_signos.xlsx")
  
  
  #faço o uso de alterar a tabela para conseguir calcular tal coisa 
  
  base_signos <-base_signos |>  
    mutate(data_nascimento = make_date(Ano,Mês,Dia))
  
  #agora depois de fazer uma coluna com o dia de nascimento eu consigo a partir da função Zodiac fazer a questão do signo de cada pessoa
  base_signos$signo <- Zodiac(base_signos$data_nascimento)
  
  View(base_signos)

  #sai em inglês porque o pacote é feito de lá 
  #então para a tradução vamos fazer uma recodificação 
  
  base_signos <- base_signos |> 
    mutate(signo = case_when(
      signo == "Virgo" ~ "Virgem",
      signo == "Capricorn" ~"Capricórnio",
      signo == "Leo" ~"Leão",
      signo == "Pisces" ~"Peixe",
      signo == "Aquarius" ~"Aquário",
      signo == "Cancer" ~ "Câncer",
      signo == "Libra" ~"Libra",
      signo == "Scorpio" ~"Escorpião",
      signo == "Taurus" ~"Touro",
      signo == "Sagittarius" ~"Sagitário",
      signo == "Aries" ~"Áries",
      signo == "Gemini" ~"Gemêos"
    ))
  
  #agora vou agregar para fazer uma estatistica avançada 
  
  df_agg <-  base_signos |>  
    group_by(signo) |> 
    summarise(n_casos=n()) #calcula a frequência de pessoas de cada signo
  
  df_agg$perc <-  df_agg$n_casos/sum(df_agg$n_casos)* 100 #calcula a frequencia relativa dos numeros de casos
  df_agg$perc <-  round(df_agg$perc,1 )   #arredondar os numeros para 1 percentuais em 1 casa decimal
  
  #Base pronta é só fazer um gráfico
  df_agg  |> 
  mutate(signo = fct_reorder(signo,n_casos)) |> 
    ggplot(aes(signo,perc)) + 
    geom_col(col = "white",
             fill = "green",
             alpha =.7) + 
    labs(x= "",
         y= ""
         , title = "Signos mais frequentes entre a base de dados",
         subtitle = "é rapaiz são os signos",
         caption = "Carlos Canto (2026)") 
 
```

</details>
<details>
<summary> 📱 6. Geração de QR Code no R</summary>

```r
  #pacote do qr code
install.packages("qrcode")
  library(qrcode)
  #basta colar o link, em que ele gera lá o df
  code <-  qr_code("link de interesse")
  #e o o plot mostra o qr code de forma automática já
  plot(code)
  
  #tem a possibilidade de com a função utilizar a generate pra onde voce quer armazenar esse info
  
  generate_svg(code,filename = "pasta de destino/qr.svg")
```
</details>
<details> 
<summary> 📊 7. Análise de Outliers: O Quarteto de Anscombe</summary>  

```r

#script que vai reproduzir um gráfico de um artigo sobre outlier
  # https://www.sjsu.edu/faculty/gerstman/StatPrimer/anscombel1973.pdf
  #Graphs in Statistical Analysis de F. J. Anscomber 
  
  if(!require("pacman")) install.packages("pacman")
  library(pacman)
  pacman:: p_load(fBasics ,
                  datasets, #daonde vai pegar a base
                  ggpubr, #organiza legendas
                  ggplot2)  
  
  anscombe <-  datasets:: anscombe

  #vamos fazer os gráficos de exemplo dos outliers
  
  
  g1 <- anscombe |> 
    ggplot(aes(x1,y1)) + geom_point(size=5, alpha=.5, col="darkorange") + 
    theme_bw() + geom_smooth(method = "lm",alpha=.1) + 
    stat_cor(method = "pearson",label.x = 3,label.y = 10,size=7) +
    theme(text = element_text(size=20)) +
    labs(subtitle = "EXEMPLO")
  
  plot(g1)

  
  g2 <- anscombe |> 
    ggplot(aes(x2,y2)) + geom_point(size=5, alpha=.5, col="darkorange") + 
    theme_bw() + geom_smooth(method = "lm",alpha=.1) + 
    stat_cor(method = "pearson",label.x = 3,label.y = 10,size=7) +
    theme(text = element_text(size=20)) +
    labs(subtitle = "EXEMPLO")
  
  plot(g2)

  
  g3 <- anscombe |> 
    ggplot(aes(x3,y3)) + geom_point(size=5, alpha=.5, col="darkorange") + 
    theme_bw() + geom_smooth(method = "lm",alpha=.1) + 
    stat_cor(method = "pearson",label.x = 3,label.y = 10,size=7) +
    theme(text = element_text(size=20)) +
    labs(subtitle = "EXEMPLO")
    
  plot(g3)
  
  g4 <- anscombe |> 
    ggplot(aes(x4,y4)) + geom_point(size=5, alpha=.5, col="darkorange") + 
    theme_bw() + geom_smooth(method = "lm",alpha=.1) + 
    stat_cor(method = "pearson",label.x = 3,label.y = 10,size=7) +
    theme(text = element_text(size=20)) +
    labs(subtitle = "EXEMPLO")
  
  plot(g4)

  #nunca faça análise de correlação sem olhar pro gráfico
  
  ggarrange(g1,g2,g3,g4)
  
  #em que eles tem o mesmo R porém eles tem formas de distribuição de correlação totalmente diferentes
  
```

</details>

<details>
<summary>🎸 8. Identificação e Destaque de Outliers em Gráficos (Simulação Iron Maiden)</summary>

```r 
  if(!require("pacman")) install.packages("pacman")
  library(pacman)
  pacman::p_load(
    ggplot2,
    ggrepel, # para conseguir afastar os rótulos 
    dplyr
  )
  
  #essa raiz é sobre simulações em que se consegue ver
  
  #1º passo da simulação é fazer o primeira base de tudo para ter sempre a mesma informação 
  
  set.seed(123)

  #fazer uma variável dependente
  y  <-  rnorm(n=665,
               mean = 200,
               sd=5)
  
  mean(y)
  sd(y)  
  
  
  
  #agora criarei um caso que vai estar 6 desvios padroes abaixo da média
  
  #cria um outlier que está 6.6 desvios padrôes abaixo da média 
  Outlier <-  mean(y) - 6.6 * sd(y)
  
  y <- c(y,Outlier)
  df <-  data.frame(y)
  df$id <-c(1:666)
  
  #Informar ao R que esse caso 666 é o caso outlier
  
  
  df <- df |> 
    mutate(Outlier = 
             ifelse(id %in% c ("666"),
                    T,F))
  
  
  ggplot(df, aes(id,y)) + geom_point() + theme_bw() + 
    labs(x = "Casos possíveis", 
         y="Váriavel dependente",
         title = "Como selecionar casos como Iron Maiden?") +
    #label repel
    geom_label_repel(data = filter(df, Outlier == T), aes(label = id), size =10)
+ geom_point(data = . |>  filter(id == "666"),
  size =11, shape = 19,
  fill  = "red", color = "red" ,alpha = .5) +
  theme(text = element_text(size = 24))
  
  #Qual que a importância disso? Mostrar qual a aquela váriavel que mudou e que vai ficar só o caso com  o rótulo e não todos
  
```

</details>

<details>
<summary>⚖️9. Noçoes de Jurimetria</summary>

```r

 #Relacionado ao direito
 #jurimetria em que voce usa a estatistica para analisar os fenomenos de direito 
 
 #Jurimetria: juntar ciencia e direito 
 
 
 install.packages("abjData")
 library(abjData)       #pacote de dados da associação brasileira de jurimetria(abj), criada pra começar a difundir essas ideias 
 
 data("assuntos")  # para carregar a base 
 
 View(assuntos)
 
 names(assuntos)  # para saber o nome das variáveis 
 
 ncol(assuntos)    # para saber o número de colunas 
 
 nrow(assuntos)   #quantas linhas eu tenho na base 
 
 str(assuntos)   #tipo das variaveis 
  
 class(assuntos)  #qual a classe do objeot 
 
head(assuntos)    #primeiras linhas 

tail(assuntos)    #últimas linhas 

summary(assuntos)  #estatisticas descritivas básicas 



#Estatística Descritiva Básica 

mean(assuntos$total)

median(assuntos$total)   #ela não é afetada por outliers, mais robusta a outliers

min(assuntos$total)    #valor minimo 

max(assuntos$total)   #valor maximo 

sd(assuntos$total)   #para ver quao homogenea ou quao heterogenea é a distribuição 

range(assuntos$total)   #o espectro do menor valor para o maior valor(max - min)  amplitude 

summary(assuntos$total) #útil para variaveis quantitativas 


#Exemplo 2 

data("leiloes")   #pegar a base no ambiente 

View(leiloes)     #para ver a base 
  
dim(leiloes)  #retorna o numero de linhas e colunas 

table(leiloes$vendeu)
prop.table(table(leiloes$vendeu))

mean()   #não faz sentido pegar media de varivel qualitativa 

library(freqdist)

freqdist(leiloes$vendeu)

leiloes$dif <- leiloes$valor_total_arrematado - leiloes$valor_avaliacao_inicial

mean(leiloes$dif)

mean(leiloes$dif,na.rm = T)

min(leiloes$dif,na.rm = T)
 
max(leiloes$dif,na.rm = T) 

leiloes$ratio <- leiloes$valor_total_arrematado/leiloes$valor_avaliacao_inicial 

mean(leiloes$ratio, na.rm = T)
 
median(leiloes$ratio, na.rm = T) 
 
 
```

</details>
<details> 
<summary>📉 10. Identificação de Tipos de Distribuição (fitdistrplus)</summary>

```r
  if(!require("pacman")) install.packages("pacman")
  library(pacman)
  pacman::p_load(fitdistrplus)

#pacote fitdistrplus é um pacote que vê qual que o tipo de distribuição que você está trabalhando e ve qual o modelo mais adequado para trabalhar com aquilo  

  #iremos simular uma base de dados 
  
  set.seed(123)

  #determinar o tamanho da amostra 
  
  n <- 1000
  
  #geraremos varias distribuições 
  
  #Normal
  
  x1 <- rnorm(n,mean = 10,sd=2)
  
  #Uniforme
  
  x2 <- runif(n,min = 0,max = 1)
  
  #Exponencial
  
  x3 <- rexp(n,rate = 0.5)
  
  #Logística
  x4 <- rlogis(n,location = 0,scale = 1)  #para area de pesquisa
  
  #Beta
  x5 <- rbeta(n,shape1=2,shape2 = 5)   #frnacisco kibari que faz essa regressão beta
  
  #Lognormal
  x6 <- rlnorm(n,meanlog = 1,sdlog=0.5)
  
  #Gamma
  x7 <- rgamma(n,shape=3,scale=2)
  
 #agora juntaremos todas em um df 
  
  
  
  df <-  data.frame(c(x1,x2,x3,x4,x5,x6,x7))   #feita as variaveis
  
  #agora para testar iremos visualizar com 
  
  descdist(x1,discrete = FALSE) #NORMAL
  #OUTRA FORMA DE VER E FAZENDO UM HISTOGRAMA
  hist(x1)
  
  descdist(x2,discrete = FALSE) #UNIFORME
  #idem
  hist(x2)
  
  descdist(x3,discrete = FALSE) #EXPONENCIAL
  hist(x3)
  
  descdist(x4,discrete = FALSE) #LOGISTICA
  hist(x4)
  
  descdist(x5,discrete = FALSE) #BETA
  hist(x5)
  
  descdist(x6,discrete = FALSE) #LOGNORMAL
  hist(x6)
  
  descdist(x7,discrete = FALSE) #GAMMA
  hist(x7)
  
  
  #ELE MOSTRA A CURTOSE E A RAIZ DA SIMETRIA
  #E MOSTRA QUAL QUE É A DISTRIBUIÇÃO TEÓRICA ESPERADA, OU SEJA, AQUELA QUE MELHOR SE ENCAIXA
  #PROCURA ONDE O VERMELHO ESTÁ MAIS PRÓXIMO
```
</details>
<details>  
<summary> 💰 11. Desigualdade: Cálculo do Coeficiente de Gini e Curva de Lorenz</summary>

```r
#TAMBÉM CONHECIDO COMO ÍNDICE DE GINI - QUE CALCULA DE FORMA GERAL A CONCENTRAÇÃO DE RENDA (PODE SER QUALQUER CONCETRAÇÃO, EX:RESTAURANTES, LOJAS...)
  #UTILIZANDO TAMBÉM A CURVA DE LORENZ 
   if(!require("pacman")) install.packages("pacman")
  library(pacman)
  pacman::p_load(dplyr #MANIPULAR DADOS
                 ,ggplot2,ineq # AQUI QUE TEM A FUNÇÃO PARA CALCULAR GINI 
                 ,scales #TRABALHAR COM ESCALAS
                 ,tibble #CRIAR DFS E TAL
                  )
  
  set.seed(123)

  #Gerar dados simulados  
  
  n <- 1000
  renda <- rlnorm(n,meanlog = 8,sdlog=0.8)
dados <- tibble(id = 1:n,
                renda=renda
                )  
  
#Cálculo de gini

gini <- ineq::Gini(dados$renda)
gini
#como que interpreto? ela sempre varia entre 0 e 1 , 0 quando todo mundo tem a merma renda e 1 quando só 1 pessoa tem a renda toda
#como se fosse numa pizzaria e voce deveria pegar pelo menos 1 pedaço e se não tivesse nenhum ou voce comesse todos seria algo que essa tal coisa está concentrando aquela pizza 

#exemplo manual para ajudar a visualizar

x1 <- c(1,1,1,1,1,1,1,1,1)
ineq::Gini(x1)


x2 <- c(0,0,0,0,0,0,0,1)
ineq::Gini(x2)
#não dá 1 porque teria de tender ao infinito para todos irem convergirem a ser 1 

#Calculo manual de gini 
#Formula
#G=[2*sum(i*y_1)]\[n*sum(y_1)] -(n+1)\n

#GINI de 1 = desigualdade máxima
#Gini de 0 = igualdade perfeita




#CURVA DE LORENZ - TAMBÉM COM O PACOTE INEQ
lorenz <- ineq::Lc(dados$renda)

#converter para df 
lorenz_df <-  data.frame(
  p= lorenz$p,  #proporção acumulada da população 
  l=lorenz$L    #proporção acumulada da renda 
)

#agora vou plotar 

ggplot(lorenz_df, aes(x=p,y=L)) + 
  geom_line(size=1.2) + 
  geom_abline(linetype="dashed")+ 
  labs(title="",subtitle = "") + theme_minimal()

#quanto menor a barriga do gráfico de gini, então a concentração é menor e quanto maior a barriga do gráfico de gini então maior a concentração - de acordo com a Curva de Lorenz

#sites que tem dados bons 
#IPEA
##https://www.ipea.gov.br/desafios/index.php?option=com_content&id=2048%3Acatid%3D28
#GINI
#https://www.ipea.gov.br/desafios/index.php?option=com_content&id=2048%3Acatid%3D28
#BANCO MUNDIAL
#https://data.worldbank.org/indicator/SI.POV.GINI
```

</details> 
<details>
<summary>➗ 12. Gráfico Teórico: Equação de Heckman (Capital Humano)</summary>
  
```r
#equação de heckman é aquela equação que é o efeito do que que acontece com a produtividade quando se investe em educação básica, quanto mais cedo se investe no individuo maior a taxa de retorno

install.packages("ggplot2")
library(ggplot2)

#Simular base de dados
set.seed(123)
age <- seq(0,30,by=0.1)
return_rate <-  0.6 *exp(-0.1*age) #curva exponencial decrescente 

#criar df
df <- data.frame(age = age, return_rate= return_rate)

#Definir as regiões analisadas, pré escola, escoal e pós escola 
df$stage <-  
  cut(df$age,
      breaks = c(-Inf,5,18,Inf),
      labels = c("Preschool","School","Post-school")
      )


#Linha do custo de oportunidade

ggplot(df, aes(x=age,y=return_rate)) + 
  geom_ribbon(aes(ymin = 0,ymax = return_rate, fill = stage),alpha = 0.3) + 
  geom_line(color = "red", size=1.2)+
  geom_hline(yintercept = r_line,linetype="dashed") + 
  #annotate eu consigo escrever dentro do gráfico 
  annotate("text",x=2.2,y=0.3,label="Programas do Prézinho",color="orange",size=8) + 
  annotate("text",x=8.5,y=0.03,label="Programas na Escola",color="pink",size=8) + 
  annotate("text",x=2.2,y=0.3,label="Programas Pós Escol",color="blue",size=8) + 
  scale_fill_manual(values= c("Preschool"="gold","School"="skyblue","Post-school"="lightgreen")) +
labs(x= "Idade", y="taxa de retorno",
     title = "Taxa de retorno de investimento em capital humano",
     caption = "fonte: eu")
+theme_minimal() +
  theme_minimal(base_size=25)+
  theme(legend.title = element_blank())
                              
                              
                              
                              
       
```

</details>

<details>
<summary>📈13. Como prever a partir de pacote R a maior probabilidade da cor de alguém de acordo com seu nome </summary>
  
```r
--------

install.packages("rethnicity") #Pacote identifica a raça/cor a partir do nome da pessoa, atraves da distribuição de probabilidade do nome da pessoa seguindo a lógica de registros que são acumulados e pega a probabilidade de estar contido

library(rethnicity)

#Testes de nomes 


predict_ethnicity(firstnames = "Mirella", lastnames = "Marchini")

predict_ethnicity(firstnames = "Taylor", lastnames = "Swift")

predict_ethnicity(firstnames = "João", lastnames = "Aguiar")

predict_ethnicity(firstnames = "Whindersson", lastnames = "Nunes")

predict_ethnicity(firstnames = "Carlos", lastnames = "Arthur")

predict_ethnicity(firstnames = "Carlos", lastnames = "Carvalho")

predict_ethnicity(firstnames = "Carlos", lastnames = "Canto")

predict_ethnicity(firstnames = "Carlos Arthur", lastnames = "Carvalho Canto ")

predict_ethnicity(firstnames = "Danielle", lastnames = "Oliveira")

predict_ethnicity(firstnames = "Danielle", lastnames = "Carvalho")

predict_ethnicity(firstnames = "Arthur", lastnames = "Cardoso")

predict_ethnicity(firstnames = "Pedro", lastnames = "Gomes")

predict_ethnicity(firstnames = "João", lastnames = "Neto")

predict_ethnicity(firstnames = "Paolo", lastnames = "Storto")

predict_ethnicity(firstnames = "Percival", lastnames = "Canto")

predict_ethnicity(firstnames = "Nathália", lastnames = "Bender")

predict_ethnicity(firstnames = "Josyanne", lastnames = "Soares")

predict_ethnicity(firstnames = "Mirella", lastnames = "Marchini")

predict_ethnicity(firstnames = "Reginaldo", lastnames = "Kawa")

#artigo citado que é bom para usar de repertorio para ver qual seriam mais empregaveis quando se usa um nome mais recorrente de raça diferente 
#https://www.nber.org/papers/w9873 (LINK DO ARTIGO)







```

</details>
<details>
<summary>🗺️ 14. Geoprocessamento: Mapas Estaduais e Variação do Gini (geobr)</summary>

```r

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

```

</details>
<details>
<summary>📏 15. Distância Geográfica Computacional (geosphere)</summary>
  
```r
if(!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(readr, geosphere)

# df <- read_delim("latitude-longitude-cidades.csv", delim = ";")
recife_cords <- c(longitude = -34.87707, latitude = -8.046658) 

df$distancia_para_recife_kms <- distGeo(
  matrix(c(df$longitude, df$latitude), ncol=2),
  recife_cords
) / 1000
```

</details>

<details>
<summary>🤖 16. Web Scraping Dinâmico: Lista de Presidentes do BCB (Wikipédia)</summary>

```r
if(!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(rvest, tidyr, stringr, lubridate, janitor, readr, purrr, dplyr,
               ggstatsplot, genderBR, freqdist)

url <- "https://pt.wikipedia.org/wiki/Lista_de_presidentes_do_Banco_Central_do_Brasil"
pg <- read_html(url)

# --- Titulos das seções ---
heads <- pg |> 
  html_elements("h2 .mw-headline") |> 
  html_text() |>  
  stringr::str_squish()

# --- Total de wikitables ---
tabs <- pg |>  
  html_elements('table.wikitable') |> 
  html_table(fill = TRUE)

# --- Função para limpar a tabela (VERSÃO BLINDADA) ---
clean_one <- function(tb, section_name) {
  temp <- tb |> 
    janitor::clean_names() |> 
    select(-matches("^foto$"), everything())
  
  # Tentativa de renomear colunas dinamicamente
  temp <- temp |> 
    rename(
      nome = matches("nome|titular|presidente_do_banco"),
      inicio = matches("in[íi]cio"),
      fim = matches("^fim$"),
      presidente_republica = matches("^presidente")
    )
  
  # PROTEÇÃO: Se a coluna 'nome' não foi criada pelo rename, cria uma fake para não dar erro no mutate
  if(!"nome" %in% names(temp)) {
    temp$nome <- NA_character_
  }
  
  temp |>
    tidyr::fill(presidente_republica, .direction = "down") |> 
    mutate(secao = section_name)
}

# --- Identificação das Seções Alvo ---
alvos_regex <- "Ditadura Militar|Nova República"
alvos_idx <- which(str_detect(heads, alvos_regex))

# Filtramos apenas as tabelas que estão sob esses títulos
section_labels <- heads[alvos_idx]
tabs_filtradas <- tabs[alvos_idx]

# --- Limpar e unificar ---
bc_raw <- purrr::map2_dfr(tabs_filtradas, section_labels, clean_one)

# --- Dados limpos ---
bc <- bc_raw |> 
  # Agora o 'nome' existe obrigatoriamente (mesmo que como NA)
  filter(!is.na(nome)) |> 
  mutate(
    nome = stringr::str_squish(as.character(nome)),
    interino = str_detect(str_to_lower(nome), "interino"),
    nome = str_remove_all(nome, "(?i)\\s*\\(interino\\)\\s*")
  ) |>
  mutate(
    fim = ifelse(str_detect(str_to_lower(fim), "exerc|atual"), NA_character_, fim),
    inicio = str_squish(inicio),
    fim = str_squish(fim),
    inicio_date = suppressWarnings(lubridate::dmy(inicio)),
    fim_date = suppressWarnings(lubridate::dmy(fim))
  ) |>
  # Filtra para remover linhas de cabeçalho repetidas ou vazias
  filter(nome != "" & !str_detect(nome, "Independ|Presidente|№|Titular")) |> 
  transmute(
    secao, 
    nome_presidente_bc = nome,
    interino,
    inicio, fim,
    inicio_date, fim_date,
    presidente_da_republica = presidente_republica
  )

# --- Duração e Sexo ---
bc <- bc |> 
  mutate(
    duracao_dias = as.integer(fim_date - inicio_date),
    duracao_anos = round(duracao_dias/365.25, 2)
  ) |> 
  mutate(primeiro_nome = str_extract(nome_presidente_bc, "^\\w+")) |>
  mutate(sexo = genderBR::get_gender(primeiro_nome)) |> 
  mutate(sexo = case_when(
    nome_presidente_bc == "Wadico Waldir Bucchi" ~ "Male",
    sexo == "Female" ~ "Feminino",
    sexo == "Male" ~ "Masculino",
    TRUE ~ sexo
  ))

# --- Salvar e Plotar ---
write_csv(bc, "presidente_bcb_wikipedia.csv")
cat("\nArquivos salvos!")

print(head(bc, 10))

ggstatsplot::ggpiestats(data = bc, x = sexo, title = "Sexo dos Presidentes do BCB")

```

</details> 
<details>
<summary>🎥 17. Dataviz Interativa: Gráficos Animados com gganimate (Parte 1)</summary> 
  
```r
if(!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(gganimate,
               tidyr  #Transformar a base em formato long
, dplyr #manipulação de dados
  ,lubridate,
  ,gghighlight #destaque pra ficar mais bonito
,
  ggplot2,ggpurrr, ggstatsplot,gganimate # pacote que vai animar os gráficos
,
ggrepel #para repelir rótulos
,ggthemes #formate o gráfico a partir de temas pré estabelecidos
)

#ler base de dados

ideb |>  read.excel("IDEB")

#Data frame

ideb_df <- ideb_df |>  
  mutate(across(x(ideb_17,ideb_18,ideb_19)))

#Converter todas as colunas que tem ideb em formato numérico

#agora usar tidyr para mudar todos que tem wide em formato long
#Graficos e estatisticas 

ideb_long <- ideb_df |>  
  pivot_longer(
    cols= starts_with("ideb_"),  #Seleciona todas as colunas que tem ideb 
    names_to= "ano", #nome da nova coluna ano 
    values_to = "ideb" # Nome da nova coluna ano com os valores de ideb
  )


ideb_long <- ideb_long |> 
  mutate(
    ano = case_when(
      ano==  "ideb_17" ~ 2017,
      ano==  "ideb_18" ~ 2018,
      ano==  "ideb_19" ~ 2019,
      TRUE ~ as.numeric(ano)
    )
  )
class(ideb_long$ano) #A variavel de tempo tem que ser numeric, senão vai dar erro 

#EXEMPLO 

quixaba <- ideb_long |>  
  filter(uf =="PE" & 
           rede == "Pública" & 
           nível == "EM") |> 
  ggplot(aes(ano,ideb,colour = nome_mun,
             fil="blue")) + 
  geom_line(size= 1.5,
            alpha = .5,
            fill="blue",
            col="blue") +
  geomhighlight(nome_mun == "Quixaba",
                label_pares=list(size = 8,
                                 fill ="gray",
                                 col=.9)) + 
  geom_text(aes(label = nome_mun),
            size= 9 ,
            fontface = 'bold', col = "blue") +
  theme_bw() + 
  labs(x= "",
       y="IDEB",
       title="IDEB em Pernambuco(2017-2023)",
       caption="fonbte: EU") +
  theme(text= element_text(size=22)) + 
  scale_x_continuous(breaks=seq(2017,2019,by=2)) + 
  transition_reveal(ano)  #Informação que eu quero que a informação seja animada por ano 


quixaba  #estudo de caso dessa cidade é mostrado 

anim_save("IDEB/quixaba.gif")

#Outra abordagem 
ideg_agg <- ideb_long |> 
  filter(rede =="Pública" & nível == "EM") |> 
  group_by(uf,ano) |> 
  summarise(avg_ideb = mean(ideb,na.rm =T))

 sp_pe  <- ideg_agg |> 
   ggplot(aes(ano, avg_ideb , 
              colour = uf,   #A cor é baseada no nome da uf
              fill= uf     # a cor de preenchimento é baseada na uf 
              )) +
  geom_line(size=1.5, alpha =.5)+
   geomhighlight(uf %in% #esse in é pra ele escolher esses dois que estão no vetor, ou seja, SP e PE 
                   
                   c("SP","PE"), #DESTACA SP E PE NOS DEMAIS 
                label_params=list(size = 8,
                                  fill = "gray",
                                  col= "blue",
                                  alpha= .9)) + 
   geom_text_repel(aes(label=uf),
                   size =9,
                   fontface = 'bold',
                   vjust=-.3) + 
   scale_color_manual(values = c("SP" = "blue", "PE"="red"))
                   
sp_pe_economist <-  sp_pe + theme_economist()  #para aparecer como se fosse do tipo economista 

#ou fazer do tipo wsj

sp_pe_wsj <- sp_pe + scale_color_wsj() #para ficar com gif com tipo de wall street journal

```

</details>
<details>
<summary>🌐 18. Análise de Redes: Mapeamento de Coautoria (Google Scholar)</summary>

```r
  
install.packages("scholar")  #salvando o pacote de R 
library(scholar)  #esse comando chama o pacote para o ambiente do R , ajuda a automatizar os dados do perfil do google escola 
  
#data frame
  
  data_scholar <-  get_coauthors('5sz_jBoAAAAJ6hl')  #para puxar os dados da internet, em que ele puxa esses dados do google escola e com o user que é esse ai, sendo esse o id de cada pesquisador do mundo 
  
  
 #nivel 1 
  plot_coauthors(data_scholar)  #esse é o gráfico de coautores dos autores de todos aqueles que são coautores 
  #daria pra fazer uma rede de crimes tambem, dá pra fazer uma rede de varias coisas 
  
  
  install.packages("ggplot2")  
  library(ggplot2) 
  
  #Nivel 2- adicionei as legendas em pt br 
  
  plot_coauthors(data_scholar) + 
    labs(title = "rede de cobertura ",
         subtitle = "dados",
         caption = "fonte:eu")
  
  #Nivel 3 - mudei o tamanho das fontes 
  plot_coauthors(data_scholar) + 
    labs(title = "rede de cobertura ",
         subtitle = "dados",
         caption = "fonte:eu") +_
  theme_void(base_size = 20) + 
    theme(
      plot.title = element_text(size=36,face="bold"),
      plot.subtitle = element_text(size = 24),
      plot.caption = element_text(size = 16),
      plot.title.position = "plot"
    )
  
  #nivel 4 - automatizei com a questão de aparecer o dia já de prontidão 
  
  nome <- data_scholar$author
  
  plot_coauthors(data_scholar) + 
    labs(title = paste("rede de cobertura de",nome),
         subtitle = paste("Atualizado em", format (Sys.Date(),"%d de %B de %Y")),
         caption = "fonte:eu") +
  theme_void(base_size = 20) + 
    theme(
      plot.title = element_text(size=36,face="bold"),
      plot.subtitle = element_text(size = 24),
      plot.caption = element_text(size = 16),
      plot.title.position = "plot"
    )
  
  #exemplos 
  
  coauthors_network <-  get_coauthors("codigo id")
  
  nom <- coauthors_network$autor
  plot_coauthors(coauthors_network) + 
    labs(title = paste("rede de cobertura de",nome),
         subtitle = paste("Atualizado em", format (Sys.Date(),"%d de %B de %Y")),
         caption = "fonte:eu") +
    theme_void(base_size = 20) + 
    theme(
      plot.title = element_text(size=36,face="bold"),
      plot.subtitle = element_text(size = 24),
      plot.caption = element_text(size = 16),
      plot.title.position = "plot"
    )
  
  
```

</details>

<details>
<summary>🧪 19. Estatística Paramétrica: Testando Pressupostos de Regressão Linear</summary>

```r

#Aprender a testar os pressupostos do modelo de regressão 

if(!require("pacman")) install.packages("pacman")
library(pacman)

pacman:: p_load(
  performance, 
  dplyr,
  ggplot2
)

set.seed(123)  # Para reprodutibilidade

options()
??options
#Tamanho da amostra 

n <-  1000

#Gerando preditores 

x1 <- rnorm(n, mean = 5, sd=2)
x2 <- rnorm(n, mean = 3, sd=1)
x3 <- rnorm(n, mean = 0, sd=1)
x4 <- rnorm(n, mean = -2, sd=1)


y <-2*x1 + 1.5*x2 + 0.1*x3 - 2.5*x4 + rnorm(n)    #a variavel com menor impacto é x3 e a com maior é x4 por ser a maior e ser negativo, e o rnorm(n) é o erro

#criando o data frame 

data <- data_frame(y,x1,x2,x3,x4)

#dados criados agora mão na massa da regressão linear 

modelo1 <- lm(y ~ x1+x2+x3+x4 , data = data)
summary(modelo1)

#os modelos estão próximos dos que foram estipulados, com modelos simulados é fácil NA VIDA REAL É DIFERNTE 

#agora que foi estipulado iremos ver no pacote performance:
#valores e signifcancia dos coeficientes, ou seja, ele vai mostrar estatisticas de ajuste

performance:: performance(modelo1)

performance::check_collinearity(modelo1)

performance::check_normality(modelo1)

performance::check_outliers(modelo1)  #modelo de regressão não pode ter nenhum outlier muito danoso

performance::check_heteroscedasticity(modelo1)  #para ver se a diferença nas variaâncias

performance::check_model(modelo1)

#Vamos visualizar os resultados graficamente

install.packages("sjPlot")
library(sjPlot)

plot <- sjPlot:: plot_model(modelo1,show.intercept = T, ci.lvl = 0.95) +
  theme_bw()
  
plot(plot)

plot + geom_hline(yintercept = 0,linetype = "bold",color= "green",size=2,alpha=.5)

```
</details>

<details>
<summary>🌍 20. Mapas de Padrão GloboNews: Índice V-Dem (Democracia Eleitoral)</summary>   

```r
library(vdemdata)
library(dplyr)
library(ggplot2)
library(countrycode)
library(rnaturalearth)
library(ggpubr)

# Função personalizada desenvolvida para extração dos scores de Poliarquia (v2x_polyarchy)
# Geração e plotagem via ggplot + geom_sf formatada nos padrões de dashboards profissionais (Tema Globonews / Carlos Canto).
# Uso de ISO3C keys para left_join espacial impecável.
```
</details>

<details>
<summary>🔍 21. Forense Eleitoral e Auditoria: A Lei de Benford</summary>
  
```r
  # Forense Eleitoral
  # detecção estatistica de sistematica de irregularidades eleitorais,feitos pelo Klimack e o Turner
  # o que é visto é a taxa de comparecimento e os votantes
  # nao se deve ter nenhuma concentração de pontos no canto direito porque todos naquela zona "coincidentemente" votaram no mesmo cara, isso pode ser uma sinalização de que aquilo pode ser algo muito estranho ali
  # a frequencia é algo totalmemte necessário para que eu possa ver algo no mínimo relevante para analisar
  # science advances tambéme é outra fonte que valida essa forma de analisar
  # pode se criar anomalias ou presenças de algo diferente porem não sao coisas que so podem levar as fraudes, podem ser catastrofes,eventos urbanos cimo blitz e etc...
  # Técnica que quanto mais desagregada melhor, ou seja, por sessão é o melhor e depois zona, mun e estado
  # para mudar e não ter nada é muito deveriam ter que ficar com varias salas com coordenação muito especifica para não ficar rastros 
  
  if(!require("pacman")) install.packages("pacman" )
  # Nota: 'sclaes' foi corrigido para 'scales'. Adicionado 'electionsBR' para consumo da API do TSE.
  p_load(dplyr, ggplot2, patchwork, janitor, readxl, scales, viridis, stringr, electionsBR ) 
  
  options(scipen = 999 )
  
  
  # 1. Funções Auxiliares
  
  
  padronizar_chaves_secao <- function(base) {
    base %>%
      mutate(
        CD_MUNICIPIO = as.character(CD_MUNICIPIO),
        NR_ZONA = as.character(NR_ZONA),
        NR_SECAO = as.character(NR_SECAO)
      )
  }
  
  fazer_fingerprint <- function(base, 
                                y_var = "vote_share",
                                titulo,
                                label_y,
                                bins = 35,
                                legenda_fill = "Número de\nseções") {
    
    ggplot(base, aes(x = turnout, y = .data[[y_var]])) +
      geom_bin_2d(bins = bins) +
      scale_x_continuous(
        labels = percent_format(accuracy = 1),
        limits = c(0, 1)
      ) +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        limits = c(0, 1)
      ) +
      scale_fill_viridis_c(
        option = "plasma",
        name = legenda_fill
      ) +
      labs(
        x = "Turnout",
        y = label_y,
        title = titulo
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        text = element_text(size = 14)
      )
  }
  
  resumir_votos_turno <- function(base, turno) {
    base %>%
      filter(NR_TURNO == turno) %>%
      group_by(NM_VOTAVEL, NR_VOTAVEL) %>%
      summarise(
        total_votos = sum(QT_VOTOS, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        votos_validos = sum(
          total_votos[!NM_VOTAVEL %in% c("VOTO NULO", "VOTO BRANCO")],
          na.rm = TRUE
        ),
        percentual = round(100 * total_votos / votos_validos, 2)
      ) %>%
      select(-votos_validos) %>%
      arrange(desc(total_votos))
  }
  
  preparar_base_fingerprint_secao <- function(base, turno) {
    base %>%
      filter(
        NR_TURNO == turno,
        !is.na(turnout),
        !is.na(vote_share),
        between(turnout, 0, 1),
        between(vote_share, 0, 1)
      ) %>%
      distinct(
        ANO_ELEICAO,
        NR_TURNO,
        CD_MUNICIPIO, # Inserido para ligar as imagens cortadas
        NR_ZONA,      # Inserido para ligar as imagens cortadas
        NR_SECAO,     # Inserido para ligar as imagens cortadas
        turnout,
        vote_share
      )
  }
  
  filtrar_candidato <- function(base, candidato) {
    base %>%
      filter(NM_VOTAVEL == candidato)
  }
  
  # 2. Dados via API (Exemplo com electionsBR)

  # Exemplo de como usar a API para puxar os dados diretamente do Repositório 
  # de Dados Eleitorais do TSE.
  
  dados_tse <- vote_section_fed(year = 2022, uf = "all") %>% 
  clean_names()
 
  # 3. MUNICÍPIOS - 1º turno
  
  
  dados_municipio <- read_excel(
    "raw-data/votos_presidente_muni_nexojornal_2022.xlsx",
    sheet = "absoluto-1t-2022"
  ) %>%
    clean_names() %>%
    mutate(
      turnout = comparecimento / eleitores,
      # Aqui você precisa calcular a proporção de votos do candidato alvo
      # Supondo que a coluna de votos dele se chame 'votos_candidato_x'
      vote_share = votos_candidato_x / comparecimento 
    )

  # 4. PROCESSAMENTO DOS DADOS (EXEMPLO COM DADOS POR SEÇÃO DO TSE)

  # Como você anotou, a análise por seção é a melhor. Vamos simular o fluxo
  # de cálculo de Turnout e Vote Share assumindo que usamos a base 'dados_tse'
  
  # 4.1 Calcular comparecimento (turnout) e votos totais por seção
  dados_secao <- dados_tse %>%
    group_by(ano_eleicao, nr_turno, sg_uf, cd_municipio, nr_zona, nr_secao) %>%
    mutate(
      # Total de votos registrados na urna (votos válidos + nulos + brancos)
      comparecimento = sum(qt_votos, na.rm = TRUE),
      
      # Turnout (Taxa de comparecimento): Comparecimento / Eleitores Aptos
      # Certifique-se de que a coluna de eleitores aptos se chama 'qt_aptos'
      turnout = comparecimento / qt_aptos 
    ) %>%
    ungroup()
  
  # 4.2 Escolher o candidato alvo e calcular o Vote Share
  candidato_alvo <- "NOME DO CANDIDATO" # Substitua pelo nome exato que está na base
  
  base_candidato <- dados_secao %>%
    # Usa a função que você criou para filtrar o candidato
    filtrar_candidato(candidato_alvo) %>%
    mutate(
      # Vote share: Proporção de votos do candidato em relação ao comparecimento total da seção
      vote_share = qt_votos / comparecimento
    )
  
  # 4.3 Preparar a base final para o plot (exemplo: 1º Turno)
  base_pronta_fingerprint <- preparar_base_fingerprint_secao(
    base = base_candidato, 
    turno = 1
  )
  
  # 5. GERANDO E SALVANDO O FINGERPRINT (GRÁFICO)
 
  
  # Usa a função principal que você construiu com o ggplot2
  grafico_fingerprint <- fazer_fingerprint(
    base = base_pronta_fingerprint,
    y_var = "vote_share",
    titulo = paste("Fingerprint Eleitoral (1º Turno) -", candidato_alvo),
    label_y = "Vote Share (Votos do Candidato / Comparecimento)",
    bins = 40 # Você pode ajustar o tamanho dos "quadradinhos" aqui
  )
  
  # Exibe o gráfico no visualizador do RStudio
  print(grafico_fingerprint)
  
  # Caso queira salvar o gráfico em alta resolução
 ggsave("fingerprint_candidato_1t.png", plot = grafico_fingerprint, width = 10, height = 7, dpi = 300) 
 
```

</details>

<details>
<summary> ➕ 22. Implementação do Método Rozenas </summary>

```r
#Metodologia desenvolvida pelo Arthur Rozenas que é pouco conhecido mas é muito citado
 
  # ANÁLISE FORENSE ELEITORAL: MÉTODO DE ROZENAS (2017)
 
 # 
 # -A intuição do método: O ser humano é péssimo em inventar números aleatórios e por isso usa disso para achar rastros de fraude
 #   Quando tentam fraudar dados eleitorais, há um viés cognitivo para atribuir que é ai que se pega quem fzaz coisa errada
 #   percentuais inteiros ou terminados em 0 e 5 (ex: 80%, 85%) e fazer a conta reversa. utilziar de formas de arrendomentos é sinal de fraude 
 # Em uma distribuição honesta, a divisão (Votos / Comparecimento) gera 
 #números fracionados complexos.
 # O algoritmo compara a frequência de números "arredondados" ou dígitos adjacentes com a distribuição esperada ao acaso são o grande sinal de adulteração. 
 #Uitlizdando sempre de numeros com relaçõers a zonas e munciipios por ser mais dificil de ter erros e ser mais preciso

 #Artigo Principal: Arturas Rozenas (2017) - "Detecting Election Fraud from Irregularities in Vote-Share Distributions", publicado na Political Analysis 
 # Psicologia/Viés: Artigo de Beber & Scacco (2012) sobre a preferência humana por dígitos adjacentes e arredondamento
 # Outros autores: Walter Mebane (citado como referência em metodologias forenses) 
 # Pacote R: {spikes}, desenvolvido pelo próprio Arturas Rozenas 
 
#Exemplo do que vai ser utilizaado
 # - Base utilizada: Eleição Presidencial de 2022 (Lula x Bolsonaro)
 # Utiliza se o pacote spikes 
 
 # Carregando os pacotes padrão de manipulação e o pacote do modelo
 pacman::p_load(tidyverse, spikes, janitor)
 

 # O pacote {spikes} exige uma formatação bem sencisevlk logo ele só aceita bases com 
 # EXATAMENTE as três colunas a seguir (sem valores faltantes - NA):
 # N = Eleitores Registrados
 # T = Comparecimento (Turnout)
 # V = Votos nominais no Candidato
 
 set.seed(4187) # Semente fixada
 
 # Simulando dados de 1.000 seções eleitorais quaisqueres
 df_eleicoes <- tibble(
   id_secao = 1:1000,
   N = round(runif(1000, min = 100, max = 500)),       # Eleitores registrados
   T = round(N * runif(1000, min = 0.70, max = 0.85)), # Comparecimento 
   V = round(T * runif(1000, min = 0.40, max = 0.60))  # Votos no candidato alvo
 )
 
 # Inserindo "fraudes" propositais para o pacote detectar:
 # Vamos forçar que em 30 seções o candidato teve exatamente 90% (número cheio)
 fraudes_idx <- sample(1:1000, 30)
 df_eleicoes$V[fraudes_idx] <- round(df_eleicoes$T[fraudes_idx] * 0.90)
 df_rozenas <- df_eleicoes %>%
   # 1. Manter apenas seções com amostragem relevante (conforme o artigo)
   filter(N >= 100) %>% 
   
   # 2. Omitir qualquer linha com NA (isso quebra a função do pacote)
   drop_na() %>%        
   
   # 3. Selecionar as três variáveis exigidas (N, T e V) senão não vai funcionar de jeito nenmhum 
   select(N, T, V)
 
 
 #Análise - Aplicamos aqui o metodo de Rozenas
 
 # Aviso: O método por trás (densidade de kernel com estatística Bayesiana) 
 # é bastante pesado computacionalmente. O pacote sugere 1000 reamostragens, 
 # mas reduzi para 100 aqui para você conseguir rodar rápido na sua máquina.
 
 cat("Estimando o modelo de Rozenas... Isso pode demorar um pouco.\n")
 
 resultado_fraude <- spikes(
   data = df_rozenas, 
   resamples = 100   # Para a base real do TSE, altere para 1000
 )
 
 # O objeto gerado é uma lista. O mais importante é o índice "fraud".
 
 # Extraindo a proporção de seções contaminadas
 estimativa <- resultado_fraude$fraud
 cat("\nProporção estimada de fraude na base simulada:", round(estimativa * 100, 2), "%\n")
 
 # Gráfico de densidade gerado pelo próprio pacote
 # (Ele cruza a distribuição esperada com a observada. Os picos mostram as anomalias)
 plot(resultado_fraude)
 
 #Esse processo todo pode mostrar algo que mostre algo porém ele ainda pode ser falho nao se usa aqui os numeros verdadeiros mas ele mostra um cenario bom de se visualizar nesse exemplo 
 
 #O ideal que se faça isso por sessão mas demora BASTANTE 
 

```

</details>

<details>
<summary>  23. Estimador de status quo </summary>
```r
   
#utilizamos o começo de lei 
 if(!require("pacman")) install.packages("pacman")
 library("pacman")
 pacman::p_load(
   readr, #leitura de arquivos 
   stringr, #$manipulaçaõ de texto
   genderR,  #identificação de sexo a partir do nome
   dplyr,   #manipulação de dados
   scales  #formatação
   ,freqdist  #Distribuição de frequencias
   ,janitor #Limpa 
 )
 
 options(scipen = 999)
 
 #Base de dados
 dados_brutos <- read_csv2("raw-data/logradouros_recife.csv") %>% 
   clean_names()
 
 #remover duplicatas e de itens repetidos 
 
 #aqui so se tem um nome por vez 
 dados_unicos <- dados_brutos %>%
   distinct(nome_oficial, .keep_all = TRUE)
 
 #separando o tipo e o homenageado da rua 
 
 dados_separados <- dados_unicos %>%
   mutate(
     # Isola o tipo do logradouro (Avenida, Rua, Travessa, etc.) pegando a primeira palavra
     tipo_logradouro = word(nome_oficial, 1),
     
     # Isola a pessoa homenageada (da segunda palavra em diante)
     nome_homenageado = word(nome_oficial, 2, -1),
     
     # Padronização de caixa para evitar falhas no agrupamento
     tipo_logradouro = str_to_upper(tipo_logradouro),
     nome_homenageado = str_to_title(nome_homenageado)
   ) %>%
   # Limpeza de categorias numéricas ou residuais (ex: Rua "1")
   filter(!str_detect(tipo_logradouro, "[0-9]"))
 
 #Limpando o titulo de nomes honrosos e obbtendo o  primeiro nome das ruas 
 
 Precisamos do primeiro nome limpo para que o algoritmo do genderBR acerte a previsão
 titulos_remover <- "Dr |Doutor |Prof |Professor |Eng |Engenheiro |Gen |General |Dep |Deputado |Pres |Presidente |Des |Desembargador |Pe |Padre |Dom |Cap |Capitão |Sgto |Sargento "
 
 dados_genero <- dados_separados %>%
   mutate(
     # Retira os pronomes de tratamento e títulos mapeados
     nome_limpo = str_remove_all(nome_homenageado, regex(titulos_remover, ignore_case = TRUE)),
     nome_limpo = str_trim(nome_limpo),
     
     # Isola o primeiro nome real
     primeiro_nome = word(nome_limpo, 1)
   ) %>%
   # A função get_gender cruza com os microdados do IBGE para classificar Masculino ou Feminino 
   mutate(
     genero_previsto = get_gender(primeiro_nome)
   )
 
 #Agrego e pego resultado desses nomes 
 
 resumo_machismo <- dados_genero %>%
   filter(!is.na(genero_previsto)) %>%
   count(genero_previsto) %>%
   mutate(
     percentual = n / sum(n),
     percentual_formatado = percent(percentual, accuracy = 0.1)
   ) %>%
   arrange(desc(n))
 
 print(resumo_machismo)
 
 #agrego pelo nivel do bairro pra ver se é bairro nobre ou pobre 
 
 resumo_por_bairro <- dados_genero %>%
   filter(!is.na(genero_previsto)) %>%
   group_by(nome_bairro, genero_previsto) %>%
   summarise(quantidade = n(), .groups = 'drop') %>%
   group_by(nome_bairro) %>%
   mutate(
     total_bairro = sum(quantidade),
     prop_masculina = quantidade / total_bairro
   ) %>%
   filter(genero_previsto == "Male") %>%
   arrange(desc(prop_masculina))
 
 #aqui conseguimos ver atraves dos nomes das ruas que cada uma tem um nome que é de forma grande chamada por nome de um Homem 
 
```











