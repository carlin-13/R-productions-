# R-productions-
Aqui dentro contém minhas produções utilizando Rstudio 


## 📚 Laboratório de R: Projetos & Estudos (Datafobia)

Abaixo estão diversos scripts e projetos desenvolvidos em **R**, baseados nos estudos de análise de dados, estatística e jurimetria do canal Datafobia. Clique nas setas para expandir e visualizar o código de cada projeto.

<Estudos/>
```r
```
</details>
<details>
<summary>🔎 1. Buscas Bibliográficas e Nuvem de Palavras (SciELO/CAPES)</summary>

```r
if(!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(stringr, dplyr, writexl, ggplot2, easyScieloPack, ggtext, ggpubr, ggrepel, arrow, purrr, capesR, usethis)

# Busca automatizada no SciELO
base_scielo <- search_scielo("Supremo Tribunal Federal", n_max = 1000)
base_scielo <- base_scielo |>  
  mutate(
    titulo_min = tolower(title),
    tema = stringr::str_detect(titulo_min, "supremo tribunal federal|\\bstf\\b|supremo tribunal brasileiro|corte suprema brasileira| corte constitucional brasileira|surpreme federal court")
  )
df_tematica_scielo <- base_scielo |> filter(tema == "TRUE") |> arrange(year) |> dplyr::select(-tema)
write.xlsx(df_tematica_scielo, "datafobia1.xlsx")

# Busca na CAPES e Nuvem de Palavras
capes <- download_capes_data(c(1987:2022))
df_capes <- map_dfr(capes, read_parquet)
# ... (Omissão de partes repetitivas para otimização do script) ...

pacman::p_load(tidyverse, wordcloud2, tm)
scielo_corpus <- VCorpus(VectorSource((df_tematica_scielo)))
scielo_corpus <- scielo_corpus |> tm_map(removeNumbers) |> tm_map(removePunctuation) |> tm_map(content_transformer(tolower)) |> tm_map(removeWords, c(stopwords("english"), stopwords("spanish"), stopwords("portuguese")))

tdm <- TermDocumentMatrix(scielo_corpus) |> as.matrix()
palavras <- sort(rowSums(tdm), decreasing = T)
df <- data.frame(palavra = names(palavras), freq = palavras)
df$peso <- round(df$freq/sum(df$freq)*100, 2)
wordcloud2(df, size= 0.3, shape= "star", rotateRatio = .5, ellipticity = .10, color = "pink")
```

</details>

<details>
<summary>🎬 2. Análise Exploratória: Como escolher bons filmes? (MUBI)</summary>

```r
if(!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, ggplot2, forcats)

# load("base de dados que já está disponível no meu projeto de R")
# Agregações de Dados e Criação de Gráficos
movies_agg <- base |>  
  group_by() |> 
  summarise(n_casos=n()) |>  
  arrange(-n_casos)

"Planilha" |>  
  filter() |> slice() |> mutate(fct_reorder()) |>
  ggplot() + geom_col() + geom_text() + coord_flip() + theme_bw() + labs()
```

</details>

<details>
<summary>🕷️ 3. Web Scraping: Raspagem de Dados de Pacotes do CRAN</summary>

```r
if(!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(rvest, lubridate, dplyr, stringr, ggrepel, ggplot2)

url <- "[https://cran.r-project.org/web/packages/available_packages_by_date.html](https://cran.r-project.org/web/packages/available_packages_by_date.html)"
page <- read_html(url)

packages_data <- page |> html_node("table") |> html_nodes("tr") |> html_nodes("td") |> html_text() |> matrix(ncol = 3, byrow = TRUE)

pacotes_r <- as.data.frame(packages_data, stringsAsFactors = FALSE)
colnames(pacotes_r) <- c("data","nome_pacote","descricao")
pacotes_r$data <- as.Date(pacotes_r$data, format = "%Y-%m-%d")
pacotes_r$ano <- year(pacotes_r$data)
pacotes_r$descricao <- tolower(pacotes_r$descricao)

# Filtro por tema (ex: cluster)
pacotes_r$tema <- str_detect(pacotes_r$descricao,"cluster")
df_tematica <- pacotes_r |> filter(tema == "TRUE") |> arrange(data) |> dplyr::select(-tema)

# Visualização da série histórica
df_tematica |>  
  group_by(ano) |> summarise(n_pacotes=n()) |> mutate(ano=as.numeric(ano)) |>  
  ggplot(aes(ano,n_pacotes)) + geom_line(size = 3, col = "blue") + geom_point(size = 25, alpha = .25, col = "blue") + theme_bw()
```

</details>

<details>
<summary>👤 4. Identificação de Gênero a partir do Nome (IBGE/genderBR)</summary>

```r
install.packages("genderBR")
library(genderBR)

nome <- c("PEDRO","AUGUSTO","AMANDA","KLEITON","GEOVANA")
idade <- c(18,19,20,30,25)

bse <- as.data.frame(cbind(nome,idade))
bse$sexo <- get_gender(bse$nome) # Busca na base do IBGE via probabilidade
View(bse)
```
</details>

<details>
<summary>♈ 5. Manipulação de Datas: Cálculo do Signo do Zodíaco</summary>

```r
if(!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(readxl, DescTools, dplyr, lubridate, ggplot2, forcats, rio)

base_signos <- read_excel("Teste_signos.xlsx") |> mutate(data_nascimento = make_date(Ano,Mês,Dia))
base_signos$signo <- Zodiac(base_signos$data_nascimento)

base_signos <- base_signos |> 
  mutate(signo = case_when(
    signo == "Virgo" ~ "Virgem", signo == "Capricorn" ~"Capricórnio",
    signo == "Leo" ~"Leão", signo == "Pisces" ~"Peixe",
    signo == "Aquarius" ~"Aquário", signo == "Cancer" ~ "Câncer",
    signo == "Libra" ~"Libra", signo == "Scorpio" ~"Escorpião",
    signo == "Taurus" ~"Touro", signo == "Sagittarius" ~"Sagitário",
    signo == "Aries" ~"Áries", signo == "Gemini" ~"Gemêos"
  ))

df_agg <- base_signos |> group_by(signo) |> summarise(n_casos=n())
df_agg$perc <- round(df_agg$n_casos/sum(df_agg$n_casos)*100, 1)

df_agg |> mutate(signo = fct_reorder(signo,n_casos)) |> 
  ggplot(aes(signo,perc)) + geom_col(col = "white", fill = "green", alpha =.7) + theme_minimal()
```

</details>
<details>
<summary> 📱 6. Geração de QR Code no R</summary>

```r
install.packages("qrcode")
library(qrcode)

code <- qr_code("link de interesse")
plot(code)
generate_svg(code, filename = "pasta de destino/qr.svg")
```
</details>
<details> 
<summary> 📊 7. Análise de Outliers: O Quarteto de Anscombe</summary>  

```r
if(!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(fBasics, datasets, ggpubr, ggplot2)  

anscombe <- datasets::anscombe

g1 <- anscombe |> ggplot(aes(x1,y1)) + geom_point(size=5, alpha=.5, col="darkorange") + theme_bw() + geom_smooth(method = "lm",alpha=.1) + stat_cor(method = "pearson",label.x = 3,label.y = 10,size=7)
g2 <- anscombe |> ggplot(aes(x2,y2)) + geom_point(size=5, alpha=.5, col="darkorange") + theme_bw() + geom_smooth(method = "lm",alpha=.1) + stat_cor(method = "pearson",label.x = 3,label.y = 10,size=7)
# (Repete para g3 e g4)
ggarrange(g1,g2,g3,g4)
# Demonstra que distribuições com o mesmo R podem ter correlações visuais totalmente diferentes.
```

</details>

<details>
<summary>🎸 8. Identificação e Destaque de Outliers em Gráficos (Simulação Iron Maiden)</summary>

```r 
if(!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(ggplot2, ggrepel, dplyr)

set.seed(123)
y <- rnorm(n=665, mean = 200, sd=5)

# Cria um outlier 6.6 desvios padrões abaixo da média
Outlier <- mean(y) - 6.6 * sd(y)
y <- c(y,Outlier)
df <- data.frame(y)
df$id <- c(1:666)

df <- df |> mutate(Outlier = ifelse(id %in% c("666"), T, F))

ggplot(df, aes(id,y)) + geom_point() + theme_bw() + 
  geom_label_repel(data = filter(df, Outlier == T), aes(label = id), size =10) +
  geom_point(data = . |> filter(id == "666"), size =11, shape = 19, fill = "red", color = "red", alpha = .5)
```

</details>

<details>
<summary>📈 9. Representação Gráfica de Hipótese de Trabalho</summary>

```r
if(!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(ggstatsplot, ggplot2, dplyr)

set.seed(123)
x <- rnorm(100)
y <- -0.11 * x + rnorm(100, mean = 0, sd= 0.2) 

data <- data.frame(x,y)
data |>  
  ggplot(aes(x,y)) + geom_point(size=5, col="black", fill="red", shape=21) + theme_bw() + geom_smooth(method = "lm", alpha=.2) + labs(x = "Escolaridade", y= "Renda", title = "Representação de uma hipótese de trabalho")
```

</details>
<details> 
<summary>📉 10. Identificação de Tipos de Distribuição (fitdistrplus)</summary>

```r
if(!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(fitdistrplus)

set.seed(123)
n <- 1000

# Geração de diversas distribuições para teste
x1 <- rnorm(n,mean = 10,sd=2)
x2 <- runif(n,min = 0,max = 1)
x3 <- rexp(n,rate = 0.5)

descdist(x1, discrete = FALSE) # Normal
descdist(x2, discrete = FALSE) # Uniforme
descdist(x3, discrete = FALSE) # Exponencial
```
</details>
<details>  
<summary> 💰 11. Desigualdade: Cálculo do Coeficiente de Gini e Curva de Lorenz</summary>

```r
if(!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, ggplot2, ineq, scales, tibble)

set.seed(123)
n <- 1000
renda <- rlnorm(n, meanlog = 8, sdlog=0.8)
dados <- tibble(id = 1:n, renda=renda)  

# Cálculo de Gini (ineq)
gini <- ineq::Gini(dados$renda)

# Curva de Lorenz
lorenz <- ineq::Lc(dados$renda)
lorenz_df <- data.frame(p = lorenz$p, L = lorenz$L)

ggplot(lorenz_df, aes(x=p, y=L)) + geom_line(size=1.2) + geom_abline(linetype="dashed") + theme_minimal() 
```










