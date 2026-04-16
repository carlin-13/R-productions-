# R-productions-
Aqui dentro contém minhas produções utilizando Rstudio 


## 📚 Laboratório de R: Projetos & Estudos (Datafobia)

Abaixo estão diversos scripts e projetos desenvolvidos em **R**, baseados nos estudos de análise de dados, estatística e jurimetria do canal Datafobia. Clique nas setas para expandir e visualizar o código de cada projeto.

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
