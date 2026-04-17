🔎 1. Buscas Bibliográficas e Nuvem de Palavras (SciELO/CAPES)

if(!require("pacman")) install.packages("pacman")

library(pacman)pacman::p_load(stringr, dplyr, writexl, ggplot2, easyScieloPack, ggtext, ggpubr, ggrepel, arrow, purrr, capesR, usethis)# Busca automatizada no SciELObase_scielo <- search_scielo("Supremo Tribunal Federal", n_max = 1000)base_scielo <- base_scielo |>

mutate(

titulo_min = tolower(title),

tema = stringr::str_detect(titulo_min, "supremo tribunal federal|\\bstf\\b|supremo tribunal brasileiro|corte suprema brasileira| corte constitucional brasileira|surpreme federal court")

)df_tematica_scielo <- base_scielo |> filter(tema == "TRUE") |> arrange(year) |> dplyr::select(-tema)

write.xlsx(df_tematica_scielo, "datafobia1.xlsx")# Busca na CAPES e Nuvem de Palavrascapes <- download_capes_data(c(1987:2022))df_capes <- map_dfr(capes, read_parquet)# ... (Omissão de partes repetitivas para otimização do script) ...pacman::p_load(tidyverse, wordcloud2, tm)scielo_corpus <- VCorpus(VectorSource((df_tematica_scielo)))scielo_corpus <- scielo_corpus |> tm_map(removeNumbers) |> tm_map(removePunctuation) |> tm_map(content_transformer(tolower)) |> tm_map(removeWords, c(stopwords("english"), stopwords("spanish"), stopwords("portuguese")))tdm <- TermDocumentMatrix(scielo_corpus) |> as.matrix()palavras <- sort(rowSums(tdm), decreasing = T)df <- data.frame(palavra = names(palavras), freq = palavras)df$peso <- round(df$freq/sum(df$freq)*100, 2)

wordcloud2(df, size= 0.3, shape= "star", rotateRatio = .5, ellipticity = .10, color = "pink")
