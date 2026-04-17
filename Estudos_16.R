
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

