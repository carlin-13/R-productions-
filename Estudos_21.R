
 
  
  # Dentro da election forensics, pode ser várias anomalias: fraude, erro de digitação, catástrofes, erro de importação... e bláblá
  # Em que estamos fazendo isso para que os rastros de que se a pessoa falar que não é possível confiar nos dados do TSE e para não deixar rastros tem que ser bem preciso pois deveria fazer algo com os rastros e deixar a distribuição e as estatísticas iguais
  
  # Lei de Benford do 2º Dígito - Estudo feito com Walter Mebane que é um cientista político e matemático que estuda essa lei mas não foi ele que criou a lei e sim Benford
  
  # Material da USaid é do Walter Mebane que é sobre a questão da fraude e tal 
  
  # 2BL é a lei de benford do segundo digito e que o valor esperado é o 4.187
  # Mebane pesquisa há muito tempo essa questão e com consultorias internacionais com vários países nessa questão da fraude 
  # fingerprintanalysis
  # pegar dados por município é melhor do q por estado
  # os melhores são os dados por sessão e por zona
  # sempre comparar o que foi encontrado com o número de referência 4.187
  # outra forma é vendo as frequências comparando elas as frequências que são esperadas
  # não existem indícios de manipulação dos dados do TSE
  
  
  # Pacotes
  if(!require("pacman")) install.packages("pacman") 
  library(pacman)
  p_load(dplyr, ggplot2, patchwork, ggtext, readxl, janitor, freqdist, stringr, tidyr)  
  
  
  # Dados
  options(scipen = 999) # remove notação científica
  
  # Base direto do TSE que é do nexo - 1º Turno
  df_t1 <- read_excel("raw-data/votos_presidente_muni_nexojornal_2022.xlsx",
                      sheet = "absoluto-1t-2022")
  
  # Base direto do TSE que é do nexo - 2º Turno
  df_t2 <- read_excel("raw-data/votos_presidente_muni_nexojornal_2022.xlsx",
                      sheet = "absoluto-2t-2022")
  
  
  # Variáveis - 1º Turno 
  df_t1 <- clean_names(df_t1)
  
  df_t1$turnout <- df_t1$comparecimento / df_t1$eleitores
  df_t1$votacao_lula <- df_t1$x13 / df_t1$validos
  df_t1$votacao_bolsonaro <- df_t1$x22 / df_t1$validos
  df_t1$votacao_tebet <- df_t1$x15 / df_t1$validos
  df_t1$votacao_ciro <- df_t1$x12 / df_t1$validos
  df_t1$votacao_soraia <- df_t1$x44 / df_t1$validos
  
  # Removendo exterior (ZZ) - 1º Turno
  df_t1 <- df_t1 %>% filter(uf != "ZZ")
  
  
  # Variáveis - 2º Turno 
  df_t2 <- clean_names(df_t2)
  
  df_t2$turnout <- df_t2$comparecimento / df_t2$eleitores
  df_t2$votacao_lula <- df_t2$x13 / df_t2$validos
  df_t2$votacao_bolsonaro <- df_t2$x22 / df_t2$validos
  
  # Removendo exterior (ZZ) - 2º Turno
  df_t2 <- df_t2 %>% filter(uf != "ZZ")
  
  
  # Extração do 2º Dígito (2BL) - 1º Turno 
  df_t1_d2 <- df_t1 %>%
    select(eleitores, comparecimento, abstencoes, validos, nulos, brancos, 
           x13, x22, x15, x12, x44) %>%
    mutate(
      across(
        everything(),
        ~ ifelse(nchar(as.character(.x)) >= 2,
                 as.numeric(str_sub(as.character(.x), 2, 2)),
                 NA_real_),
        .names = "{.col}_d2"
      )
    )
  
  # Extração do 2º Dígito (2BL) - 2º Turno 
  df_t2_d2 <- df_t2 %>%
    select(eleitores, comparecimento, abstencoes, validos, nulos, brancos, 
           x13, x22) %>%
    mutate(
      across(
        everything(),
        ~ ifelse(nchar(as.character(.x)) >= 2,
                 as.numeric(str_sub(as.character(.x), 2, 2)),
                 NA_real_),
        .names = "{.col}_d2"
      )
    )
  
  
  # Análises e Médias (Comparação com Referência 4.187)
  
  #  1º Turno 
  # Visualização Rápida
  df_t1_d2 %>% select(x13, x13_d2, x22, x22_d2) %>% head()
  
  # Médias
  round(mean(df_t1_d2$x13_d2, na.rm = T), 3)
  round(mean(df_t1_d2$x22_d2, na.rm = T), 3)
  
  # Distribuição de Frequências
  freqdist(df_t1_d2$x13_d2)
  freqdist(df_t1_d2$x22_d2)
  
  # Resumo de todas as médias do 1º Turno
  media_t1_d2_todas <- df_t1_d2 %>%
    summarise(
      across(ends_with("_d2"), ~ mean(.x, na.rm = TRUE))
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variavel",
      values_to = "media_d2_1t"
    )
  
  # 2º Turno
  # Visualização Rápida
  df_t2_d2 %>% select(x13, x13_d2, x22, x22_d2) %>% head()
  
  # Médias
  round(mean(df_t2_d2$x13_d2, na.rm = T), 3)
  round(mean(df_t2_d2$x22_d2, na.rm = T), 3)
  
  # Distribuição de Frequências
  freqdist(df_t2_d2$x13_d2)
  freqdist(df_t2_d2$x22_d2)
  
  # Resumo de todas as médias do 2º Turno
  media_t2_d2_todas <- df_t2_d2 %>%
    summarise(
      across(ends_with("_d2"), ~ mean(.x, na.rm = TRUE))
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variavel",
      values_to = "media_d2_2t"
    )
  
  # Opcional: Juntar as tabelas de resumo para comparar os turnos lado a lado
  resumo_comparativo <- full_join(media_t1_d2_todas, media_t2_d2_todas, by = "variavel")
  print(resumo_comparativo)
  
  
  #no primeiro turnoe no segundo turno deu tudo certo , um único teste não prova nada só mostra uma face do acontecido









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

  
  
  
