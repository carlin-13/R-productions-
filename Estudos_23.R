 
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
