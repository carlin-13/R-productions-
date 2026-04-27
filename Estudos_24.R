
 #Primeiro de tudo instalação dos pacotes 
 
 if(!require("pacman")) install.packages("pacman")
 library(pacman)
 
 pacman::p_load(
   easyScieloPack, # Pacote principal para raspar os dados do SciELO
   stringr,        # Manipulação de strings e textos
   dplyr,          # Manipulação e transformação de dados
   writexl,        # Exportação de planilhas para Excel
   ggplot2,        # Criação de gráficos
   ggtext,         # Edição avançada de textos em gráficos
   ggpubr,         # Junção de gráficos e estatísticas
   ggrepel         # Repelir rótulos nos gráficos para não sobrepor
 )
 
 
 # Nota: A função principal de busca do pacote pode variar ligeiramente de nome 
 # dependendo da versão (ex: search_scielo, get_scielo ou scielo_search). 
 # Aqui utilizaremos `search_scielo` como base padrão.
 

 #tá dando ruim então vou fazer 
 message("Iniciando teste isolado do pacote easyScieloPack...")
 
 teste_pacote <- tryCatch({
   
   # Busca um termo genérico com limite baixo só para ver se o pacote se comunica com o SciELO
   search_scielo("Brasil", n = 5)
   
 }, error = function(e) {
   
   message("Erro capturado no teste: ", e$message)
   return("PACOTE_QUEBRADO")
   
 })
 
 if(identical(teste_pacote, "PACOTE_QUEBRADO")) {
   message("\nRESULTADO DO TESTE: O pacote falhou. O SciELO provavelmente mudou a estrutura do site e o easyScieloPack precisa de atualização.")
   # Se quiser que o script pare de rodar automaticamente caso o pacote esteja quebrado, descomente a linha abaixo:
   # stop("Execução interrompida: Pacote easyScieloPack com falha de comunicação.")
 } else {
   message("\nRESULTADO DO TESTE: O pacote funcionou perfeitamente! Seguindo com o script principal...\n")
 }
 
 
 
 # 2. Buscas básicas e limites de até onde pesquisar 
 
 # Busca livre sem restrições (pode deixar normal se não estiver dando erro nela)
 # ATENÇÃO: Se esta linha der erro, comente-a com um '#' e use apenas a busca com limite abaixo.
 df_emendas <- search_scielo("Emendas parlamentares")
 View(df_emendas)
 
 # Aplicando o tryCatch na busca que tem limite de 'n', para não quebrar com o erro TRUE/FALSE
 df_emendas_100 <- tryCatch({
   
   message("Buscando artigos principais no SciELO...")
   search_scielo("Emendas parlamentares", n = 100)
   
 }, error = function(e) {
   
   message("Ocorreu um erro interno no pacote/SciELO: ", e$message)
   return(NULL) # Se der pau, ele retorna NULL em vez de parar o seu script
   
 })
 
 
 # Só roda a limpeza de dados e o gráfico SE a busca deu certo e a base existe
 if (!is.null(df_emendas_100)) {
   
   # 4. Curadoria e Filtros Específicos com Dplyr
   df_tematica <- df_emendas_100 %>%
     mutate(titulo_min = tolower(title)) %>%
     mutate(tema = str_detect(titulo_min, "emendas")) %>%
     filter(tema == TRUE) %>%
     arrange(year) %>%
     select(-tema)
   
   # 5. Agrupamento
   df_agregado_ano <- df_tematica %>%
     group_by(year) %>%
     summarise(n_casos = n()) %>%
     mutate(year = as.numeric(year))
   
   # Gerando o gráfico da evolução temporal das publicações
   grafico <- ggplot(df_agregado_ano, aes(x = year, y = n_casos)) +
     geom_line(color = "#0073C2FF", linewidth = 1) +
     geom_point(color = "#EFC000FF", size = 3) +
     theme_minimal() +
     labs(
       title = "Evolução de publicações sobre Emendas Parlamentares no SciELO",
       x = "Ano de Publicação", 
       y = "Quantidade de Trabalhos"
     ) +
     theme(plot.title = element_text(face = "italic"))
   
   # Como estamos dentro de um 'if', usamos o print() para forçar o gráfico a aparecer
   print(grafico)
   
 } else {
   message("A raspagem falhou e não retornou dados. Tente rodar de novo mais tarde.")
 }
 
 
 # 6. Funcionalidades Extras: Filtros por Idioma, Período e Revista
 # Se for automatizar essas buscas depois, você pode aplicar o mesmo tryCatch nelas!
 
 df_bayesian <- tryCatch({ search_scielo("bayesian", language = "en") }, error = function(e) NULL)
 df_slavery <- tryCatch({ search_scielo("slavery", year_start = 2019, year_end = 2025) }, error = function(e) NULL)
 df_diplomacy <- tryCatch({ search_scielo("diplomacy", journal = "Revista Brasileira de Política Internacional") }, error = function(e) NULL)
 
 
 

 
 
