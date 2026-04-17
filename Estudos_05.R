
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
 
    
