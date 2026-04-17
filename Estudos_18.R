
  
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
  
