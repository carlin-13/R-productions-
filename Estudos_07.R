#Como criar um gráfico de outliers 

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
  
  
  
  
