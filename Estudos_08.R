if(!require("pacman")) install.packages("pacman")
  library(pacman)
  #carregar pacotes 
  pacman:: p_load(ggstatsplot,ggplot2,dplyr)

  set.seed(123)
  
  x <- rnorm(100)
  y <- -0.11 * x + rnorm(100,mean = 0, sd= 0.2)  #quando eu mudar o valor termo que multiplica com x, vai mudar o valor da correlação
  #testando com uma correlação positiva com o multiplicador positivo 
  #testaando com uma correlação negativa com multiplicar negativo 
  
  data <- data.frame(x,y)
  data |>  
    ggplot(aes(x,y))+ 
    geom_point(size =5,
               col= "black",
               fill="red",
               shape=21) + 
  theme_bw() + geom_smooth(method = "lm", alpha=.2) + 
    labs(x = "Escolaridade",
         y= "Renda",
         title = "Representação de uma hipótese de trabalho") + 
    theme(text = element_text(size = 20))
  #de forma geral 
  
  
  #variavel de grupo 
  #como fazer com a diferença entre grupos 
  # criação dos grupos 
  g1 <- rnorm(100, mean = .9,sd = 1)
  g2<- rnorm(100, mean = 25,sd = 3)
  g3<- rnorm(100, mean = 20,sd = 3)
  g4<- rnorm(100, mean = .2,sd = .7)
#cria a base de dados com todos eles 
  df <-  data.frame(y=c(g1,g2,g3,g4),
                    grupo = rep(c("Brancos", "negros","pardos",
                                  "Amarelos"), each=100))
  
  
  #NÃO DEU CERTO DE USAR O GGSTATSPLOT, NÃO DEU CERTO 
  #ELE USA A ggbetweenstats como função pra gerar
  
  ggbetweenstats(df,grupo, y, centrality,label.args= list(size=5)) +
    labs(title = "iso",subtitle = "", x= "",
         y="") + theme(text = element_text(size = 20))
  
  #se eu for fazer uma simulação com apenas 2 grupos - Pesquisa Clínica ou etc
  
  g1 <- rnorm(100, mean = 40,sd = 1)  #Experimental
  g2<- rnorm(100, mean = 10,sd = 4)   #Controle meu 
  
  
