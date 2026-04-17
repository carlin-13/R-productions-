
#Aprender a testar os pressupostos do modelo de regressão 

if(!require("pacman")) install.packages("pacman")
library(pacman)

pacman:: p_load(
  performance, 
  dplyr,
  ggplot2
)

set.seed(123)  # Para reprodutibilidade

options()
??options
#Tamanho da amostra 

n <-  1000

#Gerando preditores 

x1 <- rnorm(n, mean = 5, sd=2)
x2 <- rnorm(n, mean = 3, sd=1)
x3 <- rnorm(n, mean = 0, sd=1)
x4 <- rnorm(n, mean = -2, sd=1)


y <-2*x1 + 1.5*x2 + 0.1*x3 - 2.5*x4 + rnorm(n)    #a variavel com menor impacto é x3 e a com maior é x4 por ser a maior e ser negativo, e o rnorm(n) é o erro

#criando o data frame 

data <- data_frame(y,x1,x2,x3,x4)

#dados criados agora mão na massa da regressão linear 

modelo1 <- lm(y ~ x1+x2+x3+x4 , data = data)
summary(modelo1)

#os modelos estão próximos dos que foram estipulados, com modelos simulados é fácil NA VIDA REAL É DIFERNTE 

#agora que foi estipulado iremos ver no pacote performance:
#valores e signifcancia dos coeficientes, ou seja, ele vai mostrar estatisticas de ajuste

performance:: performance(modelo1)

performance::check_collinearity(modelo1)

performance::check_normality(modelo1)

performance::check_outliers(modelo1)  #modelo de regressão não pode ter nenhum outlier muito danoso

performance::check_heteroscedasticity(modelo1)  #para ver se a diferença nas variaâncias

performance::check_model(modelo1)

#Vamos visualizar os resultados graficamente

install.packages("sjPlot")
library(sjPlot)

plot <- sjPlot:: plot_model(modelo1,show.intercept = T, ci.lvl = 0.95) +
  theme_bw()
  
plot(plot)

plot + geom_hline(yintercept = 0,linetype = "bold",color= "green",size=2,alpha=.5)


