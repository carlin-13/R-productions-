

  
 #TAMBÉM CONHECIDO COMO ÍNDICE DE GINI - QUE CALCULA DE FORMA GERAL A CONCENTRAÇÃO DE RENDA (PODE SER QUALQUER CONCETRAÇÃO, EX:RESTAURANTES, LOJAS...)
  #UTILIZANDO TAMBÉM A CURVA DE LORENZ 
   if(!require("pacman")) install.packages("pacman")
  library(pacman)
  pacman::p_load(dplyr #MANIPULAR DADOS
                 ,ggplot2,ineq # AQUI QUE TEM A FUNÇÃO PARA CALCULAR GINI 
                 ,scales #TRABALHAR COM ESCALAS
                 ,tibble #CRIAR DFS E TAL
                  )
  
  set.seed(123)

  #Gerar dados simulados  
  
  n <- 1000
  renda <- rlnorm(n,meanlog = 8,sdlog=0.8)
dados <- tibble(id = 1:n,
                renda=renda
                )  
  
#Cálculo de gini

gini <- ineq::Gini(dados$renda)
gini
#como que interpreto? ela sempre varia entre 0 e 1 , 0 quando todo mundo tem a merma renda e 1 quando só 1 pessoa tem a renda toda
#como se fosse numa pizzaria e voce deveria pegar pelo menos 1 pedaço e se não tivesse nenhum ou voce comesse todos seria algo que essa tal coisa está concentrando aquela pizza 

#exemplo manual para ajudar a visualizar

x1 <- c(1,1,1,1,1,1,1,1,1)
ineq::Gini(x1)


x2 <- c(0,0,0,0,0,0,0,1)
ineq::Gini(x2)
#não dá 1 porque teria de tender ao infinito para todos irem convergirem a ser 1 

#Calculo manual de gini 
#Formula
#G=[2*sum(i*y_1)]\[n*sum(y_1)] -(n+1)\n

#GINI de 1 = desigualdade máxima
#Gini de 0 = igualdade perfeita




#CURVA DE LORENZ - TAMBÉM COM O PACOTE INEQ
lorenz <- ineq::Lc(dados$renda)

#converter para df 
lorenz_df <-  data.frame(
  p= lorenz$p,  #proporção acumulada da população 
  l=lorenz$L    #proporção acumulada da renda 
)

#agora vou plotar 

ggplot(lorenz_df, aes(x=p,y=L)) + 
  geom_line(size=1.2) + 
  geom_abline(linetype="dashed")+ 
  labs(title="",subtitle = "") + theme_minimal()

#quanto menor a barriga do gráfico de gini, então a concentração é menor e quanto maior a barriga do gráfico de gini então maior a concentração - de acordo com a Curva de Lorenz

#sites que tem dados bons 
#IPEA
##https://www.ipea.gov.br/desafios/index.php?option=com_content&id=2048%3Acatid%3D28
#GINI
#https://www.ipea.gov.br/desafios/index.php?option=com_content&id=2048%3Acatid%3D28
#BANCO MUNDIAL
#https://data.worldbank.org/indicator/SI.POV.GINI
