
  if(!require("pacman")) install.packages("pacman")
  library(pacman)
  pacman::p_load(fitdistrplus)

#pacote fitdistrplus é um pacote que vê qual que o tipo de distribuição que você está trabalhando e ve qual o modelo mais adequado para trabalhar com aquilo  

  #iremos simular uma base de dados 
  
  set.seed(123)

  #determinar o tamanho da amostra 
  
  n <- 1000
  
  #geraremos varias distribuições 
  
  #Normal
  
  x1 <- rnorm(n,mean = 10,sd=2)
  
  #Uniforme
  
  x2 <- runif(n,min = 0,max = 1)
  
  #Exponencial
  
  x3 <- rexp(n,rate = 0.5)
  
  #Logística
  x4 <- rlogis(n,location = 0,scale = 1)  #para area de pesquisa
  
  #Beta
  x5 <- rbeta(n,shape1=2,shape2 = 5)   #frnacisco kibari que faz essa regressão beta
  
  #Lognormal
  x6 <- rlnorm(n,meanlog = 1,sdlog=0.5)
  
  #Gamma
  x7 <- rgamma(n,shape=3,scale=2)
  
 #agora juntaremos todas em um df 
  
  
  
  df <-  data.frame(c(x1,x2,x3,x4,x5,x6,x7))   #feita as variaveis
  
  #agora para testar iremos visualizar com 
  
  descdist(x1,discrete = FALSE) #NORMAL
  #OUTRA FORMA DE VER E FAZENDO UM HISTOGRAMA
  hist(x1)
  
  descdist(x2,discrete = FALSE) #UNIFORME
  #idem
  hist(x2)
  
  descdist(x3,discrete = FALSE) #EXPONENCIAL
  hist(x3)
  
  descdist(x4,discrete = FALSE) #LOGISTICA
  hist(x4)
  
  descdist(x5,discrete = FALSE) #BETA
  hist(x5)
  
  descdist(x6,discrete = FALSE) #LOGNORMAL
  hist(x6)
  
  descdist(x7,discrete = FALSE) #GAMMA
  hist(x7)
  
  
  #ELE MOSTRA A CURTOSE E A RAIZ DA SIMETRIA
  #E MOSTRA QUAL QUE É A DISTRIBUIÇÃO TEÓRICA ESPERADA, OU SEJA, AQUELA QUE MELHOR SE ENCAIXA
  #PROCURA ONDE O VERMELHO ESTÁ MAIS PRÓXIMO
  
  
