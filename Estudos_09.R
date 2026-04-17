

 #Relacionado ao direito
 #jurimetria em que voce usa a estatistica para analisar os fenomenos de direito 
 
 #Jurimetria: juntar ciencia e direito 
 
 
 install.packages("abjData")
 library(abjData)       #pacote de dados da associação brasileira de jurimetria(abj), criada pra começar a difundir essas ideias 
 
 data("assuntos")  # para carregar a base 
 
 View(assuntos)
 
 names(assuntos)  # para saber o nome das variáveis 
 
 ncol(assuntos)    # para saber o número de colunas 
 
 nrow(assuntos)   #quantas linhas eu tenho na base 
 
 str(assuntos)   #tipo das variaveis 
  
 class(assuntos)  #qual a classe do objeot 
 
head(assuntos)    #primeiras linhas 

tail(assuntos)    #últimas linhas 

summary(assuntos)  #estatisticas descritivas básicas 



#Estatística Descritiva Básica 

mean(assuntos$total)

median(assuntos$total)   #ela não é afetada por outliers, mais robusta a outliers

min(assuntos$total)    #valor minimo 

max(assuntos$total)   #valor maximo 

sd(assuntos$total)   #para ver quao homogenea ou quao heterogenea é a distribuição 

range(assuntos$total)   #o espectro do menor valor para o maior valor(max - min)  amplitude 

summary(assuntos$total) #útil para variaveis quantitativas 


#Exemplo 2 

data("leiloes")   #pegar a base no ambiente 

View(leiloes)     #para ver a base 
  
dim(leiloes)  #retorna o numero de linhas e colunas 

table(leiloes$vendeu)
prop.table(table(leiloes$vendeu))

mean()   #não faz sentido pegar media de varivel qualitativa 

library(freqdist)

freqdist(leiloes$vendeu)

leiloes$dif <- leiloes$valor_total_arrematado - leiloes$valor_avaliacao_inicial

mean(leiloes$dif)

mean(leiloes$dif,na.rm = T)

min(leiloes$dif,na.rm = T)
 
max(leiloes$dif,na.rm = T) 

leiloes$ratio <- leiloes$valor_total_arrematado/leiloes$valor_avaliacao_inicial 

mean(leiloes$ratio, na.rm = T)
 
median(leiloes$ratio, na.rm = T) 
 
 
