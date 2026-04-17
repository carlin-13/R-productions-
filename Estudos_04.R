


#poderia usar o pacote pacmnan também 
install.packages("genderBR")
library(genderBR)

#crio objetos para cria a base 
nome <-  (c("PEDRO","AUGUSTO","AMANDA","KLEITON","GEOVANA"))
idade <-  (c(18,19,20,30,25))

#faz um data frame desses dois objetos
bse <- as.data.frame(cbind(nome,idade))
#dá uma olhada
View(bse)
#cria uma nova coluna com o genero dos nomes
#como que ele sabe o sexo? esse pacote busca lá na base do IBGE os nomes e ve se é home ou muié
#ele busca lá e por meio da probabilidae ele joga

bse$sexo  <-  get_gender(bse$nome)
#e agora na nossa base ele aparece com o sexo 
View(bse)

#posso ter nome é sobrenome e não muda nada 

nome2 <- c("tauane matias", "caio batista")

nome3 <-  get_gender(nome2)
                        
#por fim ele so ve pela probabilidade e não entra tão a fundo no nome


#se for procurar por nomes mais estranhos ele vai pela probabilidade como por exemplo 
# COM "T" ELE DÁ A PROBABILIDADE E COM "F" ELE DÁ QUAL É O SEXO DE FATO 
  get_gender("João", prob = F)
  
  
