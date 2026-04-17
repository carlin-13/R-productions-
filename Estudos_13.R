--------

install.packages("rethnicity") #Pacote identifica a raça/cor a partir do nome da pessoa, atraves da distribuição de probabilidade do nome da pessoa seguindo a lógica de registros que são acumulados e pega a probabilidade de estar contido

library(rethnicity)

#Testes de nomes 


predict_ethnicity(firstnames = "Mirella", lastnames = "Marchini")

predict_ethnicity(firstnames = "Taylor", lastnames = "Swift")

predict_ethnicity(firstnames = "João", lastnames = "Aguiar")

predict_ethnicity(firstnames = "Whindersson", lastnames = "Nunes")

predict_ethnicity(firstnames = "Carlos", lastnames = "Arthur")

predict_ethnicity(firstnames = "Carlos", lastnames = "Carvalho")

predict_ethnicity(firstnames = "Carlos", lastnames = "Canto")

predict_ethnicity(firstnames = "Carlos Arthur", lastnames = "Carvalho Canto ")

predict_ethnicity(firstnames = "Danielle", lastnames = "Oliveira")

predict_ethnicity(firstnames = "Danielle", lastnames = "Carvalho")

predict_ethnicity(firstnames = "Arthur", lastnames = "Cardoso")

predict_ethnicity(firstnames = "Pedro", lastnames = "Gomes")

predict_ethnicity(firstnames = "João", lastnames = "Neto")

predict_ethnicity(firstnames = "Paolo", lastnames = "Storto")

predict_ethnicity(firstnames = "Percival", lastnames = "Canto")

predict_ethnicity(firstnames = "Nathália", lastnames = "Bender")

predict_ethnicity(firstnames = "Josyanne", lastnames = "Soares")

predict_ethnicity(firstnames = "Mirella", lastnames = "Marchini")

predict_ethnicity(firstnames = "Reginaldo", lastnames = "Kawa")

#artigo citado que é bom para usar de repertorio para ver qual seriam mais empregaveis quando se usa um nome mais recorrente de raça diferente 
#https://www.nber.org/papers/w9873 (LINK DO ARTIGO)






