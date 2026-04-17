

#equação de heckman é aquela equação que é o efeito do que que acontece com a produtividade quando se investe em educação básica, quanto mais cedo se investe no individuo maior a taxa de retorno

install.packages("ggplot2")
library(ggplot2)

#Simular base de dados
set.seed(123)
age <- seq(0,30,by=0.1)
return_rate <-  0.6 *exp(-0.1*age) #curva exponencial decrescente 

#criar df
df <- data.frame(age = age, return_rate= return_rate)

#Definir as regiões analisadas, pré escola, escoal e pós escola 
df$stage <-  
  cut(df$age,
      breaks = c(-Inf,5,18,Inf),
      labels = c("Preschool","School","Post-school")
      )


#Linha do custo de oportunidade

ggplot(df, aes(x=age,y=return_rate)) + 
  geom_ribbon(aes(ymin = 0,ymax = return_rate, fill = stage),alpha = 0.3) + 
  geom_line(color = "red", size=1.2)+
  geom_hline(yintercept = r_line,linetype="dashed") + 
  #annotate eu consigo escrever dentro do gráfico 
  annotate("text",x=2.2,y=0.3,label="Programas do Prézinho",color="orange",size=8) + 
  annotate("text",x=8.5,y=0.03,label="Programas na Escola",color="pink",size=8) + 
  annotate("text",x=2.2,y=0.3,label="Programas Pós Escol",color="blue",size=8) + 
  scale_fill_manual(values= c("Preschool"="gold","School"="skyblue","Post-school"="lightgreen")) +
labs(x= "Idade", y="taxa de retorno",
     title = "Taxa de retorno de investimento em capital humano",
     caption = "fonte: eu")
+theme_minimal() +
  theme_minimal(base_size=25)+
  theme(legend.title = element_blank())
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              




