

#Dados de golpe de estado
#GRÁFICO ANIMADO QUE INCLUI A TENDENCIA DA SÉRIE QUE ESTÁ SENDO EXAMINADA

if(!require("pacman")) install.packages("pacman")
library(pacman)
p_load(gganimate,ggplot2,ggtext  #títulos com HTML/Markdown
       ,readr, dplyr, xml2)


#Base de dados 
coups <- read.csv("base de dados em csv")

#Agora vamos preparar os dados aqui 


tot_ano_realized <- coups |>  
  group_by(year) |> 
  summarise(tot_realized = sum(realized, na.rm = TRUE),
            .groups = "drop")
#Feito o gráfico
tot_ano_realized |>  
  ggplot(aes(year,tot_realized)) +  geom_line() + theme_bw() + labs(x= "", y="Números de golpes consumados", 
           title = "Número de golpes consumados por ano", subtitle = "1945-2024"                                                       )
#agora vamos fazer a tendencia do gráfico

tot_ano_realized_com_linhacor <-   coups |>  
  group_by(year) |>  
  summarise(tot_realized = sum(realized, 
                               na.rm = TRUE),
            .groups = "drop") |> 
  mutate(lm = predict(lm(tot_realized ~ year,
                         data = .),
                      newdata distinct(.,year)))
 
#o código adiciona uma coluna com a previsão do valor 

#agora o gráfico animado 

r <- ggplot(tot_ano_realized_com_linhacor, aes(year,tot_realized, group = 1)) + #reta da regressão mais grossa e azul-escura 
  geom_segment(aes(year[1],xend = year,
                   y= lm[1],yend = lm),
               size =1.5, #espessura maior 
               color = "green", 
               alpha = .3) + 
  theme_bw() + 
  
  scale_x_continuous(breaks = c(seq(1945,2020), by = 5), 2024),limits = c(1945,2024)) +
  transition_reveal(year) + 
  ease_aes("linear")
               

#tenho duas animações ao mesmo tempo, a animação da reta do coeficiente de regressão e a do gráfico 
  
anime_save() #ele salva o gráfico em formato de gif no projeto de R 


#se antes era realizados agora vamos ver os tentados

tot_ano_attempt <-   coups |>  
  group_by(year) |>  
  summarise(tot_attempt = sum(attempt, 
                               na.rm = TRUE),
            .groups = "drop") |> 
  mutate(lm = predict(lm(tot_realized ~ year,
                         data = .),
                      newdata distinct(.,year)))




t <- ggplot(tot_ano_attempt, aes(year,tot_attempt, group = 1)) + #reta da regressão mais grossa e azul-escura 
  geom_segment(aes(year[1],xend = year,
                   y= lm[1],yend = lm),
               size =1.5, #espessura maior 
               color = "green", 
               alpha = .3) + 
  theme_bw() + 
  
  scale_x_continuous(breaks = c(seq(1945,2020), by = 5), 2024),limits = c(1945,2024)) +
  transition_reveal(year) + 
  ease_aes("linear")

#pronto agora tem o dos golpes tentados
#tem também o com os golpes conspirados também 

