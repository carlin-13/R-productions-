
if(!require("pacman")) install.packages("pacman")
library(pacman)
pacman :: p_load(readr               #lê arquivos e formatos 
)
options(scipen = 999)  #evitsa notações cientificas nas estimativas 

#usa longitude e latitude para calcular as distancias

df <-  read_delim("latitude-longitude-cidades.csv",
                  delim = ";",
                  escape_double = FALSE, 
                  trim_ws = TRUE
)

save(df, file = "df.rda")    #para salvar em formato do R 

#ou carregar direto a base em r 
load("df.rda")


#forma de ver 
names(df)

#agora para calcular as distancas usa o pacote geosphere

p_load(geosphere)

#escolher a cidade 
recife_cords <-  c(longitude = -34.87707, latitude = -8.046658) #objeto com as informações 


#fazer uma df que pega todas as distancias em comparação com a cidade analisada

df$distancia |> distGeo( #distGeo calcula em metros por isso no fim se tem 1000 para os metros virarem kms 
  matrix(c(df$longitude, df$latitude ), ncol=2),
  recife_cords
) /1000

#exibir distancias 
df$distancia_para_recife_kms

#para outras cidades é só usar o exemplo e colocar outros lugares


#tem como fazer isso em mapa

ggplot(df,aes(x=longitude, y=latitude),
       color = distancia_para_a_cidade) + 
  scale_color_viridis_c() +
  
  labs() + theme_void()
