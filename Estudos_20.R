
#Indice de democracia

if(!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, sf #dados espaciais
       ,rnaturalearth #mapa-múndi
       ,rnaturalearthdata,
       countrycode   #conversão de nomes de países para ter merges com codigos e conseguir relacionar paises 
       ,
       plotly #pacote de interatividade
       ,
       ggrepel #rótulos de sobreposição 
      )

#instalação do pacote vdmemdata

install.packages("devtools")
library(devtools)
devtools:: install_github("vdeminstitute/vdemdata")


install.packages("fs")  #tive que baixar esse pacote pois estava dando conflito 
library(fs)
packageVersion("fs")

#tem uma nova biblioteca chamada pak que pega melhor esses dados, já que o fs ira deixarde existir
install.packages("countrycode")




install.packages("pak")
library(pak)

library(vdemdata)

vdem_data <- vdem
#v2x polyarchy é uma medida que mede a democracia em que se vê como é a democracia nos países


#GRAFICO DE SERIE TEMPORAL BRASIL X EUA

#seleciona os países de interesse
paises_interesse <-  c("Brazil", "United States of America")

#Cria uma base apenas com os primeiros e últimos anos de cada pais

# para posicionar os rótulos no gráfico
labels <- vdem_data |>  
  filter(country_name %in% paises_interesse) |>  
  group_by(country_name) |> 
  filter(year == min(year)| year == max(year)) |>  
  ungroup()

#Gráfico de linhas comparando 

gráfico_series <- vdem_data |> 
  filter(country_name %in% paises_interesse) |> 
  ggplot(aes( x= year,
          y= v2x_polyarchy,
          color = country_name)
  ) + 
  geom_line(linewidth = 0.6) + 
  geom_text_repel(data = labels, aes(label = sprintf("%.2f", v2x_polyarchy)),
                  size = 5,
                  show.legend = FALSE) + 
  theme_bw(base_size = 16) + 
  labs(x="",
       y="Índice de Democracia Eleitoral",
       title = "Democracia Eleitoral em perspectiva comparada",
       subtitle="Brasil X Estados Unidos",
       caption = "Fonte: Carlos Arthur(2026)") + 
  
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 22, face ="bold"),
        plot.subtitle = element_text(size= 18),
        axis.title = element_text(size =18),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18))


#exibe o gráfico da democraciaem série temporal

gráfico_series


#Usando o mapa 

vdem_2025 <- vdem |>  # Use 'vdem' (nome padrão do pacote)
  filter(year == 2024) |>  # Note: O dado de 2025 costuma sair em 2026. Use 2024 para teste.
  select(country_name, year, v2x_polyarchy) |> 
  mutate(
    # 1. Cria o código ISO
    iso3c = countrycode(
      sourcevar = country_name,
      origin = "country.name",
      destination = "iso3c"
    ),
    # 2. Ajustes manuais dentro do MESMO mutate
    iso3c = case_when(
      country_name == "United States of America" ~ "USA",
      country_name == "Russia" ~ "RUS",
      country_name == "Venezuela" ~ "VEN",
      country_name == "Bolivia" ~ "BOL",
      country_name == "Iran" ~ "IRN",
      country_name == "Vietnam" ~ "VNM",
      TRUE ~ iso3c
    )
  )



#Base cartógrafica do mundo 

world <-  rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) |>  
  select(admin, iso_a3,geometry   #atenção essa parte de geometria ela tem que ter 
         
         ) |> 
  mutate(
    #Corrige códigos ISO ausentes ou inválidos no shapefile
  iso_a3 = if_else(
    iso_a3  == "-gg",
    countrycode(admin,"country.name","iso3c"),
    iso_a3
  )
    
  
  #Juntar os dados do v-dem com o mapa
  
  
  map_data <- world |> 
    left_join(vdem_2025, by = c("iso_a3" = "iso3c")) |>  
    mutate(
      #texto exibido ao passar o mouse 
      texto_hover = paste0(
        "<b>País:</b> ", admin,
        "<br><b>Ano:</b> ", year,
        "<br><b>Índice:</b> ",sprintf("%.2f", v2x_polyarchy)
      )
    )
  
  
  
  #Mapa Estático 
  
  mapa_estatico <- ggplot(map_data) + 
    geom_sf(
      aes(
        fill = v2x_polyarchy,
        text = texto_hover 
      ),
      color = NA,
      linewidth = 0.25
    ) + 
    scale_fill_gradient2(
      low = "red",
      mid = "yellow",
      high = "green",
      midpoint = 0.5,
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      name = NULL,
      na.value = "grey95"
    ) + 
    coord_sf(expand = FALSE) + 
    labs(
      title = "Democracia Eleitoral (2024)"
    ) +
    # Use o theme_bw() PRIMEIRO
    theme_bw() +
    # Depois customize para remover o que deseja
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      legend.position = "bottom",
      # Remove a grade (linhas de fundo)
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Remove os números dos eixos (latitude/longitude)
      axis.text = element_blank(),
      # Remove os tracinhos dos eixos
      axis.ticks = element_blank(),
      # Remove as bordas do painel se quiser um visual bem limpo
      panel.border = element_blank(),
      # Mantém seu fundo customizado
      panel.background = element_rect(fill = "#f3f3f3", color = NA),
      plot.background = element_rect(fill = "#f3f3f3", color = NA)
    )
  
  
 #mapa
  mapa_estatico
  
  
  #mapa interativo com HOVER 
  
  mapa_estatico_interativo <-  ggplotly(
    mapa_estatico, tooltip = "text"
  )
  
  mapa_estatico_interativo





  
  
  
  
  
#CONFIGURAÇÕES DO MEU GRÁFICO QUE EU POSTEI NO LINKEDIN
  
  # 1. Carregar Bibliotecas
  library(vdemdata)
  library(dplyr)
  library(ggplot2)
  library(countrycode)
  library(rnaturalearth)
  library(ggpubr)
  
  # 2. Função para Processar Dados e Criar o Mapa
  criar_mapa_vdem <- function(ano_selecionado) {
    
    vdem_filtrado <- vdem |> 
      filter(year == ano_selecionado) |> 
      select(country_name, year, v2x_polyarchy) |> 
      mutate(
        iso3c = countrycode(country_name, "country.name", "iso3c",
                            custom_match = c("Kosovo" = "XKX", "Somaliland" = "SML", "Zanzibar" = "EAZ"))
      )
    
    world <- ne_countries(scale = "medium", returnclass = "sf") |> 
      select(admin, iso_a3, geometry) |> 
      mutate(iso_a3 = if_else(iso_a3 == "-99", countrycode(admin, "country.name", "iso3c"), iso_a3))
    
    map_data <- world |> 
      left_join(vdem_filtrado, by = c("iso_a3" = "iso3c"))
    
    p <- ggplot(map_data) +
      geom_sf(aes(fill = v2x_polyarchy), color = NA) +
      scale_fill_gradient2(
        low = "#d73027", mid = "#fee08b", high = "#1a9850",
        midpoint = 0.5, limits = c(0, 1),
        breaks = c(0, 0.5, 1), labels = c("Baixo", "Médio", "Alto"),
        name = "Nível de Democracia Eleitoral", na.value = "#e0e0e0"
      ) +
      labs(title = paste("Ano:", ano_selecionado)) +
      theme_void() + 
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        legend.position = "none", 
        plot.background = element_rect(fill = "#f8f9fa", color = NA)
      )
    
    return(p)
  }
  
  # 3. Gerar os Mapas
  mapa_1984 <- criar_mapa_vdem(1984)
  mapa_2004 <- criar_mapa_vdem(2004)
  mapa_2024 <- criar_mapa_vdem(2024)
  
  # 4. Juntar e Adicionar Sua Assinatura (Carlos Canto)
  mapa_final <- ggarrange(
    mapa_2024, mapa_2004, mapa_1984, 
    ncol = 1, nrow = 3,
    common.legend = TRUE, legend = "bottom"
  ) +
    theme(plot.background = element_rect(fill = "#f8f9fa", color = NA)) +
    labs(caption = "Análise e Visualização: Carlos Canto | Fonte: V-Dem Data (v14/v15)")
  
  # 5. Salvar
  ggsave("mapa_carlos_canto_vdem.png", plot = mapa_final, width = 8, height = 12, dpi = 300)

  mapa_final
  
  
  
