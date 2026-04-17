

#Raspagem de dados em Dados estáticos 


if(!require("pacman")) install.packages("pacman")
library(pacman)

pacman:: p_load(
  rvest, #pacote utilizado para a raspagem de dados 
  lubridate #manipulação de datas ,
  , dplyr, 
  stringr,
  ggrepel #colocar rótulos de forma mais elegante
  , ggplot2
  )


#url de interesse que eu quero puxar as informações

url <-  "https://cran.r-project.org/web/packages/available_packages_by_date.html"

#ler a url que vamos mandar par o R ler o contéudo do link

page <-  read_html(url)

#extração do conteúdo ou seja vamos extrair as informações do link proveniente 

packages_data <- page |>
  #Consigo extrair as informações da página para um objeto 
  html_node("table") |>  
  html_nodes("tr") |>  
  html_nodes("td") |> 
  html_text() |> 
  #transporto tudo para uma matriz
  matrix(ncol = 3, byrow = TRUE)

#conversão da matriz de dados = depois disso, vamos estruturar em um planilha de dados

pacotes_r  <-  as.data.frame(packages_data, stringsAsFactors = FALSE)
#coloco os nomes na minha matriz que tava com os nomes muito paia
colnames(pacotes_r) <-  c("data","nome_pacote","descricao")


#Como tnho variáve data eu posso pegar o ano, ele pega e cira uma coluna só com o ano 

pacotes_r$data <- as.Date(pacotes_r$data,format = "%Y-%m-%d")
pacotes_r$ano <- year(pacotes_r$data)



#Pra finalizar eu posso passar para salvar e salvar no formato de escel
#Sys date já utiliza a data atual

write_xlsx(pacotes_r,paste("df_pacotes_r", format(
  #Sys.Date coloca a data atual e não precisa ficar atualizando 
  Sys.Date(),"%d-%m-%Y")))

#Deixar tudo minusculo de lei, para não decorrer de ocorrer do caso de falso negativo e falso positivo 

pacotes_r$descricao <-  tolower(pacotes_r$descricao)

#Vou agora escolher um tema para com que eu consiga buscar e que falem com  survey 

#ele vai olhar se na frase de descrição existe ou não na coluna descricao a palavra cluster e se tiver TRUE e senão FALSE

#cria a variável tema e com o str_detect ele procura pra ver se tem ou não na coluna a string "cluster"
pacotes_r$tema <-  str_detect(pacotes_r$descricao,"cluster")


df_tematica <-  pacotes_r |>  
  filter( tema == "TRUE") |>  
  arrange(data) |>  
  dplyr::select(-tema)

#se tiver vedadeiro se não tiver é fake


#importa para onde voce quiser
write_xlsx(df_tematica, "C:..")



#outra forma é de fazer o Web Scrapping é 


#a estrutura é a mesma, só muda a questão do que vai ser utilizado para se achar
pacotes_r$time_series <- str_detect(pacotes_r$descricao,"time series")
#se tiver é verdadeiro
df_tematica <- pacotes_r |> 
filter(time_series == "TRUE") |>  
  arrange(data) |> 
  dplyr:: select(-tema,-time_series)  

write_xlsx(df_tematica,"C: ")

  
#a estrutura é a mesma, só muda a questão do que vai ser utilizado para se achar


pacotes_r$regression <- str_detect(pacotes_r$descricao,"regression")

#se tiver é verdadeiro
filter(regression == "TRUE") |>  
  arrange(data) |> 
  dplyr:: select(-tema,-regression)  

write_xlsx(df_tematica,"C: ")




#SCRIPT EXTRA 


#Pode se agregar e fazer um gráfico


df_tematica |>  
  group_by(ano) |> 
  summarise(n_pacotes=n()) |> 
  mutate(ano=as.numeric(ano)) |>   #transforma ano em numérico
  ggplot(aes(ano,n_pacotes)) + 
  geom_line(alpha = .05, 
            size = 3., col = "blue") +
  geom_point(size = 25, alpha = .25, col = "blue") +
  scale_x_continuous(breaks = seq(2008,2025,by=1)) + 
  theme_bw() +
  labs( x="",
        y="",
        title = "Número de pacotes para conteúdo Time Series no CRAN",
        subtitle = "(2011 - 2025)",
        caption = "Fonte: Eu" + 
          geom_text(aes(label = n_pacotes),size = 10) +
          theme(
            legend.title = element_blank(),
            text = element_text(size =30),
            asis.text.x = element_text(angle=45, hjust = 1)
            
          )
  )  


