 
#Metodologia desenvolvida pelo Arthur Rozenas que é pouco conhecido mas é muito citado
 
  # ANÁLISE FORENSE ELEITORAL: MÉTODO DE ROZENAS (2017)
 
 # 
 # -A intuição do método: O ser humano é péssimo em inventar números aleatórios e por isso usa disso para achar rastros de fraude
 #   Quando tentam fraudar dados eleitorais, há um viés cognitivo para atribuir que é ai que se pega quem fzaz coisa errada
 #   percentuais inteiros ou terminados em 0 e 5 (ex: 80%, 85%) e fazer a conta reversa. utilziar de formas de arrendomentos é sinal de fraude 
 # Em uma distribuição honesta, a divisão (Votos / Comparecimento) gera 
 #números fracionados complexos.
 # O algoritmo compara a frequência de números "arredondados" ou dígitos adjacentes com a distribuição esperada ao acaso são o grande sinal de adulteração. 
 #Uitlizdando sempre de numeros com relaçõers a zonas e munciipios por ser mais dificil de ter erros e ser mais preciso

 #Artigo Principal: Arturas Rozenas (2017) - "Detecting Election Fraud from Irregularities in Vote-Share Distributions", publicado na Political Analysis 
 # Psicologia/Viés: Artigo de Beber & Scacco (2012) sobre a preferência humana por dígitos adjacentes e arredondamento
 # Outros autores: Walter Mebane (citado como referência em metodologias forenses) 
 # Pacote R: {spikes}, desenvolvido pelo próprio Arturas Rozenas 
 
#Exemplo do que vai ser utilizaado
 # - Base utilizada: Eleição Presidencial de 2022 (Lula x Bolsonaro)
 # Utiliza se o pacote spikes 
 
 # Carregando os pacotes padrão de manipulação e o pacote do modelo
 pacman::p_load(tidyverse, spikes, janitor)
 

 # O pacote {spikes} exige uma formatação bem sencisevlk logo ele só aceita bases com 
 # EXATAMENTE as três colunas a seguir (sem valores faltantes - NA):
 # N = Eleitores Registrados
 # T = Comparecimento (Turnout)
 # V = Votos nominais no Candidato
 
 set.seed(4187) # Semente fixada
 
 # Simulando dados de 1.000 seções eleitorais quaisqueres
 df_eleicoes <- tibble(
   id_secao = 1:1000,
   N = round(runif(1000, min = 100, max = 500)),       # Eleitores registrados
   T = round(N * runif(1000, min = 0.70, max = 0.85)), # Comparecimento 
   V = round(T * runif(1000, min = 0.40, max = 0.60))  # Votos no candidato alvo
 )
 
 # Inserindo "fraudes" propositais para o pacote detectar:
 # Vamos forçar que em 30 seções o candidato teve exatamente 90% (número cheio)
 fraudes_idx <- sample(1:1000, 30)
 df_eleicoes$V[fraudes_idx] <- round(df_eleicoes$T[fraudes_idx] * 0.90)
 df_rozenas <- df_eleicoes %>%
   # 1. Manter apenas seções com amostragem relevante (conforme o artigo)
   filter(N >= 100) %>% 
   
   # 2. Omitir qualquer linha com NA (isso quebra a função do pacote)
   drop_na() %>%        
   
   # 3. Selecionar as três variáveis exigidas (N, T e V) senão não vai funcionar de jeito nenmhum 
   select(N, T, V)
 
 
 #Análise - Aplicamos aqui o metodo de Rozenas
 
 # Aviso: O método por trás (densidade de kernel com estatística Bayesiana) 
 # é bastante pesado computacionalmente. O pacote sugere 1000 reamostragens, 
 # mas reduzi para 100 aqui para você conseguir rodar rápido na sua máquina.
 
 cat("Estimando o modelo de Rozenas... Isso pode demorar um pouco.\n")
 
 resultado_fraude <- spikes(
   data = df_rozenas, 
   resamples = 100   # Para a base real do TSE, altere para 1000
 )
 
 # O objeto gerado é uma lista. O mais importante é o índice "fraud".
 
 # Extraindo a proporção de seções contaminadas
 estimativa <- resultado_fraude$fraud
 cat("\nProporção estimada de fraude na base simulada:", round(estimativa * 100, 2), "%\n")
 
 # Gráfico de densidade gerado pelo próprio pacote
 # (Ele cruza a distribuição esperada com a observada. Os picos mostram as anomalias)
 plot(resultado_fraude)
 
 #Esse processo todo pode mostrar algo que mostre algo porém ele ainda pode ser falho nao se usa aqui os numeros verdadeiros mas ele mostra um cenario bom de se visualizar nesse exemplo 
 
 #O ideal que se faça isso por sessão mas demora BASTANTE 
 
