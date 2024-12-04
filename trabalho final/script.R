library(ggplot2)
library(dplyr)
library(igraph)  # Para criar grafos
library(tidyr)   # Para manipular dados
library(RedeR)   # Para visualização de redes


dados_vacina <- read.csv("C:/Users/Marcel/Documents/biologia_computacional/atividade_2/planilhas/vaccination-data.csv")
dados_covid <- read.csv("C:/Users/Marcel/Documents/biologia_computacional/atividade_2/planilhas/WHO-COVID-19-global-table-data.csv")


names(dados_covid)[names(dados_covid) == "Name"] <- "COUNTRY"


dados_combinados <- inner_join(dados_vacina, dados_covid, by = "COUNTRY")


dados_filtrados <- dados_combinados %>%
  select(COUNTRY, Vaccinated = `TOTAL_VACCINATIONS`, Deaths = `Deaths_total`) %>%
  filter(!is.na(Vaccinated) & !is.na(Deaths))

associacoes_grafo <- associacoes %>%
  select(COUNTRY, Metric, Value) %>%
  filter(Value > 0.05)  # Mantém a filtragem, mas agora vamos usar apenas duas colunas para as arestas

# Criar grafo com a associação entre país e métrica
grafo <- graph_from_data_frame(associacoes_grafo[, 1:2], directed = FALSE)

E(grafo)$weight <- associacoes_grafo$Value



# Visualizar no RedeR
rdp <- RedPort()
calld(rdp)
addGraph(rdp, grafo)
