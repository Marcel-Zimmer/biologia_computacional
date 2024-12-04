library(ggplot2)
library(dplyr)
library(igraph)  # Para criar grafos
library(tidyr)   # Para manipular dados
library(RedeR)   # Para visualização de redes

# Leitura dos dados
dados_vacina <- read.csv("C:/Users/Marcel/Documents/biologia_computacional/atividade_2/planilhas/vaccination-data.csv")
dados_covid <- read.csv("C:/Users/Marcel/Documents/biologia_computacional/atividade_2/planilhas/WHO-COVID-19-global-table-data.csv")

# Renomeando coluna para fazer o join
names(dados_covid)[names(dados_covid) == "Name"] <- "COUNTRY"

# Combinando os dados
dados_combinados <- inner_join(dados_vacina, dados_covid, by = "COUNTRY")

# Filtragem dos dados
dados_filtrados <- dados_combinados %>%
  select(COUNTRY, Vaccinated = `TOTAL_VACCINATIONS_PER100`, Deaths = `Deaths_per_100000`) %>%
  filter(!is.na(Vaccinated) & !is.na(Deaths)) %>%
  mutate(Mortalidade_Percentual = Deaths / 10) %>%  # Converte de mortes por 100.000 para percentual
  filter(Mortalidade_Percentual >= 3)  # Filtro para países com pelo menos 1% de mortalidade

# Criar categorias: Alta vs Baixa Mortalidade, Alta vs Baixa Vacinação
dados_filtrados <- dados_filtrados %>%
  mutate(Mortalidade = ifelse(Deaths > 5, "Alta Mortalidade", "Baixa Mortalidade"),
         Vacinação = ifelse(Vaccinated > 50, "Alta Vacinação", "Baixa Vacinação"))


# Criar os nós principais e subnós
nodos_principais <- c("Baixa Mortalidade", "Alta Mortalidade")
subnodos <- c("Baixa Vacinação", "Alta Vacinação")

# Preparar as arestas: conectar os países aos subnós
arestas <- dados_filtrados %>%
  mutate(from = Mortalidade, to = Vacinação) %>%
  select(from, to) %>%
  bind_rows(tibble(from = rep(dados_filtrados$COUNTRY, each = 2), to = rep(subnodos, times = nrow(dados_filtrados)))) # conecta países aos subnós

# Criar grafo com os nós principais e subnós
grafo <- graph_from_data_frame(arestas, directed = FALSE)

# Adicionar os nós principais (se necessário)
grafo <- add_vertices(grafo, 2, name = c("Baixa Mortalidade", "Alta Mortalidade"))

# Visualizar o grafo no RedeR
rdp <- RedPort()
calld(rdp)
addGraph(rdp, grafo)