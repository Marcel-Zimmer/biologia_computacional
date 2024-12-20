# Carregando bibliotecas
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

# Filtrando dados relevantes
mortalidade_baixa <- dados_combinados %>%
  select(COUNTRY, Vaccinated = `TOTAL_VACCINATIONS_PER100`, Deaths = `Deaths_per_100000`) %>%
  filter(!is.na(Vaccinated) & !is.na(Deaths)) %>%
  mutate(Mortalidade_Percentual = Deaths / 10) %>%  
  filter(Mortalidade_Percentual <= 5)

mortalidade_alta <- dados_combinados %>%
  select(COUNTRY, Vaccinated = `TOTAL_VACCINATIONS_PER100`, Deaths = `Deaths_per_100000`) %>%
  filter(!is.na(Vaccinated) & !is.na(Deaths)) %>%
  mutate(Mortalidade_Percentual = Deaths / 10) %>%  
  filter(Mortalidade_Percentual > 5)

# Agrupando países para cada subnó
paises_baixa_vacina_elevada <- mortalidade_baixa %>%
  filter(Vaccinated > 50) %>%  # Exemplo: taxa elevada > 50%
  pull(COUNTRY)

paises_baixa_vacina_baixa <- mortalidade_baixa %>%
  filter(Vaccinated <= 50) %>%  # Exemplo: taxa baixa <= 50%
  pull(COUNTRY)

paises_alta_vacina_elevada <- mortalidade_alta %>%
  filter(Vaccinated > 50) %>%
  pull(COUNTRY)

paises_alta_vacina_baixa <- mortalidade_alta %>%
  filter(Vaccinated <= 50) %>%
  pull(COUNTRY)

# Inicializa RedeR
rdp <- RedPort()
calld(rdp)

# Cria o grafo principal com dois nós (Baixa Mortalidade, Alta Mortalidade)
g1 <- graph(edges = c("Baixa Mortalidade", "Alta Mortalidade"), directed = FALSE)

# Adiciona subnós para "Baixa Mortalidade" e "Alta Mortalidade"
g1 <- add_vertices(g1, 2, name = c("Taxa de vacinação elevada (Baixa Mortalidade)", "Taxa de vacinação pequena (Baixa Mortalidade)"))
g1 <- add_edges(g1, c("Baixa Mortalidade", "Taxa de vacinação elevada (Baixa Mortalidade)", 
                      "Baixa Mortalidade", "Taxa de vacinação pequena (Baixa Mortalidade)"))

g1 <- add_vertices(g1, 2, name = c("Taxa de vacinação elevada (Alta Mortalidade)", "Taxa de vacinação pequena (Alta Mortalidade)"))
g1 <- add_edges(g1, c("Alta Mortalidade", "Taxa de vacinação elevada (Alta Mortalidade)", 
                      "Alta Mortalidade", "Taxa de vacinação pequena (Alta Mortalidade)"))

# Adiciona os países como subnós para cada categoria

# Adiciona os países da "Taxa de vacinação elevada (Baixa Mortalidade)"
g1 <- add_vertices(g1, length(paises_baixa_vacina_elevada), name = paises_baixa_vacina_elevada)
# Conecta os países ao subnó correspondente de "Taxa de vacinação elevada (Baixa Mortalidade)"
g1 <- add_edges(g1, c(rep("Taxa de vacinação elevada (Baixa Mortalidade)", length(paises_baixa_vacina_elevada)), paises_baixa_vacina_elevada))

# Adiciona os países da "Taxa de vacinação pequena (Baixa Mortalidade)"
g1 <- add_vertices(g1, length(paises_baixa_vacina_baixa), name = paises_baixa_vacina_baixa)
# Conecta os países ao subnó correspondente de "Taxa de vacinação pequena (Baixa Mortalidade)"
g1 <- add_edges(g1, c(rep("Taxa de vacinação pequena (Baixa Mortalidade)", length(paises_baixa_vacina_baixa)), paises_baixa_vacina_baixa))

# Adiciona os países da "Taxa de vacinação elevada (Alta Mortalidade)"
g1 <- add_vertices(g1, length(paises_alta_vacina_elevada), name = paises_alta_vacina_elevada)
# Conecta os países ao subnó correspondente de "Taxa de vacinação elevada (Alta Mortalidade)"
g1 <- add_edges(g1, c(rep("Taxa de vacinação elevada (Alta Mortalidade)", length(paises_alta_vacina_elevada)), paises_alta_vacina_elevada))

# Adiciona os países da "Taxa de vacinação pequena (Alta Mortalidade)"
g1 <- add_vertices(g1, length(paises_alta_vacina_baixa), name = paises_alta_vacina_baixa)
# Conecta os países ao subnó correspondente de "Taxa de vacinação pequena (Alta Mortalidade)"
g1 <- add_edges(g1, c(rep("Taxa de vacinação pequena (Alta Mortalidade)", length(paises_alta_vacina_baixa)), paises_alta_vacina_baixa))

# Atualiza os rótulos para exibir apenas o nome do país
V(g1)$label <- V(g1)$name

# Adiciona o grafo ao RedeR
addGraph(rdp, g = g1, layout = layout_with_kk(g1))
