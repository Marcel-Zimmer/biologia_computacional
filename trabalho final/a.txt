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

# Filtrando dados para países com 1% ou mais de mortalidade e 1% ou mais de pessoas vacinadas
dados_combinados <- dados_combinados %>%
  select(COUNTRY, Vaccinated = `TOTAL_VACCINATIONS_PER100`, Deaths = `Deaths_per_100000`) %>%
  filter(!is.na(Vaccinated) & !is.na(Deaths)) %>%
  mutate(Mortalidade_Percentual = Deaths / 10) %>%  
  # Filtra para mortalidade >= 1% e vacinação >= 1%
  filter(Mortalidade_Percentual >= 1 & Vaccinated >= 1)

# Filtrando dados relevantes
mortalidade_baixa <- dados_combinados %>%
  select(COUNTRY, Vaccinated, Deaths) %>%
  filter(!is.na(Vaccinated) & !is.na(Deaths)) %>%
  mutate(Mortalidade_Percentual = Deaths / 10) %>%  
  filter(Mortalidade_Percentual <= 5)

mortalidade_alta <- dados_combinados %>%
  select(COUNTRY, Vaccinated, Deaths) %>%
  filter(!is.na(Vaccinated) & !is.na(Deaths)) %>%
  mutate(Mortalidade_Percentual = Deaths / 10) %>%  
  filter(Mortalidade_Percentual > 5)

# Agrupando países para cada subnó
paises_baixa_vacina_elevada <- mortalidade_baixa %>%
  filter(Vaccinated > 50) %>%
  pull(COUNTRY)

paises_baixa_vacina_baixa <- mortalidade_baixa %>%
  filter(Vaccinated <= 50) %>%
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

# Cria o grafo principal com dois nós e uma aresta entre eles
g1 <- graph(edges = c("Baixa Mortalidade", "Alta Mortalidade"), directed = FALSE)

# Adiciona subnós para "Baixa Mortalidade"
g1 <- add_vertices(g1, 2, name = c("Taxa de vacinação elevada (Baixa Mortalidade)", "Taxa de vacinação pequena (Baixa Mortalidade)"))
g1 <- add_edges(g1, c("Baixa Mortalidade", "Taxa de vacinação elevada (Baixa Mortalidade)", 
                      "Baixa Mortalidade", "Taxa de vacinação pequena (Baixa Mortalidade)"))

# Adiciona subnós para "Alta Mortalidade"
g1 <- add_vertices(g1, 2, name = c("Taxa de vacinação elevada (Alta Mortalidade)", "Taxa de vacinação pequena (Alta Mortalidade)"))
g1 <- add_edges(g1, c("Alta Mortalidade", "Taxa de vacinação elevada (Alta Mortalidade)", 
                      "Alta Mortalidade", "Taxa de vacinação pequena (Alta Mortalidade)"))

# Adicionando os países para cada subnó

# Função para adicionar países aos subnós
adicionar_paises_subnó <- function(grafo, paises, subnó) {
  # Adiciona os países ao grafo
  grafo <- add_vertices(grafo, length(paises), name = paises)
  
  # Cria um vetor alternado para conectar o subnó aos países
  arestas <- c()
  for (pais in paises) {
    arestas <- c(arestas, subnó, pais)
  }
  
  # Adiciona as arestas ao grafo
  grafo <- add_edges(grafo, arestas)
  
  return(grafo)
}

# Adicionando países para os subnós
g1 <- adicionar_paises_subnó(g1, paises_baixa_vacina_elevada, "Taxa de vacinação elevada (Baixa Mortalidade)")
g1 <- adicionar_paises_subnó(g1, paises_baixa_vacina_baixa, "Taxa de vacinação pequena (Baixa Mortalidade)")
g1 <- adicionar_paises_subnó(g1, paises_alta_vacina_elevada, "Taxa de vacinação elevada (Alta Mortalidade)")
g1 <- adicionar_paises_subnó(g1, paises_alta_vacina_baixa, "Taxa de vacinação pequena (Alta Mortalidade)")

# Adiciona o grafo ao RedeR
addGraph(rdp, g = g1, layout = layout_with_kk(g1))
