library(ggplot2)
library(dplyr)

dados_vacina <- read.csv('/home/ubuntu/Documentos/biologia_computacional/atividade_2/planilhas/vaccination-data.csv')
dados_covid <- read.csv('/home/ubuntu/Documentos/biologia_computacional/atividade_2/planilhas/WHO-COVID-19-global-table-data.csv')

names(dados_covid)[names(dados_covid) == "Name"] <- "COUNTRY"



dados_mesclados <- merge(dados_vacina, dados_covid, by = "COUNTRY") %>%
  group_by(WHO_REGION) %>%
  summarise(
    TOTAL_VACCINATIONS_PER100000 = sum(TOTAL_VACCINATIONS_PER100 * 1000, na.rm = TRUE),
    Deaths_per_100000 = sum(Deaths_per_100000, na.rm = TRUE)
  ) %>%
  mutate(
    TOTAL_VACCINATIONS_PER100000 = format(TOTAL_VACCINATIONS_PER100000, big.mark = ",", scientific = FALSE),
    Deaths_per_100000 = format(Deaths_per_100000, big.mark = ",", scientific = FALSE)
  )

ggplot(dados_mesclados, aes(x = Deaths_per_100000, y = TOTAL_VACCINATIONS_PER100000, fill = WHO_REGION)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Vacinas x Mortes per 100000",
    x = "Mortes por 100000 habitantes",
    y = "Vacinas por 100000 habitantes"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))