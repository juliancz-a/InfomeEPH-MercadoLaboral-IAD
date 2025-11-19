filtered_db <- filtered_db %>%
  mutate(
    SEXO = case_when(
      CH04 == 1 ~ "Varón",
      CH04 == 2 ~ "Mujer",
      TRUE ~ NA_character_
    )
  )


tasas_sexo <- filtered_db %>%
  group_by(AGLO_NOMBRE, SEXO, ANO4, TRIMESTRE) %>%
  summarise(
    POB_OCUPADA    = sum(if_else(ESTADO == 1, PONDERA, 0), na.rm = TRUE),
    POB_DESOCUPADA = sum(if_else(ESTADO == 2, PONDERA, 0), na.rm = TRUE),
    POB_TOTAL_EPH  = sum(PONDERA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PEA = POB_OCUPADA + POB_DESOCUPADA,
    TASA_DESOCUPACION = 100 * POB_DESOCUPADA / PEA,
    PERIODO = paste0(ANO4, "-T", TRIMESTRE)
  )
library(ggplot2)

ggplot(
  tasas_sexo,
  aes(x = PERIODO, 
      y = TASA_DESOCUPACION, 
      color = AGLO_NOMBRE, 
      group = AGLO_NOMBRE)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ SEXO) +
  scale_y_continuous(labels = function(x) paste0(round(x,1), "%")) +
  labs(
    title = "Evolución de la Tasa de Desocupación por Sexo (2016–2025)",
    subtitle = "Comparación entre aglomerados",
    x = "Año - Trimestre",
    y = "Tasa de Desocupación",
    color = "Aglomerado"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    strip.text = element_text(size = 12)
  )
