tasas_sexo <- filtered_db %>%
  group_by(AGLO_NOMBRE, CH04, ANO4) %>%
  summarise(
    POB_TOTAL   = sum(PONDERA, na.rm = TRUE),
    POB_OCUPADA = sum(if_else(ESTADO == 1, PONDERA, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    TASA_EMPLEO = 100 * POB_OCUPADA / POB_TOTAL,
    SEXO = ifelse(CH04 == 1, "Varón", "Mujer")
  )


library(ggplot2)
library(scales)

ggplot(tasas_sexo,
       aes(x = factor(ANO4), y = TASA_EMPLEO, fill = SEXO)) +
  geom_col(position = "dodge") +
  facet_wrap(~ AGLO_NOMBRE, nrow = 1) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Tasa de Empleo por Sexo",
    subtitle = "Comparación anual 2016–2025",
    x = "Año",
    y = "Tasa de Empleo (%)",
    fill = "Sexo"
  ) +
  theme_minimal(base_size = 12)
