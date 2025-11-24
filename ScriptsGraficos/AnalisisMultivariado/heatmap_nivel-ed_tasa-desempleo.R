tasas_educacion_anual <- filtered_db %>%
  group_by(AGLO_NOMBRE, NIVEL_ED, ANO4) %>%
  summarise(
    POB_TOTAL      = sum(PONDERA, na.rm = TRUE),
    POB_OCUPADA    = sum(if_else(ESTADO == 1, PONDERA, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    TASA_EMPLEO = 100 * POB_OCUPADA / POB_TOTAL,
    PERIODO = as.character(ANO4),
    NIVEL_ED_LABEL = case_when(
      NIVEL_ED == 1 ~ "1. Primario incompleto\n(incluye educación especial)",
      NIVEL_ED == 2 ~ "2. Primario completo",
      NIVEL_ED == 3 ~ "3. Secundario incompleto",
      NIVEL_ED == 4 ~ "4. Secundario completo",
      NIVEL_ED == 5 ~ "5. Sup./Univ. incompleto",
      NIVEL_ED == 6 ~ "6. Sup./Univ. completo",
      NIVEL_ED == 7 ~ "7. Sin instrucción",
      TRUE ~ "No especificado"
    )
  )



library(ggplot2)
library(scales)

min_emp <- min(tasas_educacion_anual$TASA_EMPLEO, na.rm = TRUE)
max_emp <- max(tasas_educacion_anual$TASA_EMPLEO, na.rm = TRUE)

ggplot(
  tasas_educacion_anual,
  aes(x = PERIODO, y = NIVEL_ED_LABEL, fill = TASA_EMPLEO)
) +
  geom_tile(color = "white", linewidth = 0.4) +
  facet_wrap(~ AGLO_NOMBRE, nrow = 1) +
  labs(
    title = "Heatmap Anual: Tasa de Empleo por Nivel Educativo",
    subtitle = "Promedios ponderados anuales (EPH 2016–2025)",
    x = "Año",
    y = "Nivel Educativo",
    fill = "Empleo (%)"
  ) +
  scale_fill_viridis_c( option = "C", limits = c(min_emp, max_emp), breaks = seq(min_emp, max_emp, length.out = 5), labels = scales::percent_format(scale = 1), ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 14, face = "bold")
  )

