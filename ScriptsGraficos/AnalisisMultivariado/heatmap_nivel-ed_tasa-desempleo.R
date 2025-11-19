tasas_educacion <- filtered_db %>%
  group_by(AGLO_NOMBRE, NIVEL_ED, ANO4, TRIMESTRE) %>%
  summarise(
    POB_OCUPADA    = sum(if_else(ESTADO == 1, PONDERA, 0), na.rm = TRUE),
    POB_DESOCUPADA = sum(if_else(ESTADO == 2, PONDERA, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PEA = POB_OCUPADA + POB_DESOCUPADA,
    TASA_DESOCUPACION = 100 * POB_DESOCUPADA / PEA,
    PERIODO = paste0(ANO4, "-T", TRIMESTRE),
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

ggplot(
  tasas_educacion,
  aes(x = PERIODO, y = NIVEL_ED_LABEL, fill = TASA_DESOCUPACION)
) +
  geom_tile(color = "white", linewidth = 0.3) +   # Bordes para separar celdas
  facet_wrap(~ AGLO_NOMBRE, nrow = 1) +           # Vista horizontal más limpia
  scale_fill_viridis_c(
    option = "C",
    limits = c(0, 20),    # <<< IMPORTANTE: recorta outliers
    oob = squish,         # pone valores >20 como 20 (no arruina la paleta)
    breaks = c(0, 5, 10, 15, 20),
    labels = percent_format(scale = 1)
  ) +
  labs(
    title = "Heatmap: Desocupación por Nivel Educativo",
    subtitle = "Trimestres 2016–2025.",
    x = "",
    y = "Nivel educativo",
    fill = "Desocupación (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    panel.spacing = unit(1.5, "lines"),                 # Separación entre facetas
    axis.text.x = element_text(angle = 60, hjust = 1),  # Mejora lectura
    legend.position = "right",
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray40")
  )
