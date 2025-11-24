tasas_educ <- filtered_db %>%
  group_by(AGLO_NOMBRE, ANO4, TRIMESTRE, NIVEL_ED) %>%
  summarise(
    POB_TOTAL      = sum(PONDERA, na.rm = TRUE),
    POB_OCUPADA    = sum(if_else(ESTADO == 1, PONDERA, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    TASA_EMPLEO = 100 * POB_OCUPADA / POB_TOTAL,
    PERIODO = paste0(ANO4, "-T", TRIMESTRE),
    NIVEL_ED_LABEL = case_when(
      NIVEL_ED == 1 ~ "1. Primario incompleto",
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

ggplot(tasas_educ,
       aes(x = PERIODO, y = TASA_EMPLEO,
           color = NIVEL_ED_LABEL,
           group = NIVEL_ED_LABEL)) +
  
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.2) +
  
  facet_wrap(~ AGLO_NOMBRE, nrow = 1) +
  
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_brewer(palette = "Dark2") +   # colores visibles y contrastantes
  
  labs(
    title = "Evolución de la Tasa de Empleo por Nivel Educativo",
    subtitle = "Aglomerados EPH (2016–2025)",
    x = "Período (Año–Trimestre)",
    y = "Tasa de Empleo (%)",
    color = "Nivel Educativo"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )
