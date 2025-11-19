library(dplyr)

tasas_df <- filtered_db %>%
  group_by(AGLO_NOMBRE, FECHA, ANO4, TRIMESTRE) %>%
  summarise(
    POB_OCUPADA    = sum(if_else(ESTADO == 1, PONDERA, 0), na.rm = TRUE),
    POB_DESOCUPADA = sum(if_else(ESTADO == 2, PONDERA, 0), na.rm = TRUE),
    POB_INACTIVA   = sum(if_else(ESTADO == 3, PONDERA, 0), na.rm = TRUE),
    POB_TOTAL_EPH  = sum(PONDERA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PEA            = POB_OCUPADA + POB_DESOCUPADA,
    TASA_ACTIVIDAD = 100 * PEA / POB_TOTAL_EPH,
    TASA_EMPLEO    = 100 * POB_OCUPADA / POB_TOTAL_EPH,
    TASA_DESOCUPACION = 100 * POB_DESOCUPADA / PEA,
    PERIODO = paste0(ANO4, "-T", TRIMESTRE)
  )
summary(filtered_db$PONDERA)
table(is.na(filtered_db$PONDERA))

library(ggplot2)

# Gráfico de la Tasa de Desocupación
ggplot(tasas_df, 
       aes(
         x = PERIODO, 
         y = TASA_DESOCUPACION, 
         color = AGLO_NOMBRE,
         group = AGLO_NOMBRE 
       )) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +

  
  labs(
    title = "Evolución de la Tasa de Desocupación (2016-2025)",
    subtitle = "Comparativa CABA vs. GBA",
    x = "Período (Año-Trimestre)",
    y = "Tasa de Desocupación",
    color = "Aglomerado"
  ) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
