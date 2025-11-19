# Asumimos que GRUPO_OCUPACIONAL ha sido añadido a filtered_db.
# Asumimos que base_con_ingresos_reales contiene P47T_REAL y PONDII.
library(Hmisc) 

resumen_ingresos_ocupacional <- base_con_ingresos_reales %>%
  
  # 1. Añadir el nuevo grupo a la agrupación (Multivariado)
  group_by(AGLO_NOMBRE, PERIODO, GRUPO_OCUPACIONAL) %>% 
  summarise(
    # Medidas de Tendencia Central
    MEDIANA_INGRESO_REAL = wtd.quantile(P47T_REAL, PONDII, probs = 0.5, na.rm = TRUE),
    # Medidas de Posición
    CUARTIL_1 = wtd.quantile(P47T_REAL, PONDII, probs = 0.25, na.rm = TRUE),
    CUARTIL_3 = wtd.quantile(P47T_REAL, PONDII, probs = 0.75, na.rm = TRUE),
    .groups = "drop"
  )

library(ggplot2)

ggplot(resumen_ingresos_ocupacional, 
       aes(x = PERIODO, y = MEDIANA_INGRESO_REAL, color = AGLO_NOMBRE, group = AGLO_NOMBRE)) +
  
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  
  # 1. FACETEO: Creamos un panel para cada Grupo Ocupacional
  facet_wrap(~ GRUPO_OCUPACIONAL, scales = "free_y", ncol = 2) +
  
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  
  labs(
    title = "Evolución de la Mediana del Ingreso Real por Grupo Ocupacional",
    subtitle = "Comparativa CABA vs. GBA (Pesos constantes T2-2025)",
    x = "Período (Año-Trimestre)",
    y = "Mediana del Ingreso Real",
    color = "Aglomerado"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7),
    strip.text = element_text(face = "bold") 
  )