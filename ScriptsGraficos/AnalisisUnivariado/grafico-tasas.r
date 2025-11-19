library(tidyr)
library(dplyr)

# Columnas a pivotar: TASA_ACTIVIDAD, TASA_EMPLEO, TASA_DESOCUPACION
tasas_long <- tasas_df %>%
  pivot_longer(
    cols = starts_with("TASA_"),
    names_to = "Indicador", # Nueva columna para el nombre de la tasa
    values_to = "Valor_Tasa" # Nueva columna para el valor numérico
  ) %>%
  # Limpiamos y ordenamos las etiquetas para el gráfico
  mutate(
    Indicador = case_match(
      Indicador,
      "TASA_ACTIVIDAD" ~ "1. Tasa de Actividad",
      "TASA_EMPLEO" ~ "2. Tasa de Empleo",
      "TASA_DESOCUPACION" ~ "3. Tasa de Desocupación"
    )
  )

#Grafico
library(ggplot2)

ggplot(tasas_long, 
       aes(x = PERIODO, y = Valor_Tasa, color = AGLO_NOMBRE, group = AGLO_NOMBRE)) +
  
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  
  # Usamos facet_wrap para crear un panel por cada Indicador
  # scales = "free_y" es CRUCIAL para que cada tasa tenga su propio eje Y
  facet_wrap(~ Indicador, ncol = 1, scales = "free_y") +
  
  scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +
  
  labs(
    title = "Evolución de las Tasas del Mercado Laboral (2016-2025)",
    subtitle = "Comparativa CABA vs. GBA por indicador",
    x = "Período (Año-Trimestre)",
    y = "Valor de la Tasa",
    color = "Aglomerado"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7),
    strip.text = element_text(face = "bold") # Hace que los títulos de los paneles sean audaces
  )