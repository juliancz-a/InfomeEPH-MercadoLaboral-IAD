# Boxplot de Medidas de Posición (Cuartiles)
base_con_ingresos_reales %>%
  # Filtramos el último año completo de datos (Ejemplo: 2024)
  filter(ANO4 == 2024) %>% 
  ggplot(aes(x = AGLO_NOMBRE, y = P47T_REAL, fill = AGLO_NOMBRE)) +
  
  # Boxplot muestra: Mediana (línea), Cuartiles 1 y 3 (la caja)
  geom_boxplot() + 
  
  # Usar escala logarítmica para mejorar la visualización de la distribución
  scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
  
  labs(
    title = "Distribución del Ingreso Real por Aglomerado (Medidas de Posición)",
    subtitle = "Ocupados con ingresos, T2-2025 (Escala Logarítmica)",
    x = "Aglomerado",
    y = "Ingreso Real",
    fill = "Aglomerado"
  ) +
  theme_minimal()