# Gráfico de la Evolución histórica de la Mediana
ggplot(resumen_ingresos, 
       aes(x = PERIODO, y = MEDIANA_INGRESO_REAL, color = AGLO_NOMBRE, group = AGLO_NOMBRE)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  
  labs(
    title = "Evolución de la Mediana del Ingreso Real (P47T)",
    subtitle = paste0("Ocupados con ingresos. Pesos constantes de T2-2025"),
    x = "Período (Año-Trimestre)",
    y = "Mediana del Ingreso Real",
    color = "Aglomerado"
  ) +
  theme_minimal() +
  # Rotar etiquetas del eje X para legibilidad
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))