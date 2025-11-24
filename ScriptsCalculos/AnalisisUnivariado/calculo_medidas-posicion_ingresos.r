library(tidyverse)
library(Hmisc) 

# --- Paso 1: Unir IPC y Calcular Ingreso Real (P47T_REAL) ---

# base_con_ingresos_reales es la base individual con el coeficiente de ajuste
base_con_ingresos_reales <- filtered_db %>%
  left_join(ipc_final, by = c("ANO4", "TRIMESTRE")) %>%
  
  # Filtrar el universo de análisis: Ocupados que declararon ingreso
  filter(ESTADO == 1, P47T > 0) %>%
  
  # Calcular el Ingreso Real (Ajustado)
  mutate(
    P47T_REAL = P47T * COEF_AJUSTE
  )

# --- Paso 2: Calcular Evolución de Medidas por Trimestre ---

# Agrupamos por tiempo y aglomerado para calcular las medidas de tendencia y posición.
resumen_ingresos <- base_con_ingresos_reales %>%
  group_by(AGLO_NOMBRE, AGLOMERADO, FECHA, PERIODO) %>%
  summarise(
    # Medida de Tendencia Central Principal: Mediana (Robusta ante outliers)
    MEDIANA_INGRESO_REAL = wtd.quantile(P47T_REAL, PONDII, probs = 0.5, na.rm = TRUE),
    
    # Medida de Tendencia Central Secundaria: Media (Sensible a outliers)
    MEDIA_INGRESO_REAL = weighted.mean(P47T_REAL, PONDII, na.rm = TRUE),
    
    # Medidas de Posición: Cuartiles (Muestran la dispersión)
    CUARTIL_1 = wtd.quantile(P47T_REAL, PONDII, probs = 0.25, na.rm = TRUE),
    CUARTIL_3 = wtd.quantile(P47T_REAL, PONDII, probs = 0.75, na.rm = TRUE),
    
    # Deciles de Pobreza (Opcional, muestra desigualdad)
    DECIL_1 = wtd.quantile(P47T_REAL, PONDII, probs = 0.1, na.rm = TRUE),
    DECIL_9 = wtd.quantile(P47T_REAL, PONDII, probs = 0.9, na.rm = TRUE)
  ) %>%
  ungroup()



print("Tabla de evolución de ingresos (Mediana, Media y Cuartiles):")
print(head(resumen_ingresos))

