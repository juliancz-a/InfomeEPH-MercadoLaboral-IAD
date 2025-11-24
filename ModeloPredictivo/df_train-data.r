# 1. Definir las variables que se van a utilizar
variables_modelo <- c("P47T", "NIVEL_ED", "CH04", "AGLO_NOMBRE", "PP04G")

# 2. Crear el set de entrenamiento filtrando los NA/No-respuesta en P47T
train_data <- filtered_db %>%
  select(all_of(variables_modelo)) %>%
  filter(!is.na(P47T) & P47T > 0)

train_data <- train_data %>%
  mutate(
    # Convertir a factor para que lm() genere las variables dummy automáticamente
    NIVEL_ED = as.factor(NIVEL_ED),
    CH04 = as.factor(CH04),
    AGLO_NOMBRE = as.factor(AGLO_NOMBRE),
    PP04G = as.factor(PP04G)
    
  )


modelo_rgl <- lm(
  P47T ~ NIVEL_ED + CH04 + AGLO_NOMBRE + PP04G,
  data = train_data
)

# Resumen de los resultados y coeficientes
summary(modelo_rgl)