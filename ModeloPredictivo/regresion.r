# ==============================================================================
# PROYECTO: Modelo de Regresión EPH (2º Trimestre 2025) - NIVEL AVANZADO
# OBJETIVO: Comparativa de Aglomerados con Diagnósticos, Nuevas Variables y Selección Automática
# ==============================================================================

# 1. CARGA DE LIBRERÍAS
# ------------------------------------------------------------------------------
library(tidyverse) 
library(scales) 
if(!require(car)) install.packages("car")    # Para VIF, Outliers y tests
if(!require(lmtest)) install.packages("lmtest") # Para tests de homocedasticidad
library(car)
library(lmtest)

# ==============================================================================
# 2. PREPARACIÓN DE DATOS
# ==============================================================================

# Definimos los aglomerados de interés (CABA y GBA)
aglomerados_interes <- c(32, 33) 

# Filtramos y procesamos variables
# Nota: Asumimos que 'filtered_db' es tu dataframe original cargado previamente.
datos_procesados <- filtered_db %>%
  filter(ANO4 == 2025, TRIMESTRE == 2) %>%
  filter(AGLOMERADO %in% aglomerados_interes) %>%
  # FILTRO CLAVE: Solo Ocupados (ESTADO = 1) para tener datos laborales coherentes
  filter(ESTADO == 1) %>%
  select(P47T, NIVEL_ED, CH04, AGLOMERADO, AGLO_NOMBRE, PP04G, CH06, PP3E_TOT, CAT_OCUP, 
         PP04A, PP07A, PP07G1, PP07I) %>%
  mutate(
    # --- Variables Estructurales ---
    CH04 = factor(CH04, levels = c(1, 2), labels = c("Varon", "Mujer")),
    NIVEL_ED = as.factor(NIVEL_ED),
    Aglomerado = factor(AGLO_NOMBRE), 
    
    CAT_OCUP = factor(CAT_OCUP, levels = c(1, 2, 3, 4), 
                      labels = c("Patron", "Cta.Propia", "Asalariado", "Familiar")),
    
    # --- Nuevas Variables de Calidad del Empleo ---
    
    # Sector (PP04A): 1=Estatal, 2=Privado, 3=Otro
    Sector = case_when(
      PP04A == 1 ~ "Estatal",
      PP04A == 2 ~ "Privado",
      PP04A == 3 ~ "Otro",
      TRUE ~ NA_character_ 
    ),
    Sector = factor(Sector),
    
    # Antigüedad (PP07A): Recodificación simplificada
    Antiguedad = case_when(
      PP07A %in% c(1, 2, 3, 4) ~ "Menos_1_anio",  
      PP07A == 5 ~ "1_a_5_anios",
      PP07A == 6 ~ "Mas_5_anios",
      TRUE ~ NA_character_
    ),
    Antiguedad = factor(Antiguedad, levels = c("Menos_1_anio", "1_a_5_anios", "Mas_5_anios")),
    
    # Beneficios Laborales (Formalidad)
    Tiene_Vacaciones = factor(ifelse(PP07G1 == 1, "Si", "No")),
    Aporta_Jubilacion = factor(ifelse(PP07I == 1, "Si", "No")),
    
    # Lugar de Trabajo
    Lugar_Trabajo = case_when(
      PP04G %in% c(1, 2, 6, 7, 11, 12, 13) ~ "Establecimiento_Fijo",
      PP04G %in% c(3, 4, 9) ~ "Transito_ViaPublica",
      PP04G %in% c(5, 10, 8) ~ "Construccion_Clientes_Otros",
      TRUE ~ "Otros"
    ),
    Lugar_Trabajo = as.factor(Lugar_Trabajo),
    
    # Variables Numéricas (Mincer)
    Edad = CH06,
    Edad_Cuadrado = CH06^2,
    Horas_Trabajadas = as.numeric(as.character(PP3E_TOT)),
    Horas_Trabajadas = ifelse(Horas_Trabajadas == 999, NA, Horas_Trabajadas)
  )

# Set Inicial: Limpieza estricta de NAs para todas las variables candidatas
# Esto es vital para que step() y los gráficos no fallen por diferencias de filas.
set_entrenamiento_bruto <- datos_procesados %>%
  filter(!is.na(P47T) & P47T > 0 & !is.na(Horas_Trabajadas) & Horas_Trabajadas > 0) %>%
  na.omit() 

# ==============================================================================
# 3. DETECCIÓN Y CORRECCIÓN DE VARIABLES CONSTANTES
# ==============================================================================
# Evita el error "contrasts can be applied only to factors with 2 or more levels"

variables_factor <- c("NIVEL_ED", "CH04", "Aglomerado", "CAT_OCUP", "Sector", 
                      "Antiguedad", "Aporta_Jubilacion", "Tiene_Vacaciones", "Lugar_Trabajo")

cat("\n--- VERIFICACIÓN DE NIVELES DE FACTORES ---\n")
vars_a_eliminar <- c()

for (var in variables_factor) {
  # Contamos niveles reales presentes en el set filtrado
  niveles <- nlevels(droplevels(set_entrenamiento_bruto[[var]]))
  cat(paste(var, ":", niveles, "niveles\n"))
  
  if (niveles < 2) {
    cat(paste("¡ALERTA! La variable", var, "tiene menos de 2 niveles. Se eliminará del análisis.\n"))
    vars_a_eliminar <- c(vars_a_eliminar, var)
  }
}

# Definimos todas las variables candidatas
vars_base <- c("NIVEL_ED", "CH04", "Aglomerado", "Edad", "Edad_Cuadrado", 
               "Horas_Trabajadas", "CAT_OCUP", "Sector", "Antiguedad", 
               "Aporta_Jubilacion", "Tiene_Vacaciones", "Lugar_Trabajo")

# Quitamos las problemáticas automáticamente
vars_finales <- setdiff(vars_base, vars_a_eliminar)

# Construimos la fórmula dinámicamente
formula_base <- as.formula(paste("P47T ~", paste(vars_finales, collapse = " + ")))

print("Fórmula depurada inicial:")
print(formula_base)

# ==============================================================================
# 4. DETECCIÓN DE OUTLIERS (MÉTODO COOK'S DISTANCE)
# ==============================================================================

# Ajustamos modelo preliminar robusto
modelo_preliminar <- lm(formula_base, data = set_entrenamiento_bruto)

# Calculamos Distancia de Cook
cooksD <- cooks.distance(modelo_preliminar)
umbral_cook <- 4 / nrow(set_entrenamiento_bruto)

# Filtramos outliers
set_entrenamiento <- set_entrenamiento_bruto[cooksD < umbral_cook, , drop = FALSE]

cat("\n--- LIMPIEZA DE DATOS (COOK'S DISTANCE) ---\n")
cat("Observaciones originales:", nrow(set_entrenamiento_bruto), "\n")
cat("Observaciones eliminadas:", nrow(set_entrenamiento_bruto) - nrow(set_entrenamiento), "\n")
cat("Set final para modelado:", nrow(set_entrenamiento), "\n")

# ==============================================================================
# 5. SELECCIÓN AUTOMÁTICA DE MODELO (AIC)
# ==============================================================================

cat("\n--- SELECCIÓN AUTOMÁTICA DE VARIABLES (STEPWISE) ---\n")

# Modelo Completo con datos limpios
modelo_full <- lm(formula_base, data = set_entrenamiento)

# Selección automática (Backwards/Forwards)
modelo_optimo <- step(modelo_full, direction = "both", trace = 0) 

cat("El modelo óptimo seleccionado es:\n")
print(formula(modelo_optimo))

# ==============================================================================
# 6. DIAGNÓSTICOS Y RESULTADOS DEL MODELO ÓPTIMO
# ==============================================================================

cat("\n--- RESUMEN DEL MODELO FINAL ---\n")
print(summary(modelo_optimo))

cat("\nIntervalos de Confianza (95%):\n")
print(confint(modelo_optimo))

cat("\n--- Test de Colinealidad (VIF) ---\n")
if(require(car)) print(vif(modelo_optimo, type = 'predictor'))

cat("\n--- Test de Homocedasticidad (Breusch-Pagan) ---\n")
if(require(lmtest)) print(bptest(modelo_optimo))

# ==============================================================================
# 7. VISUALIZACIÓN DE DIAGNÓSTICO Y MÉTRICAS DETALLADAS
# ==============================================================================

# Usamos los datos EXACTOS del modelo para evitar errores de longitud
datos_del_modelo <- modelo_optimo$model

# Agregamos predicciones y residuos
datos_del_modelo$Prediccion <- predict(modelo_optimo)
datos_del_modelo$Residuo <- residuals(modelo_optimo)

# Si 'Aglomerado' fue eliminado por step(), lo recuperamos del set original por índice
# (Aunque usualmente se mantiene por ser significativa)
if(!"Aglomerado" %in% names(datos_del_modelo)) {
  datos_del_modelo$Aglomerado <- set_entrenamiento$Aglomerado[as.numeric(rownames(datos_del_modelo))]
}

# A. Métricas de Rendimiento (R2 Ajustado, MAE, RMSE)
# ------------------------------------------------------------------------------
# Función auxiliar para R2 Ajustado manual por grupo
calc_adj_r2 <- function(r2, n, p) {
  1 - (1 - r2) * ((n - 1) / (n - p - 1))
}
num_predictores <- length(coef(modelo_optimo)) - 1 # Restamos intercepto

etiquetas_metricas <- datos_del_modelo %>%
  group_by(Aglomerado) %>%
  summarise(
    # Métricas de Error
    MAE = mean(abs(Residuo)),
    MSE = mean(Residuo^2),
    RMSE = sqrt(mean(Residuo^2)),
    
    # R2 Simple
    R2 = 1 - (sum(Residuo^2) / sum((P47T - mean(P47T))^2)),
    
    # R2 Ajustado (Aproximado por grupo, asumiendo p proporcional o igual)
    # Para simplificar en gráfico usamos el R2 simple o el Ajustado global si se prefiere
    # Aquí calculamos el ajustado local usando el N del grupo
    R2_Adj = 1 - (1 - R2) * ((n() - 1) / (n() - num_predictores - 1)),
    
    # Texto para el gráfico
    Label = paste0("R² Adj = ", round(R2_Adj, 3), "\nMAE = $", round(MAE, 0)),
    
    X_pos = min(Prediccion), Y_pos = max(P47T) * 0.9
  )

print("\n--- MÉTRICAS DE RENDIMIENTO POR AGLOMERADO ---")
print(etiquetas_metricas)

# Gráfico de Ajuste
p_ajuste <- ggplot(datos_del_modelo, aes(x = Prediccion, y = P47T)) +
  geom_point(alpha = 0.2, aes(color = Aglomerado)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text(data = etiquetas_metricas, aes(x = X_pos, y = Y_pos, label = Label), 
            inherit.aes = FALSE, hjust = 0, size = 4, color = "darkred", fontface = "bold") +
  facet_wrap(~ Aglomerado) +
  scale_x_continuous(labels = scales::label_dollar()) +
  scale_y_continuous(labels = scales::label_dollar()) +
  labs(title = "Modelo Optimizado: Predicho vs Real", 
       subtitle = "Métricas: R² Ajustado y Error Medio Absoluto (MAE)") +
  theme_bw() + theme(legend.position = "none")

print(p_ajuste)

# B. Normalidad de Residuos (Q-Q Plot)
p_qq <- ggplot(datos_del_modelo, aes(sample = Residuo)) +
  stat_qq(alpha = 0.5, aes(color = Aglomerado)) + 
  stat_qq_line(color = "black", linetype="dashed") +
  facet_wrap(~ Aglomerado) +
  labs(title = "Normalidad de Residuos (Q-Q Plot)") +
  theme_bw() + theme(legend.position = "none")

print(p_qq)

# C. Homocedasticidad
p_homo <- ggplot(datos_del_modelo, aes(x = Prediccion, y = Residuo)) +
  geom_point(alpha = 0.2, aes(color = Aglomerado)) +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed") +
  facet_wrap(~ Aglomerado) +
  scale_y_continuous(labels = scales::label_dollar()) +
  scale_x_continuous(labels = scales::label_dollar()) +
  labs(title = "Homocedasticidad: Residuos vs Predichos") +
  theme_bw() + theme(legend.position = "none")

print(p_homo)

# ==============================================================================
# 8. IMPUTACIÓN FINAL
# ==============================================================================

# Preparamos el set a imputar asegurando que tenga las variables necesarias
set_a_imputar <- datos_procesados %>%
  filter(is.na(P47T) | P47T <= 0) %>%
  filter(!is.na(Horas_Trabajadas)) %>%
  # Importante: Debemos tener datos en todas las variables que quedaron en el modelo
  # Usamos na.omit() sobre las columnas seleccionadas en la formula final
  # (Esto es simplificado; idealmente se imputan las predictoras faltantes primero)
  na.omit()

if(nrow(set_a_imputar) > 0) {
  # Predecimos
  set_a_imputar$P47T_Imputado <- predict(modelo_optimo, newdata = set_a_imputar)
  # Corregimos negativos (el modelo lineal puede darlos)
  set_a_imputar$P47T_Imputado <- ifelse(set_a_imputar$P47T_Imputado < 0, 0, set_a_imputar$P47T_Imputado)
  
  cat("\n--- MUESTRA DE IMPUTACIÓN EXITOSA ---\n")
  print(head(set_a_imputar %>% select(Aglomerado, NIVEL_ED, P47T_Imputado)))
  
  # Opcional: Guardar
  # write.csv(set_a_imputar, "EPH_Imputada_2025.csv")
} else {
  cat("\nAdvertencia: No quedaron casos aptos para imputar (faltan datos en variables predictoras).\n")
}