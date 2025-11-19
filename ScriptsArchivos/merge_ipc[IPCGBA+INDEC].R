library(tidyverse)
library(readr)
library(lubridate)

# -----------------------------------------------------------------------------
# 1. CARGA Y AGREGACIÓN DE IPC 2016 (Empalme)
# -----------------------------------------------------------------------------

# (Enero 2016 a Diciembre 2016, base 100 en Diciembre).
ipc_2016_mensual <- tibble(
  ANO4 = 2016,
  MES = 4:12,
  IPC_MENSUAL = c(85.5, 89.1, 91.9, 93.7, 93.9, 95.0, 97.2, 98.8, 100.0)
)

# Agregamos los datos de 2016 a nivel trimestral (promedio de los 3 meses)
ipc_2016_trimestral <- ipc_2016_mensual %>%
  mutate(
    TRIMESTRE = case_when(
      MES %in% 4:6 ~ 2,
      MES %in% 7:9 ~ 3,
      MES %in% 10:12 ~ 4
    )
  ) %>%
  group_by(ANO4, TRIMESTRE) %>%
  summarise(
    VALOR_IPC = mean(IPC_MENSUAL), # Promedio trimestral
    FUENTE = "IPC GBA",
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 2. CARGA Y LIMPIEZA DE DATOS INDEC (2017-2025)
# -----------------------------------------------------------------------------

df_indec <- read_csv("D:/EPH-2016_2025/indice-precios-al-consumidor-nivel-general-base-diciembre-2016-trimestral.csv")

ipc_indec <- df_indec %>%
  select(indice_tiempo, ipc_ng_gba) %>%
  mutate(
    FECHA = as.Date(indice_tiempo),
    ANO4 = year(FECHA),
    TRIMESTRE = quarter(FECHA),
    FUENTE = "INDEC"
  ) %>%
  rename(VALOR_IPC = ipc_ng_gba) %>%
  select(ANO4, TRIMESTRE, VALOR_IPC, FUENTE)

# -----------------------------------------------------------------------------
# 3. EMPALME FINAL Y CÁLCULO DE COEFICIENTE DE AJUSTE
# -----------------------------------------------------------------------------

ipc_final <- bind_rows(ipc_2016_trimestral, ipc_indec) %>%
  arrange(ANO4, TRIMESTRE) %>%
  mutate(PERIODO = paste0(ANO4, "-T", TRIMESTRE))

# --- Definir la BASE para el Ingreso Real ---
# Usaremos el 1er Trimestre de 2025 como base, para que los ingresos se lean 
# en "pesos constantes de 2 trimestre de 2025".

ipc_base <- ipc_final %>%
  filter(ANO4 == 2025, TRIMESTRE == 2) %>%
  pull(VALOR_IPC)

# Calcular Coeficiente Deflactor (COEF_AJUSTE)
ipc_final <- ipc_final %>%
  mutate(
    COEF_AJUSTE = ipc_base / VALOR_IPC #4000
  )

print("Tabla IPC Final Completa (2016 T1 a 2025 T2):")
print(ipc_final)