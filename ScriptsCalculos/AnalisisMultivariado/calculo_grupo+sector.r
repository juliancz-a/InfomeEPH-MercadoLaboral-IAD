library(stringr)

filtered_db <- filtered_db %>%
  mutate(
    DIV_CAES = as.numeric(str_sub(PP04B_COD, 1, 2)),
    PRIMER_DIGITO_CNO = str_sub(PP04D_COD, 1, 1),
    
    SECTOR_INDUSTRIA = case_when(
      DIV_CAES %in% 1:3 | DIV_CAES %in% 5:9 ~ "1. Primario (Agro y Minería)",
      DIV_CAES %in% 10:33 ~ "2. Manufactura",
      DIV_CAES == 35 ~ "2. Electricidad, Gas y Energía",
      DIV_CAES %in% 36:39 ~ "2. Agua, Saneamiento y Desechos",
      DIV_CAES == 40 ~ "2. Construcción",
      DIV_CAES %in% c(45,48) ~ "3. Comercio",
      DIV_CAES %in% 49:53 ~ "3. Transporte y Logística",
      DIV_CAES %in% 55:96 ~ "3. Servicios",
      DIV_CAES %in% 97:98 ~ "3. Hogares Empleadores",
      DIV_CAES == 99 ~ "3. Organismos Internacionales",
      TRUE ~ "9. Sin Clasificar / Error"
    ),
    
    GRUPO_OCUPACIONAL = case_when(
      PRIMER_DIGITO_CNO %in% c("0","1","2") ~ "1. Directivos y Profesionales",
      PRIMER_DIGITO_CNO == "3" ~ "2. Técnicos",
      PRIMER_DIGITO_CNO %in% c("4","5") ~ "3. Administrativos, Servicios y Comercio",
      PRIMER_DIGITO_CNO == "6" ~ "4. Trabajo Agrario",
      PRIMER_DIGITO_CNO == "7" ~ "5. Construcción y Producción Extractiva",
      PRIMER_DIGITO_CNO == "8" ~ "6. Industria y Mecánica",
      PRIMER_DIGITO_CNO == "9" ~ "7. Instalación, Mantenimiento y Oficios Técnicos",
      TRUE ~ "9. Sin Clasificar"
    )
  ) %>%
  select(-DIV_CAES, -PRIMER_DIGITO_CNO)   # ← BORRA columnas auxiliares

base_con_ingresos_reales <- filtered_db %>%
  # El left_join arrastra todas las columnas de filtered_db, incluyendo GRUPO_OCUPACIONAL
  left_join(ipc_final, by = c("ANO4", "TRIMESTRE")) %>%
  
  # Filtrar el universo de análisis: Ocupados que declararon ingreso
  filter(ESTADO == 1, P47T > 0) %>%
  
  # Calcular el Ingreso Real (Ajustado)
  mutate(
    P47T_REAL = P47T * COEF_AJUSTE
  )
