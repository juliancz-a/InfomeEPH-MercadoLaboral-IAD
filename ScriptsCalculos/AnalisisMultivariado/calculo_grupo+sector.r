library(dplyr)
library(stringr)

filtered_db <- filtered_db %>%
  mutate(
    # --- CORRECCIÓN CLAVE PARA EVITAR "Sin clasificar" ---
    PP04D_COD = sprintf("%02d", as.numeric(PP04D_COD)),
    COD_CNO   = str_sub(PP04D_COD, 1, 2),
    
    # Actividad (CAES)
    DIV_CAES = suppressWarnings(as.numeric(str_sub(PP04B_COD, 1, 2))),
    
    SECTOR_INDUSTRIA = case_when(
      DIV_CAES %in% 1:3 | DIV_CAES %in% 5:9 ~ "1. Primario (Agro y Minería)",
      DIV_CAES %in% 10:33 ~ "2. Manufactura",
      DIV_CAES == 35 ~ "3. Electricidad, Gas y Energía",
      DIV_CAES %in% 36:39 ~ "4. Agua, Saneamiento y Desechos",
      DIV_CAES == 40 ~ "5. Construcción",
      DIV_CAES %in% c(45,48) ~ "6. Comercio",
      DIV_CAES %in% 49:53 ~ "7. Transporte y Logística",
      DIV_CAES %in% 55:96 ~ "8. Servicios",
      DIV_CAES %in% 97:98 ~ "9. Hogares Empleadores",
      DIV_CAES == 99 ~ "10. Organismos Internacionales",
      TRUE ~ "Sin Clasificar / Error"
    ),
    
    GRUPO_OCUPACIONAL = case_when(
      COD_CNO %in% c("00","01","02","03","04","05","06","07",
                     "10","11","20") ~ "1. Dirección, Gestión y Administración",
      
      COD_CNO %in% c("30","31","32","33","34","35","36") ~ 
        "2. Comercio, Transporte, Logística y Telecomunicaciones",
      
      COD_CNO %in% c("40","41","42","43","44","45","46",
                     "50","51","52") ~ 
        "3. Servicios Profesionales, Sociales y Culturales",
      
      COD_CNO %in% c("47","48","49","53","54","55","56","57","58") ~
        "4. Servicios Personales, Cuidado, Seguridad y Domésticos",
      
      COD_CNO %in% c("60","61","62","63","64","65") ~
        "5. Agro, Forestal, Pesca y Caza",
      
      COD_CNO %in% c("70","71","72") ~
        "6. Construcción, Energía y Extracción",
      
      COD_CNO %in% c("80","81","82") ~
        "7. Industria, Manufactura, Software y Reparación",
      
      COD_CNO %in% c("90","91","92") ~
        "8. Tecnología, Instalación, Mantenimiento y Sistemas",
      
      TRUE ~ "9. Sin clasificar"
    )
  ) %>%
  select(-DIV_CAES)   # ← elimina auxiliar

base_con_ingresos_reales <- filtered_db %>%
  # El left_join arrastra todas las columnas de filtered_db, incluyendo GRUPO_OCUPACIONAL
  left_join(ipc_final, by = c("ANO4", "TRIMESTRE")) %>%
  
  # Filtrar el universo de análisis: Ocupados que declararon ingreso
  filter(ESTADO == 1, P47T > 0) %>%
  
  # Calcular el Ingreso Real (Ajustado)
  mutate(
    P47T_REAL = P47T * COEF_AJUSTE
  )