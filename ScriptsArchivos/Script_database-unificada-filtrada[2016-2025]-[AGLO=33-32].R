library(tidyverse)

# Identifier for periods
database <- bind_rows(df_list, .id = "ID_PERIODO")

# Filter variable
aglomerados_seleccionados <- c(32, 33)

# Normalize the database and create time variables. 
filtered_db <- database %>%
  mutate(
    
    # Normalize cols
    P47T = str_replace(P47T, ",", "."),
    
    #Transform to numeric
    across(
      c(P47T, ESTADO, AGLOMERADO, ANO4, TRIMESTRE, PONDII, PONDERA, CH06), 
      as.numeric
    ),
    
    #Create time variable
    FECHA = as.Date(paste0(ANO4, "-", (TRIMESTRE * 3 - 1), "-01"))    
    
    #Filter 
  ) %>% filter(
    AGLOMERADO %in% aglomerados_seleccionados
  ) %>%
  mutate(
    AGLO_NOMBRE = case_when(
      AGLOMERADO == 32 ~ "CABA",
      AGLOMERADO == 33 ~ "GBA",
      TRUE ~ as.character(AGLOMERADO)
    )
  )