library(sf)
library(dplyr)
library(tmap) # Para mapas temáticos
library(viridis) # Para la paleta de colores
library(RColorBrewer) # Paletas de colores (opcional)

# -----------------------------------------------------
# 1. Preparación de Datos (similar a tu script anterior)
# -----------------------------------------------------

# Asumo que `base_con_ingresos_reales` existe y tiene P47T_REAL y PONDII.
# Asumo que `ipc_final` también existe.
# Asumo que `filtered_db` está limpio y listo.

# Cargar el archivo JSON de aglomerados (asegura el path correcto)
datos_geo <- st_read("D:/EPH-2016_2025/InformeEPH/DatosGeorreferenciados/aglomerados_eph.json")

# 1. Filtrar, agrupar y calcular la Mediana
# Usaremos la base de ingresos limpia si está disponible, o aseguramos limpieza aquí.
resumen_2020 <- base_con_ingresos_reales %>%
  filter(ANO4 == 2020, TRIMESTRE == 2) %>%
  mutate(AGLOMERADO = sprintf("%02d", as.numeric(AGLOMERADO))) %>% # Formatear AGLOMERADO a "01", "02"
  group_by(AGLOMERADO) %>%
  summarise(
    MEDIANA = Hmisc::wtd.quantile(P47T_REAL, PONDII, probs = 0.5, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Unir shapefile + datos
mapa_2020 <- datos_geo %>%
  filter(
    (eph_codagl == "32") |
      (eph_codagl == "33" & codaglo %in% c("0001"))
  ) %>%
  left_join(resumen_2020, by = c("eph_codagl" = "AGLOMERADO")) %>%
  filter(!is.na(MEDIANA))


# -----------------------------------------------------
# 2. CREACIÓN DEL MAPA CON TMAP
# -----------------------------------------------------

# Configurar tmap en modo de dibujo
# tm_mode("view") # Si quieres un mapa interactivo (como Leaflet)
tmap_mode("plot")


# Crear el mapa temático
mapa_final <- tm_shape(mapa_2020) +
  # --- CAPA DE MAPA BASE (de OpenStreetMap, Stamen, etc.) ---
  # El argumento `basemaps` es la clave para la imagen de fondo.
  tm_basemap(server = "OpenStreetMap") + # Puedes probar "Stamen.TonerLite", "Esri.WorldTopoMap"
  
  # --- CAPA DE POLÍGONOS CON COLORES TEMÁTICOS (Choropleth) ---
  tm_polygons(
    col = "MEDIANA",
    palette = "reds",
    style = "equal",     # <--- Esto muestra los valores reales
    title = "Mediana Ingreso Real",
    border.col = "black",
    lwd = 1.5,
    alpha = 0.7
  ) +
  
  # --- ETIQUETAS DE TEXTO (opcional, para nombres de aglomerados) ---
  
  # --- ELEMENTOS DEL MAPA ---
  tm_layout(
    title = "Mediana del Ingreso Real por Aglomerado – T2 2020",
    title.position = c("left", "top"),
    frame = FALSE, # Elimina el marco alrededor del mapa
    legend.position = c("right", "bottom"), # Posición de la leyenda
    legend.text.size = 0.8,
    main.title.size = 1.2,
    bg.color = "white" # Color de fondo
  ) +
  
  tm_credits("(C) OpenStreetMap contributors", position = c("left", "bottom")) +
  
  tm_compass(type = "arrow", position = c("right", "top")) + # Brújula
  tm_scale_bar(position = c("left", "bottom")) + # Barra de escala
  tm_text("eph_aglome", size = 1.3, col = "orange", shadow = TRUE)



# Imprimir el mapa
mapa_final

head(datos_geo)



# Para guardar el mapa como imagen (similar al screenshot)
# tmap_save(mapa_final, filename = "mapa_ingresos_2020.png", width = 8, height = 10, dpi = 300)



library(dplyr)
library(Hmisc)

medianas_trimestrales <- base_con_ingresos_reales %>%
  mutate(
    AGLOMERADO = sprintf("%02d", as.numeric(AGLOMERADO)),
    ANIO = ANO4
  ) %>%
  group_by(ANIO, TRIMESTRE, AGLOMERADO) %>%
  summarise(
    MEDIANA_TRIM = Hmisc::wtd.quantile(
      P47T_REAL, PONDII,
      probs = 0.5, na.rm = TRUE
    ),
    .groups = "drop"
  )


mapa_anual <- datos_geo %>%
  filter(
    (eph_codagl == "32") |
      (eph_codagl == "33" & codaglo %in% c("0001"))
  ) %>%
  left_join(resumen_anual, by = c("eph_codagl" = "AGLOMERADO"))


resumen_anual <- medianas_trimestrales %>%
  group_by(ANIO, AGLOMERADO) %>%
  summarise(
    MEDIANA = mean(MEDIANA_TRIM, na.rm = TRUE),
    .groups = "drop"
  )


tmap_mode("plot")

library(viridisLite)

min_v <- min(mapa_anual$MEDIANA, na.rm = TRUE)
max_v <- max(mapa_anual$MEDIANA, na.rm = TRUE)

panel_2016_2025 <- tm_shape(mapa_anual) +
  tm_polygons(
    col = "MEDIANA",
    title = "Mediana Ingreso Real (P47T)",
    palette = viridisLite::magma(9, direction=-1),        # 🔥 paleta muy contrastada
    breaks = seq(min_v, max_v, length.out = 9),
    border.col = "grey20",
    lwd = 0.7
  ) +
  tm_text("eph_aglome", size = 1.1, shadow = TRUE, col="#42C953", fontface = "bold") +
  tm_facets(by = "ANIO", ncol = 2, free.scales = FALSE) +
  tm_layout(
    panel.label.size = 1.2,
    legend.text.size = 0.8,
    legend.outside = TRUE,
    outer.margins = c(0,0,0,0),
    inner.margins = c(0,0,0,0),
    panel.show = TRUE
  ) +
  tm_basemap(server = "CartoDB.Positron")


tmap_save(
  panel_2016_2025,
  filename = "panel_2016_2025.png",
  width = 20,   # más ancho → mapas más grandes
  height = 12,  # más alto → mapas más grandes
  dpi = 300
)

panel_2016_2025
