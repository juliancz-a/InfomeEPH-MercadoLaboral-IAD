# ---- Mediana ponderada ----
weighted_median <- function(x, w) {
  df <- data.frame(x, w)
  df <- df[order(df$x), ]
  cum_w <- cumsum(df$w) / sum(df$w)
  df$x[which(cum_w >= 0.5)[1]]
}

# ---- Mediana ANUAL en pesos ----
sector_mediana_anual_pesos <- base_con_ingresos_reales %>%
  group_by(AGLO_NOMBRE, SECTOR_INDUSTRIA, ANO4) %>%
  summarise(
    MEDIANA_INGRESO_REAL = weighted_median(P47T_REAL, PONDII),
    .groups = "drop"
  ) %>%
  arrange(AGLO_NOMBRE, SECTOR_INDUSTRIA, ANO4)

library(dplyr)
library(tidyr)
library(scales)

tabla_anual_pesos <- sector_mediana_anual_pesos %>%
  mutate(
    MEDIANA_INGRESO_REAL = dollar(
      MEDIANA_INGRESO_REAL,
      prefix = "$",
      big.mark = ".",
      decimal.mark = ",",
      accuracy = 1
    )
  ) %>%
  select(AGLO_NOMBRE, SECTOR_INDUSTRIA, ANO4, MEDIANA_INGRESO_REAL) %>%
  pivot_wider(
    names_from = ANO4,
    values_from = MEDIANA_INGRESO_REAL
  ) %>%
  arrange(AGLO_NOMBRE, SECTOR_INDUSTRIA)


library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, "Mediana Anual (%)")

writeData(wb, 1, tabla_anual_pesos)

freezePane(wb, 1, firstRow = TRUE, firstCol = TRUE)

# Estilo encabezado
header_style <- createStyle(
  fontColour = "white",
  fgFill = "#4F81BD",
  halign = "center",
  textDecoration = "bold",
  border = "Bottom"
)

addStyle(wb, 1, header_style, rows = 1, cols = 1:ncol(tabla_anual), gridExpand = TRUE)

# Zebra
zebra <- createStyle(fgFill = "#F2F2F2")
conditionalFormatting(wb, 1,
                      cols = 1:ncol(tabla_anual),
                      rows = 2:(nrow(tabla_anual) + 1),
                      type = "expression",
                      rule = "MOD(ROW(),2)=0",
                      style = zebra)

# Heatmap vertical (por columna = año)
conditionalFormatting(
  wb, sheet = 1,
  cols = 3:ncol(tabla_anual),
  rows = 2:(nrow(tabla_anual) + 1),
  type = "colorScale",
  style = c("white", "#FFE699", "#FF8C00")  # Blanco → Amarillo → Naranja
)

saveWorkbook(wb, "tabla_anual_sector_pesos.xlsx", overwrite = TRUE)

library(ggplot2)

ggplot(
  sector_mediana_anual_pesos,
  aes(x = factor(ANO4), y = MEDIANA_INGRESO_REAL,
      fill = SECTOR_INDUSTRIA)
) +
  geom_col(position = "dodge") +
  facet_wrap(~ AGLO_NOMBRE, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  labs(
    title = "Mediana Anual del Ingreso Real por Sector de Actividad",
    subtitle = "Valores ajustados según IPC – Ponderación PONDII",
    x = "Año",
    y = "Mediana del Ingreso Real",
    fill = "Sector de Actividad"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
