library(readr)
library(purrr)
library(stringr)

path <- "D:/EPH-2016_2025/EPH_Individual"

files <- list.files(
  path = path,
  pattern = "\\.txt$|\\.txt\\.txt$|_txt$",
  full.names = TRUE,
  recursive = TRUE
)

cols <- c(
  "ANO4", "TRIMESTRE", "AGLOMERADO", "ESTADO", "CH06", "P47T", "PONDERA", "PONDII",
  "NIVEL_ED", "CH04", "PP04B_COD", "PP04D_COD", "PP04G", "PP3E_TOT", "CAT_OCUP",
  "PP07H", "PP04A", "PP07A", "PP07G1", "PP07I"
)

df_names <- basename(files) %>%
  sub("\\.txt\\.txt$", "", .) %>%
  sub("\\.txt$", "", .) %>%
  sub("_txt$", "", .)

df_list <- files %>%
  map(
    ~ read_delim(
      .x,  # .x (actual file)
      delim = ";",
      
      #Select only necessary cols
      col_types = cols(.default = "c"),
      col_select = all_of(cols),

      show_col_types = FALSE
    )
  ) %>%
  set_names(df_names) # Clean names assignment

print("DF Loaded on list:")
names(df_list)

