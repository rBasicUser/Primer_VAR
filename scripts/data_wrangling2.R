library(here)
library(readxl)
library(tidyverse)


# 1. Carga de datos
df_excel <- read_excel(here("data","data.xlsx"),sheet="Sheet1")

# 2. Consolidado de información

df_unido <- df_excel %>%
  full_join(df_activo, by = "Fecha") %>%
  full_join(df_cvc,    by = "Fecha")|>
  drop_na()

# 3. Consolidado

write_csv(df_unido,here("Output","consolidado.csv"))
