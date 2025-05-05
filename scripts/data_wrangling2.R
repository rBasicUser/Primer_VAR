library(here)
library(readxl)
library(tidyverse)
library(seasonal)   # o tu método preferido



# 1. Carga de datos
df_excel <- read_excel(here("data","data.xlsx"),sheet="Sheet1")

df_activo <- read_csv(here("Output","suma_activo_por_fecha.csv"))
df_cvc <- read_csv(here("Output","promedio_cvc_por_fecha.csv"))    
df_wavg <- read_csv(here("Output","promedio_ponderado_cvc.csv"))

# 2. Consolidado de información

df_unido <- df_excel %>%
  full_join(df_activo, by = "Fecha") %>%
  full_join(df_cvc,    by = "Fecha")|>
  full_join(df_wavg, by = "Fecha") |>
  drop_na()


# 1. Calcular media móvil de 12 meses (centrada)
df_ma <- df_unido %>%
  arrange(Fecha) %>%
  mutate(
    IMAE_ma12 = rollmean(IMAE, k = 12, align = "center", fill = NA)
  )

# 2. Definir crecimiento tendencial
g_anual   <- 0.035
g_mensual <- log(1 + g_anual) / 12

# 3. Construir senda potencial sobre la media móvil
df_gap <- df_ma %>%
  filter(!is.na(IMAE_ma12)) %>%            # eliminar los bordes sin MA
  mutate(
    meses      = as.numeric(difftime(Fecha, min(Fecha), units = "days")) / 30.4375,
    trend_ma12 = first(IMAE_ma12) * exp(g_mensual * meses),
    gap_pct    = 100 * (log(IMAE_ma12) - log(trend_ma12))
  )

# 4. Gráfica del output‑gap
ggplot(df_gap, aes(Fecha, gap_pct)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Brecha del producto (IMAE) Guatemala – MA12 y tendencia 3.5 % anual",
    y     = "Brecha (%)",
    x     = NULL
  )


# Escribir la serie

write_csv(imae,here("Output","imae.csv"))
