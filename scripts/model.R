# --------------------------- #
#  Mini-VAR Riesgo Sistémico  #
# --------------------------- #

# 0. Paquetes
library(here)
library(tidyverse)
library(vars)
library(tseries)    # ADF tests
library(urca)       # PP / KPSS
library(tsibble)    # manejo series

# 1. Importar & preparar datos ----------------------------
# Reemplace las rutas por su directorio
df_raw <- read_csv(here("Output","consolidado.csv")) |> 
  mutate(date = yearmonth(Fecha))|>       # yyyy-mm
  as_tsibble(index = date)|>
  dplyr::select(-Fecha)

# Serie: IMAE
adf_IMAE <- adf.test( na.omit(df_raw$IMAE), k = 12 )
p_IMAE   <- adf_IMAE$p.value

# Serie: Tasa de Interés Líder
adf_PIB  <- adf.test( na.omit(df_raw$TPolMon), k = 12 )
p_PIB    <- adf_PIB$p.value

# Serie: Índice de Morosidad
adf_IMor  <- adf.test( na.omit(df_raw$wavg_CVC), k = 12 )
p_IMor    <- adf_IMor$p.value


print(adf_results)
#   Si p_adf > 0.05 -> serie no estacionaria; diferencie

df_diff <- df_raw %>%
  arrange(date) %>%                               # asegúrate de ordenar por fecha
  mutate(
    across(
      .cols = -date,
      .fns  = ~ . - lag(.),
      .names = "{.col}_diff"
    )
  ) %>%
  slice(-1)|>
  dplyr::select(ends_with("_diff"))

# 3. Selección de rezagos ---------------------------------
lagSel <- VARselect(df %>% select(-date), lag.max = 6, type = "const")
p <- lagSel$selection["SC(n)"]      # usar Schwarz

# 4. Estimación del VAR -----------------------------------
var_mod <- VAR(df %>% select(-date), p = p, type = "const")

summary(var_mod)

# 5. Diagnósticos rápidos ---------------------------------
serial.test(var_mod, lags.pt = 12, type = "PT.asymptotic")
arch.test(var_mod, lags.multi = 12)

# 6. Choque de +100 pb en policy --------------------------
irf_mod <- irf(var_mod, impulse = "policy", response = "risk",
               n.ahead = 12, ortho = TRUE, boot = TRUE, runs = 1000)

plot(irf_mod)      # trayectoria de la morosidad 12 meses después del shock

# 7. Extraer magnitud pico del efecto ---------------------
peak <- irf_mod$irf$risk %>% max()
cat("Máximo incremento en riesgo bancario (%):", round(peak*100, 2))
