# Instala estos paquetes si aún no los tienes:
# install.packages(c("tidyverse", "openxlsx", "here"))

library(tidyverse)   # incluye readr, dplyr, tidyr, ggplot2…
library(openxlsx)    # para Excel (si luego lo necesitas)
library(here)        # para rutas reproducibles

# 1. Carga de datos
df <- read_csv(here("data", "infofin.csv"))

# 2. Transformación a formato largo
df_long <- df %>%
  pivot_longer(
    cols = matches("^\\d{1,2}/\\d{1,2}/\\d{4}$"),
    names_to  = "Fecha",
    values_to = "Valor"
  ) %>%
  mutate(
    Fecha = as.Date(Fecha, "%m/%d/%Y"),
    Valor = parse_number(Valor)     
  )

# 3. Filtrar sólo los rubros que te interesan
rubros <- c(
  "ACTIVO",
  "Cartera de créditos vencida con relación a cartera de créditos bruta (CVC)"
)

df_sel <- df_long %>%
  filter(INFOFINANCIERA %in% rubros)

# 3. Tibble para suma de ACTIVO por fecha
df_activo <- df_sel %>%
  filter(INFOFINANCIERA == "ACTIVO") %>%
  group_by(Fecha) %>%
  summarise(
    total_activo = sum(Valor, na.rm = TRUE),
    .groups      = "drop"
  )

# 4. Tibble para promedio de CVC por fecha
df_cvc <- df_sel %>%
  filter(INFOFINANCIERA == "Cartera de créditos vencida con relación a cartera de créditos bruta (CVC)") %>%
  group_by(Fecha) %>%
  summarise(
    promedio_cvc = mean(Valor, na.rm = TRUE),
    .groups       = "drop"
  )

# 5. Gráfico de ACTIVO
ggplot(df_activo, aes(x = Fecha, y = total_activo)) +
  geom_line(size = 1) +
  labs(
    title = "Evolución de ACTIVO (suma por fecha)",
    x     = "Fecha",
    y     = "Total ACTIVO"
  ) +
  theme_minimal()

# 6. Gráfico de CVC
ggplot(df_cvc, aes(x = Fecha, y = promedio_cvc)) +
  geom_line(size = 1) +
  labs(
    title = "Evolución de CVC (promedio por fecha)",
    x     = "Fecha",
    y     = "Promedio CVC"
  ) +
  theme_minimal()



# 5. Preparar tabla wide por BANCO y Fecha para el ponderado
df_wide <- df_sel %>%
  pivot_wider(
    id_cols     = c(BANCO, Fecha),
    names_from  = INFOFINANCIERA,
    values_from = Valor
  ) %>%
  rename(
    ACTIVO = `ACTIVO`,
    CVC    = `Cartera de créditos vencida con relación a cartera de créditos bruta (CVC)`
  )

# 6. Calcular promedio ponderado de CVC según ACTIVOS
df_wavg <- df_wide %>%
  group_by(Fecha) %>%
  summarise(
    wavg_CVC = sum(CVC * ACTIVO, na.rm = TRUE) / sum(ACTIVO, na.rm = TRUE),
    .groups  = "drop"
  )

# 7. Graficar el promedio ponderado
ggplot(df_wavg, aes(x = Fecha, y = wavg_CVC)) +
  geom_line(size = 1) +
  labs(
    title = "Promedio ponderado de CVC (peso = ACTIVO)",
    x     = "Fecha",
    y     = "CVC promedio ponderado"
  ) +
  theme_minimal()

# 8. Escribir la data

# 8. Exportar los tibbles a CSV
write_csv(df_wavg, here("Output", "promedio_ponderado_cvc.csv"))
write_csv(df_activo, here("Output", "suma_activo_por_fecha.csv"))
write_csv(df_cvc,   here("Output", "promedio_cvc_por_fecha.csv"))

