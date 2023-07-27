library(easypackages)
libraries("tidyverse","fpp3","patchwork","plotly","readxl","lubridate")
setwd("~/Time Series P2022")

# Importamos archivos excel
df1 <- read_excel("VP.xlsx") %>% mutate(Month = as.Date(Month, format = "%d-%m-%Y"))
df2 <- read_excel("NDS.xlsx") %>% mutate(Month = as.Date(Month, format = "%d-%m-%Y"))

# Convertimos los datos al tipo tsibble
df_vp <- df1 %>% mutate(Month = yearmonth(as.character(Month))) %>% 
                 as_tsibble(index = Month) %>% filter_index(.~"2021 Dec")
df_nds <- df2 %>% mutate(Month = yearmonth(as.character(Month))) %>% 
                  as_tsibble(index = Month) %>% filter_index(.~"2021 Dec")

# ------------------------ Pronósticos para el Volumen ------------------------

# Graficamos los datos
df_vp %>% autoplot(Volume) + ylab("Volume") + xlab("Date")

# Ajustamos un modelo
fit_vp <- df_vp %>%
  model(SES   = ETS(Volume ~ error("A") + trend("N") + season("N"), opt_crit = "mse"),
        NAIVE = NAIVE(Volume),
        Drift = RW(Volume ~ drift()),
        Mean = MEAN(Volume),
        Seasonal_NAIVE = SNAIVE(Volume)
        )

# Pronosticamos el siguiente año
fc_vp <- fit_vp %>% forecast(h = 12)

# Gráficamos el pronóstico
fc_vp %>% autoplot(df_vp %>% filter_index("2010 Jan"~.), level = NULL) +
  #geom_line(aes(y = .fitted, colour = "Fitted"), data = augment(fit_vp)) +
  ylab("Volume") + xlab("Year")





# ------------- Pronósticos para el número de nuevos distribuidores-------------

# Graficamos los datos
df_nds %>% autoplot(`# New Distributor`) + ylab("# New Distributor") + xlab("Date")

# Ajustamos un modelo
fit_vp <- df_nds %>%
  model(SES   = ETS(`# New Distributor` ~ error("A") + trend("N") + season("N"), opt_crit = "mse"),
        NAIVE = NAIVE(`# New Distributor`))

# Pronosticamos el siguiente año
fc_vp <- fit_vp %>% forecast(h = 12)

# Gráficamos el pronóstico
fc_vp %>% autoplot(df_nds %>% filter_index("2010 Jan"~.), level = NULL) +
  #geom_line(aes(y = .fitted, colour = "Fitted"), data = augment(fit_vp)) +
  ylab("# New Distributor") + xlab("Year")
