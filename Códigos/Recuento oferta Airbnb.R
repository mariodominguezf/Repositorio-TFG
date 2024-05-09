library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Cargar el archivo CSV
data <- read.csv("calendar/oferta_2023.csv", header=TRUE)

# Convertimos las fechas a formato fecha
data$date <- as.Date(data$date)

# Obtenemos el mes de las fechas
data$month <- format(data$date, "%m")

# Calcular el número de ofertas totales y mensuales por listing_id
oferta_totales_por_listing <- data %>%
  group_by(listing_id) %>%
  summarize(oferta_totales = n())

oferta_mensuales_por_listing <- data %>%
  group_by(listing_id, month) %>%
  summarize(oferta_mensuales = n())

# Pivotar la tabla
oferta_mensuales_pivot <- oferta_mensuales_por_listing %>%
  pivot_wider(names_from = month, values_from = oferta_mensuales) %>%
  replace_na(list(0))

oferta_2023 <- data %>%
  group_by(month) %>%
  summarize(oferta_mensuales = n())

# Guardamos las dos tablas
# 1) Oferta total anual por Aibnb 
write.csv(oferta_totales_por_listing, file = "oferta_anuales_por_Aribnb_2023.csv")
# 2) Oferta total mensual por Airbnb
write.csv(oferta_mensuales_pivot, file = "oferta_mensuales_por_Aribnb_2023.csv")
# 3) Oferta total en 2023
write.csv(oferta_2023, file = "oferta_2023.csv")


# Representación reservas totales por mes ---------------------------------

library(dplyr)

data_mensual <- data %>%
  group_by(month) %>%
  summarise(n = n())

ggplot(data_mensual, aes(x = month, y = n, fill = month)) +
  geom_bar(stat = "identity") +
  labs(title = "Oferta total por mes",
       x = "Meses",
       y = "Oferta total") +
  theme_minimal() +
  scale_fill_brewer(palette = "Spectral") + 
  scale_y_continuous(labels = label_number(accuracy = 1))
