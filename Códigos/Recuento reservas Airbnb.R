library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Cargar el archivo CSV
data <- read.csv("calendar/ocupados_2023.csv", header=TRUE)

# Convertimos las fechas a formato fecha
data$date <- as.Date(data$date)

# Obtenemos el mes de las fechas
data$month <- format(data$date, "%m")

# Calcular el número de reservas totales y mensuales por listing_id
reservas_totales_por_listing <- data %>%
  group_by(listing_id) %>%
  summarize(reservas_totales = n())

reservas_mensuales_por_listing <- data %>%
  group_by(listing_id, month) %>%
  summarize(reservas_mensuales = n())

# Pivotar la tabla
reservas_mensuales_pivot <- reservas_mensuales_por_listing %>%
  pivot_wider(names_from = month, values_from = reservas_mensuales) %>%
  replace_na(list(0))

demanda_2023 <- data %>%
  group_by(month) %>%
  summarize(reservas_mensuales = n())

# Guardamos las dos tablas
# 1) Reservas totales anuales por Aibnb 
write.csv(reservas_totales_por_listing, file = "reservas_anuales_por_Aribnb_2023.csv")
# 2) Reservas totales mensuales por Airbnb
write.csv(reservas_mensuales_pivot, file = "reservas_mensuales_por_Aribnb_2023.csv")
# 3) Reservas totales mensuales en 2023
write.csv(demanda_2023, file = "demanda_2023.csv")


# Representación reservas totales por mes ---------------------------------

library(dplyr)

data_mensual <- data %>%
  group_by(month) %>%
  summarise(n = n())

ggplot(data_mensual, aes(x = month, y = n, fill = month)) +
  geom_bar(stat = "identity") +
  labs(title = "Reservas totales por mes",
       x = "Meses",
       y = "Reservas totales") +
  theme_minimal() +
  scale_fill_brewer(palette = "Spectral") + 
  scale_y_continuous(labels = label_number(accuracy = 1))






