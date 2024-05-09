library(dplyr)

# OFERTA 15 MARZO - 9 JUNIO -----------------------------------------------

# Cargamos el archivo CSV
data <- read.csv("OneDrive - UFV/4º/2do Cuatri/TFG/Ingeniería del dato/airbnb/calendar/calendar_15mar.csv", header = TRUE)

# Convertimos las fechas a formato fecha
data$date <- as.Date(data$date)

# Filtramos las fechas hasta el 10 de junio
data <- data[data$date < as.Date("2023-06-10") & data$date >= as.Date("2023-01-01"),]

# Eliminamos los valores vacíos
data <- na.omit(data)

# Comprobamos que el rango de fechas sea el correcto
rango <- range(data$date)



# OFERTA 10 JUNIO - 6 SEPTIEMBRE ------------------------------------------

# Cargamos el archivo CSV
data2 <- read.csv("OneDrive - UFV/4º/2do Cuatri/TFG/Ingeniería del dato/airbnb/calendar/calendar_10jun.csv", header = TRUE)

# Convertimos las fechas a formato fecha
data2$date <- as.Date(data2$date)

# Filtramos las fechas hasta el 7 de septiembre
data2 <- data2[data2$date < as.Date("2023-09-07") & data2$date >= as.Date("2023-01-01"),]

# Eliminamos los valores vacíos
data2 <- na.omit(data2)

# Comprobamos que el rango de fechas sea el correcto
rango2 <- range(data2$date)



# OFERTA 7 SEPTIEMBRE - 31 DICIEMBRE --------------------------------------

# Cargamos el archivo CSV
data3 <- read.csv("OneDrive - UFV/4º/2do Cuatri/TFG/Ingeniería del dato/airbnb/calendar/calendar_7sept.csv", header = TRUE)

# Convertimos las fechas a formato fecha
data3$date <- as.Date(data3$date)

# Filtramos las fechas hasta el 1 de enero de 2024
data3 <- data3[data3$date < as.Date("2024-01-01") & data3$date >= as.Date("2023-01-01"),]

# Eliminamos los valores vacíos
data3 <- na.omit(data3)

# Comprobamos que el rango de fechas sea el correcto
rango3 <- range(data3$date)



# TOTAL -------------------------------------------------------------------

# Juntamos los tres archivos
oferta_2023 <- rbind(data, data2, data3)

# Filtramos por meses
oferta_marzo <- oferta_2023[oferta_2023$date < as.Date("2023-04-01") & oferta_2023$date >= as.Date("2023-03-01"),]
oferta_abril <- oferta_2023[oferta_2023$date < as.Date("2023-05-01") & oferta_2023$date >= as.Date("2023-04-01"),]
oferta_mayo <- oferta_2023[oferta_2023$date < as.Date("2023-06-01") & oferta_2023$date >= as.Date("2023-05-01"),]
oferta_junio <- oferta_2023[oferta_2023$date < as.Date("2023-07-01") & oferta_2023$date >= as.Date("2023-06-01"),]
oferta_julio <- oferta_2023[oferta_2023$date < as.Date("2023-08-01") & oferta_2023$date >= as.Date("2023-07-01"),]
oferta_agosto <- oferta_2023[oferta_2023$date < as.Date("2023-09-01") & oferta_2023$date >= as.Date("2023-08-01"),]
oferta_septiembre <- oferta_2023[oferta_2023$date < as.Date("2023-10-01") & oferta_2023$date >= as.Date("2023-09-01"),]
oferta_octubre <- oferta_2023[oferta_2023$date < as.Date("2023-11-01") & oferta_2023$date >= as.Date("2023-10-01"),]
oferta_noviembre <- oferta_2023[oferta_2023$date < as.Date("2023-12-01") & oferta_2023$date >= as.Date("2023-11-01"),]
oferta_diciembre <- oferta_2023[oferta_2023$date < as.Date("2024-01-01") & oferta_2023$date >= as.Date("2023-12-01"),]

# Contamos cuantos registros de 'listing_id' hay en cada mes
marzo <- count(oferta_marzo, "listing_id")
abril <- count(oferta_abril, "listing_id")
mayo <- count(oferta_mayo, "listing_id")
junio <- count(oferta_junio, "listing_id")
julio <- count(oferta_julio, "listing_id")
agosto <- count(oferta_agosto, "listing_id")
septiembre <- count(oferta_septiembre, "listing_id")
octubre <- count(oferta_octubre, "listing_id")
noviembre <- count(oferta_noviembre, "listing_id")
diciembre <- count(oferta_diciembre, "listing_id")

# Creamos una tabla para todos los meses y su recuento de oferta
tabla_oferta_2023 <- rbind(marzo, abril, mayo, junio, 
                           julio, agosto, septiembre, octubre, 
                           noviembre, diciembre)

meses <- c("marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")

rownames(tabla_oferta_2023) <- meses

#Quitamos la segunda columna donse solo sale en nombre de 'listing_id':
tabla_oferta_2023_sin_segunda_columna <- select(tabla_oferta_2023, -1)

names(tabla_oferta_2023_sin_segunda_columna)[1] <- "Oferta"

# Guardar la tabla
write.csv(tabla_oferta_2023_sin_segunda_columna, file = "tabla_oferta_2023.csv")



# Representación oferta total por mes ---------------------------------

library(dplyr)

# Obtenemos el mes de las fechas
oferta_2023$month <- format(oferta_2023$date, "%m")

data_mensual <- oferta_2023 %>%
  group_by(month) %>%
  summarise(n = n())

ggplot(data_mensual, aes(x = month, y = n, fill = month)) +
  geom_bar(stat = "identity") +
  labs(title = "Total supply per month",
       x = "Month",
       y = "Total supply") +
  theme_minimal() +
  scale_fill_brewer(palette = "Spectral") + 
  scale_y_continuous(labels = label_number(accuracy = 1))

