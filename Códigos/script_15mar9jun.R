# DEMANDA -----------------------------------------------------------------

# Cargamos el archivo CSV
data <- read.csv("OneDrive - UFV/4º/2do Cuatri/TFG/Ingeniería del dato/airbnb/calendar/calendar_15mar.csv", header = TRUE)

# Convertimos las fechas a formato fecha
data$date <- as.Date(data$date)

# Obtenemos el año de las fechas
data$year <- format(data$date, "%Y")

# Filtramos las fechas de 2023
data <- data[data$year == 2023,]

# Filtramos la disponiblidad por los que están ocupados.
# Available: 'f' (ocupado), Available: 't' (libre)
data <- data[data$available == "f",]

# Filtramos las fechas hasta el 10 de junio
data <- data[data$date < as.Date("2023-06-10") & data$date >= as.Date("2023-01-01"),]

# Eliminamos los valores vacíos
data <- na.omit(data)

# Obtenemos el rango de fechas
rango <- range(data$date)

# Guardamos el archivo CSV modificado
write.csv(data, "ocupados_15mar9jun.csv", row.names = FALSE)



# OFERTA ------------------------------------------------------------------

# Cargamos el archivo CSV
oferta <- read.csv("OneDrive - UFV/4º/2do Cuatri/TFG/Ingeniería del dato/airbnb/calendar/calendar_15mar.csv", header = TRUE)

# Convertimos las fechas a formato fecha
oferta$date <- as.Date(oferta$date)

# Obtenemos el año de las fechas
oferta$year <- format(oferta$date, "%Y")

# Filtramos las fechas de 2023
oferta <- oferta[oferta$year == 2023,]

# Filtramos la disponiblidad por los que están libres y ocupados.
# Available: 'f' (ocupado), Available: 't' (libre)
oferta <- oferta[oferta$available %in% c("t", "f"),]

# Filtramos las fechas hasta el 10 de junio
oferta <- oferta[oferta$date < as.Date("2023-06-10") & oferta$date >= as.Date("2023-01-01"),]

# Eliminamos los valores vacíos
oferta <- na.omit(oferta)

# Obtenemos el rango de fechas
rango <- range(oferta$date)

# Guardamos el archivo CSV modificado
write.csv(oferta, "oferta_15mar9jun.csv", row.names = FALSE)

