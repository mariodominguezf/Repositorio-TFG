# Cargamos los tres archivos CSV
marzo_junio <- read.csv("OneDrive - UFV/4º/2do Cuatri/TFG/Ingeniería del dato/airbnb/calendar/ocupados_15mar9jun.csv", header = TRUE)
junio_septiembre <- read.csv("OneDrive - UFV/4º/2do Cuatri/TFG/Ingeniería del dato/airbnb/calendar/ocupados_10jun6sept.csv", header = TRUE)
septiembre_diciembre <- read.csv("OneDrive - UFV/4º/2do Cuatri/TFG/Ingeniería del dato/airbnb/calendar/ocupados_7sept31dic.csv", header = TRUE)

# Juntamos los tres archivos CSV
total <- rbind(marzo_junio, junio_septiembre, septiembre_diciembre)

# Guardamos el archivo CSV con los datos unidos
write.csv(total, "ocupados_2023.csv", row.names = FALSE)
