
# HOTELES -----------------------------------------------------------------


# Carga las librerías necesarias
library(tidyverse)
library(ggplot2)
library(readxl)
library(patchwork)

# Carga el archivo de Excel
datos <- read_excel("Oferta y Demanda hoteles copia.xlsx")

# Verifica la estructura de los datos
str(datos)

# Resumen de las variables numéricas
summary(datos)

# Crea una variable "mes"
datos$mes <- ifelse(datos$listing_id == 1, "Enero",
                    ifelse(datos$listing_id == 2, "Febrero",
                           ifelse(datos$listing_id == 3, "Marzo",
                                  ifelse(datos$listing_id == 4, "Abril",
                                         ifelse(datos$listing_id == 5, "Mayo",
                                                ifelse(datos$listing_id == 6, "Junio",
                                                       ifelse(datos$listing_id == 7, "Julio",
                                                              ifelse(datos$listing_id == 8, "Agosto",
                                                                     ifelse(datos$listing_id == 9, "Septiembre",
                                                                            ifelse(datos$listing_id == 10, "Octubre",
                                                                                   ifelse(datos$listing_id == 11, "Noviembre",
                                                                                          ifelse(datos$listing_id == 12, "Diciembre", NA))))))))))))

# Verifica la estructura de los datos
str(datos)

# Convertir la variable "mes" a un factor con niveles ordenados
datos$mes <- factor(datos$mes, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))

# Crear el gráfico
demanda <- ggplot(datos, aes(x = mes, y = demanda_habitaciones)) +
  geom_point() +
  geom_line(group = 1) +
  labs(title = "Demanda de habitaciones por mes",
       x = "Mes",
       y = "Demanda habitaciones")
demanda

oferta <- ggplot(datos, aes(x = mes, y = oferta_habitaciones)) +
  geom_point() +
  geom_line(group = 1) +
  labs(title = "Oferta de habitaciones por mes",
       x = "Mes",
       y = "Oferta habitaciones")
oferta

# Crear el gráfico combinado con leyenda
grafico_combinado <- ggplot() +
  geom_point(data = datos, aes(x = mes, y = oferta_habitaciones, color = "Oferta"), size = 3) +
  geom_line(data = datos, aes(x = mes, y = oferta_habitaciones, group = 1, color = "Oferta"), size = 1) +
  geom_point(data = datos, aes(x = mes, y = demanda_habitaciones, color = "Demanda"), size = 3) +
  geom_line(data = datos, aes(x = mes, y = demanda_habitaciones, group = 1, color = "Demanda"), size = 1) +
  labs(title = "Demanda y oferta de habitaciones por mes (hoteles)",
       x = "Mes",
       y = "Cantidad de habitaciones") +
  theme_minimal() +
  # Añadir leyenda
  guides(color = guide_legend(override.aes = list(size = 4)))


# Mostrar el gráfico combinado con leyenda
print(grafico_combinado)


grafico_combinado1 <- ggplot() +
  geom_point(data = datos, aes(x = mes, y = demanda_plazas, color = "Demanda"), size = 3) +
  geom_line(data = datos, aes(x = mes, y = demanda_plazas, group = 1, color = "Demanda"), size = 1) +
  geom_point(data = datos, aes(x = mes, y = oferta_plazas, color = "Oferta"), size = 3) +
  geom_line(data = datos, aes(x = mes, y = oferta_plazas, group = 1, color = "Oferta"), size = 1) +
  labs(title = "Demanda y oferta de plazas por mes (hoteles)",
       x = "Mes",
       y = "Cantidad de habitaciones") +
  theme_minimal() +
  # Añadir leyenda
  guides(color = guide_legend(override.aes = list(size = 4)))

# Mostrar los dos gráficos combinado en uno
final_plot <- grafico_combinado / grafico_combinado1

# Imprimir el gráfico final
final_plot


# AIRBNB ------------------------------------------------------------------

## OFERTA
# Carga el archivo de Excel
airbnb <- read.csv("oferta_2023 copia.csv")

# Verifica la estructura de los datos
str(airbnb)

# Cambiar el nombre de la variable
airbnb <- airbnb %>%
  rename(mes = month)

# Resumen de las variables numéricas
summary(airbnb)

# Crea una variable "mes"
airbnb$mes <- ifelse(airbnb$mes == 1, "Enero",
                    ifelse(airbnb$mes == 2, "Febrero",
                           ifelse(airbnb$mes == 3, "Marzo",
                                  ifelse(airbnb$mes == 4, "Abril",
                                         ifelse(airbnb$mes == 5, "Mayo",
                                                ifelse(airbnb$mes == 6, "Junio",
                                                       ifelse(airbnb$mes == 7, "Julio",
                                                              ifelse(airbnb$mes == 8, "Agosto",
                                                                     ifelse(airbnb$mes == 9, "Septiembre",
                                                                            ifelse(airbnb$mes == 10, "Octubre",
                                                                                   ifelse(airbnb$mes == 11, "Noviembre",
                                                                                          ifelse(airbnb$mes == 12, "Diciembre", NA))))))))))))

# Verifica la estructura de los datos
str(airbnb)

# Convertir la variable "mes" a un factor con niveles ordenados
airbnb$mes <- factor(airbnb$mes, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))


## DEMANDA
# Carga el archivo de Excel
airbnb2 <- read.csv("demanda_2023 copia.csv")

# Verifica la estructura de los datos
str(airbnb2)

# Cambiar el nombre de la variable
airbnb2 <- airbnb2 %>%
  rename(mes = month)

# Resumen de las variables numéricas
summary(airbnb2)

# Crea una variable "mes"
airbnb2$mes <- ifelse(airbnb2$mes == 1, "Enero",
                     ifelse(airbnb2$mes == 2, "Febrero",
                            ifelse(airbnb2$mes == 3, "Marzo",
                                   ifelse(airbnb2$mes == 4, "Abril",
                                          ifelse(airbnb2$mes == 5, "Mayo",
                                                 ifelse(airbnb2$mes == 6, "Junio",
                                                        ifelse(airbnb2$mes == 7, "Julio",
                                                               ifelse(airbnb2$mes == 8, "Agosto",
                                                                      ifelse(airbnb2$mes == 9, "Septiembre",
                                                                             ifelse(airbnb2$mes == 10, "Octubre",
                                                                                    ifelse(airbnb2$mes == 11, "Noviembre",
                                                                                           ifelse(airbnb2$mes == 12, "Diciembre", NA))))))))))))

# Verifica la estructura de los datos
str(airbnb2)

# Convertir la variable "mes" a un factor con niveles ordenados
airbnb2$mes <- factor(airbnb2$mes, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))




# Crear el gráfico
demanda2 <- ggplot(airbnb2, aes(x = mes, y = reservas_mensuales)) +
  geom_point() +
  geom_line(group = 1) +
  labs(title = "Demanda por mes",
       x = "Mes",
       y = "Demanda de Airbnb")
demanda2

oferta2 <- ggplot(airbnb, aes(x = mes, y = oferta_mensuales)) +
  geom_point() +
  geom_line(group = 1) +
  labs(title = "Oferta por mes",
       x = "Mes",
       y = "Oferta de Airbnb")
oferta2

# Crear el gráfico combinado con leyenda
grafico_combinado2 <- ggplot() +
  geom_point(data = airbnb2, aes(x = mes, y = reservas_mensuales, color = "Demanda"), size = 3) +
  geom_line(data = airbnb2, aes(x = mes, y = reservas_mensuales, group = 1, color = "Demanda"), size = 1) +
  geom_point(data = airbnb, aes(x = mes, y = oferta_mensuales, color = "Oferta"), size = 3) +
  geom_line(data = airbnb, aes(x = mes, y = oferta_mensuales, group = 1, color = "Oferta"), size = 1) +
  labs(title = "Demanda y oferta por mes (Airbnb)",
       x = "Mes",
       y = "Número de Airbnbs ") +
  scale_y_continuous(labels = scales::comma) + # Formatear etiquetas del eje y
  theme_minimal() +
  # Añadir leyenda
  guides(color = guide_legend(override.aes = list(size = 4)))



# Mostrar el gráfico combinado con leyenda
print(grafico_combinado2)




# AIRBNB + HOTELES --------------------------------------------------------

# Unir los archivos por la variable "mes"
union_repositorios <- inner_join(datos, airbnb, by = "mes")
union_repositorios <- inner_join(union_repositorios, airbnb2, by = "mes")


# REGRESIÓN LINEAL
# Convertir "mes" a factor si es necesario
if (!is.factor(union_repositorios$mes)) {
  union_repositorios$mes <- factor(union_repositorios$mes)
  }

# Crear la fórmula para la regresión
formula <- demanda_plazas ~ reservas_mensuales + oferta_mensuales
formula2 <- oferta_plazas ~ reservas_mensuales + oferta_mensuales

# Ajustar el modelo de regresión lineal
modelo <- lm(formula, data = union_repositorios)
modelo2 <- lm(formula2, data = union_repositorios)

# Resumen del modelo
summary(modelo)
summary(modelo2)



# Cargamos la base de datos de las características de los alojamientos
airbnb_caract <- read.csv("Airbnb_Dataset2_Cleaned copia.csv")
airbnb_oferta_mensual <- read.csv("reservas_mensuales_por_Aribnb_2023 copia.csv")
airbnb_oferta_anual <- read.csv("reservas_anuales_por_Aribnb_2023 copia.csv")

# Cambiamos el nombre de la variable 'listing_id' a 'id' del segundo repositorio
# Cambiar el nombre de la variable "edad" a "Edad"
names(airbnb_oferta_mensual)[names(airbnb_oferta_mensual) == "listing_id"] <- "id"
names(airbnb_oferta_anual)[names(airbnb_oferta_anual) == "listing_id"] <- "id"

# Juntamos repositorio de las caracteristicas de airbnb con el de oferta mensuales
airbnb_caract_mensual <- inner_join(airbnb_caract, airbnb_oferta_mensual, by = "id")
airbnb_caract_anual <- inner_join(airbnb_caract, airbnb_oferta_anual, by = "id")

# Cambiamos a 0 todos los valors NA
airbnb_caract_mensual[is.na(airbnb_caract_mensual)] <- 0
airbnb_caract_anual[is.na(airbnb_caract_anual)] <- 0


ggplot(airbnb_caract_anual %>%
         filter(price <= quantile(price, 0.95)),  # Filtrar valores por debajo del percentil 95
       aes(x = reservas_totales, y = price)) +
  geom_violin() +
  labs(title = "Distribución del precio por reservas totales (Violin Plot)",
       x = "Reservas totales", y = "Precio") +
  theme_classic()

ggplot(airbnb_caract_anual %>%
         filter(accommodates <= quantile(price, 0.95)),  # Filtrar valores por debajo del percentil 95
       aes(x = reservas_totales, y = accommodates)) +
  geom_violin() +
  labs(title = "Distribución de huespedes permitidos por reservas totales (Violin Plot)",
       x = "Reservas totales", y = "Huépedes permitidos") +
  theme_classic()

ggplot(airbnb_caract_anual %>%
         filter(beds <= quantile(price, 0.95 )),  # Filtrar valores por debajo del percentil 95
       aes(x = reservas_totales, y = beds)) +
  geom_violin() +
  labs(title = "Distribución de número de camas por reservas totales (Violin Plot)",
       x = "Reservas totales", y = "Camas disponibles") +
  theme_classic()

ggplot(airbnb_caract_anual,  
       aes(x = reservas_totales, y = host_response_rate)) +
  geom_violin() +
  labs(title = "Distribución de Ratio de respuesta propietario por reservas totales (Violin Plot)",
       x = "Reservas totales", y = "Ratio de respuesta propietario") +
  theme_classic()





