
# Regresiones lineales ----------------------------------------------------


library(tidyverse)
library(ggplot2)
library(glmnet)


# Cargamos la base de datos
airbnb <- read.csv("Airbnb_Dataset2_Cleaned copia.csv")

# Quitamos valores vacios
airbnb <- na.omit(airbnb, cols = c("price", "neighbourhood_group_cleansed"))

# Cambiamos la variable 'price' a numérica
if (!is.numeric(airbnb$price)) {
  airbnb$price <- as.numeric(airbnb$price)
}

# Convertimos 'neighbourhood_group_cleaned' a factor (categorica)
airbnb$neighbourhood_group_cleansed <- factor(airbnb$neighbourhood_group_cleansed)

# Visualización 1: Violin plot
ggplot(airbnb, aes(x = neighbourhood_group_cleansed, y = price)) +
  geom_violin() +
  labs(title = "Distribución del precio por distrito (Violin Plot)",
       x = "Distritos", y = "Precio") +
  theme_classic()

# Nos interesa quitar los outliers para ver en más profundidad elr ango de precios:
# Visualización 1: Violin plot sin outliers
ggplot(airbnb %>%
         filter(price <= quantile(price, 0.95)),  # Filtrar valores por debajo del percentil 95
       aes(x = neighbourhood_group_cleansed, y = price)) +
  geom_violin() +
  labs(title = "Distribución del precio por distrito (Violin Plot)",
       x = "Distritos", y = "Precio") +
  theme_classic()

# Visualización 2
violin_beds <- ggplot(data = airbnb %>%
                        filter(price <= quantile(price, 0.95)), 
                      aes(x = beds, y = price)) +
  geom_violin() +
  labs(title = "Distribución del precio por número de camas",
       x = "Número de camas", y = "Precio") +
  theme_classic()

# Visualización 3
violin_accommodates <- ggplot(data = airbnb %>%
                                filter(price <= quantile(price, 0.95)), 
                              aes(x = accommodates, y = price)) +
  geom_violin() +
  labs(title = "Distribución del precio por número de huéspedes permitidos",
       x = "Número de huéspedes permitidos", y = "Precio") +
  theme_classic()

# Combinar gráficos horizontalmente
grid.arrange(violin_beds, violin_accommodates, ncol = 2)


# Regresión lineal entre precio y distritos (price , neighbourhood_group_cleansed)
modelo <- lm(price ~ neighbourhood_group_cleansed, data = airbnb)

# Regresión lineal entre precio y numero de camas (price , beds)
modelo2 <- lm(price ~ beds, data = airbnb)

ggplot(airbnb, aes(x = beds, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Número de Camas", y = "Precio") +
  ggtitle("Regresión Lineal: Precio vs. Número de Camas")

# Regresión lineal entre precio y numero de reviews (price , number_of_reviews)
modelo3 <- lm(price ~ number_of_reviews, data = airbnb)

ggplot(airbnb, aes(x = number_of_reviews, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Tipo de Habitación", y = "Precio") +
  ggtitle("Regresión Lineal: Precio vs. Tipo de Habitación")

# Regresión lineal entre precio y multiples variables
modelo4 <- lm(price  ~ neighbourhood_group_cleansed + beds + number_of_reviews + room_type + accommodates, 
              data = airbnb)

# Resumen analítico de los modelos
summary(modelo)
summary(modelo2)
summary(modelo3)
summary(modelo4)

# Regulaciones
# Convertir el factor neighbourhood_group_cleansed a variables dummy para evitar multicolinealidad
airbnb_dummies <- model.matrix(~ neighbourhood_group_cleansed + beds + number_of_reviews + room_type + accommodates - 1, data = airbnb)
y <- airbnb$price

# Regresión LASSO
modelo_lasso <- cv.glmnet(airbnb_dummies, y, alpha = 1)
coef_lasso <- coef(modelo_lasso, s = "lambda.min")

# Regresión RIDGE
modelo_ridge <- cv.glmnet(airbnb_dummies, y, alpha = 0)
coef_ridge <- coef(modelo_ridge, s = "lambda.min")

print(coef_lasso)
print(coef_ridge)





# Regresión polinómica ----------------------------------------------------


# Comprobamos si hay linealidad entre la variable price y minimum_nights
ggplot(airbnb %>%
         filter(price <= quantile(price, 0.95)),  # Filtramos outliers en la variable price
       aes(x = minimum_nights, y = price)) +
  geom_point() +  # Agrega los puntos al gráfico
  labs(title = "Relación entre Precio y Estadía Mínima",
       x = "Estadía Mínima (noches)",
       y = "Precio") +  # Etiquetas de los ejes
  theme_minimal()  # Estilo del gráfico

# Comprobamos si hay linealidad entre la variable price y maximum_nights
ggplot(airbnb %>%
         filter(price <= quantile(price, 0.95)),  # Filtramos outliers en la variable price
       aes(x = maximum_nights, y = price)) +
  geom_point() +  # Agrega los puntos al gráfico
  labs(title = "Relación entre Precio y Estadía Máxima",
       x = "Estadía Máxima (noches)",
       y = "Precio") +  # Etiquetas de los ejes
  theme_minimal()  # Estilo del gráfico

# Regresión polinómica entre precio y noches máximas + mínimas
modelo_polinomial <- lm(price ~ poly(maximum_nights + minimum_nights, degree = 2), data = airbnb)

# Resumen analítico del modelo polinomial
summary(modelo_polinomial)





# Predicciones ------------------------------------------------------------


# Añadimos distritos para saber cual es el precio predicho del Airbnb por noche segun distrito:
new_data <- data.frame(neighbourhood_group_cleansed = c("Centro", "Salamanca"))
precio_predicho <- predict(modelo, newdata = new_data)
cat(" Distritos introducidos para la predicción:", paste(new_data$neighbourhood_group_cleansed, collapse = " y "), "\n", 
    "Precios predichos:", paste(precio_predicho, collapse = " y "), "\n")  

# Añadimos distritos para saber cual es el precio predicho del Airbnb por noche segun número de camas:
new_data2 <- data.frame(beds = c(2, 1))
precio_predicho2 <- predict(modelo2, newdata = new_data2)
cat(" Camas introducidos para la predicción:", paste(new_data2$beds, collapse = " y "), "\n",
    "Precios predichos", paste(precio_predicho2, collapse = " y "), "\n")

# Combinamos ambas predicciones:
modelo3 <- lm(price ~ neighbourhood_group_cleansed + beds, data = airbnb)
new_data3 <- data.frame(neighbourhood_group_cleansed = c("Centro"), beds = c(2))
precio_predicho3 <- predict(modelo3, newdata = new_data3)
cat(" Distritos introducidos para la predicción:", paste(new_data3$neighbourhood_group_cleansed), "\n",
    "Camas introducidas para la predicción:", paste(new_data3$beds), "\n",
    "Precios predichos", precio_predicho3, "\n")


# Precio medio por cada distrito
mean_prices <- tapply(airbnb$price, airbnb$neighbourhood_group_cleansed, mean)
# Desviación estandar media
sd_prices <- tapply(airbnb$price, airbnb$neighbourhood_group_cleansed, sd)

# Visualización 2:
barplot(tapply(airbnb$price, airbnb$neighbourhood_group_cleansed, mean),
        las = 2, # Variables del eje x en vertical
        ylim = c(-300, 700),  
        xlab = "Distritos", ylab = "Precio medio", col = "yellow",
        names.arg = names(mean_prices))  # Nombre de las variables en el eje x
lines(mean_prices, col = "blue", lwd = 2)
lines(mean_prices + sd_prices, col = "red", lty = 2, lwd = 2)
lines(mean_prices - sd_prices, col = "red", lty = 2, lwd = 2)
legend("topleft", c("Precio medio", "+/- Desv. Estandar"), lty = c(1, 2), col = c("blue", "red"))


# Define la función para identificar outliers (valores atípicos)
find_outliers <- function(x) {
  q <- quantile(x, probs = c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  outliers <- x[x < lower_bound | x > upper_bound]
  return(outliers)
}

# Elimina outliers
airbnb_clean <- airbnb[!airbnb$price %in% find_outliers(airbnb$price), ]

# Calcula el nuevo precio medio por cada distrito sin outliers
mean_prices_clean <- tapply(airbnb_clean$price, airbnb_clean$neighbourhood_group_cleansed, mean)

# Calcula la nueva desviación estándar media sin outliers
sd_prices_clean <- tapply(airbnb_clean$price, airbnb_clean$neighbourhood_group_cleansed, sd)

# Visualización sin outliers
barplot(mean_prices_clean,
        las = 2, # Variables del eje x en vertical
        ylim = c(0, max(mean_prices_clean) + max(sd_prices_clean)),  
        xlab = "Distritos", ylab = "Precio medio", col = "white",
        names.arg = names(mean_prices_clean))  # Nombre de las variables en el eje x
lines(mean_prices_clean, col = "blue", lwd = 2)
lines(mean_prices_clean + sd_prices_clean, col = "red", lty = 2, lwd = 2)
lines(mean_prices_clean - sd_prices_clean, col = "red", lty = 2, lwd = 2)
legend("topright", c("Precio medio", "+/- Desv. Estandar"), lty = c(1, 2), col = c("blue", "red"), cex = 0.8)

