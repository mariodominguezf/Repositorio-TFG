
# Arbol de clasificación --------------------------------------------------


library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)

# Cargar los datos
airbnb <- read.csv("Airbnb_Dataset2_Cleaned copia.csv")

# Ajustar las características de las variables a utilizar
airbnb$room_type <- factor(airbnb$room_type)
airbnb$beds <- factor(airbnb$beds)
airbnb$price_interval <- factor(airbnb$price_interval)
airbnb$neighbourhood_group_cleansed <- factor(airbnb$neighbourhood_group_cleansed)

# Calcular el precio medio por número de huéspedes y vecindario
  ## Si el resultado incluye una fila para "Centro" y "2 huéspedes" con un valor de precio_medio de 100€, 
  ## significa que el precio medio de los Airbnb para 2 huéspedes en el vecindario Centro es de 100€.
precio_medio_por_huesped <- airbnb %>%
  group_by(neighbourhood_group_cleansed, accommodates) %>%
  summarise(precio_medio = mean(price))

set.seed(1649)
airbnb_train <- sample_frac(airbnb, .7)
airbnb_test <- setdiff(airbnb, airbnb_train)

# Unir los datos con el conjunto de entrenamiento
airbnb_train <- left_join(airbnb_train, precio_medio_por_huesped, by = c("neighbourhood_group_cleansed", "accommodates"))

# Ajustar el árbol de clasificación
arbol_2 <- rpart(formula = neighbourhood_group_cleansed ~ price + precio_medio, data = airbnb_train)
arbol_2

# Visualizar el árbol de clasificación especificando una paleta de colores
rpart.plot(arbol_2, box.palette = "Blues")

# Visualizar el árbol de clasificación ajustado y especificando una paleta de colores
prp(arbol_2, box.palette = "BuBn", branch = 0.25)

# Guardar el árbol en un archivo .png
png("arbol_clasificacion.png", width = 1000, height = 1500)
prp(arbol_2, box.palette = "BuBn", branch = 0.5)
dev.off()



# Conclusiones ------------------------------------------------------------

# Unir los datos con el conjunto de prueba
airbnb_test <- left_join(airbnb_test, precio_medio_por_huesped, by = c("neighbourhood_group_cleansed", "accommodates"))

# Predecir en el conjunto de prueba
predicciones <- predict(arbol_2, airbnb_test, type = "class")

# Crear matriz de confusión
confusion_matrix <- table(airbnb_test$neighbourhood_group_cleansed, predicciones)

# Mostrar matriz de confusión
confusion_matrix

# Calcular precisión del modelo
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Mostrar precisión del modelo
accuracy





library(pROC)

# Calcular las probabilidades
probabilidades <- predict(arbol_2, airbnb_test, type = "prob")

# Use multiclass.roc for multi-class problems
roc <- roc(airbnb_test$neighbourhood_group_cleansed, probabilidades[, 2])

# Calcular AUC
auc <- auc(roc)

# Visualizar curva ROC y AUC
plot(roc, print.auc = TRUE, main = "Curva ROC")






