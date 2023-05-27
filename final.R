



#Introducimos la tabla 
library(readxl)
copas <- read_excel("E:\\trabajo\\copas2.xlsx")


#Convertimos a dataframe.Los paquetes que vamos a necesitar son Tidyverse, para una sencilla manipulación y visualización de los datos; y Caret, que facilita el machine learning y el uso de métodos complejos de clasificación y regresión.
ceramicas = as.data.frame(copas)
is.data.frame(ceramicas)
View (ceramicas)


library(ISLR)
library(tidyverse)
library(caret)

#Con el paquete DT podemos visualizar de una forma más dinámica nuestra base de datos (Solo Rmarkdown) 
library(DT)
datatable(ceramicas)


#Ahora, creamos objetos para las variables que vamos a estudiar, así facilitamos su manejo.

forma = ceramicas$Forma
tecnica = ceramicas$Tecnica
borde = na.omit (ceramicas$`Diam. Borde`)
base = na.omit (ceramicas$`Diam. Base`)
altura = na.omit (ceramicas$Altura)


#Nos aseguramos que los vectores sean numéricos y no se registran como factor o character, los convertimos con as.numeric().

borde <- as.numeric(borde)
base <- as.numeric(base)
altura <- as.numeric(altura)

#Con la función glm () ajustamos los modelos. Primero, vamos a usar la forma como variable predictora, y el diámetro del borde como variable dependiente.

modelo <- glm (forma ~ borde, data = ceramicas, family = binomial)

#Con summary () obtenemos los resultados del modelo.
summary (modelo)


#Ahora, utilizamos la altura como variable dependiente

modelo1 <- glm (forma ~ altura, data = ceramicas, family = binomial)
summary(modelo1)

#Con anova() analizamos la tabla de desviación.
anova(modelo)

confint(object = modelo, level = 0.95)

#Con la función predict() podemos, incluso, obtener una respuesta (con la opción type ="response") acerca de a qué forma pertenece cada ejemplar según su diámetro de borde.

newdataborde <- data.frame(borde)
probabilities <- modelo %>% predict(newdataborde, type = "response")
predicted.classes1 <- ifelse(probabilities > 0.5, "Lekythos", "Kylix")
predicted.classes1



#Ahora, con la Altura

confint(object = modelo1, level = 0.95)

anova(modelo1, test ='Chisq')

newdataaltura <- data.frame(altura)
probabilities <- modelo %>% predict(newdataaltura, type = "response")
predicted.classes2 <- ifelse(probabilities > 0.5, "Lekythos", "Kylix")
predicted.classes2


#Regresión multiple

modelo2 <- glm (forma ~ altura + borde, data = ceramicas, family = binomial)
summary(modelo2)


confint(object = modelo1, level = 0.95)

newdatamult <- data.frame(altura + borde)
probabilities <- modelo2 %>% predict(newdatamult, type = "response")
predicted.classes2 <- ifelse(probabilities > 0.5, "Lekythos", "Kylix")
predicted.classes2

--
modelo3 <- glm (forma ~ altura + borde + base, data = ceramicas, family = binomial)
summary(modelo3)


summary(modelo3)

#Con coef () sacamos solamente los coeficientes.
coef(modelo3)


anova(modelo3, test ='Chisq')


newdatamult2 <- data.frame(altura + borde + base)
probabilities <- modelo3 %>% predict(newdatamult2, type = "response")
predicted.classes3 <- ifelse(probabilities > 0.5, "Kylix", "Lekythos")
predicted.classes3

#Técnica como predictora.

modelo_tec <- glm (tecnica ~ borde, data = ceramicas, family = binomial)


#Representaciones gráficas

library(ggplot2)

#diagrama de caja y bigote

ggplot(data = ceramicas, mapping = aes(x = forma, y = borde)) +
  geom_boxplot(aes(color = as.factor (forma))) +
  geom_point(aes(color = as.factor (forma))) +
  theme_bw() +
  theme(legend.position = "null")


#Con ggplot ()

ggplot(data = ceramicas, aes(x = borde, y = forma)) + 
  geom_point(aes(color = as.factor(forma)), shape = "I", size = 3) +
  stat_function(fun = function(x){predict(modelo,
                                          newdata = data.frame(borde = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística",
       y = "Probabilidad Forma (1 = Kylix, 0 = Lekythos") +
  theme(legend.position = "none")

#Con plot ()

plot(forma ~ borde, ceramicas, col = "darkblue",
     main = "Modelo regresión logística",
     ylab = "Forma",
     xlab = "Diámetro Borde", pch = "I")
# type = "response" devuelve las predicciones en forma de probabilidad en lugar de en log_ODDs
curve(predict(modelo, data.frame(borde = x), type = "response"),
      col = "firebrick", lwd = 2.5, add = TRUE)

#Forma y Altura
ggplot(data = ceramicas, aes(x = altura, y = forma)) + 
  geom_point(aes(color = as.factor(forma)), shape = "I", size = 3) +
  stat_function(fun = function(x){predict(modelo1,
                                          newdata = data.frame(altura = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística",
       y = "Probabilidad Forma (1 = Kylix, 0 = Lekythos") +
  theme(legend.position = "none")



#Técnica y Borde
ggplot(data = ceramicas, aes(x = borde, y = tecnica)) + 
  geom_point(aes(color = as.factor(tecnica)), shape = "I", size = 3) +
  stat_function(fun = function(x){predict(modelo_tec,
                                          newdata = data.frame(borde = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística",
       y = "Probabilidad Técnica (1 = Barniz Negro, 0 = Figurada") +
  theme(legend.position = "none")

#Forma con Borde y Altura


ggplot(data = ceramicas, aes(x = altura + borde, y = forma)) + 
  geom_point(aes(color = as.factor(forma)), shape = "I", size = 3) +
  stat_function(fun = function(x){predict(modelo,
                                          newdata = data.frame(borde = x),
                                          type = "response")}) +
  stat_function(fun = function(x){predict(modelo1,
                                          newdata = data.frame(altura = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística múltiple",
       y = "Probabilidad Forma (1 = Barniz Negro, 0 = Figurada") +
  theme(legend.position = "none")

#Diagrama de mosaico

# Cálculo de la probabilidad del modelo.

newdatafor <- predict(modelo, newdata = data.frame(forma), type = "response")

# Vector de elementos “Lekythos”
pred.modelo <- rep("Lekythos", length(newdatafor))

pred.modelo [newdatafor > 0.5] <- "Kylix"


# Matriz de confusión

matriz <- table(pred.modelo, forma,
                dnn = c("Forma", "predicciones"))

matriz

library(vcd)
mosaic(matriz, shade = T, colorize = T, 
       gp = gpar(fill = matrix(c("#99cc99", "#cc9999"), 2, 2)))


