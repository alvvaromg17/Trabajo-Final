



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


#Con anova() analizamos la tabla de desviación.
anova(modelo)

confint(object = modelo, level = 0.95)

#Con la función predict() podemos, incluso, obtener una respuesta (con la opción type ="response") acerca de a qué forma pertenece cada ejemplar según su diámetro de borde.

newdataborde <- data.frame(borde)
probabilities <- modelo %>% predict(newdataborde, type = "response")
predicted.classes1 <- ifelse(probabilities > 0.5, "Lekythos", "Kylix")
predicted.classes1




#Ahora, utilizamos la altura como variable dependiente

modelo1 <- glm (forma ~ altura, data = ceramicas, family = binomial)
summary(modelo1)


confint(object = modelo1, level = 0.95)


newdataaltura <- data.frame(altura)
probabilities <- modelo %>% predict(newdataaltura, type = "response")
predicted.classes2 <- ifelse(probabilities > 0.5, "Lekythos", "Kylix")
predicted.classes2



modelo2 <- glm (forma ~ altura + borde, data = ceramicas, family = binomial)
summary(modelo2)


confint(object = modelo1, level = 0.95)

newdatamult <- data.frame(altura + borde)
probabilities <- modelo2 %>% predict(newdatamult, type = "response")
predicted.classes2 <- ifelse(probabilities > 0.5, "Lekythos", "Kylix")
predicted.classes2


modelo3 <- glm (forma ~ altura + borde + base, data = ceramicas, family = binomial)
summary(modelo3)


coef(modelo3)
summary(modelo3)$coef


newdatamult2 <- data.frame(altura + borde + base)
probabilities <- modelo3 %>% predict(newdatamult2, type = "response")
predicted.classes3 <- ifelse(probabilities > 0.5, "Lekythos", "Kylix")
predicted.classes3

ceramicas = as.data.frame(copas)

is.data.frame(ceramicas)
table(ceramicas)


ceramicas$Forma=factor (ceramicas$Forma, levels = c('0', '1'), labels = c ("Kylix", "Castulo"))
ceramicas$Tecnica=factor (ceramicas$Tecnica, levels = c('0', '1'), labels = c ("FR", "BN"))


View (ceramicas)

freq.fr = table (ceramicas$Tecnica)
View (freq.fr)

x = ceramicas$Forma
y = ceramicas$Tecnica

datos = data.frame(x, y)
View (datos)


library(ggplot2)
ggplot(data = ceramicas, aes(x = forma, y = borde)) +
  geom_point(aes(color = as.factor(borde)), shape = 1) + 
  stat_function(fun = function(x){predict(modelo,
                                          newdata = data.frame(forma = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística",
       y = "Probabilidad Altura") +
  theme(legend.position = "none")







ggplot(data = ceramicas, aes(x = forma, y = altura)) +
  geom_point(aes(color = as.factor(altura)), shape = 1) + 
  stat_function(fun = function(x){predict(modelo,
                                          newdata = data.frame(forma = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística",
       y = "Probabilidad Altura") +
  theme(legend.position = "none")




as.numeric(borde)

plot(forma ~ borde, ceramicas, col = "darkblue",
     main = "Modelo regresión logística",
     ylab = "P(forma1|borde)",
     xlab = "ceramicas", pch = "I")
# type = "response" devuelve las predicciones en forma de probabilidad en lugar de en log_ODDs
curve(predict(modelo, data.frame(borde = x), type = "response"),
      col = "firebrick", lwd = 2.5, add = TRUE)




plot(forma ~ altura, ceramicas, col = "darkblue",
     main = "Modelo regresión logística",
     ylab = "P(forma1|altura)",
     xlab = "ceramicas", pch = "I")
# type = "response" devuelve las predicciones en forma de probabilidad en lugar de en log_ODDs
curve(predict(modelo1, data.frame(altura = x), type = "response"),
      col = "firebrick", lwd = 2.5, add = TRUE)





library(ggplot2)

ggplot(data = ceramicas, aes(x = forma, y = borde, color = forma)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  theme_bw() +
  theme(legend.position = "null")




























# Grafica que muestra la diferencia entre la variable de origen 1 y 0 
ggplot(data, aes(variablex, fill = factor(variabley))) + 
  geom_histogram(binwidth = 1, position = 'dodge') + 
  labs(title = 'Diferencia entre la variable de origen 1 y 0', 
       x = 'Altura', 
       y = 'Frecuencia')

#Calculo de la precisión del modelo

dif_residuos <- modelo$null.deviance - modelo$deviance

# Grados libertad
df <- modelo$df.null - modelo$df.residual

# p-value
p_value <- pchisq(q = dif_residuos,df = df, lower.tail = FALSE)

--------------------------
  paste("Diferencia de residuos:", round(dif_residuos, 4))

paste("Grados de libertad:", df)

paste("p-value:", p_value)
---------------------------
  #Comparación de clasificación predicha y observaciones
  
  library(vcd)

predicciones <- ifelse(test = modelo_logistico$fitted.values > 0.5, yes = 1, no = 0)

matriz_confusion <- table(variabley, predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion

#Predicciones y osbservaciones 
mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

#Conclusión 
#Los resultados de la regresión logística muestran que la altura es un factor significativo para predecir 
#el origen de los datos con un p-value de 0.000. Esto significa que a medida que aumenta la altura, es 
#más probable que el origen sea 1. Además, la matriz de confusión muestra que el modelo es un buen predictor, 
#ya que la mayoría de los registros fueron clasificados correctamente. Por lo tanto, se puede concluir que 
#la altura es un predictor significativo para predecir el origen de los datos.
