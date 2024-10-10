#selección de mi data set
wine <- read.csv("/Users/aliochaterzer/Library/Mobile Documents/com~apple~CloudDocs/Documents/IFTS18/Desarrollo IA/wine.csv", sep=";")
wine

#Visualizo el DataSet y lo analizo
View(wine)
colSums(is.na(wine)) #veo que no tengo ningun caracter vacío
summary(wine) #me permite saber de que tratan las columnas y el tipo de datos que contrare

#Descripción de las columnas
print(colnames(wine))

#Class - tipo de vino
#Alcohol - El porcentaje de alcohol por volumen en el vino.
#Malic.acid - El contenido de ácido málico en el vino, que es uno de los ácidos orgánicos presentes en las uvas y puede afectar el sabor del vino.
#Ash - La cantidad de cenizas que quedan después de quemar el vino, lo que puede estar relacionado con el contenido mineral del suelo donde se cultivaron las uvas.
#Alcalinity.of.as - La alcalinidad de las cenizas, es decir, la cantidad de una sustancia alcalina necesaria para neutralizar las cenizas.
#Magnesium - La cantidad de magnesio presente en el vino, un mineral que puede influir en el perfil de sabor y en la estructura del vino.
#Total.phenols - La cantidad total de fenoles en el vino, que incluye varios compuestos antioxidantes y puede estar relacionada con la salud y la calidad del vino.
#Flavanoids - La cantidad de flavonoides en el vino, que son compuestos antioxidantes que pueden contribuir al sabor, el color y la estabilidad del vino.
#Nonflavanoid.phenols - La cantidad de fenoles no flavonoides en el vino, que también pueden tener propiedades antioxidantes y contribuir al perfil de sabor del vino.
#Proanthocyanins - La cantidad de proantocianidinas en el vino, que son compuestos polifenólicos que pueden influir en la astringencia y el sabor del vino.
#Color.intensity - La intensidad del color del vino, que puede estar relacionada con la concentración de pigmentos en el vino.
#Hue - La tonalidad del vino, que puede estar relacionada con la percepción del color y la calidad del vino.
#OD280.OD315.of.diluted.wines - La absorbancia a 280/315 nm de vinos diluidos, que puede estar relacionada con la concentración de compuestos en el vino y se utiliza como medida de la calidad del vino.
#Proline - La cantidad de proline en el vino, que es un aminoácido que puede influir en la estabilidad y el sabor del vino.


#Reemplazo las clases por nombres en lugar de números
library(dplyr)

wine <- wine %>% 
  mutate(class = case_when(
    class == 1 ~ "Merlot",
    class == 2 ~ "Pinot",
    class == 3 ~ "Malbec",
    TRUE ~ as.character(class)  # Si no coincide con ninguno de los casos anteriores, mantener el valor original
  ))

View(wine)


#Empiezo a hacer mi arbol, descargando de las librerías que quiero usar
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)


#Inicio de mi arbol de decisión
str(wine)
set.seed(123) #para fijar las valores
str(wine)


#Hago mi muestra, tomando el 80% de mis valores
muestra <- sample.split(Y = wine$class, SplitRatio = 0.80)
summary(wine)

#Creo mi arbol de entrenamiento
entrenamiento <- subset(x = wine, muestra == TRUE)

#Creo mi arbol de prueba
prueba <- subset(x = wine, muestra == FALSE)

#Creo el modelo. Ponemos la variable objetivo a la izquierda, y el tipo de arbol a la derecha (class=clasificación), podríamos tambien pedir un arbol de regressión
arbol_wine <- rpart(class ~ ., data = entrenamiento, method = "class")


#Imprimo mi arbol y lo diagramo ccon rpart
print (arbol_wine)
rpart.plot(arbol_wine) 

#analizamos el poder predictivo, viendo cuanto pesa cada variable en la predicción

poder_prediccion <- varImp(arbol_wine)
poder_prediccion %>%
  arrange(desc(Overall))

#ahora usamos el set de prueba para predecir con el modelo
prediccion <- predict(arbol_wine, newdata = prueba, type = "class")

#vemos detalles de predicción
print(prediccion) 
plot(prediccion)


#analizamos la predicción
levels <- c("Merlot", "Pinot", "Malbec")
fprueba <- factor(prueba$class, levels = levels)
prediccion <- factor(prediccion, levels = levels)
confusionMatrix(fprueba, prediccion)

#Ajustamos los hiperparametros:
library(rpart)
control <- rpart.control(minsplit = 20, minbucket = 7, maxdepth=2)
fit <- rpart(class ~ ., data=wine, method="class", control=control)

#y se genera el arbol como parametros ajustados
arbol_wine_corregido <- rpart(class ~ ., data = entrenamiento, method="class", control=control)

#ploteamos el arbol corregido
rpart.plot(arbol_wine_corregido) #

#analizamos la preducción del arbol corregido
poder_prediccion <- varImp(arbol_wine_corregido)
poder_prediccion %>%
  arrange(desc(Overall))

