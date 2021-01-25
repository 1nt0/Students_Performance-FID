library(caret)
library(tidyverse)

#Limpio variables y fijo la seed
rm(list=ls())
set.seed(175)

# Lectura del dataset
dataset <-read.csv("../StudentsPerformance.csv" , head = TRUE)
colnames(dataset)<- c("Gender","Race","Parent_Education","Lunch","Test_Prep","Math_Score","Reading_Score","Writing_Score")

# Elimino las instancias con valor n/a
dataset<-dataset[(dataset$Test_Prep=="completed" | dataset$Test_Prep=="none"),]

dataset$Test_Prep <- as.factor (dataset$Test_Prep) 

# Usar 10-fold cross-validation para todos los metodos que siguen
ctrl = trainControl(method="cv",number=10)

# Ahora estimamos nuestro primer modelo: regresión logística
modelo1 <- train(Test_Prep~.,data=dataset,method="glm",trControl=ctrl)
modelo1

# KNN sin preprocesar
modelo2a <- train(Test_Prep~.,data=dataset,method="knn",trControl=ctrl)

# KNN con preprocesamiento
modelo2b <- train(Test_Prep~.,data=dataset,method="knn",preProcess=c("center", "scale"), trControl=ctrl)
modelo2b

# KNN con preprocesamiento y grid search para k
knnGrid <- expand.grid(k=c(1,5,10,30,100))
modelo2c <- train(Test_Prep~.,data=dataset,method="knn",preProcess=c("center", "scale"), tuneGrid=knnGrid, trControl=ctrl)
modelo2c

# CART Parámetro de control = maxdepth
modelo3a <- train(Test_Prep~.,data=dataset,method="rpart2",trControl=ctrl)
modelo3a

# CART Búsqueda de mejor Maxdepth
cartGrid <- expand.grid(maxdepth=c(1,5,10,20))
modelo3b <- train(Test_Prep~.,data=dataset,method="rpart2",tuneGrid=cartGrid,trControl=ctrl)
modelo3b
