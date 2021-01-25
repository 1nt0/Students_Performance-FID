library(caret)

#Limpio variables y fijo la seed
rm(list=ls())
set.seed(175)

# Lectura del dataset
dataset <-read.csv("../StudentsPerformance.csv" , head = TRUE)
colnames(dataset)<- c("Gender","Race","Parent_Education","Lunch","Test_Prep","Math_Score","Reading_Score","Writing_Score")

# Usar 10-fold cross-validation para todos los metodos que siguen
ctrl = trainControl(method="cv",number=10)

# Ahora estimemos nuetro primer modelo: una regresión lineal
modelo1 <- train(Writing_Score~.,data=dataset,method="lm",trControl=ctrl)
modelo1

# KNN sin procesar
modelo2 <- train(Writing_Score~.,data=dataset,method="knn",trControl=ctrl)
modelo2

# KNN con preprocesamiento
modelo2b <- train(Writing_Score~.,data=dataset,method="knn",preProcess=c("center","scale"),trControl=ctrl)
modelo2b

# KNN con preprocesamiento y grid search para k
knnGrid <- expand.grid(k=c(1,5,10,30,100))
modelo2c <- train(Writing_Score~.,data=dataset,method="knn",preProcess=c("center","scale"),tuneGrid=knnGrid,trControl=ctrl)
modelo2c

# CART Parámetro de control = maxdepth
modelo3a <- train(Writing_Score~.,data=dataset,method="rpart2",trControl=ctrl)
modelo3a

# CART Búsqueda de mejor Maxdepth
cartGrid <- expand.grid(maxdepth=c(1,5,10,20))
modelo3b <- train(Writing_Score~.,data=dataset,method="rpart2",tuneGrid=cartGrid,trControl=ctrl)
modelo3b
