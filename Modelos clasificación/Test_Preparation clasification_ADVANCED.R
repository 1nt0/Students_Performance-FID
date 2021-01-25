library(caret)
library(randomForest)
library(e1071)
library(readr)
library(tidyverse)
library(ROCR)


#Limpio variables y fijo la seed
rm(list=ls())
set.seed(175)

# Lectura del dataset
dataset <-read.csv("../StudentsPerformance.csv" , head = TRUE)
colnames(dataset)<- c("Gender","Race","Parent_Education","Lunch","Test_Prep","Math_Score","Reading_Score","Writing_Score")

# PREPROCESS: Elimino las instancias con valor n/a
dataset<-dataset[(dataset$Test_Prep=="completed" | dataset$Test_Prep=="none"),]
dataset<-dataset[!(dataset$Math_Score=="n/a"),]
dataset<-dataset[!(dataset$Reading_Score=="n/a"),]
dataset<-dataset[!(dataset$Writing_Score=="n/a"),]
dataset<-dataset[!(dataset$Gender=="n/a"),]
dataset<-dataset[!(dataset$Race=="n/a"),]
dataset<-dataset[!(dataset$Parent_Education=="n/a"),]
dataset<-dataset[!(dataset$Lunch=="n/a"),]

dataset$Test_Prep <- factor(dataset$Test_Prep) 
dataset

# Usar 5-fold cross-validation para todos los métodos que siguen
ctrl = trainControl(method="cv",number=5, verboseIter = TRUE)

# Estimemos SupportVectorMachine con Grid Search para C y sigma
# WARNING: TARDA UNOS MINUTOS EN CARGAR
svmGrid <- expand.grid( C=2^seq(-5,15,5), sigma = 2^seq(-15,3,6) )
svmGrid
modelo1 <- train(Test_Prep~., data=dataset2, method="svmRadial",preProcess=c("center", "scale"), tuneGrid=svmGrid, trControl=ctrl)
modelo1$bestTune
pred <- predict(modelo1$bestTune, dataset2)
table(dataset, pred)
plot(modelo1, data=dataset2, Math_Score ~ Reading_Score)

summary(modelo1$results$Accuracy)
summary(modelo1$results$Kappa)

#Support vector machine hecho con e1071
drop = c("Gender","Race","Parent_Education","Lunch", "Reading_Score")
dataset2 = dataset[,!(names(dataset) %in% drop)] 

index= createDataPartition(dataset2$Test_Prep, p= .7, list= FALSE)
tr_ds= dataset2[index,]
test_ds= dataset2[-index,]
str(test_ds)

tuned <- tune(svm, Test_Prep~., data = tr_ds,
     ranges = list(Cost=2^seq(-5,15,5), sigma= 2^seq(-15,3,6)),
     tunecontrol = tune.control(nrepeat = 10, sampling = "cross", cross = 10))

modelo1b <- tuned$best.model
pred <- predict(modelo1b, dataset2)
confusionMatrix(dataset2[,'Test_Prep'], pred)
plot(modelo1b, dataset2, Math_Score~Writing_Score)


# Random Forest
modelo2 <- train(Test_Prep~., data=dataset, method="rf",tuneLength  = 15,  trControl=ctrl)
modelo2
plot(modelo2,cex=1.5, lwd=2, pch=15)
predictionsWS <- predict(modelo2, dataset)
plot(predictionsWS,dataset$Math_Score)
plot(varImp(modelo2))
plot(modelo2)

# Boosting : xgboost (con Grid search)
# WARNING: TARDA BASTANTE EN CARGAR
boostingGrig <- expand.grid(eta = c(0.01,0.05,0.1,0.2,0.5,1), max_depth=c(1,3,5), nrounds = c(20,100,500,1000,2000), subsample=c(0.5,0.75,1.0), colsample_bytree=1, min_child_weight=1, gamma=0 )
modelo3 <- train(Test_Prep~., data=dataset, method="xgbTree", tuneGrid=boostingGrig,trControl=ctrl)
modelo3

plot(modelo3)

xgb.plot.tree(model = )


summary(modelo3$results$Accuracy)
summary(modelo3$results$Kappa)
