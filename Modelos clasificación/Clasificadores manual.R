library(MASS)
library(dplyr)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(caret)
library(randomForest)
library(class)
library(magrittr)
library(ggpubr)
library(rattle)
library(RColorBrewer)
library("e1071")
library(ROCR)
library(Cubist)

#Limpio variables y fijo la seed
rm(list=ls())

set.seed(175)

#Creo una funcion de evaluacion comun para los modelos
evaluation <- function(tree, training_ds, atype) {
  cat("\nMatriz de confusion:\n")
  prediction = predict(tree, training_ds, type=atype)
  xtab = table(prediction, training_ds$Test_Prep)
  cat("\nEvaluacion:\n\n")
  accuracy = sum(prediction == training_ds$Test_Prep)/length(training_ds$Test_Prep)
  precision = xtab[1,1]/sum(xtab[,1])
  recall = xtab[1,1]/sum(xtab[1,])
  f = 2 * (precision * recall) / (precision + recall)
  cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
}

# Lectura del dataset
dataset <-read.csv("../StudentsPerformance.csv" , head = TRUE)
colnames(dataset)<- c("Gender","Race","Parent_Education","Lunch","Test_Prep","Math_Score","Reading_Score","Writing_Score")


# Reparto entre set de entrenamiento y de test
index= createDataPartition(dataset$Test_Prep, p= .7, list= FALSE)

training_ds= dataset[index,]
test_ds= dataset[-index,]

print(paste("Tamaño del set de entrenamiento: ", nrow(training_ds)))
print(paste("Tamaño del set de test: ", nrow(test_ds)))

#************* MODELOS DE CLASIFICACION 
#******************************* MODELO 1 = Single Decision Tree
tree= rpart(Test_Prep ~., method = "class", data = training_ds)

fancyRpartPlot(tree)

# Evaluacion
evaluation(tree, test_ds, "class")

#******************************* MODELO 2 = Random forest
#********************Preprocesamiento manual***********************
# Creo una copia del dataset inicial
drop = c("Race","Gender","Lunch")
student_grades3 = dataset[,!(names(dataset) %in% drop)] 

str(student_grades3)

# Eliminar datos atípicos
summary(student_grades3$Math_Score)
math_bench= 57- 1.5 *IQR(student_grades3$Math_Score)


summary(student_grades3$Reading_Score)
reading_bench=59- 1.5 * IQR(student_grades3$Math_Score)


summary(student_grades3$Writing_Score)
writing_bench= 57.75 - 1.5*IQR(student_grades3$Writing_Score)


print(paste("Math benchmark: ", math_bench))
print(paste("Reading benchmark: ", reading_bench))
print(paste("Writing benchmark: ", writing_bench))
cleansed_grades=
  student_grades3 %>%
  filter(Reading_Score >= 29) %>%
  filter(Writing_Score >=25.875) %>%
  filter(Math_Score >=27) %>%
  select(Parent_Education:Writing_Score)

# comprobamos las estadísticas para cada las puntuaciones tras el limpiado
summary(cleansed_grades$Reading_Score)
summary(cleansed_grades$Writing_Score)
summary(cleansed_grades$Math_Score)

print(paste("Data Frame with outliers: ",nrow(student_grades3)))
print(paste("Data Frame w/o outliers: ",nrow(cleansed_grades)))
print(paste("Number of students removed: ", nrow(student_grades3)- nrow(cleansed_grades)))

#volvemos a hacer la partición de los datos tras el limpiado
index2= createDataPartition(cleansed_grades$Test_Prep, p= .7, list= FALSE)

grades_train2= cleansed_grades[index2,]
grades_test2= cleansed_grades[-index2,]

# Comprobamos que se ha repartido correctamente
str(grades_train2) 
str(grades_test2)

# Aplicaremos validación cruzada con k=5 
Controlparameters= trainControl(method = "cv", number = 5)

parameter_grid= expand.grid(mtry=c(2,3,4))

# Entrenamos el modelo
random_forest_model= train(Test_Prep ~., data = grades_train2, method= "rf",
                           trControl= Controlparameters, tuneGrid= parameter_grid)

# Comprobamos los resultados
random_forest_model

rand_forest_predictions= predict(random_forest_model, grades_test2)
rand_forest_predictions
rand_forest_table= table(predictions= rand_forest_predictions, actual= grades_test2$Test_Prep)
rand_forest_table

evaluation(random_forest_model, grades_test2, "raw")

rf_test_accuracy= sum(diag(rand_forest_table)/sum(rand_forest_table))

print(paste("Random Forest accuracy:", round(rf_test_accuracy *100),"%"))

