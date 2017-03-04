library(ISLR)
library(MASS)
library(class)
X= Smarket
attach(Smarket)
tra=sample((1:nrow(X)),nrow(X)*0.5)
muestra=X[tra,]
tra1=nrow(X)-tra
muestra2=X[tra1,]
tra2=sample(nrow(muestra2),nrow(muestra2)*0.5)
prueba=X[tra2,]
tra3=nrow(muestra2)-tra2
datospre=X[tra3,]
modelo_glm=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data = muestra, family = binomial)
modelo_proba=predict(modelo_glm,prueba,type = "response")
contrasts(Direction)
modelo_predic=rep("Down",312)
modelo_predic[modelo_proba>.5]="Up"
matriz_glm=table(modelo_predic,prueba$Direction) #Matriz de confusion
Accuracy=mean(modelo_predic==prueba$Direction) #Evalucion de modelo
Sensitivity=matriz_glm[2,2]/(matriz_glm[2,2]+matriz_glm[2,1])
Specificity=matriz_glm[1,1]/(matriz_glm[1,1]+matriz_glm[1,2])


##### MODELO LDA####
modelo_lda=lda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = muestra)
modelo_predic1=predict(modelo_lda,newdata = prueba)
modelo_clase=modelo_predic1$class
matriz_LDA=table(modelo_clase,prueba$Direction)#Matriz de confusion
Accuracy1=mean(modelo_clase==prueba$Direction)#Evalucion de modelo
Sensitivity1=matriz_LDA[2,2]/(matriz_LDA[2,2]+matriz_LDA[2,1])
Specificity1=matriz_LDA[1,1]/(matriz_LDA[1,1]+matriz_LDA[1,2])

#####MODELO KNN######
entrenamiento=cbind(muestra$Lag1,muestra$Lag2,muestra$Lag3,muestra$Lag4,muestra$Lag5,muestra$Volume)
prueba_knn=cbind(prueba$Lag1,prueba$Lag2,prueba$Lag3,prueba$Lag4,prueba$Lag5,prueba$Volume)
validacion_knn=cbind(datospre$Lag1,datospre$Lag2,datospre$Lag3,datospre$Lag4,datospre$Lag5,datospre$Volume)
modelo_knn=knn(entrenamiento,prueba_knn,muestra$Direction,k=1)
matriz_KNN=table(modelo_knn,prueba$Direction)#Matriz de confusion
Accuracy2=mean(modelo_knn==prueba$Direction)#Evalucion de modelo
Sensitivity2=matriz_KNN[2,2]/(matriz_KNN[2,2]+matriz_KNN[2,1])
Specificity2=matriz_KNN[1,1]/(matriz_KNN[1,1]+matriz_KNN[1,2])






