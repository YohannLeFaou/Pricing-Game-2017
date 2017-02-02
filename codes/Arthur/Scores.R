install.packages('doSNOW')
install.packages('caret')
install.packages('mlbench')

#Source doc caret : http://topepo.github.io/caret/parallel-processing.html

library(caret)
library(mlbench)

data(Sonar)

Score_GLM<-function(dataX,dataY,controle_methode,controle_nombre_fold,controle_nombre_repetition=1,fit_famille,fit_metrique = "RMSE",seed=2017){
#dataX : Les prédicteurs
#dataY : La variables Cible
#controle_methode : Paramètre de contrôle qui peut prendre les valeurs "cv","repeatedcv","LOOCV"
#controle_nombre_fold : Paramètre de contrôle qui gère le nombre de fold (segments) utilisés pour la k-fold cross-validation
#controle_nombre_repetition : Paramètre de contrôle qui gère le nombre répétition de la k-fold cross-validation (fixé à 1 par défaut)
#fit_famille : Paramètre de la fonction glm() peut prendre toutes les valeurs que propose la fonction glm()
#fit_metrique : Paramètre qui gère la métrique d'évaluation fixé à RMSE pour évaluer une régression. Si classification rentre "Accuracy"
#seed : La graine pour la reproductibilité des résultats (fixée à 2017 par défaut)

set.seed(seed)

inTraining <- createDataPartition(dataY, p = .75, list = FALSE)
training <- dataX[ inTraining,]
testing  <- dataX[-inTraining,]
Y_training <- dataY[ inTraining]
Y_testing <- dataY[-inTraining]

fitControl <- trainControl(# 10-fold CV
                           method = controle_methode,
                           number = controle_nombre_fold,
                           # On répète 10 fois la 10-fold CV
                           repeats = controle_nombre_repetition)

set.seed(seed)

GLM.fit <- train(training,Y_training,
                 method = "glm",
		     family = fit_famille,
		     metric = fit_metrique,
                 trControl = fitControl)

res<-data.frame(Fold = as.character(GLM.fit$resample$Resample),
		    RMSE = as.numeric(GLM.fit$resample$RMSE),
		    Rsquared = as.numeric(GLM.fit$resample$Rsquared))
return(res)
}

#Exemple

df<-Sonar
df<-df[,-61]

Cible<-as.data.frame(apply(df,1,sum))
colnames(Cible)<-c("Cible")

df<-cbind(df,Cible)

#Paralellisation sous Windows
library('doSNOW')
cl<-makeCluster(4,type="SOCK")
registerDoSNOW(cl)

Scores<-Score_GLM(dataX = df[,-61],
	    		dataY = df[,61],
	    		controle_methode = "repeatedcv",
	    		controle_nombre_fold = 10,
	    		controle_nombre_repetition = 15,
	    		fit_famille = "gaussian",
	    		fit_metrique = "RMSE",
	    		seed=2017)
          
Scores
