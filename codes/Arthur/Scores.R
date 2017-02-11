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

#---------------------------------------------------------------------------------------------------------------------------------------------------
#
#															Construction des bases (frÃ©quence et coÃ»t)
#
#---------------------------------------------------------------------------------------------------------------------------------------------------

#Base frÃ©quence

a1 = data_claim0 %>% 
  group_by(id_client, id_vehicle) %>% 
  summarise(freq = n())


base_freq = merge(x = data_prospect0,
                  y = a1,
                  by.x = c("id_client","id_vehicle"),
                  by.y = c("id_client","id_vehicle"),
                  all.x = T )

base_freq$freq[ is.na(base_freq$freq)] = 0


#Base coÃ»t moyen

a2 = data_claim0 %>% 
  group_by(id_client, id_vehicle, id_claim) %>% 
  summarise(cout = mean(claim_amount))

a3 = merge(x = data_prospect0,
           y = a2,
           by.x = c("id_client","id_vehicle"),
           by.y = c("id_client","id_vehicle"),
           all.x = F )

base_cout = merge(x = a3,
                  y = base_freq,
                  by.x = c("id_client","id_vehicle"),
                  by.y = c("id_client","id_vehicle"),
                  all.x = T)


#---------------------------------------------------------------------------------------------------------------------------------------------------
#
#															                                  Ajustement du GLM
#
#---------------------------------------------------------------------------------------------------------------------------------------------------

#Etape 1 (Loi de Y)

Y<-base_freq$freq

hist(Y,freq = FALSE)

p<-mean(Y)/var(Y)
n<-mean(Y)^2/(var(Y)-mean(Y))

lines(dnbinom(1:6,n,p),col="red")
lines(dgamma(1:6,scale=var(Y)/mean(Y),shape=mean(Y)^2/var(Y)),col="green")

#Etape 2 (Découpage de variables quantitatives)

decoup<-function(variable,quantiles,classe){
  
  q<-unique(quantile(variable,quantiles))
  
  qages<-cut(variable,q)
  tab<-table(qages,base_freq$freq)
  
  barplot(t(prop.table(tab,1)[,classe]),xlab="Variable",ylab="Cible")
  abline(h=mean(t(prop.table(tab,1)[,classe])),col="red")
  
}

decoup(base_freq$drv_age2,seq(0,1,by=0.03),1)

#Stockage du nom de toutes les variables quantitatives
variables_num<-names(base_freq[,sapply(base_freq,is.numeric)])

#Représentation graphique
#Chnager le nom des variables
par(mfrow=c(2,3))

for(i in 1:6){
  decoup(base_freq$vh_value,seq(0,1,by=0.03),i)
}

#Etape 3 (corrÃ©lation entre les prÃ©dicteurs)

str(base_freq)
base_freq$vh_age[is.na(base_freq$vh_age)]<-0

M<-cor(base_freq[,sapply(base_freq,is.numeric)],method="pearson")

corrplot(M,method="circle")

#Etape 4 (sÃ©lÃ©ction des variables)

attach(base_freq)

#Variables quantitatives
variables_num<-names(base_freq[,sapply(base_freq,is.numeric)])
variables_num<-variables_num[-grep("freq",variables_num)]

#Variables qualitatives
variables_qual<-names(base_freq[,sapply(base_freq,is.factor)])
variables_qual<-variables_qual[-grep("id",variables_qual)]

for(vari in variables_qual)
{
  if(length(levels(eval(as.name(vari))))>10)
  {
    variables_qual<-variables_qual[variables_qual!=vari]
  }
}

#Un premier modèle brutal sans découpage de variables (pas de suppression de prédicteurs trop corrélés)

formule<-paste("freq",paste(c(variables_num,variables_qual),collapse = "+"),sep="~")

glm.fit<-glm(formule, data = base_freq, family = "poisson")

#Paralellisation sous Windows
library('doSNOW')
cl<-makeCluster(4,type="SOCK")
registerDoSNOW(cl)

Scores<-Score_GLM(dataX = base_freq[,c(variables_num,variables_qual)],
                  dataY = base_freq[,"freq"],
                  controle_methode = "repeatedcv",
                  controle_nombre_fold = 10,
                  controle_nombre_repetition = 15,
                  fit_famille = "poisson",
                  fit_metrique = "RMSE",
                  seed=2017)

apply(Scores[,2:3],2,mean)






