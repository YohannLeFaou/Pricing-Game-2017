####### fct arthur


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



#### chargement des packages nécessaires

wants <- c("ggplot2", # pour faire des graphiques plus jolies qu'avec les fonctions de base
           "randomForestSRC", # foret alatoire
           "reshape", # pour utiliser la fonction "melt"
           "parallel"
)
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
for(i in wants){ library(i,character.only = TRUE)}


setwd("Y:/Forsides France/06_Solo/Yohann LE FAOU/mission_forsides/pricing_game_2017")
source("codes/INIT.R")
setwd(YOHANN.DIR)
source(file = "fonctions.r")


load("train_freq.RData")
## dans train_freq il y a une ligne qui comporte une valeur NA
# load("base_cout.RData")

## RF avec les variable quanti
selected_lines = sample(x = 1:nrow(train_freq), size = 50000)
options(mc.cores=detectCores()-1, rf.cores = detectCores()-1)

t1 = Sys.time()
rf1 = rfsrc( formula = freq ~ . ,
             data = train_freq[selected_lines,c("freq",x_var_RF1)],
             ntree = 5, 
             nodedepth = 6,
             nsplit = 6,
             forest = T,
             importance = "permute")
Sys.time() - t1

rf1$importance
plot_rfSRC_importance(rfSRC = rf1, nb_variable = 100)


###################################

res_calibre = calibre_rf(data = d[selected_lines,],
           var_y = "freq",
           vars_x = c(variable1),
           vect_nodedepth = c(2,3,4,5,6,7,8,9),
           vect_ntree = c(100), 
           vect_nodesize = c(5))




################################ test sur les variables dummies

### RF

t1 = Sys.time()
rf1 = rfsrc( formula = freq ~ . ,
             data = train_freq[selected_lines,c("freq",x_var_quali_freq_dummy)],
             ntree = 100, 
             nodedepth = 6,
             nsplit = 6,
             forest = T,
             seed = 2017)
Sys.time() - t1

### GLM

Scores<-Score_GLM(dataX = train_freq[,x_var_quali_freq_dummy],
                  dataY = train_freq$freq,
                  controle_methode = "repeatedcv",
                  controle_nombre_fold = 2,
                  controle_nombre_repetition = 10,
                  fit_famille = "gaussian",
                  fit_metrique = "RMSE",
                  seed=2017)
Scores
