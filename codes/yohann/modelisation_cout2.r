library(caret)
library(MASS)

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
  
  
  # inTraining <- createDataPartition(dataY, p = .75, list = FALSE)
  # training <- data.frame(dataX[ inTraining,])
  # testing  <- data.frame(dataX[-inTraining,])
  # Y_training <- dataY[ inTraining]
  # Y_testing <- dataY[-inTraining]
  
  fitControl <- trainControl(# 10-fold CV
    method = controle_methode,
    number = controle_nombre_fold, # On répète 10 fois la 10-fold CV
    repeats = controle_nombre_repetition,
    allowParallel = F)
  
  GLM.fit <- train(x = dataX, 
                   y = dataY,
                   method = "glm",
                   family = fit_famille,
                   metric = fit_metrique,
                   trControl = fitControl)
  
  # GLM.fit <- train(training,Y_training,
  #                  method = "glm",
  #                  family = fit_famille,
  #                  metric = fit_metrique,
  #                  trControl = fitControl)
  
  # res<-data.frame(Fold = as.character(GLM.fit$resample$Resample),
  #                 RMSE = as.numeric(GLM.fit$resample$RMSE),
  #                 Rsquared = as.numeric(GLM.fit$resample$Rsquared))
  return(GLM.fit$resample)
}

#--------------------------------------------------------------------------------------------------------------------------------------------------------------


wants <- c("ggplot2", # pour faire des graphiques plus jolies qu'avec les fonctions de base
           "randomForestSRC", # foret alatoire
           "reshape", # pour utiliser la fonction "melt"
           "doParallel",
           "glmnet",
           "dplyr"
)
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
for(i in wants){ library(i,character.only = TRUE)}


setwd("Y:/Forsides France/06_Solo/Yohann LE FAOU/mission_forsides/pricing_game_2017")
source("codes/INIT.R")
setwd(YOHANN.DIR)
source(file = "fonctions.r")

load("train_cout.RData")


## RF avec les variable quanti (pour avoir une idée du R2)

### calibrage des parametres clefs et regard sur le R2 pour le coup

options(mc.cores=detectCores()-1, rf.cores = detectCores()-1)

### avec les variables quanti non decoupees
res_calibre1 = calibre_rf(data = train_cout[,c("cout",x_var_RF1)],
                         var_y = "cout",
                         vars_x = x_var_RF1,
                         vect_nsplit = c(6),
                         vect_nodedepth = c(1000),
                         vect_ntree = c(100), 
                         vect_nodesize = c(5, 10, 20, 30, 50, 100, 200, 400, 1000, 2000, 3000, 5000))

### avec les variables quanti passées en quali

res_calibre2 = calibre_rf(data = train_cout[,c("cout",setdiff(x_var_quali_cout_dummy,
                                                             var_dummy_delete_cout))],
                         var_y = "cout",
                         vars_x = paste0("`",setdiff(x_var_quali_cout_dummy,
                                                     var_dummy_delete_cout),"`" ),
                         vect_nsplit = c(6),
                         vect_nodedepth = c(1000),
                         vect_ntree = c(100), 
                         vect_nodesize = c(5, 10, 20, 30, 50, 100, 200, 400, 1000, 2000, 3000, 5000))



t1 = Sys.time()
rf2 = rfsrc( formula = cout ~ . ,
             data = train_cout[,c("cout",setdiff(x_var_quali_cout_dummy,
                                                 var_dummy_delete_cout))],
             ntree = 100, nodesize = 1000,
             #nodedepth = 6,
             nsplit = 6,
             forest = T,
             importance = "permute",
             seed = 2017)
Sys.time() - t1

rf1$importance
plot_rfSRC_importance(rfSRC = rf2, nb_variable = 40)


fit <- glm(formula = cout ~ ., 
           data = train_cout[,c("cout",setdiff(x_var_quali_cout_dummy, c(modes_quali_var_cout, var_dummy_delete_cout)))],
           family = "gaussian")

summary(fit)
step <- stepAIC(fit, direction="both")

best<-step$model

save(best,file="meilleur_aic_cout.RData")


fit.gamma <- glm(formula = cout ~ ., 
                 data = train_cout[train_cout$cout>0,c("cout",setdiff(x_var_quali_cout_dummy, c(modes_quali_var_cout, var_dummy_delete_cout)))],
                 family = Gamma(link="log"))

step <- stepAIC(fit.gamma, direction="both")

#var_num<-sapply(train_cout[1,1:50],is.numeric)
#bis<-train_cout[,1:50]

# glm.fit <- glm(cout ~.,
#                data = bis[,var_num],
#                family = "gaussian")

Scores<-Score_GLM(dataX = train_cout[,c("cout",setdiff(x_var_quali_cout_dummy, c(modes_quali_var_cout, var_dummy_delete_cout)))],
                  dataY = train_cout$cout,
                  controle_methode = "repeatedcv",
                  controle_nombre_fold = 2,
                  controle_nombre_repetition = 3,
                  fit_famille = "gaussian",
                  fit_metrique = "RMSE",
                  seed=2017)
Scores
