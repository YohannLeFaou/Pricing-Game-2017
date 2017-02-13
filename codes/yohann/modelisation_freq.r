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

stepwise_GLM = function(data, variable_importance, var_y, controle_methode,
                        controle_nombre_fold, controle_nombre_repetition, fit_famille, fit_metrique){
  
  cat("", file = "computation_progress.txt")
  
  variable_importance = sort(variable_importance, decreasing = T)
  
  n_cores <- 4
  cl<-makeCluster(n_cores)
  registerDoParallel(cl)
  
  tab_results = foreach(i=1:length(variable_importance), 
                        .combine = rbind,
                        .packages = c("caret"),
                        .export = "Score_GLM",
                        .verbose = TRUE) %dopar% {
                          
                          cat(paste0(i," in ", length(variable_importance), "\n"),
                              file = "computation_progress.txt",
                              append = TRUE)
                          
                          Score_GLM(
                            dataX = data.frame(data[,names(variable_importance)[1:i]]),
                            dataY = data[,var_y],
                            controle_methode = controle_methode,
                            controle_nombre_fold = controle_nombre_fold,
                            controle_nombre_repetition = controle_nombre_repetition,
                            fit_famille = fit_famille,
                            fit_metrique = fit_metrique,
                            seed=i) # a voir pour changer la seed
                          
                        }
  stopCluster(cl)
  tab_results$n_vars = rep(1:length(variable_importance), 
                           times = rep(controle_nombre_fold * controle_nombre_repetition,length(variable_importance)))
  return(tab_results)
}

#### chargement des packages nécessaires

wants <- c("ggplot2", # pour faire des graphiques plus jolies qu'avec les fonctions de base
           "randomForestSRC", # foret alatoire
           "reshape", # pour utiliser la fonction "melt"
           "doParallel"
)
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
for(i in wants){ library(i,character.only = TRUE)}


setwd("Y:/Forsides France/06_Solo/Yohann LE FAOU/mission_forsides/pricing_game_2017")
source("codes/INIT.R")
setwd(YOHANN.DIR)
#source(file = "fonctions.r")


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
             #importance = "permute",
             seed = 2017)
Sys.time() - t1

rf1$importance
plot_rfSRC_importance(rfSRC = rf2, nb_variable = 40)


###################################

res_calibre = calibre_rf(data = train_freq[selected_lines,c("freq",
                                                            setdiff(x_var_quali_freq_dummy,
                                                                               c(modes_quali_var, var_dummy_delete) ))],
                         var_y = "freq",
                         vars_x = paste0("`",setdiff(x_var_quali_freq_dummy,
                                                     c(modes_quali_var, var_dummy_delete) ),"`" ),
                         vect_nodedepth = c(1000),
                         vect_ntree = c(100), 
                         vect_nodesize = c(5, 10, 20, 30, 50, 100, 200, 400, 1000, 2000, 3000, 5000))


################################ test sur les variables dummies

### RF

t1 = Sys.time()
rf1 = rfsrc( formula = freq ~ . ,
             data = train_freq[selected_lines,c("freq",
                                                setdiff(x_var_quali_freq_dummy,
                                                        c(modes_quali_var, var_dummy_delete) ))],
             ntree = 100, 
             #nodeepth = 6, 
             nodesize = 100,
             importance = "permute",
             nsplit = 6,
             forest = T,
             seed = 2017)
Sys.time() - t1
rf1$importance

t2 = Sys.time()
rf2 = rfsrc( formula = freq ~ . ,
             data = train_freq[selected_lines,c("freq",
                                                setdiff(x_var_quali_freq_dummy,
                                                        c(modes_quali_var, var_dummy_delete) ))],
             ntree = 100, 
             #nodeepth = 6, 
             nodesize = 1000,
             importance = "permute",
             #nsplit = 2, que des binaires donc pas beoin de ce parametre en fait
             forest = T,
             seed = 2017)
Sys.time() - t2
rf2$importance

#save(rf2 , file = "resultats 2 sur variables binaires/objetRF.RData")

### GLM

r = glm(formula = freq ~ ., 
        data = train_freq[,c("freq",
                             setdiff(x_var_quali_freq_dummy,
                                     c(modes_quali_var, var_dummy_delete) ))],
        family = "gaussian")



Scores<-Score_GLM(dataX = train_freq[,setdiff(x_var_quali_freq_dummy,
                                              c(modes_quali_var, var_dummy_delete) )],
                  dataY = train_freq$freq,
                  controle_methode = "repeatedcv",
                  controle_nombre_fold = 2,
                  controle_nombre_repetition = 3,
                  fit_famille = "gaussian",
                  fit_metrique = "RMSE",
                  seed=2017)
Scores

### GLM : remontee de la variable importance

res2 = stepwise_GLM(data = train_freq,
             variable_importance = rf2$importance,
             var_y = "freq",
             controle_methode = "repeatedcv",
             controle_nombre_fold = 2,
             controle_nombre_repetition = 2,
             fit_famille = "gaussian",
             fit_metrique = "RMSE")

res_agrege = res2 %>% group_by(n_vars) %>% 
  summarise(meanR2 = mean(Rsquared), sdR2 = sd(Rsquared))
data.frame(res_agrege)

#### GLM final avec les variables selectionnees

variables_finales = sort(rf2$importance , decreasing = T)[1:nb_optimal]


glm_final = glm(formula = freq ~ ., 
                data = train_freq[,c("freq", variables_finales)],
                family = "gaussian")

## représentation de l'influence des variables dans le GLM




