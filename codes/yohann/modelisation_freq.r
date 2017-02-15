
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

stepwise_GLM = function(data, variable_decrease_imp_order, var_y, controle_methode,
                        controle_nombre_fold, controle_nombre_repetition, fit_famille, fit_metrique){
  
  cat("", file = "computation_progress.txt")
  
  n_cores <- 4
  cl<-makeCluster(n_cores)
  registerDoParallel(cl)
  
  tab_results = foreach(i=1:length(variable_decrease_imp_order), 
                        .combine = rbind,
                        .packages = c("caret"),
                        .export = "Score_GLM",
                        .verbose = TRUE) %dopar% {
                          
                          cat(paste0(i," in ", length(variable_decrease_imp_order), "\n"),
                              file = "computation_progress.txt",
                              append = TRUE)
                          
                          Score_GLM(
                            dataX = data.frame(data[,variable_decrease_imp_order[1:i]]),
                            dataY = data[,var_y],
                            controle_methode = controle_methode,
                            controle_nombre_fold = controle_nombre_fold,
                            controle_nombre_repetition = controle_nombre_repetition,
                            fit_famille = fit_famille,
                            fit_metrique = fit_metrique,
                            seed=i) # a voir pour changer la seed
                          
                        }
  stopCluster(cl)
  tab_results$n_vars = rep(1:length(variable_decrease_imp_order), 
                           times = rep(controle_nombre_fold * controle_nombre_repetition,length(variable_decrease_imp_order)))
  return(tab_results)
}

#### chargement des packages nécessaires

wants <- c("ggplot2", # pour faire des graphiques plus jolies qu'avec les fonctions de base
           "randomForestSRC", # foret alatoire
           "reshape", # pour utiliser la fonction "melt"
           "doParallel",
           "glmnet"
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


################################ selection de variables sur les variables dummies

#### avec une RF

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

##### avec le LASSO

# ## glmnet package
# fit = glmnet(x = as.matrix(train_freq[,setdiff(x_var_quali_freq_dummy,
#                                     c(modes_quali_var, var_dummy_delete) )]),
#              y = train_freq$freq, nlambda = 10000)
# length(unique(fit$df)) == length(setdiff(x_var_quali_freq_dummy,
#                                  c(modes_quali_var, var_dummy_delete) ))
# 
# fit$
# a = cv.glmnet(x = as.matrix(train_freq[,setdiff(x_var_quali_freq_dummy,
#                                                 c(modes_quali_var, var_dummy_delete) )]),
#               y = train_freq$freq)
# plot(a)
# summary(a)
# log(a$lambda.1se)
### lars package
b = lars(x = as.matrix(train_freq[,setdiff(x_var_quali_freq_dummy,
                                           c(modes_quali_var, var_dummy_delete) )]),
         y = train_freq$freq)

c = cv.lars(x = as.matrix(train_freq[,setdiff(x_var_quali_freq_dummy,
                                          c(modes_quali_var, var_dummy_delete) )]),
        y = train_freq$freq, )


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

## variable importance de la RF
res2 = stepwise_GLM(data = train_freq,
             variable_importance = names(sort(rf2$importance, decreasing = T)),
             var_y = "freq",
             controle_methode = "repeatedcv",
             controle_nombre_fold = 2,
             controle_nombre_repetition = 2,
             fit_famille = "gaussian",
             fit_metrique = "RMSE")

res_agrege = res2 %>% group_by(n_vars) %>% 
  summarise(meanR2 = mean(Rsquared), sdR2 = sd(Rsquared))
data.frame(res_agrege)

plot(x = res_agrege$n_vars, y = res_agrege$meanR2, type = "l")
lines(x = res_agrege$n_vars, y = res_agrege$meanR2 + res_agrege$sdR2, type = "l", col = "red")
lines(x = res_agrege$n_vars, y = res_agrege$meanR2 - res_agrege$sdR2, type = "l", col = "red")
nb_optimal = 86

### classement du lasso

res3 = stepwise_GLM(data = train_freq,
                    variable_decrease_imp_order = do.call( "c" , lapply(b$actions , FUN = function(x){(names(x))} )),
                    var_y = "freq",
                    controle_methode = "repeatedcv",
                    controle_nombre_fold = 2,
                    controle_nombre_repetition = 2,
                    fit_famille = "gaussian",
                    fit_metrique = "RMSE")

res_agrege3 = res3 %>% group_by(n_vars) %>% 
  summarise(meanR2 = mean(Rsquared), sdR2 = sd(Rsquared))
data.frame(res_agrege3)

plot(x = res_agrege3$n_vars, y = res_agrege3$meanR2, type = "l")
lines(x = res_agrege3$n_vars, y = res_agrege3$meanR2 + res_agrege3$sdR2, type = "l", col = "red")
lines(x = res_agrege3$n_vars, y = res_agrege3$meanR2 - res_agrege3$sdR2, type = "l", col = "red")


#### GLM final avec les variables selectionnees

## RF
variables_finales_RF = names(sort(rf2$importance , decreasing = T)[1:nb_optimal])

glm_final = glm(formula = freq ~ ., 
                data = train_freq[,c("freq", variables_finales_RF)],
                family = "gaussian")
summary(glm_final)


## Lasso
variables_finales_lasso = unique(do.call( "c" , lapply(b$actions , FUN = function(x){(names(x))} )))
glm_final_lasso = glm(formula = freq ~ ., 
                data = train_freq[,c("freq", variables_finales_lasso)],
                family = "gaussian")
summary(glm_final_lasso)


## selection par GLM

glm_all_var = glm(formula = freq ~ ., 
                      data = train_freq[1:2000,c("freq", setdiff(x_var_quali_freq_dummy,
                                                           c(modes_quali_var, var_dummy_delete) )[1:5])],
                      family = "gaussian")
e = summary(glm_all_var)
variables_finales_GLM = gsub("`", "",setdiff( row.names(e$coefficients[which(e$coefficients[,"Pr(>|t|)"] < 0.05),]), "(Intercept)"))

glm_final_glm = glm(formula = freq ~ ., 
                  data = train_freq[,c("freq", variables_finales_GLM)],
                  family = "gaussian")

Scores<-Score_GLM(dataX = train_freq[,variables_finales_GLM],
                  dataY = train_freq$freq,
                  controle_methode = "repeatedcv",
                  controle_nombre_fold = 2,
                  controle_nombre_repetition = 3,
                  fit_famille = "gaussian",
                  fit_metrique = "RMSE",
                  seed=2017)
Scores


t = step(glm_all_var) # "backward",
plot(t)     
############ correlations entre les variables quali
table_correlation = cor(train_freq[,setdiff(x_var_quali_freq_dummy,
                        c(modes_quali_var, var_dummy_delete) )])
write.csv2(table_correlation, file = "table_correlations.csv2")

## représentation de l'influence des variables dans le GLM


