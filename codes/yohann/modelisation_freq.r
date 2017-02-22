
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
  
  return(GLM.fit$resample)
}


stepwise_GLM = function(data, variable_decrease_imp_order, var_y, controle_methode,
                        controle_nombre_fold, controle_nombre_repetition, fit_famille, fit_metrique){
  
  cat("", file = "computation_progress.txt")
  
  n_cores <- detectCores()-1
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

#### chargement des packages nÃ©cessaires

wants <- c("ggplot2", # pour faire des graphiques plus jolies qu'avec les fonctions de base
           "randomForestSRC", # foret alatoire
           "reshape", # pour utiliser la fonction "melt"
           "doParallel",
           #"glmnet",
           "caret",
           "lars",
           "MASS",
           "dplyr"
)
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
for(i in wants){ library(i,character.only = TRUE)}


setwd("Y:/Forsides France/06_Solo/Yohann LE FAOU/mission_forsides/pricing_game_2017")
source("codes/INIT.R")
setwd(YOHANN.DIR)
source(file = "fonctions.r")
options(mc.cores=detectCores()-1, rf.cores = detectCores()-1)

load("train_freq.RData")


#------------------------------------------------------------------------------------
#
#         distribution de la variable "freq"
#
#------------------------------------------------------------------------------------


ggplot( aes(x = freq) , data = train_freq) +
  geom_bar(aes(y = (..count..)/sum(..count..)) ) +
  ggtitle("Distribution de cout_moyen_s_DTA") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("") +
  xlab("frequence")

d = count_(x = train_freq, vars = "freq")
d$n = d$n / nrow(train_freq)
## comparons la distribution de frequence avec une poisson
d$n
round(dpois(x = 0:6, lambda = 0.135) , digits = 5)
## on est plutot pproche d'une poisson finalement !!


#-----------------------------------------------------------------------------
#
#                  RF de base (pour avoir une idée du R2)
#
# -----------------------------------------------------------------------------


## RF avec les variable quanti
selected_lines = sample(x = 1:nrow(train_freq), size = 1000)

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
plot_rfSRC_importance(rfSRC = rf1, nb_variable = 40)

## avec les variables quanti passées en quali

res_calibre = calibre_rf(data = train_freq[selected_lines,c("freq",
                                                            setdiff(x_var_quali_freq_dummy,
                                                                    c(modes_quali_var_freq, var_dummy_delete_freq) ))],
                         var_y = "freq",
                         vars_x = paste0("`",setdiff(x_var_quali_freq_dummy,
                                                     c(modes_quali_var_freq, var_dummy_delete_freq) ),"`" ),
                         vect_nsplit = c(5),
                         vect_nodedepth = c(1000),
                         vect_ntree = c(100), 
                         vect_nodesize = c(5, 10, 20, 30, 50, 100, 200, 400, 1000, 2000, 3000, 5000))



#--------------------------------------------------------------------------------
#
#       Methode pour classer les variables binaires par ordre d'importance
#
#--------------------------------------------------------------------------------

#### avec une RF

t2 = Sys.time()
rf2 = rfsrc( formula = freq ~ . ,
             data = train_freq[selected_lines,c("freq",
                                                setdiff(x_var_quali_freq_dummy,
                                                        c(modes_quali_var_freq, var_dummy_delete_freq) ))],
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
                                           c(modes_quali_var_freq, var_dummy_delete_freq) )]),
         y = train_freq$freq)


### avec le GLM

r = glm(formula = freq ~ ., 
        data = train_freq[,c("freq",
                             setdiff(x_var_quali_freq_dummy,
                                     c(modes_quali_var_freq, var_dummy_delete_freq) ))],
        family = "gaussian")


Scores<-Score_GLM(dataX = train_freq[,setdiff(x_var_quali_freq_dummy,
                                              c(modes_quali_var_freq, var_dummy_delete_freq) )],
                  dataY = train_freq$freq,
                  controle_methode = "repeatedcv",
                  controle_nombre_fold = 2,
                  controle_nombre_repetition = 3,
                  fit_famille = "gaussian",
                  fit_metrique = "RMSE",
                  seed=2017)

#-----------------------------------------------------------------------------------
#
#                     GLM : remontee de la variable importance
#
#-----------------------------------------------------------------------------------

## variable importance de la RF
res1 = stepwise_GLM(data = train_freq,
                    variable_decrease_imp_order = names(sort(rf2$importance, decreasing = T)),
             var_y = "freq",
             controle_methode = "repeatedcv",
             controle_nombre_fold = 5,
             controle_nombre_repetition = 10,
             fit_famille = "gaussian",
             fit_metrique = "RMSE")

res_agrege1 = res1 %>% group_by(n_vars) %>%
  summarise(meanR2 = mean(Rsquared), sdR2 = sd(Rsquared))
data.frame(res_agrege1)

plot(x = res_agrege1$n_vars, y = res_agrege1$meanR2, type = "l")
lines(x = res_agrege1$n_vars, y = res_agrege1$meanR2 + res_agrege1$sdR2, type = "l", col = "red")
lines(x = res_agrege1$n_vars, y = res_agrege1$meanR2 - res_agrege1$sdR2, type = "l", col = "red")

### classement du lasso

res2 = stepwise_GLM(data = train_freq,
                    variable_decrease_imp_order = do.call( "c" , lapply(b$actions , FUN = function(x){(names(x))} )),
                    var_y = "freq",
                    controle_methode = "repeatedcv",
                    controle_nombre_fold = 5,
                    controle_nombre_repetition = 10,
                    fit_famille = "gaussian",
                    fit_metrique = "RMSE")

write.csv2(res2, file = "results_freq/stepwise_GLM_gaussien_order_lasso_v2.csv", row.names = F)

res3 = stepwise_GLM(data = train_freq,
                    variable_decrease_imp_order = do.call( "c" , lapply(b$actions , FUN = function(x){(names(x))} )),
                    var_y = "freq",
                    controle_methode = "repeatedcv",
                    controle_nombre_fold = 5,
                    controle_nombre_repetition = 10,
                    fit_famille = "poisson",
                    fit_metrique = "RMSE")

write.csv2(res3, file = "results_freq/stepwise_GLM_poisson_order_lasso_v2.csv", row.names = F)

res_agrege2 = res2 %>% group_by(n_vars) %>% 
  summarise(meanR2 = mean(Rsquared), sdR2 = sd(Rsquared))
data.frame(res_agrege2)

plot(x = res_agrege2$n_vars, y = res_agrege2$meanR2, type = "l")
lines(x = res_agrege2$n_vars, y = res_agrege2$meanR2 + res_agrege2$sdR2, type = "l", col = "red")
lines(x = res_agrege2$n_vars, y = res_agrege2$meanR2 - res_agrege2$sdR2, type = "l", col = "red")

res_agrege3 = res3 %>% group_by(n_vars) %>% 
  summarise(meanR2 = mean(Rsquared), sdR2 = sd(Rsquared))
data.frame(res_agrege3)

plot(x = res_agrege3$n_vars, y = res_agrege3$meanR2, type = "l")
lines(x = res_agrege3$n_vars, y = res_agrege3$meanR2 + res_agrege3$sdR2, type = "l", col = "red")
lines(x = res_agrege3$n_vars, y = res_agrege3$meanR2 - res_agrege3$sdR2, type = "l", col = "red")

#------------------------------------------------------------------------------
#
#   GLM final (sur les variables selectionnees par les differentes methodes)
#
#------------------------------------------------------------------------------

## RF
variables_finales_RF = names(sort(rf2$importance , decreasing = T)[1:30])

glm_final = glm(formula = freq ~ ., 
                data = train_freq[,c("freq", variables_finales_RF)],
                family = "gaussian")
summary(glm_final)


## Lasso
variables_finales_lasso = unique(do.call( "c" , lapply(b$actions , FUN = function(x){(names(x))} )))[1:30]
glm_final_lasso = glm(formula = freq ~ ., 
                data = train_freq[,c("freq", variables_finales_lasso)],
                family = "poisson")
summary(glm_final_lasso)


## selection par GLM

glm_all_var = glm(formula = freq ~ ., 
                      data = train_freq[,c("freq", setdiff(x_var_quali_freq_dummy,
                                                           c(modes_quali_var_freq, var_dummy_delete_freq) ))],
                      family = "poisson")
e = summary(glm_all_var)
variables_finales_GLM = gsub("`", "",setdiff( row.names(e$coefficients[which(e$coefficients[,"Pr(>|z|)"] < 0.05),]), "(Intercept)"))

glm_final_glm = glm(formula = freq ~ ., 
                  data = train_freq[,c("freq", setdiff(variables_finales_GLM,
                                                       c("vh_din_quali_freq.[70,85)",
                                                         "vh_din_quali_freq.[85,100)",
                                                         "vh_speed_quali_freq.[170,190)",
                                                         "vh_speed_quali_freq.[215,Inf)")))],
                  family = "poisson")
summary(glm_final_glm)

Scores<-Score_GLM(dataX = train_freq[,setdiff(variables_finales_GLM,
                                              c("vh_din_quali_freq.[70,85)",
                                                "vh_din_quali_freq.[85,100)",
                                                "vh_speed_quali_freq.[170,190)",
                                                "vh_speed_quali_freq.[215,Inf)"))],
                  dataY = train_freq$freq,
                  controle_methode = "repeatedcv",
                  controle_nombre_fold = 5,
                  controle_nombre_repetition = 10,
                  fit_famille = "poisson",
                  fit_metrique = "RMSE",
                  seed=2017)
mean(Scores$Rsquared)


t = step(glm_all_var) # "backward",
plot(t)     
############ correlations entre les variables quali
table_correlation = cor(train_freq[,setdiff(x_var_quali_freq_dummy,
                        c(modes_quali_var, var_dummy_delete) )])
write.csv2(table_correlation, file = "table_correlations.csv2")

## representation de l'influence des variables dans le GLM


#------------------------------------------------------------------------------
#
#                      Calcul des predictions finales
#
#------------------------------------------------------------------------------

# finalement on retient la selection de variables directement par le GLM
## on a 41 variables (binaires)

glm_final_freq_year1 = glm(formula = freq ~ ., 
                    data = train_freq[,c("freq", setdiff(variables_finales_GLM,
                                                         c("vh_din_quali_freq.[70,85)",
                                                           "vh_din_quali_freq.[85,100)",
                                                           "vh_speed_quali_freq.[170,190)",
                                                           "vh_speed_quali_freq.[215,Inf)")))],
                    family = "poisson")
save(glm_final_freq_year1, file = "results_freq/glm_final_freq_year1.RData")
summary(glm_final)

# ## test pour voir si la moyenne est respectée (parfait)
# mean(train_freq$freq)
# mean(exp(predict(glm_final, newdata = train_freq)))

load("test_freq.RData")
test_freq_predictions = exp(predict(glm_final, newdata = test_freq))
#hist(test_predictions_freq, breaks = 200)


write.csv2(cbind(id_policy = as.character(test_freq$id_policy), prediction = test_freq_predictions),
           file = "results_freq/prediction_freq_year1.csv", row.names = F)

