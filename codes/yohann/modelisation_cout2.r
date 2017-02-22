
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

#--------------------------------------------------------------------------------------------------------------------------------------------------------------


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

load("train_cout.RData")
constante_decalage = - min(train_cout$cout) * 1.000001
train_cout$cout  = train_cout$cout + constante_decalage

#-----------------------------------------------------------------------------
#
#                      Distribution du cout
#
#-----------------------------------------------------------------------------


ggplot( aes(x = cout) , data = train_cout) +
  geom_histogram(binwidth = 200,aes(y = (..count..)/sum(..count..)) ) +
  ggtitle("Distribution de cout_moyen_s_DTA") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("") +
  xlab("coût moyen")

## on en leve les gros sinistres pour voir quelque chose
ggplot( aes(x = cout) , data = train_cout[train_cout$cout < 10000,]) +
  geom_histogram(binwidth = 50,aes(y = (..count..)/sum(..count..)) ) +
  ggtitle("Distribution de cout_moyen_s_DTA") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("") +
  xlab("coût moyen")

# ça ressemble bien à une gamma


#-----------------------------------------------------------------------------
#
#                  RF de base (pour avoir une idée du R2)
#
# -----------------------------------------------------------------------------


# calibrage des parametres clefs et regard sur le R2 pour le cout moyen

## avec les variables quanti non decoupees
### (mettre un nsplit faible permet d'accelerer la foret aleatoire)
res_calibre1 = calibre_rf(data = train_cout[,c("cout",x_var_RF1)],
                          var_y = "cout",
                          vars_x = x_var_RF1,
                          vect_nsplit = c(6),
                          vect_nodedepth = c(1000),
                          vect_ntree = c(100), 
                          vect_nodesize = c(5, 10, 20, 30, 50, 100, 200, 400, 1000, 2000, 3000, 5000))

## avec les variables quanti passées en quali

res_calibre2 = calibre_rf(data = train_cout[,c("cout",setdiff(x_var_quali_cout_dummy,
                                                             var_dummy_delete_cout))],
                         var_y = "cout",
                         vars_x = paste0("`",setdiff(x_var_quali_cout_dummy,
                                                     var_dummy_delete_cout),"`" ),
                         vect_nsplit = c(6),
                         vect_nodedepth = c(1000),
                         vect_ntree = c(100), 
                         vect_nodesize = c(5, 10, 20, 30, 50, 100, 200, 400, 1000, 2000, 3000, 5000))


#--------------------------------------------------------------------------------
#
#       Methode pour classer les variables binaires par ordre d'importance
#
#--------------------------------------------------------------------------------


# avec une Random Forest
## avec les variables quanti non decoupees

t1 = Sys.time()
rf1 = rfsrc( formula = cout ~ . ,
             data = train_cout[,c("cout",x_var_RF1)],
             ntree = 100,
             nodesize = 1000,
             nsplit = 6,
             forest = T,
             importance = "permute",
             seed = 2017)
Sys.time() - t1
plot_rfSRC_importance(rfSRC = rf, nb_variable = 40)

## avec les variables quanti passées en quali

t1 = Sys.time()
rf2 = rfsrc( formula = cout ~ . ,
             data = train_cout[,c("cout",setdiff(x_var_quali_cout_dummy,
                                                 var_dummy_delete_cout))],
             ntree = 100,
             nodesize = 1000,
             nsplit = 6,
             forest = T,
             importance = "permute",
             seed = 2017)
Sys.time() - t1
plot_rfSRC_importance(rfSRC = rf2, nb_variable = 40)

# RF pour obtenir le classement de variable importance
## (on enlve les modes)

t1 = Sys.time()
rf3 = rfsrc( formula = cout ~ . ,
             data = train_cout[,c("cout",setdiff(x_var_quali_cout_dummy, 
                                                 c(modes_quali_var_cout, var_dummy_delete_cout)))],
             ntree = 100,
             nodesize = 1000,
             nsplit = 6,
             forest = T,
             importance = "permute",
             seed = 2017)
Sys.time() - t1
plot_rfSRC_importance(rfSRC = rf3, nb_variable = 40)
order_var_rf3 = names(sort(rf3$importance, decreasing = T))

save(rf3, file = "results_cout/results_cout_rf3_v1.RData")


# avec une methode lasso

b = lars(x = as.matrix(train_cout[,setdiff(x_var_quali_cout_dummy,
                                           c(modes_quali_var_cout, var_dummy_delete_cout))]),
         y = train_cout$cout)

order_var_lasso = unique(do.call( "c" , lapply(b$actions , FUN = function(x){(names(x))} )))


# ------------------------------------------------------------------------------------
#
#                             GLM sur toutes les variables
#
# ------------------------------------------------------------------------------------

fit.gamma <- glm(formula = cout ~ ., 
                 data = train_cout[train_cout$cout>0,c("cout",setdiff(x_var_quali_cout_dummy, c(modes_quali_var_cout, var_dummy_delete_cout)))],
                 family = Gamma(link="log"))
summary(fit.gamma)

#-----------------------------------------------------------------------------------
#
#                     GLM : remontee de la variable importance
#
#-----------------------------------------------------------------------------------

## avec la variable importance de la RF

# avec une loi gamma pour 
t1 = Sys.time()
stepwise_GLM_1 = stepwise_GLM(data = train_cout,
                    variable_decrease_imp_order = order_var_rf3,
                    var_y = "cout",
                    controle_methode = "repeatedcv",
                    controle_nombre_fold = 5,
                    controle_nombre_repetition = 100,
                    fit_famille = Gamma(link = "log"), # 
                    fit_metrique = "RMSE")
write.csv2(stepwise_GLM_1, file = "results_cout/stepwise_GLM_gamma_5_folds_100_repet_ordre_variable_RF.csv")
Sys.time() - t1
res_agrege1 = stepwise_GLM_1 %>% group_by(n_vars) %>%
  summarise(meanR2 = mean(Rsquared), sdR2 = sd(Rsquared))
data.frame(res_agrege1)

plot(x = res_agrege1$n_vars, y = res_agrege1$meanR2, type = "l")
lines(x = res_agrege1$n_vars, y = res_agrege1$meanR2 + res_agrege1$sdR2, type = "l", col = "red")
lines(x = res_agrege1$n_vars, y = res_agrege1$meanR2 - res_agrege1$sdR2, type = "l", col = "red")


## avec le lasso

t2 = Sys.time()
stepwise_GLM_2 = stepwise_GLM(data = train_cout,
                              variable_decrease_imp_order = order_var_lasso,
                              var_y = "cout",
                              controle_methode = "repeatedcv",
                              controle_nombre_fold = 5,
                              controle_nombre_repetition = 15 ,
                              fit_famille = Gamma(link = "log"), # , #"gaussian"
                              fit_metrique = "RMSE")
Sys.time() - t2
write.csv2(stepwise_GLM_2, file = "results_cout/stepwise_GLM_gamma_5_folds_100_repet_ordre_variable_lasso.csv")
res_agrege2 = stepwise_GLM_2 %>% group_by(n_vars) %>%
  summarise(meanR2 = mean(Rsquared), sdR2 = sd(Rsquared))
data.frame(res_agrege2)

plot(x = res_agrege2$n_vars, y = res_agrege2$meanR2, type = "l")
lines(x = res_agrege2$n_vars, y = res_agrege2$meanR2 + 1.96 / sqrt(15) * res_agrege2$sdR2, type = "l", col = "red")
lines(x = res_agrege2$n_vars, y = res_agrege2$meanR2 - 1.96 / sqrt(15) * res_agrege2$sdR2, type = "l", col = "red")


#------------------------------------------------------------------------------
#
#          GLM final (sur les variables selectionnees par lasso)
#
#------------------------------------------------------------------------------


fit_final_gamma <- glm(formula = cout ~ ., 
                    data = train_cout[,c("cout",order_var_lasso[1:20])],
                    family = Gamma(link = "log"))
summary(fit_final_gamma)

## on peut hesiter à en garder 11, 14 ou 23
### pour choisir on peut essayer d'evaluer les scores plus précisement
perf_fit_final_gamma <- Score_GLM(dataX = train_cout[,order_var_lasso[1:14]],
                                  dataY = train_cout$cout,
                                  controle_methode = "repeatedcv",
                                  controle_nombre_fold = 5,
                                  controle_nombre_repetition = 100,
                                  fit_famille = Gamma(link = "log"),
                                  fit_metrique = "RMSE"
                                  #,seed=2017
                                  )
mean = mean(perf_fit_final_gamma$Rsquared)
sd = sd(perf_fit_final_gamma$Rsquared)
c(mean - 1.96/sqrt(100) * sd, mean, mean + 1.96/sqrt(100) * sd)


#-------------------------------------------------------------------------------
#
#                                   Modele final
#
#-------------------------------------------------------------------------------

final_variable_cout = c("vh_make_bis.BMW","region.Provence",
                        "pol_coverage.Median2","drv_sex2.M","pol_coverage.Median1",
                        "vh_sale_begin_quali_cout.[14,16)","region.Normandie",
                        "drv_age2_quali_cout.[1,27)",
                        "pol_duration_quali_cout.[22,Inf)","region.AlsaceEst",
                        "vh_make_bis.MERCEDES BENZ",
                        "region.Aquitaine")

glm_final_cout_year1 <- glm(formula = cout ~ ., 
                       data = train_cout[,c("cout",final_variable_cout)],
                       family = Gamma(link = "log"))
summary(glm_final_cout_year1)
save(fit_final_cout_year1, file = "results_cout/glm_final_cout_year1.RData")
# ## test pour voir si le modèle respecte le fait de predire la bonne moyenne (c'est presque ok)
# mean(exp(predict(fit_final_gamma, newdata = train_cout[,final_variable_cout])))
# mean(train_cout$cout)

## prediction

load("test_cout.RData")
test_cout_predictions = exp(predict(fit_final_gamma, newdata = test_cout[,final_variable_cout])) - 
  constante_decalage

write.csv2(cbind(id_policy = as.character(test_cout$id_policy), prediction = test_cout_predictions),
          file = "results_cout/prediction_cout_year1.csv", row.names = F)

