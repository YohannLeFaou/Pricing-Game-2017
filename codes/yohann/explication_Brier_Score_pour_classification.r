
wants <- c("randomForestSRC")

has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
for(i in wants){ library(i,character.only = TRUE)}


setwd("Y:/Forsides France/06_Solo/Yohann LE FAOU/mission_forsides/pricing_game_2017")
source("codes/INIT.R")
setwd(YOHANN.DIR)
source(file = "fonctions.r")
options(mc.cores=detectCores()-1, rf.cores = detectCores()-1)

load("train_cout.RData")


#------------------------------------------------------------------------------
#
#         test pour voir si l'on peut expliquer les sinistres negatifs
#
#------------------------------------------------------------------------------


train_cout$is_cout_positive = as.factor(1 * (train_cout$cout > 0))

t1 = Sys.time()
rf1 = rfsrc( formula = is_cout_positive ~ . ,
             data = train_cout[,c("is_cout_positive",x_var_RF1, "id_vehicle")],
             ntree = 100,
             nodesize = 1000,
             nsplit = 6,
             forest = T,
             importance = "permute",
             seed = 2017)
Sys.time() - t1
rf1$importance[order(rf1$importance[,"all"], decreasing = T),"all"]
head(rf1$predicted.oob, 50)

## Brier Score classique
brier_score = 1/length(train_cout$cout) * sum( (as.numeric(as.character(train_cout$is_cout_positive)) - rf1$predicted.oob[,2] )^2 )

## Brier score train
1/length(train_cout$cout) * sum( (as.numeric(as.character(train_cout$is_cout_positive)) - rf1$predicted[,2])^2 )

## le mieux est de comparer le Brier score a la variance de la variable de Bernoulli que l'on etudie
### (d'ailleurs c'est p(1-p) )
1/length(train_cout$cout) * sum( (as.numeric(as.character(train_cout$is_cout_positive)) - 
                                    mean(as.numeric(as.character(train_cout$is_cout_positive))))^2 )


### Enfin le "Normalized Brier score" donne dans l'objet rf1 est egal à 4 * Brier_Score

