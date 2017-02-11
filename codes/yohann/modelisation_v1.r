
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

var_forest = c("")



apply(X = train_freq[,c("freq",variable1)], MARGIN = 2, FUN = function(x){sum(is.na(x))})

variable1 = setdiff(colnames(train_freq), c("id_client",
                                           "id_vehicle",
                                           "id_policy",
                                           "id_year",
                                           "freq","pol_insee_code", "vh_make",
                                           "vh_model")
                    )

d = na.omit(train_freq[,c("freq",variable1, var_insee)])
selected_lines = sample(x = 1:nrow(d), size = 50000)

options(mc.cores=detectCores()-1, rf.cores = detectCores()-1)

res_calibre = calibre_rf(data = d[selected_lines,],
           var_y = "freq",
           vars_x = c(variable1),
           vect_nodedepth = c(2,3,4,5,6,7,8,9),
           vect_ntree = c(100), 
           vect_nodesize = c(5))



rf1 = rfsrc( formula = freq ~ . , 
             data = d[selected_lines,],
             ntree = 100, 
             nodedepth = 6,
             forest = T,
             importance = "permute")

rf1$importance

plot_rfSRC_importance(rfSRC = rf1, nb_variable = 100)
plot_error_rfSRC(rf1) # pbm avec cette fonction, ajouter argument tree.err = T
# je pense



