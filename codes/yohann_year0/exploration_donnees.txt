

#### chargement des packages n�cessaires

wants <- c("ggplot2", # pour faire des graphiques plus jolies qu'avec les fonctions de base
           "dplyr",
           "caret"
)
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
for(i in wants){ library(i,character.only = TRUE)}


setwd("Y:/Forsides France/06_Solo/Yohann LE FAOU/mission_forsides/pricing_game_2017")
source("codes/INIT.R")

setwd(YOHANN.DIR)

data_claim0 = read.csv(file = "../../data/PG_2017_CLAIMS_YEAR0.csv", sep = ",",
                       dec = ".", header = T, na.strings = c(""," "))
data_prospect0 = read.csv(file = "../../data/PG_2017_YEAR0.csv", sep = ",",
                          dec = ".", header = T, na.strings = c(""," "))
data_prospect1 = read.csv(file = "../../data/PG_2017_YEAR1.csv", sep = ",",
                          dec = ".", header = T, na.strings = c(""," "))

#Remarques sur les donnees
## valeurs manquantes
apply(X = data_claim0, MARGIN = 2, FUN = function(x){sum(is.na(x))})
apply(X = data_prospect0, MARGIN = 2, FUN = function(x){sum(is.na(x))})
apply(X = data_prospect1, MARGIN = 2, FUN = function(x){sum(is.na(x))})

#Les sinistres en 0 (data_claim0)

#Parfois il y a des trop payes, ce qui explique les somme negatives (qui sont des regularisations par l'assureur)
#Exemples
data_claim0[which(data_claim0$id_client == "A00033511"),]
data_claim0[which(data_claim0$id_client == "A00058104"),]
data_claim0[which(data_claim0$id_client == "A00000241"),]
data_claim0[which(data_claim0$id_client == "A00003988"),] ## somme des sinistres est negative sur 1 an


#Dans la base (data_claim0) une ligne vaut pour un client et une voiture et une declaration
#Pour la base des couts moyens le regroupement doit se faire de cette maniere. On doit garder la maille la plus fine.
data_claim0[which(data_claim0$id_client == "A00044168"),]

#Verification
nrow(count_(data_claim0, vars = c("id_client","id_vehicle","id_claim"))) == nrow(data_claim0)
nrow(count_(data_claim0, vars = c("id_client","id_vehicle")))

head(data_prospect)
summary(data_claim0)




#--------------------------------------------------------------------------------------------
#
#													#		Analyse en composantes principales
#
#
#---------------------------------------------------------------------------------------------

load("train_freq.RData")
num<-which(sapply(train_freq[1,],is.numeric))

#Il reste un NA
which(is.na(train_freq),arr.ind = TRUE)

train_freq[840,20]<-0

library(ade4)

#Je retiens 4 axes principaux
acp<-dudi.pca(train_freq[,num], center = TRUE, scale = TRUE)

#Pourcentages de variance cumulée
cumsum(acp$eig/sum(acp$eig)*100)

#Représentation des individus (pas très utile ici)
plot(acp$li[,1],acp$li[,2])

#Représentation des variables
s.corcircle(acp$co,xax=1,yax=2)

