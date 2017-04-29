

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

setwd("Y:\\Forsides France\\06_Solo\\Arhur MAILLART\\Pricing Game\\Pricing-Game-2017-master\\codes\\yohann_year2")


data_claim0 = read.csv(file = "../../data/PG_2017_CLAIMS_YEAR0.csv", sep = ",",
                       dec = ".", header = T, na.strings = c(""," "))
data_prospect0 = read.csv(file = "../../data/PG_2017_YEAR0.csv", sep = ",",
                          dec = ".", header = T, na.strings = c(""," "))

###### year 0

a0 = data_claim0 %>% 
  group_by(id_client, id_vehicle) %>% 
  summarise(n_claim_y0 = n(),
            sum_claim_y0 = sum(claim_amount))


a0 = merge(x = data_prospect0,
           y = a0,
           by.x = c("id_client","id_vehicle"),
           by.y = c("id_client","id_vehicle"),
           all.x = T )
a0$n_claim_y0[is.na(a0$n_claim_y0)] = 0
a0$sum_claim_y0[is.na(a0$sum_claim_y0)] = 0

pred_cout <- read.csv2("../yohann_year0/results_cout/prediction_cout_year1.csv",header=TRUE,sep=";")
pred_cout = pred_cout[order(pred_cout$id_policy),]

pred_freq <- read.csv2("../yohann_year0/results_freq/prediction_freq_year1.csv",header=TRUE,sep=";")
pred_freq = pred_freq[order(pred_freq$id_policy),]

a0$pred_freq_y0 = as.numeric(as.character(pred_freq$prediction))
a0$pred_cout_y0 = as.numeric(as.character(pred_cout$prediction))

# ce que je considere comme prime pure est le produit des predictions freq * cout_moyen, normÃ© tel que
# au global on ait un S/P de 1
a0$prime_pure_y0 = a0$pred_freq_y0 * a0$pred_cout_y0 * sum(a0$sum_claim_y0)/sum(a0$pred_freq_y0 * a0$pred_cout_y0)

a0 = a0[,c("id_policy","pred_freq_y0", "pred_cout_y0", "prime_pure_y0",
           "n_claim_y0","sum_claim_y0")]


###### year 1

data_claim1 = read.csv("resultats year1/PG_2017_CLAIMS_YEAR1_insurer21.csv")
a = data_claim1 %>% group_by(id_client, id_vehicle) %>% summarise(n_claim_y1 = n(), sum_claim_y1 = sum(claim_amount))
a$id_policy = paste0(a$id_client,"-",a$id_vehicle)

a1 = read.csv("resultats year1/PG_2017_YEAR1_ID_INSURED_insurer21.csv")
a1 = merge(x = a1, y = a, by = c("id_policy"), all.x = T)
a1$n_claim_y1[is.na(a1$n_claim_y1)] = 0
a1$sum_claim_y1[is.na(a1$sum_claim_y1)] = 0
a1$final_premium_y0 = a1$premium

a1 = a1[,c("id_policy","final_premium_y0","n_claim_y1","sum_claim_y1")]


###### year 2

data_claim2 = read.csv("resultats year1/PG_2017_CLAIMS_YEAR2_insurer21.csv")
b = data_claim2 %>% group_by(id_client, id_vehicle) %>% summarise(n_claim_y2 = n(), sum_claim_y2 = sum(claim_amount))
b$id_policy = paste0(b$id_client,"-",b$id_vehicle)

a2 = read.csv("resultats year1/PG_2017_YEAR2_ID_INSURED_insurer21.csv")
a2 = merge(x = a2, y = b, by = c("id_policy"), all.x = T)
a2$n_claim_y2[is.na(a2$n_claim_y2)] = 0
a2$sum_claim_y2[is.na(a2$sum_claim_y2)] = 0
a2$final_premium_y1 = a2$premium

a2 = a2[,c("id_policy","final_premium_y1","n_claim_y2","sum_claim_y2")]

########### recap

recap = merge(a0, a1, by = "id_policy", all.x = T)
recap = merge(recap, a2, by = "id_policy", all.x = T)
recap$is_insured_y1 = (!is.na(recap$n_claim_y1)) * 1
recap$is_insured_y2 = (!is.na(recap$n_claim_y2)) * 1

recap$final_premium_y0[is.na(recap$final_premium_y0)] = 0
recap$n_claim_y1[is.na(recap$n_claim_y1)] = 0
recap$sum_claim_y1[is.na(recap$sum_claim_y1)] = 0
recap$final_premium_y1[is.na(recap$final_premium_y1)] = 0
recap$n_claim_y2[is.na(recap$n_claim_y2)] = 0
recap$sum_claim_y2[is.na(recap$sum_claim_y2)] = 0

# -------------------------------------------------------------------------------------------
#                            analyse des resultats du y0
# -------------------------------------------------------------------------------------------

### a-t-on récupérer bcp de gens pour qui on avait mis un malus (pour avoir eu un accident pdt l'annee 0) ?
sum(recap$n_claim_y0[recap$is_insured_y1 == 1] > 0) / sum(recap$is_insured_y1)
sum(recap$n_claim_y0 > 0) / 100000
# on en a récupéré, moins que la moyenne mais quand même


### a-t-on eu raison de mettre ce malus ?
# frequence d'accident pour ceux accidente le y0
freq_accidente = sum((recap$n_claim_y1[recap$n_claim_y0 > 0] > 0), na.rm = T) / sum(recap$is_insured_y1[recap$n_claim_y0 > 0])
# frequence d'accident pour ceux non accidente le y0
freq_non_accidente = sum((recap$n_claim_y1[recap$n_claim_y0 == 0] > 0), na.rm = T) / sum(recap$is_insured_y1[recap$n_claim_y0 == 0])

## il semblerait que le malus etait justifie, mais le constat est-il statistiquement significatif ?
# calcul de la p-valeur de l'hypothèse H0 : pas de différence significative entre les accidentes le y0 et les autres.

# p_valeur approximative
n1 = sum(recap$is_insured_y1[recap$n_claim_y0 > 0])
n2 = sum(recap$is_insured_y1[recap$n_claim_y0 == 0])
p = sum((recap$n_claim_y1 > 0), na.rm = T) / sum(recap$is_insured_y1)
stat = sqrt(n1) * sqrt(n2) / sqrt(n1+n2) / sqrt(p*(1-p)) * (freq_accidente - freq_non_accidente)
p_valeur = 2*pnorm(-stat)
# wouh^^ c'est tres significatif !!

## ok sur les frequences mais y a-t-il une difference sur les cout ?
ratio_accidentés = sum(recap$sum_claim_y1[recap$n_claim_y0 > 0], na.rm = T) / 
  sum(recap$n_claim_y1[recap$n_claim_y0 > 0], na.rm = T)
ratio_non_accidentés = sum(recap$sum_claim_y1[recap$n_claim_y0 == 0], na.rm = T) / 
  sum(recap$n_claim_y1[recap$n_claim_y0 == 0], na.rm = T)

# -> pas de différence ! 

# -------------------------------------------------------------------------------------------
#                            analyse des resultats du y1
# -------------------------------------------------------------------------------------------

### a-t-on récupérer bcp de gens pour qui on avait mis un malus (pour avoir eu un accident pdt l'annee 0) ?
sum(recap$n_claim_y1[recap$is_insured_y2 == 1] > 0) / sum(recap$is_insured_y2)
sum(recap$n_claim_y1 > 0) / 100000

#On a réussi à tous les faire partir !! Mais aussi le reste de nos clients :)


### a-t-on eu raison de mettre ce malus ?

# frequence d'accident pour ceux accidente le y1

freq_accidente = sum((recap$n_claim_y2[recap$n_claim_y1 > 0] > 0), na.rm = T) / sum(recap$is_insured_y2[recap$n_claim_y1 > 0])

# frequence d'accident pour ceux non accidente le y1

freq_non_accidente = sum((recap$n_claim_y2[recap$n_claim_y1 == 0] > 0), na.rm = T) / sum(recap$is_insured_y2[recap$n_claim_y1 == 0])

# -----------------------------------------------------------------------------------
#                              tarif year1
# -----------------------------------------------------------------------------------

# notre S/P prime pure pour les claim du year1
sum(recap$sum_claim_y1, na.rm = T) / sum(recap$prime_pure_y0[recap$is_insured_y1 == 1])

# on recale notre prime pure sur la nouvelle moyenne (avec les sinistre du y1)
recap$prime_pure_y1 = recap$prime_pure_y0 * 
  (sum(recap$sum_claim_y0) + sum(recap$sum_claim_y1, na.rm = T)) /
  (sum(recap$prime_pure_y0) + sum(recap$prime_pure_y0[recap$is_insured_y1 == 1]))

## 4 categories
# non assure y1, pas d'accident pdt 2 ans : 76960
sum((recap$is_insured_y1 == 0) & ((recap$n_claim_y0 == 0) & (is.na(recap$n_claim_y1) | (recap$n_claim_y1 == 0))))
# non assure y1, au moins un accident sur ls 2 premieres annees
sum((recap$is_insured_y1 == 0) & !((recap$n_claim_y0 == 0) & (is.na(recap$n_claim_y1) | (recap$n_claim_y1 == 0))))
# assure y1, pas d'accident pdt 2 ans : 
sum((recap$is_insured_y1 == 1) & ((recap$n_claim_y0 == 0) & (is.na(recap$n_claim_y1) | (recap$n_claim_y1 == 0))))
# assure y1, au moins un accident sur les 2 années
sum((recap$is_insured_y1 == 1) & !((recap$n_claim_y0 == 0) & (is.na(recap$n_claim_y1) | (recap$n_claim_y1 == 0))))


recap$final_premium_y1 = ifelse((recap$is_insured_y1 == 0) & ((recap$n_claim_y0 == 0) & (is.na(recap$n_claim_y1) | (recap$n_claim_y1 == 0))) , 0.9 * recap$prime_pure_y1 / 1.10,
                                ifelse( (recap$is_insured_y1 == 0) & !((recap$n_claim_y0 == 0) & (is.na(recap$n_claim_y1) | (recap$n_claim_y1 == 0))) , 1.47 * recap$prime_pure_y1 / 1.05,
                                        ifelse( (recap$is_insured_y1 == 1) & ((recap$n_claim_y0 == 0) & (is.na(recap$n_claim_y1) | (recap$n_claim_y1 == 0))), 0.9 * recap$prime_pure_y1 / 0.65,
                                                1.47 * recap$prime_pure_y1 / 0.65 )))


## S/P global de la stratégie
sum(recap$prime_pure_y1) / sum(recap$final_premium_y1)


write.csv2(cbind(id_policy = as.character(recap$id_policy), premium = recap$final_premium_y1), 
          file = "Tarif_year2_Team_Forsides.csv",
          row.names = F)

# -----------------------------------------------------------------------------------
#                              tarif year2
# -----------------------------------------------------------------------------------

#Notre S/P prime pure pour les claim du year2

sum(recap$sum_claim_y2, na.rm = T) / sum(recap$final_premium_y1[recap$is_insured_y2 == 1])

#On recale notre prime pure sur la nouvelle moyenne (avec les sinistre du y2)
#Ici, je recale par rapport à l'année 0.Je ne sais pas ce que tu en penses mais ça me semble être le plus raisonnable.


recap$prime_pure_y2 = recap$prime_pure_y0 * 
  (sum(recap$sum_claim_y0) + sum(recap$sum_claim_y2, na.rm = T)) /
  (sum(recap$prime_pure_y0) + sum(recap$prime_pure_y0[recap$is_insured_y2 == 1]))

## 4 categories
# Non assure y2, pas d'accident pdt 3 ans : 70990

sum((recap$is_insured_y2 == 0) & ((recap$n_claim_y0 == 0) & (is.na(recap$n_claim_y1) | (recap$n_claim_y1 == 0))) & ((recap$n_claim_y1 == 0) & (is.na(recap$n_claim_y2) | (recap$n_claim_y2 == 0))))
groupe_1 <- (recap$is_insured_y2 == 0) & ((recap$n_claim_y0 == 0) & (is.na(recap$n_claim_y1) | (recap$n_claim_y1 == 0))) & ((recap$n_claim_y1 == 0) & (is.na(recap$n_claim_y2) | (recap$n_claim_y2 == 0)))

# Non assure y2, au moins un accident sur ls 3 premieres annees
sum((recap$is_insured_y2 == 0) & !((recap$n_claim_y0 == 0) & (is.na(recap$n_claim_y1) | (recap$n_claim_y1 == 0))) & ((recap$n_claim_y1 == 0) & (is.na(recap$n_claim_y2) | (recap$n_claim_y2 == 0))))
groupe_2 <- (recap$is_insured_y2 == 0) & !((recap$n_claim_y0 == 0) & (is.na(recap$n_claim_y1) | (recap$n_claim_y1 == 0))) & ((recap$n_claim_y1 == 0) & (is.na(recap$n_claim_y2) | (recap$n_claim_y2 == 0)))

# Assure y2, pas d'accident pdt 3 ans : 
sum((recap$is_insured_y2 == 1) & ((recap$n_claim_y0 == 0) & (is.na(recap$n_claim_y1) | (recap$n_claim_y1 == 0))) & ((recap$n_claim_y1 == 0) & (is.na(recap$n_claim_y2) | (recap$n_claim_y2 == 0))))
groupe_3 <- (recap$is_insured_y2 == 1) & ((recap$n_claim_y0 == 0) & (is.na(recap$n_claim_y1) | (recap$n_claim_y1 == 0))) & ((recap$n_claim_y1 == 0) & (is.na(recap$n_claim_y2) | (recap$n_claim_y2 == 0)))

# Assure y2, au moins un accident sur les 3 années
sum((recap$is_insured_y2 == 1) & !((recap$n_claim_y0 == 0) & (is.na(recap$n_claim_y1) | (recap$n_claim_y1 == 0))) & ((recap$n_claim_y1 == 0) & (is.na(recap$n_claim_y2) | (recap$n_claim_y2 == 0))))
groupe_4 <- (recap$is_insured_y2 == 1) & !((recap$n_claim_y0 == 0) & (is.na(recap$n_claim_y1) | (recap$n_claim_y1 == 0))) & ((recap$n_claim_y1 == 0) & (is.na(recap$n_claim_y2) | (recap$n_claim_y2 == 0)))

recap$final_premium_y2 = ifelse(groupe_1, 0.9 * recap$prime_pure_y2 / 1.10,
                                ifelse(groupe_2, 1.1 * recap$prime_pure_y2 / 0.9,
                                        ifelse(groupe_3, 0.9 * recap$prime_pure_y2 / 1.10,
                                                1.1 * recap$prime_pure_y2 / 0.9 )))


## S/P global de la stratégie
sum(recap$prime_pure_y2) / sum(recap$final_premium_y2)


write.csv2(cbind(id_policy = as.character(recap$id_policy), premium = recap$final_premium_y2), 
           file = "Tarif_year3_Team_Forsides.csv",
           row.names = F)



