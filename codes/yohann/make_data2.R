
#---------------------------------------------------------------------------------------------------------------------------------------------------
#
#															Fonctions
#
#---------------------------------------------------------------------------------------------------------------------------------------------------



#---------------------------------------------------------------------------------------------------------------------------------------------------
#
#															Importation des donnees
#
#---------------------------------------------------------------------------------------------------------------------------------------------------

#### chargement des packages n?cessaires

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
source("fonctions.r")


data_claim0 = read.csv(file = "../../data/PG_2017_CLAIMS_YEAR0.csv", sep = ",",
                       dec = ".", header = T, na.strings = c(""," "))
data_prospect0 = read.csv(file = "../../data/PG_2017_YEAR0.csv", sep = ",",
                          dec = ".", header = T, na.strings = c(""," "))
data_prospect1 = read.csv(file = "../../data/PG_2017_YEAR1.csv", sep = ",",
                          dec = ".", header = T, na.strings = c(""," "))


# les valeurs manquantes pour la variable "drv_sex2" sont remplacees par une
# nouvelle modalite : "autre"
data_prospect0$drv_sex2 = as.character(data_prospect0$drv_sex2)
data_prospect0$drv_sex2[is.na(data_prospect0$drv_sex2)] = "autre"
data_prospect0$drv_sex2 = as.factor(data_prospect0$drv_sex2)

data_prospect1$drv_sex2 = as.character(data_prospect1$drv_sex2)
data_prospect1$drv_sex2[is.na(data_prospect1$drv_sex2)] = "autre"
data_prospect1$drv_sex2 = as.factor(data_prospect1$drv_sex2)

data_prospect0 = na.omit(data_prospect0)

data_prospect = rbind(data_prospect0, data_prospect1)

#---------------------------------------------------------------------------------------------------------------------------------------------------
#
#															Construction des bases (frequence et cout)
#
#---------------------------------------------------------------------------------------------------------------------------------------------------

# correspondance regions

corres_dep_region = data.frame(
  departement = c("62","59","80","60","02","50","14","61","27","76","78","95","91",
                  "75","92","93","94","77","08","51","10","52","55","57","54","88",
                  "67","68","29","56","22","35","85","44","49","53","72","28","41",
                  "37","45","36","18","89","58","21","71","39","25","90","70","79","86",
                  "17","16","87","23","19","24","33","47","40","64","03","63","15",
                  "43","42","69","07","01","38","26","73","74","65","32","82","46",
                  "31","81","12","09","11","66","34","30","48","13","84","04","05",
                  "83","06","97","98","20","2A", "2B") ,
  region = c( rep("Nord",5) , rep("Normandie",5) ,  rep("regionParis" , 8),
              rep("AlsaceEst",10) , rep("Bretagne" , 4) , rep("PaysDeLoire",5),
              rep("Centre",6 ),rep("Bourgogne",8) , rep("Aquitaine",12),
              rep("AuvergneRhoneAlpes", 12) , rep("PyreneeLanguedoc" ,13),
              rep("Provence",6),rep("OutreMer-Corse",5) ) )

data_prospect$departement = substr(data_prospect$pol_insee_code , 1,2)

data_prospect = merge(x = data_prospect, 
                      y = corres_dep_region,
                      by.x = "departement",
                      by.y = "departement",
                      all.x = T)

data_prospect$pol_duration_quali_freq = 
  cut(data_prospect$pol_duration, breaks = c(0,30,Inf),include.lowest = F, right = F)
data_prospect$pol_sit_duration_quali_freq = 
  cut(data_prospect$pol_sit_duration, breaks = c(1,2,3,4,5,6,Inf),include.lowest = F, 
      right = F, labels = c("1","2","3","4","5","[6,Inf)"))
data_prospect$drv_age1_quali_freq =
  cut(data_prospect$drv_age1, breaks = c(0,45,75,Inf),include.lowest = F, right = F)
data_prospect$drv_age2_quali_freq =
  cut(data_prospect$drv_age2, breaks = c(0,1,28,75,Inf),include.lowest = F, right = F)
data_prospect$drv_age_lic1_quali_freq =
  cut(data_prospect$drv_age_lic1, breaks = c(0,5,15,30,55,Inf),include.lowest = F, right = F)
data_prospect$vh_age_quali_freq = 
  cut(data_prospect$vh_age, breaks = c(0,5,10,15,Inf),include.lowest = F, right = F)
data_prospect$vh_cyl_quali_freq =
  cut(data_prospect$vh_cyl, breaks = c(0,1200,1400,1600,1800,2100,Inf),
      include.lowest = F, right = F, dig.lab = 4)
data_prospect$vh_din_quali_freq = 
  cut(data_prospect$vh_din, breaks = c(0,70,85,100,115,140,Inf),include.lowest = F, 
      right = F)
data_prospect$vh_sale_begin_quali_freq = 
  cut(data_prospect$vh_sale_begin, breaks = c(0,10,20,Inf),include.lowest = F, right = F)
data_prospect$vh_sale_end_quali_freq = 
  cut(data_prospect$vh_sale_end, breaks = c(0,10,20,Inf),include.lowest = F, right = F)
data_prospect$vh_speed_quali_freq = 
  cut(data_prospect$vh_speed, breaks = c(0,150,170,190,215,Inf),include.lowest = F, right = F)
data_prospect$vh_value_quali_freq =
  cut(data_prospect$vh_value, breaks = c(0,10000,20000,30000,40000,Inf),include.lowest = F,dig.lab = 5,
      right = F)
data_prospect$vh_weight_quali_freq =
  cut(data_prospect$vh_weight, breaks = c(0,800,1000,1300,1500,1800,Inf),include.lowest = F,dig.lab = 4, 
      right = F)
data_prospect$pol_bonus_quali_freq =
  cut(data_prospect$pol_bonus, breaks = c(0.5,0.51,Inf),include.lowest = F, 
      right = F)

d = data.frame(count_(x = data_prospect0, vars = "vh_make"))
d = d[order(d$n, decreasing = T),]
corres_vh_make = data.frame(vh_make = d$vh_make,
                            vh_make_bis = c("RENAULT","PEUGEOT","CITROEN","VOLKSWAGEN","FORD","OPEL","TOYOTA", 
                                            "MERCEDES BENZ", "FIAT", "NISSAN", "AUDI", "BMW",
                                            rep("AUTRES", 89)))

data_prospect = merge(x = data_prospect,
                      y = corres_vh_make,
                      by.x = "vh_make", 
                      by.y = "vh_make",
                      all.x = T)

#Base frequence
a1 = data_claim0 %>% 
  group_by(id_client, id_vehicle) %>% 
  summarise(freq = n())


train_freq = merge(x = data_prospect[which(data_prospect$id_year == "Year 0"),],
                   y = a1,
                   by.x = c("id_client","id_vehicle"),
                   by.y = c("id_client","id_vehicle"),
                   all.x = T )
train_freq$freq[ is.na(train_freq$freq)] = 0
test_freq = data_prospect[which(data_prospect$id_year == "Year 1"),]

x_var_RF1 = c("pol_bonus", 
              "pol_coverage","pol_duration","pol_sit_duration","pol_pay_freq",
              "pol_payd", "pol_usage", "drv_drv2", "drv_age1", "drv_age2",
              "drv_sex1",
              "drv_sex2", "drv_age_lic1", "drv_age_lic2", "vh_age", "vh_cyl",
              "vh_din", "vh_fuel", "vh_sale_begin", "vh_sale_end", "vh_speed",
              "vh_type", "vh_value", "vh_weight"
              , "region", "vh_make_bis"
)

x_var_quali_freq = c("pol_coverage", "pol_pay_freq", "pol_payd", "pol_usage", "drv_drv2",
                     "drv_sex1", "drv_sex2", "vh_fuel", "vh_type", "pol_duration_quali_freq",
                     "pol_sit_duration_quali_freq","pol_bonus_quali_freq",
                     "drv_age1_quali_freq", 
                     "drv_age2_quali_freq", "drv_age_lic1_quali_freq", 
                     "vh_age_quali_freq", "vh_cyl_quali_freq", "vh_din_quali_freq",
                     "vh_sale_begin_quali_freq", "vh_sale_end_quali_freq",
                     "vh_speed_quali_freq", "vh_value_quali_freq", "vh_weight_quali_freq",
                     "vh_make_bis", "region")

b_freq = dummyVars(formula = as.formula( paste0("~", paste0(x_var_quali_freq , collapse = " + ") )), 
              data = train_freq[,x_var_quali_freq])

dummy_data_train_freq = predict(b_freq, newdata = train_freq[,x_var_quali_freq])
dummy_data_test_freq = predict(b_freq, newdata = test_freq[,x_var_quali_freq])

x_var_quali_freq_dummy = colnames(dummy_data_train_freq)

### certaines variables dummy sont a supprimer, sinon certaines variables sont liées
### (c'est a cause des variables sur le driver 2 -> valeurs manquantes)
var_dummy_delete_freq = c("drv_sex2.autre", "drv_age2_quali_freq.[0,1)")

### modes variables quali
### (utile de notes les modes car il faut les enlever manuellement dans le GLM)
### modalite la plus reprensentee (qui correspond a l'absence de driver 2) mais la seconde
### la plus representee
modes_quali_var_freq = c("pol_coverage.Maxi", "pol_pay_freq.Yearly",
                    "pol_payd.No", "pol_usage.WorkPrivate", "drv_drv2.No",
                    "drv_sex1.M", "drv_sex2.F", "vh_fuel.Diesel",
                    "vh_type.Tourism", "region.regionParis", 
                    "vh_make_bis.RENAULT","pol_duration_quali_freq.[0,30)",
                    "pol_sit_duration_quali_freq.1", "pol_bonus_quali_freq.[0.5,0.51)",
                    "drv_age1_quali_freq.[45,75)", "drv_age2_quali_freq.[28,75)",
                    "drv_age_lic1_quali_freq.[30,55)", "vh_age_quali_freq.[5,10)",
                    "vh_cyl_quali_freq.[1800,2100)","vh_din_quali_freq.[0,70)",
                    "vh_sale_begin_quali_freq.[0,10)", "vh_sale_end_quali_freq.[0,10)",
                    "vh_speed_quali_freq.[150,170)", "vh_value_quali_freq.[10000,20000)",
                    "vh_weight_quali_freq.[1000,1300)")

train_freq = cbind(train_freq, dummy_data_train_freq)
test_freq = cbind(test_freq, dummy_data_test_freq)

save(train_freq, file = "train_freq.RData")
save(test_freq, file = "test_freq")

#Base cout moyen

#----------------------------------------------------------------------------------------------------------------------------------------------------------#

data_claim0 = read.csv(file = "../../data/PG_2017_CLAIMS_YEAR0.csv", sep = ",",
                       dec = ".", header = T, na.strings = c(""," "))
data_prospect0 = read.csv(file = "../../data/PG_2017_YEAR0.csv", sep = ",",
                          dec = ".", header = T, na.strings = c(""," "))
data_prospect1 = read.csv(file = "../../data/PG_2017_YEAR1.csv", sep = ",",
                          dec = ".", header = T, na.strings = c(""," "))


# les valeurs manquantes pour la variable "drv_sex2" sont remplacees par une
# nouvelle modalite : "autre"
data_prospect0$drv_sex2 = as.character(data_prospect0$drv_sex2)
data_prospect0$drv_sex2[is.na(data_prospect0$drv_sex2)] = "autre"
data_prospect0$drv_sex2 = as.factor(data_prospect0$drv_sex2)

data_prospect1$drv_sex2 = as.character(data_prospect1$drv_sex2)
data_prospect1$drv_sex2[is.na(data_prospect1$drv_sex2)] = "autre"
data_prospect1$drv_sex2 = as.factor(data_prospect1$drv_sex2)

data_prospect0 = na.omit(data_prospect0)

data_prospect = rbind(data_prospect0, data_prospect1)

# correspondance regions

corres_dep_region = data.frame(
  departement = c("62","59","80","60","02","50","14","61","27","76","78","95","91",
                  "75","92","93","94","77","08","51","10","52","55","57","54","88",
                  "67","68","29","56","22","35","85","44","49","53","72","28","41",
                  "37","45","36","18","89","58","21","71","39","25","90","70","79","86",
                  "17","16","87","23","19","24","33","47","40","64","03","63","15",
                  "43","42","69","07","01","38","26","73","74","65","32","82","46",
                  "31","81","12","09","11","66","34","30","48","13","84","04","05",
                  "83","06","97","98","20","2A", "2B") ,
  region = c( rep("Nord",5) , rep("Normandie",5) ,  rep("regionParis" , 8),
              rep("AlsaceEst",10) , rep("Bretagne" , 4) , rep("PaysDeLoire",5),
              rep("Centre",6 ),rep("Bourgogne",8) , rep("Aquitaine",12),
              rep("AuvergneRhoneAlpes", 12) , rep("PyreneeLanguedoc" ,13),
              rep("Provence",6),rep("OutreMer-Corse",5) ) )

data_prospect$departement = substr(data_prospect$pol_insee_code , 1,2)

data_prospect = merge(x = data_prospect, 
                      y = corres_dep_region,
                      by.x = "departement",
                      by.y = "departement",
                      all.x = T)

d = data.frame(count_(x = data_prospect0, vars = "vh_make"))
d = d[order(d$n, decreasing = T),]
corres_vh_make = data.frame(vh_make = d$vh_make,
                            vh_make_bis = c("RENAULT","PEUGEOT","CITROEN","VOLKSWAGEN","FORD","OPEL","TOYOTA", 
                                            "MERCEDES BENZ", "FIAT", "NISSAN", "AUDI", "BMW",
                                            rep("AUTRES", 89)))

data_prospect = merge(x = data_prospect,
                      y = corres_vh_make,
                      by.x = "vh_make", 
                      by.y = "vh_make",
                      all.x = T)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------#

a2 = data_claim0 %>% 
  group_by(id_client, id_vehicle, id_claim) %>% 
  summarise(cout = mean(claim_amount))

# ajout de la variable frequence
## (pour voir s'il y a independance entre cout et frequence)

# a1 = data_claim0 %>% 
#   group_by(id_client, id_vehicle) %>% 
#   summarise(freq = n())
# 
# a2 = merge(x = a2, y = a1, by = c("id_client", "id_vehicle"), all.x = T)
# 
# a2 %>% group_by(freq) %>% summarise(nb_sinistre = n(),cout_moyen = mean(cout))

#Pour ces 5 premières variables je n'ai pas trouvé de seuil intéressant (elle sont donc coupées arbitrairement en 6)

data_prospect$pol_bonus_quali_cout = 
  cut(data_prospect$pol_bonus, breaks = c(0.5,0.51,Inf),include.lowest = F, 
      right = F)

#quantile(x = data_prospect$pol_duration, probs = c(0:5/6))
data_prospect$pol_duration_quali_cout = 
  cut(data_prospect$pol_duration, breaks = c(1,3,6,9,14,22,Inf),include.lowest = F, right = F)

data_prospect$pol_sit_duration_quali_cout = 
  cut(data_prospect$pol_sit_duration, breaks = c(1,3,6,Inf),include.lowest = F, 
      right = F)

data_prospect$drv_age1_quali_cout =
  cut(data_prospect$drv_age1, breaks = c(18,25,35,50,70,78,Inf),include.lowest = F, right = F)

data_prospect$drv_age2_quali_cout =
  cut(data_prospect$drv_age2, breaks = c(0,1,27,50,70,Inf),
      include.lowest = F, right = F)

#quantile(x = data_prospect$drv_age_lic1, probs = c(0:5/6))
data_prospect$drv_age_lic1_quali_cout =
  cut(data_prospect$drv_age_lic1, breaks = c(1,19,27,35,45,55,Inf),include.lowest = F, right = F)

#Des variabes que j'ai découpé

data_prospect$vh_age_quali_cout = 
  cut(data_prospect$vh_age, breaks = c(1,seq(6,14,by=2),17,Inf),include.lowest = F, right = F)

data_prospect$vh_cyl_quali_cout =
  cut(data_prospect$vh_cyl, breaks = c(0,1000,1500,1700,2000,2200,3000,Inf),
      include.lowest = F, right = F, dig.lab = 4)

data_prospect$vh_din_quali_cout = 
  cut(data_prospect$vh_din, breaks = c(0,50,75,100,150,200,250,Inf),include.lowest = F, 
      right = F)

data_prospect$vh_sale_begin_quali_cout = 
  cut(data_prospect$vh_sale_begin, breaks = c(0,8,10,12,14,16,18,20,Inf),include.lowest = F, right = F)

data_prospect$vh_sale_end_quali_cout = 
  cut(data_prospect$vh_sale_end, breaks = c(0,6,8,10,12,14,Inf),include.lowest = F, right = F)

data_prospect$vh_speed_quali_cout = 
  cut(data_prospect$vh_speed, breaks = c(0,125,150,175,200,220,Inf),include.lowest = F, right = F)

data_prospect$vh_value_quali_cout =
  cut(data_prospect$vh_value, breaks = c(0,12000,20000,30000,40000,50000,Inf),include.lowest = F,dig.lab = 5,
      right = F)

#Je n'ai pas trouvé non plus pour la dernière variable

data_prospect$vh_weight_quali_cout =
  cut(data_prospect$vh_weight, breaks = c(0,1,800,1200,1600,2000,Inf),include.lowest = F,dig.lab = 4, 
      right = F)

train_cout = merge(x = data_prospect[which(data_prospect$id_year == "Year 0"),],
                   y = a2,
                   by.x = c("id_client","id_vehicle"),
                   by.y = c("id_client","id_vehicle"),
                   all.x = F )

test_cout = data_prospect[which(data_prospect$id_year == "Year 1"),]

x_var_quali_cout = c("pol_coverage", "pol_pay_freq", "pol_payd", "pol_usage", "drv_drv2",
                     "drv_sex1", "drv_sex2", "vh_fuel", "vh_type","pol_bonus_quali_cout",
                     "pol_duration_quali_cout",
                     "pol_sit_duration_quali_cout",
                     "drv_age1_quali_cout", 
                     "drv_age2_quali_cout", "drv_age_lic1_quali_cout", 
                     "vh_age_quali_cout", "vh_cyl_quali_cout", "vh_din_quali_cout",
                     "vh_sale_begin_quali_cout", "vh_sale_end_quali_cout",
                     "vh_speed_quali_cout", "vh_value_quali_cout", "vh_weight_quali_cout",
                     "vh_make_bis", "region")

b_cout = dummyVars(formula = as.formula( paste0("~", paste0(x_var_quali_cout , collapse = " + ") )), 
                   data = train_cout[,x_var_quali_cout])

dummy_data_train_cout = predict(b_cout, newdata = train_cout[,x_var_quali_cout])
dummy_data_test_cout = predict(b_cout, newdata = test_cout[,x_var_quali_cout])

x_var_quali_cout_dummy = colnames(dummy_data_cout)

#Suppression des variables liées

var_dummy_delete_cout = c("drv_sex2.autre","drv_age2_quali_cout.[0,1)")

#Marquage des modes

# modes_quali_var_cout = c("pol_coverage.Maxi",
#                          "pol_pay_freq.Yearly",
#                          "pol_payd.No",
#                          "pol_usage.WorkPrivate",
#                          "drv_drv2.No",
#                          "drv_sex1.M",
#                          "drv_sex2.F",
#                          "vh_fuel.Diesel",
#                          "vh_type.Tourism",
#                          "region.regionParis", 
#                          "vh_make_bis.RENAULT",
#                          "pol_duration_quali_cout.[0.959,7.83)",
#                          "pol_sit_duration_quali_cout.1",
#                          "pol_bonus_quali_cout.[0.498,0.777)",
#                          "drv_age1_quali_cout.[47.3,61.5)",
#                          "drv_age2_quali_cout.[-0.1,16.7)",
#                          "drv_age_lic1_quali_cout.[19.5,38)",
#                          "vh_age_quali_cout.[1,5)",
#                          "vh_cyl_quali_cout.[1000,1500)",
#                          "vh_din_quali_cout.[100,150)",
#                          "vh_sale_begin_quali_cout.[0,8)",
#                          "vh_sale_end_quali_cout.[0,8)",
#                          "vh_speed_quali_cout.[150,175)",
#                          "vh_value_quali_cout.[0,20000)",
#                          "vh_weight_quali_cout.[-7.901,1317)")

modes_quali_var_cout = c("pol_coverage.Maxi",
                         "pol_pay_freq.Yearly",
                         "pol_payd.No",
                         "pol_usage.WorkPrivate",
                         "drv_drv2.No",
                         "drv_sex1.M",
                         "drv_sex2.F",
                         "vh_fuel.Diesel",
                         "vh_type.Tourism",
                         "region.regionParis", 
                         "vh_make_bis.RENAULT",
                         "pol_duration_quali_cout.[3,6)",
                         "pol_sit_duration_quali_cout.[1,3)",
                         "pol_bonus_quali_cout.[0.5,0.51)",
                         "drv_age1_quali_cout.[50,70)",
                         "drv_age2_quali_cout.[27,50)",
                         "drv_age_lic1_quali_cout.[27,35)",
                         "vh_age_quali_cout.[1,6)",
                         "vh_cyl_quali_cout.[1000,1500)",
                         "vh_din_quali_cout.[100,150)",
                         "vh_sale_begin_quali_cout.[0,8)",
                         "vh_sale_end_quali_cout.[0,6)",
                         "vh_speed_quali_cout.[150,175)",
                         "vh_value_quali_cout.[12000,20000)",
                         "vh_weight_quali_cout.[800,1200)")

train_cout = cbind(train_cout, dummy_data_train_cout)
test_cout = cbind(test_cout, dummy_data_test_cout)

save(train_cout,file = "train_cout.RData")
save(test_cout, file = "test_cout.RData")

#sum(apply(X = test_cout, MARGIN = 2, FUN = function(x){sum(is.na(x))}) != 0)

#---------------------------------------------------------------------------------------------------------------------------------------------------
#
#															Statistiques descriptives
#
#---------------------------------------------------------------------------------------------------------------------------------------------------

# variables quali
## de base

# Certaines variables qualitatives ont trop de modalites pour etre representees avec
# la fonction "mean_sd_by_modal" : 
# pol_insee_code, vh_make, vh_model, 
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "pol_coverage")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "pol_pay_freq")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "pol_payd")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "pol_usage")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "drv_drv2")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "drv_sex1")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "drv_sex2")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "vh_fuel")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "vh_type")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "region")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "vh_make_bis")


# Quelques graphiques pour voir l'influence des variables quantitatives sur la cible 
scatter_plot_int(data = train_freq, var_y = "freq", var_x = "pol_duration")
scatter_plot_int(data = train_freq, var_y = "freq", var_x = "pol_sit_duration")
scatter_plot_int(data = train_freq, var_y = "freq", var_x = "drv_age1", y_lim_haut = c(0,0.3))
scatter_plot_int(data = train_freq, var_y = "freq", var_x = "drv_age2")
scatter_plot_int(data = train_freq, var_y = "freq", var_x = "drv_age_lic1")
scatter_plot_int(data = train_freq, var_y = "freq", var_x = "drv_age_lic2")
scatter_plot_int(data = train_freq, var_y = "freq", var_x = "vh_age")
scatter_plot_int(data = train_freq, var_y = "freq", var_x = "vh_cyl", y_lim_haut = c(0,0.5), y_bin = 50)
scatter_plot_int(data = train_freq, var_y = "freq", var_x = "vh_din", y_lim_haut = c(0,0.5), y_bin = 5)
scatter_plot_int(data = train_freq, var_y = "freq", var_x = "vh_sale_begin")
scatter_plot_int(data = train_freq, var_y = "freq", var_x = "vh_sale_end")
scatter_plot_int(data = train_freq, var_y = "freq", var_x = "vh_speed", y_lim_haut = c(0,0.45))
scatter_plot_int(data = train_freq, var_y = "freq", var_x = "vh_value", y_lim_haut = c(0,1), y_bin = 1000)
scatter_plot_int(data = train_freq, var_y = "freq", var_x = "vh_weight", y_bin = 60)
scatter_plot_int( data = train_freq, var_y = "freq", var_x = "pol_bonus")


# Graphiques pour les variables quanti decoupees en modalites

mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "pol_duration_quali_freq")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "pol_sit_duration_quali_freq")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "pol_bonus_quali_freq")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "drv_age1_quali_freq")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "drv_age2_quali_freq")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "drv_age_lic1_quali_freq")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "vh_age_quali_freq")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "vh_cyl_quali_freq")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "vh_din_quali_freq")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "vh_sale_begin_quali_freq")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "vh_sale_end_quali_freq")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "vh_speed_quali_freq")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "vh_value_quali_freq")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "vh_weight_quali_freq")



#Representation des variables
s.corcircle(acp$co,xax=1,yax=2)

#---------------------------------------------------------------------------------------------------------------------------------------------------
#
#															Decoupage des variables cout
#
#---------------------------------------------------------------------------------------------------------------------------------------------------



var_num<-colnames(base_cout)[sapply(base_cout[1,],is.numeric)]
var_num<-var_num[-grep("cout",var_num)]
var_num<-var_num[-grep("freq",var_num)]

#Variables pol_bonus
scatter_plot("cout","pol_bonus",base_cout,c(0,3000))
#Rien de vraiment significatif. Pourtant, je m'attends a trouver une relation du type bonus=0.5 gros sinistres et plus on se rapproche de 1.5 moins les sinistres
#sont importants. Un assure avec un bon bonus n'a pas interet a le perdre le perdre, il ne rapporte donc que les sinistres graves.

#Je ne vois pas ou couper
# 0.50, 0.51, Inf

#Variables pol_bonus
scatter_plot("cout","pol_duration",base_cout,c(0,2500)) 
#Difficile a dire. Je couperais regulierement.
# 1,3,6,9,14,22,Inf

#Variables pol_sit_duration
scatter_plot("cout","pol_sit_duration",base_cout,c(0,4000)) 
#Difficile a dire. Je couperais regulierement.
# 1,3,6,Inf

#Variables drv_age1
scatter_plot("cout","drv_age1",base_cout,c(0,5000)) 
#
# 18,25,35,50,70,78,Inf

#Variables drv_age2
scatter_plot("cout","drv_age2",base_cout,c(0,2500)) 
#
# 0,1,27,50,70,Inf

#Variables drv_age_lic1
scatter_plot("cout","drv_age_lic1",base_cout,c(0,2500)) 
#Enfin ! une pente
#1,19,27,35,45,55,Inf

#Variables drv_age_lic2
scatter_plot("cout","drv_age_lic2",base_cout,c(0,2500)) 
# on la laisse tomber
#ok

#Variables vh_age
scatter_plot("cout","vh_age",base_cout,c(0,5000)) 
#Enfin une variables d'interet !

#[0,8[ de 2 en 2 jusqu'a 15 et ensuite le reste
#1,6,8,10,12,14,17,Inf

#Variables vh_cyl
scatter_plot("cout","vh_cyl",base_cout,c(0,3000)) 
scatter_plot_int("cout","vh_cyl",base_cout,c(0,3000), 50)
# 0,1000 puis 1000,1500 puis 1500,1700 puis 1700,2000 puis 2000, 2200 puis 2200 3000 puis le reste
#ok

#Variables vh_din
scatter_plot("cout","vh_din",base_cout,c(0,5000)) 
scatter_plot_int("cout","vh_din",base_cout,c(0,5000), 5)
#0,25,50,75,100,150,200,300 lereste
#0,50,75,100,150,200,250 lereste


#Variables vh_sale_begin
scatter_plot("cout","vh_sale_begin",base_cout,c(0,5000)) 
#0,8,10,12,14,16,18 20 le reste
#ok

#Variables vh_sale_end
scatter_plot("cout","vh_sale_end",base_cout,c(0,5000)) 
#idem
#0,6,8,10,12,14,Inf


#Variables vh_speed
scatter_plot("cout","vh_speed",base_cout,c(0,5000)) 
scatter_plot_int("cout","vh_speed",base_cout, c(0,5000))
#O,100,150,175,200,250 le reste
#0,125,150,175,200,220,Inf


#Variables vh_value
scatter_plot("cout","vh_value",base_cout,c(0,4000))
scatter_plot_int("cout","vh_value",base_cout,c(0,4000),1000)
#0,2000,4000,5000,le reste (faut mettre un zero en plus)
#0,12000,20000,30000,40000,50000,Inf

#Variables vh_weight
scatter_plot("cout","vh_weight",base_cout,c(0,4000)) 
scatter_plot_int("cout","vh_weight",base_cout,c(0,4000),30)
#Couper regulierement
# 0,1,800, 1200,1600,2000, Inf

#Bonus la heatmap (pas evident a manipuler mais des resultats interessants)
heatmap(prop.table(table(cut(cout, breaks = c(seq(0,1000,by=100),seq(2000,8000,by=1000),Inf)),base_cout$vh_age)))

#Ce qui est interessant c'est a la fois l'aspect graphique et la classification hierarchique qui se trouve en haut

