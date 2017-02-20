
#---------------------------------------------------------------------------------------------------------------------------------------------------
#
#															Fonctions
#
#---------------------------------------------------------------------------------------------------------------------------------------------------


mean_sd_by_modal = function(data, var_y, var_x, scale_y = 0){
  ### fonction qui permet de representer pour une variable quali, la moyenne et l'Ã©cart type par modalitÃ©, ainsi que la
  ### la distribution de var_x
  # data : data.frame ; contient var_y et var_x
  # var_y : string ; nom de la variable Y
  # var_x : string ; nom de la variable X
  # scale_y : numeric >= 0 (par defaut 0) ; permet d'elargir la fenetre des valeurs de Y
  stat = aggregate(x = data[,var_y],
                   by = list(var_x = data[,var_x]),
                   FUN = function(x){c(moyenne = mean(x),ecart_type = sd(x))})
  
  stat$moyenne = stat$x[,"moyenne"]
  stat$ecart_type = stat$x[,"ecart_type"] / 4
  stat$borne_inf = stat$moyenne - stat$ecart_type
  stat$borne_sup = stat$moyenne + stat$ecart_type
  stat$x2 = stat$var_x
  
  plot1 = ggplot( aes_string(x = var_x) , 
                  data = data[!is.na(data[,var_y]) ,]) + # & (data[,var_y] > 0)
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size =10)) +
    #scale_y_continuous(labels = scales::percent) +
    theme(text = element_text(size = 17)) +
    ylab(paste0("Distribution ",var_x)) +
    xlab("")
  
  plot2 = ggplot(data = stat, aes(x = factor(x2), y = moyenne, group = 1)) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = borne_inf, ymax = borne_sup), fill = "blue", alpha = 0.3) +
    theme(text = element_text(size=17),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x = element_blank()) +
    ylab(paste0("Moyenne ", var_y)) +
    ggtitle(paste0("Moyenne par modalite +/- 1/4.ecart type : ",var_x)) +
    scale_y_continuous(limits = c(stat$borne_inf[which.min(stat$borne_inf)] - 
                                    (stat$moyenne[which.min(stat$borne_inf)] -
                                       stat$borne_inf[which.min(stat$borne_inf)]) * scale_y,
                                  stat$borne_sup[which.max(stat$borne_sup)] - 
                                    (stat$moyenne[which.max(stat$borne_sup)] -
                                       stat$borne_sup[which.max(stat$borne_sup)]) * scale_y ) )
  
  multiplot(plot2, plot1, cols = 1)
}


scatter_plot = function(var_y, var_x, data, vec_y_lim){
  
  if(missing(vec_y_lim))
  {
    plot2 = ggplot(data = data, aes_string(x = var_x, y = var_y)) +
      geom_point(alpha = 0.3, colour = "blue") +
      geom_smooth(colour = "red") +
      theme_bw() +
      theme(text = element_text(size = 20)) +
      ggtitle(paste0("Influence de la variable ",var_x, " sur ",var_y))
  }
  else
  {
    plot2 = ggplot(data = data, aes_string(x = var_x, y = var_y)) +
      geom_point(alpha = 0.3, colour = "blue") +
      ylim(vec_y_lim[1],vec_y_lim[2]) +
      geom_smooth(colour = "red") +
      theme_bw() +
      theme(text = element_text(size = 20)) +
      ggtitle(paste0("Influence de la variable ",var_x, " sur ",var_y))
  }
  
  # var_x <- "pol_bonus"
  # var_y <- "cout"
  # data <- base_cout
  
  plot1 = ggplot( aes_string(x = var_x) , 
                  data = data[!is.na(data[,var_y]) ,]) + # & (data[,var_y] > 0)
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size =10)) +
    #scale_y_continuous(labels = scales::percent) +
    theme(text = element_text(size = 17)) +
    ylab(paste0("Distribution ",var_x)) +
    xlab("")
  
  multiplot(plot2, plot1, cols = 1)
}


scatter_plot_int = function(var_y, var_x, data, y_lim_haut, y_bin){
  
  if(!missing(y_bin)){
    data[,var_x] = data[,var_x] - (data[,var_x] %% y_bin)
  }
  a = data[,c(var_x, var_y)]
  colnames(a) = c("x","y")
  to_plot = a %>% 
    group_by(x) %>% 
    summarise(y = mean(y))
  
  if(missing(y_lim_haut)){
    plot1 = ggplot(data = to_plot, aes(x = x, y = y)) +
      geom_point(alpha = 0.3, colour = "blue") +
      geom_smooth(colour = "red", method = 'loess') +
      theme_bw() +
      theme(text = element_text(size = 20)) +
      ggtitle(paste0("Influence de la variable ",var_x, " sur ",var_y))
  } else {
    plot1 = ggplot(data = to_plot, aes(x = x, y = y)) +
      geom_point(alpha = 0.3, colour = "blue") +
      geom_smooth(colour = "red", method = 'loess') +
      theme_bw() +
      theme(text = element_text(size = 20)) +
      ylim(y_lim_haut) +
      ggtitle(paste0("Influence de la variable ",var_x, " sur ",var_y))
  }
  
  plot2 = ggplot( aes_string(x = var_x) , 
                  data = data[!is.na(data[,var_y]) & (data[,var_y] > 0),]) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    #scale_y_continuous(labels = scales::percent) +
    theme(text = element_text(size = 17)) +
    ylab(paste0("Distribution ",var_x)) +
    xlab("")
  
  multiplot(plot1, plot2, cols = 1)
}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  ## fonction qui permet de faire des images avec plusieurs graphiques avec ggplot2
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


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

dummy_data_freq = predict(b_freq, newdata = train_freq[,x_var_quali_freq])
x_var_quali_freq_dummy = colnames(dummy_data_freq)

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

train_freq = cbind(train_freq, dummy_data_freq)
save(train_freq, file = "train_freq.RData")

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


# a3 = merge(x = data_prospect0,
#                   y = a2,
#                   by.x = c("id_client","id_vehicle"),
#                   by.y = c("id_client","id_vehicle"),
#                   all.x = F )

#base_cout = merge(x = data_prospect[which(data_prospect$id_year == "Year 0"),],
# y = a2,
# by.x = c("id_client","id_vehicle"),
# by.y = c("id_client","id_vehicle"),
# all.x = F)

# save(base_cout, file = "base_cout.RData")


# save(train_freq, file = "train_freq.RData")
# save(base_cout, file = "base_cout.RData")

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

#head(train_cout[which(train_cout$cout < 0),])


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

dummy_data_cout = predict(b_cout, newdata = train_cout[,x_var_quali_cout])
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


train_cout = cbind(train_cout, dummy_data_cout)

apply(train_cout[,51:192],2,sum)

save(train_cout,file = "train_cout.RData")

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

