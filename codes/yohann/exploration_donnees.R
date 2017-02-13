

#---------------------------------------------------------------------------------------------------------------------------------------------------
#
#															Fonctions
#
#---------------------------------------------------------------------------------------------------------------------------------------------------


mean_sd_by_modal = function(data, var_y, var_x, scale_y = 0){
### fonction qui permet de representer pour une variable quali, la moyenne et l'écart type par modalité, ainsi que la
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

scatter_plot = function(var_y, var_x, data){
    ggplot(data = data, aes_string(x = var_x, y = var_y)) +
    geom_point(alpha = 0.3, colour = "blue") +
    geom_smooth(colour = "red") +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    ggtitle(paste0("Influence de la variable ",var_x, " sur ",var_y))
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

#Remarques sur les donnees
## valeurs manquantes
apply(X = data_claim0, MARGIN = 2, FUN = function(x){sum(is.na(x))})
apply(X = data_prospect0, MARGIN = 2, FUN = function(x){sum(is.na(x))})
apply(X = data_prospect1, MARGIN = 2, FUN = function(x){sum(is.na(x))})


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
  cut(data_prospect$pol_duration, breaks = c(0,30,Inf),include.lowest = F, right = F, ordered_result = T)
data_prospect$pol_sit_duration_quali_freq = 
  cut(data_prospect$pol_sit_duration, breaks = c(1,2,3,4,5,6,Inf),include.lowest = F, 
      right = F, ordered_result = T, labels = c("1","2","3","4","5","[6,Inf)"))
data_prospect$drv_age1_quali_freq =
  cut(data_prospect$drv_age1, breaks = c(0,45,75,Inf),include.lowest = F, right = F, ordered_result = T)
data_prospect$drv_age2_quali_freq =
  cut(data_prospect$drv_age2, breaks = c(0,1,28,75,Inf),include.lowest = F, right = F, ordered_result = T)
data_prospect$drv_age_lic1_quali_freq =
  cut(data_prospect$drv_age_lic1, breaks = c(0,5,15,30,55,Inf),include.lowest = F, right = F, ordered_result = T)
data_prospect$vh_age_quali_freq = 
  cut(data_prospect$vh_age, breaks = c(0,5,10,15,Inf),include.lowest = F, right = F, ordered_result = T)
data_prospect$vh_cyl_quali_freq =
  cut(data_prospect$vh_cyl, breaks = c(0,1200,1400,1600,1800,2100,Inf),
      include.lowest = F, right = F, ordered_result = T, dig.lab = 4)
data_prospect$vh_din_quali_freq = 
  cut(data_prospect$vh_din, breaks = c(0,70,85,100,115,140,Inf),include.lowest = F, 
      right = F, ordered_result = T)
data_prospect$vh_sale_begin_quali_freq = 
  cut(data_prospect$vh_sale_begin, breaks = c(0,10,20,Inf),include.lowest = F, right = F, ordered_result = T)
data_prospect$vh_sale_end_quali_freq = 
  cut(data_prospect$vh_sale_end, breaks = c(0,10,20,Inf),include.lowest = F, right = F, ordered_result = T)
data_prospect$vh_speed_quali_freq = 
  cut(data_prospect$vh_speed, breaks = c(0,150,170,190,215,Inf),include.lowest = F, right = F, ordered_result = T)
data_prospect$vh_value_quali_freq =
  cut(data_prospect$vh_value, breaks = c(0,10000,20000,30000,40000,Inf),include.lowest = F,dig.lab = 5,
      right = F, ordered_result = T)
data_prospect$vh_weight_quali_freq =
  cut(data_prospect$vh_weight, breaks = c(0,800,1000,1300,1500,1800,Inf),include.lowest = F,dig.lab = 4, 
      right = F, ordered_result = T)
data_prospect$pol_bonus_quali_freq =
  cut(data_prospect$pol_bonus, breaks = c(0.5,0.51,Inf),include.lowest = F, 
      right = F, ordered_result = T)

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

b = dummyVars(formula = as.formula( paste0("~", paste0(x_var_quali_freq , collapse = " + ") )), 
              data = train_freq[,x_var_quali_freq])
dummy_data_freq = predict(b, newdata = train_freq[,x_var_quali_freq])
x_var_quali_freq_dummy = colnames(dummy_data_freq)

train_freq = cbind(train_freq, dummy_data_freq)





str(train_freq[,x_var_quali_freq])
#Base cout moyen

a2 = data_claim0 %>% 
  group_by(id_client, id_vehicle, id_claim) %>% 
  summarise(cout = mean(claim_amount))



# a3 = merge(x = data_prospect0,
#                   y = a2,
#                   by.x = c("id_client","id_vehicle"),
#                   by.y = c("id_client","id_vehicle"),
#                   all.x = F )

base_cout = merge(x = a2,
                  y = train_freq,
                  by.x = c("id_client","id_vehicle"),
                  by.y = c("id_client","id_vehicle"),
                  all.x = T)


# save(train_freq, file = "train_freq.RData")
# save(base_cout, file = "base_cout.RData")


#Pour ces 5 premières variables je n'ai pas trouvé de seuil intéressant

data_prospect$pol_duration_quali_cout = 
  cut(data_prospect$pol_duration, breaks = c(0,30,Inf),include.lowest = F, right = F, ordered_result = T)

data_prospect$pol_sit_duration_quali_cout = 
  cut(data_prospect$pol_sit_duration, breaks = c(1,2,3,4,5,6,Inf),include.lowest = F, 
      right = F, ordered_result = T, labels = c("1","2","3","4","5","[6,Inf)"))

data_prospect$drv_age1_quali_cout =
  cut(data_prospect$drv_age1, breaks = c(0,45,75,Inf),include.lowest = F, right = F, ordered_result = T)

data_prospect$drv_age2_quali_cout =
  cut(data_prospect$drv_age2, breaks = c(0,1,28,75,Inf),include.lowest = F, right = F, ordered_result = T)

data_prospect$drv_age_lic1_quali_cout =
  cut(data_prospect$drv_age_lic1, breaks = c(0,5,15,30,55,Inf),include.lowest = F, right = F, ordered_result = T)

#Des variabes que j'ai découpé

data_prospect$vh_age_quali_cout = 
  cut(data_prospect$vh_age, breaks = c(1,5,seq(6,15,by=2),Inf),include.lowest = F, right = F, ordered_result = T)

data_prospect$vh_cyl_quali_cout =
  cut(data_prospect$vh_cyl, breaks = c(0,1000,1500,1700,2000,2200,3000,Inf),
      include.lowest = F, right = F, ordered_result = T, dig.lab = 4)

data_prospect$vh_din_quali_cout = 
  cut(data_prospect$vh_din, breaks = c(0,25,30,75,100,150,200,300,Inf),include.lowest = F, 
      right = F, ordered_result = T)

data_prospect$vh_sale_begin_quali_cout = 
  cut(data_prospect$vh_sale_begin, breaks = c(0,8,10,12,14,16,18,20,Inf),include.lowest = F, right = F, ordered_result = T)

data_prospect$vh_sale_end_quali_cout = 
  cut(data_prospect$vh_sale_end, breaks = c(0,8,10,12,14,16,18,20,Inf),include.lowest = F, right = F, ordered_result = T)

data_prospect$vh_speed_quali_cout = 
  cut(data_prospect$vh_speed, breaks = c(0,100,150,175,200,250,Inf),include.lowest = F, right = F, ordered_result = T)

data_prospect$vh_value_quali_cout =
  cut(data_prospect$vh_value, breaks = c(0,20000,40000,50000,Inf),include.lowest = F,dig.lab = 5,
      right = F, ordered_result = T)

#Je n'ai pas trouvé non plus pour la dernière variable

data_prospect$vh_weight_quali_cout =
  cut(data_prospect$vh_weight, breaks = c(0,800,1000,1300,1500,1800,Inf),include.lowest = F,dig.lab = 4, 
      right = F, ordered_result = T)

#---------------------------------------------------------------------------------------------------------------------------------------------------
#
#															Statistiques descriptives
#
#---------------------------------------------------------------------------------------------------------------------------------------------------

str(train_freq)

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
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "pol_bonus")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "region")
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "vh_make_bis")


#Proposition pour les variables ayant trop de modalites
#Pour pol_insee on fait des departements
mean_sd_by_modal(data = data.frame(freq=train_freq$freq, departement=substr(train_freq$pol_insee_code,1,2)), var_y = "freq", var_x = "departement")
#Resultat pas top, difficile a exploiter surtout dans un GLM

#Quelques regressions pour voir l'influence des variables quantitatives sur la cible 
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



scatter_plot(var_y = "freq", var_x = "pol_bonus", data = train_freq)
mean_sd_by_modal(data = train_freq, var_y = "freq", var_x = "pol_bonus")
scatter_plot_int( data = train_freq, var_y = "freq", var_x = "pol_bonus")

#---------------------------------------------------------------------------------------------------------------------------------------------------
#
#															Analyse en composantes principales
#
#---------------------------------------------------------------------------------------------------------------------------------------------------

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

#---------------------------------------------------------------------------------------------------------------------------------------------------
#
#															Découpage des variables cout
#
#---------------------------------------------------------------------------------------------------------------------------------------------------

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

var_num<-colnames(base_cout)[sapply(base_cout[1,],is.numeric)]
var_num<-var_num[-grep("cout",var_num)]
var_num<-var_num[-grep("freq",var_num)]

#Variables pol_bonus
scatter_plot("cout","pol_bonus",base_cout,c(0,3000)) 
#Rien de vraiment significatif. Pourtant, je m'attends à trouver une relation du type bonus=0.5 gros sinistres et plus on se rapproche de 1.5 moins les sinistres
#sont importants. Un assuré avec un bon bonus n'a pas intérêt à le perdre, il ne rapporte donc que les sinistres graves.

#Je ne vois pas où couper

#Variables pol_bonus
scatter_plot("cout","pol_duration",base_cout,c(0,2500)) 
#Difficile à dire. Je couperais régulièrement.

#Variables pol_sit_duration
scatter_plot("cout","pol_sit_duration",base_cout,c(0,4000)) 
#Difficile à dire. Je couperais régulièrement.

#Variables drv_age1
scatter_plot("cout","drv_age1",base_cout,c(0,5000)) 

#Variables drv_age2
scatter_plot("cout","drv_age2",base_cout,c(0,2500)) 

#Variables drv_age_lic1
scatter_plot("cout","drv_age_lic1",base_cout,c(0,2500)) 
#Enfin ! une pente

#Variables drv_age_lic2
scatter_plot("cout","drv_age_lic2",base_cout,c(0,2500)) 

#Variables vh_age
scatter_plot("cout","vh_age",base_cout,c(0,5000)) 
#Enfin une variables d'intérêt !

#[0,8[ de 2 en 2 jusqu'à 15 et ensuite le reste

#Variables vh_cyl
scatter_plot("cout","vh_cyl",base_cout,c(0,3000)) 

# 0,1000 puis 1000,1500 puis 1500,1700 puis 1700,2000 puis 2000, 2200 puis 2200 3000 puis le reste

#Variables vh_din
scatter_plot("cout","vh_din",base_cout,c(0,5000)) 

#0,25,50,75,100,150,200,300 lereste

#Variables vh_sale_begin
scatter_plot("cout","vh_sale_begin",base_cout,c(0,5000)) 

#0,8,10,12,14,16,18 20 le reste

#Variables vh_sale_end
scatter_plot("cout","vh_sale_end",base_cout,c(0,5000)) 

#idem

#Variables vh_speed
scatter_plot("cout","vh_speed",base_cout,c(0,5000)) 

#O,100,150,175,200,250 le reste

#Variables vh_value
scatter_plot("cout","vh_value",base_cout,c(0,4000))

#0,2000,4000,5000,le reste

#Variables vh_weight
scatter_plot("cout","vh_weight",base_cout,c(0,4000)) 

#Couper régulièrement

#Bonus la heatmap (pas évident à manipuler mais des résltats intéressants)

heatmap(prop.table(table(cut(cout, breaks = c(seq(0,1000,by=100),seq(2000,8000,by=1000),Inf)),base_cout$vh_age)))

#Ce qui est intéressant c'est à la fois l'aspect graphique et la classification hiérarchique qui se trouve en haut
