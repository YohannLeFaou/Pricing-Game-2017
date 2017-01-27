library(dplyr)
library(ggplot2)
source("codes/INIT.R")

#---------------------------------------------------------------------------------------------------------------------------------------------------
#
#															Fonctions
#
#---------------------------------------------------------------------------------------------------------------------------------------------------


mean_sd_by_modal = function(data, var_y, var_x, scale_y = 0){
### fonction qui permet de représenter pour une variable quali, la moyenne et l'écart type par modalité, ainsi que la
### la distribution de var_x
# data : data.frame ; contient var_y et var_x
# var_y : string ; nom de la variable Y
# var_x : string ; nom de la variable X
# scale_y : numeric >= 0 (par défaut 0) ; permet d'élargir la fenêtre des valeurs de Y
		stat = aggregate(x = data[,var_y],
		               by = list(var_x = data[,var_x]),
		               FUN = function(x){c(moyenne = mean(x),ecart_type = sd(x))})

		stat$moyenne = stat$x[,"moyenne"]
		stat$ecart_type = stat$x[,"ecart_type"] / 4
		stat$borne_inf = stat$moyenne - stat$ecart_type
		stat$borne_sup = stat$moyenne + stat$ecart_type
		stat$x2 = stat$var_x

plot1 = ggplot( aes_string(x = var_x) , 
     	data = data[!is.na(data[,var_y]) & (data[,var_y] > 0),]) +
		geom_bar(aes(y = (..count..)/sum(..count..))) +
		scale_y_continuous(labels = scales::percent) +
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
		ggtitle(paste0("Moyenne par modalit? +/- 1/4.ecart type : ",var_x)) +
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

scatter_plot_int = function(var_y, var_x, data){
    a = data[,c(var_x, var_y)]
    colnames(a) = c("x","y")
    to_plot = a %>% 
    group_by(x) %>% 
    summarise(y = mean(y))
    ggplot(data = to_plot, aes(x = x, y = y)) +
    geom_point(alpha = 0.3, colour = "blue") +
    geom_smooth(colour = "red") +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    ggtitle(paste0("Influence de la variable ",var_x, " sur ",var_y))
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
#															Import des données
#
#---------------------------------------------------------------------------------------------------------------------------------------------------

setwd(YOHANN.DIR)

data_claim0 = read.csv(file = "../../data/PG_2017_CLAIMS_YEAR0.csv", sep = ",", dec = ".", header = T)
data_prospect0 = read.csv(file = "../../data/PG_2017_YEAR0.csv", sep = ",", dec = ".", header = T)
data_prospect1 = read.csv(file = "../../data/PG_2017_YEAR1.csv", sep = ",", dec = ".", header = T)

#Remarques sur les données
#Les sinistres en 0 (data_claim0)

#Parfois il y a des trop payés, ce qui explique les somme négatives (qui sont des régularisations par l'assureur)
#Exemples
data_claim0[which(data_claim0$id_client == "A00033511"),]
data_claim0[which(data_claim0$id_client == "A00058104"),]
data_claim0[which(data_claim0$id_client == "A00000241"),]
data_claim0[which(data_claim0$id_client == "A00003988"),] ## somme des sinistres est n?gative sur 1 an


#Dans la base (data_claim0) une ligne vaut pour un client et une voiture et une déclaration
#Pour la base des coûts moyens le regroupement doit se faire de cette manière. On doit garder la maille la plus fine.
data_claim0[which(data_claim0$id_client == "A00044168"),]

#Vérification
nrow(count_(data_claim0, vars = c("id_client","id_vehicle","id_claim"))) == nrow(data_claim0)
nrow(count_(data_claim0, vars = c("id_client","id_vehicle")))

head(data_prospect0)
summary(data_claim0)

#---------------------------------------------------------------------------------------------------------------------------------------------------
#
#															Construction des bases (fréquence et coût)
#
#---------------------------------------------------------------------------------------------------------------------------------------------------

#Base fréquence

a1 = data_claim0 %>% 
  group_by(id_client, id_vehicle) %>% 
  summarise(freq = n())


base_freq = merge(x = data_prospect0,
                  y = a1,
                  by.x = c("id_client","id_vehicle"),
                  by.y = c("id_client","id_vehicle"),
                  all.x = T )

base_freq$freq[ is.na(base_freq$freq)] = 0


#Base coût moyen

a2 = data_claim0 %>% 
  group_by(id_client, id_vehicle, id_claim) %>% 
  summarise(cout = mean(claim_amount))

a3 = merge(x = data_prospect0,
                  y = a2,
                  by.x = c("id_client","id_vehicle"),
                  by.y = c("id_client","id_vehicle"),
                  all.x = F )

base_cout = merge(x = a3,
                  y = base_freq,
                  by.x = c("id_client","id_vehicle"),
                  by.y = c("id_client","id_vehicle"),
                  all.x = T)

#---------------------------------------------------------------------------------------------------------------------------------------------------
#
#															Construction des bases (fréquence et coût)
#
#---------------------------------------------------------------------------------------------------------------------------------------------------

str(base_freq)

#Certaines variables qualitatives ont trop de modalités pour être représentées.
# pol_insee_code, vh_make, vh_model, 
mean_sd_by_modal(data = base_freq, var_y = "freq", var_x = "pol_coverage")
mean_sd_by_modal(data = base_freq, var_y = "freq", var_x = "pol_pay_freq")
mean_sd_by_modal(data = base_freq, var_y = "freq", var_x = "pol_payd")
mean_sd_by_modal(data = base_freq, var_y = "freq", var_x = "pol_usage")
mean_sd_by_modal(data = base_freq, var_y = "freq", var_x = "drv_drv2")
mean_sd_by_modal(data = base_freq, var_y = "freq", var_x = "drv_sex1")
mean_sd_by_modal(data = base_freq, var_y = "freq", var_x = "drv_sex2")
mean_sd_by_modal(data = base_freq, var_y = "freq", var_x = "vh_fuel")
mean_sd_by_modal(data = base_freq, var_y = "freq", var_x = "vh_type")
#Proposition pour les variables ayant trop de modalités
#Pour pol_insee on fait des départements
mean_sd_by_modal(data = data.frame(freq=base_freq$freq, departement=substr(base_freq$pol_insee_code,1,2)), var_y = "freq", var_x = "departement")
#Résultat pas top, difficile à exploiter surtout dans un GLM

#Quelques régressions pour voir l'influence des variables quantitatives sur la cible 
scatter_plot_int(data = base_freq, var_y = "freq", var_x = "pol_duration")
scatter_plot_int(data = base_freq, var_y = "freq", var_x = "pol_sit_duration")
scatter_plot_int(data = base_freq, var_y = "freq", var_x = "drv_age1")
scatter_plot_int(data = base_freq, var_y = "freq", var_x = "drv_age2")
scatter_plot_int(data = base_freq, var_y = "freq", var_x = "drv_age_lic1")
scatter_plot_int(data = base_freq, var_y = "freq", var_x = "drv_age_lic2")
scatter_plot_int(data = base_freq, var_y = "freq", var_x = "vh_age")
scatter_plot_int(data = base_freq, var_y = "freq", var_x = "vh_cyl")
scatter_plot_int(data = base_freq, var_y = "freq", var_x = "vh_din")
scatter_plot_int(data = base_freq, var_y = "freq", var_x = "vh_sale_begin")
scatter_plot_int(data = base_freq, var_y = "freq", var_x = "vh_sale_end")
scatter_plot_int(data = base_freq, var_y = "freq", var_x = "vh_speed")
scatter_plot_int(data = base_freq, var_y = "freq", var_x = "vh_sale_end")
scatter_plot_int(data = base_freq, var_y = "freq", var_x = "vh_value")
scatter_plot_int(data = base_freq, var_y = "freq", var_x = "vh_weight")


#---------------------------------------------------------------------------------------------------------------------------------------------------
#
#															Analyse en composantes principales
#
#---------------------------------------------------------------------------------------------------------------------------------------------------

num<-which(sapply(base_freq[1,],is.numeric))

#Il reste un NA
which(is.na(base_freq),arr.ind = TRUE)

base_freq[840,20]<-0

library(ade4)

#Je retiens 4 axes principaux
acp<-dudi.pca(base_freq[,num], center = TRUE, scale = TRUE)

#Pourcentages de variance cumulée
cumsum(acp$eig/sum(acp$eig)*100)

#Représentation des individus (pas très utile ici)
plot(acp$li[,1],acp$li[,2])

#Représentation des variables
s.corcircle(acp$co,xax=1,yax=2)
