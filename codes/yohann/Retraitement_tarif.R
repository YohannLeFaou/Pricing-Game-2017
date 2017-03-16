
setwd("Y:/Forsides France/06_Solo/Yohann LE FAOU/mission_forsides/pricing_game_2017")
source("codes/INIT.R")
setwd(YOHANN.DIR)

pred_cout <- read.csv2("results_cout/prediction_cout_year1.csv",header=TRUE,sep=";")
pred_cout$prediction<-as.numeric(as.character(pred_cout$prediction))

pred_freq <- read.csv2("results_freq/prediction_freq_year1.csv",header=TRUE,sep=";")
pred_freq$prediction<-as.numeric(as.character(pred_freq$prediction))

claim <- read.csv2("../../data/PG_2017_CLAIMS_YEAR0.csv",header=TRUE,sep=",")
claim$claim_amount <- as.numeric(as.character(claim$claim_amount))

S_sur_P<-100*sum(claim$claim_amount)/sum(pred_cout$prediction*pred_freq$prediction)

attach(claim)

sinistres<-aggregate(claim[,5], by=list(id_client,id_vehicle), sum)

df<-data.frame(id_policy = paste(sinistres[,1],sinistres[,2],sep="-"), Nb_sinistres = sinistres[,3])

final<-data.frame(id_policy = pred_cout$id_policy,
                  Fréquence = pred_freq$prediction,
                  Coût = pred_cout$prediction)

final<-merge(final,
		 df,
		 by.x=c("id_policy"),
		 by.y=c("id_policy"),
		 all.x=TRUE)

final[is.na(final)] <- 0

final$Tarif_115 <- final$Fréquence * final$Coût * S_sur_P/115

S_sur_P_corrigé <- 100*sum(claim$claim_amount)/sum(final$Tarif_115)

final$Tarif_Final <- rep(0, dim(final)[1])
final$Tarif_Final[final$Nb_sinistres==0] <- final$Tarif_115[final$Nb_sinistres==0]
final$Tarif_Final[final$Nb_sinistres==1] <- final$Tarif_115[final$Nb_sinistres==1] * 1.30
final$Tarif_Final[final$Nb_sinistres>1] <- final$Tarif_115[final$Nb_sinistres>1] * 1.35

S_sur_P_final<-100*sum(claim$claim_amount)/sum(final$Tarif_Final)

ecriture <- data.frame(id_policy = final$id_policy, premium = final$Tarif_Final)

write.csv2(ecriture, file = "Tarif_year1.csv", row.names=FALSE)
