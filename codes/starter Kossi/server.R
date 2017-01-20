# Project: Olivier_tarif_prov
# 
# Author: kossi
###############################################################################

# Chargement des packages necessaires

ChargePkg <- function(x){ # fonction pour charger plus facilement les packages
	x <- as.character(x)
	if(!require(x, character.only = TRUE)){
# 		install.packages(pkgs = x, repos = "http://cran.r-project.org")
		install.packages(pkgs = x, dependencies = TRUE)
		require(x, character.only = TRUE)
	}
}

pkgs  <- list("jsonlite", "ggplot2", "plyr", "tree", "shiny")

lapply(pkgs, ChargePkg)

PlotVarVersusNbsinistre <- function(varSinistre){
# Impact de l'age du conducteur (variable quantitative) sur la frequence de sinistres
	if(varSinistre == "ageconducteur"){
		
		reg1 <- glm(nombre~as.factor(ageconducteur),offset=log(exposition),data=BASEN,family=poisson(link="log"))
		age=seq(20,80)
		prednb1 <- predict(reg1,newdata=data.frame(ageconducteur=age,exposition=1),type="response")
		
		p <- ggplot(data = data.frame(age = age, prednb1 = prednb1), aes(x = age, y = prednb1))
		p <- p + geom_point(size = 3)
		p <- p + xlab("Age du conducteur") + ylab("Nombre de sinistres")
		
		reg2 <- glm(nombre~ageconducteur,offset=log(exposition),data=BASEN,family=poisson(link="log"))
		prednb2 <- predict(reg2,newdata=data.frame(ageconducteur=age,exposition=1),type="response")
		
		p <- p + geom_line(data = data.frame(age = age, prednb2 = prednb2), aes(x = age, y = prednb2), color = "blue")
		
		p	
	# Impact de l'age du vehicule (variable quantitative) sur la frequence de sinistres
	} else if(varSinistre == "agevehicule"){
		
		reg3 <- glm(nombre~as.factor(agevehicule),offset=log(exposition),data=BASEN,family=poisson(link="log"))
		agev=seq(0,40)
		prednb3<-predict(reg3,newdata=data.frame(agevehicule=agev,exposition=1),type="response")
		
		p <- ggplot(data = data.frame(agev = agev, prednb3 = prednb3), aes(x = agev, y = prednb3))
		p <- p + geom_point(size = 3)
		p <- p + xlab("Age du véhicule") + ylab("Nombre de sinistres")
		
		reg4<-glm(nombre~agevehicule,offset=log(exposition),data=BASEN,family=poisson(link="log"))
		prednb4<-predict(reg4,newdata=data.frame(agevehicule=agev,exposition=1),type="response")
		
		p <- p + geom_line(data = data.frame(agev = agev, prednb4 = prednb4), aes(x = agev, y = prednb4), color = "blue")
		p
	# Impact de anciennete du permis (variable quantitative) sur la frequence de sinistres	
	} else if(varSinistre == "agepermis") {
		
		reg5<-glm(nombre~as.factor(agepermis),offset=log(exposition),data=BASEN,family=poisson(link="log"))
		agep=seq(0,40)
		prednb5<-predict(reg5,newdata=data.frame(agepermis=agep,exposition=1),type="response")
		
		p <- ggplot(data = data.frame(agep = agep, prednb5 = prednb5), aes(x = agep, y = prednb5))
		p <- p + geom_point(size = 3)
		p <- p + xlab("Age du permis") + ylab("Nombre de sinistres")
		
		
		reg6<-glm(nombre~ agepermis,offset=log(exposition),data=BASEN,family=poisson(link="log"))
		prednb6<-predict(reg6,newdata=data.frame(agepermis =agep,exposition=1),type="response")
		
		p <- p + geom_line(data = data.frame(agep = agep, prednb6 = prednb6), aes(x = agep, y = prednb6), color = "blue")
		p
	# Impact du type du vehicule (variable qualitative) sur la frequence de sinistres		
	} else if(varSinistre == "voiture"){
		
		typeveh <- ddply(BASEN, ~voiture, summarise, Nb_sinistres = (sum(nombre) / sum(exposition)))
		p <- ggplot(data = typeveh, aes(x = voiture, y = Nb_sinistres, fill = voiture))
		p <- p + geom_bar(stat = "identity")
		p <- p + geom_text(aes(label=round(Nb_sinistres, 3)), vjust=-0.2, colour="red", size = 3.5)
		p	
	# Impact du sexe du conducteur principal sur la frequence de sinistres	
	}else if(varSinistre == "sexeconducteur"){
		
		sexeCond <- ddply(BASEN, ~sexeconducteur, summarise, Nb_sinistres = (sum(nombre) / sum(exposition)))
		p <- ggplot(data = sexeCond, aes(x = sexeconducteur, y = Nb_sinistres, fill = sexeconducteur))
		p <- p + geom_bar(stat = "identity")
		p <- p + geom_text(aes(label=round(Nb_sinistres, 3)), vjust=-0.2, colour="red", size = 3.5)
		p
	# Impact de la situation familiale du conducteur sur la frequence de sinistres
	}else if(varSinistre == "situationfamiliale"){
		
		sitFamilyCond <- ddply(BASEN, ~situationfamiliale, summarise, Nb_sinistres = (sum(nombre) / sum(exposition)))
		p <- ggplot(data = sitFamilyCond, aes(x = situationfamiliale, y = Nb_sinistres, fill = situationfamiliale))
		p <- p + geom_bar(stat = "identity")
		p <- p + geom_text(aes(label=round(Nb_sinistres, 3)), vjust=-0.2, colour="red", size = 3.5)
		p	
	# Impact la lieu d'habitation sur la frequence de sinistres
	}else if(varSinistre == "habitation"){
		
		habitationCond <- ddply(BASEN, ~habitation, summarise, Nb_sinistres = (sum(nombre) / sum(exposition)))
		p <- ggplot(data = habitationCond, aes(x = habitation, y = Nb_sinistres, fill = habitation))
		p <- p + geom_bar(stat = "identity")
		p <- p + geom_text(aes(label=round(Nb_sinistres, 3)), vjust=-0.2, colour="red", size = 3.5)
		p
	# Impact le statut d'habitation (locataire ou locataire) sur la frequence de sinistres
	}else if(varSinistre == "proprietaire"){
		
		proprietaireCond <- ddply(BASEN, ~proprietaire, summarise, Nb_sinistres = (sum(nombre) / sum(exposition)))
		p <- ggplot(data = proprietaireCond, aes(x = proprietaire, y = Nb_sinistres, fill = proprietaire))
		p <- p + geom_bar(stat = "identity")
		p <- p + geom_text(aes(label=round(Nb_sinistres, 3)), vjust=-0.2, colour="red", size = 3.5)
		p
	# Impact la frequence de paiement de la prime sur la frequence de sinistres
	} else if(varSinistre == "payment"){
		
		paymentCond <- ddply(BASEN, ~payment, summarise, Nb_sinistres = (sum(nombre) / sum(exposition)))
		p <- ggplot(data = paymentCond, aes(x = payment, y = Nb_sinistres, fill = payment))
		p <- p + geom_bar(stat = "identity")
		p <- p + geom_text(aes(label=round(Nb_sinistres, 3)), vjust=-0.2, colour="red", size = 3.5)
		p
	# Impact le poids du vehicule sur la frequence de sinistres
	}else if(varSinistre == "poids"){
		
		poidsCond <- ddply(BASEN, ~poids, summarise, Nb_sinistres = (sum(nombre) / sum(exposition)))
		p <- ggplot(data = poidsCond, aes(x = poids, y = Nb_sinistres, fill = poids))
		p <- p + geom_bar(stat = "identity")
		p <- p + geom_text(aes(label=round(Nb_sinistres, 3)), vjust=-0.2, colour="red", size = 3.5)
		p
	# Impact la marque du vehicule sur la frequence de sinistres
	}else if(varSinistre == "marque"){
		
		marqueCond <- ddply(BASEN, ~marque, summarise, Nb_sinistres = (sum(nombre) / sum(exposition)))
		p <- ggplot(data = marqueCond, aes(x = marque, y = Nb_sinistres, fill = marque))
		p <- p + geom_bar(stat = "identity")
		p <- p + geom_text(aes(label=round(Nb_sinistres, 3)), vjust=-0.2, colour="red", size = 3.5)
		p
	} else{}	
	
}



PlotVarVersusCout <- function(varCout){
	
	# Etude des couts individuels en fonction du sexe du conducteur
	if(varCout == "sexeconducteur"){
		p <- ggplot(data = BASEY, aes(x = sexeconducteur, y = cout, fill = sexeconducteur))
		p <- p + geom_boxplot() 
		p <- p + coord_flip() # Renverser le graphique
		p <- p + scale_y_log10()
		p
	# Etude des couts individuels en fonction de la situation familiale du conducteur
	}else if(varCout == "situationfamiliale"){
		p <- ggplot(data = BASEY, aes(x = situationfamiliale, y = cout, fill = situationfamiliale))
		p <- p + geom_boxplot() 
		p <- p + coord_flip() # Renverser le graphique
		p <- p + scale_y_log10()
		p
	# Etude des couts individuels en fonction du lieu d'habitation
	}else if(varCout == "habitation"){
		p <- ggplot(data = BASEY, aes(x = habitation, y = cout, fill = habitation))
		p <- p + geom_boxplot() 
		p <- p + coord_flip() # Renverser le graphique
		p <- p + scale_y_log10()
		p	
	# Etude des couts individuels en fonction du statut d'habitation (locataire, proprietaire)
	}else if(varCout == "proprietaire"){
		p <- ggplot(data = BASEY, aes(x = proprietaire, y = cout, fill = proprietaire))
		p <- p + geom_boxplot() 
		p <- p + coord_flip() # Renverser le graphique
		p <- p + scale_y_log10()
		p
	# Etude des couts individuels en fonction du mode de paiement de la prime
	}else if(varCout == "payment"){
		p <- ggplot(data = BASEY, aes(x = payment, y = cout, fill = payment))
		p <- p + geom_boxplot() 
		p <- p + coord_flip() # Renverser le graphique
		p <- p + scale_y_log10()
		p
	# cout de sinistres versus age du conducteur
	}else if(varCout == "ageconducteur"){
		regr1=glm(cout~as.factor(ageconducteur),data=BASEY,family=Gamma(link="log"))
		age=c(seq(22,29),seq(31,80))
		prednb1=predict(regr1,newdata=data.frame(ageconducteur=age),type="response")
		
		p <- ggplot(data = data.frame(age = age, prednb1 = prednb1), aes(x = age, y = prednb1))
		p <- p + geom_point(size = 3)
		
		
		regr2=glm(cout~ageconducteur,data=BASEY,family=Gamma(link="log"))
		prednb2=predict(regr2,newdata=data.frame(ageconducteur=age),type="response")
		
		p <- p + geom_line(data = data.frame(age = age, prednb2 = prednb2), aes(x = age, y = prednb2), color = "blue")
		p
	}else if(varCout == "agevehicule"){
		regr3=glm(cout~as.factor(agevehicule),data=BASEY,family=Gamma(link="log"))
		agev=seq(0,29)
		prednb3=predict(regr3,newdata=data.frame(agevehicule=agev),type="response")
		
		p <- ggplot(data = data.frame(agev = agev, prednb3 = prednb3), aes(x = agev, y = prednb3))
		p <- p + geom_point(size = 3)
		
		
		regr4=glm(cout~agevehicule,data=BASEY,family=Gamma(link="log"))
		prednb4=predict(regr4,newdata=data.frame(agevehicule=agev),type="response")
		
		p <- p + geom_line(data = data.frame(agev = agev, prednb4 = prednb4), aes(x = agev, y = prednb4), color = "blue")
		p
	}else{}
}

Modele <- function(choixModele){
	if(choixModele == "Modele_frequence"){
		
		reg13=glm(nombre~ ageconducteur+proprietaire+ payment+situationfamiliale+ agepermis+marque+voiture+ sexeconducteur+
				  habitation+proprietaire,offset=log(exposition),data=BASEN,family=poisson(link="log"))
        summary(reg13)
	}else if(choixModele == "Modele_cout"){
		regr5=glm(cout~.-exposition,data=BASEY,family=Gamma(link="log"))
		summary(regr5)
		
	}else{}
		
	
}



# modeleArbre <- function(){
#	
#	arbre=rpart(nombre~ageconducteur+agepermis+sexeconducteur+situationfamiliale+habitation+agevehicule+proprietaire+payment+
#					marque+poids+usage+voiture,data=BASEN)
#	plot(arbre)
#	text(arbre)
# }

arbre <- function(){
	
	arbre=tree(nombre~ageconducteur+agepermis+sexeconducteur+situationfamiliale+habitation+agevehicule+proprietaire+payment+
			   marque+poids+usage+voiture,data=BASEN,split="gini", mincut = 3500)
	plot(arbre)
	text(arbre)
	
}
shinyServer(
		function(input,output)
		{
			
# Affichage de la base de sinistres			
			output$baseSinistre <- renderDataTable({
						
						BASEN
								
					}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
			
# Affichage de la base de coût
			output$baseCout <- renderDataTable({
						
						BASEY
						
					}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
			
# Affichage influence variables tarifaires sur nombre de sinistres
			output$influenceSinistre <- renderPlot({
						p  <- PlotVarVersusNbsinistre(input$variable_sinistre)
						
						print(p)		
					})
			
# Affichage influence variables tarifaires sur cout de sinistres
			output$influenceCout <- renderPlot({
						p  <- PlotVarVersusCout(input$variable_cout)
						# if(input$logScale) p <- p + scale_y_log10()
						print(p)		
					})
			
# Affichage influence variables tarifaires sur cout de sinistres
			output$Modeles <- renderPrint({
							
						Modele(input$Model)
					})
			output$Arbre <- renderPlot({
						
						arbre()

					})
			
		}
)

