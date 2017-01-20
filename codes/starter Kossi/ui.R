# Project: Olivier_tarif_prov
# 
# Author: kossi
###############################################################################




if(!require(shiny)){
	install.packages("shiny", dependencies = TRUE)
	require(shiny)
}

shinyUI(fluidPage(theme = "bootstrap.css",
				pageWithSidebar(
						
						# Application title
						headerPanel("Tarification Dataviz"),
						
    	sidebarPanel(
				
	################# Conditionnal Panel pour la description des bases de donnees utilisees ############			      
						conditionalPanel(
										condition = "input.theTabs == 'firstTab' ",
								h3("Bases de tarification"),
								
								helpText("Remarquer la différence entre ces deux bases: la base de sinistres (resp la base de coûts) contient la variable nombre 
										 correspondant au nombre de sinistres (resp la variable cout correspondant 
                                         au coût d'un sinistre)"),
								
								
								br(), 
								br(),
								img(src = "Forsides.png", height = 300, width = 300)
						
						     ),
							 
	################# Conditionnal Panel l'anlyse de la base sinistre ############
							 conditionalPanel(
									 condition = "input.theTabs == 'secondTab' ",
									 
									 h3('Variables de la base sinistre'), 
									 
									 helpText("Noter qu'on ne tarifie plus en fonction du sexe mais je laisse cette variable à des fins exploratoires. De plus
                                              le nombre de sinistres est ramené à l'exposition"),
									 
									 selectInput("variable_sinistre","Selectionner une variable:", choices = c("ageconducteur", "agevehicule", 
												 "agepermis", "voiture", "sexeconducteur", "situationfamiliale", "habitation", "proprietaire",
												 "payment", "poids", "marque"), selected = "ageconducteur"),
								     # j'aurais pu utiliser: colnames(BASEN), pour obtenir les variables
									 # checkboxInput("logScale","Log scale",value = FALSE)  
										
									 br(),
									 br(),
									 img(src = "Forsides.png", height = 300, width = 300)
							 ),
							 
							 ################# Conditionnal Panel l'anlyse de la base cout ############
							 conditionalPanel(
									 condition = "input.theTabs == 'thirdTab' ",
									 
									 h3('Variables de la base coût'), 
									 
									 helpText("Remarque: Une échelle logarithmique (sur le coût) a été utilisée pour la réalisation des boxplot. 
											  Noter qu'on ne tarifie plus en fonction du sexe mais je laisse cette variable à des fins exploratoires"),
							  
									 selectInput("variable_cout","Selectionner une variable:", choices = c("ageconducteur", "agevehicule", 
												"sexeconducteur", "situationfamiliale", "habitation", "proprietaire",
												"payment"), selected = "sexeconducteur"),
									 # j'aurais pu utiliser: colnames(BASEN), pour obtenir les variables
									 # checkboxInput("logScale","Echelle logarithmique",value = FALSE),  
									 
									 br(),
									 br(),
									 img(src = "Forsides.png", height = 300, width = 300)
							 ),
							 
							 ################# Conditionnal Panel Modeles ############
							 conditionalPanel(
									 condition = " input.theTabs == 'fourthTab' ",
									 
									 h3('Modèles'), 
									 
									 # helpText("Remarque: Une échelle logarithmique (sur le coût) a été utilisée pour la réalisation des boxplot. 
									 #		    Noter qu'on ne tarifie plus en fonction du sexe mais je laisse cette variable à des fins exploratoires"),
									 
									 selectInput("Model", "Selectionner un modèle:", choices = c("Modele_frequence", "Modele_cout"), selected = "Modele_frequence"),
									 # j'aurais pu utiliser: colnames(BASEN), pour obtenir les variables
									 # checkboxInput("logScale","Echelle logarithmique",value = FALSE),  
									 
									 br(),
									 br(),
									 img(src = "Forsides.png", height = 300, width = 300)
							 ),
							 ################## Modele d'arbre ##################
							 conditionalPanel(
									 condition = "input.theTabs == 'fivethTab' ",
									 h3('Arbre de décision'), 
									
									 
									 br(),
									 br(),
									 img(src = "Forsides.png", height = 300, width = 300)
							 
							 )
					),
						
	     mainPanel(
						
						tabsetPanel(
						
						## tabpanel pour visualiser les tableaux de donnees
						tabPanel("Données", h2("Base de sinistres"), dataTableOutput("baseSinistre"), 
								h2("Base de coûts"), dataTableOutput("baseCout"), value = "firstTab"),
						## tabpanel pour analyse des variables de la base sinistre
						tabPanel("Influence variables / fréquence de sinistres", plotOutput("influenceSinistre", height = 700, width = 950), value = "secondTab"),
						## tabpanel pour analyse des variables de la base sinistre
						tabPanel("Influence variables / Coût sinistres", plotOutput("influenceCout", height = 700, width = 950), value = "thirdTab"),
						tabPanel("Modèles (Fréquence / Coût sinistres)", verbatimTextOutput("Modeles"), value = "fourthTab"),
						tabPanel("Bonus: Modèle d'arbre pour fréquence de sinistres", plotOutput("Arbre", height = 900, width = 950), value = "fivethTab"),
								id = "theTabs")
				  )
			)
		)
   )
