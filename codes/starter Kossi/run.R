# Project: Olivier_tarif_prov
# 
# Author: kossi
###############################################################################



setwd("C:/Users/BIGDATA/Documents/BIG DATA 2/Projets_Rstudio/PERSO/Tarif_Shiny") ## Chemin où se trouve le fichier décompressé

if(!require(shiny)){
	install.packages("shiny", dependencies = TRUE)
	require(shiny)
}

runApp("Tarif_Shiny")

