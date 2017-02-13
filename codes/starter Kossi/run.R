# Project: Olivier_tarif_prov
# 
# Author: kossi
###############################################################################


## Chemin où se trouve le fichier décompressé
setwd("Y:/Forsides France/06_Solo/Yohann LE FAOU/mission_forsides/pricing_game_2017/codes")

if(!require(shiny)){
	install.packages("shiny", dependencies = TRUE)
	require(shiny)
}

runApp("starter Kossi")

