# Project: Olivier_tarif_prov
# 
# Author: kossi
###############################################################################


###
###tarification et calcul de primes pures
###
BASEN=read.table("http://freakonometrics.free.fr/baseN.txt",header=TRUE,sep=";") #Base des sinistres car elle contient la
#variable nombre

BASEY=read.table("http://freakonometrics.free.fr/baseY.txt",header=TRUE,sep=";") #Base des couts car elle contient la 
#variable cout


#Parmi les variables, la description (sommaire) est la suivante,
#1-ageconducteur: age du conducteur principal du vehicule
#2-agepermis: anciennete du permis de conduire du conducteur principal du vehicule
#3-sexeconducteur: sexe du conducteur principal (H ou F)
#4-situationfamiliale: situation familiale du conducteur principal ("Celiba", "Marie" ou "Veuf/Div")
#5-habitation: zone d'habitation du conducteur principal ("peri-urbain", "rural" ou "urbain" )
#6-zone: zone d'habitation (allant de 1 a 8)
#7-agevehicule: age du vehicule
#8-proprietaire: si le conducteur principal possede un contrat Habitation, son statut ("locataire" ou "proprietaire")  Sinon "sans mrp"
#9-payment:type de fractionnement de la prime d'assurance automobile ("Annuel", "Mensuel" ou "Semestriel")
#10-marque: marque du vehicule
#11-poids: classe de poids du vehicule
#12-usage: utilisation du vehicule principal ("PROMENADE" ou "TOUS_DEPLACEMENTS")
#13-exposition: exposition, en annees (exposition, i.e. la duree de la periode de couverture: une personne restant 6 mois au lieu d'un an devrait avoir deux fois moins de sinistres, en moyenne, qu'une personne ayant des caracteristiques identiques.)
#14-nombre: nombre d'accident responsabilite civile du conducteur principal, pendant l'annee passee
#15-cout: cout du sinistre
#16-voiture: type de vehicule

