
# Packages ----------------------------------------------------------------

wants <- c("readr","dplyr","lubridate","ggplot2","JGR","Rcmdr","rattle")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
for(i in wants){ library(i,character.only = TRUE)}


# Data --------------------------------------------------------------------


PG_2017_CLAIMS_YEAR0 <- read_csv("Y:/Forsides France/06_Solo/Mohammed ETTABIB/Projets/Pricing-Game-2017/data/PG_2017_CLAIMS_YEAR0.csv")
