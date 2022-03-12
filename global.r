library(shiny)
library(colourpicker)
library(shinydashboard)
library(rAmCharts)
library(DT)
library(tidyverse)
library(leaflet)
library(viridis)
library(corrplot)
library(stargazer)
library(plotly)
library(FactoMineR)
library(shinythemes)
library(lmtest)
library(hrbrthemes)
library(car)


# Imporation jeux de donnees
# Nettoyage + fusion 

# Ouverture premiere BDD relative à l'année 2021
donnees_2021 <- read.csv("world-happiness-report-2021.csv")
donnees_2021 <- donnees_2021[,c(1:3,7:12)]
colnames(donnees_2021) <- c("Pays","Region","Indice_du_bonheur","log_PIB_par_hab","Soutien_social","Esperance_de_vie_en_bonne_sante","Liberte_de_faire_des_choix","Generosite","Perception_de_corruption")
donnees_2021$Annees <- 2021

# Ouverture de la deuxième BDD (années antérieures à 2021)
donnees_annees <- read.csv("world-happiness-report.csv")
donnees_annees <- donnees_annees[,1:9]
colnames(donnees_annees) <- c("Pays","Annees","Indice_du_bonheur","log_PIB_par_hab","Soutien_social","Esperance_de_vie_en_bonne_sante","Liberte_de_faire_des_choix","Generosite","Perception_de_corruption")

# Ajout de la colonne Region à donnees_annees
tmp <- merge(data.frame(Pays = donnees_2021$Pays, Region = donnees_2021$Region), 
             data.frame(Pays = donnees_annees$Pays),
             all.y = TRUE)
donnees_annees$Region <- tmp$Region
donnees <- rbind(donnees_2021, donnees_annees)

donnees$Region[donnees$Pays == "Angola"] <- "Sub-Saharan Africa"                
donnees$Region[donnees$Pays == "Belize"] <- "Latin America and Caribbean"
donnees$Region[donnees$Pays == "Bhutan"] <- "South Asia"
donnees$Region[donnees$Pays == "Central African Republic"] <- "Sub-Saharan Africa"                
donnees$Region[donnees$Pays == "Congo (Kinshasa)"] <- "Sub-Saharan Africa" 
donnees$Region[donnees$Pays == "Cuba"] <- "Latin America and Caribbean"
donnees$Region[donnees$Pays == "Djibouti"] <- "Sub-Saharan Africa"                
donnees$Region[donnees$Pays == "Guyana"] <- "Latin America and Caribbean"
donnees$Region[donnees$Pays == "Oman"] <- "Middle East and North Africa"
donnees$Region[donnees$Pays == "Qatar"] <- "Middle East and North Africa"                
donnees$Region[donnees$Pays == "Somalia"] <- "Middle East and North Africa" # ou Afrique
donnees$Region[donnees$Pays == "Somaliland region"] <- "Middle East and North Africa" # ou Afrique
donnees$Region[donnees$Pays == "South Sudan"] <- "Sub-Saharan Africa"                
donnees$Region[donnees$Pays == "Sudan"] <- "Sub-Saharan Africa"
donnees$Region[donnees$Pays == "Suriname"] <- "Latin America and Caribbean"
donnees$Region[donnees$Pays == "Syria"] <- "Middle East and North Africa"                
donnees$Region[donnees$Pays == "Trinidad and Tobago"] <- "Latin America and Caribbean"


# Données carte
# On télécharge le nom des pays associé à leur code ISO (3 lettres)
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
# On supprime la colonne GDP qui était un exemple
df <- df[,-2]
