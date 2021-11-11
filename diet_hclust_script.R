# 15 March 2021 - November 2021
# S.A. Leggett, University of Cambridge, University of Oxford
# Script for 'Tackling Early Medieval Dietary Transitions Using a Hierarchical and Multi-isotope Approach'

#data from: https://doi.org/10.1002/ecy.3349

#loading packages - some just in case 
library(readr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(ggsci)
library(ggExtra)
library(ggpmisc)
library(ggpubr)
library(forcats)
library(viridis)
library(ggridges)
library(dplyr)
library(magrittr)
library(ggdendro)
library(ape)
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R") # load code of A2R function
library(factoextra)
library(cluster)
library(NbClust)
library(tidyr)
library(scales)
library(ggthemes)
library(ztable)
library(BEST) #make sure you have the latest JAGS and rjags installed, if you're having problems, quit R, reinstall JAGS, open R and reinstall both rjags and BEST
library(cowplot)
library(gridExtra)
library(grid)
library(rlang)

#colour blind friendly palettes
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#read in data
C_N_Database_bone <- read.csv("~/medieval_palaeoeco_human_bone_CN.csv") #read in all europe bone data
View(C_N_Database_bone)
C_N_dentine<-read.csv("~/medieval_palaeoeco_human_dentine_CN.csv") 
View(C_N_dentine)

Oxygen_Sr_Database <- read.csv("~/medieval_palaeoeco_human_apatite.csv")
View(Oxygen_Sr_Database)
summary(Oxygen_Sr_Database)
#remove bone data
OSr_teeth<-subset(Oxygen_Sr_Database, `Bone/Tooth`!="Bone")
head(OSr_teeth)
summary(OSr_teeth)
#Ireland still seems to be skewing the data - to do with Ryan et al. 2018 - REMOVE this data 
OSr_teeth<-subset(OSr_teeth, Reference!='Ryan SE, Reynard LM, Crowley QG, Snoeck C, Tuross N (2018). "Early Medieval reliance on the land and the local: An Integrated multi-isotope study (87Sr/86Sr, δ18O, δ13C, δ15N) of diet and mirgration in Co. Meath, Ireland." Journal of Archaeological Science 98: 59-71.')
summary(OSr_teeth)



#subset for only English material
England_CN_bone<- subset(C_N_Database_bone, Country=="England")
View(England_CN_bone)
summary(England_CN_bone)
England_CN_dentine<-subset(C_N_dentine, Country=="England")
Oxy_England<- subset(OSr_teeth, Country=="England")
summary(Oxy_England)

#defining european regions
#bone
C_N_Database_bone$EuRegion<-C_N_Database_bone$County
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Split-Dalmatia', 'Croatia'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Zadar', 'Croatia'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Vis', 'Croatia'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Vukovar-Srijem', 'Croatia'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Šibenik-Knin', 'Croatia'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Roskilde', 'Skagerrak-Kattegat-Jutland Basin'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Zealand', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Funen', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Ålborg', 'Skagerrak-Kattegat-Jutland Basin'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Saare', 'Baltic'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Turku and Pori', Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Nord-Trøndelag', 'Atlantic & Arctic Norway'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Nordland', 'Atlantic & Arctic Norway'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Troms', 'Atlantic & Arctic Norway'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Rogaland', 'Atlantic & Arctic Norway'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Sogn og Fjordane', 'Atlantic & Arctic Norway'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Vest-Agder', 'Skagerrak-Kattegat-Jutland Basin'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Vestfold', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Hordaland', 'Atlantic & Arctic Norway'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Hedmark', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Trøndelag', 'Atlantic & Arctic Norway'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Telemark', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Akershus', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Oppland', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Sør Trøndelag', 'Atlantic & Arctic Norway'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Uppland', 'Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Uppsala', 'Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Kalmar', 'Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Stockholm', 'Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Gotland', 'Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Öland', 'Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Gävleborg', 'Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Adelsö', 'Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Leningrad Oblast', 'Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Nord Trøndelag', 'Atlantic & Arctic Norway'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Aarhus', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Hjørring', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Skive', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Oslo', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Isle of Man', 'Irish Sea'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Pembrokeshire', 'Irish Sea'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Vale of Glamorgan', 'Irish Sea'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Meath', 'Irish Sea'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Dublin', 'Irish Sea'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Ireland', 'Irish Sea'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Outer Hebrides', 'Scotland and Scottish Isles'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Scotland', 'Scotland and Scottish Isles'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Rousay', 'Scotland and Scottish Isles'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Highlands', 'Scotland and Scottish Isles'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Orkney', 'Scotland and Scottish Isles'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Mainland', 'Scotland and Scottish Isles'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Highlands, Scotland', 'Scotland and Scottish Isles'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Sanday', 'Scotland and Scottish Isles'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Dumfries and Galloway', 'Scotland and Scottish Isles'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Bedfordshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Cambridgeshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Dorset', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Derbyshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'East Sussex', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Sussex', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Kent', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Hampshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Wiltshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Warwickshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Suffolk', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Oxfordshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Lincolnshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Tyne & Wear', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Northumberland', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Yorkshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Rutland', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Nottinghamshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Hertfordshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Buckinghamshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'North Lincolnshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Somerset', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Norfolk', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Gloucestershire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Surrey', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Northamptonshire', 'England'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Kujalleq', 'North Atlantic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Mosfell', 'North Atlantic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Lake Myvatn', 'North Atlantic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Piemont', 'Po Valley'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Friuli-Venezia Giulia', 'Po Valley'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Piedmont', 'Po Valley'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Ibiza', 'Balearic & Tyrrhenian Seas'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Rome', 'Balearic & Tyrrhenian Seas'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Barcelona', 'Balearic & Tyrrhenian Seas'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Valencia', 'Balearic & Tyrrhenian Seas'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Languedoc', 'Balearic & Tyrrhenian Seas'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Normandy', 'Normandy/Neustria'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Flanders', 'Normandy/Neustria'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Wallonia', 'Normandy/Neustria'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Aragon', 'Inland & Western Iberia'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Pontevedra', 'Inland & Western Iberia'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Madrid', 'Inland & Western Iberia'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Setubal', 'Inland & Western Iberia'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Alentejo', 'Inland & Western Iberia'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Lower Saxony', 'Frisia & Saxony'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Friesland', 'Frisia & Saxony'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Saxony-Anhalt', 'Frisia & Saxony'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Rhineland', 'Austrasia & Burgundy'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Grand Est', 'Austrasia & Burgundy'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Rhein-Kreis Neuss, Nordrhine-Westphalia', 'Austrasia & Burgundy'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Gem. Bedburg-Königshoven, Rhein-Erft-Kreis, Northrhine-Westphalia', 'Austrasia & Burgundy'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Baden-Württemberg', 'Austrasia & Burgundy'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Bavaria', 'Austro-Hungary & Bavaria'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Lower Austria', 'Austro-Hungary & Bavaria'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Northern Hungary', 'Austro-Hungary & Bavaria'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Tyrol', 'Austro-Hungary & Bavaria'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Vienna', 'Austro-Hungary & Bavaria'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Crete', 'Greece'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Peloponnese', 'Greece'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Western Macedonia', 'Greece'))

C_N_Database_bone$EuRegion = factor(C_N_Database_bone$EuRegion,
                                    levels=c("North Atlantic", "Atlantic & Arctic Norway","Skagerrak-Kattegat-Jutland Basin","Baltic", "Scotland and Scottish Isles", "Irish Sea", "England", "Frisia & Saxony", "Normandy/Neustria", "Austrasia & Burgundy","Austro-Hungary & Bavaria", "Po Valley", "Croatia", "Balearic & Tyrrhenian Seas","Inland & Western Iberia", "Greece"),ordered=TRUE)

#dentine
C_N_dentine$EuRegion<-C_N_dentine$County
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Bedfordshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Lincolnshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Northumberland', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Nottinghamshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Kent', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Wiltshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Cambridgeshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Rutland', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Hertfordshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Yorkshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Northamptonshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Dorset', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Warwickshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Suffolk', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Lincolnshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Vukovar-Srijem', 'Croatia'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Funen', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Troms', 'Atlantic & Arctic Norway'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Nordland', 'Atlantic & Arctic Norway'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Oslo', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Öland', 'Baltic'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Piedmont', 'Po Valley'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Highlands, Scotland', 'Scotland and Scottish Isles'))

C_N_dentine$EuRegion = factor(C_N_dentine$EuRegion,
                              levels=c("Atlantic & Arctic Norway","Skagerrak-Kattegat-Jutland Basin", "Baltic","Scotland and Scottish Isles", "England", "Po Valley", "Croatia"),ordered=TRUE)





#hierarchical clustering and dendrograms
#dendrograms as an alternative to k means clustering which doesn't require you to select the number of cluster

#prepping the data for hierarchical clustering
EMEUboneclean <- C_N_Database_bone
class(as.data.frame(EMEUboneclean))
head(EMEUboneclean)
EMEUboneclean <- data.frame(EMEUboneclean)
rownames(EMEUboneclean) <- EMEUboneclean[,1]
EMEUboneclean <- EMEUboneclean[,-1]
head(EMEUboneclean)
show(EMEUboneclean)
EMEUboneclean<-EMEUboneclean[c(32,33)]
head(EMEUboneclean)
EMEUboneclean <- scale(EMEUboneclean)
EMEUboneclean <- na.omit(EMEUboneclean)
head(EMEUboneclean)
#hierarchical clustering
ddEMEU_CN_bone<-dist(EMEUboneclean, method = "euclidean") 
hcEMEU_CN_bone<-hclust(ddEMEU_CN_bone, method = "ward.D2")
ggdendrogram(hcEMEU_CN_bone)
ggdendrogram(hcEMEU_CN_bone, rotate = TRUE,)
plot(as.phylo(hcEMEU_CN_bone), type = "fan")
set.seed(123)
fviz_nbclust(EMEUboneclean, hcut, method = "silhouette")
set.seed(123)
gap_stat <- clusGap(EMEUboneclean, FUN = hcut, nstart = 25, K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
print(gap_stat, method = "globalmax")
fviz_gap_stat(gap_stat)
set.seed(123)
fviz_nbclust(EMEUboneclean, hcut, method = "wss")
NbClust(data = EMEUboneclean, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, method = "ward.D2")
A2Rplot(hcEMEU_CN_bone, k = 2, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)
A2Rplot(hcEMEU_CN_bone, k = 3, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)
A2Rplot(hcEMEU_CN_bone, k = 6, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)

#import! manually entered scatterplot groups from full size print out of dendrogram above 
CNB_hclusters <- read_excel("~/CNB_hclusters.xlsx")
#View(CNB_hclusters)
C_N_Database_bone$`CNB_hcluster`<-CNB_hclusters$CNB_EMEU_hclust_group

#with 3 clusters from Nbclust, need to combine sub-clusters
C_N_Database_bone$`CNB_hcluster3`<-C_N_Database_bone$`CNB_hcluster`
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster3 = replace(CNB_hcluster3, CNB_hcluster3 == '1.1.1', '1'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster3 = replace(CNB_hcluster3, CNB_hcluster3 == '1.1.2', '1'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster3 = replace(CNB_hcluster3, CNB_hcluster3 == '1.2.1', '1'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster3 = replace(CNB_hcluster3, CNB_hcluster3 == '1.2.2', '1'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster3 = replace(CNB_hcluster3, CNB_hcluster3 == '2.1.1', '2.1'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster3 = replace(CNB_hcluster3, CNB_hcluster3 == '2.1.2', '2.1'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster3 = replace(CNB_hcluster3, CNB_hcluster3 == '2.2.1', '2.2'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster3 = replace(CNB_hcluster3, CNB_hcluster3 == '2.2.2', '2.2'))


#scatterplot for EMEU C&N dendrogram
tiff("Dropbox/Publications/EJA/Leggett_Fig3.tiff", units="in", width=18.04, height=10.76, res=300)
ggplot(C_N_Database_bone,aes(d13C, d15N, color=`CNB_hcluster3`))+ #3groups
  theme_bw()+
  geom_point(size=3,shape=16)+
  scale_colour_manual(values=cbbPalette, name="Cluster Number")+
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1))+scale_y_continuous(limits=c(0,20),breaks=seq(0,20,1))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.title =element_text(size=22), legend.text = element_text(size=18))
dev.off()

#stacked bar plots 
#stacked barchart with country and cluster 
#counts for country categories

CNBcleanplus<- C_N_Database_bone
class(as.data.frame(CNBcleanplus))
head(CNBcleanplus)
CNBcleanplus <- data.frame(CNBcleanplus)
rownames(CNBcleanplus) <-CNBcleanplus[,1]
CNBcleanplus <- CNBcleanplus[,-1]
head(CNBcleanplus)
View(CNBcleanplus)
CNBcleanplus<-CNBcleanplus[c(2,3,37,40)] #country, country code, region and cluster group
head(CNBcleanplus)
CNBcleanplus <- na.omit(CNBcleanplus)
head(CNBcleanplus)

CNBdfr <- CNBcleanplus %>%             
  mutate(Country = as.factor(Country)     # categorical values to factor
         , `CNB_hcluster2` = as.ordered(`CNB_hcluster3`))# character to ordered factor (like a grade)


#fct_reorder(carbcleanplus$Country, carbcleanplus$`Country Code`)

#dfr <- na.omit(dfr)
CNBdfr_prop <- CNBdfr %>% 
  count(Country, `CNB_hcluster3`) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(CNBdfr_prop)           

CNBdfr_prop2 <- CNBdfr %>% 
  count(`CNB_hcluster3`, Country) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(CNBdfr_prop2)  

ggplot(CNBdfr_prop, aes(CNBdfr_prop$`CNB_hcluster3`,CNBdfr_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = Country, weight=`CNB_hcluster3`, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_viridis(discrete = TRUE, name = "Country")+
  xlab(expression(paste("Cluster")))+ylab(expression(paste("Proportion")))+
  theme_bw()

ggplot(CNBdfr_prop, aes(Country,CNBdfr_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = CNBdfr_prop$`CNB_hcluster3`, weight=Country, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_manual(values=cbbPalette, name="Cluster")+
  xlab(expression(paste("Country")))+ylab(expression(paste("Proportion")))+
  theme_bw()


CNBdfr2 <- CNBcleanplus %>%             
  mutate(EuRegion = as.factor(EuRegion), `CNB_hcluster3` = as.ordered(`CNB_hcluster3`))

CNBdfr2_prop <- CNBdfr2 %>% 
  count(EuRegion, `CNB_hcluster3`) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(CNBdfr2_prop)   

ggplot(CNBdfr2_prop, aes(CNBdfr2_prop$`CNB_hcluster3`,CNBdfr2_prop$prop,)) +
  theme_bw()+
  geom_bar(colour = "black", aes(fill = EuRegion, weight=`CNB_hcluster3`, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_viridis(discrete = TRUE, name = "Region")+
  xlab(expression(paste("Cluster")))+ylab(expression(paste("Proportion")))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=22), legend.text =element_text(size=18),legend.title = element_text(size = 22))

ggplot(CNBdfr2_prop, aes(EuRegion,CNBdfr2_prop$prop,)) +
  theme_bw()+
  geom_bar(colour = "black", aes(fill = CNBdfr2_prop$`CNB_hcluster3`, weight=EuRegion, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_manual(values=cbbPalette, name="Cluster")+
  xlab(expression(paste("Region")))+ylab(expression(paste("Proportion")))+
  theme(axis.text=element_text(size=16),axis.text.x = element_text(angle = 45, hjust = 1),axis.title=element_text(size=22), legend.text =element_text(size=18),legend.title = element_text(size = 22))


#dentine hierarchical clustering
#prepping the data for hierarchical clustering
EMEUdentineclean <- C_N_dentine
class(as.data.frame(EMEUdentineclean))
head(EMEUdentineclean)
EMEUdentineclean <- data.frame(EMEUdentineclean)
rownames(EMEUdentineclean) <- EMEUdentineclean[,1]
EMEUdentineclean <- EMEUdentineclean[,-1]
head(EMEUdentineclean)
View(EMEUdentineclean)
EMEUdentineclean<-EMEUdentineclean[c(33,34)] #choosing just d13C and d15N
head(EMEUdentineclean)
EMEUdentineclean <- scale(EMEUdentineclean)
EMEUdentineclean <- na.omit(EMEUdentineclean)
head(EMEUdentineclean)
#hierarchical clustering
ddEMEU_CN_dentine<-dist(EMEUdentineclean, method = "euclidean") 
hcEMEU_CN_dentine<-hclust(ddEMEU_CN_dentine, method = "ward.D2")
ggdendrogram(hcEMEU_CN_dentine)
ggdendrogram(hcEMEU_CN_dentine, rotate = TRUE,)
plot(as.phylo(hcEMEU_CN_dentine), type = "fan")
set.seed(123)
fviz_nbclust(EMEUdentineclean, hcut, method = "silhouette")
set.seed(123)
gap_stat <- clusGap(EMEUdentineclean, FUN = hcut, nstart = 25, K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
print(gap_stat, method = "globalmax")
fviz_gap_stat(gap_stat)
set.seed(123)
fviz_nbclust(EMEUdentineclean, hcut, method = "wss")
NbClust(data = EMEUdentineclean, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, method = "ward.D2")
A2Rplot(hcEMEU_CN_dentine, k = 7, boxes = FALSE, col.up = "gray50", col.down = c("black", "#E69F00","#0072B2","#009E73","#CC79A7","#56B4E9","#D55E00"))
A2Rplot(hcEMEU_CN_dentine, k = 8, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)
A2Rplot(hcEMEU_CN_dentine, k = 6, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)
A2Rplot(hcEMEU_CN_dentine, k = 5, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)
A2Rplot(hcEMEU_CN_dentine, k = 3, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)

#import manually entered scatterplot groups from above as per with bone 
CND_hclusters <- read_excel("~/CND_hclusters.xlsx")
#View(CND_hclusters)
C_N_dentine$`CND_hcluster`<-CND_hclusters$CND_hcluster_group5
C_N_dentine <- C_N_dentine %>% mutate(CND_hcluster = replace(CND_hcluster, CND_hcluster == '1.1000000000000001', '1.1'))
C_N_dentine <- C_N_dentine %>% mutate(CND_hcluster = replace(CND_hcluster, CND_hcluster == '1.1', '1'))
C_N_dentine <- C_N_dentine %>% mutate(CND_hcluster = replace(CND_hcluster, CND_hcluster == '1.2', '1'))
C_N_dentine <- C_N_dentine %>% mutate(CND_hcluster = replace(CND_hcluster, CND_hcluster == '2.2.1', '2.2'))
C_N_dentine <- C_N_dentine %>% mutate(CND_hcluster = replace(CND_hcluster, CND_hcluster == '2.2.2', '2.2'))

#scatterplot for EMEU C&N dendrogram dentine
tiff("Dropbox/Publications/EJA/Leggett_Fig7.tiff", units="in", width=18.04, height=10.76, res=300)
ggplot(C_N_dentine,aes(d13C, d15N, color=`CND_hcluster`))+ #3 clusters
  theme_bw()+
  geom_point(size=3,shape=16)+
  scale_colour_manual(values=cbbPalette, name="Cluster Number")+
  ylab(expression(paste(delta^{15},N["dentine (AIR)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1))+scale_y_continuous(limits=c(0,20),breaks=seq(0,20,1))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.title =element_text(size=22), legend.text = element_text(size=18))
dev.off()

ggplot(C_N_dentine,aes(d13C, d15N, color=`CND_hcluster`))+ #3 clusters
  theme_bw()+
  geom_point(size=3,shape=16)+
  scale_colour_manual(values=cbbPalette, name="Cluster Number")+
  ylab(expression(paste(delta^{15},N["dentine (AIR)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1))+scale_y_continuous(limits=c(0,20),breaks=seq(0,20,1))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.title =element_text(size=22), legend.text = element_text(size=18))
ggsave("Dropbox/Publications/EJA/Leggett_Fig7_v2.tiff", width=18.04, height=10.76, dpi = 320) #dpi still weirdly 72??? whyyyyy???

bitmap("Dropbox/Publications/EJA/Leggett_Fig7_v3.tiff", height = 10.76, width = 18.04, 
       units = 'cm', type="tiff", res=300)
ggplot(C_N_dentine,aes(d13C, d15N, color=`CND_hcluster`))+ #3 clusters
  theme_bw()+
  geom_point(size=3,shape=16)+
  scale_colour_manual(values=cbbPalette, name="Cluster Number")+
  ylab(expression(paste(delta^{15},N["dentine (AIR)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1))+scale_y_continuous(limits=c(0,20),breaks=seq(0,20,1))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.title =element_text(size=22), legend.text = element_text(size=18))
dev.off()

postscript("Dropbox/Publications/EJA/Leggett_Fig7.eps", height = 10.76, width = 18.04, 
           horizontal = FALSE, onefile = FALSE, paper = "special", 
           colormodel = "cmyk")
ggplot(C_N_dentine,aes(d13C, d15N, color=`CND_hcluster`))+ #3 clusters
  theme_bw()+
  geom_point(size=3,shape=16)+
  scale_colour_manual(values=cbbPalette, name="Cluster Number")+
  ylab(expression(paste(delta^{15},N["dentine (AIR)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1))+scale_y_continuous(limits=c(0,20),breaks=seq(0,20,1))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.title =element_text(size=22), legend.text = element_text(size=18))
dev.off() #works but doesn't include the percent per mille sign... 


#stacked bar plots 
#stacked barchart with country and cluster 
#counts for country categories
CNDcleanplus<- C_N_dentine
class(as.data.frame(CNDcleanplus))
head(CNDcleanplus)
CNDcleanplus <- data.frame(CNDcleanplus)
rownames(CNDcleanplus) <-CNDcleanplus[,1]
CNDcleanplus <- CNDcleanplus[,-1]
head(CNDcleanplus)
View(CNDcleanplus)
CNDcleanplus<-CNDcleanplus[c(2,3,36,37)] #country, country code, region and cluster group
head(CNDcleanplus)
#CNDcleanplus <- na.omit(CNDcleanplus)
#head(CNDcleanplus)

CNDdfr <- CNDcleanplus %>%             
  mutate(Country = as.factor(Country)     # categorical values to factor
         , `CND_hcluster` = as.ordered(`CND_hcluster`))# character to ordered factor (like a grade)


#fct_reorder(carbcleanplus$Country, carbcleanplus$`Country Code`)

#dfr <- na.omit(dfr)
CNDdfr_prop <- CNDdfr %>% 
  count(Country, `CND_hcluster`) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(CNDdfr_prop)           

CNDdfr_prop2 <- CNDdfr %>% 
  count(`CND_hcluster`, Country) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(CNDdfr_prop2)  

ggplot(CNDdfr_prop, aes(CNDdfr_prop$`CND_hcluster`,CNDdfr_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = Country, weight=`CND_hcluster`, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_viridis(discrete = TRUE, name = "Country")+
  xlab(expression(paste("Cluster")))+ylab(expression(paste("Proportion")))+
  theme_bw()

ggplot(CNDdfr_prop, aes(Country,CNDdfr_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = CNDdfr_prop$`CND_hcluster`, weight=Country, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_manual(values=cbbPalette, name="Cluster")+
  xlab(expression(paste("Country")))+ylab(expression(paste("Proportion")))+
  theme_bw()


CNDdfr2 <- CNDcleanplus %>%             
  mutate(EuRegion = as.factor(EuRegion), `CND_hcluster` = as.ordered(`CND_hcluster`))

CNDdfr2_prop <- CNDdfr2 %>% 
  count(EuRegion, `CND_hcluster`) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(CNDdfr2_prop)   

ggplot(CNDdfr2_prop, aes(CNDdfr2_prop$`CND_hcluster`,CNDdfr2_prop$prop,)) +
  theme_bw()+
  geom_bar(colour = "black", aes(fill = EuRegion, weight=`CND_hcluster`, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_viridis(discrete = TRUE, name = "Region")+
  xlab(expression(paste("Cluster")))+ylab(expression(paste("Proportion")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.text =element_text(size=18),legend.title = element_text(size = 22))


ggplot(CNDdfr2_prop, aes(EuRegion,CNDdfr2_prop$prop,)) +
  theme_bw()+
  geom_bar(colour = "black", aes(fill = CNDdfr2_prop$`CND_hcluster`, weight=EuRegion, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_manual(values=cbbPalette, name="Cluster")+
  xlab(expression(paste("Region")))+ylab(expression(paste("Proportion")))+
  theme(axis.text=element_text(size=16),axis.text.x = element_text(angle = 45, hjust = 1),axis.title=element_text(size=22), legend.text =element_text(size=18),legend.title = element_text(size = 22))

#England diet through time
#simplifying the date categories
England_CN_bone$SimpleDate<-England_CN_bone$`Date Category`
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'A-C', 'A-D'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'A/B', 'A-D'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'A-D', 'A-E'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'B-D', 'B-G'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'B-E', 'B-G'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'B-F', 'B-G'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'B/C', 'B-G'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'C-F', 'C/D'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'C-E', 'C-H'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'C-G', 'C-H'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'F-H', 'F-I'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'E-G', 'E-H'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'D-G', 'D-H'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'D/E', 'D-F'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'E', 'E/F'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'F', 'E/F'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'E-H', 'E/F'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'F-I', 'E-I'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'G', 'G-I'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'G/H', 'G-I'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'I', 'G-I'))

England_CN_bone$`SimpleDate` = factor(England_CN_bone$`SimpleDate`,
                                      levels=c("A", "A-E","B","B-G","C","C/D","C-H","D","D-F","D-H","E/F","E-I","F/G","G-I"),ordered=TRUE)


#combine date categories further similar to carbonate with pre-migration period, migration period to "viking" and viking to Norman
#now for date categories super simple - 200BC-450 AD, ~350-790AD, ~790AD-1066+
England_CN_bone$SimpleDate<-as.character(England_CN_bone$SimpleDate)
England_CN_bone$PeriodBroad<-England_CN_bone$`SimpleDate`
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A', '200BC-450AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A-E', 'c.350AD-790AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B', 'c.350AD-790AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-G', 'c.350AD-790AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C', 'c.350AD-790AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C/D', 'c.350AD-790AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C-H', 'c.350AD-790AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D', 'c.350AD-790AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-F', 'c.350AD-790AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E/F', 'c.350AD-790AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-H', 'c.790AD-1066AD+'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E-I', 'c.790AD-1066AD+'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F/G', 'c.790AD-1066AD+'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'G-I', 'c.790AD-1066AD+'))

England_CN_bone$`PeriodBroad` = factor(England_CN_bone$`PeriodBroad`,
                                       levels=c("200BC-450AD", "c.350AD-790AD","c.790AD-1066AD+"),ordered=TRUE)


Eng_Bone_d13C_broad<-ggplot(data=England_CN_bone, aes(x=England_CN_bone$PeriodBroad, y=England_CN_bone$d13C, fill=PeriodBroad))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  #scale_color_manual(values=qualcolourPalette)+
  ylab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(-25,-15),breaks=seq(-25,-15,2))+
  theme(legend.position = "none", axis.text = element_text(size = 18), axis.title = element_text(size=20))
Eng_Bone_d13C_broad

Eng_Bone_d15N_broad<-ggplot(data=England_CN_bone, aes(x=England_CN_bone$PeriodBroad, y=England_CN_bone$d15N, fill=PeriodBroad))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  #scale_color_manual(values=qualcolourPalette)+
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(0,18),breaks=seq(0,18,2))+
  theme(legend.position = "none", axis.text = element_text(size = 18), axis.title = element_text(size=20))
Eng_Bone_d15N_broad

England_Bone_Broad_Bag<- ggplot(England_CN_bone, aes(d13C, d15N, colour = PeriodBroad, fill = PeriodBroad)) +
  theme_bw()+
  geom_bag()+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_colour_viridis(discrete = TRUE, name="Broad Period")+
  scale_fill_viridis(discrete = TRUE,name="Broad Period")+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=24), legend.position = "bottom")
England_Bone_Broad_Bag

#dentine
England_CN_dentine$PeriodBroad<-England_CN_dentine$`Date Category`
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A-C', '200BC-450AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A-D', '200BC-450AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A/B', '200BC-450AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-D', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-E', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-F', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-G', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B/C', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C-F', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C-G', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C/D', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D/E', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-F', 'c.790AD-1066AD+'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-G', 'c.790AD-1066AD+'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-H', 'c.790AD-1066AD+'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E/F', 'c.790AD-1066AD+'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F', 'c.790AD-1066AD+'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F-H', 'c.790AD-1066AD+'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F/G', 'c.790AD-1066AD+'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'G', 'c.790AD-1066AD+'))


England_CN_dentine$`PeriodBroad` = factor(England_CN_dentine$`PeriodBroad`,
                                          levels=c("200BC-450AD", "c.450AD-790AD","c.790AD-1066AD+"),ordered=TRUE)
#enamel carbonate
Oxy_England$SimpleDate<-Oxy_England$`Date Category`
summary(Oxy_England)
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'A-C', 'A-D'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'A/B', 'A-D'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'B-D', 'B-G'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'B-E', 'B-G'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'B-F', 'B-G'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'B/C', 'B-G'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'C-F', 'C/D'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'F-H', 'F-I'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'E-G', 'E-H'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'D-G', 'D-H'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'E', 'E/F'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'F', 'E/F'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'E-H', 'E/F'))

Oxy_England$`SimpleDate` = factor(Oxy_England$`SimpleDate`,
                                  levels=c("A", "A-D","B","B-G", "C", "C/D", "D", "D/E", "D-F", "D-H", "E/F", "F/G","F-I"),ordered=TRUE)
#now for date categories super simple - 200BC-450 AD, ~200-790AD, ~790AD-1066+
#A->200BC-450 AD
#A/B, A-C, A-D, B, B-D, B-E, B/C, C, C/D, D, D/E, E->~200AD-790 AD
Oxy_England$PeriodBroad<-Oxy_England$`Date Category`
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A', '200BC-450AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A/B', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A-C', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A-D', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-D', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-E', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B/C', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C/D', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D/E', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C-F', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-F', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-G', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F', 'c.790AD-1066AD+'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F-H', 'c.790AD-1066AD+'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E/F', 'c.790AD-1066AD+'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F-I', 'c.790AD-1066AD+'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F/G', 'c.790AD-1066AD+'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E-G', 'c.790AD-1066AD+'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E-H', 'c.790AD-1066AD+'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-G', 'c.790AD-1066AD+'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-H', 'c.790AD-1066AD+'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-F', 'c.790AD-1066AD+'))



Oxy_England$`PeriodBroad` = factor(Oxy_England$`PeriodBroad`,
                                   levels=c("200BC-450AD", "c.350AD-790AD","c.790AD-1066AD+"),ordered=TRUE)

#carbonate and collagen offsets
## big delta offset scatters  
matched_bone_dent_enamel_England<-read.csv("~/matched_bone_dent_enamel_england.csv")

matched_bone_dent_enamel_England$PeriodBroad<-matched_bone_dent_enamel_England$`Date Category`
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A-C', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A/B', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-D', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-E', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-F', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B/C', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C/D', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D/E', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C-F', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-F', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E/F', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F', 'c.790AD-1066AD+'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C-G', 'c.790AD-1066AD+'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-G', 'c.790AD-1066AD+'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F/G', 'c.790AD-1066AD+'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'G', 'c.790AD-1066AD+'))



matched_bone_dent_enamel_England$`PeriodBroad` = factor(matched_bone_dent_enamel_England$`PeriodBroad`,
                                                        levels=c("c.350AD-790AD","c.790AD-1066AD+"),ordered=TRUE)

enamel_dent_diff_d13carb_bag_period<-ggplot(matched_bone_dent_enamel_England, aes(D13C_tooth_enamel_dent, enamel_d13C, colour=`PeriodBroad`, fill=`PeriodBroad`)) +
  theme_bw()+
  geom_bag()+
  scale_fill_manual(values=c("#21908CFF","#FDE725FF"), name="Broad Period")+
  scale_colour_manual(values=c("#21908CFF","#FDE725FF"), name="Broad Period")+
  xlab(expression(paste(Delta^{13},C["carbonate-dentine"], " (\u2030)")))+ylab(expression(paste(delta^{13},C["carb"]," (\u2030)")))+ 
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.position = "none")

ggarrange(Eng_Bone_d13C_broad+rremove("x.text"), England_Bone_Broad_Bag, Eng_Bone_d15N_broad+ rremove("x.text"), enamel_dent_diff_d13carb_bag_period,  labels = c("A", "C", "B", "D"), font.label=list(size=22), ncol = 2, nrow = 2, common.legend = TRUE)

legend_bonebag<-get_legend(England_Bone_Broad_Bag)
grid.arrange(arrangeGrob(Eng_Bone_d13C_broad+rremove("x.text"), 
                         England_Bone_Broad_Bag+ theme(legend.position="none"), 
                         Eng_Bone_d15N_broad+ rremove("x.text"),
                         enamel_dent_diff_d13carb_bag_period, ncol = 2, nrow = 2), 
             legend_bonebag, 
             nrow=2,heights=c(10, 1))
prow <- plot_grid(Eng_Bone_d13C_broad+rremove("x.text"), England_Bone_Broad_Bag+ theme(legend.position="none"), Eng_Bone_d15N_broad+ rremove("x.text"),enamel_dent_diff_d13carb_bag_period, align = 'vh', labels = c("A", "C", "B", "D"), hjust = -0.5,nrow = 2)
prow
tiff("Fig10.tiff", units="in", width=12.6, height=9, res=300)
plot_grid(prow, legend_bonebag, ncol = 1, rel_heights = c(1, .1))
dev.off()

