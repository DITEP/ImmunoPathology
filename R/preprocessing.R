rm(list=ls())
library(dplyr)
library(dummies)

########### PROJET INNO : 2eme JET DE DONNÉES ##########################

# Lecture des fichiers
IHC <- read.csv(file="C:/Users/nsanchezescobar/Documents/Projet inno/IHC_total.csv", header=TRUE, sep=";", na.strings="NA", dec=",")
ClinicRNA <- read.csv(file="C:/Users/nsanchezescobar/Documents/Projet inno/ClinicRNA_total_OS.csv", header=TRUE, sep=";", na.strings="NA", dec=",")
MOSCATO <- read.csv(file="C:/Users/nsanchezescobar/Documents/Projet inno/MOSCATO_IHC_PDL1_b.csv", header=TRUE, sep=";", na.strings="NA", dec=",")

#Fusion en 1 seul csv
dIHC <- data.frame(IHC$NIP, IHC$MP_ANTICORPS, IHC$MP_AC_DENSMOY, IHC$MP_AC_DENSMIN, IHC$MP_AC_DENSMAX, IHC$MP_AC_DIST_IT, IHC$MP_AC_DIST_IST, IHC$MP_AC_DIST_SD)
names(dIHC)[names(dIHC) == "IHC.NIP"] <- "NIP"
names(dIHC)[names(dIHC) == "IHC.MP_ANTICORPS"] <- "MP_ANTICORPS"
names(dIHC)[names(dIHC) == "IHC.MP_AC_DENSMOY"] <- "MP_AC_DENSMOY"
names(dIHC)[names(dIHC) == "IHC.MP_AC_DENSMIN"] <- "MP_AC_DENSMIN"
names(dIHC)[names(dIHC) == "IHC.MP_AC_DENSMAX"] <- "MP_AC_DENSMAX"
names(dIHC)[names(dIHC) == "IHC.MP_AC_DIST_IT"] <- "MP_AC_DIST_IT"
names(dIHC)[names(dIHC) == "IHC.MP_AC_DIST_IST"] <- "MP_AC_DIST_IST"
names(dIHC)[names(dIHC) == "IHC.MP_AC_DIST_SD"] <- "MP_AC_DIST_SD"

test.CD8 <- filter(dIHC, dIHC$MP_ANTICORPS == "CD8")
test.CD8$MP_ANTICORPS <- NULL
names(test.CD8)[names(test.CD8) == "NIP"] <- "NIP"
names(test.CD8)[names(test.CD8) == "MP_AC_DENSMOY"] <- "CD8_MP_AC_DENSMOY"
names(test.CD8)[names(test.CD8) == "MP_AC_DENSMIN"] <- "CD8_MP_AC_DENSMIN"
names(test.CD8)[names(test.CD8) == "MP_AC_DENSMAX"] <- "CD8_MP_AC_DENSMAX"
names(test.CD8)[names(test.CD8) == "MP_AC_DIST_IT"] <- "CD8_MP_AC_DIST_IT"
names(test.CD8)[names(test.CD8) == "MP_AC_DIST_IST"] <- "CD8_MP_AC_DIST_IST"
names(test.CD8)[names(test.CD8) == "MP_AC_DIST_SD"] <- "CD8_MP_AC_DIST_SD"

test.FOXP3 <- filter(dIHC, dIHC$MP_ANTICORPS == "FOXP3")
test.FOXP3$MP_ANTICORPS <- NULL
names(test.FOXP3)[names(test.FOXP3) == "NIP"] <- "NIP"
names(test.FOXP3)[names(test.FOXP3) == "MP_AC_DENSMOY"] <- "FOXP3_MP_AC_DENSMOY"
names(test.FOXP3)[names(test.FOXP3) == "MP_AC_DENSMIN"] <- "FOXP3_MP_AC_DENSMIN"
names(test.FOXP3)[names(test.FOXP3) == "MP_AC_DENSMAX"] <- "FOXP3_MP_AC_DENSMAX"
names(test.FOXP3)[names(test.FOXP3) == "MP_AC_DIST_IT"] <- "FOXP3_MP_AC_DIST_IT"
names(test.FOXP3)[names(test.FOXP3) == "MP_AC_DIST_IST"] <- "FOXP3_MP_AC_DIST_IST"
names(test.FOXP3)[names(test.FOXP3) == "MP_AC_DIST_SD"] <- "FOXP3_MP_AC_DIST_SD"

test.CD3 <- filter(dIHC, dIHC$MP_ANTICORPS == "CD3")
test.CD3$MP_ANTICORPS <- NULL
names(test.CD3)[names(test.CD3) == "NIP"] <- "NIP"
names(test.CD3)[names(test.CD3) == "MP_AC_DENSMOY"] <- "CD3_MP_AC_DENSMOY"
names(test.CD3)[names(test.CD3) == "MP_AC_DENSMIN"] <- "CD3_MP_AC_DENSMIN"
names(test.CD3)[names(test.CD3) == "MP_AC_DENSMAX"] <- "CD3_MP_AC_DENSMAX"
names(test.CD3)[names(test.CD3) == "MP_AC_DIST_IT"] <- "CD3_MP_AC_DIST_IT"
names(test.CD3)[names(test.CD3) == "MP_AC_DIST_IST"] <- "CD3_MP_AC_DIST_IST"
names(test.CD3)[names(test.CD3) == "MP_AC_DIST_SD"] <- "CD3_MP_AC_DIST_SD"

RESULT <- merge(test.CD3, test.CD8, by = "NIP")
RESULT <- merge(RESULT, test.FOXP3, by  = "NIP")


#dClinicRNA <- data.frame(ClinicRNA$NIP.1, ClinicRNA$Sexe_patient, ClinicRNA$GlobalHisto, ClinicRNA$TCGA, 
#                         ClinicRNA$CODE_PATHOLOGIE_Simbad, ClinicRNA$Organe, ClinicRNA$Lesion, ClinicRNA$CD274, 
#                         ClinicRNA$PDCD1, ClinicRNA$CD8A, ClinicRNA$CD3G, ClinicRNA$CD3E, ClinicRNA$CD3D, 
#                         ClinicRNA$FOXP3, ClinicRNA$LAG3, ClinicRNA$Age, ClinicRNA$STATUS, ClinicRNA$OS)

DiffDay<-as.Date(ClinicRNA$date_biopsie,"%d/%m/%Y")-as.Date(ClinicRNA$DATE_NAISSANCE_Simbad,"%d/%m/%Y")
DiffMonth<-as.numeric(floor((DiffDay/365)*12))

dClinicRNA <- data.frame(ClinicRNA$NIP.1, ClinicRNA$Sexe_patient, ClinicRNA$GlobalHisto, ClinicRNA$TCGA, 
                         ClinicRNA$CODE_PATHOLOGIE_Simbad, ClinicRNA$Organe, ClinicRNA$Lesion, ClinicRNA$CD274, 
                         ClinicRNA$PDCD1, ClinicRNA$CD8A, ClinicRNA$CD3G, ClinicRNA$CD3E, ClinicRNA$CD3D, 
                         ClinicRNA$FOXP3, ClinicRNA$LAG3, DiffMonth, ClinicRNA$STATUS, ClinicRNA$OS)

names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.NIP.1"] <- "NIP"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.Sexe_patient"] <- "Sexe_patient"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.GlobalHisto"] <- "GlobalHisto"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.TCGA"] <- "TCGA"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.CODE_PATHOLOGIE_Simbad"] <- "CODE_PATHOLOGIE_Simbad"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.Organe"] <- "Organe"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.Lesion"] <- "Lesion"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.CD274"] <- "CD274"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.PDCD1"] <- "PDCD1"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.CD8A"] <- "CD8A"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.CD3G"] <- "CD3G"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.CD3E"] <- "CD3E"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.CD3D"] <- "CD3D"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.FOXP3"] <- "FOXP3"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.LAG3"] <- "LAG3"
#names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.Age"] <- "Age"
names(dClinicRNA)[names(dClinicRNA) == "DiffMonth"]<-"Age_biopsie"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.STATUS"] <- "STATUS"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.OS"] <- "OS"

#dMOSCATO <- data.frame(MOSCATO$NIP, MOSCATO$MP_POURCENT0_PDL1, MOSCATO$MP_POURCENT1_PDL1, MOSCATO$MP_POURCENT2_PDL1, MOSCATO$MP_POURCENT3_PDL1, MOSCATO$MP_PDL1_PCI)
dMOSCATO <- data.frame(MOSCATO$NIP, MOSCATO$MP_POURCENT0, MOSCATO$MP_POURCENT1, MOSCATO$MP_POURCENT2, MOSCATO$MP_POURCENT3, MOSCATO$MP_PDL1_PCI)

names(dMOSCATO)[names(dMOSCATO) == "MOSCATO.NIP"] <- "NIP"
#names(dMOSCATO)[names(dMOSCATO) == "MOSCATO.MP_POURCENT0_PDL1"] <- "MP_POURCENT0_PDL1"
#names(dMOSCATO)[names(dMOSCATO) == "MOSCATO.MP_POURCENT1_PDL1"] <- "MP_POURCENT1_PDL1"
#names(dMOSCATO)[names(dMOSCATO) == "MOSCATO.MP_POURCENT2_PDL1"] <- "MP_POURCENT2_PDL1"
#names(dMOSCATO)[names(dMOSCATO) == "MOSCATO.MP_POURCENT3_PDL1"] <- "MP_POURCENT3_PDL1"
names(dMOSCATO)[names(dMOSCATO) == "MOSCATO.MP_POURCENT0"] <- "MP_POURCENT0"
names(dMOSCATO)[names(dMOSCATO) == "MOSCATO.MP_POURCENT1"] <- "MP_POURCENT1"
names(dMOSCATO)[names(dMOSCATO) == "MOSCATO.MP_POURCENT2"] <- "MP_POURCENT2"
names(dMOSCATO)[names(dMOSCATO) == "MOSCATO.MP_POURCENT3"] <- "MP_POURCENT3"
names(dMOSCATO)[names(dMOSCATO) == "MOSCATO.MP_PDL1_PCI"] <- "MP_PDL1_PCI"

RESULT <- merge(dClinicRNA, RESULT, by = "NIP")

RESULT <- merge(dMOSCATO, RESULT, by = "NIP")

#Filtrage des données inexploitables (données quantitatives manquantes)

RESULT <- filter(RESULT, RESULT$CD3D != "NA" & RESULT$OS != "NA" 
                 & RESULT$CD8_MP_AC_DENSMOY != "NA" & RESULT$MP_POURCENT3_PDL1!="NA" 
                 & RESULT$MP_PDL1_PCI != "" & RESULT$CD3_MP_AC_DIST_IT != "NA" 
                 & RESULT$FOXP3_MP_AC_DIST_SD != "NA" & RESULT$MP_PDL1_PCI != "")


#One-hot encoding

names = c("Sexe_patient", "GlobalHisto","TCGA","CODE_PATHOLOGIE_Simbad","Organe","Lesion")

for (i in names){
  d <- dummy(RESULT[[i]], sep = " : ", verbose = TRUE)
  colnames(d) <- gsub("RESULT", i, fixed = TRUE, colnames(d))
  RESULT[[i]] <- NULL
  if (length(colnames(d)) < 20){
    RESULT <- cbind(RESULT, d)
  }
}

#Élimination des données catégorielles trop peu représentées : limite ici = 6

#for (i in colnames(RESULT)) {
#  if (sum(RESULT[[i]]) < 6) {
#    RESULT[[i]] <- NULL
#  }
#}

for (i in colnames(RESULT)) {
  if (is.numeric(RESULT[[i]])) {
      if (sum(RESULT[[i]]) < 6) {
        RESULT[[i]] <- NULL
      }
  }
}


RESULT["GlobalHisto : "] <- NULL
RESULT["GlobalHisto : NA"] <- NULL


write.csv2(RESULT, "Onehot_wNA.csv")


#m_correlation <- cor(R1, method = "kendall", use = "pairwise.complete.obs")
#write.csv2(m_correlation, "Correlations.csv")

# ############################### Corrélations ######################
# data_correlation <- RESULT1
# data_correlation$NIP <- NULL
# data_correlation$Sexe_patient <- NULL
# data_correlation$TCGA <- NULL
# data_correlation$CODE_PATHOLOGIE_Simbad <- NULL
# data_correlation$Lesion <- NULL
# data_correlation$DATE_DERNIERE_NOUVELLE_Simbad <- NULL
# ################ ORgane ##############
# correlation_organe <- data_correlation
# correlation_organe$GlobalHisto <- NULL
# 
# correlation_FF <- filter(correlation_organe, correlation_organe$Organe == "FF")
# correlation_FF$Organe <- NULL
# correlation_RP <- filter(correlation_organe, correlation_organe$Organe == "RP")
# correlation_RP$Organe <- NULL
# correlation_SG <- filter(correlation_organe, correlation_organe$Organe == "SG")
# correlation_SG$Organe <- NULL
# 
# write.csv2(cor(correlation_FF, method = "kendall", use = "pairwise.complete.obs"), "Corr_FF.csv")
# write.csv2(cor(correlation_RP, method = "kendall", use = "pairwise.complete.obs"), "Corr_RP.csv")
# write.csv2(cor(correlation_SG, method = "kendall", use = "pairwise.complete.obs"), "Corr_SG.csv")
# 
# 
# ################ GlobalHisto ################
# correlation_histo <- data_correlation
# correlation_histo$Organe <- NULL
# 
# 
# correlation_gastro <- filter(correlation_histo, correlation_histo$GlobalHisto == "GASTRO INTESTINAL")
# correlation_gastro$GlobalHisto <- NULL
# correlation_gynecological <- filter(correlation_histo, correlation_histo$GlobalHisto == "GYNECOLOGICAL SPHERE")
# correlation_gynecological$GlobalHisto <- NULL
# correlation_headneck <- filter(correlation_histo, correlation_histo$GlobalHisto == "HEAD AND NECK")
# correlation_headneck$GlobalHisto <- NULL
# correlation_urological <- filter(correlation_histo, correlation_histo$GlobalHisto == "UROLOGICAL SPHERE")
# correlation_urological$GlobalHisto <- NULL
# 
# 
# write.csv2(cor(correlation_gastro, method = "kendall", use = "pairwise.complete.obs"), "Corr_gastro.csv")
# write.csv2(cor(correlation_gynecological, method = "kendall", use = "pairwise.complete.obs"), "Corr_gynecological.csv")
# write.csv2(cor(correlation_headneck, method = "kendall", use = "pairwise.complete.obs"), "Corr_headneck.csv")
# write.csv2(cor(correlation_urological, method = "kendall", use = "pairwise.complete.obs"), "Corr_urological.csv")
# 
# 
# 
# 
