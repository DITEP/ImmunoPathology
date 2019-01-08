rm(list=ls())
library(dplyr)
library(dummies)
library(zoo)

########### PROJET INNO : 2eme JET DE DONNÉES ##########################

# Lecture des fichiers
IHC <- read.csv(file="E:/Leo/Bioinfo/Project/Imuno_loic/IHCimmuno/IHC_total_V3_M2421.csv", header=TRUE, sep=";", na.strings="NA", dec=",",skip=1)
ClinicRNA <- read.csv(file="E:/Leo/Bioinfo/Project/Imuno_loic/IHCimmuno/ClinicRNA_total_OS_Update_post3.csv", header=TRUE, sep=";", na.strings="NA", dec=",",skip = 1)
MOSCATO <- read.csv(file="E:/Leo/Bioinfo/Project/Imuno_loic/IHCimmuno/IHC_PDL1_V4.csv", header=TRUE, sep=";", na.strings="NA", dec=",",skip=1)

#Fusion en 1 seul csv
dIHC <- data.frame(IHC$NIP, IHC$MP_ANTICORPS, IHC$MP_AC_DENSMOY, 
                   IHC$MP_AC_DIST_IT, IHC$MP_AC_DIST_IST, IHC$MP_AC_DIST_SD)
names(dIHC)[names(dIHC) == "IHC.NIP"] <- "NIP"
names(dIHC)[names(dIHC) == "IHC.MP_ANTICORPS"] <- "MP_ANTICORPS"
names(dIHC)[names(dIHC) == "IHC.MP_AC_DENSMOY"] <- "MP_AC_DENSMOY"
names(dIHC)[names(dIHC) == "IHC.MP_AC_DIST_IT"] <- "MP_AC_DIST_IT"
names(dIHC)[names(dIHC) == "IHC.MP_AC_DIST_IST"] <- "MP_AC_DIST_IST"
names(dIHC)[names(dIHC) == "IHC.MP_AC_DIST_SD"] <- "MP_AC_DIST_SD"

test.CD8 <- filter(dIHC, dIHC$MP_ANTICORPS == "CD8")
test.CD8$MP_ANTICORPS <- NULL
test.CD8<-aggregate(test.CD8,by=list(test.CD8$NIP), mean ,na.rm=TRUE,na.action="na.pass")
test.CD8$NIP<-NULL
names(test.CD8)[names(test.CD8) == "Group.1"] <- "NIP"
names(test.CD8)[names(test.CD8) == "MP_AC_DENSMOY"] <- "CD8_MP_AC_DENSMOY"
names(test.CD8)[names(test.CD8) == "MP_AC_DIST_IT"] <- "CD8_MP_AC_DIST_IT"
names(test.CD8)[names(test.CD8) == "MP_AC_DIST_IST"] <- "CD8_MP_AC_DIST_IST"
names(test.CD8)[names(test.CD8) == "MP_AC_DIST_SD"] <- "CD8_MP_AC_DIST_SD"


test.FOXP3 <- filter(dIHC, dIHC$MP_ANTICORPS == "FOXP3")
test.FOXP3$MP_ANTICORPS <- NULL
test.FOXP3<-aggregate(test.FOXP3,by=list(test.FOXP3$NIP), mean ,na.rm=TRUE,na.action="na.pass")
test.FOXP3$NIP<-NULL
names(test.FOXP3)[names(test.FOXP3) == "Group.1"] <- "NIP"
names(test.FOXP3)[names(test.FOXP3) == "MP_AC_DENSMOY"] <- "FOXP3_MP_AC_DENSMOY"
names(test.FOXP3)[names(test.FOXP3) == "MP_AC_DIST_IT"] <- "FOXP3_MP_AC_DIST_IT"
names(test.FOXP3)[names(test.FOXP3) == "MP_AC_DIST_IST"] <- "FOXP3_MP_AC_DIST_IST"
names(test.FOXP3)[names(test.FOXP3) == "MP_AC_DIST_SD"] <- "FOXP3_MP_AC_DIST_SD"


test.CD3 <- filter(dIHC, dIHC$MP_ANTICORPS == "CD3")
test.CD3$MP_ANTICORPS <- NULL
test.CD3<-aggregate(test.CD3,by=list(test.CD3$NIP), mean ,na.rm=TRUE,na.action="na.pass")
test.CD3$NIP<-NULL
names(test.CD3)[names(test.CD3) == "Group.1"] <- "NIP"
names(test.CD3)[names(test.CD3) == "MP_AC_DENSMOY"] <- "CD3_MP_AC_DENSMOY"
names(test.CD3)[names(test.CD3) == "MP_AC_DIST_IT"] <- "CD3_MP_AC_DIST_IT"
names(test.CD3)[names(test.CD3) == "MP_AC_DIST_IST"] <- "CD3_MP_AC_DIST_IST"
names(test.CD3)[names(test.CD3) == "MP_AC_DIST_SD"] <- "CD3_MP_AC_DIST_SD"

RESULT <- merge(test.CD3, test.CD8, by = "NIP")
RESULT <- merge(RESULT, test.FOXP3, by  = "NIP")


DiffDay<-as.Date(ClinicRNA$date_biopsie,"%d/%m/%Y")-as.Date(ClinicRNA$DATE_NAISSANCE_Simbad,"%d/%m/%Y")
DiffMonth<-as.numeric(floor((DiffDay/365)*12))
DiffYear<-as.numeric(floor(DiffDay/365))

#Check Age
CheckAge<-data.frame(NIP=ClinicRNA$NIP.1,DATE_NAISSANCE=ClinicRNA$DATE_NAISSANCE_Simbad,
              date_biopsie=ClinicRNA$date_biopsie,DATE_DECES=ClinicRNA$DATE_DECES_Simbad,
              DATE_VENUE_DERNIERE=ClinicRNA$DATE_VENUE_DERNIERE_Simbad,
              DATE_HOSPI_DERNIERE=ClinicRNA$DATE_HOSPI_DERNIERE_Simbad,
              AgeBiopsie=DiffYear,age=ClinicRNA$age,absDiff=abs(DiffYear-ClinicRNA$age))

head(CheckAge[order(CheckAge$absDiff,decreasing = TRUE),],n=10)

#Compute STATUS
STATUS<-as.numeric(!is.na(ClinicRNA$DATE_DECES_Simbad))


#Compute OS
OS<-as.Date(apply(data.frame(as.Date(ClinicRNA$DATE_DECES_Simbad,"%d/%m/%Y"),as.Date(ClinicRNA$DATE_VENUE_DERNIERE_Simbad,"%d/%m/%Y"),
    as.Date(ClinicRNA$DATE_HOSPI_DERNIERE_Simbad,"%d/%m/%Y")),1,max,na.rm=TRUE))-as.Date(ClinicRNA$date_biopsie,"%d/%m/%Y")

#Convert in Month
OS<-as.numeric(OS)*0.0328767


dClinicRNA <- data.frame(ClinicRNA$NIP.1, ClinicRNA$Organe, ClinicRNA$GlobalHisto, ClinicRNA$Lesion, 
                         ClinicRNA$PS, ClinicRNA$RMH, ClinicRNA$MTA_fu, ClinicRNA$IO_fu, ClinicRNA$H_score, DiffMonth,STATUS, OS)

names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.NIP.1"] <- "NIP"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.Organe"] <- "Organe"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.GlobalHisto"] <- "GlobalHisto"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.Lesion"] <- "Lesion"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.PS"] <- "PS"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.RMH"] <- "RMH"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.MTA_fu"] <- "MTA_fu"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.IO_fu"] <- "IO_fu"
names(dClinicRNA)[names(dClinicRNA) == "ClinicRNA.H_score"] <- "H_score"
names(dClinicRNA)[names(dClinicRNA) == "DiffMonth"]<-"Age_biopsie"




##dMOSCATO <- data.frame(MOSCATO$NIP, MOSCATO$MP_POURCENT0_PDL1, MOSCATO$MP_POURCENT1_PDL1, MOSCATO$MP_POURCENT2_PDL1, 
##                       MOSCATO$MP_POURCENT3_PDL1, MOSCATO$MP_PDL1_PCI)

#Check Hscore
H<-data.frame(NIP=MOSCATO$NIP,H=MOSCATO$MP_POURCENT0*0+MOSCATO$MP_POURCENT1*1+MOSCATO$MP_POURCENT2*2+MOSCATO$MP_POURCENT3*3)
CheckHscore<-merge(dClinicRNA[,c(1,9)],H,by=1,all.x=TRUE)
CheckHscore<-data.frame(CheckHscore,absDiff=abs(CheckHscore$H_score-CheckHscore$H))
head(CheckHscore[order(CheckHscore$absDiff,decreasing = TRUE),],n=10)

#continue with my_Hscore
dClinicRNA$H_score<-NULL

dMOSCATO <- data.frame(MOSCATO$NIP, MOSCATO$MP_POURCENT0, MOSCATO$MP_POURCENT1, MOSCATO$MP_POURCENT2, 
                       MOSCATO$MP_POURCENT3, MOSCATO$MP_PDL1_PCI,H$H)

names(dMOSCATO)[names(dMOSCATO) == "MOSCATO.NIP"] <- "NIP"
names(dMOSCATO)[names(dMOSCATO) == "MOSCATO.MP_POURCENT0"] <- "MP_POURCENT0"
names(dMOSCATO)[names(dMOSCATO) == "MOSCATO.MP_POURCENT1"] <- "MP_POURCENT1"
names(dMOSCATO)[names(dMOSCATO) == "MOSCATO.MP_POURCENT2"] <- "MP_POURCENT2"
names(dMOSCATO)[names(dMOSCATO) == "MOSCATO.MP_POURCENT3"] <- "MP_POURCENT3"
names(dMOSCATO)[names(dMOSCATO) == "MOSCATO.MP_PDL1_PCI"] <- "MP_PDL1_PCI"
names(dMOSCATO)[names(dMOSCATO) == "H.H"] <- "my_Hscore"

RESULT <- merge(dClinicRNA, RESULT, by = "NIP")

RESULT <- merge(dMOSCATO, RESULT, by = "NIP")


#CD8_FOXP3_ratio	CD8_PDL1_ratio	CD8_CD3_ratio	FOXP3_CD3_ratio

#CD8_FOXP3_ratio<-as.data.frame(RESULT$CD8_MP_AC_DENSMOY/RESULT$FOXP3_MP_AC_DENSMOY)
#colnames(CD8_FOXP3_ratio)="CD8_FOXP3_ratio"
#CD8_PDL1_ratio<-as.data.frame(RESULT$CD8_MP_AC_DENSMOY/RESULT$my_Hscore)
#colnames(CD8_PDL1_ratio)="CD8_PDL1_ratio"
#CD8_CD3_ratio<-as.data.frame(RESULT$CD8_MP_AC_DENSMOY/RESULT$CD3_MP_AC_DENSMOY)
#colnames(CD8_CD3_ratio)="CD8_CD3_ratio"
#FOXP3_CD3_ratio<-as.data.frame(RESULT$FOXP3_MP_AC_DENSMOY/RESULT$CD3_MP_AC_DENSMOY)
#colnames(FOXP3_CD3_ratio)="FOXP3_CD3_ratio"

#division by 0 issues

#RESULT<-cbind(RESULT,CD8_CD3_ratio,FOXP3_CD3_ratio)


#pool organe and PDL1 class
Organe_PDL1<-as.data.frame(paste(RESULT$Organe,":",RESULT$MP_PDL1_PCI,sep=""))
colnames(Organe_PDL1)="Organe_PDL1"

RESULT<-cbind(RESULT,Organe_PDL1)



#Filtrage des données inexploitables (données quantitatives manquantes)

#RESULT <- filter(RESULT, RESULT$CD3D != "NA" & RESULT$OS != "NA" 
#                & RESULT$CD8_MP_AC_DENSMOY != "NA" & RESULT$MP_POURCENT3_PDL1!="NA" 
#                & RESULT$MP_PDL1_PCI != "" & RESULT$CD3_MP_AC_DIST_IT != "NA" 
#                & RESULT$FOXP3_MP_AC_DIST_SD != "NA" & RESULT$MP_PDL1_PCI != "")



RESULT <- filter(RESULT, !is.na(RESULT$OS)  & !is.na(RESULT$Age_biopsie)  & !is.na(RESULT$STATUS) 
                 & !is.na(RESULT$MP_POURCENT0)  & !is.na(RESULT$MP_POURCENT1)  & !is.na(RESULT$MP_POURCENT2)  & !is.na(RESULT$MP_POURCENT3) 
                & !is.na(RESULT$MP_PDL1_PCI)  & RESULT$MP_PDL1_PCI != ""
                & !is.na(RESULT$CD8_MP_AC_DENSMOY)
                & !is.na(RESULT$CD8_MP_AC_DIST_IT)  & !is.na(RESULT$CD8_MP_AC_DIST_IST)  & !is.na(RESULT$CD8_MP_AC_DIST_SD)
                & !is.na(RESULT$CD3_MP_AC_DENSMOY)  
                & !is.na(RESULT$CD3_MP_AC_DIST_IT)  & !is.na(RESULT$CD3_MP_AC_DIST_IST)  & !is.na(RESULT$CD3_MP_AC_DIST_SD)                
                & !is.na(RESULT$FOXP3_MP_AC_DENSMOY)  
                & !is.na(RESULT$FOXP3_MP_AC_DIST_IT)  & !is.na(RESULT$FOXP3_MP_AC_DIST_IST) & !is.na(RESULT$FOXP3_MP_AC_DIST_SD) 
                & !is.na(RESULT$my_Hscore)  & !is.na(RESULT$PS)  & !is.na(RESULT$RMH)  & !is.na(RESULT$MTA_fu)  & !is.na(RESULT$IO_fu) )



#One-hot encoding

#names = c("Sexe_patient", "GlobalHisto","TCGA","CODE_PATHOLOGIE_Simbad","Organe","Lesion")

#for (i in names){
#  d <- dummy(RESULT[[i]], sep = " : ", verbose = TRUE)
#  colnames(d) <- gsub("RESULT", i, fixed = TRUE, colnames(d))
#  RESULT[[i]] <- NULL
#  if (length(colnames(d)) < 20){
#    RESULT <- cbind(RESULT, d)
#  }
#}


names = c("GlobalHisto","Organe","Lesion","Organe_PDL1")

for (i in names){
  d <- dummy(RESULT[[i]], sep = " : ", verbose = TRUE)
  RESULT[[i]] <- NULL
  colnames(d) <- gsub("RESULT", i, fixed = TRUE, colnames(d))
  RESULT <- cbind(RESULT, d)
}





#Élimination des données catégorielles trop peu représentées : limite ici = 6

##for (i in colnames(RESULT)) {
##    if (sum(RESULT[[i]]) < 6) {
##      RESULT[[i]] <- NULL
##    }
##}

for (i in colnames(RESULT)) {
  if (is.integer(RESULT[[i]])) {
      if (sum(RESULT[[i]]) < 6) {
        RESULT[[i]] <- NULL
      }
  }
}



write.csv2(RESULT, "Onehot_noNA_v2.csv",row.names = FALSE)


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
