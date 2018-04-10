#### check survival
FUSIHC<-read.csv2("~/ClinicRNA_total.csv",stringsAsFactors = F,row.names = 1)

# put status
FUSIHC$DATE_DECES_Simbad[grep("9999",FUSIHC$DATE_DECES_Simbad)]<-NA
FUSIHC[,"STATUS"]<-0
FUSIHC[!is.na(FUSIHC$DATE_DECES_Simbad),"STATUS"]<-1

# transform date 9999 in NA
FUSIHC$DATE_VENUE_DERNIERE_Simbad[grep("9999",FUSIHC$DATE_VENUE_DERNIERE_Simbad)]<-NA
FUSIHC$DATE_HOSPI_DERNIERE_Simbad[grep("9999",FUSIHC$DATE_HOSPI_DERNIERE_Simbad)]<-NA

# compute the "date de derniere nouvelle"
DDN<-cbind(FUSIHC$DATE_HOSPI_DERNIERE_Simbad[is.na(FUSIHC$DATE_DECES_Simbad)],
FUSIHC$DATE_VENUE_DERNIERE_Simbad[is.na(FUSIHC$DATE_DECES_Simbad)])

#check
cbind(DDN,apply(DDN,1,function(x)diff(as.Date(x,format="%d/%m/%Y"))))

#put the greater date as the DDN
DDN3<-ifelse(apply(DDN,1,function(x)diff(as.Date(x,format="%d/%m/%Y")))>=0,DDN[,2],DDN[,1])
DDN<-cbind(DDN,DDN3)

# fill when NA (impossible to do diff)
DDN[is.na(DDN[,1]),3] <-DDN[is.na(DDN[,1]),2]
DDN[is.na(DDN[,2])&is.na(DDN[,3]),3] <-DDN[is.na(DDN[,2])&is.na(DDN[,3]),1]

# add to the table
FUSIHC[is.na(FUSIHC$DATE_DECES_Simbad),"DATE_DECES_Simbad"]<-DDN[,3]

# check
data.frame(DC=as.Date(FUSIHC$DATE_DECES_Simbad,format="%d/%m/%Y"),B=as.Date(FUSIHC$date_biopsie,format="%d/%m/%Y"))

# an error for 1
FUSIHC[grep("2015",FUSIHC$DATE_DECES_Simbad),"DATE_DECES_Simbad"]<-FUSIHC[grep("2015",FUSIHC$DATE_DECES_Simbad),"date_biopsie"]

library(survival)
FUSIHC$OS<-as.numeric(as.Date(FUSIHC$DATE_DECES_Simbad,format="%d/%m/%Y")-as.Date(FUSIHC$date_biopsie,format="%d/%m/%Y"))
plot(survfit(Surv(FUSIHC$OS,FUSIHC$STATUS)~NULL),xlab = "Time in days", ylab = "Probability of survival",mark.time=T)

write.csv2(FUSIHC,"~/ClinicRNA_total_OS.csv")
