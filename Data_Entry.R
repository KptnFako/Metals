# DATA FROM ACCESS
setwd("C:/Users/Robin Burgold/tubCloud/shared1/Grafiken/All/IC/Neu")

Nitratfaktor<-4.42664
Nitritfaktor<-3.28443
Ammoniumfaktor<-1.28786

# Manipuliert eine Standardabfrage aus Access in das GGPLOT(long)-Format
# ACHTUNG HIER WIRD NICHT DIE LISTE ÜBERGEBEN, SONDERN NUR DAS ERGEBNIS AUS BEIDEN STANDORTEN KOMBINIERT ALS DATAFRAME
CSVTOGGPLOTDF<-function(df){
  #Datum als Datum einlesen
  df$Datum<-as.Date(df$Datum)
  df$Probenart<-NULL
  # Tabellentrennung Standorte
  df_A<-df[df[,3]=="A",]
  df_B<-df[df[,3]=="B",]
  # Löschung der Standortzeile (Info ist jetzt im Tabellenname oder irrelevant)
  df_A$Standort<-NULL
  df_B$Standort<-NULL
  df$Standort<-NULL
  # Mittelwert der Triplicates 
  MW_df_A<-MEAN(df_A,3)
  MW_df_B<-MEAN(df_B,3)
  MW_df<-MEAN(df,6)
  # Standardabweichung der Triplicates 
  SD_df_A<-STABW(df_A,3)
  SD_df_B<-STABW(df_B,3)
  SD_df<-STABW(df,6)
  
  # Convert data from wide format to long format
  data_long_A <- pivot_longer(MW_df_A[,-1], cols = -Datum, names_to = "Dataset", values_to = "Value")
  data_long_B <- pivot_longer(MW_df_B[,-1], cols = -Datum, names_to = "Dataset", values_to = "Value")
  data_long_All <- pivot_longer(MW_df[,-1], cols = -Datum, names_to = "Dataset", values_to = "Value")
  #Convert data from STABW to long format
  data_sd_long_A <- pivot_longer(SD_df_A[,-1], cols = -Datum, names_to = "Dataset", values_to = "SD")
  data_sd_long_B <- pivot_longer(SD_df_B[,-1], cols = -Datum, names_to = "Dataset", values_to = "SD")
  data_sd_long_All <- pivot_longer(SD_df[,-1], cols = -Datum, names_to = "Dataset", values_to = "SD")
  # Correct Datum in SD
  data_sd_long_A$Datum<-data_long_A$Datum
  data_sd_long_B$Datum<-data_long_B$Datum
  data_sd_long_All$Datum<-data_long_All$Datum
  
  # Combine data
  data_combined_A <- merge(data_long_A, data_sd_long_A, by = c("Datum", "Dataset"))
  data_combined_B <- merge(data_long_B, data_sd_long_B, by = c("Datum", "Dataset"))
  data_combined_All <- merge(data_long_All, data_sd_long_All, by = c("Datum", "Dataset"))
  
  Data<-(list(data_combined_All,data_combined_A, data_combined_B))
  names(Data)<-c("All","A","B")
  return(Data[[1]])
}

#Schäfersee 2022 
SVIC22 <- (read.table("22_SV_IC.csv", header=TRUE, dec=",", sep=";"))
NH4_22 <- (read.table("22_NH4.csv", header=TRUE, dec=",", sep=";"))

SVIC22$NO2.N<-SVIC22$Nitrit/Nitritfaktor
SVIC22$NO3.N<-SVIC22$Nitrat/Nitratfaktor
NH4_22$NH4.N<-NH4_22$Ammonium/Ammoniumfaktor

NO2.N_22<-SVIC22[,-c(5:8,10)]
NO3.N_22<-SVIC22[,-c(5:9)]
PO4_22<-SVIC22[,-c(5:6,8:10)]
SO4_22<-SVIC22[,-c(5:7,9:10)]
NH4_22<-NH4_22[,-5]

NO2.N_TW_22<-NO2.N_22[NO2.N_22[,4]=="TW",]
NO2.N_PW_22<-NO2.N_22[NO2.N_22[,4]=="PW",]
NO2.N_OW_22<-NO2.N_22[NO2.N_22[,4]=="OW",]

NO3.N_TW_22<-NO3.N_22[NO3.N_22[,4]=="TW",]
NO3.N_PW_22<-NO3.N_22[NO3.N_22[,4]=="PW",]
NO3.N_OW_22<-NO3.N_22[NO3.N_22[,4]=="OW",]

PO4_TW_22<-PO4_22[PO4_22[,4]=="TW",]
PO4_PW_22<-PO4_22[PO4_22[,4]=="PW",]
PO4_OW_22<-PO4_22[PO4_22[,4]=="OW",]

SO4_TW_22<-SO4_22[SO4_22[,4]=="TW",]
SO4_PW_22<-SO4_22[SO4_22[,4]=="PW",]
SO4_OW_22<-SO4_22[SO4_22[,4]=="OW",]

NH4_TW_22<-NH4_22[NH4_22[,4]=="TW",]
NH4_PW_22<-NH4_22[NH4_22[,4]=="PW",]
NH4_OW_22<-NH4_22[NH4_22[,4]=="OW",]

names(NO2.N_TW_22)[5]<-"Bottom Water"
names(NO2.N_PW_22)[5]<-"Pore Water"
names(NO2.N_OW_22)[5]<-"Surface Water"

names(NO3.N_TW_22)[5]<-"Bottom Water"
names(NO3.N_PW_22)[5]<-"Pore Water"
names(NO3.N_OW_22)[5]<-"Surface Water"

names(PO4_TW_22)[5]<-"Bottom Water"
names(PO4_PW_22)[5]<-"Pore Water"
names(PO4_OW_22)[5]<-"Surface Water"

names(SO4_TW_22)[5]<-"Bottom Water"
names(SO4_PW_22)[5]<-"Pore Water"
names(SO4_OW_22)[5]<-"Surface Water"

lNO2.N_TW_22<-CSVTOGGPLOTDF(NO2.N_TW_22)
lNO2.N_PW_22<-CSVTOGGPLOTDF(NO2.N_PW_22)
lNO2.N_OW_22<-CSVTOGGPLOTDF(NO2.N_OW_22)

lNO3.N_TW_22<-CSVTOGGPLOTDF(NO3.N_TW_22)
lNO3.N_PW_22<-CSVTOGGPLOTDF(NO3.N_PW_22)
lNO3.N_OW_22<-CSVTOGGPLOTDF(NO3.N_OW_22)

lPO4_TW_22<-CSVTOGGPLOTDF(PO4_TW_22)
lPO4_PW_22<-CSVTOGGPLOTDF(PO4_PW_22)
lPO4_OW_22<-CSVTOGGPLOTDF(PO4_OW_22)

lSO4_TW_22<-CSVTOGGPLOTDF(SO4_TW_22)
lSO4_PW_22<-CSVTOGGPLOTDF(SO4_PW_22)
lSO4_OW_22<-CSVTOGGPLOTDF(SO4_OW_22)

lNH4_TW_22<-CSVTOGGPLOTDF(NH4_TW_22)
lNH4_PW_22<-CSVTOGGPLOTDF(NH4_PW_22)
lNH4_OW_22<-CSVTOGGPLOTDF(NH4_OW_22)

lNO2<-rbind(lNO2.N_TW_22,lNO2.N_PW_22,lNO2.N_OW_22)
lNO3<-rbind(lNO3.N_TW_22,lNO3.N_PW_22,lNO3.N_OW_22)
lPO4<-rbind(lPO4_TW_22,lPO4_PW_22,lPO4_OW_22)
lSO4<-rbind(lSO4_TW_22,lSO4_PW_22,lSO4_OW_22)

PLOTSINGLE<-function(dataframe,plottitle,colnumber,yaxis){
  Plots<-GENERALPLOT(dataframe,plottitle,colnumber, yaxis, "2022")
  return(Plots)
}

NO2Plots<-PLOTSINGLE(lNO2,"NO2-N", 3, "[mg/L]")



NO3Plots<-PLOTSINGLE(lNO3,"NO3-N", 3, "[mg/L]")
SO4Plots<-PLOTSINGLE(lSO4,"SO4", 1, "[mg/L]")
PO4Plots<-PLOTSINGLE(lPO4,"PO4", 1, "[mg/L]")

# Speichert die Plots als PNG mit dem Präfix PLOTNAME
SAVEPLOTS<-function(plotlist,Plotname){
  ggsave(paste0(Plotname, "_1.png"), plot = plotlist, width = 12, height = 4)
}

SAVEPLOTS(NO3Plots,"Nitrat")

getwd()
# 
# #Schäfersee 2021
# SVIC21 <- (read.table("22_SV_IC.csv", header=TRUE, dec=",", sep=";"))
# 
# SVIC21$NO2.N<-SVIC21$Nitrit/Nitritfaktor
# SVIC21$NO3.N<-SVIC21$Nitrat/Nitratfaktor
# 
# 
# 
# #2021 Data
# SVIC21 <- (read.table("21_SV_IC.csv", header=TRUE, dec=",", sep=";"))
# 
# ICPW21<-SVIC21[SVIC21[,4]=="PW",]
# ICTW21<-SVIC21[SVIC21[,4]=="TW",]
# 
# NH21 <- (read.table("21_NH4.csv", header=TRUE, dec=",", sep=";"))
# NHPW21<-NH21[NH21[,4]=="PW",]
# NHTW21<-NH21[NH21[,4]=="TW",]
# 
# ICOW22Data <- CSVTOGGPLOT(read.table("22_IC_OW.csv", header=TRUE, dec=",", sep=";"))
# ICPW22Data <- CSVTOGGPLOT(read.table("22_IC_PW.csv", header=TRUE, dec=",", sep=";"))
# ICTW22Data <- CSVTOGGPLOT(read.table("22_IC_TW.csv", header=TRUE, dec=",", sep=";"))
# NHOW22Data <- CSVTOGGPLOT(read.table("22_NH4_OW.csv", header=TRUE, dec=",", sep=";"))
# NHPW22Data <- CSVTOGGPLOT(read.table("22_NH4_OW.csv", header=TRUE, dec=",", sep=";"))
# NHTW22Data <- CSVTOGGPLOT(read.table("22_NH4_TW.csv", header=TRUE, dec=",", sep=";"))
# 
# 
# ICPW21Data <- CSVTOGGPLOT(ICPW21)
# ICTW21Data <- CSVTOGGPLOT(ICTW21)
# NHPW21Data <- CSVTOGGPLOT(NHPW21)
# NHTW21Data <- CSVTOGGPLOT(NHTW21)
# 
# 
# ICOW22DataNO2<-ICOW22Data[[1]][ICOW22Data[[1]][,2]=="NO2.N",]
# ICOW22DataNO2[,2]="OW"
# ICPW22DataNO2<-ICPW22Data[[1]][ICPW22Data[[1]][,2]=="NO2.N",]
# ICPW22DataNO2[,2]="PW"
# ICTW22DataNO2<-ICTW22Data[[1]][ICTW22Data[[1]][,2]=="NO2.N",]
# ICTW22DataNO2[,2]="TW"
# 
# 
# ICPW21DataNO2<-ICPW21Data[[1]][ICPW21Data[[1]][,2]=="NO2.N",]
# ICPW21DataNO2[,2]="PW"
# ICTW21DataNO2<-ICTW21Data[[1]][ICTW21Data[[1]][,2]=="NO2.N",]
# ICTW21DataNO2[,2]="TW"
# 
# 
# ICOW22DataNO3<-ICOW22Data[[1]][ICOW22Data[[1]][,2]=="NO3.N",]
# ICOW22DataNO3[,2]="OW"
# ICPW22DataNO3<-ICPW22Data[[1]][ICPW22Data[[1]][,2]=="NO3.N",]
# ICPW22DataNO3[,2]="PW"
# ICTW22DataNO3<-ICTW22Data[[1]][ICTW22Data[[1]][,2]=="NO3.N",]
# ICTW22DataNO3[,2]="TW"
# 
# ICPW21DataNO3<-ICPW21Data[[1]][ICPW21Data[[1]][,2]=="NO3.N",]
# ICPW21DataNO3[,2]="PW"
# ICTW21DataNO3<-ICTW21Data[[1]][ICTW21Data[[1]][,2]=="NO3.N",]
# ICTW21DataNO3[,2]="TW"
# 
# 
# ICOW22DataPO4<-ICOW22Data[[1]][ICOW22Data[[1]][,2]=="Phosphat",]
# ICOW22DataPO4[,2]="OW"
# ICPW22DataPO4<-ICPW22Data[[1]][ICPW22Data[[1]][,2]=="Phosphat",]
# ICPW22DataPO4[,2]="PW"
# ICTW22DataPO4<-ICTW22Data[[1]][ICTW22Data[[1]][,2]=="Phosphat",]
# ICTW22DataPO4[,2]="TW"
# 
# ICPW21DataPO4<-ICPW21Data[[1]][ICPW21Data[[1]][,2]=="Phosphat",]
# ICPW21DataPO4[,2]="PW"
# ICTW21DataPO4<-ICTW21Data[[1]][ICTW21Data[[1]][,2]=="Phosphat",]
# ICTW21DataPO4[,2]="TW"
# 
