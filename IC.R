# Define the function to check and load packages
check_and_load_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}
# List of packages to check and load
packages_to_check <- c("ggplot2","dplyr","tidyr")
# Call the function with the list of packages
check_and_load_packages(packages_to_check)


# These two lines make the axis labels english.
Sys.setlocale("LC_TIME", "C")
strptime("Mon, 14 Mar 2011 23:42:16 GMT", format = "%a, %d %b %Y %H:%M:%S", tz="GMT")


# Graphic theme_sv
theme_sv<- function(){
  theme(
    plot.title = element_text(color = "#0099f9", face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0),
    axis.title.x = element_text(color = "#00008B", face = "bold"),
    axis.title.y = element_text(color = "#00008B", face = "bold"),
    axis.text.x.top = element_blank(), 
    axis.ticks.x.top = element_blank(),
    axis.title.x.top = element_blank(),
    axis.text=element_text( face="bold"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text=element_text(face="bold"),
    panel.background =  element_blank(),
    axis.line = element_line(colour = "black"),
    strip.text = element_text(face = "bold", color = "black"),
    strip.background = element_rect(fill = "white", colour = "white")
  )
}


#Funktionen Mittelwert und Standardabweichung für Tabellen aus Datenbank
MEAN<-function(df,amount){aggregate(df, list(rep(1:(nrow(df)/amount), each=amount)), mean)[,-1]}
STABW<-function(df,amount){aggregate(df, list(rep(1:(nrow(df)/amount), each=amount)), sd)[,-1]}

# Manipuliert eine Standardabfrage aus Access in das GGPLOT(long)-Format
CSVTOGGPLOT<-function(df){
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
  return(Data)
}

# Manipuliert den insgesamten Aufbau des Plots
GENERALPLOT<-function(data, plottitle,colnumber, yaxis, Standort){
  ggplot(data, aes(x = Datum, y = Value, shape = Dataset, color = Dataset)) +
    #geom_point(size = 1) + 
    geom_line(linewidth=0.7) +
    geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD), width = 0.2) +
    #geom_hline(data = data.frame(Dataset = names(hline_values), y = hline_values),
    #           aes(yintercept = y, color = Dataset), linetype = "dashed") +
    #geom_text(data = annotations, aes(label = Text), hjust = 1, vjust = 1, size =4) +  # Add this line
    scale_shape_manual(values = c(15,16, 17,18,19,20,0,1,2,3,4,5,6,8,9,13)) +  # Shapes: circle, triangle, square, diamond
    theme_sv()+
    ylim(0,NA)+
    
    #theme_minimal() + 
    labs(title = plottitle, subtitle= Standort, x = "", y = yaxis) +
    theme(legend.position = "none") +  # Remove legend  for simplicity
    facet_wrap(~ Dataset, ncol = colnumber, scales = "free_y")  # 2x2 grid of plots with free y-scales
}

# Liste mit den Plots für beide Standorte, Standort A und Standort B
# Eingabeargumente sind eine Datalist aus CSVTOGGPLOT, der Titel des Plots, die Anzahl der Spalten der Grafiken und die Y-Achsenbeschriftung
PLOTLIST<-function(datalist,plottitle,colnumber,yaxis){
  Plot_All<-GENERALPLOT(datalist[[1]],plottitle,colnumber, yaxis, "2022")
  Plot_A<-GENERALPLOT(datalist[[2]],plottitle,colnumber, yaxis, "Standort A")
  Plot_B<-GENERALPLOT(datalist[[3]],plottitle,colnumber, yaxis, "Standort B")
  
  
  Plots<-(list(Plot_All,Plot_A, Plot_B))
  names(Plots)<-c("All","A","B")
  return(Plots)
  
}
# Speichert die Plots als PNG mit dem Präfix PLOTNAME
SAVEPLOTS<-function(plotlist,Plotname){
  ggsave(paste0(Plotname, "_All.png"), plot = plotlist[[1]], width = 8 , height = 7)
  ggsave(paste0(Plotname, "_LocA.png"), plot = plotlist[[2]], width = 8 , height = 7)
  ggsave(paste0(Plotname, "_LocB.png"), plot = plotlist[[3]], width = 8 , height = 7)
}


setwd("C:/Users/Robin Burgold/tubCloud/shared1/Grafiken/All/IC/Neu")

#2022
ICOW22 <- (read.table("22_IC_OW.csv", header=TRUE, dec=",", sep=";"))
ICPW22 <- (read.table("22_IC_PW.csv", header=TRUE, dec=",", sep=";"))
ICTW22 <- (read.table("22_IC_TW.csv", header=TRUE, dec=",", sep=";"))
NHOW22 <- (read.table("22_NH4_OW.csv", header=TRUE, dec=",", sep=";"))
NHPW22 <- (read.table("22_NH4_OW.csv", header=TRUE, dec=",", sep=";"))
NHTW22 <- (read.table("22_NH4_TW.csv", header=TRUE, dec=",", sep=";"))

#2021
IC21 <- (read.table("21_PW.csv", header=TRUE, dec=",", sep=";"))
ICPW21<-IC21[ICPW21[,4]=="PW",]
ICTW21<-IC21[ICPW21[,4]=="TW",]


ICOW22Data<-CSVTOGGPLOT(ICOW22)
ICOW22Data[[1]][,2]

ICOWNO2.N22Data<-ICOW22Data[[1]][ICOW22Data[[1]][,2]=="NO2.N",]

       