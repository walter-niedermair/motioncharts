#############################################################

## zoo and xts - na.locf
# https://campus.datacamp.com/courses/manipulating-time-series-data-with-xts-and-zoo-in-r/merging-and-modifying-time-series?ex=6
#
## imputeTS - na.mean
# https://cran.r-project.org/web/packages/imputeTS/imputeTS.pdf
# https://stackoverflow.com/questions/9322773/how-to-replace-na-with-mean-by-group-subset


#############################################################
# packages and libraries
#############################################################

if (!require("data.table")) install.packages("data.table") ; library (data.table)
if (!require("openxlsx"))   install.packages("openxlsx")   ; library (openxlsx)
if (!require("stringr"))    install.packages("stringr")    ; library (stringr)
if (!require("Cairo"))      install.packages("Cairo")      ; library (Cairo)
if (!require("zoo"))        install.packages("zoo")        ; library (zoo)
if (!require("xts"))        install.packages("xts")        ; library (xts)
if (!require("imputeTS"))   install.packages("imputeTS")   ; library (imputeTS)
if (!require("rvest"))      install.packages("rvest")      ; library (rvest)
if (!require("jsonlite"))   install.packages("jsonlite")   ; library (jsonlite)



#############################################################
# define directories
#############################################################

getwd()
DIR<-getwd()
directorydati<-paste(DIR,"d",sep="/")

#############################################################

 # IMPORTING THE DATA

 # 1)dati Covid giornalieri Protezione Civile - Daten zum täglichen Covidfällen Zivilschutz Südtiroler Landesverwaltung
 # 2)dati Covid giornalieri Provincia di Bolzano  - Daten zum täglichen Covidfällen auf Gemeindebene

#############################################################


Quelle.dati.PC<- "http://www.provinz.bz.it/sicherheit-zivilschutz/zivilschutz/aktuelle-daten-zum-coronavirus.asp"

Quelle.dati.BZ<- "https://github.com/abaumg/covid19-bz-scraper/blob/master/data/covid19_bz_municipalities.csv"
  
webpage.dati.BZ<-read_html(Quelle.dati.BZ)


#############################################################

# ANALISI DATI PROTEZIONE CIVILE - ANALYSE DATEN DER ZIVILSCHUTZ

#############################################################

dati.PC<-read.csv(file=paste(DIR,"d","Corona.Data.Detail.csv",sep="/"),header=TRUE, sep=";")
str(dati.PC) 
unique(substr(dati.PC$istat,1,2))

#############################################################

# ESTRAZIONE DATI COMUNALI PROVINCIA DI BOLZANO PER CODICE ISTAT 
# DATENEXTRAKTION NACH ISTAT CODE DER GEMEINDEN DER PROVINZ BOZEN

#############################################################

new.dati.PC <- subset(dati.PC,substr(dati.PC$istat,1,2)==21,select=c("istat","datum","positiv"))
colnames(new.dati.PC)

unique(new.dati.PC$istat)

new.dati.PC$datum<-as.Date(new.dati.PC$datum)
new.dati.PC$positiv<-as.numeric(new.dati.PC$positiv)
str(new.dati.PC)

plot(new.dati.PC$datum,new.dati.PC$positiv)


#############################################################

# DATI PROTEZIONE CIVILE CON VALORI MANCANTI  
# ZIVILSCHUTZ DATEN MIT FEHLENDEN INFORMATIONEN

#############################################################

null.data<-subset(new.dati.PC,is.na(new.dati.PC$positiv))

#############################################################

### INTERPOLAZIONE LINEARE DEI DATI MANCANTI /
### LINEARE INTERPOLATION DER FEHLENDEN DATEN

#############################################################

setDT(new.dati.PC)
new.dati.PC[, positiv := na_interpolation(positiv, option = "linear"), by = "istat"]

#############################################################

# ANALISI DATI COMUNALI BOLZANO/ 
# ANALYSE DER DATEN ZUM TÄGLICHEN COVIDFÄLLE AUF GEMEINDEBENE

#############################################################


dati.Comuni.BZ<-read.csv(file =paste(directorydati,"covid19_bz_municipalities.csv", sep="/"),header=TRUE, sep=",")
str(dati.Comuni.BZ)

unique(dati.Comuni.BZ$ISTAT_code)

dati.Comuni.BZ$ISTAT_code<-as.numeric(dati.Comuni.BZ$ISTAT_code)
dati.Comuni.BZ$datum <- as.Date(dati.Comuni.BZ$datum)

############################################################

# ESTRAZIONE DATI COMUNALI PROVINCIA DI BOLZANO PER CODICE ISTAT
# DATENEXTRAKTION NACH ISTAT CODE DER GEMEINDEN DER PROVINZ BOZEN

############################################################

new.dati.Comuni.BZ <- subset(dati.Comuni.BZ,substr(dati.Comuni.BZ$ISTAT_code,1,2)==21, select=c("ISTAT_code","datum","totals"))
new.dati.Comuni.BZ <- subset(new.dati.Comuni.BZ, datum< as.Date("2020-12-18"))
str(new.dati.Comuni.BZ)

new.dati.Comuni.BZ$datum<-as.Date(new.dati.Comuni.BZ$datum)

setnames(new.dati.PC,c("istat","positiv"),c("ISTAT_code","totals"))
colnames(new.dati.PC)
colnames(new.dati.Comuni.BZ)

############################################################

# CREAZIONE NUOVO DATA.FRAME  COVID.DATA
# ERSCHAFFUNG DES NEUEN DATA.FRAME COVID.DATA

##########################################################

Covid.data <- rbind(new.dati.Comuni.BZ,new.dati.PC)

str(Covid.data) 
Covid.data$ISTAT_code<-as.numeric(Covid.data$ISTAT_code) 
setDT(Covid.data)

##########################################################

# AGGIUNTA NUOVE COLONNE LAG.VALUE E NUOVI CONTAGI
# ZUSATZ DER NEUEN SPALTEN LAG.VALUE UND NEUE COVIDFÄLLE

##########################################################

Covid.data[, lag.value:=c(NA, totals[-.N]), by="ISTAT_code"]
Covid.data$lag.value[is.na(Covid.data$lag.value)]<-0
Covid.data$nuovi_contagi <- Covid.data$totals- Covid.data$lag.value

##########################################################

# new.data.set Covid.data = without NA

##########################################################
new.Covid.data<-subset(Covid.data,Covid.data$totals!="NA", select=c("datum","totals","nuovi_contagi"))
str(new.Covid.data)

##########################################################

# ESTRAZIONE DATI, RIMOZIONE OUTLIERS E RAPPRESENTAZIONE GRAFICA 
# CONTAGI PER COMUNE
# DATENEXTRAKTION, AUSREISSERENTFERNUNG, GRAFISCHE DARSTELLUNG 
# DER INFEKTIONEN NACH GEMEINDEN

##########################################################

comune <-21008 # scegliere valore codice istat / wählen einen Wert der Istat Code

pippo <- subset(Covid.data, ISTAT_code == comune, select = c("datum","nuovi_contagi"))
plot(pippo, type= "p")

pippo.outliers <- identify(pippo, n=2)  
pippo[-c(pippo.outliers),]

plot(pippo[-c(pippo.outliers)], type= "p")
plot(pippo[-c(pippo.outliers)],type="l")

grafico.pippo<-barplot(pippo$nuovi_contagi[-c(pippo.outliers)], main= "Nuovi contagi giornalieri", xlab= "Giorno", ylab="Nuovi_casi", names.arg = pippo$datum, col="red") 

#################################################################

# FOR LOOP PER SELEZIONARE I DATI NELLA STESSA LINGUA
# FOR LOOP UM DIE DATEN IN DERSELBEN SPRACHE EINZULESEN

#################################################################

Lingua<-"Deutsch"
sheetsXLS <- c('Comuni', 'Com_AggrDimora', 'Com_AggrDimora_DC', 'Com_AggrASDimora', 'Com_AggrPAFDimora',"Label")

for(i in sheetsXLS){
  pluto <- read.xlsx(paste(directorydati, 'geo--comuni.xlsx',sep="/"), sheet = i)
  pluto <- subset(pluto,Sys_Lingua==Lingua)
  pluto <- pluto[!names(pluto)=="Sys_Lingua"]
  assign(paste0('GEM_', i), pluto)
  rm(pluto)
}

pluto <- read.xlsx(paste(directorydati, 'geo--comuni.xlsx',sep="/"), sheet = "Comuni")
pluto$Chiave <- as.numeric(pluto$Chiave)

Label.long <- read.xlsx(paste(directorydati, 'geo--comuni.xlsx',sep="/"), sheet = "Label")
setDT(Label.long)

Label.wide <-melt(Label.long, id.vars = "Chiave",variable.name = "Sys_Lingua", value.name = "Descrizione")
setDT(Label.wide)

Label <- subset(Label.wide,Sys_Lingua==Lingua, select=c("Chiave","Descrizione"))

#################################################################

# CREAZIONE GRAFICO
# ERSTELLUNG DER GRAPHIK

#################################################################

codice.istat <- sort(unique(Covid.data$ISTAT_code))


CairoPDF("test.pdf",width = 10, height = 14)
par(mfrow=c(2,1))

for (comune in codice.istat){
  pippo <- subset(Covid.data,ISTAT_code==comune,select= c("datum","totals","nuovi_contagi"))
  plot(pippo$datum,          
       pippo$nuovi_contagi,  
       col="red",            
       type= "l",            
       lty=1,               
       lwd=1,                
       xaxt="n",             
       ylim=range(pippo$nuovi_contagi), 
       frame.plot = FALSE,
       xlab =Label[Chiave=="xdesc"]$Descrizione,     
       ylab =Label[Chiave=="ydesc"]$Descrizione
       )
  title(main=c(Label[Chiave=="main.title"]$Descrizione, pluto[pluto$Sys_Lingua==Lingua& pluto$Chiave==comune,c("Descr_shortDimora")]))
       axis.Date(1, at = seq(pippo$datum[1], pippo$datum[length(pippo$datum)], by="month"),pos=0,
       labels= seq(pippo$datum[1], pippo$datum[length(pippo$datum)], by="month"),
                 format="%Y-%m", las = 0)
       
  plot(pippo$datum,
       pippo$totals,
       col="red",
       type="h",
       xaxt="n", 
       ylim=range(pippo$totals),
       frame.plot= FALSE,
       xlab= Label[Chiave=="xdesc"]$Descrizione,
       ylab= Label[Chiave=="ydesc"]$Descrizione)
       
       title(main=c(Label[Chiave=="main.title2"]$Descrizione, pluto[pluto$Sys_Lingua==Lingua& pluto$Chiave==comune,c("Descr_shortDimora")]))
       
       axis.Date(1, at = seq(pippo$datum[1], pippo$datum[length(pippo$datum)], by="month"),pos=0,
                 labels= seq(pippo$datum[1], pippo$datum[length(pippo$datum)], by="month"),
                 format="%Y-%m", las = 0)
}
dev.off()
       
## Chiave e Wetter station

Wetter.station<-read.xlsx(paste(directorydati, 'geo--comuni.xlsx',sep="/"), sheet = "Wetter_station")
View(Wetter.station)

# Wetter stationen

Stazioni.meteo<-jsonlite::fromJSON("http://daten.buergernetz.bz.it/services/meteo/v1/stations")
Stazioni.meteo<-Stazioni.meteo$features$properties
str(Stazioni.meteo)
View(Stazioni.meteo)

codice.stazione<-Stazioni.meteo$SCODE

# Import der Sensoren [LT,N,SD] nach Station_code Zeitintervall: 01-01-2020 bis heute # Frag Walter ob ich ein Doppel egal einsetzen soll

sensoren <- c("LT","N","SD") #3 Sensoren
stazione <- Wetter.station$Wetter_station #43 Wetterstationen

for (stazione in codice.stazione){
  
  for (sensore_code in sensoren) {
    pippo <- jsonlite::fromJSON(sprintf("http://daten.buergernetz.bz.it/services/meteo/v1/timeseries?station_code=%s&sensor_code=%s&date_from=20200101&date_to=20210407",stazione,sensore_code))
    setDT(pippo)# DATE VALUE
    pippo$DATE <- as.Date(pippo$DATE)
    if (sensore_code == "LT") pippo[,list(LT=mean(VALUE)),by=c("DATE")] ; pluto <- pippo
    if (sensore_code == "N" ) pippo[,list( N=sum(VALUE)) ,by=c("DATE")] ; pluto <- merge(pluto,pippo,by="DATE")
    if (sensore_code == "SD") pippo[,list(SD=sum(VALUE)) ,by=c("DATE")] ; pluto <- merge(pluto,pippo,by="DATE")
    # data.table mit diesen Spalen  DATE | LT | N | SD
  } 
  
  # hinzufügen der Spalte "stazione"
  pluto$stazione <-  stazione
  # data.table mit diesen Spalen  DATE | LT | N | SD | stazione
  if (stazione == codice.stazione[1]) minnie <- pluto else minnie <- rbind(minnie,pluto)

}

# Wetterstation Bozen Wetterstation Bozen ID_station "83200MS"

# stazione<-"83200MS"
for (stazione in codice.stazione){
  LT<-jsonlite::fromJSON("http://daten.buergernetz.bz.it/services/meteo/v1/timeseries?station_code=stazione&sensor_code=LT&date_from=20200101&date_to=20210407")
  N<-jsonlite::fromJSON("http://daten.buergernetz.bz.it/services/meteo/v1/timeseries?station_code=stazione&sensor_code=N&date_from=20200101&date_to=20210407")
  SD<-jsonlite::fromJSON("http://daten.buergernetz.bz.it/services/meteo/v1/timeseries?station_code=stazione&sensor_code=LD&date_from=20200101&date_to=20210407")
}



BZ.LT<-jsonlite::fromJSON("http://daten.buergernetz.bz.it/services/meteo/v1/timeseries?station_code=83200MS&sensor_code=LT&date_from=20200101&date_to=20210407")
BZ.N<-jsonlite::fromJSON("http://daten.buergernetz.bz.it/services/meteo/v1/timeseries?station_code=83200MS&sensor_code=N&date_from=20200101&date_to=20210407")
BZ.SD<-jsonlite::fromJSON("http://daten.buergernetz.bz.it/services/meteo/v1/timeseries?station_code=83200MS&sensor_code=SD&date_from=20200101&date_to=20210407")

setDT(BZ.LT)
setDT(BZ.N)
setDT(BZ.SD)


View(BZ.LT)
View(BZ.N)
View(BZ.SD)


Sensoren.BZ<-merge(BZ.LT,BZ.SD, by="DATE") 
View(Sensoren.BZ)
  
# Error in merge.data.table(BZ.LT, BZ.N, BZ.SD, by = "DATE") : 
# by.x` and `by.y` must be of same length.

# 
ID_Station<-Wetter.station$Wetter_station

for (Gemeinde in ID_Station){
  Wetter.station<-read.xlsx(paste(directorydati, 'geo--comuni.xlsx',sep="/"), sheet = "Wetter_station")
  Wetter.station<-subset(Wetter.station,Wetter.station$Wetter_station==Gemeinde)
 # Wetter.station$LF<-
 # Wetter.station$N<-
 # Wetter.station$SD
}


# FRAGEN AN WALTER

# 1) Wie wird besser die Uhr bestimmt?
# 2) soll ich ein doppeles Gleichheitszeichnen verwenden?





