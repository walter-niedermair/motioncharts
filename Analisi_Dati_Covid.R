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


Quelle.dati.PC  <- "http://www.provinz.bz.it/sicherheit-zivilschutz/zivilschutz/aktuelle-daten-zum-coronavirus.asp"

Quelle.dati.BZ  <- "https://github.com/abaumg/covid19-bz-scraper/blob/master/data/covid19_bz_municipalities.csv"

webpage.dati.BZ <- read_html(Quelle.dati.BZ)


#############################################################

# ANALISI DATI PROTEZIONE CIVILE - ANALYSE DATEN DER ZIVILSCHUTZ

#############################################################

dati.PC         <- read.csv(file=paste(DIR,"d","Corona.Data.Detail.csv",sep="/"),header=TRUE, sep=";")

# str(dati.PC) 
# unique(substr(dati.PC$istat,1,2))

#############################################################

# ESTRAZIONE DATI COMUNALI PROVINCIA DI BOLZANO PER CODICE ISTAT 
# DATENEXTRAKTION NACH ISTAT CODE DER GEMEINDEN DER PROVINZ BOZEN

#############################################################

new.dati.PC         <- subset(dati.PC,substr(dati.PC$istat,1,2)==21,select=c("istat","datum","positiv"))

# colnames(new.dati.PC)
# unique(new.dati.PC$istat)

new.dati.PC$datum   <- as.Date(new.dati.PC$datum)
new.dati.PC$positiv <- as.numeric(new.dati.PC$positiv)

#  str(new.dati.PC)

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


dati.Comuni.BZ            <- read.csv(file =paste(directorydati,"covid19_bz_municipalities.csv", sep="/"),header=TRUE, sep=",")

# str(dati.Comuni.BZ)
# unique(dati.Comuni.BZ$ISTAT_code)

dati.Comuni.BZ$ISTAT_code <- as.numeric(dati.Comuni.BZ$ISTAT_code)
dati.Comuni.BZ$datum      <- as.Date(dati.Comuni.BZ$datum)

############################################################

# ESTRAZIONE DATI COMUNALI PROVINCIA DI BOLZANO PER CODICE ISTAT
# DATENEXTRAKTION NACH ISTAT CODE DER GEMEINDEN DER PROVINZ BOZEN

############################################################

new.dati.Comuni.BZ        <- subset(dati.Comuni.BZ,substr(dati.Comuni.BZ$ISTAT_code,1,2)==21, select=c("ISTAT_code","datum","totals"))
new.dati.Comuni.BZ        <- subset(new.dati.Comuni.BZ, datum< as.Date("2020-12-18"))

# str(new.dati.Comuni.BZ)

new.dati.Comuni.BZ$datum  <- as.Date(new.dati.Comuni.BZ$datum)
setnames(new.dati.PC,c("istat","positiv"),c("ISTAT_code","totals"))

# colnames(new.dati.PC)
# colnames(new.dati.Comuni.BZ)

############################################################

# CREAZIONE NUOVO DATA.FRAME  COVID.DATA
# ERSCHAFFUNG DES NEUEN DATA.FRAME COVID.DATA

##########################################################

Covid.data            <- rbind(new.dati.Comuni.BZ,new.dati.PC)

# str(Covid.data) 

Covid.data$ISTAT_code <- as.numeric(Covid.data$ISTAT_code) 
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

new.Covid.data          <- subset(Covid.data,Covid.data$totals!="NA", select=c("datum","totals","nuovi_contagi"))

# str(new.Covid.data)

##########################################################

# ESTRAZIONE DATI, RIMOZIONE OUTLIERS E RAPPRESENTAZIONE GRAFICA 
# CONTAGI PER COMUNE
# DATENEXTRAKTION, AUSREISSERENTFERNUNG, GRAFISCHE DARSTELLUNG 
# DER INFEKTIONEN NACH GEMEINDEN

##########################################################

comune                 <- 21008 # scegliere valore codice istat / wählen einen Wert der Istat Code
pippo                  <- subset(Covid.data, ISTAT_code == comune, select = c("datum","nuovi_contagi"))
plot(pippo, type= "p")
grafico.pippo          <- barplot(pippo$nuovi_contagi, main= "Nuovi contagi giornalieri", xlab= "Giorno", ylab="Nuovi_casi", names.arg = pippo$datum, col="red") 

#################################################################

# FOR LOOP PER SELEZIONARE I DATI NELLA STESSA LINGUA
# FOR LOOP UM DIE DATEN IN DERSELBEN SPRACHE EINZULESEN

#################################################################

Lingua                <- "Deutsch"
sheetsXLS             <- c('Comuni', 'Com_AggrDimora', 'Com_AggrDimora_DC', 'Com_AggrASDimora', 'Com_AggrPAFDimora')

for(i in sheetsXLS){
  pluto               <- read.xlsx(paste(directorydati, 'geo--comuni.xlsx',sep="/"), sheet = i)
  pluto               <- subset(pluto,Sys_Lingua==Lingua)
  pluto               <- pluto[!names(pluto)=="Sys_Lingua"]
  assign(paste0('GEM_', i), pluto)
  rm(pluto)
}

pluto                 <- read.xlsx(paste(directorydati, 'geo--comuni.xlsx',sep="/"), sheet = "Comuni")
pluto$Chiave          <- as.numeric(pluto$Chiave)

Label.long            <- read.xlsx(paste(directorydati, 'geo--comuni.xlsx',sep="/"), sheet = "Label")
setDT(Label.long)

Label.wide            <- melt(Label.long, id.vars = "Chiave",variable.name = "Sys_Lingua", value.name = "Descrizione")
setDT(Label.wide)

Label                 <- subset(Label.wide,Sys_Lingua==Lingua, select=c("Chiave","Descrizione"))

#################################################################

# CREAZIONE GRAFICO
# ERSTELLUNG DER GRAPHIK

#################################################################

codice.istat          <- sort(unique(Covid.data$ISTAT_code))


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

####################################################################

# IMPORT DATI STAZIONI METEO PER COMUNE
# IMPORT DATEN WETTER STATIONEN AUF GEMEINDE EBENE

####################################################################

Wetter.station        <-    read.xlsx(paste(directorydati, 'geo--comuni.xlsx',sep="/"), sheet = "Wetter_station")

####################################################################

# DOWNLOAD DATI STAZIONI METEO
# HERUNTERLADUNG DATEN DER WETTER STATIONEN 

####################################################################

Quelle.wetter.station  <-  "http://daten.buergernetz.bz.it/services/meteo/v1/stations"

#### CHECK DATEN ALLER WETTER_STATIONEN AUS DER WEBSEITE http://daten.buergernetz.bz.it


# Stazioni.meteo<-jsonlite::fromJSON("http://daten.buergernetz.bz.it/services/meteo/v1/stations")
# Stazioni.meteo<-Stazioni.meteo$features$properties
# str(Stazioni.meteo)
# codice.stazione        <- Stazioni.meteo$SCODE

####################################################################

## DOWNLOAD METADATI SENSORI [LT,N,SD] PER ID_STAZIONE PERIODO DI OSSERVAZIONE: 01-01-2020 FINO AD OGGI 
## IMPORT DER SENSOREN [LT,N,SD] NACH STATION_CODE ZEITINTERVAL: 01-01-2020 BIS HEUTE


## LEGEND: 
## LT = Temperatura dell'aria in °C / Lufttemperatur in °C
## N  = Pioggia in mm /Niederschlag in mm
## SD = tempo di soleggiamento in secondi / Sonnenscheindauer in Sekunden
####################################################################


sensoren   <- c("LT","N","SD") 
stazione   <- unique(Wetter.station$Wetter_station) 

fileToSave <- paste(directorydati,"meteodaten1.rds",sep="/")

if (!file.exists(fileToSave)) {
  
  for (meteostation in stazione){
    print(sprintf("momentan mache ich %s/%s Wetterstation: %s" ,match(meteostation,stazione),NROW(stazione),meteostation)) # print nummer der Wetter Station, die ausgedr
    
    for (sensore_code in sensoren) {
      print(sprintf("momentan macht R %s", sensore_code))
      pippo      <- jsonlite::fromJSON(sprintf("http://daten.buergernetz.bz.it/services/meteo/v1/timeseries?station_code=%s&sensor_code=%s&date_from=20200101&date_to=20210408",meteostation,sensore_code))
      setDT(pippo)
      pippo$DATE <- as.Date(pippo$DATE)
      if (NROW(pippo)!=0) {
        if (sensore_code == "LT") {pippo <- pippo[,list(LT=mean(VALUE)),by=c("DATE")] ; pluto <- pippo }
        if (sensore_code == "N" ) {pippo <- pippo[,list( N=sum(VALUE)) ,by=c("DATE")] ; pluto <- merge(pluto,pippo,by="DATE")}
        if (sensore_code == "SD") {pippo <- pippo[,list(SD=sum(VALUE)) ,by=c("DATE")] ; pluto <- merge(pluto,pippo,by="DATE")}
      } else { # if (NROW(pippo) == 0)
        if (sensore_code == "LT") {pippo <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("DATE", "LT")) ; pluto <- pippo}
        if (sensore_code == "N" ) {pippo <- setNames(data.frame(matrix(ncol = 1, nrow = 0)), c("N"))  ; pluto <- cbind(pluto,pippo)}
        if (sensore_code == "SD") {pippo <- setNames(data.frame(matrix(ncol = 1, nrow = 0)), c("SD")) ; pluto <- cbind(pluto,pippo)}
      }
    } 
    
    pluto$stazione   <- meteostation
    if (meteostation == stazione[1]) meteodaten <- pluto else meteodaten <- rbind(meteodaten,pluto) 
  }  
  
}


####################################################################

##  SALVATAGGIO OUTPUT FOR LOOP "METEODATEN" NEL FILE "meteodaten2.rds"
##  OUTPUTSPEICHERUNG DER SCHLEIFE "METEODATEN" IN DEM FILE "meteodaten2.rds"

####################################################################

saveRDS(meteodaten, file = fileToSave)

####################################################################

# LETTURA FILE meteodaten2.rds   
# FILE meteodaten2.rds EINLESEN 

####################################################################

dati.meteo      <- readRDS(file = paste(directorydati,"meteodaten2.rds",sep="/"))
dati.meteo$SD   <- floor(dati.meteo$SD/3600) 
dati.meteo$DATE <- as.Date(dati.meteo$DATE)

####################################################################

# DEFINIZIONE GIORNATA PIOVOSA
# E'considerato giorno di pioggia, il giorno in cui il livello di precipitazione 
# nell'arco di 24 ore è pari o maggiore a 0,1 mm (equivalente a 0,1 l/m²)


# REGENTAG DEFINITION
# Ein Regentag ist ein Tag mit einer 24-stündigen gemessenen Regenhöhe 
# größer/gleich 0,1 mm (entspricht 0,1 l/m²)

# CREAZIONE NUOVA COLONNA GIORNO DI PIOGGIA
# DEFINITION EINER NEUEN SPALTE REGENTAG 

####################################################################

dati.meteo$Regentag <- 0
dati.meteo <- within(dati.meteo,Regentag[N>= 0.1]<-1)

# head(dati.meteo)

plot(dati.meteo$DATE,dati.meteo$Regentag, type ="p")

table(dati.meteo$Regentag)
plot(table(dati.meteo$N))

#################################################################

# IMPORT FILE METEODATEN2.RDS
# IMPORT FILE METEODATEN2.RDS

#################################################################

meteodaten2          <- readRDS(paste(directorydati,"meteodaten2.rds",sep="/"))
meteodaten2$Regentag <- 0
meteodaten2          <- within(meteodaten2,Regentag[N>=0.1]<-1)
meteodaten2$SD       <- floor(meteodaten2$SD/3600)

##################################################################

# CREAZIONE UNICO DATASET DEI DATI NUOVI_CONTAGI E DATI.METEO PER COMUNE
# DATASET ERSTELLUNG MIT NEUEN FAELLEN UND METEODATEN AUF GEMEINDEBENE

##################################################################

# PRIMO MERGE TRA METEODATEN.RDS E STAZIONI METEO
# ERSTE VERKNÜPFUNG ZWISCHEN METEODATEN.RDS UND WETTER.STATION

##################################################################
# colnames(meteodaten2)
# colnames(Wetter.station)

Verknuepfung1       <- merge(Wetter.station,meteodaten2, by.x = "Wetter_station", by.y="stazione", allow.cartesian = TRUE)
setDT(Verknuepfung1)

# setDT(Wetter.station)
# setkey(Wetter.station,Wetter_station)


########################################################################

# SECONDO MERGE CON COVID.DATA
# ZWEITE VERKNÜPFUNG MIT COVID.DATA

########################################################################

# colnames(Verknüpfung1)
# colnames(Covid.data)

# str(Verknüpfung1)
# str(Covid.data)

Covid.data$datum            <- as.Date(Covid.data$datum)
Verknuepfung1$DATE          <- as.Date(Verknuepfung1$DATE)

Covid.data$ISTAT_code       <- as.numeric(Covid.data$ISTAT_code)
Verknuepfung1$Chiave        <- as.numeric(Verknuepfung1$Chiave)  


Verknuepfung2               <- merge(Covid.data,Verknuepfung1,all.x=TRUE, by.x= c("datum","ISTAT_code"), by.y= c("DATE","Chiave"))
setDT(Verknuepfung2)

####################################################################

# ERSTELLUNG DER SPALTE KALENDARWOCHE

####################################################################

Verknuepfung2$KW                    <- isoweek(Verknuepfung2$datum) ## nummer der Kalenderwoche nach 

Verknuepfung2$JJ                    <- year(Verknuepfung2$datum)

Verknuepfung2$KW2                   <- round(difftime(Verknuepfung2$datum, as.Date("2020-03-18"), units = "weeks"))


Verknuepfung3                       <- Verknuepfung2[,list(nuovi_contagi=sum(nuovi_contagi),Regentag=sum(Regentag), LT=mean(LT),SD=mean(SD)),by=c("KW2","ISTAT_code","DescrizioneDimora_DC","Com_AggrPAF")]
setDT(Verknuepfung3)


model.no_LAG                        <- lm(nuovi_contagi~ Regentag, data=Verknuepfung3)
summary(model.no_LAG)


#####################################################################################

# CREAZIONE DELLA COLONNA REGENTAG1 E MODELLO CON LAG 1 (SETTIMANA PRECEDENTE)
# ERSTELLUNG DER SPALTE REGENTAG1 UND MODEL MIT LAG 1 WOCHE VOR

#####################################################################################

Verknuepfung3[,Regentag.lag1:=c(NA, Regentag[-.N]), by="KW2"]
Verknuepfung3$Regentag.lag1 [is.na(Verknuepfung3$Regentag.lag1)] <- 0
Verknuepfung3$Regentag1                                          <- Verknuepfung3$Regentag - Verknuepfung3$Regentag.lag1


model.lag1                                                       <- lm(nuovi_contagi~ Regentag+Regentag1 ,data=Verknuepfung3)
summary(model.lag1)

#####################################################################################

# CREAZIONE DELLA COLONNA REGENTAG2 E MODELLO CON LAG 2 (SETTIMANE PRECEDENTI)
# ERSTELLUNG DER SPALTE REGENTAG2 UND MODEL MIT LAG 2 WOCHEN VOR

#####################################################################################

Verknuepfung3[,Regentag.lag2 :=c(NA,Regentag1[-.N]),by="KW2"]
Verknuepfung3$Regentag.lag2 [is.na(Verknuepfung3$Regentag.lag2)] <- 0
Verknuepfung3$Regentag2                                          <- Verknuepfung3$Regentag1 - Verknuepfung3$Regentag.lag2

model.lag2                                                       <- lm(nuovi_contagi ~ Regentag + Regentag2 ,data=Verknuepfung3)
summary(model.lag2)
