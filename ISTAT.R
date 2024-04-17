# Laden der Pakete ---

if (!require("rsdmx"))      install.packages("rsdmx")      ; library(rsdmx)

if (!require("data.table")) install.packages("data.table") ; library(data.table)

### Daten abrufen ---
# Definiere die URL des SDMX-Datensatzes

urldata <- "https://astatsdmxservices.prov.bz.it/dsm/NSI_WS/rest/data/ITH1,DF_TOUR_ACCOMODATION_CAPACITY_1,1.0/A.CALENDER_YEAR.ADMINISTRATIVE_UNIT..TOTAL.ARRIVALS_N+OVERNIGHTS_N/ALL/?detail=full&dimensionAtObservation=TIME_PERIOD"
# Rufe den SDMX-Datensatz ab
data <- readSDMX(urldata)
# Zeige die Struktur der abgerufenen Daten an
str(data)
# Konvertiere die Daten in einen DataTable
dt <- as.data.table(data)

# creare una tabella con colonne gem, year, arrivi, presenze
?dcast
dcast(dt, formula = TOUR_GEO+obsTime ~ INDICATOR, value.var = "obsValue")
paperino <- dcast(dt, formula = TOUR_GEO+obsTime ~ INDICATOR, value.var = "obsValue")
print(paperino)

#solo con datatable, dataframe vuole la virgola --> per entrambi funzione subset(DF,CONDIITON: SUBSTR(), SELECT COLONNE O RIGHE)
paperino[substr(paperino$TOUR_GEO, 1,3) == "021"] 
#SELEZIONARE CON DATATABLE O DATAFRAME 
paperino[1]
paperino[,1]
# tabella definitiva pulita 
paperone <- paperino[substr(paperino$TOUR_GEO, 1,3) == "021"] 
paperone[,1]
paperone["TOUR_GEO"]
paperone[,"TOUR_GEO"]


paperone1 <- paperino[!substr(paperino$TOUR_GEO, 1,3) == "021"]
paperone1[,"TOUR_GEO"]
rm(paperone1)

?set.seed
??DataFrame.loc
#Virgola!
paperone[,Gastone:=TOUR_GEO]
paperone[,TOUR_GEO:=substr(TOUR_GEO,4,6)]
paperone$Gastone <- NULL
setnames(paperone, c("TOUR_GEO","obsTime","ARRIVALS_N","OVERNIGHTS_N"), c("gem","year", "arrivi","presenze"))
View(paperone)
library(openxlsx)
paperoga <- read.xlsx("C:/Users/anton/Documents/motioncharts/d/DatiComunaliExportDaQV.xlsx")
write.xlsx(paperoga,"C:/Users/anton/Documents/motioncharts/d/DatiComunaliExportDaQV.xlsx")
View(paperoga)
merge(paperoga, paperone, by = c("gem","year"))
etabeta <-(merge(paperoga, paperone, by = c("gem","year")))

#Attenzione a convertire i dataframe in datatable dopo 
paperone[,gem:=as.numeric(gem)]
paperoga[,gem:= as.numeric(gem)]
etabeta <- merge(paperoga, paperone, by = c("gem","year"), all.y = TRUE)
setDT(etabeta)
etabeta [,DIFF:= arrivi.y - arrivi.x]
etabeta[,gem:=as.numeric(gem)]
etabeta[,year:=as.numeric(year)]
str(etabeta)
View(etabeta)


# Load necessary packages
library(readxl)
library(openxlsx)
library(data.table)
library(rsdmx)
# Read the source workbook
etabeta <- read.xlsx("C:/Users/anton/Documents/motioncharts/etabeta.xlsx")
setDT(etabeta)
max(etabeta$year)
min(etabeta$year)
colnames(etabeta)
View(etabeta)

# Read the target workbook
DatiComunaliExportDaQV <- read.xlsx("C:/Users/anton/Documents/motioncharts/DatiComunaliExportDaQV.xlsx")
setDT(DatiComunaliExportDaQV)
max(DatiComunaliExportDaQV$year)
min(DatiComunaliExportDaQV$year)


#import data population from Istat
urldata2 <- "https://esploradati.istat.it/SDMXWS/rest/data/IT1,22_289_DF_DCIS_POPRES1_25,1.0/A..JAN.9.TOTAL.99/ALL/?detail=full&startPeriod=2019-01-01&endPeriod=2024-12-31&dimensionAtObservation=TIME_PERIOD"
mercedes <- readSDMX(urldata2)
#setDT(prova) --> non è un'alternativa al creare un datatable, vale solo se convertito da dataframe
dt <- as.data.table(mercedes)
#ricorda che puoi fare tutto in uno step: filtro, selezione colonne ++ ultimo argomento sono possibilmente i criteri che vengono richiesti 
sonic <- dt [substr(dt$REF_AREA,1,3) == "021", c("REF_AREA", "obsValue", "obsTime")]
View(sonic)


# ID and obsValue 
?mean
#rinominare una variabili a seconda delle operazioni !!!! ricorda list() +++ ricorda primo posto *criterio
sonic [, list (varianza = var(obsValue), media = mean(obsValue), apice = max(obsValue), minimo = min(obsValue), somma = sum(obsValue)), by = REF_AREA]
sonic [,  list (media = mean(obsValue)), by = REF_AREA]

#rename the columns 
setnames(sonic, c("REF_AREA","obsTime","obsValue"), c ("gem","year", "pop"))
View(sonic)
sonic[,gem:=as.numeric(substr(gem,4,6))]
etabeta[,gem:=as.numeric(substr(gem,1,3))]
View(etabeta)

is.data.table(etabeta) == TRUE
is.data.table(sonic) == TRUE

str(sonic)
sonic[,year:=as.numeric(year)]
Richard <- merge(etabeta, sonic, by = c("gem","year"), all.x = TRUE)
str(Richard)
View(Richard)


directorymain <- getwd()
print(directorymain)
write.xlsx(Richard, paste(directorymain, 'd','DatiComunaliExportDaQV.xlsx',sep="/")) 



