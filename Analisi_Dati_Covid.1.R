if (!require("openxlsx"))   install.packages("openxlsx")   ; library (openxlsx)
if (!require("data.table")) install.packages("data.table") ; library (data.table)
if (!require("stringr"))    install.packages("stringr")    ; library (stringr)


getwd()
DIR<-getwd()
directorydati<-"C:/Users/nicol/Desktop/PROGETTO/motioncharts/d"


# file Covid Protezione Civile

dati.PC<-read.csv(file ="C:/Users/nicol/Desktop/PROGETTO/motioncharts/d/Corona.Data.Detail.csv",header=TRUE, sep=";")
setDT(dati.PC)
str(dati.PC)
View(dati.PC)

# dati.out<-which(dati.PC$nome=="Fuori provincia")

dati.PC <- dati.PC[-c(1:95),]
View(dati.PC)

dati.PC$istat<-factor(dati.PC$istat)
dati.PC$iddat<-as.numeric(dati.PC$iddat)
dati.PC$datum <- as.POSIXct(dati.PC$datum)
dati.PC$name<-factor(dati.PC$name)
dati.PC$nome<-factor(dati.PC$nome)
dati.PC$positiv<-as.numeric(dati.PC$positiv)
dati.PC$antigen<-as.numeric(dati.PC$antigen)
dati.PC$pcr.pos.geh..tote<-as.numeric(dati.PC$pcr.pos.geh..tote)
dati.PC$quarantaene<-as.numeric(dati.PC$quarantaene)
dati.PC$sumPos.pcr.antigen.<-as.numeric(dati.PC$sumPos.pcr.antigen.)
dati.PC$pcr.pos.geh..antigen.pos.geh..tote<-as.numeric(dati.PC$pcr.pos.geh..antigen.pos.geh..tote)

setnames(dati.PC,c("istat","name","nome","positiv"),c("ISTAT_code","name_DE","name_IT","totals"))

View(dati.PC)

# NON FUNZIONA

# dati.PC$datum <- as.Date.POSIXct(dati.PC$datum, tryFormats = "%Y/%m/%d")


dati.Comuni.BZ<-read.csv(file ="C:/Users/nicol/Desktop/PROGETTO/motioncharts/d/covid19_bz_municipalities.csv",header=TRUE, sep=";")
setDT(dati.Comuni.BZ)
str(dati.Comuni.BZ)
View(dati.Comuni.BZ)

dati.Comuni.BZ$ISTAT_code<-as.numeric(dati.Comuni.BZ$ISTAT_code)
dati.PC$ISTAT_code<-as.numeric(dati.PC$ISTAT_code)

dati.Comuni.BZ$datum <- as.Date(dati.Comuni.BZ$datum)
dati.Comuni.BZ <-subset(dati.Comuni.BZ,dati.Comuni.BZ$ISTAT_code== c(
                          "21001",  "21002",  "21003",  "21004",  "21005",  "21006",
                          "21007",  "21008",  "21009",  "21010",  "21011",  "21012",  "21013",  "21014",  "21015",  "21016",
                          "21017",  "21018",  "21019",  "21020",  "21021",  "21022",  "21023",  "21024",  "21025",  "21026",
                          "21027",  "21028",  "21029",  "21030",  "21031",  "21032",  "21033",  "21034",  "21035",  "21036",
                          "21037",  "21038", "21039",  "21040" , "21041", "21042",  "21043" , "21044",  "21045",  "21046",
                          "21047",  "21048",  "21049",  "21050",  "21051",  "21052",  "21053",  "21054",  "21055",  "21056",
                          "21057",  "21058",  "21059",  "21060",  "21061",  "21062",  "21063",  "21064",  "21065",  "21066",
                          "21067",  "21068",  "21069",  "21070",  "21071",  "21072",  "21073",  "21074",  "21075",  "21076",
                          "21077",  "21079", "21080",  "21081", "21082",  "21083",  "21084",  "21085",  "21086",  "21087",
                          "21088",  "21089", "21091",  "21092", "21093",  "21094",  "21095",  "21096",  "21097",  "21098",
                          "21099",  "21100", "21101",  "21102",  "21103",  "21104",  "21105",  "21106",  "21107",  "21108",
                          "21109",  "21110",  "21111", "21112",  "21113",  "21114",  "21115",  "21116",  "21117",  "21118"))
View(dati.Comuni.BZ)

## al posto del merge dobbiamo mettere insieme i due dataset con la funzione rbind (row bind), praticamente concatenando i due, uno va fino al 21.12.2020 e l'altro parte il 18.12.2020, cosi possiamo avere un dataset unico che contiene dati dal marzo 2020 fino alla data attuale.
## piccolo hint: formattare la date in entrambi con as.Date  esempio: dati.Comuni.BZ$datum <- as.Date(dati.Comuni.BZ$datum)
## inoltre le colonne comuni nei due dataset dovrebbero essere i seguenti: datum, comune(codice istat), contaggi, e altri indicatori di contaggio. Per l'analisi i singoli comuni fuori provincia possono essere raggruppati in una singola voce: fuori provincia e con il codice '888' o '999' come preferisci

Covid.data<-rbind(dati.PC[,c(2,1,5,6)],dati.Comuni.BZ)

## per me rivedere: come modificare da formato as.POSIXct in Date
## capire come mai non funziona as.date per dati.comuni.BZ$datum
## creare nuova colonna Covid.data$neue.Fälle attraverso la funzione diff(Covid.data$totals)