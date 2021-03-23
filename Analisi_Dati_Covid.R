if (!require("openxlsx"))   install.packages("openxlsx")   ; library (openxlsx)
if (!require("data.table")) install.packages("data.table") ; library (data.table)
if (!require("stringr"))    install.packages("stringr")    ; library (stringr)


getwd()
DIR<-getwd()
directorydati<-"C:/Users/nicol/Desktop/PROGETTO/motioncharts/d"
directorydati<-paste(DIR,"d",sep="/")



# file Covid Protezione Civile

dati.PC<-read.csv(file =paste(directorydati,"Corona.Data.Detail.csv",sep="/"),header=TRUE, sep=";")
setDT(dati.PC)
# Eliminazione colonna 3
## Perchè eliminare la colonna 3 (la data ci serve, voremmo avere l'andamento per giorno e per comune)
dati.PC<-dati.PC[,-3]

setnames(dati.PC,c("istat","nome"),c("ISTAT_code","name_IT"))
str(dati.PC)

dati.PC[,c("ISTAT_code","positiv","antigen","pcr.pos.geh..tote","quarantaene","pcr.pos.geh..antigen.pos.geh..tote")]<-as.numeric(dati.PC[,c("ISTAT_code","positiv","antigen","pcr.pos.geh..tote","quarantaene","pcr.pos.geh..antigen.pos.geh..tote")])
View(dati.PC)

# file 

dati.Comuni.BZ<-read.csv(file = paste(directorydati,"covid19_bz_municipalities.csv",sep="/") ,header=TRUE, sep=",")
View(dati.Comuni.BZ)
setDT(dati.Comuni.BZ)
colnames(dati.Comuni.BZ)
str(dati.Comuni.BZ)

dati.Comuni.BZ$ISTAT_code<-as.numeric(dati.Comuni.BZ$ISTAT_code)
dati.PC$ISTAT_code<-as.numeric(dati.PC$ISTAT_code)
Covid.data<-merge(dati.PC,dati.Comuni.BZ, by=c("ISTAT_code","name_IT"))
View(Covid.data)

## al posto del merge dobbiamo mettere insieme i due dataset con la funzione rbind (row bind), praticamente concatenando i due, uno va fino al 21.12.2020 e l'altro parte il 18.12.2020, cosi possiamo avere un dataset unico che contiene dati dal marzo 2020 fino alla data attuale.
## piccolo hint: formattare la date in entrambi con as.Date  esempio: dati.Comuni.BZ$datum <- as.Date(dati.Comuni.BZ$datum)
## inoltre le colonne comuni nei due dataset dovrebbero essere i seguenti: datum, comune(codice istat), contaggi, e altri indicatori di contaggio. Per l'analisi i singoli comuni fuori provincia possono essere raggruppati in una singola voce: fuori provincia e con il codice '888' o '999' come preferisci