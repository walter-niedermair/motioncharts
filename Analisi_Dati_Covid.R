if (!require("data.table")) install.packages("data.table") ; library (data.table)

getwd()
DIR<-getwd()
directorydati<-paste(DIR,"d",sep="/") 

# file Covid Protezione Civile

dati.PC<-read.csv(file=paste(DIR,"d","Corona.Data.Detail.csv",sep="/"),header=TRUE, sep=";")

str(dati.PC) # struttura data.frame

View(table(dati.PC$istat)) # Frequenze per codice istat
unique(substr(dati.PC$istat,1,2))


new.dati.PC <- subset(dati.PC,substr(dati.PC$istat,1,2)==21,select=c("istat","datum","sumPos.pcr.antigen."))
colnames(new.dati.PC)

unique(new.dati.PC$istat)
new.dati.PC$datum<-as.Date(new.dati.PC$datum)
str(new.dati.PC)

plot(new.dati.PC$datum,new.dati.PC$sumPos.pcr.antigen.)


dati.Comuni.BZ<-read.csv(file =paste(directorydati,"covid19_bz_municipalities.csv", sep="/"),header=TRUE, sep=",")

str(dati.Comuni.BZ)
View(dati.Comuni.BZ)

unique(dati.Comuni.BZ$ISTAT_code)

dati.Comuni.BZ$ISTAT_code<-as.numeric(dati.Comuni.BZ$ISTAT_code)

#dati.Comuni.BZ$datum<-as.Date(dati.Comuni.BZ$datum,format= "%m/%d/%y") # produce NAs as a result

View(new.dati.PC$datum)
dati.Comuni.BZ$datum <- format(as.Date(dati.Comuni.BZ$datum, format = "%d/%m/%Y"), "%Y-%m-%d")
View(dati.Comuni.BZ$datum)




substr(dati.Comuni.BZ$ISTAT_code,1,2)## primi due caratteri 

new.dati.Comuni.BZ <- subset(dati.Comuni.BZ,substr(dati.Comuni.BZ$ISTAT_code,1,2)==21, select=c("ISTAT_code","datum","totals"))
View(new.dati.Comuni.BZ)

str(new.dati.Comuni.BZ)
new.dati.Comuni.BZ$datum<-as.Date(new.dati.Comuni.BZ$datum, format= "%Y-%m-%d")

# new.dati.Comuni.BZ$datum <- format(as.Date(new.dati.Comuni.BZ$datum, format = "%d/%m/%Y"), "%Y-%m-%d") sbagliato il formato


colnames(new.dati.Comuni.BZ)
colnames(new.dati.PC)

setnames(new.dati.PC,c("istat","sumPos.pcr.antigen."),c("ISTAT_code","totals"))
colnames(new.dati.PC)

new.dati.Comuni.BZ <- subset(new.dati.Comuni.BZ, datum< as.Date("2020-12-18"))



# inoltre le colonne comuni nei due dataset dovrebbero essere i seguenti: datum, comune(codice istat), contaggi, e altri indicatori di contaggio. 
# Per l'analisi i singoli comuni fuori provincia possono essere raggruppati in una singola voce: fuori provincia e con il codice '888' o '999' come preferisci

# mettere insieme i due dataset con la funzione rbind (row bind),

Covid.data <- rbind(new.dati.Comuni.BZ,new.dati.PC)
View(Covid.data)


# structure

str(Covid.data) 

Covid.data$ISTAT_code<-as.numeric(Covid.data$ISTAT_code) # Format change: character > numeric

pippo<-subset(Covid.data,ISTAT_code==21008,select=c("datum","totals"))

plot(pippo, type = "l")

setDT(Covid.data)


# Nuova Colonna con Lag 

library(data.table)

Covid.data[, lag.value:=c(NA, totals[-.N]), by="ISTAT_code"]
View(Covid.data)

Covid.data$nuovi_contagi <- Covid.data$totals- Covid.data$lag.value

pippo <- subset(Covid.data, ISTAT_code == 21008, select = c("datum","nuovi_contagi"))

plot(pippo, type= "p")

# to do: per ogni comune plottare bar charts and line

# for loop

codice.istat <- 21001:21118

for(comune in codice.istat){
  nuovi.contagi<- subset(Covid.data,ISTAT_code == comune,select="nuovi_contagi")
  barplot(nuovi_contagi)
}



## create a pdf file through the function dev.pdf
