if (!require("data.table")) install.packages("data.table") ; library (data.table)

library(ggplot2)

getwd()
DIR<-getwd()
directorydati<-paste(DIR,"d",sep="/") 

# file Covid Protezione Civile

dati.PC<-read.csv(file=paste(DIR,"d","Corona.Data.Detail.csv",sep="/"),header=TRUE, sep=";")

str(dati.PC) # struttura data.frame

#View(table(dati.PC$istat)) # Frequenze per codice istat
unique(substr(dati.PC$istat,1,2))


new.dati.PC <- subset(dati.PC,substr(dati.PC$istat,1,2)==21,select=c("istat","datum","sumPos.pcr.antigen."))
colnames(new.dati.PC)

unique(new.dati.PC$istat)
new.dati.PC$datum<-as.Date(new.dati.PC$datum)
str(new.dati.PC)

plot(new.dati.PC$datum,new.dati.PC$sumPos.pcr.antigen.)


dati.Comuni.BZ<-read.csv(file =paste(directorydati,"covid19_bz_municipalities.csv", sep="/"),header=TRUE, sep=",")

str(dati.Comuni.BZ)
#View(dati.Comuni.BZ)

unique(dati.Comuni.BZ$ISTAT_code)

dati.Comuni.BZ$ISTAT_code<-as.numeric(dati.Comuni.BZ$ISTAT_code)

#dati.Comuni.BZ$datum<-as.Date(dati.Comuni.BZ$datum,format= "%m/%d/%y") # produce NAs as a result

#View(new.dati.PC$datum)
dati.Comuni.BZ$datum <- format(as.Date(dati.Comuni.BZ$datum, format = "%d/%m/%Y"), "%Y-%m-%d")
#View(dati.Comuni.BZ$datum)




substr(dati.Comuni.BZ$ISTAT_code,1,2)## primi due caratteri 

new.dati.Comuni.BZ <- subset(dati.Comuni.BZ,substr(dati.Comuni.BZ$ISTAT_code,1,2)==21, select=c("ISTAT_code","datum","totals"))
#View(new.dati.Comuni.BZ)

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
#View(Covid.data)


# structure

str(Covid.data) 

Covid.data$ISTAT_code<-as.numeric(Covid.data$ISTAT_code) # Format change: character > numeric

pippo<-subset(Covid.data,ISTAT_code==21008,select=c("datum","totals"))

plot(pippo, type = "p")
lines(pippo)


setDT(Covid.data)


# Nuova Colonna con Lag 

library(data.table)

setDT(Covid.data)

Covid.data[, lag.value:=c(NA, totals[-.N]), by="ISTAT_code"]
View(Covid.data)

Covid.data$nuovi_contagi <- Covid.data$totals- Covid.data$lag.value

pippo <- subset(Covid.data, ISTAT_code == 21008, select = c("datum","nuovi_contagi"))
View(pippo)
plot(pippo, type= "p")
lines(pippo)

table(pippo$nuovi_contagi) ## frequency table for new cases

# utilizzando bar plot

grafico.pippo<-barplot(pippo$nuovi_contagi, main= "Nuovi contagi giornalieri", xlab= "Giorno", ylab="Nuovi_casi", names.arg = pippo$datum, col="red") ## bar chart of the new cases for Bolzano

# 

library(Cairo)

#View(pippo)

pippo<-as.data.frame(pippo)
typeof(pippo$datum)
pippo$datum<-as.Date(pippo$datum)

str(pippo)

pippo.grafico <- ggplot(pippo, aes(x = "datum", y = "nuovi_contagi")) +
  geom_bar(data = pippo) +
  annotate("text", x = 0, y = 0, label = "nuovi_casi_giornalieri",
           family = "Papyrus", color = "darkred", size = 8) +
  labs(title = "Nuovi_contagi_giornalieri") +
  theme_light(base_family = "Comic Sans MS")

pippo.grafico

rlang::last_error()

ggsave(pippo.grafico,filename = paste(DIR,"d","prova1.pdf",sep="/"),device = cairo_pdf,
       width = 4, height = 3, units = "in")
library(Cairo)

CairoPDF(pippo.grafico, file = paste(DIR,"d","prova1.pdf",sep="/"),bg="transparent")
dev.off()

# for loop

#View(Covid.data$ISTAT_code)

codice.istat <- c(21001:21118)

new.cases <- function(comune){
for (comune in codice.istat){
 nuovi_casi<- subset(Covid.data,Covid.data$ISTAT_code==comune,select=c("datum","nuovi_contagi"))
 grafico.nuovi_casi<-barplot(nuovi_casi$nuovi_contagi, main= "Nuovi contagi giornalieri", xlab= "Giorno", ylab="Nuovi_casi", names.arg = nuovi_casi$datum, col="red") ## bar chart of the new cases for Bolzano
 cairoPDF(grafico.nuovi_casi,file=(paste(DIR,"d","prova2.pdf",sep="/")))
 return(nuovi_casi)
}
}

new.cases(21010)
head(new.cases(21008))
## head(new.cases(21008))
## datum nuovi_contagi
## 1 2020-12-18            NA
## 2 2020-12-19             1
## 3 2020-12-20             0
## 4 2020-12-21             0
## 5 2020-12-22             0
## 6 2020-12-23             2


new.cases(21011)
head(new.cases(21011))



