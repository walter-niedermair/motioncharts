if (!require("openxlsx"))   install.packages("openxlsx")   ; library (openxlsx)
if (!require("data.table")) install.packages("data.table") ; library (data.table)
if (!require("stringr"))    install.packages("stringr")    ; library (stringr)


getwd()
DIR<-getwd()
directorydati<-"C:/Users/nicol/Desktop/PROGETTO/motioncharts/d"


# file Covid Protezione Civile

dati.PC<-read.csv(file ="C:/Users/nicol/Desktop/PROGETTO/motioncharts/d/Corona.Data.Detail.csv",header=TRUE, sep=";")
setDT(dati.PC)
# Eliminazione colonna 3

dati.PC<-dati.PC[,-3]

setnames(dati.PC,c("istat","nome"),c("ISTAT_code","name_IT"))
str(dati.PC)

dati.PC[,c("ISTAT_code","positiv","antigen","pcr.pos.geh..tote","quarantaene","pcr.pos.geh..antigen.pos.geh..tote")]<-as.numeric(dati.PC[,c("ISTAT_code","positiv","antigen","pcr.pos.geh..tote","quarantaene","pcr.pos.geh..antigen.pos.geh..tote")])
View(dati.PC)

# file 

dati.Comuni.BZ<-read.csv(file ="C:/Users/nicol/Desktop/PROGETTO/motioncharts/d/covid19_bz_municipalities.csv",header=TRUE, sep=";")
View(dati.Comuni.BZ)
setDT(dati.Comuni.BZ)
colnames(dati.Comuni.BZ)
str(dati.Comuni.BZ)

dati.Comuni.BZ$ISTAT_code<-as.numeric(dati.Comuni.BZ$ISTAT_code)
dati.PC$ISTAT_code<-as.numeric(dati.PC$ISTAT_code)
Covid.data<-merge(dati.PC,dati.Comuni.BZ, by=c("ISTAT_code","name_IT"))
View(Covid.data)
