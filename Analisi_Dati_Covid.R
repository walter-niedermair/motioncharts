#############################################################
## zoo and xts - na.locf
# https://campus.datacamp.com/courses/manipulating-time-series-data-with-xts-and-zoo-in-r/merging-and-modifying-time-series?ex=6
#
## imputeTS - na.mean
# https://cran.r-project.org/web/packages/imputeTS/imputeTS.pdf
# https://stackoverflow.com/questions/9322773/how-to-replace-na-with-mean-by-group-subset
#
#############################################################

getwd()
DIR<-getwd()
directorydati<-paste(DIR,"d",sep="/")

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

#############################################################
## define directories
#############################################################
getwd()
DIR<-getwd()
directorydati<-paste(DIR,"d",sep="/")

#############################################################
# read and/or define functions
#############################################################

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

#############################################################
# Read data
#############################################################

getwd()
DIR<-getwd()
directorydati<-paste(DIR,"d",sep="/") 

# file Covid Protezione Civile

dati.PC<-read.csv(file=paste(DIR,"d","Corona.Data.Detail.csv",sep="/"),header=TRUE, sep=";")

str(dati.PC) # struttura data.frame

#View(table(dati.PC$istat)) # Frequenze per codice istat

unique(substr(dati.PC$istat,1,2))

new.dati.PC <- subset(dati.PC,substr(dati.PC$istat,1,2)==21,select=c("istat","datum","positiv"))
colnames(new.dati.PC)

unique(new.dati.PC$istat)

new.dati.PC$datum<-as.Date(new.dati.PC$datum)
str(new.dati.PC)

null.data<-subset(new.dati.PC,new.dati.PC$positiv=="null")## recurrent pattern: 2021-02-10 & 2021-02-11
View(null.data)

new.dati.PC$positiv<-as.numeric(new.dati.PC$positiv) 

plot(new.dati.PC$datum,new.dati.PC$positiv)

# Aldein - 21001

Aldein<-subset(new.dati.PC,new.dati.PC$istat==21001)
View(Aldein)
str(Aldein)
Aldein$datum<-as.Date(Aldein$datum)
Aldein$positiv<-as.numeric(Aldein$positiv)


#############################################################
# soluzione 1 - data.table con funzione definita sopra "impute.mean" 
# questa funzione mette il overall mean al posto del valore mancante
setDT(new.dati.PC)
new.dati.PC[, positiv := impute.mean(positiv), by = "istat"]
#############################################################
# soluzione 2 - data.table con funzione "na_interpolation" dal pacchetto imputeTS 
# questa funzione fa una interpolazione "lineare" o altra per i valori missing
setDT(new.dati.PC)
new.dati.PC[, positiv := na_interpolation(positiv, option = "linear"), by = "istat"]
#############################################################







which(is.na(Aldein$positiv))
which(Aldein$datum=="2021-02-09"|Aldein$datum=="2021-02-12") ## to check the position of obs corresponding to the dates 

# metodo poco efficiente e mi trasforma tutti i valori della colonna in decimale
Aldein$positiv[is.na(Aldein$positiv)]<-round(mean(c(Aldein[Aldein$datum=="2021-02-09"|Aldein$datum=="2021-02-12","positiv"])),digits=0)

View(Aldein)

# trasformare Aldein in ts 

Aldein<-subset(new.dati.PC,new.dati.PC$istat==21001)
View(Aldein)

Aldein.ts<-xts(Aldein$positiv,order.by=Aldein$datum)
View(Aldein.ts)
Aldein.ts<-zoo(Aldein.ts)

## definizione di una funzione che dipende dalla data e dal comune

codice<-new.dati.PC$ISTAT_code
data<- c("2021-02-10","2021-02-11")
data<-as.Date(data)
View(data)

myfunction<-function(codice,data){
  for (i in codice )
  prova<-subset(new.dati.PC,new.dati.PC$ISTAT_code==codice)
  if(any(prova$datum==data)){prova$totals<- round(mean(prova$totals[data+1],prova$totals[data-1], digits=0))
  }
  }

dati.Comuni.BZ<-read.csv(file =paste(directorydati,"covid19_bz_municipalities.csv", sep="/"),header=TRUE, sep=",")

str(dati.Comuni.BZ)

#View(dati.Comuni.BZ)

unique(dati.Comuni.BZ$ISTAT_code)

dati.Comuni.BZ$ISTAT_code<-as.numeric(dati.Comuni.BZ$ISTAT_code)

#dati.Comuni.BZ$datum<-as.Date(dati.Comuni.BZ$datum,format= "%m/%d/%y") # produce NAs as a result


dati.Comuni.BZ$datum <- as.Date(dati.Comuni.BZ$datum)

substr(dati.Comuni.BZ$ISTAT_code,1,2)## primi due caratteri 

new.dati.Comuni.BZ <- subset(dati.Comuni.BZ,substr(dati.Comuni.BZ$ISTAT_code,1,2)==21, select=c("ISTAT_code","datum","totals"))

View(new.dati.Comuni.BZ)

str(new.dati.Comuni.BZ)
new.dati.Comuni.BZ$datum<-as.Date(new.dati.Comuni.BZ$datum)


colnames(new.dati.Comuni.BZ)
colnames(new.dati.PC)

setnames(new.dati.PC,c("istat","positiv"),c("ISTAT_code","totals"))
colnames(new.dati.PC)

new.dati.Comuni.BZ <- subset(new.dati.Comuni.BZ, datum< as.Date("2020-12-18"))

View(new.dati.Comuni.BZ)


Covid.data <- rbind(new.dati.Comuni.BZ,new.dati.PC)
View(Covid.data)


# structure

str(Covid.data) 

Covid.data$ISTAT_code<-as.numeric(Covid.data$ISTAT_code) # Format change: character > numeric

setDT(Covid.data)


# Nuova Colonna con Lag 

library(data.table)

setDT(Covid.data)
Covid.data

Covid.data[, lag.value:=c(NA, totals[-.N]), by="ISTAT_code"]
View(Covid.data)

Covid.data$lag.value[is.na(Covid.data$lag.value)]<-0

Covid.data$nuovi_contagi <- Covid.data$totals- Covid.data$lag.value

View(Covid.data)


pippo <- subset(Covid.data, ISTAT_code == 21008, select = c("datum","nuovi_contagi"))

View(pippo)
plot(pippo, type= "p")
# identify the outlier
identify(pippo, n=2)  # observation n 303: outlier

pippo[-c(301,303),] # outliers

plot(pippo[-c(301,303)], type= "p")
plot(pippo[-303],type="l")


table(pippo$nuovi_contagi) ## frequency table for new cases

# utilizzando bar plot

grafico.pippo<-barplot(pippo$nuovi_contagi[-303], main= "Nuovi contagi giornalieri", xlab= "Giorno", ylab="Nuovi_casi", names.arg = pippo$datum, col="red") ## bar chart of the new cases for Bolzano

# new.data.set Covid.data = without NA

new.Covid.data<-subset(Covid.data,Covid.data$totals!="NA", select=c("datum","totals","nuovi_contagi"))
str(new.Covid.data)
View(new.Covid.data)

# for loop

Lingua<-"Deutsch"
sheetsXLS <- c('Comuni', 'Com_AggrDimora', 'Com_AggrDimora_DC', 'Com_AggrASDimora', 'Com_AggrPAFDimora',"label")

#-- funzione per leggere i diversi sheets del file excel, selezionando la lingua 

for(i in sheetsXLS){
  pluto <- read.xlsx(paste(directorydati, 'geo--comuni.xlsx',sep="/"), sheet = i)
  pluto <- subset(pluto,Sys_Lingua==Lingua)
  pluto <- pluto[!names(pluto)=="Sys_Lingua"]
  assign(paste0('GEM_', i), pluto)
  rm(pluto)
}


pluto <- read.xlsx(paste(directorydati, 'geo--comuni.xlsx',sep="/"), sheet = "Comuni")
#> colnames(pluto)
#[1] "Chiave"                "Sys_Lingua"            "DescrizioneDimora_DC"  "DescrizioneLLavoro_DC" "DescrizioneDimora"    
#[6] "DescrizioneLLavoro"    "Descr_shortDimora"     "Descr_shortLLavoro"    "DescrizioneDimoraDis"  "Com_AggrDimora"       
#[11] "Com_AggrDimora_DC"     "Com_AggrLLavoro"       "Com_AggrLLavoro_DC"    "Com_AggrDimoraDis"     "Com_AggrCP"           
#[16] "Com_AggrAS"            "Com_AggrPAF" 


pluto$Chiave <- as.numeric(pluto$Chiave)

codice.istat <- sort(unique(Covid.data$ISTAT_code))

## creare file excel (geo--comuni.xlsx, sheet =label) # long format 

Label.long <- read.xlsx(paste(directorydati, 'geo--comuni.xlsx',sep="/"), sheet = "Label")
View(Label.long)

## wide format using the reshape function

Label.wide <-reshape(Label.long, idvar = "Sys_Lingua", timevar = "Label", direction = "wide")
View(Label.wide)



comune<-21008

if (Lingua=="Deutsch"){ ydesc <-"Neue_Positive"} else { ydesc <-"Nuovi_positivi"}
if (Lingua=="Deutsch"){ xdesc <- "Datum"} else { xdesc <- "data"}
if (Lingua=="Deutsch"){ main.title <-paste("Neue Tägliche Fallzahlen",pluto[pluto$Sys_Lingua==Lingua & pluto$Chiave==comune,c("Descr_shortDimora")],sep = "\n")} else {
  main.title <- paste("Nuovi contagi giornalieri")
}
if (Lingua=="Deutsch"){ main.title2 <-paste("Gesamte Tägliche Fallzahlen",pluto[pluto$Sys_Lingua==Lingua & pluto$Chiave==comune,c("Descr_shortDimora")],sep = "\n")} else {
  main.title2 <- paste("Totale contagi giornalieri")
}

  CairoPDF("test.pdf",width = 10, height = 14)
par(mfrow=c(2,1))

for (comune in codice.istat){
 pippo <- subset(Covid.data,ISTAT_code==comune,select= c("datum","totals","nuovi_contagi"))
 plot(pippo$datum, # x variable
      pippo$nuovi_contagi,  # y variable
      col="red",            # line colour
      type= "l",            # line graph
      lty=1,                # line type
      lwd=1,                # line width,
      xaxt="n",             # suppress x axis
      ylim=range(pippo$nuovi_contagi), #c(min(pippo$nuovi_contagi),max(pippo$nuovi_contagi)) # values plotted on the y axis)  
      # col.lab="black", cex.lab=1.75
      frame.plot = FALSE,
      xlab = xdesc,    # x-axis label
      ylab = ydesc)
 
      title(main= paste(main.title,pluto[pluto$Sys_Lingua==Lingua & pluto$Chiave==comune,c("Descr_shortDimora")],sep = "\n"))
              
      #paste("Nuovi contagi giornalieri",pluto[pluto$Sys_Lingua==Lingua&pluto$Chiave==comune,c("Descr_shortDimora")],sep = "\n"))
      
      
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
      xlab= xdesc,
      ylab= ydesc)
  
##################################################################################
 
 # TITLE OF THE GRAPH
  title(main= paste(main.title2,pluto[pluto$Sys_Lingua==Lingua&pluto$Chiave==comune,c("Descr_shortDimora")],sep = "\n"))
      
  axis.Date(1, at = seq(pippo$datum[1], pippo$datum[length(pippo$datum)], by="month"),pos=0,
            labels= seq(pippo$datum[1], pippo$datum[length(pippo$datum)], by="month"),
            format="%Y-%m", las = 0)

  # axis.Date(1, at=seq(min(pippo$datum), max(pippo$datum), by="months"), format="%m-%Y")
}
dev.off()


#     barplot(Covid.data[Covid.data$ISTAT_code==comune]$nuovi_contagi, 
#            main= paste("Nuovi contagi giornalieri",comune,sep = "\n"), 
#            xlab= "Giorno", 
#            ylab="Nuovi_casi", 
#            names.arg = Covid.data[Covid.data$ISTAT_code==comune]$datum, 
#            col="red",
#            border =NA)


