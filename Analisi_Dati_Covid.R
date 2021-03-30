if (!require("data.table")) install.packages("data.table") ; library (data.table)
if (!require("openxlsx"))   install.packages("openxlsx")   ; library (openxlsx)
if (!require("stringr"))    install.packages("stringr")    ; library (stringr)
if (!require("Cairo"))      install.packages("Cairo")      ; library (Cairo)

library(ggplot2)


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
new.dati.PC$positiv<-as.integer(new.dati.PC$positiv)

plot(new.dati.PC$datum,new.dati.PC$positiv)


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
identify(pippo, n=1)  # observation n 303: outlier

pippo[303,] # outlier: observation on 2021-02-12         10539

plot(pippo[-303], type= "p")
plot(pippo[-303],type="l")

# idea per compilare valori mancanti
bz20210209 <- subset(dati.PC,istat==21008&datum=="2021-02-09",select=c("positiv"))
bz20210212 <-  subset(dati.PC,istat==21008&datum=="2021-02-12",select=c("positiv"))
# esempio per Bolzano
#> 10539-10315
#[1] 224
#> (10539-10315)/2
#[1] 112
#> (10539-10315)/3
#[1] 74.66667

 
table(pippo$nuovi_contagi) ## frequency table for new cases

# utilizzando bar plot

grafico.pippo<-barplot(pippo$nuovi_contagi[-303], main= "Nuovi contagi giornalieri", xlab= "Giorno", ylab="Nuovi_casi", names.arg = pippo$datum, col="red") ## bar chart of the new cases for Bolzano

# new.data.set Covid.data = without NA

new.Covid.data<-subset(Covid.data,Covid.data$totals!="NA", select=c("datum","totals","nuovi_contagi"))
str(new.Covid.data)
View(new.Covid.data)

# for loop

Lingua<-"Deutsch"
sheetsXLS <- c('Comuni', 'Com_AggrDimora', 'Com_AggrDimora_DC', 'Com_AggrASDimora', 'Com_AggrPAFDimora')

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


# 78 90
#nome.comune<-subset(dati.PC,dati.PC$ISTAT_code==c(21001:21118), select=c("ISTAT_code","name","nome"))

#codice.istat<-nome.comune$ISTAT_code

#if(Lingua=="Deutsch") {
#  codice.istat<-nome.comune$name} 
#else {codice.istat<-nome.comune$nome}

#### pluto[pluto$Sys_Lingua==Lingua&Chiave==comune,c("Descr_shortDimora")]



  CairoPDF("test.pdf",width = 10, height = 14)
par(mfrow=c(2,1))
for (comune in codice.istat){
  #     barplot(Covid.data[Covid.data$ISTAT_code==comune]$nuovi_contagi, 
  #            main= paste("Nuovi contagi giornalieri",comune,sep = "\n"), 
  #            xlab= "Giorno", 
  #            ylab="Nuovi_casi", 
  #            names.arg = Covid.data[Covid.data$ISTAT_code==comune]$datum, 
  #            col="red",
  #            border =NA) 
  
 pippo <- subset(Covid.data,ISTAT_code==comune,select= c("datum","totals","nuovi_contagi"))
  plot(pippo$nuovi_contagi, type= "l",main=paste("Nuovi contagi giornalieri",pluto[pluto$Sys_Lingua==Lingua&pluto$Chiave==comune,c("Descr_shortDimora")],sep = "\n"))
  plot(pippo$totals, type="h")
  }
dev.off()
