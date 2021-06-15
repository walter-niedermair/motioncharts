if (!require("openxlsx"))   install.packages("openxlsx")   ; library (openxlsx)
if (!require("data.table")) install.packages("data.table") ; library (data.table)
if (!require("stringr"))    install.packages("stringr")    ; library (stringr)

#-- funzione da usare
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#-- selezionare la Lingua di esecuzione dello script

Lingua <- "Deutsch" # Italiano

fullyear <- 2020

#-- definisco le directory 

directorymain <- getwd()
directorydati <- paste(directorymain,"d", sep="/")
directoryddf  <- sprintf('%s/ddf--%s-amb',directorymain,tolower(substr(Lingua,1,2)))

#-- creo la directory ddf se non esiste già

if (!dir.exists(directoryddf)) dir.create(sprintf('%s/ddf--%s-amb',directorymain,tolower(substr(Lingua,1,2)))) 

#-- leggo i nomi dei comuni, circoscrizioni, piccole aree funzionali, etc

sheetsXLS <- c('Comuni', 'Com_AggrDimora', 'Com_AggrDimora_DC', 'Com_AggrASDimora', 'Com_AggrPAFDimora', 'Com_AggrCPDimora')

#-- funzione per leggere i diversi sheets del file excel, selezionando la lingua 

for(i in sheetsXLS){
  pippo <- read.xlsx(paste(directorydati, 'geo--comuni.xlsx',sep="/"), sheet = i)
  if(any(colnames(pippo)=="X")){pippo <- pippo[,1:(which(colnames(pippo)=="X")-1)]}
  pippo <- subset(pippo,Sys_Lingua==Lingua)
  pippo <- pippo[!names(pippo)=="Sys_Lingua"]
  assign(paste0('GEM_', i), setDT(pippo))
  rm(pippo)
}

# funzione da creare
# df         = "GEM_Comuni","GEM_Com_AggrASDimora"
# colnamein  = "DescrizioneDimora","Descrizione"
# colnameout = "short"

CreaShort <- function(nomedf = "df", colnamein  = "Descrizione", colnameout = "short") {
  #nomedf <- deparse(substitute(df))
  df <-  eval(parse(text=nomedf))
  if (substr(nomedf,1,12) != "GEM_Comuni") df <- df[get(colnames(df)[1]) != '?',]; set(df, NULL, as.integer(1), as.numeric(df[[1]])) # rimuovo righe con la chiave '?'
  df$short <- tolower(gsub(" ", "", str_replace_all(df[,get(colnamein)], "[^[:alnum:]]", " "), fixed = TRUE))
  if (substr(nomedf,1,12) != "GEM_Comuni") df$short <- paste0(tolower(substr(strsplit(nomedf,"_")[[1]][NROW(strsplit(nomedf,"_")[[1]])],5,nchar(strsplit(nomedf,"_")[[1]][NROW(strsplit(nomedf,"_")[[1]])]))),"_",df$short)
  df$short <- str_replace_all(df$short,c("ü" = "ue", "ä" = "ae", "ö" = "oe", "ë" = "e"))
#  if (substr(nomedf,1,10) != "GEM_Comuni") {
#    df[ , eval(colnameout) := NULL] # rimuovo la colonna
#    setnames(df, "short", colnameout) # rinomiono la colonna
#  } 
  assign(nomedf, df, envir = parent.frame())
}

CreaShort(nomedf="GEM_Comuni"           , colnamein  ="DescrizioneDimora")
CreaShort(nomedf="GEM_Com_AggrASDimora" )
CreaShort(nomedf="GEM_Com_AggrPAFDimora")
CreaShort(nomedf="GEM_Com_AggrDimora"   )
CreaShort(nomedf="GEM_Com_AggrDimora_DC")
CreaShort(nomedf="GEM_Com_AggrCPDimora", colnamein  ="Com_AggrCP")

#-- merge per creare GEM

GEM <- merge(GEM_Comuni,GEM_Com_AggrASDimora[,c("Com_AggrAS","Descrizione","short")],by="Com_AggrAS")
GEM$Com_AggrAS <- NULL
setnames(GEM,"short.x","short")
setnames(GEM,"short.y","com_aggr_as")
setnames(GEM,"Descrizione","com_aggr_as_Descrizione")

#-- tolta la chiave "021", cambio formato da string a numeric
GEM$gem <- as.integer(substrRight(GEM$Chiave, 3))
#GEM$gem <- GEM$Chiave-21000
GEM     <- GEM[order(GEM$short),]

##--- neu Com_AggrPAF
GEM <- merge(GEM,GEM_Com_AggrPAFDimora[,c("Com_AggrPAF","Descrizione","short")],by="Com_AggrPAF")
GEM$Com_AggrPAF <- NULL
setnames(GEM,"short.x","short")
setnames(GEM,"short.y","com_aggr_paf")
setnames(GEM,"Descrizione","com_aggr_paf_descrizione")
##--- neu Com_AggrPAF

##--- neu Com_AggrDimora
GEM <- merge(GEM,GEM_Com_AggrDimora[,c("Com_AggrDimora","Descrizione","short")],by="Com_AggrDimora")
GEM$Com_AggrDimora <- NULL
setnames(GEM,"short.x","short")
setnames(GEM,"short.y","com_aggr_dimora")
setnames(GEM,"Descrizione","com_aggrdimora_descrizione")
##--- neu Com_AggrDimora


#-- preparo per l'export del dominio geo (gemeinde)

exp <- subset(GEM,select = c("short","color","DescrizioneDimora","com_aggr_as","com_aggr_paf","com_aggr_dimora","svg"))
setnames(exp,c("short","DescrizioneDimora"),c("gem","name"))
setDT(exp)
setcolorder(exp,"gem")
exp$`is--gem` <- "true"

setnames(exp,c("gem","com_aggr_as","com_aggr_paf","com_aggr_dimora","svg"),c("geo","asdimora","pafdimora","dimora","shape_lores_svg"))
write.csv(exp,file = paste(directoryddf,'ddf--entities--geo--gem.csv',sep = "/"),
          row.names = FALSE,fileEncoding = "UTF-8",quote=FALSE)

#-- preparo per l'export del dominio geo (Com_AggrASDimora)

GEM_Com_AggrASDimora$Com_AggrAS <- NULL
setnames(GEM_Com_AggrASDimora,"Descrizione","Com_AggrAS")
setnames(GEM_Com_AggrASDimora,"short","com_aggr_as")
setnames(GEM_Com_AggrASDimora,c("Com_AggrAS","com_aggr_as","svg"),c("name","asdimora","shape_lores_svg"))
GEM_Com_AggrASDimora$`is--asdimora` <- "true"
setnames(GEM_Com_AggrASDimora,"asdimora","geo")
setDT(GEM_Com_AggrASDimora)
setcolorder(GEM_Com_AggrASDimora,"geo")
write.csv(GEM_Com_AggrASDimora,file = paste(directoryddf,"ddf--entities--geo--asdimora.csv",sep = "/"),
          row.names = FALSE,fileEncoding = "UTF-8",quote=FALSE)

#-- preparo per l'export del dominio geo (Com_AggrPAFDimora)
GEM_Com_AggrPAFDimora$Com_AggrPAF <- NULL
setnames(GEM_Com_AggrPAFDimora,"Descrizione","Com_AggrPAF")
setnames(GEM_Com_AggrPAFDimora,"short","com_aggr_paf")
setnames(GEM_Com_AggrPAFDimora,c("Com_AggrPAF","com_aggr_paf","svg"),c("name","pafdimora","shape_lores_svg"))
GEM_Com_AggrPAFDimora$`is--pafdimora` <- "true"
setnames(GEM_Com_AggrPAFDimora,"pafdimora","geo")
setDT(GEM_Com_AggrPAFDimora)
setcolorder(GEM_Com_AggrPAFDimora,"geo")
write.csv(GEM_Com_AggrPAFDimora,file = paste(directoryddf,"ddf--entities--geo--pafdimora.csv",sep = "/"),
          row.names = FALSE,fileEncoding = "UTF-8",quote=FALSE)

#-- preparo per l'export del dominio geo (Com_AggrDimora)
GEM_Com_AggrDimora$Com_AggrDimora <- NULL
GEM_Com_AggrDimora$Descr_short <- NULL
GEM_Com_AggrDimora$DescrizioneDis <- NULL
GEM_Com_AggrDimora$DescrizioneDis_short <- NULL

setnames(GEM_Com_AggrDimora,"Descrizione","Com_AggrDimora")
setnames(GEM_Com_AggrDimora,"short","com_aggr_dimora")
setnames(GEM_Com_AggrDimora,c("Com_AggrDimora","com_aggr_dimora","svg"),c("name","dimora","shape_lores_svg"))
GEM_Com_AggrDimora$`is--dimora` <- "true"
setnames(GEM_Com_AggrDimora,"dimora","geo")
setDT(GEM_Com_AggrDimora)
setcolorder(GEM_Com_AggrDimora,"geo")
write.csv(GEM_Com_AggrDimora,file = paste(directoryddf,"ddf--entities--geo--dimora.csv",sep = "/"),
          row.names = FALSE,fileEncoding = "UTF-8",quote=FALSE)

#-- leggo indicatori AMB (OML) - tod, tod_f, tod_m, alq, alq_f, alq_m, occ, dis
occ <- fread(paste(directorydati,"MCharts_occupazione.tsv"   ,sep = "/"))
dis <- fread(paste(directorydati,"MCharts_disoccupazione.tsv",sep = "/"))
occdis <- merge(occ,dis,by=c("jj","gem","Sesso"))
occdis[is.na(occdis)] <- 0
occdis <- occdis[,list(occ=round(sum(occupati)),
                       occ_f=round(sum(occupati[Sesso == 'F'])),  
                       occ_m=round(sum(occupati[Sesso == 'M'])),
                       app=round(sum(apprendisti)),
                       app_f=round(sum(apprendisti[Sesso == 'F'])),  
                       app_m=round(sum(apprendisti[Sesso == 'M'])),
                       ind=round(sum(indeterminato)),
                       ind_f=round(sum(indeterminato[Sesso == 'F'])),  
                       ind_m=round(sum(indeterminato[Sesso == 'M'])),
                       tod=round(sum(fl1564)/sum(pop1564)*100,1),
                       tod_f=round(sum(fl1564[Sesso == 'F'])/sum(pop1564[Sesso == 'F'])*100,1),  
                       tod_m=round(sum(fl1564[Sesso == 'M'])/sum(pop1564[Sesso == 'M'])*100,1),
                       dis=round(sum(disoccupati)),
                       dis_f=round(sum(disoccupati[Sesso == 'F'])),  
                       dis_m=round(sum(disoccupati[Sesso == 'M'])),
                       alq=round(sum(dis1564)/(sum(dis1564)+sum(fl1564))*100,1),
                       alq_f=round(sum(dis1564[Sesso == 'F'])/(sum(dis1564[Sesso == 'F'])+sum(fl1564[Sesso == 'F']))*100,1), 
                       alq_m=round(sum(dis1564[Sesso == 'M'])/(sum(dis1564[Sesso == 'M'])+sum(fl1564[Sesso == 'M']))*100,1))
                 ,by=c("gem","jj")]

#-- leggo indicatori provenienti da ASTAT Qlikview
astat <- read.xlsx(paste(directorymain, 'd','DatiComunaliExportDaQV.xlsx',sep="/"), sheet = 1)
setDT(astat)
astat$gem <- as.integer(astat$gem)
astat$year <- as.integer(astat$year)

#-- PCS = percentuale stranieri / Anteil Ausländische Staatsbürger
astat[,pcs := round((1-(astat$ita/astat$pop))*100,1)]

#-- GGperm = giorni di permanenza / Aufenthaltsdauer in Tagen
astat[,ggperm := round(astat$presenze/astat$arrivi,1)]

#-- variazione popolazione - Bevölkerungsentwicklung
pluto <- subset(astat,select=c(gem,year,pop))
pluto$year <- pluto$year+1
pluto <- pluto[year<=max(astat$year)]
setnames(pluto,"pop","pop_prec")
astat <- merge(astat,pluto,by=c("gem","year"),all.x = TRUE)
astat[,popvar := round((pop-pop_prec)*1000/pop_prec,1)  ]
rm(pluto)

#-- Entrate dei comuni pro capite - Einnahmen der Gemeinden (pro Kopf) 
astat[,en_pc := round(entrate_com/pop)]

#-- Spesa dei comuni pro capite - Ausgaben der Gemeinden (pro Kopf)
astat[,sp_pc := round(spese_com/pop)]

#-- merge pippo con occdis
setnames(occdis,"jj"  ,"time")
setnames(astat ,"year","time")
astat <- subset(astat,select = c("gem","time","pop","pcs","ggperm","en_pc","sp_pc","mig","presenze","popvar","punti_vendita"))

#-- aggiungo il valore dell'ultimo anno che da ASTAT non esiste ancora
new <- astat[time==fullyear-1]
new$time <- fullyear
astat <- rbind(astat[time!=fullyear],new)

ddf <- merge(astat,occdis,by=c("gem","time"),all.x = T)
ddf <- ddf[complete.cases(ddf), ]

#-- dimoraDC - Bezirke
ddf2 <- merge(ddf,subset(GEM,select = c("gem","Com_AggrCP")),by="gem")
pippo <- ddf2[,list(pop=sum(pop),ggperm=mean(ggperm),presenze=sum(presenze),punti_vendita=sum(punti_vendita)) ,by=c("time","Com_AggrCP")]



#-- ddf--datapoints--<indicators>--by--<dimensions>.csv

ddf <- merge(ddf,subset(GEM,select = c("gem","short")),by="gem")
setcolorder(ddf,c("short"))
ddf$gem <- NULL
setnames(ddf,c("short"),c("geo"))

write.csv(ddf,file = paste(directoryddf,'ddf--datapoints--indicators--by--geo--time.csv',sep = "/"),
          row.names = FALSE,fileEncoding = "UTF-8",quote=FALSE)

#-- read file concepts.xlsx - sheet CONCEPT in d folder

concepts <- read.xlsx(paste(directorydati,"concepts.xlsx", sep="/"),sheet = "concept")
setDT(concepts)

if(Lingua=="Deutsch") col_to_remove <- "_IT$" else col_to_remove <- "_DE$"
drop.cols <- grep(col_to_remove,colnames(concepts))
concepts[, (drop.cols) := NULL]

colnames(concepts) <- sub(paste0("_",toupper(substr(Lingua,1,2))), "", colnames(concepts))

write.csv(concepts,
          file= paste(directoryddf,"ddf--concepts.csv",sep="/"),
          row.names = FALSE,
          fileEncoding = "UTF-8",
          quote=match(c("description","name","name_catalog","name_short","tags","scales","source","drill_up"),colnames(concepts)),
          na=""
          )

#-- read file concepts.xlsx - sheet TAG in d folder

tags <- read.xlsx(paste(directorydati,"concepts.xlsx", sep="/"),sheet = "tag")
setDT(tags)

if(Lingua=="Deutsch") col_to_remove <- "_IT$" else col_to_remove <- "_DE$"
drop.cols <- grep(col_to_remove,colnames(tags))
tags[, (drop.cols) := NULL]

colnames(tags) <- sub(paste0("_",toupper(substr(Lingua,1,2))), "", colnames(tags))

write.csv(tags,
          file= paste(directoryddf,"ddf--entities--tag.csv",sep="/"),
          row.names = FALSE,
          fileEncoding = "UTF-8",
          quote=match(c("name"),colnames(tags)),
          na=""
)



