####################################################################################
###### Téléchargement de données météo sur ftp://ftp.ncdc.noaa.gov/pub/data/gsod  avec R

### David Pinaud CEBC-CNRS/ULR 
### pinaud@cebc.cnrs.fr, 12/02/2016

# les lignes suivantes sont à modifier (marqué **A DEFINIR**) et copier-coller sour R

# Marche à suivre :
#	- repérer le code USAF des stations d'intérêt
#	- renseigner ce code et l'année dans le script (création d'un fichier de référence)
#	- récupérer les fichiers disponibles pour les stations et les années (zippés) sur le serveur et les copier en local
#	- décompresser et donne un fichier texte par an et station (.txt, lisible sous Excel)
#


######### Les codes des stations ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt :
# Integrated Surface Hourly Database Station History, March 2007
#
#USAF = Air Force Datsav3 station number
#WBAN = NCDC WBAN number
#CTRY = WMO historical country ID, followed by FIPS country ID
#ST = State for US stations
#CALL = ICAO call sign
#LAT = Latitude in thousandths of decimal degrees
#LON = Longitude in thousandths of decimal degrees
#ELEV = Elevation in tenths of meters
#BEGIN = Beginning Period Of Record (YYYYMMDD). There may be reporting gaps within the P.O.R.
#END = Ending Period Of Record (YYYYMMDD). There may be reporting gaps within the P.O.R.
#
#Notes:
#- Missing station name, etc indicate the metadata are not currently available.
#- The term "bogus" indicates that the station name, etc are not available.
#- For a small % of the station entries in this list, climatic data are not 
#  available. These issues will be addressed. To determine data availability 
#  for each location, see the 'ish-inventory.txt' or 'ish-inventory.csv' file. 
#
# Quelques stations d'intérêt :
#USAF   WBAN  STATION NAME                  CTRY  ST CALL  LAT    LON     ELEV(.1M) BEGIN    END
#619960 99999 MARTIN DE VIVIES /I           FR FS          -37800 +077500 +00290    19730101 20120618
#619970 99999 ALFRED FAURE /ILES            FR FS          -46433 +051850 +01420    19740903 20120618
#619980 99999 PORT-AUX-FRANCAIS             FR FS          -49300 +070200 +00300    19730101 20120618
#619800 99999 SAINT-DENIS/GILLOT            RE RE    FMEE  -20900 +055533 +00250    19730101 20120618
#896420 99999 DUMONT DURVILLE               AY AY          -66667 +140017 +00430    19850701 20120618
#896430 99999 PORT MARTIN                   AY AY          -66817 +141383 +00390    19910924 20090131
#073300 99999 NIORT                         FR FR    LFBN  +46316 -000400 +00610    19860101 20130705

# le listing des inventaires disponibles : ftp://ftp.ncdc.noaa.gov/pub/data/inventories/ISH-INVENTORY.TXT  

## The daily elements included in the dataset (as available from each station) are:
#Mean temperature (.1 Fahrenheit)
#Mean dew point (.1 Fahrenheit)
#Mean sea level pressure (.1 mb)
#Mean station pressure (.1 mb)
#Mean visibility (.1 miles)
#Mean wind speed (.1 knots)
#Maximum sustained wind speed (.1 knots)
#Maximum wind gust (.1 knots)
#Maximum temperature (.1 Fahrenheit)
#Minimum temperature (.1 Fahrenheit)
#Precipitation amount (.01 inches)
#Snow depth (.1 inches)
#Indicator for occurrence of:  Fog
#                              Rain or Drizzle
#                              Snow or Ice Pellets
#                              Hail
#                              Thunder
#                              Tornado/Funnel Cloud
#		
# plus de détails sur ftp://ftp.ncdc.noaa.gov/pub/data/gsod/GSOD_DESC.txt


#### début du script :

# définir le répertoire existant où seront placés les fichiers **A DEFINIR**
setwd("D:/Documents/Outils/R/Scripts R/ExtractionDataInternet")  

    ## D'abord, tu dois renseigner les années et stations d'intérêt (dans un dataframe "dat2") 

        # exemple avec 2 stations et 5 années:     **A DEFINIR**
year <-  c(1999, 2000, 2001, 2002, 2003)   # *liste des annees qui t'interessent*
USAF <-  c("017452", "562488")   # *code USAF des stations qui t'interessent* 
WBAN <-  c(99999, 99999)   # *code WBAN*


# copier les lignes suivantes jusqu'à la fin :

dat2 <- expand.grid(year=year, USAF=USAF, WBAN=WBAN)  # objet qui liste les stations d'intérêt
      # liste des chemins d'accès :
meteopaths <- paste("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/", dat2$year, "/", dat2$USAF, "-", dat2$WBAN, "-", dat2$year, ".op.gz", sep= "")
  #tu trouves ces codes dans le metafichier ISH-inventory.txt recuperable sur ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt

      ## la fonction downloadNCDC telecharge un fichier dont tu as donne l'annee, le WBAN et USAF codes
downloadNCDC <- function(i)
    {
    download.file(zz <- meteopaths[i], destfile= paste(dat2$USAF[i], "-", dat2$WBAN[i], "-", dat2$year[i], ".op.gz", sep= ""), method= "internal", mode= "wb")
   }
      ## boucle pour télécharger tous les fichiers (même si pas dispo), test pour chaque fichier et ajout du résultat (disponible ou pas) dans "dat2$Metavail"
      # un fichier est créé sur le disque dur pour chaque station (dispo ou pas)
test <- vector("list", nrow(dat2))   # objet d'accueil
              # boucle  d'importation et de test
for(i in 1:nrow(dat2)){
  test[[i]] <- try(downloadNCDC(i))
}
 dat2$Metavail<- sapply(test, function(x) !inherits(x, "try-error"))   # répond T ou F si pas d'erreur
       ## boucle pour lire et transformer les fichiers (seulement les disponibles) sous R à partir du disque dur
        # header :
heade <- c("STN---", "WBAN", "YEARMODA", "TEMP", "count.temp", "DEWP", "count.dewp", "SLP", "count.slp", "STP", "count.stp", "VISIB", "count.visib",  "WDSP", "count.wdsp", "MXSPD", "GUST", "MAX", "flag.max", "MIN", "flag.min", "PRCP", "flag.prcp", "SNDP", "fog", "rain", "snow", "hail", "thunder", "tornado")
        # boucle pour chaque fichier :
for(i in 1:nrow(dat2))
  {
  if(dat2$Metavail[i])  # pour les fichiers dispo uniquement
      {
            # connexion
      dd <- readLines(zz <- gzfile(paste(dat2$USAF[i], "-", dat2$WBAN[i], "-", dat2$year[i], ".op.gz", sep= ""), "r"))
      close(zz)
            # écriture du fichier brut
      write.table(dd, "dd.txt", sep="\t", quote=F, row.names=F)
            # importation avec les colonnes délimitées
	  a <- read.fwf("dd.txt", skip=2, widths=c(6, -1, 5, -2, 8, -2, 6, -1, 2, -2, 6, -1, 2, -2, 6, -1, 2, -2, 6, -1, 2, -2, 5, -1, 2, -2, 5, -1, 2, -2, 5, -2, 5, -2, 6, 1, -1, 6, 1, -1, 5, 1, -1, 5, -2, 1, 1, 1, 1, 1, 1))
            # remplacement du header
      colnames(a) <- heade
	  a[, paste(heade[1])] <- as.character(rep(dat2$USAF[i], nrow(a)))
            # exportation en .txt, nom du fichier = nom de la station (codeUSAF puis codeWBAN) et année
      write.table(a, paste(dat2$USAF[i], "-", dat2$WBAN[i], "-", dat2$year[i], ".txt", sep = ""), sep="\t", quote=F, row.names=F)
      rm(a, dd)
      }
  }

         # variante : 1 seul fichier en sortie :
i <- min(which(dat2$Metavail))
dd <- readLines(zz <- gzfile(paste(dat2$USAF[i], "-", dat2$WBAN[i], "-", dat2$year[i], ".op.gz", sep= ""), "r"))
close(zz)
	# écriture du fichier brut
write.table(dd, "dd.txt", sep="\t", quote=F, row.names=F)
	# importation avec les colonnes délimitées
a <- read.fwf("dd.txt", skip=2, widths=c(6, -1, 5, -2, 8, -2, 6, -1, 2, -2, 6, -1, 2, -2, 6, -1, 2, -2, 6, -1, 2, -2, 5, -1, 2, -2, 5, -1, 2, -2, 5, -2, 5, -2, 6, 1, -1, 6, 1, -1, 5, 1, -1, 5, -2, 1, 1, 1, 1, 1, 1))
	# remplacement du header
colnames(a) <- heade
a[, paste(heade[1])] <- as.character(rep(dat2$USAF[i], nrow(a)))
mm <- a[FALSE,]
 
for(i in 1:nrow(dat2))
  {
  if(dat2$Metavail[i])  # pour les fichiers dispo uniquement
      {
            # connexion
      dd <- readLines(zz <- gzfile(paste(dat2$USAF[i], "-", dat2$WBAN[i], "-", dat2$year[i], ".op.gz", sep= ""), "r"))
      close(zz)
            # écriture du fichier brut
      write.table(dd, "dd.txt", sep="\t", quote=F, row.names=F)
            # importation avec les colonnes délimitées
	  a <- read.fwf("dd.txt", skip=2, widths=c(6, -1, 5, -2, 8, -2, 6, -1, 2, -2, 6, -1, 2, -2, 6, -1, 2, -2, 6, -1, 2, -2, 5, -1, 2, -2, 5, -1, 2, -2, 5, -2, 5, -2, 6, 1, -1, 6, 1, -1, 5, 1, -1, 5, -2, 1, 1, 1, 1, 1, 1))
            # remplacement du header
      colnames(a) <- heade
	  a[, paste(heade[1])] <- as.character(rep(dat2$USAF[i], nrow(a)))
            # on colle tout
	  mm <- rbind(mm, a)	
      rm(a, dd)
      }
  }
write.table(mm, paste("donnéesMétéo_", Sys.Date(), ".txt", sep=""), sep="\t", quote=F, row.names=F)
 
  
  
### fin

a <- read.table(paste(dat2$USAF[i], "-", dat2$WBAN[i], "-", dat2$year[i], ".txt", sep = ""), sep="\t", h=T)
a <- mm
#Mean temperature (.1 Fahrenheit)
#Mean dew point (.1 Fahrenheit)
#Mean sea level pressure (.1 mb)
#Mean station pressure (.1 mb)
#Mean visibility (.1 miles)
#Mean wind speed (.1 knots)
#Maximum sustained wind speed (.1 knots)
#Maximum wind gust (.1 knots)
#Maximum temperature (.1 Fahrenheit)
#Minimum temperature (.1 Fahrenheit)
#Precipitation amount (.01 inches)
#Snow depth (.1 inches)
#Indicator for occurrence of:  Fog, Rain or Drizzle, Snow or Ice Pellets, Hail, Thunder, Tornado/Funnel Cloud



# Conversions diverses en unités métriques (°C, km/h, mm):
b <- data.frame(Date=strptime(a$YEARMODA, "%Y%m%d"), 
				MeanTemp=(a$TEMP-32)/1.8,
				MinTemp=(a$MIN-32)/1.8,
				MaxTemp=(a$MAX-32)/1.8,
				MeanSeaLevelPres=a$SLP,
				MeanWindSpeed=a$WDSP/1.852,
				PrecTot=a$PRCP*25.4)
				
plot(MeanTemp~Date, b, t="l", xaxt="n")
r <- range(b$Date)
axis.POSIXct(1, at = seq(r[1], r[2], by = "months"), format = "%b")
abline(v=seq(r[1], r[2], by = "weeks"), col="grey")
points(MeanTemp~Date, b, t="l", col="orange", lwd=2)
b$months <- cut(b$Date, seq(r[1], r[2], by = "months"))
write.table(b, paste("donnéesMétéo_", Sys.Date(), ".txt", sep=""), sep="\t", quote=F, row.names=F)


t <- tapply(b$PrecTot, b$months, sum)
mt <- tapply(b$MeanTemp, b$months, mean)
dimnames(t)[[1]]
sum(t)
barplot(t, xaxt="n")
axis.POSIXct(1, at = seq(r[1], r[2], by = "months"), "months")
