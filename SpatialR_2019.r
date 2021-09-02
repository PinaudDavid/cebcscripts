
### analyses spatiales avec R

# 15/04/2019

# major read for using package "sf": https://geocompr.robinlovelace.net/


	### packages
library(rgdal)  	# import / export de fichiers spatiaux
library(sp)  		# classes 'sp'
library(mapview)  	# visualisation rapide dynamique
library(raster)		# travail avec les rasters
library(trip)		# travail avec les classes 'trip' (mouvements)
library(viridis)	# pour les couleurs
library(ggplot2)	# graphics


	#### locals
projWGS84 <- CRSargs(CRS("+init=epsg:4326")) #  projection parameters for GPS/Argos coordinates, code EPSG (http://spatialreference.org/)
# projWGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"  # équivalent à la ligne supra

setwd("P:/Ressources/Formation/R/Spatial")  
pathfile <- "P:/Ressources/Formation/R/Spatial/data/AdeliePenguin"

colo <- data.frame(Long=140.0052, Lat=-66.66322)  # colonie
coordinates(colo) <- ~ Long + Lat  # SpatialPointsDataFrame
proj4string(colo) <- CRS(projWGS84)  # définit la projection

	# données spatiales de base :
world <- readOGR("P:/Ressources/Formation/DataSIG/Continents", "World_continental_WGS84")  # import
bathy <- raster("P:/Ressources/Formation/DataSIG/Bathymetrie/etopo1_Ice_DDU_20190418.tif") # import avec "rgdal". Source: https://maps.ngdc.noaa.gov/viewers/wcs-client/



 
###############################################
### import et exploration rapide

		### trajets GPS Manchot Adélie, Kato et al
# inc : 1 trip / ind, fixed intervalle 30 min. 3 inds.
# rearing : severals trips / ind, fixed intervalle 10 min. 1 ind.
# local time : "Antarctica/DumontDUrville", GMT+10

meta <- read.table("./data/AdelieGPSmetadata.txt", sep="\t", header=TRUE)
meta$deploy.DateTime <- as.POSIXct(strptime(paste(meta$deployment_date_loc, meta$deployment_time_loc), "%d/%m/%Y %H:%M:%S"), tz="Antarctica/DumontDUrville")  # convertit en DateHeure explicite, GMT+10 (DDU) see https://twiki.org/cgi-bin/xtra/tzdatepick.html
meta$recap.DateTime <- as.POSIXct(strptime(paste(meta$recapture_date, meta$recapture_time), "%d/%m/%Y %H:%M"), tz="Antarctica/DumontDUrville")  # convertit en DateHeure explicite, GMT+10 (DDU)
ind <- levels(meta$GPS_file)

i=1 
pp <- read.table(paste(pathfile, "/", ind[i], ".csv", sep=""), sep=",", header=TRUE)  # test / initialisation avec i=1
# str(pp)
# 'data.frame':   429 obs. of  10 variables:
 # $ Date     : Factor w/ 18 levels "2015/11/19","2015/11/20",..: 1 1 1 1 1 1 1 1 1 1 ...
 # $ Time     : Factor w/ 429 levels " 00:01:59"," 00:04:55",..: 292 305 313 319 327 333 341 351 360 372 ...
 # $ Latitude : num  -66.7 -66.7 -66.7 -66.7 -66.7 ...
 # $ Longitude: num  140 140 140 140 140 ...
 # $ Altitude : num  18.6 18.6 43.8 61.3 50.1 ...
 # $ Speed    : num  288 288 0 0 0 ...
 # $ Course   : int  146 146 203 288 111 153 170 0 12 318 ...
 # $ Type     : int  -2 0 0 0 0 0 0 0 0 0 ...
 # $ Distance : num  0 0 29.71 22.68 5.36 ...
 # $ Essential: int  1 1 1 1 1 1 1 1 1 1 ...

		# ok, on va construire un seul objet avec tous les fichiers :
pp$Ind <- ind[i]
pp$DateTimeTU <- as.POSIXct(strptime(paste(pp$Date, pp$Time), "%Y/%m/%d %H:%M:%S"), "Antarctica/DumontDUrville")  # convertit en DateHeure explicite, GMT+10
pp <- pp[pp$DateTimeTU > meta$deploy.DateTime[meta$GPS_file==ind[i]] & pp$DateTimeTU < meta$recap.DateTime[meta$GPS_file==ind[i]], ]

for(i in 2:nrow(meta))
	{
	ppi <- read.table(paste(pathfile, "/", ind[i], ".csv", sep=""), sep=",", header=TRUE)  # test / initialisation avec i=1
	ppi$Ind <- ind[i]
	ppi$DateTimeTU <- as.POSIXct(strptime(paste(ppi$Date, ppi$Time), "%Y/%m/%d %H:%M:%S"), "Antarctica/DumontDUrville")  # convertit en DateHeure explicite, GMT+10
	ppi <- ppi[ppi$DateTimeTU > meta$deploy.DateTime[meta$GPS_file==ind[i]] & ppi$DateTimeTU < meta$recap.DateTime[meta$GPS_file==ind[i]], ]

	pp <- rbind(pp, ppi)
	}
rm(ppi)	


		# au cas où on n'aurait pas un metafile, on peut récupérer les noms des fichiers dans le répertoire
# lf <- list.files(path = pathfile)  # liste les fichiers dans le répertoire, trajets GPS Manchot Adélie, Kato et al
# ind <- substr(lf, start=1, stop=nchar(lf)-4)  # extrait le nom des individus
# i=1 
# pp <- read.table(paste(pathfile, lf[i], sep="/"), sep=",", header=TRUE)  # trajets GPS Manchot Adélie, Kato et al

pp$Ind <- factor(pp$Ind)  # as factor
pp <- pp[order(pp$Ind, pp$DateTimeTU), ]

ppdf <- pp # copie, au cas où
coordinates(pp) <- ~ Longitude + Latitude  # promote to SpatialPointsDataFrame
proj4string(pp) <- CRS(projWGS84)  # définit la projection

library(mapview)
mapView(pp, z="Ind")  # exploration carto dynamique


### passage en "trip" et calcul de différents paramètres


pp <- remove.duplicates(pp, zero = 0.00000000)  # remove duplicates based on distance between locations
library(trip)
tpp <- trip(pp, TORnames=c("DateTimeTU", "Ind"))

bbox(tpp)
ze <- extent(c(135.8, 145, -67.5, -62))  # xmin, xmax, ymin, ymax

windows(width=15, height=19)
library(viridis)
plot(bathy, ext=ze, col=cividis(500, alpha=0.7)[0:400], main="Adélie Penguins")
contour(bathy, add=TRUE, levels=c(-4000, -3000, -2000, -1000, -500), col="grey")
plot(world, add=T, lwd=1.5, col=gray(0.9))
lines(tpp, lwd=2)
plot(tpp, pch=".", add=T)
scalebar(d=100, xy = c(136.5, -67.2), type = "bar", below = "Km", lonlat = TRUE, cex=0.7, adj=c(0.5, -1.50))
box()

g <- as(tpp, "SpatialLinesDataFrame")
mapView(g, z="tripID")

	### distance et angle entre chaque loc :
tpp@data$dist <- trackDistance(tpp)  # km
tpp@data$ang <- trackAngle(tpp)  # degré

	### intertime (ici en secs)
tpp@data$intertime <- c(0, difftime(getTimeID(tpp)[2:nrow(tpp), 1], getTimeID(tpp)[1:(nrow(tpp)-1), 1], units="secs"))	
tpp@data$intertime[tapply(1:nrow(tpp), getTimeID(tpp)[, 2], min)] <- 0  # pour les changements de trajets

	### vitesse
tpp@data$speed <- tpp@data$dist / (tpp@data$intertime/3600)	# speed in km/h
tpp@data$speed[tapply(1:nrow(tpp), getTimeID(tpp)[, 2], min)] <- NA  # pour les changements de trajets

	### dist à la colo:
tpp@data$dist2colo <- spDistsN1(tpp, colo, longlat=TRUE) # distance à a colonie pour chaque loc
ggplot(data=tpp@data, aes(x=DateTimeTU, y=dist2colo, color=Ind)) + geom_line()
tppi <- tpp[tpp@data$Ind  %in%  meta$GPS_file[meta$Stage=="incubation"], ]  # les trajets en incubation
ggplot(data=tppi@data, aes(x=DateTimeTU, y=dist2colo, color=Ind)) + geom_line()

hist(tpp@data$speed, nclass=100, col="green")
summary(tpp@data$speed)
plot(speed ~ intertime, tpp@data)
plot(speed ~ intertime, tpp@data, xlim=c(500, 2000))  # super penguins!!!!

	### au cas où, speed filter:
sf <- speedfilter(tpp, max.speed = 8, test = FALSE) # speed filter with max speed (algorithm from McConnell et al. (1992))
tpp2 <- tpp  # copie
tpp <- tpp[sf,]  
plot(tpp2)
lines(tpp2, col="grey")
lines(tpp)
		# recalcul des vitesses etc :
tpp@data$dist <- trackDistance(tpp)  # km
tpp@data$ang <- trackAngle(tpp)  # degré
tpp@data$intertime <- c(0, difftime(getTimeID(tpp)[2:nrow(tpp), 1], getTimeID(tpp)[1:(nrow(tpp)-1), 1], units="secs"))	
tpp@data$intertime[tapply(1:nrow(tpp), getTimeID(tpp)[, 2], min)] <- 0  # pour les changements de trajets
tpp@data$speed <- tpp@data$dist / (tpp@data$intertime/3600)	# speed in km/h
tpp@data$speed[tapply(1:nrow(tpp), getTimeID(tpp)[, 2], min)] <- NA  # pour les changements de trajets
tpp@data$dist2colo <- spDistsN1(tpp, colo, longlat=TRUE) # distance à a colonie pour chaque loc
hist(tpp@data$speed, nclass=100, col="green")
summary(tpp@data$speed)


	###  exemple de calcul synthétique par trajet 
homedist(tpp, home = coordinates(colo))  # distance maxi à la colonie pour chaque trip (ou au premier point, )global
tapply(tpp@data$speed, tpp@data$Ind, mean, na.rm=TRUE)  # distance moyenne etc...


		# on peut aussi convertir le trip en 'ltraj' (calcul direct de différents paramètres)
lpp <- as(tpp2, "ltraj")


### calcul JOUR / NUIT
library(GeoLight)

	## calculation of the DateTime of sunrise and sunset at every location
Hrise <- twilight(tpp@data$DateTimeTU, lon=coordinates(tpp)[, 1], lat=coordinates(tpp)[, 2], rise=T, zenith = 96, iters = 3)  # for solar elevation -6 from horizon (zenith = 96)
Hset <- twilight(tpp@data$DateTimeTU, lon=coordinates(tpp)[, 1], lat=coordinates(tpp)[, 2], rise=F, zenith = 96, iters = 3)  # for solar elevation -6 from horizon (zenith = 96)

	## ctppulation of the solar angle from the zenith (horizon = 90) at every location
s <- solar(tpp@data$DateTimeTU)
tpp@data$Hsolar <- zenith(s, lon=coordinates(tpp)[, 1], lat=coordinates(tpp)[, 2])  # 
hist(tpp@data$Hsolar)		
## and night/twilight/day phase
tpp@data$phase <- cut(tpp@data$Hsolar, breaks=c(0, 90.83, 96, 180), labels=c("day", "twilight", "night"))  # cut solar angle for every loc, using civil twilight (-6)
table(tpp@data$phase)


save(tpp, file="locAdelie_tpp.RData")



##### animation des trajets avec moveVis
library(raster)
library(rgdal)
library(moveVis)
library(RColorBrewer)
library(ggplot2)
library(ggspatial)
library(sf)

	#### locals
projWGS84 <- CRSargs(CRS("+init=epsg:4326")) #  projection parameters for GPS/Argos coordinates, code EPSG (http://spatialreference.org/)
# projWGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"  # équivalent à la ligne supra

setwd("P:/Ressources/Formation/R/Spatial")  
pathfile <- "P:/Ressources/Formation/R/Spatial/data/AdeliePenguin"

colo <- data.frame(Long=140.0052, Lat=-66.66322)  # colonie
coordinates(colo) <- ~ Long + Lat  # SpatialPointsDataFrame
proj4string(colo) <- CRS(projWGS84)  # définit la projection

	# données spatiales de base :
world <- st_read("P:/Ressources/Formation/DataSIG/Continents", "World_continentalMulti_WGS84")  # import
world <- readOGR("P:/Ressources/Formation/DataSIG/Continents", "World_continentalMulti_WGS84")  # import

bathy <- raster("P:/Ressources/Formation/DataSIG/Bathymetrie/etopo1_Ice_DDU_20190418.tif") # import avec "rgdal". Source: https://maps.ngdc.noaa.gov/viewers/wcs-client/

	# metadonnées :
meta <- read.table("./data/AdelieGPSmetadata.txt", sep="\t", header=TRUE)
meta$deploy.DateTime <- as.POSIXct(strptime(paste(meta$deployment_date_loc, meta$deployment_time_loc), "%d/%m/%Y %H:%M:%S"), "Antarctica/DumontDUrville")  # convertit en DateHeure explicite, GMT+10 (DDU) see https://twiki.org/cgi-bin/xtra/tzdatepick.html
meta$recap.DateTime <- as.POSIXct(strptime(paste(meta$recapture_date, meta$recapture_time), "%d/%m/%Y %H:%M"), "Antarctica/DumontDUrville")  # convertit en DateHeure explicite, GMT+10 (DDU)
ind <- levels(meta$GPS_file)

	# données trajets
load("locAdelie_tpp.RData")
dpp <- as(tpp, "data.frame")
dppi <- dpp[dpp$Ind  %in%  meta$GPS_file[meta$Stage=="incubation"], ]  # les trajets en incubation
dppi <- droplevels(dppi)
coastline <- st_crop(world, bbox(bathy))

mppi <- df2move(dppi, proj = projWGS84, x = "Longitude", y = "Latitude", time = "DateTimeTU", track_id = "Ind")
m <- align_move(mppi, res = 10, unit = "hours")  # même résolution temporelle pour tous les trajets, 10 hours (pour test !!!)


lb <- list()
lb[[1]] <- bathy
lt <- list()
lt[[1]] <- mean(dppi$DateTimeTU)


frames <- frames_spatial(m,  r_list=lb, r_times=mean(dppi$DateTimeTU), fade_raster=FALSE, alpha = 0.8)  # static local map, ~1 min
frames <-  add_timestamps(frames, type = "label")

framesb <- add_gg(frames, gg=expr(geom_sf(data=world, aes())), data=world)
framesb <- add_gg(frames, gg=expr(list(geom_sf(data=world, aes(colour='grey', fill='white')), xlim(bbox(bathy)[1,]), ylim(bbox(bathy)[2,]))), data=world)

frames <- frames_spatial(m,  map_service = "osm", map_type = "topographic")  # static local map, ~1 min


ggplot(world, aes(x = long, y = lat, group = "ID")) + 
   geom_polygon(colour='black', fill='white')  # nop
   
ggplot(world) +
  geom_sf(colour='grey', fill='white')     # ok

ggplot(world) + geom_sf(colour='grey', fill='white') + xlim(bbox(bathy)[1,]) + ylim(bbox(bathy)[2,]) # ok


animate_frames(frames, out_file = "test1.mov")  # ~3 min


# with osm watercolor base map:
frames <- frames_spatial(m, map_service = "osm", map_type = "watercolor", alpha = 0.5)  # ~1 min
animate_frames(frames, out_file = "test1.mov")





















pic <- ggplot(dppi, aes(
    x = Longitude, 
    y = Latitude, 
    colour = Ind)) + 
  geom_point(show.legend = FALSE,
             size = .15, alpha = 1) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  transition_time(time = DateTimeTU)  +
  ease_aes('linear') + 
  shadow_wake(.1)
   
# create
animate(pic, nframes = 500, fps = 20)
anim_save("ess1.gif")




#### a function to group successive TRUE events of a vector with the same group number. David Pinaud 2015-07-06 R version 3.2.1 (2015-06-18)
grpt <- function(condition) {    
		a1 <- condition
		a2 <- a1
		a3 <- a1
		a2[2:length(a1)] <-  a1[1:(length(a1)-1)]  # décalage vers le bas 
		a2[1] <- FALSE
		a3 <- ifelse(a1 != a2, T, F)  # détection des entrées et des sorties des séquences
		grp <- vector(mode="numeric", length=length(a1))
		indice <- 1
		for (j in 1:length(a1))
		    {
			    if (a1[j]==TRUE & a3[j]==TRUE)   # changement
			     {
				     grp[j] <- indice  # numérotation des séquences
				     indice <- indice + 1
			     }
		      if (a1[j]==TRUE & a3[j]==FALSE)  # cas où la séquense est la même
		       {
			       grp[j] <- grp[j-1]
		       }
		        
		    }
		rm(a1, a2, a3) 
		grp[grp==0] <- NA
    return(grp)
  }

## partionning:
d <- 0.5  # set the distance to the nest threshold (km)
alc@data$distToNest <- spDistsN1(pts=alc, pt=nestXY, longlat = T)  # distance between each location and the nest, in m

library(ggplot2)

ggplot(data=alc@data, aes(x=date, y=distToNest, colour=id.track)) +
	geom_line(show.legend = FALSE)


plot(alc@data$date, alc@data$distToNest, t="l", col=alc@data$id.track)
abline(h=d, col="red")        # add a horizontal line (at the threshold)

grTrip <- grpt(tr@data$distToNest > d)  # a vector with the trip ID 
points(tr@data$date, tr@data$distToNest, col=grTrip)



