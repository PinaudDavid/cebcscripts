# calcul JOUR / NUIT


library(GeoLight)

library(rgdal)
library(adehabitatLT)  # for the dataset example

#### locals
projWGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"  # projection parameters for GPS/Argos coordinates
projUTM42 <- "+proj=utm +zone=42 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # projection parameters for UTM zone 42



	### Data preparation: load R dataset, projection to WGS1984. 
	#                         /!\ time format should be correct, with correct TimeZone

data(albatross)  # Some Wandering Albatross Argos locations from AdehabitatLT, by the team of H. Weimerskirch from the CEBC-CNRS (Chizé, France). 
alb1 <- albatross[2][[1]]
coordinates(alb1) <- ~ x + y  # promote to a SpatialPointsDataFrame
proj4string(alb1) <- CRS(projUTM42)  # set the projection
alb <- spTransform(alb1, CRS(projWGS84))
loc <- data.frame(Long=coordinates(alb)[, 1], Lat=coordinates(alb)[, 2], Date=alb@data$date)
plot(Lat ~ Long, data=loc, t="l")


	## calculation of the DateTime of sunrise and sunset at every location
loc$Hrise <- twilight(loc$Date, lon=loc$Long, lat=loc$Lat, rise=T, zenith = 96, iters = 3)  # for solar elevation -6 from horizon (zenith = 96)
loc$Hset <- twilight(loc$Date, lon=loc$Long, lat=loc$Lat, rise=F, zenith = 96, iters = 3)

	## calculation of the solar angle from the zenith (horizon = 90) at every location
s <- solar(loc$Date)
loc$Hsolar <- zenith(s, lon=loc$Long, lat=loc$Lat)  # 
		## and night/twilight/day phase
loc$phase <- cut(loc$Hsolar, breaks=c(0, 90.83, 96, 180), labels=c("day", "twilight", "night"))  # cut solar angle for every loc, using civil twilight (-6)












