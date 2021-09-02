################  cut movement coordinates of  an individual based on the distance to a specific point (nest...), -> successive trips



#### libraries and functions
library(sp)
library(rgdal)
library(trip)
library(adehabitatLT)  # for the dataset example
library(maps)  # to draw some world maps
library(mapdata)  # to draw some world maps

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




#### locals
projWGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"  # projection parameters for GPS/Argos coordinates
projUTM42 <- "+proj=utm +zone=42 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # projection parameters for UTM zone 42


### EXAMPLE

### Data preparation: load R dataset and conversion to a trip object, coords of the nest.
data(albatross)  # Some Wandering Albatross Argos locations from AdehabitatLT, by the team of H. Weimerskirch from the CEBC-CNRS (Chizé, France). 
alb1 <- albatross[6][[1]]
alb1$ID <- rep("8337", nrow(alb1))
coordinates(alb1) <- ~ x + y  # promote to a SpatialPointsDataFrame
proj4string(alb1) <- CRS(projUTM42)  # set the projection
tr <- trip(alb1, c("date", "ID"))  # promote to a 'trip' object

nestXY <- tr[1, ] # coords of the nest (= 1st position)

		# verification by plotting:
plot(coordinates(tr), type="n", asp=mapasp(tr))
map('worldHires', add=T, fill=T, col=gray(0.9))
plot(as(tr, "SpatialLinesDataFrame"), col = "red", add=T)
points(nestXY, pch=24, bg="darkgreen", cex=1.3)

## partionning:
d <- 25000  # set the distance to the nest threshold (in CRS units, here m)

tr@data$distToNest <- spDistsN1(pts=tr, pt=nestXY, longlat = F)  # distance between each location and the nest, in m

plot(tr@data$date, tr@data$distToNest, t="l")
abline(h=d, col="red")        # add a horizontal line (at the threshold)

grTrip <- grpt(tr@data$distToNest > d)  # a vector with the trip ID 
points(tr@data$date, tr@data$distToNest, col=grTrip)

trs <- explode(tr)
writeOGR(trs, "T:/SIG/Echange/DavidP/Documents/CEBC/ProgrammeRecherche/Chiro/MaximeL/Annepont2015/data/SIG", "cumulPathPointsRF_170320", driver="ESRI Shapefile", overwrite_layer=TRUE)