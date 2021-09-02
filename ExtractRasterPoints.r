# Pour croiser les données d'un raster à des points (locs GPS ou autre), il faut bien s’assurer que les deux couches ont la même projection :

library(raster)
projWGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"  # parameters WGS1984

bat <- raster("Tes Documents / ton fichier raster")  # import raster bathy
crs(bat) <- projWGS84 # force la projection

loc <- readOGR(“tes locs”) # import shapefile locs
proj4string(loc) <- CRS(projWGS84) # force la projection

loc$bat <- bat[loc,]  # extrait la valeur du raster bat sous chaque loc et la colle dans le slot @data (« table attributaire »)
