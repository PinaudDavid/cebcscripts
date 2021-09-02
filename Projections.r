library(sp)
library(raster)
library(rgdal)


# WGS1984 EPSG:4326
projWGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# WGS84 projection definition EPSG:7030 projLonLat <- '+proj=longlat
# +ellps=WGS84'
projWGS84 <- CRSargs(CRS("+init=epsg:4326"))


# 'NTF-Lambert II étendu' = 'NTF (Paris) / Lambert zone II' ; ID 10090
# EPSG:27572
projL2E <- "+proj=lcc +nadgrids=ntf_r93.gsb,null +a=6378249.2000 +rf=293.4660210000000 +pm=2.337229167 +lat_0=46.800000000 +lon_0=0.000000000 +k_0=0.99987742 +lat_1=46.800000000 +x_0=600000.000 +y_0=2200000.000 +units=m +no_defs"

# European Proj ETRS89 LAEA ; EPSG:3035
projETRS <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

# RGF93-Lambert93 ; EPSG:2154
projL93 <- "+init=epsg:2154 +proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
projL93 <- CRSargs(CRS("+init=epsg:2154"))

# 'x' est un data.frame, dont les colonnes "Longitude" et "Latitude" donnent des valeurs de coordonnées GPS (WGS84)
coordinates(x) <- ~ Longitude + Latitude		# convertit 'x' en SpatialPointsDataFrame
proj4string(x) <- CRS(projWGS84)		# définie la projection de 'x' en WGS84
x_L93 <- spTransform(x, CRS = CRS(projL93)) 	# reprojete 'x' en Lambert93
x_L93 <- spTransform(x, CRS = CRS("+init=epsg:2154")) # idem, variante avec code EPSG

# reprojection d'un raster R1 avec un autre raster Rb en référence (CRS)
Rap <- projectRaster(from=Ra, to=Rb) 




########################################
#### projection Albers Equal Area (conique)


library(rgdal)


# 'spa' est un objet data.frame avec les coordonnées GPS en WGS1984

projWGS84 <- CRSargs(CRS("+init=epsg:4326"))   # les paramètres de projection pour WGS84


coordinates(spa) <- ~ lon + lat		# convertit 'spa' en SpatialPointsDataFrame
proj4string(spa) <- CRS(projWGS84)		# définie la projection de 'spa' en WGS84


spa@bbox  # l'étendu des coordonnées
           # min       max
# lon  -9.965302 128.53578
# lat -70.069900 -38.91609

apply(coordinates(spa), 2, mean)  # les moyennes des coordonnées
     # lon       lat 
 # 62.24285 -56.34974

# reprojete 'spa' en Albers Equal Area (conique) en se calant sur l'étendu des données (coords en metre en sortie). 
# On utilise l'étendue en latitude (+lat_1=XXX +lat_2=XXX) et la moyenne de la longitude (+lon_0=XXX) pour les paramètres de projection :
spp <- spTransform(spa, CRS = CRS("+proj=aea +lat_1=-70 +lat_2=-38 +lon_0=62 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 	



# ckeck plot

world <- readOGR("P:/Ressources/Formation/DataSIG/Continents", "World_continental_WGS84")  # on charge le shp world (WGS84)
wp <- spTransform(world, CRS = CRS("+proj=aea +lat_1=-70 +lat_2=-38 +lon_0=62 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 	# reprojete 'world' en Albers Equal Area (conique) en se calant sur l'étendu des données des eleph

		# plot en proj (units = metres):
plot(spp, axes=T)
plot(wp, add=T)

		# plot en WGS84
plot(spa, axes=T)
plot(world, add=T)