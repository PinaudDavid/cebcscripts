

# ncdf

library(raster)
library(rgdal)

a <- raster("T:/SIG/Echange/DavidP/Documents/Papiers/ArticlesDP/Karine/Alcidae/MODIS-AQUA__C6__SST_v2014.0__4km__201607__UHAM-ICDC__fv0.02_302.6E-304.6E_46.0N-47.5N.nc4/MODIS-AQUA__C6__SST_v2014.0__4km__20160701__UHAM-ICDC__fv0.01_302.600E-304.600E_46.0000N-47.5000N.nc4")


# resolution en m d'un raster 'a' (en WGS84) :
midx <- floor(ncol(a)/2)
midy <- floor(nrow(a)/2)
mida <- crop(a, extent(a, midx, midx+1, midy, midy)) # les deux cellules au centre de a
mida[] <- c(1, 1) #  pour s'assurer que pas de NA
spmida <- rasterToPoints(mida, spatial=TRUE)
max(pointDistance(spmida, lonlat=TRUE), na.rm=TRUE) # distance entre les centres de 2 cellules au centre du raster, en mètres




a <- stack("T:/SIG/Echange/DavidP/Documents/Papiers/ArticlesDP/Karine/Alcidae/MODIS-AQUA__C6__SST_v2014.0__4km__201607__UHAM-ICDC__fv0.02_302.6E-304.6E_46.0N-47.5N.nc4/MODIS-AQUA__C6__SST_v2014.0__4km__20160701__UHAM-ICDC__fv0.01_302.600E-304.600E_46.0000N-47.5000N.nc4")


