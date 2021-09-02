
#### construction raster veget Chizé





###########################
# MASKs :

library(raster)
library(rgdal)

	# MNS à 25 cm
mns25 <- raster("E:/Documents/LongTermeForet/Orthophotos/diffMNSMNT_025.tif")
mns1 <- aggregate(mns25, fact=4, fun=mean)
writeRaster(mns1, filename="E:/Documents/LongTermeForet/Orthophotos/diffMNSMNT_1m.tif", overwrite=TRUE)  # ok superpose !
rm(mns25)

mns1 <- raster("E:/Documents/LongTermeForet/Orthophotos/diffMNSMNT_1m.tif")

	
	#### mask 1m
	# d'après BDForet simplifiée (regroupement sur QGIS)
fr <- readOGR("E:/Documents/LongTermeForet/Orthophotos", layer="BDFORETv2_Chizé_2017_simp")	
rfr <- rasterize(fr, y=mns1, field=1)
dataType(rfr) <- "INT2U"
writeRaster(rfr, filename="E:/Documents/LongTermeForet/Orthophotos/maskBDF_1m.tif", overwrite=TRUE)  
m1 <- rfr
m1[] <- NA
	# puis d'après MNS > 12m
m1[!is.na(rfr) & rfr==1 & mns1 > 12 & !is.na(mns1)] <- 1	
plot(m1)
writeRaster(m1, filename="E:/Documents/LongTermeForet/Orthophotos/mask_BDF_MNSsup12_1m.tif", overwrite=TRUE)  


	#### mask 20m
			#	le masque de la foret BDF :
rfr <- raster("E:/Documents/LongTermeForet/Orthophotos/maskBDF_1m.tif")  
agrfr <- aggregate(rfr, fact=20, fun=sum, na.rm=TRUE)
writeRaster(agrfr, filename="E:/Documents/LongTermeForet/Orthophotos/maskBDF_sum1m_20m.tif", overwrite=TRUE)  
	# mask 1m > 12m, sum sur 20m cell
			# mask 1m > 12m
m1 <- raster("E:/Documents/LongTermeForet/Orthophotos/mask_BDF_MNSsup12_1m.tif")
mask20m.sum <- aggregate(m1, fact=20, fun=sum, na.rm=TRUE)
writeRaster(mask20m.sum, filename="E:/Documents/LongTermeForet/Orthophotos/maskSum_20m.tif", overwrite=TRUE)  

mask20m.sum <- raster("E:/Documents/LongTermeForet/Orthophotos/maskSum_20m.tif")
dataType(mask20m.sum) <- "INT2U" # https://www.rdocumentation.org/packages/raster/versions/3.4-5/topics/dataType

	
	#### mask 20m
	# passe le mask 20m à 0.25 m, transfert simple de valeur
mask20m025.sum <- disaggregate(mask20m.sum, fact=4*20)  
res(mask20m025.sum) # 0.25
writeRaster(mask20m025.sum, filename="E:/Documents/LongTermeForet/Orthophotos/maskSum_20m025u.tif", overwrite=TRUE)  
mask20m025.sum <- raster("E:/Documents/LongTermeForet/Orthophotos/maskSum_20m025u.tif")
rm(mask20m.sum) ;gc();gc()
gc();gc()
dataType(mask20m025.sum) <- "INT2U"

	# on veut garder les valeurs > 200 (50% de couverture h>12m, en res 1m))
# mask20m025.sum[mask20m025.sum < 200] <- NA  # error memory
canProcessInMemory(mask20m025.sum, verbose = TRUE) # FALSE...
				# beginCluster(n = 4)
				# t_parallel <- system.time({
				  # pm <- clusterR(mask20m025.sum, fun = function(x) { x[x < 300] <- NA })
				# })
				# endCluster()

		# par blocks https://strimas.com/post/processing-large-rasters-in-r/
			# file paths
f_in <- "E:/Documents/LongTermeForet/Orthophotos/maskSum_20m025u.tif"
f_out <- "E:/Documents/LongTermeForet/Orthophotos/maskSum_20m025sup200.tif"
			# input and output rasters
r_in <- raster(f_in)
r_out <- raster(r_in)
b <- blockSize(r_in)
print(b) # 203 blocks

r_in <- readStart(r_in)
r_out <- writeStart(r_out, filename = f_out)
			# loop over blocks
for (i in seq_along(b$row)) {
			  # read values for block
			  # format is a matrix with rows the cells values and columns the layers
  v <- getValues(r_in, row = b$row[i], nrows = b$nrows[i])
  v <- ifelse(v < 200, NA, 1) 
				# write to output file
  r_out <- writeValues(r_out, v, b$row[i])
}
				# close files
r_out <- writeStop(r_out)
r_in <- readStop(r_in)

ss <- raster("E:/Documents/LongTermeForet/Orthophotos/maskSum_20m025sup200.tif")
plot(ss)
dataType(ss) <- "INT1U"
writeRaster(ss, filename="E:/Documents/LongTermeForet/Orthophotos/maskSum_20m025sup200.tif", overwrite=TRUE, datatype="INT1U")  




###########################
# Préparation données pour classif

# Dispo :
# Data (res 0.25 m) :
	# MNS
	# visible R
	# visible G
	# visible B
	# NIR
# texture (GRASS r.texture, sur luminace 1m grayscale, size=7)
	# Var : variance
	# IDM : Inverse Difference Moment
	# Entr : Entropy 
	# Corr : Correlation 
	# ASM : Angular Second Moment (= uniformity)
# 20m :
	# densité TreeTop > 12m (sur MNS 1m, QGIS)


# https://inventaire-forestier.ign.fr/IMG/pdf/IF_25_proche_infrarouge.pdf
	# canal vert -> couleur bleu 
	# canal rouge -> couleur vert 
	# canal NIR -> couleur rouge 


rgb <- stack("E:/Documents/LongTermeForet/Orthophotos/ORTHOMOSAIQUE VISIBLE/ORTHOMOSAIQUE_CEBC-CNRS_CHIZE_RGB_1M.tif")
nlayers(rgb) # 4
rgb <- dropLayer(rgb, 4)  # on enlève la 4eme bande qui sert à rien
plotRGB(rgb)

rv <- subset(rgb, 2)  # green
rr <- subset(rgb, 1)  # red

nir <- stack("E:/Documents/LongTermeForet/Orthophotos/ORTHOMOSAIQUE NIR/ORTHOMOSAIQUE_CEBC-CNRS_CHIZE_NIR_1M.tif")
nlayers(nir) # 2
nir <- dropLayer(nir, 2) # on enlève la 2eme bande qui sert à rien

ss <- stack(nir, rr, rv)

plotRGB(ss)
writeRaster(ss, filename="E:/Documents/LongTermeForet/Orthophotos/compoFC_1M.tif", overwrite=TRUE)


# grey scale / luminance: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3254613/ Luminance is calculated as 0.3R + 0.59G + 0.11B

library(raster)
library(rgdal)

rgb <- stack("E:/Documents/LongTermeForet/Orthophotos/ORTHOMOSAIQUE VISIBLE/ORTHOMOSAIQUE_CEBC-CNRS_CHIZE_RGB_1M.tif")
nlayers(rgb) # 4
rgb <- dropLayer(rgb, 4)  # on enlève la 4eme bande qui sert à rien
lum.1m <- subset(rgb, 1)*0.3 + subset(rgb, 2)*0.59 + subset(rgb, 3)*0.11
str(lum.1m)
plot(lum.1m, col=grey.colors(256))
lum.1m[] <- ceiling(lum.1m[])
dataType(lum.1m) <- "INT1U"
writeRaster(lum.1m, filename="E:/Documents/LongTermeForet/Orthophotos/Luminance_1M.tif", datatype="INT1U", overwrite=TRUE)

# grey scale / Value: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3254613/ Value is calculated as max(R, G, B)
val.1m <- stackApply(rgb, indices=c(1,1,1), fun=max)
str(val.1m)
plot(val.1m, col=grey.colors(256))
val.1m[] <- ceiling(val.1m[])
dataType(val.1m) <- "INT1U"
writeRaster(val.1m, filename="E:/Documents/LongTermeForet/Orthophotos/Value_1M.tif", datatype="INT1U", overwrite=TRUE)


fc <- stack("E:/Documents/LongTermeForet/Orthophotos/compoFC_25CM.tif")
valfc.25 <- stackApply(fc, indices=c(1,1,1), fun=max)
valfc.25 <- ceiling(valfc.25)
# tt <- valfc.25
# tt[valfc.25==255] <- NA
# dataType(valfc.25) <- "INT1U"
writeRaster(valfc.25, filename="E:/Documents/LongTermeForet/Orthophotos/ValueFC_25CM.tif", datatype="INT1U", overwrite=TRUE)



# rgb <- stack("E:/Documents/LongTermeForet/Orthophotos/ORTHOMOSAIQUE VISIBLE/ORTHOMOSAIQUE_CEBC-CNRS_CHIZE_RGB_25CM.tif")
# nlayers(rgb) # 4
# rgb <- dropLayer(rgb, 4)  # on enlève la 4eme bande qui sert à rien
# lum.1m <- subset(rgb, 1)*0.3 + subset(rgb, 2)*0.59 + subset(rgb, 3)*0.11
# str(lum.1m)
# plot(lum.1m)
# writeRaster(lum.1m, filename="E:/Documents/LongTermeForet/Orthophotos/Luminance_25CM.tif", overwrite=TRUE)

# lum.1m[] <- ceiling(lum.1m[])  # error memory
# f_in <- "E:/Documents/LongTermeForet/Orthophotos/Luminance_25CM.tif"
# f_out <- "E:/Documents/LongTermeForet/Orthophotos/Luminancei_25CM.tif.tif"
			# input and output rasters
# r_in <- raster(f_in)
# r_out <- raster(r_in)
# b <- blockSize(r_in)
# print(b) # 203 blocks

# r_in <- readStart(r_in)
# r_out <- writeStart(r_out, filename = f_out)
			# loop over blocks
# for (i in seq_along(b$row)) {
			  # read values for block
			  # format is a matrix with rows the cells values and columns the layers
  # v <- getValues(r_in, row = b$row[i], nrows = b$nrows[i])
  # v <- ceiling(v)) 
				# write to output file
  # r_out <- writeValues(r_out, v, b$row[i])
# }
				# close files
# r_out <- writeStop(r_out)
# r_in <- readStop(r_in)

# ss <- raster("E:/Documents/LongTermeForet/Orthophotos/maskSum_20m025sup200.tif")
# plot(ss)
# dataType(ss) <- "INT1U"
# writeRaster(ss, filename="E:/Documents/LongTermeForet/Orthophotos/maskSum_20m025sup200.tif", overwrite=TRUE, datatype="INT1U")  
# dataType(lum.1m) <- "INT1U"
# writeRaster(lum.1m, filename="E:/Documents/LongTermeForet/Orthophotos/Luminance_25CM.tif", datatype="INT1U", overwrite=TRUE)

# Vegetation indice NDVI (https://rspatial.org/rs/rs.pdf):
vi <-function(img, k, i) 
	{
	bk <- img[[k]]
	bi <- img[[i]]
	vi <- (bk - bi) / (bk + bi)
	return(vi)
	}

# k = NIR, i = red.
ndvi <- vi(ss, 1, 2)
plot(ndvi, col =rev(terrain.colors(10)), main = "NDVI")
writeRaster(ndvi, filename="E:/Documents/LongTermeForet/Orthophotos/ndvi_1M.tif", overwrite=TRUE)
rm(rgb, rv, rr, nir, ss, ndvi)
gc();gc()



# library(ForestTools)
# library(raster)

# lin <- function(x){x * 0.05 + 0.6}
# ttops <- vwf(CHM = kootenayCHM, winFun = lin, minHeight = 2)

# variables à calculer :
# - tree top position -> tree density over a grid (cell size ?)
# - texture

# https://cran.r-project.org/web/packages/foto/vignettes/foto-vignette.html
# http://azvoleff.com/articles/calculating-image-textures-with-glcm/


library(raster)
library(rgdal)

		# MNS 1m
mns1 <- raster("E:/Documents/LongTermeForet/Orthophotos/diffMNSMNT_1m.tif")

		# MNS .25m
mns25 <- raster("E:/Documents/LongTermeForet/Orthophotos/diffMNSMNT_025.tif")

		# mask 1m > 12m
m1 <- raster("E:/Documents/LongTermeForet/Orthophotos/mask_BDF_MNSsup12_1m.tif")

		# tree top position MNS 1m / mask 1m > 12m, window 5p (QGIS Extension Tree Density)
ttp <- readOGR("E:/Documents/LongTermeForet/Orthophotos", layer="treetop_masksup12m_1m_window_5_point")	


		# mns1m, mean sur 20m cell
mns20m.mean <- aggregate(mns1, fact=20, fun=mean)
writeRaster(mns20m.mean, filename="E:/Documents/LongTermeForet/Orthophotos/MNSmean_20m.tif", overwrite=TRUE)  
		# mns.25m, sd sur 20m cell
mns20m.sd <- aggregate(mns25, fact=4*20, fun=sd, na.rm=TRUE)
writeRaster(mns20m.sd, filename="E:/Documents/LongTermeForet/Orthophotos/MNSsd_20m.tif", overwrite=TRUE) 
rm(mns25);gc()
gc()
		

		# number of treetop on 20m cell
nttp20m <- rasterize(ttp, y=mns20m.mean, fun=function(x,...)length(x))
nttp20m <- nttp20m[[2]]
writeRaster(nttp20m, filename="E:/Documents/LongTermeForet/Orthophotos/NbTT_20m.tif", overwrite=TRUE)  


### - texture

# On travaille en entrée sur les pixels 0.25m (mask 75%? )et en sortie sur des pixels de 20m

library(raster)
library(rgdal)





# rgb <- stack("E:/Documents/LongTermeForet/Orthophotos/ORTHOMOSAIQUE VISIBLE/ORTHOMOSAIQUE_CEBC-CNRS_CHIZE_RGB_25CM.tif")
# nlayers(rgb) # 4
# rgb <- dropLayer(rgb, 4)  # on enlève la 4eme bande qui sert à rien
# writeRaster(rgb, filename="E:/Documents/LongTermeForet/Orthophotos/ORTHOMOSAIQUE VISIBLE/ORTHOMOSAIQUE_CEBC-CNRS_CHIZE_RGB_25CM3b.tif", overwrite=TRUE)  
rgb <- stack("E:/Documents/LongTermeForet/Orthophotos/ORTHOMOSAIQUE VISIBLE/ORTHOMOSAIQUE_CEBC-CNRS_CHIZE_RGB_25CM3b.tif")

f_in <- "E:/Documents/LongTermeForet/Orthophotos/maskSum_20m025u.tif"
f_out <- "E:/Documents/LongTermeForet/Orthophotos/maskSum_20m025sup300.tif"
# input and output rasters
r_in <- raster(f_in)
r_out <- raster(r_in)
b <- blockSize(r_in)
print(b) # 203 blocks

r_in <- readStart(r_in)
r_out <- writeStart(r_out, filename = f_out)
# loop over blocks
for (i in seq_along(b$row)) {
  # read values for block
  # format is a matrix with rows the cells values and columns the layers
  v <- getValues(r_in, row = b$row[i], nrows = b$nrows[i])
    # mean cell value across layers
  v <- ifelse(v < 300, NA, 1) 
    # write to output file
  r_out <- writeValues(r_out, v, b$row[i])
}
# close files
r_out <- writeStop(r_out)
r_in <- readStop(r_in)


# res(rgb)  # 0.25 0.25
rgb[mask20m025.sum < 300] <- NA
mr <- mask(r, m)
rm(rgb) ;gc();gc()
gc();gc()



# FOTO (Fourier Transform Textural Ordination) 
# https://cran.r-project.org/web/packages/foto/vignettes/foto-vignette.html


#####################################
#####################################
# calcul des rasters stats (mean, min, max, sd) sur le mask 20m


library(raster)
library(rgdal)

	# sum des surfaces MNS>12m sur res 20m, pour mask
mask20m.sum <- raster("E:/Documents/LongTermeForet/Orthophotos/maskSum_20m.tif")


# st <- c("E:/Documents/LongTermeForet/Orthophotos/ORTHOMOSAIQUE VISIBLE/ORTHOMOSAIQUE_CEBC-CNRS_CHIZE_RGB_1M.tif",
		# "E:/Documents/LongTermeForet/Orthophotos/ORTHOMOSAIQUE NIR/ORTHOMOSAIQUE_CEBC-CNRS_CHIZE_NIR_1M.tif")

st <- c("E:/Documents/LongTermeForet/Orthophotos/ORTHOMOSAIQUE VISIBLE/ORTHOMOSAIQUE_CEBC-CNRS_CHIZE_RGB_25CM.tif",
		"E:/Documents/LongTermeForet/Orthophotos/ORTHOMOSAIQUE NIR/ORTHOMOSAIQUE_CEBC-CNRS_CHIZE_NIR_25CM.tif")

# rgb
lay <- c("vr", "vg", "vb")

# for(i in 1:length(st))
	# {
	i=1
	a <- stack(st[i])
	a <- dropLayer(a, nlayers(a))  # on enlève la dernière bande qui sert à rien
	
	ag <- aggregate(a, fact=20*4, fun=mean, na.rm=TRUE)
	for(j in 1:3)
		{
		writeRaster(subset(ag, j), filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_mean_20m.tif", sep=""), overwrite=TRUE)  
		}
	ag[mask20m.sum < 200] <- NA
	for(j in 1:3)
		{
		writeRaster(subset(ag, j), filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_mean_20mM.tif", sep=""), overwrite=TRUE)  
		}

	ag <- aggregate(a, fact=20*4, fun=sd, na.rm=TRUE)
	for(j in 1:3)
		{
		writeRaster(subset(ag, j), filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_sd_20m.tif", sep=""), overwrite=TRUE)  
		}
	ag[mask20m.sum < 200] <- NA
	for(j in 1:3)
		{
		writeRaster(subset(ag, j), filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_sd_20mM.tif", sep=""), overwrite=TRUE)  
		}
	
	ag <- aggregate(a, fact=20*4, fun=min, na.rm=TRUE)
	for(j in 1:3)
		{
		writeRaster(subset(ag, j), filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_min_20m.tif", sep=""), overwrite=TRUE)  
		}
	ag[mask20m.sum < 200] <- NA
	for(j in 1:3)
		{
		writeRaster(subset(ag, j), filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_min_20mM.tif", sep=""), overwrite=TRUE)  
		}
		
	ag <- aggregate(a, fact=20*4, fun=max, na.rm=TRUE)
	for(j in 1:3)
		{
		writeRaster(subset(ag, j), filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_max_20m.tif", sep=""), overwrite=TRUE)  
		}
	ag[mask20m.sum < 200] <- NA
	for(j in 1:3)
		{
		writeRaster(subset(ag, j), filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_max_20mM.tif", sep=""), overwrite=TRUE)  
		}

rm(a, ag);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE)
	
	# nir
	i=2
	j=1
	lay <- c("nir")

	a <- stack(st[i])
	a <- dropLayer(a, nlayers(a))  # on enlève la dernière bande qui sert à rien
	
	ag <- aggregate(a, fact=20*4, fun=mean, na.rm=TRUE)
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_mean_20m.tif", sep=""), overwrite=TRUE)  
	ag[mask20m.sum < 200] <- NA
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_mean_20mM.tif", sep=""), overwrite=TRUE)  
	
	ag <- aggregate(a, fact=20*4, fun=sd, na.rm=TRUE)
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_sd_20m.tif", sep=""), overwrite=TRUE)  
	ag[mask20m.sum < 200] <- NA
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_sd_20mM.tif", sep=""), overwrite=TRUE)  
		
	ag <- aggregate(a, fact=20*4, fun=min, na.rm=TRUE)
	writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_min_20m.tif", sep=""), overwrite=TRUE)  
	ag[mask20m.sum < 200] <- NA
	writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_min_20mM.tif", sep=""), overwrite=TRUE)  
		
	ag <- aggregate(a, fact=20*4, fun=max, na.rm=TRUE)
	writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_max_20m.tif", sep=""), overwrite=TRUE)  
	ag[mask20m.sum < 200] <- NA
	writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_max_20mM.tif", sep=""), overwrite=TRUE)  

rm(a, ag);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE)
	

# texture
st <- c("output22cc6fb94956468d851d1d4dee7827cb_Var.tif",
"output22cc6fb94956468d851d1d4dee7827cb_IDM.tif",
"output22cc6fb94956468d851d1d4dee7827cb_Entr.tif",
"output22cc6fb94956468d851d1d4dee7827cb_Corr.tif",
"output22cc6fb94956468d851d1d4dee7827cb_ASM.tif")

lay <- c("Var", "IDM", "Entr", "Corr", "ASM")


for(i in 1:length(st))
	{
	j <- i
	a <- raster(paste("E:/Documents/LongTermeForet/Orthophotos/", st[i], sep=""))
	
	ag <- aggregate(a, fact=20, fun=mean, na.rm=TRUE)
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_mean_20m.tif", sep=""), overwrite=TRUE)  
	ag[mask20m.sum < 200] <- NA
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_mean_20mM.tif", sep=""), overwrite=TRUE)  
	
	ag <- aggregate(a, fact=20, fun=sd, na.rm=TRUE)
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_sd_20m.tif", sep=""), overwrite=TRUE)  
	ag[mask20m.sum < 200] <- NA
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_sd_20mM.tif", sep=""), overwrite=TRUE)  
		
	ag <- aggregate(a, fact=20, fun=min, na.rm=TRUE)
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_min_20m.tif", sep=""), overwrite=TRUE)  
	ag[mask20m.sum < 200] <- NA
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_min_20mM.tif", sep=""), overwrite=TRUE)  
		
	ag <- aggregate(a, fact=20, fun=max, na.rm=TRUE)
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_max_20m.tif", sep=""), overwrite=TRUE)  
	ag[mask20m.sum < 200] <- NA
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_max_20mM.tif", sep=""), overwrite=TRUE)  
rm(a, ag);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE)
	
	}
	
	
# MNS à 25 cm
	a <- raster("E:/Documents/LongTermeForet/Orthophotos/diffMNSMNT_025.tif")
	i=1
	j=1
	lay <- c("mns")

	ag <- aggregate(a, fact=20*4, fun=mean, na.rm=TRUE)
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_mean_20m.tif", sep=""), overwrite=TRUE)  
		ag[mask20m.sum < 200] <- NA
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_mean_20mM.tif", sep=""), overwrite=TRUE)  
	
	ag <- aggregate(a, fact=20*4, fun=sd, na.rm=TRUE)
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_sd_20m.tif", sep=""), overwrite=TRUE)  
	ag[mask20m.sum < 200] <- NA
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_sd_20mM.tif", sep=""), overwrite=TRUE)  
		
	ag <- aggregate(a, fact=20*4, fun=min, na.rm=TRUE)
	writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_min_20m.tif", sep=""), overwrite=TRUE)  
	ag[mask20m.sum < 200] <- NA
	writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_min_20mM.tif", sep=""), overwrite=TRUE)  
		
	ag <- aggregate(a, fact=20*4, fun=max, na.rm=TRUE)
	writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_max_20m.tif", sep=""), overwrite=TRUE)  
	ag[mask20m.sum < 200] <- NA
	writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_max_20mM.tif", sep=""), overwrite=TRUE)  

rm(a, ag);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE)



# Vegetation indice NDVI (https://rspatial.org/rs/rs.pdf):

# rgb <- stack("E:/Documents/LongTermeForet/Orthophotos/ORTHOMOSAIQUE VISIBLE/ORTHOMOSAIQUE_CEBC-CNRS_CHIZE_RGB_25CM.tif")
# rgb <- dropLayer(rgb, 4)  # on enlève la 4eme bande qui sert à rien

# rv <- subset(rgb, 2)  # green
# rr <- subset(rgb, 1)  # red

# nir <- stack("E:/Documents/LongTermeForet/Orthophotos/ORTHOMOSAIQUE NIR/ORTHOMOSAIQUE_CEBC-CNRS_CHIZE_NIR_25CM.tif")
# nir <- dropLayer(nir, 2) # on enlève la 2eme bande qui sert à rien

# ss <- stack(nir, rr, rv)
# rm(rgb, rv, rr, nir);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE)

# plotRGB(ss)
# writeRaster(ss, filename="E:/Documents/LongTermeForet/Orthophotos/compoFC_25CM.tif", overwrite=TRUE)

# vi <-function(img, k, i) 
	# {
	# bk <- img[[k]]
	# bi <- img[[i]]
	# vi <- (bk - bi) / (bk + bi)
	# return(vi)
	# }
# k = NIR, i = red.
# ndvi <- vi(ss, 1, 2)
# writeRaster(ndvi, filename="E:/Documents/LongTermeForet/Orthophotos/ndvi_25CM.tif", overwrite=TRUE)
# rm(ss);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE)

	a <- raster("E:/Documents/LongTermeForet/Orthophotos/ndvi_25CM.tif")
	# rm(ndvi);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE)

	i=1
	j=1
	lay <- c("ndvi")

	ag <- aggregate(a, fact=20*4, fun=mean, na.rm=TRUE)
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_mean_20m.tif", sep=""), overwrite=TRUE)  
		ag[mask20m.sum < 200] <- NA
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_mean_20mM.tif", sep=""), overwrite=TRUE)  
	
	ag <- aggregate(a, fact=20*4, fun=sd, na.rm=TRUE)
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_sd_20m.tif", sep=""), overwrite=TRUE)  
	ag[mask20m.sum < 200] <- NA
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_sd_20mM.tif", sep=""), overwrite=TRUE)  
		
	ag <- aggregate(a, fact=20*4, fun=min, na.rm=TRUE)
	writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_min_20m.tif", sep=""), overwrite=TRUE)  
	ag[mask20m.sum < 200] <- NA
	writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_min_20mM.tif", sep=""), overwrite=TRUE)  
		
	ag <- aggregate(a, fact=20*4, fun=max, na.rm=TRUE)
	writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_max_20m.tif", sep=""), overwrite=TRUE)  
	ag[mask20m.sum < 200] <- NA
	writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_max_20mM.tif", sep=""), overwrite=TRUE)  

rm(a, ag);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE)




	a <- raster("E:/Documents/LongTermeForet/Orthophotos/ValueFC_25CM.tif")
	i=1
	j=1
	lay <- c("ValueFC")

	ag <- aggregate(a, fact=20*4, fun=mean, na.rm=TRUE)
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_mean_20m.tif", sep=""), overwrite=TRUE)  
		ag[mask20m.sum < 200] <- NA
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_mean_20mM.tif", sep=""), overwrite=TRUE)  
	
	ag <- aggregate(a, fact=20*4, fun=sd, na.rm=TRUE)
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_sd_20m.tif", sep=""), overwrite=TRUE)  
	ag[mask20m.sum < 200] <- NA
		writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_sd_20mM.tif", sep=""), overwrite=TRUE)  
		
	ag <- aggregate(a, fact=20*4, fun=min, na.rm=TRUE)
	writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_min_20m.tif", sep=""), overwrite=TRUE)  
	ag[mask20m.sum < 200] <- NA
	writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_min_20mM.tif", sep=""), overwrite=TRUE)  
		
	ag <- aggregate(a, fact=20*4, fun=max, na.rm=TRUE)
	writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_max_20m.tif", sep=""), overwrite=TRUE)  
	ag[mask20m.sum < 200] <- NA
	writeRaster(ag, filename=paste("E:/Documents/LongTermeForet/Orthophotos/", lay[j], "_max_20mM.tif", sep=""), overwrite=TRUE)  

rm(a, ag);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE);gc(verbose=FALSE)



# densité TreeTop > 12m
a <- raster("E:/Documents/LongTermeForet/Orthophotos/NbTT_20m.tif")
		# traitement des zeros
			#	le masque de la foret BDF :
rfr <- raster("E:/Documents/LongTermeForet/Orthophotos/maskBDF_sum1m_20m.tif", overwrite=TRUE)  
a[rfr > 1 & is.na(a)] <- 0
writeRaster(a, filename="E:/Documents/LongTermeForet/Orthophotos/NbTT0_20m.tif", overwrite=TRUE)  





