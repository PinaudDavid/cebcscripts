#### abalyses raster veget Chizé 2021

# Classif non supervisée
# Classif supervisée



###########################
# données pour classif non supervisée

# Data (base res 0.25 m) :
	# visible R
	# visible G
	# visible B
	# NIR
# texture (GRASS r.texture, sur luminace base 1 m grayscale, size=7)
	# Var : variance
	# IDM : Inverse Difference Moment
	# Entr : Entropy 
	# Corr : Correlation 
	# ASM : Angular Second Moment (= uniformity)
# MNS base 0.25 m
# ndvi base 0.25 m
# valueFC : composite NIR-RED-GREEN fausse couleur passée en value (max(NIR,RED,GREEN))
# densité TreeTop > 12m (sur MNS 1m, QGIS)


library(raster)
library(rgdal)


# liste des rasters :
lay <- c("vr", "vg", "vb", "nir", "Var", "IDM", "Entr", "Corr", "ASM", "mns", "ndvi", "ValueFC")
stats <- c("mean", "sd", "min", "max")

ls <- expand.grid(lay, stats)
lr <- paste("E:/Documents/LongTermeForet/Orthophotos/", ls[,1], "_", ls[,2], "_20m.tif", sep="")
lr <- c(lr, "E:/Documents/LongTermeForet/Orthophotos/NbTT0_20m.tif")
table(file.exists(lr))  # 49 TRUE

i <- 1
a <- raster(lr[i])

for(i in 2:length(lr))
	{
	a <- stack(a, raster(lr[i]))
		}

names(a) <- c(paste(ls[, 1], "_", ls[, 2], sep=""), "NbTT0")
writeRaster(a, "E:/Documents/LongTermeForet/Orthophotos/dataClassif49_20m.tif", overwrite=TRUE)


###########################
# MASKs :

a <- stack("E:/Documents/LongTermeForet/Orthophotos/dataClassif49_20m.tif")
lay <- c("vr", "vg", "vb", "nir", "Var", "IDM", "Entr", "Corr", "ASM", "mns", "ndvi", "ValueFC")
stats <- c("mean", "sd", "min", "max")
ls <- expand.grid(lay, stats)
names(a) <- c(paste(ls[, 1], "_", ls[, 2], sep=""), "NbTT0")

mask20m.sum <- raster("E:/Documents/LongTermeForet/Orthophotos/maskSum_20m.tif")
a[mask20m.sum < 200 | is.na(mask20m.sum)] <- NA
rm(mask20m.sum); gc();gc()
gc();gc()

	# test raster
# r <- raster(ncol=10, nrow=10)
# r2 <- r
# values(r) <- 1:ncell(r)
# values(r2) <- runif(ncell(r))
# r[r %in% c(2,45,23,48,77,89)] <- NA
# r2[r2 %in% c(21,45,23,28,78,82)] <- NA
# rr <- stack(r, r2)
# rr[c(1:15, 92:100)] <- NA
# nr <- as.data.frame(getValues(rr))
# nro <- na.omit(nr)


# ACP

library(ade4)

nr <- as.data.frame(getValues(a))
ii <- sample(1:nrow(nr), 100000)
nri <- nr[ii,]

acp <- dudi.pca(na.omit(nri)) # 8
plot(acp$li[,1:2], asp=1, pch=".")
s.arrow(acp$c1*100, lab = colnames(nr), add.plot=T)
rm(a, mask20m.sum);gc();gc()
dista <- dist.dudi(acp)

myclust <- hclust(dista, method = "ward.D2")
plot(myclust)

inertie <- sort(myclust$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie",lwd=2);grid()  # 5


# kmeans clustering (https://rspatial.org/raster/rs/4-unsupclassification.html)
nr <- getValues(a)
rm(a); gc();gc()
gc();gc()

# It is important to set the seed generator because `kmeans` initiates the centers in random locations
set.seed(99)
# We want to create 10 clusters, allow 500 iterations, start with 5 random sets using "Lloyd" method
kmncluster <- kmeans(na.omit(nr), centers = 10, iter.max = 500, nstart = 5, algorithm="Lloyd")
knr <- raster("E:/Documents/LongTermeForet/Orthophotos/maskSum_20m.tif")
values(knr) <- kmncluster$cluster # pb des NA


# https://gis.stackexchange.com/questions/123639/unsupervised-classification-with-kmeans-in-r
library(cluster)
library(raster)

a <- stack("E:/Documents/LongTermeForet/Orthophotos/dataClassif49_20m.tif")
lay <- c("vr", "vg", "vb", "nir", "Var", "IDM", "Entr", "Corr", "ASM", "mns", "ndvi", "ValueFC")
stats <- c("mean", "sd", "min", "max")
ls <- expand.grid(lay, stats)
names(a) <- c(paste(ls[, 1], "_", ls[, 2], sep=""), "NbTT0")

mask20m.sum <- raster("E:/Documents/LongTermeForet/Orthophotos/maskSum_20m.tif")
a[mask20m.sum < 200 | is.na(mask20m.sum)] <- NA
rm(mask20m.sum); gc();gc()
gc();gc()
plot(a, maxnl=49)


as <- scale(a)
rv <- getValues(a)
idx <- 1:ncell(a)
idx <- idx[-unique(which(is.na(rv), arr.ind=TRUE)[,1])]  

clus <- cluster::clara(na.omit(rv), k=5)
r.clust <- a[[1]]
r.clust[] <- NA
r.clust[idx] <- clus$clustering
plot(r.clust) 
writeRaster(r.clust, filename="E:/Documents/LongTermeForet/Orthophotos/Clust5c_20m.tif", overwrite=TRUE)


## Synthèse / visu des classes etc

library(rgdal)
library(raster)

mask20m.sum <- raster("E:/Documents/LongTermeForet/Orthophotos/maskSum_20m.tif")


bdf <- readOGR("E:/Documents/LongTermeForet", layer="BDFORETv2_Chizé_2017")
bdf@data$ESSENCE <- as.factor(bdf@data$ESSENCE)
levels(bdf@data$ESSENCE)
 # [1] "Chênes décidus"       
 # [2] "Conifères"            
 # [3] "Feuillus"             
 # [4] "Hêtre"                
 # [5] "Mixte"                
 # [6] "NC"                   
 # [7] "NR"                   
 # [8] "Peuplier"             
 # [9] "Pin laricio, pin noir"
# [10] "Pin sylvestre"        
# [11] "Pins mélangés"  

rfr <- rasterize(bdf, y=mask20m.sum, field="ESSENCE")
names(rfr) <- "Ess"
table(rfr[])
   # 1     2     3     4     5     6     7     8     9    10    11 
# 42459   370 50760 14910 11331   124 22157  2753  1097   190   202 
writeRaster(rfr, filename="E:/Documents/LongTermeForet/Orthophotos/rBDforet_20m.tif", overwrite=TRUE)

rfr[mask20m.sum < 200 | is.na(mask20m.sum)] <- NA

kc <- raster("E:/Documents/LongTermeForet/Orthophotos/Clust5c_20m.tif")

a <- stack("E:/Documents/LongTermeForet/Orthophotos/dataClassif49_20m.tif")
lay <- c("vr", "vg", "vb", "nir", "Var", "IDM", "Entr", "Corr", "ASM", "mns", "ndvi", "ValueFC")
stats <- c("mean", "sd", "min", "max")
ls <- expand.grid(lay, stats)
names(a) <- c(paste(ls[, 1], "_", ls[, 2], sep=""), "NbTT0")

# [1] "vr_mean"      "vg_mean"      "vb_mean"      "nir_mean"     "Var_mean"     "IDM_mean"     "Entr_mean"    "Corr_mean"   
 # [9] "ASM_mean"     "mns_mean"     "ndvi_mean"    "ValueFC_mean" "vr_sd"        "vg_sd"        "vb_sd"        "nir_sd"      
# [17] "Var_sd"       "IDM_sd"       "Entr_sd"      "Corr_sd"      "ASM_sd"       "mns_sd"       "ndvi_sd"      "ValueFC_sd"  
# [25] "vr_min"       "vg_min"       "vb_min"       "nir_min"      "Var_min"      "IDM_min"      "Entr_min"     "Corr_min"    
# [33] "ASM_min"      "mns_min"      "ndvi_min"     "ValueFC_min"  "vr_max"       "vg_max"       "vb_max"       "nir_max"     
# [41] "Var_max"      "IDM_max"      "Entr_max"     "Corr_max"     "ASM_max"      "mns_max"      "ndvi_max"     "ValueFC_max" 
# [49] "NbTT0"       

# on prend 
v <- c(10, 22, 34, 46) 

ens <- stack(subset(a, v), rfr, kc)
ens[mask20m.sum < 200 | is.na(mask20m.sum)] <- NA


nr <- as.data.frame(getValues(ens))
nr <- nr[!is.na(nr$mns_mean),]

nr$Clust5c_20m <- as.character(nr$Clust5c_20m)

nr$Ess <- as.character(factor(nr$Ess, levels=as.character(1:11), labels=levels(bdf@data$ESSENCE)))
nr$Ess.kc <- paste(nr$Ess, nr$Clust5c_20m, sep="_")
nr$Ess.kc <- as.factor(nr$Ess.kc)
levels(nr$Ess.kc)


library(ggplot2)

par(mfrow=c(2,2))
ggplot(nr) + geom_violin(aes(y = Clust5c_20m, x = mns_mean))
ggplot(nr) + geom_violin(aes(y = Clust5c_20m, x = mns_sd))
ggplot(nr) + geom_violin(aes(y = Clust5c_20m, x = mns_min))
ggplot(nr) + geom_violin(aes(y = Clust5c_20m, x = mns_max))

ggplot(nr) + geom_violin(aes(y = Ess.kc, x = mns_mean))




###########################
# données pour classif supervisée

# Data (base res 0.25 m) :
	# visible R
	# visible G
	# visible B
	# NIR
# texture (GRASS r.texture, sur luminace base 1 m grayscale, size=7)
	# Var : variance
	# IDM : Inverse Difference Moment
	# Entr : Entropy 
	# Corr : Correlation 
	# ASM : Angular Second Moment (= uniformity)
# MNS base 0.25 m
# ndvi base 0.25 m
# valueFC : composite NIR-RED-GREEN fausse couleur passée en value (max(NIR,RED,GREEN))
# densité TreeTop > 12m (sur MNS 1m, QGIS)

# position des aires connues (1) et points absence d'aire (0, selon les zones prospectées ou pas favorable sans aire)

library(raster)
library(rgdal)

# données raster :
a <- stack("E:/Documents/LongTermeForet/Orthophotos/dataClassif49_20m.tif")
lay <- c("vr", "vg", "vb", "nir", "Var", "IDM", "Entr", "Corr", "ASM", "mns", "ndvi", "ValueFC")
stats <- c("mean", "sd", "min", "max")
ls <- expand.grid(lay, stats)
names(a) <- c(paste(ls[, 1], "_", ls[, 2], sep=""), "NbTT0")

rfr <- raster("E:/Documents/LongTermeForet/Orthophotos/rBDforet_20m.tif")  # BDForet
names(rfr) <- "Ess"

a <- stack(a, rfr)


# MASKs :
mask20m.sum <- raster("E:/Documents/LongTermeForet/Orthophotos/maskSum_20m.tif")

# position des aires connues (1) et points absence d'aire (0, selon les zones prospectées ou pas favorable sans aire)
pa <- readOGR("E:/Documents/LongTermeForet", "PresAbs_aire")
# plot(rfr)
# plot(pa, add=T)
PA <- rasterize(pa, y=mask20m.sum, field="PresAbs")

a <- stack(a, PA)
# names(a)
# [1] "vr_mean"      "vg_mean"      "vb_mean"      "nir_mean"     "Var_mean"     "IDM_mean"     "Entr_mean"   
 # [8] "Corr_mean"    "ASM_mean"     "mns_mean"     "ndvi_mean"    "ValueFC_mean" "vr_sd"        "vg_sd"       
# [15] "vb_sd"        "nir_sd"       "Var_sd"       "IDM_sd"       "Entr_sd"      "Corr_sd"      "ASM_sd"      
# [22] "mns_sd"       "ndvi_sd"      "ValueFC_sd"   "vr_min"       "vg_min"       "vb_min"       "nir_min"     
# [29] "Var_min"      "IDM_min"      "Entr_min"     "Corr_min"     "ASM_min"      "mns_min"      "ndvi_min"    
# [36] "ValueFC_min"  "vr_max"       "vg_max"       "vb_max"       "nir_max"      "Var_max"      "IDM_max"     
# [43] "Entr_max"     "Corr_max"     "ASM_max"      "mns_max"      "ndvi_max"     "ValueFC_max"  "NbTT0"       
# [50] "Ess"          "layer"       
names(a)[51] <- "PA"
writeRaster(a, "E:/Documents/LongTermeForet/Orthophotos/dataClassifPA_20m.tif", overwrite=TRUE)


#   https://www.r-exercises.com/2018/03/07/advanced-techniques-with-raster-data-part-2-supervised-classification/
library(raster)
library(rgdal)
library(ade4)


a <- stack("E:/Documents/LongTermeForet/Orthophotos/dataClassifPA_20m.tif")
lay <- c("vr", "vg", "vb", "nir", "Var", "IDM", "Entr", "Corr", "ASM", "mns", "ndvi", "ValueFC")
stats <- c("mean", "sd", "min", "max")
ls <- expand.grid(lay, stats)
names(a) <- c(paste(ls[, 1], "_", ls[, 2], sep=""), "NbTT0", "Ess", "PA")


mask20m.sum <- raster("E:/Documents/LongTermeForet/Orthophotos/maskSum_20m.tif")
a[mask20m.sum < 200 | is.na(mask20m.sum)] <- NA


pa <- subset(a, "PA")
a <- dropLayer(a, "PA")
nr <- getValues(a)
idx <- 1:ncell(a)
idx <- idx[-unique(which(is.na(nr), arr.ind=TRUE)[,1])]  

acp <- dudi.pca(na.omit(nr), scannf = FALSE, nf = 10) # 10

racp <- subset(a, 1:10)
racp[] <- NA
racp[idx] <- as.matrix(acp$li)
# plot(racp) 
names(racp) <- paste("F", 1:10, sep="")
writeRaster(racp, filename="E:/Documents/LongTermeForet/Orthophotos/rPCA10mask_20m.tif", overwrite=TRUE)
racp <- stack("E:/Documents/LongTermeForet/Orthophotos/rPCA10mask_20m.tif")
names(racp) <- paste("F", 1:10, sep="")

pa <- as.factor(pa)
a <- stack(racp, pa)
rm(acp, idx, nr, racp)
gc();gc()
gc();gc()

# table(getValues(subset(a, "PA")))
 # 0   1 
# 424  33 

library(randomForest)
datr <- as.data.frame(getValues(a))
datr <- datr[!is.na(datr$PA),]
datr$PA <- as.factor(datr$PA)
modelRF <- randomForest(PA ~., data=datr, importance = TRUE)

## Inspect the confusion matrix of the OOB error assessment
modelRF$confusion
    # 0 1 class.error
# 0 418 6  0.01415094
# 1  27 6  0.81818182

varImpPlot(modelRF)


# MASKs :
mask20m.sum <- raster("E:/Documents/LongTermeForet/Orthophotos/maskSum_20m.tif")
a[mask20m.sum < 200 | is.na(mask20m.sum)] <- NA
ax <- dropLayer(a, "PA")


apr <- predict(ax, model=modelRF, na.rm=TRUE)

plot(apr)


datr <- as.data.frame(getValues(subset(a, c(1:6, 11))))
datr <- datr[!is.na(datr$PA),]
datr$PA <- as.factor(datr$PA)
modelRF <- randomForest(PA ~., data=datr, importance = TRUE)

## Inspect the confusion matrix of the OOB error assessment
modelRF$confusion
    # 0 1 class.error
# 0 418 6  0.01415094
# 1  27 6  0.81818182

varImpPlot(modelRF)


# MASKs :
mask20m.sum <- raster("E:/Documents/LongTermeForet/Orthophotos/maskSum_20m.tif")
a[mask20m.sum < 200 | is.na(mask20m.sum)] <- NA
a <- dropLayer(a, "PA")


apr <- predict(a, model=modelRF, na.rm=TRUE)

plot(apr)




###########################
# données pour classif supervisée
## 2eme tour avec données élargies

# Data (base res 0.25 m) :
	# visible R
	# visible G
	# visible B
	# NIR
# texture (GRASS r.texture, sur luminace base 1 m grayscale, size=7)
	# Var : variance
	# IDM : Inverse Difference Moment
	# Entr : Entropy 
	# Corr : Correlation 
	# ASM : Angular Second Moment (= uniformity)
# MNS base 0.25 m
# ndvi base 0.25 m
# valueFC : composite NIR-RED-GREEN fausse couleur passée en value (max(NIR,RED,GREEN))
# densité TreeTop > 12m (sur MNS 1m, QGIS)

# position des aires connues (1) et points absence d'aire (0, selon les zones prospectées ou pas favorable sans aire)

library(raster)
library(rgdal)

# données raster :
a <- stack("E:/Documents/LongTermeForet/Orthophotos/dataClassif49_20m.tif")
lay <- c("vr", "vg", "vb", "nir", "Var", "IDM", "Entr", "Corr", "ASM", "mns", "ndvi", "ValueFC")
stats <- c("mean", "sd", "min", "max")
ls <- expand.grid(lay, stats)
names(a) <- c(paste(ls[, 1], "_", ls[, 2], sep=""), "NbTT0")

rfr <- raster("E:/Documents/LongTermeForet/Orthophotos/rBDforet_20m.tif")  # BDForet
names(rfr) <- "Ess"

a <- stack(a, rfr)


# MASKs :
mask20m.sum <- raster("E:/Documents/LongTermeForet/Orthophotos/maskSum_20m.tif")

# position des aires connues ou secteurs favorables (1) et points absence d'aire (0, selon les zones prospectées pas favorable sans aire) données élargies
pa <- readOGR("E:/Documents/LongTermeForet", "PresAbs_aire_2")
# plot(rfr)
# plot(pa, add=T)
PA <- rasterize(pa, y=mask20m.sum, field="PresAbs")

a <- stack(a, PA)
# names(a)
# [1] "vr_mean"      "vg_mean"      "vb_mean"      "nir_mean"     "Var_mean"     "IDM_mean"     "Entr_mean"   
 # [8] "Corr_mean"    "ASM_mean"     "mns_mean"     "ndvi_mean"    "ValueFC_mean" "vr_sd"        "vg_sd"       
# [15] "vb_sd"        "nir_sd"       "Var_sd"       "IDM_sd"       "Entr_sd"      "Corr_sd"      "ASM_sd"      
# [22] "mns_sd"       "ndvi_sd"      "ValueFC_sd"   "vr_min"       "vg_min"       "vb_min"       "nir_min"     
# [29] "Var_min"      "IDM_min"      "Entr_min"     "Corr_min"     "ASM_min"      "mns_min"      "ndvi_min"    
# [36] "ValueFC_min"  "vr_max"       "vg_max"       "vb_max"       "nir_max"      "Var_max"      "IDM_max"     
# [43] "Entr_max"     "Corr_max"     "ASM_max"      "mns_max"      "ndvi_max"     "ValueFC_max"  "NbTT0"       
# [50] "Ess"          "layer"       
names(a)[51] <- "PA"
writeRaster(a, "E:/Documents/LongTermeForet/Orthophotos/dataClassifPA_20m.tif", overwrite=TRUE)


#   https://www.r-exercises.com/2018/03/07/advanced-techniques-with-raster-data-part-2-supervised-classification/
library(raster)
library(rgdal)
library(ade4)


a <- stack("E:/Documents/LongTermeForet/Orthophotos/dataClassifPA_20m.tif")
lay <- c("vr", "vg", "vb", "nir", "Var", "IDM", "Entr", "Corr", "ASM", "mns", "ndvi", "ValueFC")
stats <- c("mean", "sd", "min", "max")
ls <- expand.grid(lay, stats)
names(a) <- c(paste(ls[, 1], "_", ls[, 2], sep=""), "NbTT0", "Ess", "PA")


mask20m.sum <- raster("E:/Documents/LongTermeForet/Orthophotos/maskSum_20m.tif")
a[mask20m.sum < 200 | is.na(mask20m.sum)] <- NA


pa <- subset(a, "PA")
a <- dropLayer(a, "PA")
nr <- getValues(a)
idx <- 1:ncell(a)
idx <- idx[-unique(which(is.na(nr), arr.ind=TRUE)[,1])]  

acp <- dudi.pca(na.omit(nr), scannf = FALSE, nf = 10) # 10
acp <- dudi.pca(na.omit(nr)) # ~tous (37)

n <- 37

racp <- subset(a, 1:n)
racp[] <- NA
racp[idx] <- as.matrix(acp$li)
# plot(racp) 
names(racp) <- paste("F", 1:n, sep="")
writeRaster(racp, filename=paste("E:/Documents/LongTermeForet/Orthophotos/rPCA", n, "mask_20m.tif", sep=""), overwrite=TRUE)
racp <- stack("E:/Documents/LongTermeForet/Orthophotos/rPCA10mask_20m.tif")
names(racp) <- paste("F", 1:n, sep="")

pa <- as.factor(pa)
a <- stack(racp, pa)
rm(acp, idx, nr, racp)
gc();gc()
gc();gc()

table(getValues(subset(a, "PA")))
 # 0   1 
# 513 72

library(randomForest)
datr <- as.data.frame(getValues(a))
datr <- datr[!is.na(datr$PA),]
datr$PA <- as.factor(datr$PA)
modelRF <- randomForest(PA ~., data=datr, importance = TRUE)

## Inspect the confusion matrix of the OOB error assessment
modelRF$confusion
    # 0 1 class.error
# 0 418 6  0.01415094
# 1  27 6  0.81818182

varImpPlot(modelRF)


# MASKs :
mask20m.sum <- raster("E:/Documents/LongTermeForet/Orthophotos/maskSum_20m.tif")
a[mask20m.sum < 200 | is.na(mask20m.sum)] <- NA
ax <- dropLayer(a, "PA")


apr <- predict(ax, model=modelRF, na.rm=TRUE)

plot(apr)
writeRaster(apr, filename="E:/Documents/LongTermeForet/Orthophotos/predRFelargie37_mask_20m.tif", overwrite=TRUE)


datr <- as.data.frame(getValues(subset(a, c(1:6, 11))))
datr <- datr[!is.na(datr$PA),]
datr$PA <- as.factor(datr$PA)
modelRF <- randomForest(PA ~., data=datr, importance = TRUE)

## Inspect the confusion matrix of the OOB error assessment
modelRF$confusion
    # 0 1 class.error
# 0 418 6  0.01415094
# 1  27 6  0.81818182

varImpPlot(modelRF)


# MASKs :
mask20m.sum <- raster("E:/Documents/LongTermeForet/Orthophotos/maskSum_20m.tif")
a[mask20m.sum < 200 | is.na(mask20m.sum)] <- NA
a <- dropLayer(a, "PA")


apr <- predict(a, model=modelRF, na.rm=TRUE)
apr[mask20m.sum < 200 | is.na(mask20m.sum)] <- NA

plot(apr)
writeRaster(apr, filename="E:/Documents/LongTermeForet/Orthophotos/predRFelagie_mask_20m.tif", overwrite=TRUE)







