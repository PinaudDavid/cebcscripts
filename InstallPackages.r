# installe les packages suivants apr�s une installation d'une nouvelle version
# de R.
# Cette mani�re permet de faire automatiquement et rapidement, sans notamment les droits administrateurs.

# David Pinaud, pinaud@cebc.cnrs.fr
# 29/04/2019

# To know were your package are installed:
.libPaths() 



### d'apr�s un entrep�t ('repos') du CRAN :
		# les sites miroirs disponibles :
mir <- getCRANmirrors()
mir[mir$Country=="France",]  # Parmis ceux situ�s en France, les 2 plus rapides :
# Montpellier [https]    https://ftp.igh.cnrs.fr/pub/CRAN/
# Lyon 1 [https]     https://pbil.univ-lyon1.fr/CRAN/
# ...

install.packages(c("adehabitatHR", "adehabitatHS", "adehabitatLT", "adehabitatMA", "AICcmodavg",
"bioacoustics",
     "caTools", "circular", 
	 "devtools", "dplyr",
	 # "fields", 
	 "gam", "gdalUtils", "gdistance", "geoR", "geoRglm", "ggplot2", "glmmML", "gstat", 
	 "installr",
	 "lme4", 
	 "maps", "mapdata", "maptools", "mapproj", "mapview", 
	 "nlme", 
	 # "PBSmapping", "proj4", 
    "raster", "rasterVis", "Rcpp", "rgdal", "rgeos", "RColorBrewer", "RODBC", 
	# "RQGIS", 
	"seewave", "shapes", "sgeostat", "sp", "spatstat", "spdep", "splancs", "spgrass6", 
	"tuneR", "trip", "tripEstimation", 
	"viridis", "viridisLite"), repos = "https://ftp.igh.cnrs.fr/pub/CRAN/", 
    dependencies = TRUE)

### ou un autre ('sourceforge')
install.packages("raster", repos = "http://R-Forge.R-project.org")

### d'apr�s un fichier .zip, en local :
install.packages(pkgs = "C:/Documents and Settings/pinaud/Mes documents/Outils/R/Packages/spgrass6_0.6-14.zip", 
    dependencies = TRUE)
	
### ou sur une adresse web :
install.packages(pkgs = "http://spatial.nhh.no/R/Devel/spgrass6_0.6-13.zip", dependencies = TRUE, 
    repos = NULL)
	
### GitHub (devtools requis):
# install.packages('devtools')
devtools::install_github('thomasp85/gganimate')	



# si pas de connection internet sur un PC, on t�l�charge d'abord les packages sur un autre PC puis install en local :
		
		# 1- sur un PC avec connection internet 
library(tools)
myPkgs <- c("bioacoustics", "seewave", "tuneR")  # les packages que l'on veut installer
myPkgs <- c(myPkgs, "remotes")  # on ajoute le package "remotes" qui nous servira sur le PC non connect�
pdb <- available.packages(repos = "https://pbil.univ-lyon1.fr/CRAN/")  # check les packages dispo sur le d�p�t de Lyon
dep1 <- package_dependencies(myPkgs, db = pdb, recursive = TRUE) # all arguments at default
lis <- unique(c(myPkgs, unlist(dep1)))
install.packages(pkgs=lis, type="source", destdir="E:/Documents/pkgs",
					repos = "https://pbil.univ-lyon1.fr/CRAN/")
					# --> les sources des packages (*.tar.gz) sont maintenant dans le r�pertoire sp�cifi�, � copier sur le PC non connect�

		# 2 - sur le PC sans connection internet
path_to_file2 <- "E:/Documents/pkgs"  # le r�pertoire sur le 2eme PC avec les fichiers des packages *.tar.gz
install.packages(paste(path_to_file2, "/remotes_2.3.0.tar.gz", repos = NULL, type="source") # on installe d'abord "remotes" (v�rifier le nom du fichier, sa version)
remotes:::install_local(path_to_file2)


# installation de R nouvelle version via package installr
library(installr)	
updateR()