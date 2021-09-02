library(adehabitatHR)


BS20852loc <- read.table("I:/Labo/AGoarant/PredM/RICARD/Exemples trajet/BS20852loc.txt", 
    sep = "\t", h = T)


plot(BS20852loc[, c("Long", "Lat")], t = "l")
kUD <- kernelUD(BS20852loc[, c("Long", "Lat")], h = 1, grid = 500)
image(kUD)
udvol <- getvolumeUD(kUD)
image(udvol)

plot(BS20852loc[, c("Long", "Lat")], t = "l")
ver <- getverticeshr(kUD, 95)
plot(ver, add = TRUE, lwd = 1, colborder = "red", colpol = NA)
ver <- getverticeshr(kUD, 50)
plot(ver, add = TRUE, lwd = 1, colborder = "green", colpol = NA)

for (i in seq(5, 95, by = 5))
{
    ver <- getverticeshr(kUD, i)
    plot(ver, add = TRUE, lwd = 1, colborder = "red", colpol = NA)
}
points(BS20852loc[, c("Long", "Lat")])
 
