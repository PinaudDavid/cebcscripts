# image raster avec legende manuelle

plot(cond2, axes=F, box=F, legend=FALSE, col=green.colors(500))
r.range <- c(0, 10)
plot(cond2, legend.only=TRUE, col=green.colors(500),
     legend.width=1.0, legend.shrink=0.5,
     axis.args=list(at=seq(0, 1.2, 0.2),
                    labels=c(seq(0, 1, 0.2), 10), 
                    cex.axis=0.8),
     legend.args=list(text="Conductance", side=4, font=2, line=-2, cex=1))
plot(colo, add=T, pch=24, bg="grey", cex=1.7, lwd=2)
sc <- 2000
Xa= 426010
Ya=6526829
Xb=Xa+sc
Yb=6526829
# rect(xleft=Xa-50, ybottom=Ya-290, xright=Xb+250, ytop=Ya+270, border="grey", col="white")
arrows(x0=Xa, x1=Xb, y0=Ya, angle=90, code=3, length=0.05)
text(x=Xa+sc/2, y=Ya-330, labels="2000 m", cex=0.7)
