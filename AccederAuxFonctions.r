## regarder dans une fonction

http://cran.r-project.org/doc/Rnews/Rnews_2006-4.pdf

# S3
sd
methods(drop1)
stats:::drop1.glm
getAnywhere("drop1")

# S4 & Generic
summary
showMethods("summary")
isGeneric("summary")
findMethods("summary", classes="ANY")

mm <-  findMethods("summary")
findMethodSignatures(methods = mm)

methods("plot", class="raster")
getMethod(f="plot", signature="Raster")

