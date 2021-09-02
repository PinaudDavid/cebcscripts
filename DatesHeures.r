# format Date et heure

######### extraction d'après un nom de fichier SM3BAT
# "SM01__0__20180626_212703"
#  123456789012345678901234
a$DateTimeStart <- strptime(substr(a$FileName, 10, 24), "%Y%m%d_%H%M%S")

as.Date(a$DateTimeStart, tz=XXX) # la date
a$DateNuit <- ifelse(hh < 12, as.Date(a$DateTimeStart, tz=XXX) - 1, as.Date(a$DateTimeStart,tz=XXX)) # date nuit

# fraction seconds:
as.numeric(strptime(x, format="%Y-%m-%d %H:%M:%OS"))

#### fichiers GPS, Argos.... :
a$datetime <- as.POSIXct(strptime(as.character(a$datetime), "%d/%m/%Y %H:%M:%S"), tz="GMT")


### Time Zone
 Sys.timezone()  # actuel sur le système
 # time-zone database : see https://twiki.org/cgi-bin/xtra/tzdatepick.html
 
 
 
## suppose we have a time in seconds since 1960-01-01 00:00:00 GMT
## (the origin used by SAS)
z <- 1472562988
# ways to convert this
as.POSIXct(z, origin = "1960-01-01")                # local
as.POSIXct(z, origin = "1960-01-01", tz = "GMT")    # in UTCas.POSIXct(z, origin = "1960-01-01")  
 
 
 