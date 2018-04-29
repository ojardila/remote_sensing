dyn.load('/Library/Java/JavaVirtualMachines/jdk-10.jdk/Contents/Home/lib/server/libjvm.dylib')

library(xlsx)
library(ggplot2)
library(reshape2)
library(xml2)
library(rjson)
library(lubridate)
library(RStoolbox)
library(raster)

setwd("/Users/oscar/Code/Maestria/perc_remota/data/")

spectralProfile <- function(raster_data,name) {
bands <- c("Blue","Green","Red","NIR")
band_centers <- c(485.0, 545.0, 630.0, 820.0)
names(raster_data) <- bands

reflectance_profile <- cellStats(raster_data, stat='mean', na.rm=TRUE)
  
solar_irradiance <- read.csv(file="solar_irradiance.csv", header=TRUE, sep=",")
rsr <- read.csv(file="rsr.csv", header=TRUE, sep=",")


## Import OLI band response
bands <- c("Blue","Green","Red","NIR")
rsr$wl <- rsr$wl * 1000
resp <- lapply(seq(2,ncol(rsr)), function(x){
  tmp <- rsr[,c(1,(x))]
  names(tmp) <- c("Wavelength","BA.RSR..watts.")
  data.frame(tmp)
})
names(resp) <- bands

## Plot
respDf <- melt(resp, c("Wavelength", "BA.RSR..watts."))
plotbase <- ggplot(respDf, aes( x = Wavelength, y = BA.RSR..watts., colour = L1)) +
  geom_line() + ggtitle(name)+
  xlab("Wavelength (nm)")+
  ylab("Relative Spectral Response")

d1 <- data.frame(c1 = band_centers, c2 = reflectance_profile, L1 = bands)

addlinetoplot <- function(dataset, varx, vary) { 
  list(
    geom_line(data=dataset, aes_string(x=varx, y=vary,group=1)  ), 
    geom_point(data=dataset, aes_string(x=varx, y=vary))
  )
}
plotbase + addlinetoplot(d1, varx = "c1", vary = "c2")
}




water_reflectance = stack("Datos\ de\ Salida/agua-32.tif")



cafe_reflectance = stack("Datos\ de\ Salida/cafe-33.tif")



urban_reflectance = stack("Datos\ de\ Salida/casco_urbano-32.tif")
= stack("Datos\ de\ Salida/casco_urbano-32.tif")





