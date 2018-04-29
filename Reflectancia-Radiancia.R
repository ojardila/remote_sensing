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
solar_irradiance <- read.csv(file="solar_irradiance.csv", header=TRUE, sep=",")
ggplot(solar_irradiance, aes(x = nm, y = mW.m2.nm)) + geom_line() + ggtitle("Thuillier (2003) Solar Irradiance Spectrum") + xlab("Wavelength (nm)")
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
ggplot(respDf, aes( x = Wavelength, y = BA.RSR..watts., colour = L1)) +
  geom_line() + ggtitle("Planetscope spectral response")+
  xlab("Wavelength (nm)")+
  ylab("Relative Spectral Response")

## Linearly interpolate solar irradiance spectrum even finer
sora <- range(solar_irradiance[,1])
xnew <- seq(sora[1],sora[2], 0.01)
ynew <- resamp  <- approx(x = solar_irradiance[,1], y = solar_irradiance[,2], xout = xnew)[["y"]]
solar_irradiance  <- cbind(xnew,ynew)


Esun <- sapply(resp, function(x){
  ## Resample band response to solar spectrum
  resamp  <- approx(x = x[,1], y = x[,2], xout = solar_irradiance[,1])[["y"]]
  ## Convert to relative weights  
  weights <- resamp / sum(resamp, na.rm = TRUE)
  ## Calculated weighted sum
  sum(solar_irradiance[,2] * weights, na.rm = TRUE)            
})

#Loading Radiometric scale factors
image_id='20171220_143931_1024'
image_metadata_route = sprintf("images/%s/%s_3B_AnalyticMS_metadata.xml",image_id,image_id)
image_metadata <- read_xml(image_metadata_route)
radiometric_scale_factors <- xml_double(xml_find_all(image_metadata, "//ps:radiometricScaleFactor"))
image_acquisition_date <- xml_text(xml_find_all(image_metadata, "//ps:acquisitionDateTime"))
image_yday <- yday(as.Date(image_acquisition_date))
names(radiometric_scale_factors) <- bands
image_json_route = sprintf("images/%s/%s_metadata.json",image_id,image_id)
json_data <- fromJSON(paste(readLines(image_json_route), collapse=""))
#Loading image
image_route <- sprintf("images/%s/%s_3B_AnalyticMS.tif",image_id,image_id)
image <-  stack(image_route)
names(image) <- bands

#TOA Reflectance

reflectance_coefficient <- xml_double(xml_find_all(image_metadata, "//ps:reflectanceCoefficient"))
names(reflectance_coefficient) <- bands
ac_toa <- image
for(band in bands) {
  ac_toa[[band]] <- reflectance_coefficient[band] * image[[band]]
}

#Solar Zenith Angle
theta <- 90 -json_data$properties$sun_elevation




#Calculating  spectral radiance for each band in the image
lsx <- image
for(band in bands) {
  lsx[[band]] <- radiometric_scale_factors[band] * image[[band]]
}

#Estimating Earth-Sun Distance d
.ESdistance <- readRDS("sun_earth_dists.rds")
d <- .ESdistance[image_yday]

#Calculating Lpath
haze <- estimateHaze(lsx, hazeBands = 1:nlayers(image), plot = TRUE)
Ldo       <- (0.01 * (Esun * cos(theta))) / (pi * d ^ 2)
Lpath <- haze - Ldo

# Calculating DOS correction
ac_dos <- image
for(band in bands) {
  fun <- function(x) { (pi * d^ 2 * (x - Lpath[band])) / (Esun[band] * cos(theta)) }
  ac_dos[[band]] <- calc(lsx[[band]],fun)
}
# Calculating COS correction
ac_cos <- ac_dos / cos( theta)

grayscale_colors <- gray.colors(100,            # number of different color levels 
                                start = 0.0,    # how black (0) to go
                                end = 1.0,      # how white (1) to go
                                gamma = 2.2,    # correction between how a digital 
                                # camera sees the world and how human eyes see it
                                alpha = NULL)   #Null=colors are notT transparent
plot(ac_dos[["Blue"]], 
     col=grayscale_colors, 
     axes=FALSE,
     main="Blue Band DOS Reflectance") 


