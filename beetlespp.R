### POINT PROCESSES BEETLES

start_time <- Sys.time()

library(tidyverse)
library(terra)
library(raster)
library(sf)
library(sp)

# Load required package
library(readxl)

# URL of the Excel file on GitHub
file_url <- "https://github.com/abraham-arbelaez/SDMs-beetles-project/raw/main/JB_GBIF_DataByState.xlsx"

# Download the Excel file
download.file(file_url, destfile = "JB_GBIF_DataByState.xlsx", mode = "wb")

# Read the Excel file into R
data <- read_excel("JB_GBIF_DataByState.xlsx")

library(tidyverse)

dataI <- data %>%
  filter(State == "Iowa")

dataK <- data %>%
  filter(State == "Kansas")

dataM <- data %>%
  filter(State == "Missouri")

dataN <- data %>%
  filter(State == "Nebraska")

data <- rbind(dataI, dataK, dataM, dataN)

unique(data$Year)

data <- data %>%
  filter(Year >= 2017)

data <- data %>%
  filter(Year <= 2021)

unique(data$Year)

data2017 <- data %>%
  filter(Year == 2017)

data2018 <- data %>%
  filter(Year == 2018)

data2019 <- data %>%
  filter(Year == 2019)

data2020 <- data %>%
  filter(Year == 2020)

data2021 <- data %>%
  filter(Year == 2021)

library(sf)
library(sp)

data <- st_as_sf(data, coords = c("Longitude", "Latitude"))

data2017s <- st_as_sf(data2017, coords = c("Longitude", "Latitude"))

data2018s <- st_as_sf(data2018, coords = c("Longitude", "Latitude"))

data2019s <- st_as_sf(data2019, coords = c("Longitude", "Latitude"))

data2020s <- st_as_sf(data2020, coords = c("Longitude", "Latitude"))

data2021s <- st_as_sf(data2021, coords = c("Longitude", "Latitude"))

#########################################
#
# county
#
#########################################

cty <- st_read("cb_2018_us_county_500k/cb_2018_us_county_500k.shp")

ctyi <- cty %>%
  filter(STATEFP == "19")

ctyk <- cty %>%
  filter(STATEFP == "20")

ctym <- cty %>%
  filter(STATEFP == "29")

ctyn <- cty %>%
  filter(STATEFP == "31")

cty <- rbind(ctyi, ctyk, ctym, ctyn)

# transforming crs of data

st_crs(data) <- st_crs(cty)

st_crs(data2017s) <- st_crs(cty)

st_crs(data2018s) <- st_crs(cty)

st_crs(data2019s) <- st_crs(cty)

st_crs(data2020s) <- st_crs(cty)

st_crs(data2021s) <- st_crs(cty)

# some plots

ggplot() +
  geom_sf(data = cty) +
  geom_sf(data = data2017s) +
  theme_minimal()

ggplot() +
  geom_sf(data = cty) +
  geom_sf(data = data2018s) +
  theme_minimal()

ggplot() +
  geom_sf(data = cty) +
  geom_sf(data = data2019s) +
  theme_minimal()

ggplot() +
  geom_sf(data = cty) +
  geom_sf(data = data2020s) +
  theme_minimal()

ggplot() +
  geom_sf(data = cty) +
  geom_sf(data = data2021s) +
  theme_minimal()

ggplot() +
  geom_sf(data = cty) +
  geom_sf(data = data) +
  theme_minimal()

ggplot() +
  geom_sf(data = cty) +
  geom_sf(data = data, size = 0.5) +
  theme_minimal()

table(data$Year)

# Get raster for all states!

library(cdlTools)

ks.nlcd <- getCDL("Kansas", c(2017,2018, 2019, 2020, 2021))
ks.nlcd.17 <- ks.nlcd[[1]]
ks.nlcd.18 <- ks.nlcd[[2]]
ks.nlcd.19 <- ks.nlcd[[3]]
ks.nlcd.20 <- ks.nlcd[[4]]
ks.nlcd.21 <- ks.nlcd[[5]]

mo.nlcd <- getCDL("Missouri", c(2017,2018, 2019, 2020, 2021))
mo.nlcd.17 <- mo.nlcd[[1]]
mo.nlcd.18 <- mo.nlcd[[2]]
mo.nlcd.19 <- mo.nlcd[[3]]
mo.nlcd.20 <- mo.nlcd[[4]]
mo.nlcd.21 <- mo.nlcd[[5]]

ne.nlcd <- getCDL("Nebraska", c(2017,2018, 2019, 2020, 2021))
ne.nlcd.17 <- ne.nlcd[[1]]
ne.nlcd.18 <- ne.nlcd[[2]]
ne.nlcd.19 <- ne.nlcd[[3]]
ne.nlcd.20 <- ne.nlcd[[4]]
ne.nlcd.21 <- ne.nlcd[[5]]

ia.nlcd <- getCDL("Iowa", c(2017,2018, 2019, 2020, 2021))
ia.nlcd.17 <- ia.nlcd[[1]]
ia.nlcd.18 <- ia.nlcd[[2]]
ia.nlcd.19 <- ia.nlcd[[3]]
ia.nlcd.20 <- ia.nlcd[[4]]
ia.nlcd.21 <- ia.nlcd[[5]]

### For 2017

nlcd.17 <- mosaic(ks.nlcd.17, mo.nlcd.17, ne.nlcd.17, ia.nlcd.17, fun = sum)
plot(nlcd.17)
plot(st_coordinates(data2017s))

st_crs(nlcd.17) == st_crs(data2017s)

points_reprojected17 <- st_transform(data2017s, crs = st_crs(nlcd.17))

st_crs(nlcd.17) == st_crs(points_reprojected17)

plot(nlcd.17)
points(points_reprojected17)

## Corn and Soy
nlcd.17.corn <- nlcd.17
nlcd.17.corn[] <- ifelse(nlcd.17.corn[] %in% c(1,12,13,225,226,228,237,241,
                                               5,26,239,240,241,254), 1,0)

plot(nlcd.17)
plot(nlcd.17.corn)

beetles.17 <- points_reprojected17

beetles.17$CornAndSoy <- unlist(lapply(raster::extract(nlcd.17.corn, 
                                                       points_reprojected17,
                                                       buffer=15000),
                                       mean))

## Developed total
nlcd.17.dev <- nlcd.17
nlcd.17.dev[] <- ifelse(nlcd.17.dev[] %in% c(82,121,122,123,124), 1,0)

plot(nlcd.17)
plot(nlcd.17.dev)

beetles.17$DevelopedTotal <- unlist(lapply(raster::extract(nlcd.17.dev, 
                                                           points_reprojected17,
                                                           buffer=15000),
                                           mean))

## Deciduous Forest
nlcd.17.decforest <- nlcd.17
nlcd.17.decforest[] <- ifelse(nlcd.17.decforest[] %in% c(141), 1,0)

plot(nlcd.17)
plot(nlcd.17.decforest)

beetles.17$DeciduousForest <- unlist(lapply(raster::extract(nlcd.17.decforest, 
                                                           points_reprojected17,
                                                           buffer=15000),
                                           mean))

## Grass pasture
nlcd.17.grass <- nlcd.17
nlcd.17.grass[] <- ifelse(nlcd.17.grass[] %in% c(62,176), 1,0)

plot(nlcd.17)
plot(nlcd.17.grass)

beetles.17$GrassPasture <- unlist(lapply(raster::extract(nlcd.17.grass, 
                                                            points_reprojected17,
                                                            buffer=15000),
                                            mean))


## Avg Temperature
avgtemp.17 <- raster("climate/2017/PRISM_tmean_stable_4kmM3_2017_bil/PRISM_tmean_stable_4kmM3_2017_bil.bil")

plot(avgtemp.17)

st_crs(nlcd.17) == st_crs(avgtemp.17)

avgtemp.17 <- projectRaster(avgtemp.17, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.17) == st_crs(avgtemp.17)

plot(avgtemp.17)
points(points_reprojected17)

avgtemp.17 <- crop(avgtemp.17, nlcd.17)

plot(avgtemp.17)
points(points_reprojected17)

beetles.17$AvgTemp<- unlist(lapply(raster::extract(avgtemp.17, 
                                                            points_reprojected17,
                                                            buffer=15000),
                                            mean))



## Min Temperature
mintemp.17 <- raster("climate/2017/PRISM_tmin_stable_4kmM3_2017_bil/PRISM_tmin_stable_4kmM3_2017_bil.bil")

plot(mintemp.17)

st_crs(nlcd.17) == st_crs(mintemp.17)

mintemp.17 <- projectRaster(mintemp.17, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.17) == st_crs(mintemp.17)

plot(mintemp.17)
points(points_reprojected17)

mintemp.17 <- crop(mintemp.17, nlcd.17)

plot(mintemp.17)
points(points_reprojected17)

beetles.17$MinTemp<- unlist(lapply(raster::extract(mintemp.17, 
                                                   points_reprojected17,
                                                   buffer=15000),
                                   mean))


## Max Temperature
maxtemp.17 <- raster("climate/2017/PRISM_tmax_stable_4kmM3_2017_bil/PRISM_tmax_stable_4kmM3_2017_bil.bil")

plot(maxtemp.17)

st_crs(nlcd.17) == st_crs(maxtemp.17)

maxtemp.17 <- projectRaster(maxtemp.17, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.17) == st_crs(maxtemp.17)

plot(maxtemp.17)
points(points_reprojected17)

maxtemp.17 <- crop(maxtemp.17, nlcd.17)

plot(maxtemp.17)
points(points_reprojected17)

beetles.17$MaxTemp<- unlist(lapply(raster::extract(maxtemp.17, 
                                                   points_reprojected17,
                                                   buffer=15000),
                                   mean))


## Precipitation
precip.17 <- raster("climate/2017/PRISM_ppt_stable_4kmM3_2017_bil/PRISM_ppt_stable_4kmM3_2017_bil.bil")

plot(precip.17)

st_crs(nlcd.17) == st_crs(precip.17)

precip.17 <- projectRaster(precip.17, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.17) == st_crs(precip.17)

plot(precip.17)
points(points_reprojected17)

precip.17 <- crop(precip.17, nlcd.17)

plot(precip.17)
points(points_reprojected17)

beetles.17$Precipitation <- unlist(lapply(raster::extract(precip.17, 
                                                   points_reprojected17,
                                                   buffer=15000),
                                   mean))


### MODEL

## setting boundary
t.nlcd <- rast(nlcd.17)
plot(t.nlcd)
#t.nlcd[t.nlcd == 0] <- NA
#plot(t.nlcd)
bd <- as.polygons(t.nlcd > 0)
bd <- as(bd, 'Spatial') 
plot(bd)


quad.17 <- spsample(bd, n = 20, type = "random")
plot(quad.17)


plot(bd)
#plot(quad.17, add = T)
plot(beetles.17$geometry, pch = 3, col = "red", add = T)


## getting vars for quad ----
# Make SpatialPoints data frame
df.quad.17 <- data.frame(long = coordinates(quad.17)[,1],lat = coordinates(quad.17)[,2])
quad.beetles17 <- data.frame(long = coordinates(quad.17)[,1],lat = coordinates(quad.17)[,2])
coordinates(quad.beetles17) =~ long + lat
proj4string(quad.beetles17) <- CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")


df.quad.17$CornAndSoy <- unlist(lapply(raster::extract(nlcd.17.corn, 
                                                       quad.beetles17,
                                                       buffer=15000),
                                       mean))

df.quad.17$DevelopedTotal <- unlist(lapply(raster::extract(nlcd.17.dev, 
                                                           quad.beetles17,
                                                           buffer=15000),
                                           mean))

df.quad.17$DeciduousForest <- unlist(lapply(raster::extract(nlcd.17.decforest, 
                                                            quad.beetles17,
                                                            buffer=15000),
                                            mean))

df.quad.17$GrassPasture <- unlist(lapply(raster::extract(nlcd.17.grass, 
                                                         quad.beetles17,
                                                         buffer=15000),
                                         mean))

df.quad.17$AvgTemp<- unlist(lapply(raster::extract(avgtemp.17, 
                                                   quad.beetles17,
                                                   buffer=15000),
                                   mean))

df.quad.17$MinTemp<- unlist(lapply(raster::extract(mintemp.17, 
                                                   quad.beetles17,
                                                   buffer=15000),
                                   mean))

df.quad.17$MaxTemp<- unlist(lapply(raster::extract(maxtemp.17, 
                                                   quad.beetles17,
                                                   buffer=15000),
                                   mean))

df.quad.17$Precipitation <- unlist(lapply(raster::extract(precip.17, 
                                                          quad.beetles17,
                                                          buffer=15000),
                                          mean))


df.quad.17$presence <- rep(0, length(df.quad.17$CornAndSoy))
beetles.17$presence <- rep(1, length(beetles.17$CornAndSoy))

df.quad.17t <- st_as_sf(df.quad.17, coords = c("long", "lat"))

st_crs(df.quad.17t) <- st_crs(beetles.17)

df.quad.17t <- st_transform(df.quad.17t, crs = st_crs(beetles.17))

df.quad.17t$long <- st_coordinates(df.quad.17t$geometry)[,1]
df.quad.17t$lat <- st_coordinates(df.quad.17t$geometry)[,2]

beetles.17$long <- st_coordinates(beetles.17$geometry)[,1]
beetles.17$lat <- st_coordinates(beetles.17$geometry)[,2]

beetles_17 <- beetles.17[,25:35]
beetles_17 <- rbind(beetles_17, df.quad.17t)


#######################
## FINALLY MODELING! ##
#######################

wt <- 1e-5
beetles_17$wt <- wt

f1 <- presence/wt ~ CornAndSoy + DevelopedTotal + DeciduousForest +
  GrassPasture + AvgTemp + MinTemp + MaxTemp + Precipitation

glmmod<- glm(f1, data = beetles_17, family = poisson, weights = wt)

summary(glmmod)

# GAMS with Backward Elimination
library(mgcv)

f2 <- update(f1, ~ . + s(long,lat, bs = "gp"))

ipp <- gam(f2, data = beetles_17, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f3 <- update(f2, ~ . + s(long,lat, bs = "gp") - Precipitation)

ipp <- gam(f3, data = beetles_17, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f4 <- update(f3, ~ . + s(long,lat, bs = "gp") - MinTemp)

ipp <- gam(f4, data = beetles_17, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f5 <- update(f4, ~ . + s(long,lat, bs = "gp") - MaxTemp)

ipp <- gam(f5, data = beetles_17, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f6 <- update(f5, ~ . + s(long,lat, bs = "gp") - AvgTemp)

ipp <- gam(f6, data = beetles_17, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f7 <- update(f6, ~ . + s(long,lat, bs = "gp") - GrassPasture)

ipp.17 <- gam(f7, data = beetles_17, family = poisson,
           weights = wt, method = "REML")

summary(ipp.17)


#################
## PREDICTIONS ##
#################


rl.E.y.17 <- raster(,nrow=65,ncols=65,ext=extent(bd),crs=crs(bd))
df.pred.17 <- data.frame(long = xyFromCell(rl.E.y.17,cell=1:length(rl.E.y.17[]))[,1],
                      lat = xyFromCell(rl.E.y.17,cell=1:length(rl.E.y.17[]))[,2])

pred.CornAndSoy <- unlist(lapply(raster::extract(nlcd.17.corn, 
                                                 df.pred.17,
                                                       buffer=15000),
                                       mean))

pred.DevelopedTotal <- unlist(lapply(raster::extract(nlcd.17.dev, 
                                                     df.pred.17,
                                                           buffer=15000),
                                           mean))

pred.DeciduousForest <- unlist(lapply(raster::extract(nlcd.17.decforest, 
                                                      df.pred.17,
                                                            buffer=15000),
                                            mean))

pred.GrassPasture <- unlist(lapply(raster::extract(nlcd.17.grass, 
                                                   df.pred.17,
                                                         buffer=15000),
                                         mean))

pred.AvgTemp<- unlist(lapply(raster::extract(avgtemp.17, 
                                             df.pred.17,
                                                   buffer=15000),
                                   mean))

pred.MinTemp<- unlist(lapply(raster::extract(mintemp.17, 
                                             df.pred.17,
                                                   buffer=15000),
                                   mean))

pred.MaxTemp<- unlist(lapply(raster::extract(maxtemp.17, 
                                             df.pred.17,
                                                   buffer=15000),
                                   mean))

pred.Precipitation <- unlist(lapply(raster::extract(precip.17, 
                                                    df.pred.17,
                                                          buffer=15000),
                                          mean))

df.pred.17$CornAndSoy <- pred.CornAndSoy
df.pred.17$DevelopedTotal <- pred.DevelopedTotal
df.pred.17$DeciduousForest <- pred.DeciduousForest
df.pred.17$GrassPasture <- pred.GrassPasture
df.pred.17$AvgTemp <- pred.AvgTemp
df.pred.17$MinTemp <- pred.MinTemp
df.pred.17$pred.MaxTemp <- pred.MaxTemp
df.pred.17$Precipitation <- pred.Precipitation



rl.E.y.17[] <- exp(c(predict(ipp.17,newdata=df.pred.17,type="link")))

#image(rl.E.y,axes=FALSE,box=FALSE)
plot(rl.E.y.17,axes=FALSE,box=FALSE)
plot(bd,add=TRUE)
points(points_reprojected17, pch = 16, col = "red")



####################################################################################################################

## For 2018

nlcd.18 <- mosaic(ks.nlcd.18, mo.nlcd.18, ne.nlcd.18, ia.nlcd.18, fun = sum)
plot(nlcd.18)
plot(st_coordinates(data2018s))

st_crs(nlcd.18) == st_crs(data2018s)

points_reprojected18 <- st_transform(data2018s, crs = st_crs(nlcd.18))

st_crs(nlcd.18) == st_crs(points_reprojected18)

plot(nlcd.18)
points(points_reprojected18)

## Corn and Soy
nlcd.18.corn <- nlcd.18
nlcd.18.corn[] <- ifelse(nlcd.18.corn[] %in% c(1,12,13,225,226,228,237,241,
                                               5,26,239,240,241,254), 1,0)

plot(nlcd.18)
plot(nlcd.18.corn)

beetles.18 <- points_reprojected18

beetles.18$CornAndSoy <- unlist(lapply(raster::extract(nlcd.18.corn, 
                                                       points_reprojected18,
                                                       buffer=15000),
                                       mean))

## Developed total
nlcd.18.dev <- nlcd.18
nlcd.18.dev[] <- ifelse(nlcd.18.dev[] %in% c(82,121,122,123,124), 1,0)

plot(nlcd.18)
plot(nlcd.18.dev)

beetles.18$DevelopedTotal <- unlist(lapply(raster::extract(nlcd.18.dev, 
                                                           points_reprojected18,
                                                           buffer=15000),
                                           mean))

## Deciduous Forest
nlcd.18.decforest <- nlcd.18
nlcd.18.decforest[] <- ifelse(nlcd.18.decforest[] %in% c(141), 1,0)

plot(nlcd.18)
plot(nlcd.18.decforest)

beetles.18$DeciduousForest <- unlist(lapply(raster::extract(nlcd.18.decforest, 
                                                            points_reprojected18,
                                                            buffer=15000),
                                            mean))

## Grass pasture
nlcd.18.grass <- nlcd.18
nlcd.18.grass[] <- ifelse(nlcd.18.grass[] %in% c(62,176), 1,0)

plot(nlcd.18)
plot(nlcd.18.grass)

beetles.18$GrassPasture <- unlist(lapply(raster::extract(nlcd.18.grass, 
                                                         points_reprojected18,
                                                         buffer=15000),
                                         mean))


## Avg Temperature
avgtemp.18 <- raster("climate/2018/PRISM_tmean_stable_4kmM3_2018_bil/PRISM_tmean_stable_4kmM3_2018_bil.bil")

plot(avgtemp.18)

st_crs(nlcd.18) == st_crs(avgtemp.18)

avgtemp.18 <- projectRaster(avgtemp.18, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.18) == st_crs(avgtemp.18)

plot(avgtemp.18)
points(points_reprojected18)

avgtemp.18 <- crop(avgtemp.18, nlcd.18)

plot(avgtemp.18)
points(points_reprojected18)

beetles.18$AvgTemp<- unlist(lapply(raster::extract(avgtemp.18, 
                                                   points_reprojected18,
                                                   buffer=15000),
                                   mean))



## Min Temperature
mintemp.18 <- raster("climate/2018/PRISM_tmin_stable_4kmM3_2018_bil/PRISM_tmin_stable_4kmM3_2018_bil.bil")

plot(mintemp.18)

st_crs(nlcd.18) == st_crs(mintemp.18)

mintemp.18 <- projectRaster(mintemp.18, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.18) == st_crs(mintemp.18)

plot(mintemp.18)
points(points_reprojected18)

mintemp.18 <- crop(mintemp.18, nlcd.18)

plot(mintemp.18)
points(points_reprojected18)

beetles.18$MinTemp<- unlist(lapply(raster::extract(mintemp.18, 
                                                   points_reprojected18,
                                                   buffer=15000),
                                   mean))


## Max Temperature
maxtemp.18 <- raster("climate/2018/PRISM_tmax_stable_4kmM3_2018_bil/PRISM_tmax_stable_4kmM3_2018_bil.bil")

plot(maxtemp.18)

st_crs(nlcd.18) == st_crs(maxtemp.18)

maxtemp.18 <- projectRaster(maxtemp.18, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.18) == st_crs(maxtemp.18)

plot(maxtemp.18)
points(points_reprojected18)

maxtemp.18 <- crop(maxtemp.18, nlcd.18)

plot(maxtemp.18)
points(points_reprojected18)

beetles.18$MaxTemp<- unlist(lapply(raster::extract(maxtemp.18, 
                                                   points_reprojected18,
                                                   buffer=15000),
                                   mean))


## Precipitation
precip.18 <- raster("climate/2018/PRISM_ppt_stable_4kmM3_2018_bil/PRISM_ppt_stable_4kmM3_2018_bil.bil")

plot(precip.18)

st_crs(nlcd.18) == st_crs(precip.18)

precip.18 <- projectRaster(precip.18, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.18) == st_crs(precip.18)

plot(precip.18)
points(points_reprojected18)

precip.18 <- crop(precip.18, nlcd.18)

plot(precip.18)
points(points_reprojected18)

beetles.18$Precipitation <- unlist(lapply(raster::extract(precip.18, 
                                                          points_reprojected18,
                                                          buffer=15000),
                                          mean))


### MODEL

## setting boundary
t.nlcd <- rast(nlcd.18)
plot(t.nlcd)
#t.nlcd[t.nlcd == 0] <- NA
#plot(t.nlcd)
#bd <- as.polygons(t.nlcd > 0)
#bd <- as(bd, 'Spatial') 
#plot(bd)


quad.18 <- spsample(bd, n = 20, type = "random")
plot(quad.18)


plot(bd)
#plot(quad.17, add = T)
plot(beetles.18$geometry, pch = 3, col = "red", add = T)


## getting vars for quad ----
# Make SpatialPoints data frame
df.quad.18 <- data.frame(long = coordinates(quad.18)[,1],lat = coordinates(quad.18)[,2])
quad.beetles18 <- data.frame(long = coordinates(quad.18)[,1],lat = coordinates(quad.18)[,2])
coordinates(quad.beetles18) =~ long + lat
proj4string(quad.beetles18) <- CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")


df.quad.18$CornAndSoy <- unlist(lapply(raster::extract(nlcd.18.corn, 
                                                       quad.beetles18,
                                                       buffer=15000),
                                       mean))

df.quad.18$DevelopedTotal <- unlist(lapply(raster::extract(nlcd.18.dev, 
                                                           quad.beetles18,
                                                           buffer=15000),
                                           mean))

df.quad.18$DeciduousForest <- unlist(lapply(raster::extract(nlcd.18.decforest, 
                                                            quad.beetles18,
                                                            buffer=15000),
                                            mean))

df.quad.18$GrassPasture <- unlist(lapply(raster::extract(nlcd.18.grass, 
                                                         quad.beetles18,
                                                         buffer=15000),
                                         mean))

df.quad.18$AvgTemp<- unlist(lapply(raster::extract(avgtemp.18, 
                                                   quad.beetles18,
                                                   buffer=15000),
                                   mean))

df.quad.18$MinTemp<- unlist(lapply(raster::extract(mintemp.18, 
                                                   quad.beetles18,
                                                   buffer=15000),
                                   mean))

df.quad.18$MaxTemp<- unlist(lapply(raster::extract(maxtemp.18, 
                                                   quad.beetles18,
                                                   buffer=15000),
                                   mean))

df.quad.18$Precipitation <- unlist(lapply(raster::extract(precip.18, 
                                                          quad.beetles18,
                                                          buffer=15000),
                                          mean))


df.quad.18$presence <- rep(0, length(df.quad.18$CornAndSoy))
beetles.18$presence <- rep(1, length(beetles.18$CornAndSoy))

df.quad.18t <- st_as_sf(df.quad.18, coords = c("long", "lat"))

st_crs(df.quad.18t) <- st_crs(beetles.18)

df.quad.18t <- st_transform(df.quad.18t, crs = st_crs(beetles.18))

df.quad.18t$long <- st_coordinates(df.quad.18t$geometry)[,1]
df.quad.18t$lat <- st_coordinates(df.quad.18t$geometry)[,2]

beetles.18$long <- st_coordinates(beetles.18$geometry)[,1]
beetles.18$lat <- st_coordinates(beetles.18$geometry)[,2]

beetles_18 <- beetles.18[,25:35]
beetles_18 <- rbind(beetles_18, df.quad.18t)


#######################
## FINALLY MODELING! ##
#######################

wt <- 1e-5
beetles_18$wt <- wt

f1 <- presence/wt ~ CornAndSoy + DevelopedTotal + DeciduousForest +
  GrassPasture + AvgTemp + MinTemp + MaxTemp + Precipitation

glmmod<- glm(f1, data = beetles_18, family = poisson, weights = wt)

summary(glmmod)

# GAMS with Backward Elimination
library(mgcv)

f2 <- update(f1, ~ . + s(long,lat, bs = "gp"))

ipp <- gam(f2, data = beetles_18, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f3 <- update(f2, ~ . + s(long,lat, bs = "gp") - AvgTemp)

ipp <- gam(f3, data = beetles_18, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f4 <- update(f3, ~ . + s(long,lat, bs = "gp") - MaxTemp)

ipp <- gam(f4, data = beetles_18, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f5 <- update(f4, ~ . + s(long,lat, bs = "gp") - Precipitation)

ipp <- gam(f5, data = beetles_18, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f6 <- update(f5, ~ . + s(long,lat, bs = "gp") - MinTemp)

ipp <- gam(f6, data = beetles_18, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f7 <- update(f6, ~ . + s(long,lat, bs = "gp") - GrassPasture)

ipp.18 <- gam(f7, data = beetles_18, family = poisson,
              weights = wt, method = "REML")

summary(ipp.18)


#################
## PREDICTIONS ##
#################

rl.E.y.18 <- raster(,nrow=65,ncols=65,ext=extent(bd),crs=crs(bd))
df.pred.18 <- data.frame(long = xyFromCell(rl.E.y.18,cell=1:length(rl.E.y.18[]))[,1],
                         lat = xyFromCell(rl.E.y.18,cell=1:length(rl.E.y.18[]))[,2])

pred.CornAndSoy <- unlist(lapply(raster::extract(nlcd.18.corn, 
                                                 df.pred.18,
                                                 buffer=15000),
                                 mean))

pred.DevelopedTotal <- unlist(lapply(raster::extract(nlcd.18.dev, 
                                                     df.pred.18,
                                                     buffer=15000),
                                     mean))

pred.DeciduousForest <- unlist(lapply(raster::extract(nlcd.18.decforest, 
                                                      df.pred.18,
                                                      buffer=15000),
                                      mean))

pred.GrassPasture <- unlist(lapply(raster::extract(nlcd.18.grass, 
                                                   df.pred.18,
                                                   buffer=15000),
                                   mean))

pred.AvgTemp<- unlist(lapply(raster::extract(avgtemp.18, 
                                             df.pred.18,
                                             buffer=15000),
                             mean))

pred.MinTemp<- unlist(lapply(raster::extract(mintemp.18, 
                                             df.pred.18,
                                             buffer=15000),
                             mean))

pred.MaxTemp<- unlist(lapply(raster::extract(maxtemp.18, 
                                             df.pred.18,
                                             buffer=15000),
                             mean))

pred.Precipitation <- unlist(lapply(raster::extract(precip.18, 
                                                    df.pred.18,
                                                    buffer=15000),
                                    mean))

df.pred.18$CornAndSoy <- pred.CornAndSoy
df.pred.18$DevelopedTotal <- pred.DevelopedTotal
df.pred.18$DeciduousForest <- pred.DeciduousForest
df.pred.18$GrassPasture <- pred.GrassPasture
df.pred.18$AvgTemp <- pred.AvgTemp
df.pred.18$MinTemp <- pred.MinTemp
df.pred.18$pred.MaxTemp <- pred.MaxTemp
df.pred.18$Precipitation <- pred.Precipitation



rl.E.y.18[] <- exp(c(predict(ipp.18,newdata=df.pred.18,type="link")))

#image(rl.E.y,axes=FALSE,box=FALSE)
plot(rl.E.y.18,axes=FALSE,box=FALSE)
plot(bd,add=TRUE)
points(points_reprojected18, pch = 16, col = "red")



#############################################################################################################


## For 2019

nlcd.19 <- mosaic(ks.nlcd.19, mo.nlcd.19, ne.nlcd.19, ia.nlcd.19, fun = sum)
#plot(nlcd.19)
#plot(st_coordinates(data2019s))

#st_crs(nlcd.19) == st_crs(data2019s)

points_reprojected19 <- st_transform(data2019s, crs = st_crs(nlcd.19))

#st_crs(nlcd.19) == st_crs(points_reprojected19)

plot(nlcd.19)
points(points_reprojected19)

## Corn and Soy
#nlcd.19.corn <- nlcd.19
#nlcd.19.corn[] <- ifelse(nlcd.19.corn[] %in% c(1,12,13,225,226,228,237,241,
#                                               5,26,239,240,241,254), 1,0)

#plot(nlcd.19)
#plot(nlcd.19.corn)

beetles.19 <- points_reprojected19

beetles.19$CornAndSoy <- unlist(lapply(raster::extract(nlcd.18.corn, 
                                                       points_reprojected19,
                                                       buffer=15000),
                                       mean))

## Developed total
#nlcd.19.dev <- nlcd.19
#nlcd.19.dev[] <- ifelse(nlcd.19.dev[] %in% c(82,121,122,123,124), 1,0)

#plot(nlcd.19)
#plot(nlcd.19.dev)

beetles.19$DevelopedTotal <- unlist(lapply(raster::extract(nlcd.18.dev, 
                                                           points_reprojected19,
                                                           buffer=15000),
                                           mean))

## Deciduous Forest
#nlcd.19.decforest <- nlcd.19
#nlcd.19.decforest[] <- ifelse(nlcd.19.decforest[] %in% c(141), 1,0)

#plot(nlcd.19)
#plot(nlcd.19.decforest)

beetles.19$DeciduousForest <- unlist(lapply(raster::extract(nlcd.18.decforest, 
                                                            points_reprojected19,
                                                            buffer=15000),
                                            mean))

## Grass pasture
#nlcd.19.grass <- nlcd.19
#nlcd.19.grass[] <- ifelse(nlcd.19.grass[] %in% c(62,176), 1,0)

#plot(nlcd.19)
#plot(nlcd.19.grass)

beetles.19$GrassPasture <- unlist(lapply(raster::extract(nlcd.18.grass, 
                                                         points_reprojected19,
                                                         buffer=15000),
                                         mean))


## Avg Temperature
avgtemp.19 <- raster("climate/2019/PRISM_tmean_stable_4kmM3_2019_bil/PRISM_tmean_stable_4kmM3_2019_bil.bil")

plot(avgtemp.19)

st_crs(nlcd.19) == st_crs(avgtemp.19)

avgtemp.19 <- projectRaster(avgtemp.19, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.19) == st_crs(avgtemp.19)

plot(avgtemp.19)
points(points_reprojected19)

avgtemp.19 <- crop(avgtemp.19, nlcd.19)

plot(avgtemp.19)
points(points_reprojected19)

beetles.19$AvgTemp<- unlist(lapply(raster::extract(avgtemp.19, 
                                                   points_reprojected19,
                                                   buffer=15000),
                                   mean))



## Min Temperature
mintemp.19 <- raster("climate/2019/PRISM_tmin_stable_4kmM3_2019_bil/PRISM_tmin_stable_4kmM3_2019_bil.bil")

plot(mintemp.19)

st_crs(nlcd.19) == st_crs(mintemp.19)

mintemp.19 <- projectRaster(mintemp.19, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.19) == st_crs(mintemp.19)

plot(mintemp.19)
points(points_reprojected19)

mintemp.19 <- crop(mintemp.19, nlcd.19)

plot(mintemp.19)
points(points_reprojected19)

beetles.19$MinTemp<- unlist(lapply(raster::extract(mintemp.19, 
                                                   points_reprojected19,
                                                   buffer=15000),
                                   mean))


## Max Temperature
maxtemp.19 <- raster("climate/2019/PRISM_tmax_stable_4kmM3_2019_bil/PRISM_tmax_stable_4kmM3_2019_bil.bil")

plot(maxtemp.19)

st_crs(nlcd.19) == st_crs(maxtemp.19)

maxtemp.19 <- projectRaster(maxtemp.19, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.19) == st_crs(maxtemp.19)

plot(maxtemp.19)
points(points_reprojected19)

maxtemp.19 <- crop(maxtemp.19, nlcd.19)

plot(maxtemp.19)
points(points_reprojected19)

beetles.19$MaxTemp<- unlist(lapply(raster::extract(maxtemp.19, 
                                                   points_reprojected19,
                                                   buffer=15000),
                                   mean))


## Precipitation
precip.19 <- raster("climate/2019/PRISM_ppt_stable_4kmM3_2019_bil/PRISM_ppt_stable_4kmM3_2019_bil.bil")

plot(precip.19)

st_crs(nlcd.19) == st_crs(precip.19)

precip.19 <- projectRaster(precip.19, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.19) == st_crs(precip.19)

plot(precip.19)
points(points_reprojected19)

precip.19 <- crop(precip.19, nlcd.19)

plot(precip.19)
points(points_reprojected19)

beetles.19$Precipitation <- unlist(lapply(raster::extract(precip.19, 
                                                          points_reprojected19,
                                                          buffer=15000),
                                          mean))


### MODEL

## setting boundary
t.nlcd <- rast(nlcd.19)
plot(t.nlcd)
#t.nlcd[t.nlcd == 0] <- NA
#plot(t.nlcd)
#bd <- as.polygons(t.nlcd > 0)
#bd <- as(bd, 'Spatial') 
#plot(bd)


quad.19 <- spsample(bd, n = 20, type = "random")
plot(quad.17)


plot(bd)
#plot(quad.19, add = T)
plot(beetles.19$geometry, pch = 3, col = "red", add = T)


## getting vars for quad ----
# Make SpatialPoints data frame
df.quad.19 <- data.frame(long = coordinates(quad.19)[,1],lat = coordinates(quad.19)[,2])
quad.beetles19 <- data.frame(long = coordinates(quad.19)[,1],lat = coordinates(quad.19)[,2])
coordinates(quad.beetles19) =~ long + lat
proj4string(quad.beetles19) <- CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")


df.quad.19$CornAndSoy <- unlist(lapply(raster::extract(nlcd.18.corn, 
                                                       quad.beetles19,
                                                       buffer=15000),
                                       mean))

df.quad.19$DevelopedTotal <- unlist(lapply(raster::extract(nlcd.18.dev, 
                                                           quad.beetles19,
                                                           buffer=15000),
                                           mean))

df.quad.19$DeciduousForest <- unlist(lapply(raster::extract(nlcd.18.decforest, 
                                                            quad.beetles19,
                                                            buffer=15000),
                                            mean))

df.quad.19$GrassPasture <- unlist(lapply(raster::extract(nlcd.18.grass, 
                                                         quad.beetles19,
                                                         buffer=15000),
                                         mean))

df.quad.19$AvgTemp<- unlist(lapply(raster::extract(avgtemp.18, 
                                                   quad.beetles19,
                                                   buffer=15000),
                                   mean))

df.quad.19$MinTemp<- unlist(lapply(raster::extract(mintemp.18, 
                                                   quad.beetles19,
                                                   buffer=15000),
                                   mean))

df.quad.19$MaxTemp<- unlist(lapply(raster::extract(maxtemp.18, 
                                                   quad.beetles19,
                                                   buffer=15000),
                                   mean))

df.quad.19$Precipitation <- unlist(lapply(raster::extract(precip.18, 
                                                          quad.beetles19,
                                                          buffer=15000),
                                          mean))


df.quad.19$presence <- rep(0, length(df.quad.19$CornAndSoy))
beetles.19$presence <- rep(1, length(beetles.19$CornAndSoy))

df.quad.19t <- st_as_sf(df.quad.19, coords = c("long", "lat"))

st_crs(df.quad.19t) <- st_crs(beetles.19)

df.quad.19t <- st_transform(df.quad.19t, crs = st_crs(beetles.19))

df.quad.19t$long <- st_coordinates(df.quad.19t$geometry)[,1]
df.quad.19t$lat <- st_coordinates(df.quad.19t$geometry)[,2]

beetles.19$long <- st_coordinates(beetles.19$geometry)[,1]
beetles.19$lat <- st_coordinates(beetles.19$geometry)[,2]

beetles_19 <- beetles.19[,25:35]
beetles_19 <- rbind(beetles_19, df.quad.19t)


#######################
## FINALLY MODELING! ##
#######################

wt <- 1e-5
beetles_19$wt <- wt

f1 <- presence/wt ~ CornAndSoy + DevelopedTotal + DeciduousForest +
  GrassPasture + AvgTemp + MinTemp + MaxTemp + Precipitation

glmmod<- glm(f1, data = beetles_17, family = poisson, weights = wt)

summary(glmmod)

# GAMS with Backward Elimination
library(mgcv)

f2 <- update(f1, ~ . + s(long,lat, bs = "gp"))

ipp <- gam(f2, data = beetles_19, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f3 <- update(f2, ~ . + s(long,lat, bs = "gp") - Precipitation)

ipp <- gam(f3, data = beetles_19, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f4 <- update(f3, ~ . + s(long,lat, bs = "gp") - MinTemp)

ipp <- gam(f4, data = beetles_19, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f5 <- update(f4, ~ . + s(long,lat, bs = "gp") - MaxTemp)

ipp <- gam(f5, data = beetles_19, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f6 <- update(f5, ~ . + s(long,lat, bs = "gp") - AvgTemp)

ipp <- gam(f6, data = beetles_19, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f7 <- update(f6, ~ . + s(long,lat, bs = "gp") - GrassPasture)

ipp.19 <- gam(f7, data = beetles_19, family = poisson,
              weights = wt, method = "REML")

summary(ipp.19)


#################
## PREDICTIONS ##
#################

rl.E.y.19 <- raster(,nrow=65,ncols=65,ext=extent(bd),crs=crs(bd))
df.pred.19 <- data.frame(long = xyFromCell(rl.E.y.19,cell=1:length(rl.E.y.19[]))[,1],
                         lat = xyFromCell(rl.E.y.19,cell=1:length(rl.E.y.19[]))[,2])

pred.CornAndSoy <- unlist(lapply(raster::extract(nlcd.18.corn, 
                                                 df.pred.19,
                                                 buffer=15000),
                                 mean))

pred.DevelopedTotal <- unlist(lapply(raster::extract(nlcd.18.dev, 
                                                     df.pred.19,
                                                     buffer=15000),
                                     mean))

pred.DeciduousForest <- unlist(lapply(raster::extract(nlcd.18.decforest, 
                                                      df.pred.19,
                                                      buffer=15000),
                                      mean))

pred.GrassPasture <- unlist(lapply(raster::extract(nlcd.18.grass, 
                                                   df.pred.19,
                                                   buffer=15000),
                                   mean))

pred.AvgTemp<- unlist(lapply(raster::extract(avgtemp.18, 
                                             df.pred.19,
                                             buffer=15000),
                             mean))

pred.MinTemp<- unlist(lapply(raster::extract(mintemp.18, 
                                             df.pred.19,
                                             buffer=15000),
                             mean))

pred.MaxTemp<- unlist(lapply(raster::extract(maxtemp.18, 
                                             df.pred.19,
                                             buffer=15000),
                             mean))

pred.Precipitation <- unlist(lapply(raster::extract(precip.18, 
                                                    df.pred.19,
                                                    buffer=15000),
                                    mean))

df.pred.19$CornAndSoy <- pred.CornAndSoy
df.pred.19$DevelopedTotal <- pred.DevelopedTotal
df.pred.19$DeciduousForest <- pred.DeciduousForest
df.pred.19$GrassPasture <- pred.GrassPasture
df.pred.19$AvgTemp <- pred.AvgTemp
df.pred.19$MinTemp <- pred.MinTemp
df.pred.19$pred.MaxTemp <- pred.MaxTemp
df.pred.19$Precipitation <- pred.Precipitation



rl.E.y.19[] <- exp(c(predict(ipp.19,newdata=df.pred.19,type="link")))

#image(rl.E.y,axes=FALSE,box=FALSE)
plot(rl.E.y.17,axes=FALSE,box=FALSE)
plot(bd,add=TRUE)
points(points_reprojected19, pch = 16, col = "red")

plot(rl.E.y.18,axes=FALSE,box=FALSE)
plot(bd,add=TRUE)
points(points_reprojected19, pch = 16, col = "red")

plot(rl.E.y.19,axes=FALSE,box=FALSE)
plot(bd,add=TRUE)
points(points_reprojected19, pch = 16, col = "red")




##################################################################################################################


## For 2020

nlcd.20 <- mosaic(ks.nlcd.20, mo.nlcd.20, ne.nlcd.20, ia.nlcd.20, fun = sum)
plot(nlcd.20)
plot(st_coordinates(data2020s))

#st_crs(nlcd.20) == st_crs(data2020s)

points_reprojected20 <- st_transform(data2020s, crs = st_crs(nlcd.20))

#st_crs(nlcd.20) == st_crs(points_reprojected20)

#plot(nlcd.20)
#points(points_reprojected20)

## Corn and Soy
#nlcd.20.corn <- nlcd.20
#nlcd.20.corn[] <- ifelse(nlcd.20.corn[] %in% c(1,12,13,225,226,228,237,241,
#                                               5,26,239,240,241,254), 1,0)

#plot(nlcd.20)
#plot(nlcd.20.corn)

beetles.20 <- points_reprojected20

beetles.20$CornAndSoy <- unlist(lapply(raster::extract(nlcd.18.corn, 
                                                       points_reprojected20,
                                                       buffer=15000),
                                       mean))

## Developed total
#nlcd.20.dev <- nlcd.20
#nlcd.20.dev[] <- ifelse(nlcd.20.dev[] %in% c(82,121,122,123,124), 1,0)

#plot(nlcd.20)
#plot(nlcd.20.dev)

beetles.20$DevelopedTotal <- unlist(lapply(raster::extract(nlcd.18.dev, 
                                                           points_reprojected20,
                                                           buffer=15000),
                                           mean))

## Deciduous Forest
#nlcd.20.decforest <- nlcd.20
#nlcd.20.decforest[] <- ifelse(nlcd.20.decforest[] %in% c(141), 1,0)

#plot(nlcd.20)
#plot(nlcd.20.decforest)

beetles.20$DeciduousForest <- unlist(lapply(raster::extract(nlcd.18.decforest, 
                                                            points_reprojected20,
                                                            buffer=15000),
                                            mean))

## Grass pasture
#nlcd.20.grass <- nlcd.20
#nlcd.20.grass[] <- ifelse(nlcd.20.grass[] %in% c(62,176), 1,0)

#plot(nlcd.20)
#plot(nlcd.20.grass)

beetles.20$GrassPasture <- unlist(lapply(raster::extract(nlcd.18.grass, 
                                                         points_reprojected20,
                                                         buffer=15000),
                                         mean))


## Avg Temperature
avgtemp.20 <- raster("climate/2020/PRISM_tmean_stable_4kmM3_2020_bil/PRISM_tmean_stable_4kmM3_2020_bil.bil")

plot(avgtemp.20)

st_crs(nlcd.20) == st_crs(avgtemp.20)

avgtemp.20 <- projectRaster(avgtemp.20, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.20) == st_crs(avgtemp.20)

plot(avgtemp.20)
points(points_reprojected20)

avgtemp.20 <- crop(avgtemp.20, nlcd.20)

plot(avgtemp.20)
points(points_reprojected20)

beetles.20$AvgTemp<- unlist(lapply(raster::extract(avgtemp.20, 
                                                   points_reprojected20,
                                                   buffer=15000),
                                   mean))



## Min Temperature
mintemp.20 <- raster("climate/2020/PRISM_tmin_stable_4kmM3_2020_bil/PRISM_tmin_stable_4kmM3_2020_bil.bil")

plot(mintemp.20)

st_crs(nlcd.20) == st_crs(mintemp.20)

mintemp.20 <- projectRaster(mintemp.20, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.20) == st_crs(mintemp.20)

plot(mintemp.20)
points(points_reprojected20)

mintemp.20 <- crop(mintemp.20, nlcd.20)

plot(mintemp.20)
points(points_reprojected20)

beetles.20$MinTemp<- unlist(lapply(raster::extract(mintemp.20, 
                                                   points_reprojected20,
                                                   buffer=15000),
                                   mean))


## Max Temperature
maxtemp.20 <- raster("climate/2020/PRISM_tmax_stable_4kmM3_2020_bil/PRISM_tmax_stable_4kmM3_2020_bil.bil")

plot(maxtemp.20)

st_crs(nlcd.20) == st_crs(maxtemp.20)

maxtemp.20 <- projectRaster(maxtemp.20, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.20) == st_crs(maxtemp.20)

plot(maxtemp.20)
points(points_reprojected20)

maxtemp.20 <- crop(maxtemp.20, nlcd.20)

plot(maxtemp.20)
points(points_reprojected20)

beetles.20$MaxTemp<- unlist(lapply(raster::extract(maxtemp.20, 
                                                   points_reprojected20,
                                                   buffer=15000),
                                   mean))


## Precipitation
precip.20 <- raster("climate/2020/PRISM_ppt_stable_4kmM3_2020_bil/PRISM_ppt_stable_4kmM3_2020_bil.bil")

plot(precip.20)

st_crs(nlcd.20) == st_crs(precip.20)

precip.20 <- projectRaster(precip.20, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.20) == st_crs(precip.20)

plot(precip.20)
points(points_reprojected20)

precip.20 <- crop(precip.20, nlcd.20)

plot(precip.20)
points(points_reprojected20)

beetles.20$Precipitation <- unlist(lapply(raster::extract(precip.20, 
                                                          points_reprojected20,
                                                          buffer=15000),
                                          mean))


### MODEL

## setting boundary
t.nlcd <- rast(nlcd.20)
plot(t.nlcd)
#t.nlcd[t.nlcd == 0] <- NA
#plot(t.nlcd)
#bd <- as.polygons(t.nlcd > 0)
#bd <- as(bd, 'Spatial') 
#plot(bd)


quad.20 <- spsample(bd, n = 20, type = "random")
plot(quad.20)


plot(bd)
#plot(quad.20, add = T)
plot(beetles.20$geometry, pch = 3, col = "red", add = T)


## getting vars for quad ----
# Make SpatialPoints data frame
df.quad.20 <- data.frame(long = coordinates(quad.20)[,1],lat = coordinates(quad.20)[,2])
quad.beetles20 <- data.frame(long = coordinates(quad.20)[,1],lat = coordinates(quad.20)[,2])
coordinates(quad.beetles20) =~ long + lat
proj4string(quad.beetles20) <- CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")


df.quad.20$CornAndSoy <- unlist(lapply(raster::extract(nlcd.18.corn, 
                                                       quad.beetles20,
                                                       buffer=15000),
                                       mean))

df.quad.20$DevelopedTotal <- unlist(lapply(raster::extract(nlcd.18.dev, 
                                                           quad.beetles20,
                                                           buffer=15000),
                                           mean))

df.quad.20$DeciduousForest <- unlist(lapply(raster::extract(nlcd.18.decforest, 
                                                            quad.beetles20,
                                                            buffer=15000),
                                            mean))

df.quad.20$GrassPasture <- unlist(lapply(raster::extract(nlcd.18.grass, 
                                                         quad.beetles20,
                                                         buffer=15000),
                                         mean))

df.quad.20$AvgTemp<- unlist(lapply(raster::extract(avgtemp.20, 
                                                   quad.beetles20,
                                                   buffer=15000),
                                   mean))

df.quad.20$MinTemp<- unlist(lapply(raster::extract(mintemp.20, 
                                                   quad.beetles20,
                                                   buffer=15000),
                                   mean))

df.quad.20$MaxTemp<- unlist(lapply(raster::extract(maxtemp.20, 
                                                   quad.beetles20,
                                                   buffer=15000),
                                   mean))

df.quad.20$Precipitation <- unlist(lapply(raster::extract(precip.20, 
                                                          quad.beetles20,
                                                          buffer=15000),
                                          mean))


df.quad.20$presence <- rep(0, length(df.quad.20$CornAndSoy))
beetles.20$presence <- rep(1, length(beetles.20$CornAndSoy))

df.quad.20t <- st_as_sf(df.quad.20, coords = c("long", "lat"))

st_crs(df.quad.20t) <- st_crs(beetles.20)

df.quad.20t <- st_transform(df.quad.20t, crs = st_crs(beetles.20))

df.quad.20t$long <- st_coordinates(df.quad.20t$geometry)[,1]
df.quad.20t$lat <- st_coordinates(df.quad.20t$geometry)[,2]

beetles.20$long <- st_coordinates(beetles.20$geometry)[,1]
beetles.20$lat <- st_coordinates(beetles.20$geometry)[,2]

beetles_20 <- beetles.20[,25:35]
beetles_20 <- rbind(beetles_20, df.quad.20t)


#######################
## FINALLY MODELING! ##
#######################

wt <- 1e-5
beetles_20$wt <- wt

f1 <- presence/wt ~ CornAndSoy + DevelopedTotal + DeciduousForest +
  GrassPasture + AvgTemp + MinTemp + MaxTemp + Precipitation

glmmod<- glm(f1, data = beetles_17, family = poisson, weights = wt)

summary(glmmod)

# GAMS with Backward Elimination
library(mgcv)

f2 <- update(f1, ~ . + s(long,lat, bs = "gp"))

ipp <- gam(f2, data = beetles_20, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f3 <- update(f2, ~ . + s(long,lat, bs = "gp") - Precipitation)

ipp <- gam(f3, data = beetles_20, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f4 <- update(f3, ~ . + s(long,lat, bs = "gp") - MinTemp)

ipp <- gam(f4, data = beetles_20, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f5 <- update(f4, ~ . + s(long,lat, bs = "gp") - MaxTemp)

ipp <- gam(f5, data = beetles_20, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f6 <- update(f5, ~ . + s(long,lat, bs = "gp") - AvgTemp)

ipp <- gam(f6, data = beetles_20, family = poisson,
           weights = wt, method = "REML")

summary(ipp)


f7 <- update(f6, ~ . + s(long,lat, bs = "gp") - GrassPasture)

ipp.20 <- gam(f7, data = beetles_20, family = poisson,
              weights = wt, method = "REML")

summary(ipp.20)


#################
## PREDICTIONS ##
#################

rl.E.y.20 <- raster(,nrow=65,ncols=65,ext=extent(bd),crs=crs(bd))
df.pred.20 <- data.frame(long = xyFromCell(rl.E.y.20,cell=1:length(rl.E.y.20[]))[,1],
                         lat = xyFromCell(rl.E.y.20,cell=1:length(rl.E.y.20[]))[,2])

pred.CornAndSoy <- unlist(lapply(raster::extract(nlcd.18.corn, 
                                                 df.pred.20,
                                                 buffer=15000),
                                 mean))

pred.DevelopedTotal <- unlist(lapply(raster::extract(nlcd.18.dev, 
                                                     df.pred.20,
                                                     buffer=15000),
                                     mean))

pred.DeciduousForest <- unlist(lapply(raster::extract(nlcd.18.decforest, 
                                                      df.pred.20,
                                                      buffer=15000),
                                      mean))

pred.GrassPasture <- unlist(lapply(raster::extract(nlcd.18.grass, 
                                                   df.pred.20,
                                                   buffer=15000),
                                   mean))

pred.AvgTemp<- unlist(lapply(raster::extract(avgtemp.20, 
                                             df.pred.20,
                                             buffer=15000),
                             mean))

pred.MinTemp<- unlist(lapply(raster::extract(mintemp.20, 
                                             df.pred.20,
                                             buffer=15000),
                             mean))

pred.MaxTemp<- unlist(lapply(raster::extract(maxtemp.20, 
                                             df.pred.20,
                                             buffer=15000),
                             mean))

pred.Precipitation <- unlist(lapply(raster::extract(precip.20, 
                                                    df.pred.20,
                                                    buffer=15000),
                                    mean))

df.pred.20$CornAndSoy <- pred.CornAndSoy
df.pred.20$DevelopedTotal <- pred.DevelopedTotal
df.pred.20$DeciduousForest <- pred.DeciduousForest
df.pred.20$GrassPasture <- pred.GrassPasture
df.pred.20$AvgTemp <- pred.AvgTemp
df.pred.20$MinTemp <- pred.MinTemp
df.pred.20$pred.MaxTemp <- pred.MaxTemp
df.pred.20$Precipitation <- pred.Precipitation



rl.E.y.20[] <- exp(c(predict(ipp.20,newdata=df.pred.20,type="link")))

#image(rl.E.y,axes=FALSE,box=FALSE)
plot(rl.E.y.20,axes=FALSE,box=FALSE)
plot(bd,add=TRUE)
points(points_reprojected20, pch = 16, col = "red")

plot(rl.E.y.17,axes=FALSE,box=FALSE)
plot(bd,add=TRUE)
points(points_reprojected20, pch = 16, col = "red")

plot(rl.E.y.18,axes=FALSE,box=FALSE)
plot(bd,add=TRUE)
points(points_reprojected20, pch = 16, col = "red")

plot(rl.E.y.19,axes=FALSE,box=FALSE)
plot(bd,add=TRUE)
points(points_reprojected20, pch = 16, col = "red")

plot(rl.E.y.20,axes=FALSE,box=FALSE)
plot(bd,add=TRUE)
points(points_reprojected20, pch = 16, col = "red")


################################################################################################################################



## For 2021

nlcd.21 <- mosaic(ks.nlcd.21, mo.nlcd.21, ne.nlcd.21, ia.nlcd.21, fun = sum)
plot(nlcd.21)
plot(st_coordinates(data2021s))

st_crs(nlcd.21) == st_crs(data2021s)

points_reprojected21 <- st_transform(data2021s, crs = st_crs(nlcd.21))

st_crs(nlcd.21) == st_crs(points_reprojected21)

plot(nlcd.21)
points(points_reprojected21)

## Corn and Soy
#nlcd.21.corn <- nlcd.21
#nlcd.21.corn[] <- ifelse(nlcd.21.corn[] %in% c(1,12,13,225,226,228,237,241,
#                                               5,26,239,240,241,254), 1,0)

#plot(nlcd.21)
#plot(nlcd.21.corn)

beetles.21 <- points_reprojected21

beetles.21$CornAndSoy <- unlist(lapply(raster::extract(nlcd.18.corn, 
                                                       points_reprojected21,
                                                       buffer=15000),
                                       mean))

## Developed total
#nlcd.21.dev <- nlcd.21
#nlcd.21.dev[] <- ifelse(nlcd.21.dev[] %in% c(82,121,122,123,124), 1,0)

#plot(nlcd.21)
#plot(nlcd.21.dev)

beetles.21$DevelopedTotal <- unlist(lapply(raster::extract(nlcd.18.dev, 
                                                           points_reprojected21,
                                                           buffer=15000),
                                           mean))

## Deciduous Forest
#nlcd.21.decforest <- nlcd.21
#nlcd.21.decforest[] <- ifelse(nlcd.21.decforest[] %in% c(141), 1,0)

#plot(nlcd.21)
#plot(nlcd.21.decforest)

beetles.21$DeciduousForest <- unlist(lapply(raster::extract(nlcd.18.decforest, 
                                                            points_reprojected21,
                                                            buffer=15000),
                                            mean))

## Grass pasture
#nlcd.21.grass <- nlcd.21
#nlcd.21.grass[] <- ifelse(nlcd.21.grass[] %in% c(62,176), 1,0)

#plot(nlcd.21)
#plot(nlcd.21.grass)

beetles.21$GrassPasture <- unlist(lapply(raster::extract(nlcd.18.grass, 
                                                         points_reprojected21,
                                                         buffer=15000),
                                         mean))


## Avg Temperature
avgtemp.21 <- raster("climate/2021/PRISM_tmean_stable_4kmM3_2021_bil/PRISM_tmean_stable_4kmM3_2021_bil.bil")

plot(avgtemp.21)

st_crs(nlcd.21) == st_crs(avgtemp.21)

avgtemp.21 <- projectRaster(avgtemp.21, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.21) == st_crs(avgtemp.21)

plot(avgtemp.21)
points(points_reprojected21)

avgtemp.21 <- crop(avgtemp.21, nlcd.21)

plot(avgtemp.21)
points(points_reprojected17)

beetles.21$AvgTemp<- unlist(lapply(raster::extract(avgtemp.21, 
                                                   points_reprojected21,
                                                   buffer=15000),
                                   mean))



## Min Temperature
mintemp.21 <- raster("climate/2021/PRISM_tmin_stable_4kmM3_2021_bil/PRISM_tmin_stable_4kmM3_2021_bil.bil")

plot(mintemp.21)

st_crs(nlcd.21) == st_crs(mintemp.21)

mintemp.21 <- projectRaster(mintemp.21, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.21) == st_crs(mintemp.21)

plot(mintemp.21)
points(points_reprojected21)

mintemp.21 <- crop(mintemp.21, nlcd.21)

plot(mintemp.21)
points(points_reprojected21)

beetles.21$MinTemp<- unlist(lapply(raster::extract(mintemp.21, 
                                                   points_reprojected21,
                                                   buffer=15000),
                                   mean))


## Max Temperature
maxtemp.21 <- raster("climate/2021/PRISM_tmax_stable_4kmM3_2021_bil/PRISM_tmax_stable_4kmM3_2021_bil.bil")

plot(maxtemp.21)

st_crs(nlcd.21) == st_crs(maxtemp.21)

maxtemp.21 <- projectRaster(maxtemp.21, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.21) == st_crs(maxtemp.21)

plot(maxtemp.21)
points(points_reprojected21)

maxtemp.21 <- crop(maxtemp.21, nlcd.21)

plot(maxtemp.21)
points(points_reprojected21)

beetles.21$MaxTemp<- unlist(lapply(raster::extract(maxtemp.21, 
                                                   points_reprojected21,
                                                   buffer=15000),
                                   mean))


## Precipitation
precip.21 <- raster("climate/2021/PRISM_ppt_stable_4kmM3_2021_bil/PRISM_ppt_stable_4kmM3_2021_bil.bil")

plot(precip.21)

st_crs(nlcd.21) == st_crs(precip.21)

precip.21 <- projectRaster(precip.21, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.21) == st_crs(precip.21)

plot(precip.21)
points(points_reprojected21)

precip.21 <- crop(precip.21, nlcd.21)

plot(precip.21)
points(points_reprojected21)

beetles.21$Precipitation <- unlist(lapply(raster::extract(precip.21, 
                                                          points_reprojected21,
                                                          buffer=15000),
                                          mean))


### MODEL

## setting boundary
t.nlcd <- rast(nlcd.21)
plot(t.nlcd)
#t.nlcd[t.nlcd == 0] <- NA
#plot(t.nlcd)
#bd <- as.polygons(t.nlcd > 0)
#bd <- as(bd, 'Spatial') 
#plot(bd)


quad.21 <- spsample(bd, n = 20, type = "random")
plot(quad.21)


plot(bd)
#plot(quad.17, add = T)
plot(beetles.21$geometry, pch = 3, col = "red", add = T)


## getting vars for quad ----
# Make SpatialPoints data frame
df.quad.21 <- data.frame(long = coordinates(quad.21)[,1],lat = coordinates(quad.21)[,2])
quad.beetles21 <- data.frame(long = coordinates(quad.21)[,1],lat = coordinates(quad.21)[,2])
coordinates(quad.beetles21) =~ long + lat
proj4string(quad.beetles21) <- CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")


df.quad.21$CornAndSoy <- unlist(lapply(raster::extract(nlcd.18.corn, 
                                                       quad.beetles21,
                                                       buffer=15000),
                                       mean))

df.quad.21$DevelopedTotal <- unlist(lapply(raster::extract(nlcd.18.dev, 
                                                           quad.beetles21,
                                                           buffer=15000),
                                           mean))

df.quad.21$DeciduousForest <- unlist(lapply(raster::extract(nlcd.18.decforest, 
                                                            quad.beetles21,
                                                            buffer=15000),
                                            mean))

df.quad.21$GrassPasture <- unlist(lapply(raster::extract(nlcd.18.grass, 
                                                         quad.beetles21,
                                                         buffer=15000),
                                         mean))

df.quad.21$AvgTemp<- unlist(lapply(raster::extract(avgtemp.21, 
                                                   quad.beetles21,
                                                   buffer=15000),
                                   mean))

df.quad.21$MinTemp<- unlist(lapply(raster::extract(mintemp.21, 
                                                   quad.beetles21,
                                                   buffer=15000),
                                   mean))

df.quad.21$MaxTemp<- unlist(lapply(raster::extract(maxtemp.21, 
                                                   quad.beetles21,
                                                   buffer=15000),
                                   mean))

df.quad.21$Precipitation <- unlist(lapply(raster::extract(precip.21, 
                                                          quad.beetles21,
                                                          buffer=15000),
                                          mean))


df.quad.21$presence <- rep(0, length(df.quad.21$CornAndSoy))
beetles.21$presence <- rep(1, length(beetles.21$CornAndSoy))

df.quad.21t <- st_as_sf(df.quad.21, coords = c("long", "lat"))

st_crs(df.quad.21t) <- st_crs(beetles.21)

df.quad.21t <- st_transform(df.quad.21t, crs = st_crs(beetles.21))

df.quad.21t$long <- st_coordinates(df.quad.21t$geometry)[,1]
df.quad.21t$lat <- st_coordinates(df.quad.21t$geometry)[,2]

beetles.21$long <- st_coordinates(beetles.21$geometry)[,1]
beetles.21$lat <- st_coordinates(beetles.21$geometry)[,2]

beetles_21 <- beetles.21[,25:35]
beetles_21 <- rbind(beetles_21, df.quad.21t)


#######################
## FINALLY MODELING! ##
#######################

wt <- 1e-5
beetles_21$wt <- wt

f1 <- presence/wt ~ CornAndSoy + DevelopedTotal + DeciduousForest +
  GrassPasture + AvgTemp + MinTemp + MaxTemp + Precipitation

glmmod<- glm(f1, data = beetles_21, family = poisson, weights = wt)

summary(glmmod)

# GAMS with Backward Elimination
library(mgcv)

f1 <- presence/wt ~ s(CornAndSoy) + s(DevelopedTotal) + s(DeciduousForest) +
  s(GrassPasture) + s(AvgTemp) + s(MinTemp) + s(MaxTemp) + s(Precipitation)

f2 <- update(f1, ~ . + s(long,lat, bs = "gp"))

ipp <- gam(f2, data = beetles_21, family = poisson,
           weights = wt, method = "REML", control = gam.control(maxit = 2000))

summary(ipp)
AIC(ipp) # 49.9535

f3 <- update(f2, ~ . + s(long,lat, bs = "gp") - s(AvgTemp))

ipp <- gam(f3, data = beetles_21, family = poisson,
           weights = wt, method = "REML", control = gam.control(maxit = 2000))

summary(ipp)
AIC(ipp) # 47.95349

f4 <- update(f3, ~ . + s(long,lat, bs = "gp") - s(Precipitation))

ipp <- gam(f4, data = beetles_21, family = poisson,
           weights = wt, method = "REML", control = gam.control(maxit = 2000))

summary(ipp)
AIC(ipp) # 45.95463

f5 <- update(f4, ~ . - s(long,lat, bs = "gp"))

ipp <- gam(f5, data = beetles_21, family = poisson,
           weights = wt, method = "REML")

summary(ipp)
AIC(ipp) # 42.14726


f6 <- update(f5, ~ . - s(MaxTemp))

ipp <- gam(f6, data = beetles_21, family = poisson,
           weights = wt, method = "REML")

summary(ipp)
AIC(ipp) # 42.5445

f7 <- update(f6, ~ . - s(GrassPasture))

ipp.21 <- gam(f7, data = beetles_21, family = poisson,
              weights = wt, method = "REML")

summary(ipp.21)
AIC(ipp.21) # 42.0845

#################
## PREDICTIONS ##
#################

rl.E.y.21 <- raster(,nrow=65,ncols=65,ext=extent(bd),crs=crs(bd))
df.pred.21 <- data.frame(long = xyFromCell(rl.E.y.21,cell=1:length(rl.E.y.21[]))[,1],
                         lat = xyFromCell(rl.E.y.21,cell=1:length(rl.E.y.21[]))[,2])

pred.CornAndSoy <- unlist(lapply(raster::extract(nlcd.18.corn, 
                                                 df.pred.21,
                                                 buffer=15000),
                                 mean))

pred.DevelopedTotal <- unlist(lapply(raster::extract(nlcd.18.dev, 
                                                     df.pred.21,
                                                     buffer=15000),
                                     mean))

pred.DeciduousForest <- unlist(lapply(raster::extract(nlcd.18.decforest, 
                                                      df.pred.21,
                                                      buffer=15000),
                                      mean))

pred.GrassPasture <- unlist(lapply(raster::extract(nlcd.18.grass, 
                                                   df.pred.21,
                                                   buffer=15000),
                                   mean))

pred.AvgTemp<- unlist(lapply(raster::extract(avgtemp.21, 
                                             df.pred.21,
                                             buffer=15000),
                             mean))

pred.MinTemp<- unlist(lapply(raster::extract(mintemp.21, 
                                             df.pred.21,
                                             buffer=15000),
                             mean))

pred.MaxTemp<- unlist(lapply(raster::extract(maxtemp.21, 
                                             df.pred.21,
                                             buffer=15000),
                             mean))

pred.Precipitation <- unlist(lapply(raster::extract(precip.21, 
                                                    df.pred.21,
                                                    buffer=15000),
                                    mean))

df.pred.21$CornAndSoy <- pred.CornAndSoy
df.pred.21$DevelopedTotal <- pred.DevelopedTotal
df.pred.21$DeciduousForest <- pred.DeciduousForest
df.pred.21$GrassPasture <- pred.GrassPasture
df.pred.21$AvgTemp <- pred.AvgTemp
df.pred.21$MinTemp <- pred.MinTemp
df.pred.21$pred.MaxTemp <- pred.MaxTemp
df.pred.21$Precipitation <- pred.Precipitation


rl.E.y.21[] <- exp(c(predict(ipp.21,newdata=df.pred.21,type="link")))

#image(rl.E.y,axes=FALSE,box=FALSE)
plot(rl.E.y.21,axes=FALSE,box=FALSE)
plot(bd,add=TRUE)
points(points_reprojected21, pch = 16, col = "red")

par(mfrow = c(2,3))

plot(rl.E.y.17,axes=FALSE,box=FALSE, main = "2017")
plot(bd,add=TRUE)
points(points_reprojected17, pch = 16, col = "red")

plot(rl.E.y.18,axes=FALSE,box=FALSE, main = "2018")
plot(bd,add=TRUE)
points(points_reprojected18, pch = 16, col = "red")

plot(rl.E.y.19,axes=FALSE,box=FALSE, main = "2019")
plot(bd,add=TRUE)
points(points_reprojected19, pch = 16, col = "red")

plot(rl.E.y.20,axes=FALSE,box=FALSE, main = "2020")
plot(bd,add=TRUE)
points(points_reprojected20, pch = 16, col = "red")

plot(rl.E.y.21,axes=FALSE,box=FALSE, main = "2021")
plot(bd,add=TRUE)
points(points_reprojected21, pch = 16, col = "red")




# REAL DEAL ----

nlcd.21 <- mosaic(ks.nlcd.21, mo.nlcd.21, ne.nlcd.21, ia.nlcd.21, fun = sum)
plot(nlcd.21)
plot(st_coordinates(data))

st_crs(nlcd.21) == st_crs(data)

points_reprojected <- st_transform(data, crs = st_crs(nlcd.21))

st_crs(nlcd.21) == st_crs(points_reprojected)

plot(nlcd.21)
points(points_reprojected)

## Corn and Soy
#nlcd.21.corn <- nlcd.21
#nlcd.21.corn[] <- ifelse(nlcd.21.corn[] %in% c(1,12,13,225,226,228,237,241,
#                                               5,26,239,240,241,254), 1,0)

#plot(nlcd.21)
#plot(nlcd.21.corn)

beetles <- points_reprojected

beetles$CornAndSoy <- unlist(lapply(raster::extract(nlcd.18.corn, 
                                                       points_reprojected,
                                                       buffer=15000),
                                       mean))

## Developed total
#nlcd.21.dev <- nlcd.21
#nlcd.21.dev[] <- ifelse(nlcd.21.dev[] %in% c(82,121,122,123,124), 1,0)

#plot(nlcd.21)
#plot(nlcd.21.dev)

beetles$DevelopedTotal <- unlist(lapply(raster::extract(nlcd.18.dev, 
                                                           points_reprojected,
                                                           buffer=15000),
                                           mean))

## Deciduous Forest
#nlcd.21.decforest <- nlcd.21
#nlcd.21.decforest[] <- ifelse(nlcd.21.decforest[] %in% c(141), 1,0)

#plot(nlcd.21)
#plot(nlcd.21.decforest)

beetles$DeciduousForest <- unlist(lapply(raster::extract(nlcd.18.decforest, 
                                                            points_reprojected,
                                                            buffer=15000),
                                            mean))

## Grass pasture
#nlcd.21.grass <- nlcd.21
#nlcd.21.grass[] <- ifelse(nlcd.21.grass[] %in% c(62,176), 1,0)

#plot(nlcd.21)
#plot(nlcd.21.grass)

beetles$GrassPasture <- unlist(lapply(raster::extract(nlcd.18.grass, 
                                                         points_reprojected,
                                                         buffer=15000),
                                         mean))


## Avg Temperature
avgtemp.21 <- raster("climate/2021/PRISM_tmean_stable_4kmM3_2021_bil/PRISM_tmean_stable_4kmM3_2021_bil.bil")

plot(avgtemp.21)

st_crs(nlcd.21) == st_crs(avgtemp.21)

avgtemp.21 <- projectRaster(avgtemp.21, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.21) == st_crs(avgtemp.21)

plot(avgtemp.21)
points(points_reprojected)

avgtemp.21 <- crop(avgtemp.21, nlcd.21)

plot(avgtemp.21)
points(points_reprojected)

beetles$AvgTemp<- unlist(lapply(raster::extract(avgtemp.21, 
                                                   points_reprojected,
                                                   buffer=15000),
                                   mean))



## Min Temperature
mintemp.21 <- raster("climate/2021/PRISM_tmin_stable_4kmM3_2021_bil/PRISM_tmin_stable_4kmM3_2021_bil.bil")

plot(mintemp.21)

st_crs(nlcd.21) == st_crs(mintemp.21)

mintemp.21 <- projectRaster(mintemp.21, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.21) == st_crs(mintemp.21)

plot(mintemp.21)
points(points_reprojected)

mintemp.21 <- crop(mintemp.21, nlcd.21)

plot(mintemp.21)
points(points_reprojected)

beetles$MinTemp<- unlist(lapply(raster::extract(mintemp.21, 
                                                   points_reprojected,
                                                   buffer=15000),
                                   mean))


## Max Temperature
maxtemp.21 <- raster("climate/2021/PRISM_tmax_stable_4kmM3_2021_bil/PRISM_tmax_stable_4kmM3_2021_bil.bil")

plot(maxtemp.21)

st_crs(nlcd.21) == st_crs(maxtemp.21)

maxtemp.21 <- projectRaster(maxtemp.21, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.21) == st_crs(maxtemp.21)

plot(maxtemp.21)
points(points_reprojected)

maxtemp.21 <- crop(maxtemp.21, nlcd.21)

plot(maxtemp.21)
points(points_reprojected)

beetles$MaxTemp<- unlist(lapply(raster::extract(maxtemp.21, 
                                                   points_reprojected,
                                                   buffer=15000),
                                   mean))


## Precipitation
precip.21 <- raster("climate/2021/PRISM_ppt_stable_4kmM3_2021_bil/PRISM_ppt_stable_4kmM3_2021_bil.bil")

plot(precip.21)

st_crs(nlcd.21) == st_crs(precip.21)

precip.21 <- projectRaster(precip.21, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

st_crs(nlcd.21) == st_crs(precip.21)

plot(precip.21)
points(points_reprojected)

precip.21 <- crop(precip.21, nlcd.21)

plot(precip.21)
points(points_reprojected)

beetles$Precipitation <- unlist(lapply(raster::extract(precip.21, 
                                                          points_reprojected,
                                                          buffer=15000),
                                          mean))


### MODEL

## setting boundary
t.nlcd <- rast(nlcd.21)
plot(t.nlcd)
#t.nlcd[t.nlcd == 0] <- NA
#plot(t.nlcd)
bd <- as.polygons(t.nlcd > 0)
bd <- as(bd, 'Spatial') 
plot(bd)


quad <- spsample(bd, n = 20, type = "random")
plot(quad)


plot(bd)
#plot(quad.17, add = T)
plot(beetles$geometry, pch = 3, col = "red", add = T)


## getting vars for quad ----
# Make SpatialPoints data frame
df.quad <- data.frame(long = coordinates(quad)[,1],lat = coordinates(quad)[,2])
quad.beetles <- data.frame(long = coordinates(quad)[,1],lat = coordinates(quad)[,2])
coordinates(quad.beetles) =~ long + lat
proj4string(quad.beetles) <- CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")


df.quad$CornAndSoy <- unlist(lapply(raster::extract(nlcd.18.corn, 
                                                       quad.beetles,
                                                       buffer=15000),
                                       mean))

df.quad$DevelopedTotal <- unlist(lapply(raster::extract(nlcd.18.dev, 
                                                           quad.beetles,
                                                           buffer=15000),
                                           mean))

df.quad$DeciduousForest <- unlist(lapply(raster::extract(nlcd.18.decforest, 
                                                            quad.beetles,
                                                            buffer=15000),
                                            mean))

df.quad$GrassPasture <- unlist(lapply(raster::extract(nlcd.18.grass, 
                                                         quad.beetles,
                                                         buffer=15000),
                                         mean))

df.quad$AvgTemp<- unlist(lapply(raster::extract(avgtemp.21, 
                                                   quad.beetles,
                                                   buffer=15000),
                                   mean))

df.quad$MinTemp<- unlist(lapply(raster::extract(mintemp.21, 
                                                   quad.beetles,
                                                   buffer=15000),
                                   mean))

df.quad$MaxTemp<- unlist(lapply(raster::extract(maxtemp.21, 
                                                   quad.beetles,
                                                   buffer=15000),
                                   mean))

df.quad$Precipitation <- unlist(lapply(raster::extract(precip.21, 
                                                          quad.beetles,
                                                          buffer=15000),
                                          mean))


df.quad$presence <- rep(0, length(df.quad$CornAndSoy))
beetles$presence <- rep(1, length(beetles$CornAndSoy))

df.quad$Year <- rdunif(length(df.quad$DevelopedTotal), 2017, 2021)

df.quad.t <- st_as_sf(df.quad, coords = c("long", "lat"))

st_crs(df.quad.t) <- st_crs(beetles)

df.quad.t <- st_transform(df.quad.t, crs = st_crs(beetles))

df.quad.t$long <- st_coordinates(df.quad.t$geometry)[,1]
df.quad.t$lat <- st_coordinates(df.quad.t$geometry)[,2]

beetles$long <- st_coordinates(beetles$geometry)[,1]
beetles$lat <- st_coordinates(beetles$geometry)[,2]

beetles_final <- beetles[,c(9,25:35)]
beetles_final <- rbind(beetles_final, df.quad.t)


beetles_final <- rbind(beetles_17, beetles_18, beetles_19, beetles_20, beetles_21)

Year17 <- rep(2017, length(beetles_17$CornAndSoy))
Year18 <- rep(2018, length(beetles_18$CornAndSoy))
Year19 <- rep(2019, length(beetles_19$CornAndSoy))
Year20 <- rep(2020, length(beetles_20$CornAndSoy))
Year21 <- rep(2021, length(beetles_21$CornAndSoy))

beetlesyear <- c(Year17,Year18,Year19,Year20,Year21)

beetles_final$Year <- beetlesyear

#######################
## FINALLY MODELING! ##
#######################


f1 <- presence/wt ~ CornAndSoy + DevelopedTotal + DeciduousForest +
  GrassPasture + AvgTemp + MinTemp + MaxTemp + Precipitation

glmmod<- glm(f1, data = beetles_final, family = poisson, weights = wt)

summary(glmmod)

# GAMS with Backward Elimination
library(mgcv)

f1 <- presence/wt ~ (CornAndSoy) + (DevelopedTotal) + (DeciduousForest) +
  (GrassPasture) + s(AvgTemp) + s(MinTemp) + s(MaxTemp) + s(Precipitation) + Year

f1 <- presence/wt ~ s(CornAndSoy) + 
  s(DevelopedTotal) + s(DeciduousForest) +
  s(GrassPasture) + s(AvgTemp) + s(MinTemp) + 
  s(MaxTemp) + s(Precipitation) +
  Year + s(long,lat)

f2 <- update(f1, ~ .)

ipp <- gam(f2, data = beetles_final, family = poisson,
           weights = wt, method = "REML", control = gam.control(maxit = 2000))

summary(ipp)
AIC(ipp) # 140.4131

f3 <- update(f2, ~ . - s(Precipitation))

ipp <- gam(f3, data = beetles_final, family = poisson,
           weights = wt, method = "REML", control = gam.control(maxit = 2000))

summary(ipp)
AIC(ipp) # 138.4438

f4 <- update(f3, ~ . - s(MinTemp))

ipp <- gam(f4, data = beetles_final, family = poisson,
           weights = wt, method = "REML", control = gam.control(maxit = 2000))

summary(ipp)
AIC(ipp) # 138.5858

f5 <- update(f4, ~ . - s(long,lat))

ipp <- gam(f5, data = beetles_final, family = poisson,
           weights = wt, method = "REML")

summary(ipp)
AIC(ipp) # 136.4969


f6 <- update(f5, ~ . - Year)

ipp <- gam(f6, data = beetles_final, family = poisson,
           weights = wt, method = "REML")

summary(ipp)
AIC(ipp) # 136.529


#################
## PREDICTIONS ##
#################

rl.E.y.final <- raster(,nrow=65,ncols=65,ext=extent(bd),crs=crs(bd))
df.pred.final <- data.frame(long = xyFromCell(rl.E.y.final,cell=1:length(rl.E.y.final[]))[,1],
                         lat = xyFromCell(rl.E.y.final,cell=1:length(rl.E.y.final[]))[,2])

pred.CornAndSoy <- unlist(lapply(raster::extract(nlcd.18.corn, 
                                                 df.pred.final,
                                                 buffer=15000),
                                 mean))

pred.DevelopedTotal <- unlist(lapply(raster::extract(nlcd.18.dev, 
                                                     df.pred.final,
                                                     buffer=15000),
                                     mean))

pred.DeciduousForest <- unlist(lapply(raster::extract(nlcd.18.decforest, 
                                                      df.pred.final,
                                                      buffer=15000),
                                      mean))

pred.GrassPasture <- unlist(lapply(raster::extract(nlcd.18.grass, 
                                                   df.pred.final,
                                                   buffer=15000),
                                   mean))

pred.AvgTemp<- unlist(lapply(raster::extract(avgtemp.21, 
                                             df.pred.final,
                                             buffer=15000),
                             mean))

pred.MinTemp<- unlist(lapply(raster::extract(mintemp.21, 
                                             df.pred.final,
                                             buffer=15000),
                             mean))

pred.MaxTemp<- unlist(lapply(raster::extract(maxtemp.21, 
                                             df.pred.final,
                                             buffer=15000),
                             mean))

pred.Precipitation <- unlist(lapply(raster::extract(precip.21, 
                                                    df.pred.final,
                                                    buffer=15000),
                                    mean))

df.pred.final$CornAndSoy <- pred.CornAndSoy
df.pred.final$DevelopedTotal <- pred.DevelopedTotal
df.pred.final$DeciduousForest <- pred.DeciduousForest
df.pred.final$GrassPasture <- pred.GrassPasture
df.pred.final$AvgTemp <- pred.AvgTemp
df.pred.final$MinTemp <- pred.MinTemp
df.pred.final$MaxTemp <- pred.MaxTemp
df.pred.final$Precipitation <- pred.Precipitation
df.pred.final$Year <- rep(2021, length(pred.Precipitation))

rl.E.y.final[] <- exp(c(predict(ipp,newdata=df.pred.final,type="link")))

#image(rl.E.y,axes=FALSE,box=FALSE)
plot(rl.E.y.final,axes=FALSE,box=FALSE)
plot(bd,add=TRUE)
points(points_reprojected21, pch = 16, col = "red")


# For predictions
# 2.5 degrees
df.pred.final$CornAndSoy <- pred.CornAndSoy
df.pred.final$DevelopedTotal <- pred.DevelopedTotal
df.pred.final$DeciduousForest <- pred.DeciduousForest
df.pred.final$GrassPasture <- pred.GrassPasture
df.pred.final$AvgTemp <- pred.AvgTemp
df.pred.final$MinTemp <- pred.MinTemp
df.pred.final$MaxTemp <- pred.MaxTemp
df.pred.final$Precipitation <- pred.Precipitation
df.pred.final$Year <- rep(2021, length(pred.Precipitation))

rl.E.y.final[] <- exp(c(predict(ipp,newdata=df.pred.final,type="link")))

#image(rl.E.y,axes=FALSE,box=FALSE)
plot(rl.E.y.final,axes=FALSE,box=FALSE)
plot(bd,add=TRUE)
points(points_reprojected, pch = 16, col = "red")


# 5 degrees
df.pred.final$CornAndSoy <- pred.CornAndSoy
df.pred.final$DevelopedTotal <- pred.DevelopedTotal
df.pred.final$DeciduousForest <- pred.DeciduousForest
df.pred.final$GrassPasture <- pred.GrassPasture
df.pred.final$AvgTemp <- (pred.AvgTemp-32*(5/9))+4
df.pred.final$MinTemp <- (pred.MinTemp-32*(5/9))+4
df.pred.final$MaxTemp <- (pred.MaxTemp-32*(5/9))+4
df.pred.final$Precipitation <- pred.Precipitation*0.9
df.pred.final$Year <- rep(2100, length(pred.Precipitation))

rl.E.y.final[] <- exp(c(predict(ipp,newdata=df.pred.final,type="link")))

#image(rl.E.y,axes=FALSE,box=FALSE)
plot(rl.E.y.final,axes=FALSE,box=FALSE)
plot(bd,add=TRUE)
points(points_reprojected, pch = 16, col = "red")


library(tigris)

ks <- counties("Kansas", cb = TRUE)
ks <- ks[,13]
ks <- st_transform(ks, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

mo <- counties("Missouri", cb = TRUE)
mo <- mo[,13]
mo <- st_transform(mo, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

ia <- counties("Iowa", cb = TRUE)
ia <- ia[,13]
ia <- st_transform(ia, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

ne <- counties("Nebraska", cb = TRUE)
ne <- ne[,13]
ne <- st_transform(ne, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")



plot(rl.E.y.final,axes=FALSE,box=FALSE)
plot(bd,add=TRUE)
plot(ks,add=TRUE)
plot(mo,add=TRUE)
plot(ia,add=TRUE)
plot(ne,add=TRUE)
points(points_reprojected, pch = 16, col = "red")



rl.E.y.final.df <- as.data.frame(rl.E.y.final, xy = TRUE)

ggplot()+
  geom_raster(data = rl.E.y.final.df, aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c(option = "turbo")+
  geom_sf(data = ks, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = mo, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = ne, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = ia, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = points_reprojected, color = "black", size = 0.5) +
  labs(x = "Longitude", y = "Latitude")+
  theme_minimal()
  
  
library(patchwork)
  
rl.E.y.17.df <- as.data.frame(rl.E.y.17, xy = TRUE)

a <- ggplot()+
  geom_raster(data = rl.E.y.17.df, aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c(option = "turbo")+
  geom_sf(data = ks, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = mo, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = ne, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = ia, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = points_reprojected17, color = "black", size = 0.5) +
  labs(x = "Longitude", y = "Latitude", title = "2017")+
  theme_minimal()
  

rl.E.y.18.df <- as.data.frame(rl.E.y.18, xy = TRUE)

b <- ggplot()+
  geom_raster(data = rl.E.y.18.df, aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c(option = "turbo")+
  geom_sf(data = ks, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = mo, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = ne, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = ia, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = points_reprojected18, color = "black", size = 0.5) +
  labs(x = "Longitude", y = "Latitude", title = "2018")+
  theme_minimal()


rl.E.y.19.df <- as.data.frame(rl.E.y.19, xy = TRUE)

c <- ggplot()+
  geom_raster(data = rl.E.y.19.df, aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c(option = "turbo")+
  geom_sf(data = ks, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = mo, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = ne, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = ia, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = points_reprojected19, color = "black", size = 0.5) +
  labs(x = "Longitude", y = "Latitude", title = "2019")+
  theme_minimal()


rl.E.y.20.df <- as.data.frame(rl.E.y.20, xy = TRUE)

d <- ggplot()+
  geom_raster(data = rl.E.y.21.df, aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c(option = "turbo")+
  geom_sf(data = ks, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = mo, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = ne, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = ia, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = points_reprojected20, color = "black", size = 0.5) +
  labs(x = "Longitude", y = "Latitude", title = "2020")+
  theme_minimal()


rl.E.y.21.df <- as.data.frame(rl.E.y.21, xy = TRUE)

e <- ggplot()+
  geom_raster(data = rl.E.y.20.df, aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c(option = "turbo")+
  geom_sf(data = ks, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = mo, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = ne, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = ia, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = points_reprojected21, color = "black", size = 0.5) +
  labs(x = "Longitude", y = "Latitude", title = "2021")+
  theme_minimal()

f <- ggplot()+
  geom_raster(data = rl.E.y.final.df, aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c(option = "turbo")+
  geom_sf(data = ks, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = mo, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = ne, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = ia, color = "black", fill = "white", size = 0.25, alpha = 0.02) +
  geom_sf(data = points_reprojected, color = "black", size = 0.5) +
  labs(x = "Longitude", y = "Latitude", title = "Cumulative")+
  theme_minimal()
  
a + b + c + d + e + f
  
end_time <- Sys.time()
elapsed_time <- end_time - start_time
print(elapsed_time)  
