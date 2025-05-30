##############################################################################
#
# This script executes the following:
# 1) Create a spatial layer from bounding boxes
# 2) Create a 1km & 5km buffer around each 'site' (i.e., bounding box)
# 3) Gather spatial data from Google Earth Engine
# 4) Calculate climate velocity 
#
#  Script was written by Michael T. Hallworth, Ph.D. 
#                        Conservation and Data Scientist
#                        Vermont Center for Ecostudies
#
##############################################################################

# Load required libraries 

library(sf)
library(terra)
library(rgee) 

# Read in the coordinates for the bounding boxes # 
bbox_data <- read.csv("Data/all_sites_bounding_boxes.csv")

# number of 'sites' #
nsites <- nrow(bbox_data)

###############################################################################
#
# 1) Create a spatial layer from the bounding box data 
#    Note - coordinates are in WGS84 - i.e., crs = 4326
###############################################################################

# Use the coordinates provided to create bounding box 

# take a look at the structure of the data 
str(bbox_data,1)

# for loop to create the boxes # 
bboxes <- vector('list', nsites)

for(i in 1:nsites){
bboxes[[i]] <- sf::st_as_sf(bbox_data[i,],
                      geometry = st_as_sfc(
                        sf::st_bbox(c(xmin = bbox_data$bounding_west[i],
                                  xmax = bbox_data$bounding_east[i],
                                  ymin = bbox_data$bounding_south[i], 
                                  ymax = bbox_data$bounding_north[i]),
                      crs = 4326)))
} 

# put them all together into a single object # 

site_bbox <- do.call(rbind, bboxes)

# write a shapefile of the bounding boxes # 
# sf::st_write(site_bbox, "Data/site_bounding_boxes.shp")

# Uncomment this code to read in the shapefile directly #
# sf::st_read("Data/site_bounding_boxes.shp")

###############################################################################
#
# 2) Create a buffer around each bounding box 
#    Note - coordinates are in WGS84 - i.e., crs = 4326
###############################################################################

# site_bbox <- sf::st_read("Data/site_bounding_boxes.shp")

# create a 1km buffer # 

# steps - 
# project into World Equidistant Projection (in meters) # 
# create a buffer (in meters) 
# re-project to WGS84 

site_bbox_1km <- sf::st_transform(site_bbox, crs = 4087) %>% 
                  sf::st_buffer(dist = 1000) %>% # 1km 
                  sf::st_transform(4326)

site_bbox_5km <- sf::st_transform(site_bbox, crs = 4087) %>% 
                  sf::st_buffer(dist = 5000) %>% # 5km 
                  sf::st_transform(4326)

# Plot to confirm # 
# plot(site_bbox_5km[1,]$geometry)
# plot(site_bbox_1km[1,]$geometry, add = TRUE)
# plot(site_bbox[1,]$geometry, add = TRUE)

# sf::st_write(site_bbox_1km, "Data/site_bounding_boxes_1km_buffer.shp")
# sf::st_write(site_bbox_5km, "Data/site_bounding_boxes_5km_buffer.shp")

###############################################################################
#
# 3) Connect to Google Earth Engine to get spatial data  
#
###############################################################################
userName <- SECRETS$userName
projectID <- SECRETS$projecID

# Credentials are needed to access Google Earth Engine 
ee_Initialize(user = userName,
              project = projectID, 
              drive = TRUE)

# convert from spatial to earth engine object # 
site_box_ee <- rgee::sf_as_ee(site_bbox)
site_box_1km_ee <- rgee::sf_as_ee(site_bbox_1km)
site_box_5km_ee <- rgee::sf_as_ee(site_bbox_5km)


# Monthly climate data 
# https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_TERRACLIMATE #
# tmin, tmax, precip 
# 1958-2024 
TerraClim_tmin <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$select("tmmn")
TerraClim_tmax <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$select("tmmx")
TerraClim_ppt <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$select("pr")
TerraClim_dsi <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$select("pdsi")

# Scaling parameter for the data # 
tmin_scaling <- 0.1
tmax_scaling <- 0.1
# ppt_scaling #NOT APPLICABLE 
pdsi_scaling <- 0.01

# Alternative dataset # 
# https://developers.google.com/earth-engine/datasets/catalog/NASA_GDDP-CMIP6#bands
# CMIP6 <- ee$ImageCollection("NASA/GDDP-CMIP6") 

# Create a vector that stores the June and January system data to extract just the data that we want #
JunYrs <- paste0(1990:2024, formatC(6,flag = 0, digits = 1))
JanYrs <- paste0(1990:2024, formatC(1,flag = 0, digits = 1))

# Get the rasters that we want 
# tmin
TerraClim_jan_tmin <- TerraClim_tmin$filter(ee$Filter$inList("system:index",JanYrs))
TerraClim_jun_tmin <- TerraClim_tmin$filter(ee$Filter$inList("system:index",JunYrs))

TerraClim_jan_tmax <- TerraClim_tmax$filter(ee$Filter$inList("system:index",JanYrs))
TerraClim_jun_tmax <- TerraClim_tmax$filter(ee$Filter$inList("system:index",JunYrs))

TerraClim_jan_ppt <- TerraClim_ppt$filter(ee$Filter$inList("system:index",JanYrs))
TerraClim_jun_ppt <- TerraClim_ppt$filter(ee$Filter$inList("system:index",JunYrs))

TerraClim_jan_dsi <- TerraClim_dsi$filter(ee$Filter$inList("system:index",JanYrs))
TerraClim_jun_dsi <- TerraClim_dsi$filter(ee$Filter$inList("system:index",JunYrs))


# Extract and download the raster data #

# TMAX 
sites_TC_tmax_jan <- TerraClim_jan_tmax$map(function(image){return(image$clip(site_box_5km_ee))})

#ee_imagecollection_to_local(ic = sites_TC_tmax_jan,
#                            region = site_box_5km_ee$geometry(),
#                            dsn = "./ClimateData/tmax/Jan/",
#                            via = "drive",
#                            container = "EarthEngine")

# Extract and download the raster data #

sites_TC_tmax_jun <- TerraClim_jun_tmax$map(function(image){return(image$clip(site_box_5km_ee))})

#ee_imagecollection_to_local(ic = sites_TC_tmax_jun,
#                            region = site_box_5km_ee$geometry(),
#                            dsn = "./ClimateData/tmax/Jun/",
#                            via = "drive",
#                            container = "EarthEngine")

# TMIN 
sites_TC_tmin_jan <- TerraClim_jan_tmin$map(function(image){return(image$clip(site_box_5km_ee))})

# ee_imagecollection_to_local(ic = sites_TC_tmin_jan,
#                             region = site_box_5km_ee$geometry(),
#                            dsn = "./ClimateData/tmin/Jan/",
#                            via = "drive",
#                            container = "EarthEngine")


sites_TC_tmin_jun <- TerraClim_jun_tmax$map(function(image){return(image$clip(site_box_5km_ee))})

# ee_imagecollection_to_local(ic = sites_TC_tmin_jun,
#                            region = site_box_5km_ee$geometry(),
#                            dsn = "./ClimateData/tmin/Jun/",
#                            via = "drive",
#                            container = "EarthEngine")

# PPT 
sites_TC_ppt_jan <- TerraClim_jan_ppt$map(function(image){return(image$clip(site_box_5km_ee))})

# ee_imagecollection_to_local(ic = sites_TC_ppt_jan,
#                             region = site_box_5km_ee$geometry(),
#                             dsn = "./ClimateData/ppt/Jan/",
#                             via = "drive",
#                             container = "EarthEngine")


sites_TC_ppt_jun <- TerraClim_jun_ppt$map(function(image){return(image$clip(site_box_5km_ee))})

# ee_imagecollection_to_local(ic = sites_TC_ppt_jun,
#                            region = site_box_5km_ee$geometry(),
#                            dsn = "./ClimateData/ppt/Jun/",
#                            via = "drive",
#                            container = "EarthEngine")

# DSI 
sites_TC_dsi_jan <- TerraClim_jan_dsi$map(function(image){return(image$clip(site_box_5km_ee))})

#ee_imagecollection_to_local(ic = sites_TC_dsi_jan,
#                            region = site_box_5km_ee$geometry(),
#                            dsn = "./ClimateData/dsi/Jan/",
#                            via = "drive",
#                            container = "EarthEngine")


sites_TC_dsi_jun <- TerraClim_jun_dsi$map(function(image){return(image$clip(site_box_5km_ee))})

# ee_imagecollection_to_local(ic = sites_TC_dsi_jun,
#                            region = site_box_5km_ee$geometry(),
#                            dsn = "./ClimateData/dsi/Jun/",
#                            via = "drive",
#                            container = "EarthEngine")

####################################################################################
#
# 4) Read in the spatial data and calculate climate velocity (within the 5km buffer) 
#
####################################################################################

library(VoCC)

# objects to hold the file names and paths # 
tminFiles <- list.files("ClimateData/tmin", pattern = "*.tif", full.names = TRUE, recursive = TRUE)
tmaxFiles <- list.files("ClimateData/tmax", pattern = "*.tif", full.names = TRUE, recursive = TRUE)
pptFiles <- list.files("ClimateData/ppt", pattern = "*.tif", full.names = TRUE, recursive = TRUE)
dsiFiles <- list.files("ClimateData/dsi", pattern = "*.tif", full.names = TRUE, recursive = TRUE)

# subset and read in rasters # 
tminJan <- terra::rast(grep(tminFiles, pattern = "/Jan/", value = TRUE))
tminJun <- terra::rast(grep(tminFiles, pattern = "/Jun/", value = TRUE))
tmaxJan <- terra::rast(grep(tmaxFiles, pattern = "/Jan/", value = TRUE))
tmaxJun <- terra::rast(grep(tmaxFiles, pattern = "/Jun/", value = TRUE))
pptJan <- terra::rast(grep(pptFiles, pattern = "/Jan/", value = TRUE))
pptJun <- terra::rast(grep(pptFiles, pattern = "/Jun/", value = TRUE))
dsiJan <- terra::rast(grep(dsiFiles, pattern = "/Jan/", value = TRUE))
dsiJun <- terra::rast(grep(dsiFiles, pattern = "/Jun/", value = TRUE))

# Calculate temporal trend #


tmin_jan_trend <- tempTrend(raster::stack(tminJan), th = 5)
tmin_jan_grad <- spatGrad(raster::stack(tminJan), projected = FALSE)
tmin_jan_velocity <- terra::rast(gVoCC(tmin_jan_trend, tmin_jan_grad))

tmin_jun_trend <- tempTrend(raster::stack(tminJun), th = 5)
tmin_jun_grad <- spatGrad(raster::stack(tminJun), projected = FALSE)
tmin_jun_velocity <- terra::rast(gVoCC(tmin_jun_trend, tmin_jun_grad))


tmax_jan_trend <- tempTrend(raster::stack(tmaxJan), th = 5)
tmax_jan_grad <- spatGrad(raster::stack(tmaxJan), projected = FALSE)
tmax_jan_velocity <- terra::rast(gVoCC(tmax_jan_trend, tmax_jan_grad))

tmax_jun_trend <- tempTrend(raster::stack(tmaxJun), th = 5)
tmax_jun_grad <- spatGrad(raster::stack(tmaxJun), projected = FALSE)
tmax_jun_velocity <- terra::rast(gVoCC(tmax_jun_trend, tmax_jun_grad))


ppt_jan_trend <- tempTrend(raster::stack(pptJan), th = 5)
ppt_jan_grad <- spatGrad(raster::stack(pptJan), projected = FALSE)
ppt_jan_velocity <- terra::rast(gVoCC(ppt_jan_trend, ppt_jan_grad))

ppt_jun_trend <- tempTrend(raster::stack(pptJun), th = 5)
ppt_jun_grad <- spatGrad(raster::stack(pptJun), projected = FALSE)
ppt_jun_velocity <- terra::rast(gVoCC(ppt_jun_trend, ppt_jun_grad))

dsi_jan_trend <- tempTrend(raster::stack(dsiJan), th = 5)
dsi_jan_grad <- spatGrad(raster::stack(dsiJan), projected = FALSE)
dsi_jan_velocity <- terra::rast(gVoCC(dsi_jan_trend, dsi_jan_grad))

dsi_jun_trend <- tempTrend(raster::stack(dsiJun), th = 5)
dsi_jun_grad <- spatGrad(raster::stack(dsiJun), projected = FALSE)
dsi_jun_velocity <- terra::rast(gVoCC(dsi_jun_trend, dsi_jun_grad))

names(tmin_jan_velocity) <- paste0("Jan_tmin_",names(tmin_jan_velocity))
names(tmin_jun_velocity) <- paste0("Jun_tmin_",names(tmin_jun_velocity))
names(tmax_jan_velocity) <- paste0("Jan_tmax_",names(tmax_jan_velocity))
names(tmax_jun_velocity) <- paste0("Jun_tmax_",names(tmax_jun_velocity))
names(ppt_jan_velocity) <- paste0("Jan_ppt_",names(ppt_jan_velocity))
names(ppt_jun_velocity) <- paste0("Jun_ppt_",names(ppt_jun_velocity))
names(dsi_jan_velocity) <- paste0("Jan_dsi_",names(dsi_jan_velocity))
names(dsi_jun_velocity) <- paste0("Jun_dsi_",names(dsi_jun_velocity))

# stack all the layers together into a single raster stack # 

climate_velocities <- c(tmin_jan_velocity,tmin_jun_velocity,
                        tmax_jan_velocity,tmax_jun_velocity,
                        ppt_jan_velocity,ppt_jun_velocity, 
                        dsi_jan_velocity, dsi_jun_velocity)

# terra::writeRaster(climate_velocities, "Gradient_Climate_Velocities.tif")

# estimate residence time within the 5km buffer area #
residenceTimes <- array(NA, c(nrow(site_bbox_5km), nlyr(climate_velocities)),
                        dimnames = list(site_bbox_5km$package_id,
                                        paste0("resTime_",names(climate_velocities))))
for(i in 1:nlyr(climate_velocities)){
residenceTimes[,i] <- resTime(as(site_bbox_5km,"Spatial"), raster::raster(climate_velocities[[i]]))$resTim
}

# write.csv(residenceTimes, "G:/My Drive/Forest-Biodiversity/residenceTimes_climate_variables.csv") 


