# Script for processing AVIRIS/NEON AOP flightline trait data from 
# Phil Townsend Spectral Lab at UWisc
# Trait data based on Wang et al. (2020, New Phytologist)
# Code by Natasha Stavros, CU Boulder

library(rgdal)
library(raster)
library(gdalUtils)
library(sf)
library(ggplot2)
library(caTools)
source("/Users/natasha/Desktop/RussianRiver_Analysis/ProcessAVIRIStraits_Functions.R")

##### Static Variables #####
# path to all geotifs for all traits and all flightlines to mosaic
AVIRISpath = "/Users/natasha/Desktop/RussianRiver_Analysis/2018-AVIRIS/"
# kml of perimeter
boundingKML = "/Users/natasha/Desktop/RussianRiver_Analysis/RR_HU8_projected.kml"
missingData = -9999

# From Wang et al. (2020) New Phytologist, Table 1 based on 
# minimum (rounded down) - standard deviation (rounded up) for the lower limit and 
# maximum (rounded up) + standard deviation (rounded up) for upper limit
# NOTE: "additions" are adjustments based on the local area
traits = c("Carbon","carotenoid","Cellulose","chl","d13C","d15N","Fiber","Lignin",
           "LMA","Nitrogen","NSC","Phenolics","Phosphorus","Sugar","rgbim")
dataThresholds_max = c(606,3,413,13,-13,4,847,557,508,59,472,272,4,395,1000)
dataThresholds_min = c(413,0,27,0,-41,-5,12, 5 + 185,33,7,21,9,0,14,0)
dataThresholds_std = c(38,2,67,6,4,2,132,87,63,8,78,37,1,68,0)
dataThresholds_upper = dataThresholds_max + dataThresholds_std
dataThresholds_lower = dataThresholds_min - dataThresholds_std
dataThresholds_df = data.frame(traits,dataThresholds_min,dataThresholds_max, 
                               dataThresholds_std, dataThresholds_lower,
                               dataThresholds_upper)

##### Begin Mosaics of Traits ####
for (i in 1:length(traits)){
  print (paste("Working on trait: ",traits[i],sep=""))

  # get geotiff list
  AVIRIStifs <- list.files(AVIRISpath,pattern="\\.tif$") #list all Tifs
  trait_data <- Filter(function(x) grepl(traits[i],x), AVIRIStifs)
  
  # Clean Data, but first check if you have run this function before 
  # and if you have clipped flightlines already, skip this step
  trait_data_clipped <- Filter(function(x) grepl("clip",x), trait_data)
  if (length(trait_data_clipped) == 0){
    flightline_data <- clean_AVIRISFlightTraits(traitGeoTiffs = trait_data, 
                                                path = AVIRISpath,
                                                trait = traits[i], 
                                                missingData = missingData,
                                                perimeter = boundingKML, 
                                                dataThresholds = c(dataThresholds_upper[i],
                                                                   dataThresholds_lower[i]),
                                                saveFile = FALSE)
    
    # get new clipped geotiff list
    new_AVIRIStifs <- list.files(AVIRISpath,pattern="\\.tif$") #list all Tifs
    new_AVIRIStraits <- Filter(function(x) grepl("PLSR",x), new_AVIRIStifs)
    trait_data <- Filter(function(x) grepl(traits[i],x), new_AVIRIStraits)
    trait_data_clipped <- Filter(function(x) grepl("clip_mask",x), trait_data)
  }
  
  # creates a new layer list based on flightline # in flightline_data list
  clip_layer_list <- vector()
  for (j in 1:length(trait_data_clipped)){
    layer_name <- paste("layer",j,sep="")
    clip_layer_list[j] <- assign(layer_name, 
                                 paste(AVIRISpath,trait_data_clipped[j],sep=""))
  }
  
  # mosaic flightlines
  test <- mosaic_rasters(gdalfile=clip_layer_list,
                 dst_dataset=paste(AVIRISpath,"mosaic_",traits[i],sep=""),
                 r = "nearest", separate=FALSE,of="ENVI",
                 verbose=FALSE,output_Raster = TRUE, vrtnodata = missingData,
                 hidenotdata = TRUE,addalpha=TRUE, overwrite=TRUE)
  
  # clean up variables
  for (j in 1:length(trait_data_clipped)){file.remove(list=clip_layer_list[[j]])}
}
