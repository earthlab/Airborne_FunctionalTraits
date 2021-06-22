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
library(plot.matrix)
source("/Users/natasha/Coding/RFunctions/Airborne_FunctionalTraits/Functions/Preprocess.R")
source("/Users/natasha/Coding/RFunctions/Airborne_FunctionalTraits/Initial_Parameters.R")
source("/Users/natasha/Coding/RFunctions/Airborne_FunctionalTraits/Functions/Diversity.R")

AVIRISmosaics <- 
  list.files(AVIRISpath,pattern= paste0(outputfile_descriptor,"_mosaic_"))

if (length(AVIRISmosaics) == 0){
  # Mosaic Flightlines assuming all flightlines are saved to one path
  mosaic_flightlines(tratis = traits, AVIRSpath = AVIRISpath,
                     bounding_vector = boundingKML, missingData = missingData,
                     dataThresholds_upper = dataThresholds_upper,
                     dataThresholds_lower = dataThresholds_lower,
                     save_intermediate_output = FALSE, 
                     outputfile_descriptor = outputfile_descriptor) 
}

i = 1
trait_test <- raster(paste0(AVIRISpath,AVIRISmosaics[i]),band=1)
trait_normalized <- normalize_traits(trait_test)

# ##### Begin Mosaics of Traits ####
# for (i in 1:length(traits)){
#   print (paste("Working on trait: ",traits[i],sep=""))
# 
#   # get geotiff list
#   AVIRIStifs <- list.files(AVIRISpath,pattern="\\.tif$") #list all Tifs
#   trait_data <- Filter(function(x) grepl(traits[i],x), AVIRIStifs)
#   
#   # Clean Data, but first check if you have run this function before 
#   # and if you have clipped flightlines already, skip this step
#   trait_data_clipped <- Filter(function(x) grepl("clip",x), trait_data)
#   if (length(trait_data_clipped) == 0){
#     flightline_data <- clean_AVIRISFlightTraits(traitGeoTiffs = trait_data, 
#                                                 path = AVIRISpath,
#                                                 trait = traits[i], 
#                                                 missingData = missingData,
#                                                 perimeter = boundingKML, 
#                                                 dataThresholds = c(dataThresholds_upper[i],
#                                                                    dataThresholds_lower[i]),
#                                                 saveFile = FALSE)
#     
#     # get new clipped geotiff list
#     new_AVIRIStifs <- list.files(AVIRISpath,pattern="\\.tif$") #list all Tifs
#     new_AVIRIStraits <- Filter(function(x) grepl("PLSR",x), new_AVIRIStifs)
#     trait_data <- Filter(function(x) grepl(traits[i],x), new_AVIRIStraits)
#     trait_data_clipped <- Filter(function(x) grepl("clip_mask",x), trait_data)
#   }
#   
#   # creates a new layer list based on flightline # in flightline_data list
#   clip_layer_list <- vector()
#   for (j in 1:length(trait_data_clipped)){
#     layer_name <- paste("layer",j,sep="")
#     clip_layer_list[j] <- assign(layer_name, 
#                                  paste(AVIRISpath,trait_data_clipped[j],sep=""))
#   }
#   
#   # mosaic flightlines
#   test <- mosaic_rasters(gdalfile=clip_layer_list,
#                  dst_dataset=paste(AVIRISpath,"mosaic_",traits[i],".tif",sep=""),
#                  r = "nearest", separate=FALSE, of = "GTiff", #of="ENVI",
#                  verbose=FALSE,output_Raster = TRUE, vrtnodata = missingData,
#                  hidenotdata = TRUE,addalpha=TRUE, overwrite=TRUE)
#   
#   # clean up variables
#   for (j in 1:length(trait_data_clipped)){file.remove(list=clip_layer_list[[j]])}
# }

