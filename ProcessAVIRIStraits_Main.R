# Script for processing AVIRIS/NEON AOP flightline trait data from 
# Phil Townsend Spectral Lab at UWisc
# Natasha Stavros, CU Boulder

# Trait data based on Wang et al. (2020, New Phytologist)

library(rgdal)
library(raster)
library(gdalUtils)
library(sf)
library(ggplot2)
library(caTools)
source("/Users/natasha/Desktop/RussianRiver_Analysis/ProcessAVIRIStraits_Functions.R")

##### Given Variables
AVIRISpath = "/Users/natasha/Desktop/RussianRiver_Analysis/2018-AVIRIS/"
boundingKML = "/Users/natasha/Desktop/RussianRiver_Analysis/RR_HU8_projected.kml"
missingData = -9999
# From Wang et al. (2020) New Phytologist, Table 1
dataThresholds = c(700,4,500,15,0,5,900,600,600,75,500,300,5,400)
traits = c("Carbon","carotenoid","Cellulose","chl","d13C","d15N","Fiber","Lignin",
           "LMA","Nitrogen","NSC","Phenolics","Phosphorus","Sugar")

##### Derived Variables
# list only geotiffs and ignore JSON
AVIRIStifs = list.files(AVIRISpath,pattern="\\.tif$") #list all Tifs
# list only trait data
AVIRIStraits = Filter(function(x) grepl("PLSR",x), AVIRIStifs)
#perimeter = st_read(boundingKML)
perimeter = boundingKML

for (i in 1:length(traits)){
  print (paste("Working on trait: ",traits[i],sep=""))

  # get geotiff list
  new_AVIRIStifs <- list.files(AVIRISpath,pattern="\\.tif$") #list all Tifs
  new_AVIRIStraits <- Filter(function(x) grepl("PLSR",x), new_AVIRIStifs)
  trait_data <- Filter(function(x) grepl(traits[i],x), new_AVIRIStraits)
  
  # check if you have run this function before and if you have clipped flightlines
  # already
  trait_data_clipped <- Filter(function(x) grepl("clip",x), trait_data)
  if (length(trait_data_clipped) == 0){
    flightline_data <- clean_AVIRISFlightTraits(traitGeoTiffs = AVIRIStraits, path = AVIRISpath,
                                                trait = traits[i], missingData = missingData,
                                                perimeter = perimeter, dataThreshold = dataThresholds[i],
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

