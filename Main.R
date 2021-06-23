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
library(mgcv)
library(rdist)
source("/Users/natasha/Coding/RFunctions/Airborne_FunctionalTraits/Functions/Preprocess.R")
source("/Users/natasha/Coding/RFunctions/Airborne_FunctionalTraits/Initial_Parameters.R")
source("/Users/natasha/Coding/RFunctions/Airborne_FunctionalTraits/Functions/Diversity.R")


## Prep Trait data into mosaics
AVIRISmosaics <- 
  list.files(AVIRISpath,pattern= paste0(outputfile_descriptor,"_mosaic_"))

if (length(AVIRISmosaics) == 0){
  # Mosaic Flightlines assuming all flightlines are saved to one path
  mosaic_flightlines(traits = traits, path = AVIRISpath,
                     bounding_vector = boundingKML, missingData = missingData,
                     dataThresholds_upper = dataThresholds_upper,
                     dataThresholds_lower = dataThresholds_lower,
                     save_intermediate_output = FALSE, 
                     outputfile_descriptor = outputfile_descriptor) 
}

# Read in one trait mosaic to determine size
temp <- raster(paste0(AVIRISpath,AVIRISmosaics[1]),band=1)
mosaic_size <- dim(temp)
mosaic_size_1D <- mosaic_size[1]*mosaic_size[2]
rm(temp)

## Calculate Diversity Geotiffs
Trait_Diversity_Matrix <- trait_matrix_prep(mosaic_size_1D, traits, traits_4_diversity, 
                                                        AVIRISpath, AVIRISmosaics)

# Create Diversity Matrices
physio_divergence <- functional_divergence(Trait_Diversity_Matrix)

# Rasterize Diversity matrices raster information

# Write Raster to Geotiff