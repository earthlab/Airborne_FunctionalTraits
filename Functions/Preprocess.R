library(rgdal)
library(raster)
library(gdalUtils)
library(sf)
library(ggplot2)

clean_AVIRISFlightTraits <- function(traitGeoTiffs, path, trait, perimeter,
                                     missingData,dataThresholds,saveFile=FALSE){
  # Preconditions:traitGeoTiffs = list of trait geotiff file names as strings, 
  #       assumed to be in the same folder, path is a string with the path
  #       to that folder; trait = the string of the trait name as spelt in filename,
  #       missingData is the data value used for missing data and the
  #       dataThresholds is a list with the max value for "reasonable" data and
  #       the min value for "reasonable data. Savefile = FALSE
  #       will remove the warpped, clipped geotiff; TRUE will keep it.
  
  # Postcondition: list of clipped and cleaned rasters of each flightline
  
  # get list of all flightlines in folder
  trait_data = Filter(function(x) grepl(trait,x), traitGeoTiffs)
  
  flightline_data = list()
  for (i in 1:length(trait_data)){
    print(paste("flightline ",as.character(i)," of ",as.character(length(trait_data))))
    
    # create datafile path and name
    datafile <- paste(path,trait_data[i],sep="")
    
    data1 <- raster(datafile,band=1)
    crs_reference <- crs(data1)
    rm(data1)

    # rotate and clip data files to bounding perimeter
    data_rectified <- gdalwarp(srcfile = datafile,
                               dstfile=paste(gsub("\\..*","",datafile),"_clip.tif",sep=""),
                               t_srs=crs_reference,output_Raster=TRUE, overwrite=TRUE,
                               crop_to_cutline = TRUE, dstalpha = TRUE, 
                               cutline = boundingKML,srcnodata = missingData,
                               dstnodata = missingData, ot = 'Float32')
    
    # mask bad values
    data_rectified[data_rectified >= dataThresholds[[1]]] <- missingData
    data_rectified[data_rectified <= dataThresholds[[2]]] <- missingData
    
    # edit geotif for new data_rectified with masked bad values
    writeRaster(data_rectified, 
                filename = paste(gsub("\\..*","",datafile),"_clip_mask.tif",sep=""), 
                format = 'GTiff',datatype='FLT4S',overwrite=TRUE, 
                NAflag = missingData)
    
    # store cleaned flightlien for mosiac
    flightline_data[[i]] <- data_rectified
    
    # delete rotated file
    if (saveFile == FALSE){file.remove(paste(gsub("\\..*","",datafile),"_clip.tif",sep=""))}
  }
  return(flightline_data)
}

mosaic_flightlines <- function(traits, AVIRISpath,bounding_vector, missingData,
                               dataThresholds_upper, dataThresholds_lower,
                               save_intermediate_output = FALSE,outputfile_descriptor){
  # Preconditions: traits are a list of strings specifying the traits to process,
  #       AVIRISpath is a string with the path to the folder with all the trait geotiffs,
  #       bounding_vector is a vector file (e.g., kml or esri shapefile) for the area
  #       to which we clip the trait data, missingData is the assumed value of
  #       the missing data, dataThresholds_upper is a vector of the upper thresholds
  #       for a reasonable range on the traits (see Wang et al. 2020, New Phytologist),
  #       dataThresholds_lower is the lower threshold for a reasonable range on
  #       the traits (see Wang et al 2020, New Phytologist). save_intermediate_output
  #       determines whether we save the clipped flightline data pre-mosaic.
  #       outputfile_descriptor is a string for naming the output file. Recommend
  #       "FlightboxName_date" or something equivalent.
  
  # Postcondition: A saved geotiff to AVIRISpath of the mosaicked traits
  
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
                                                  perimeter = bounding_vector, 
                                                  dataThresholds = c(dataThresholds_upper[i],
                                                                     dataThresholds_lower[i]),
                                                  saveFile = save_intermediate_output)
      
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
                           dst_dataset=paste(AVIRISpath,outputfile_descriptor,"_mosaic_",traits[i],".tif",sep=""),
                           r = "nearest", separate=FALSE, of = "GTiff", #of="ENVI",
                           verbose=FALSE,output_Raster = TRUE, vrtnodata = missingData,
                           hidenotdata = TRUE,addalpha=TRUE, overwrite=TRUE)
    
    # clean up variables ### UNNECESSARY IN FUNCTION
    # for (j in 1:length(trait_data_clipped)){file.remove(list=clip_layer_list[[j]])}
  }
}

raster2matrix <- function(RasterLayer){
  # Precondition: RasterLayer is a raster object
  
  # Postcondition: a matrix object of the RasterLayer data values
  
  if(dim(RasterLayer)[3] <= 1){
    A <- raster::as.matrix(RasterLayer)
    m <- t(A[nrow(A):1,])
  }else{
    m <- aperm(raster::as.array(flip(RasterLayer,direction=2)),c(2,1,3))
  }
  return(m)
}

normalize_traits <- function(RasterLayer){
  # Precondition: R is a raster object of trait data to be normalized
  
  # Postconditions: normalized trait raster to provide values between 0 and 1. 
  #           See Fabian D Schneider et al. (2017) Nature Communications
  #           DOI: 10.1038/s41467-017-01530-3.
  
  raster_matrix <- raster2matrix(RasterLayer)
  raster_max <- max(raster_matrix[!is.na(raster_matrix)])
  raster_matrix_normalized <- apply(raster_matrix, MARGIN = c(1,2), FUN = function(x) x/(trait_max))
  
  return (raster_matrix_normalized)
}
