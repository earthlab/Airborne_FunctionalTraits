library(rgdal)
library(raster)
library(gdalUtils)
library(sf)
library(ggplot2)
library(caTools)

clean_AVIRISFlightTraits <- function(trait_list, path, trait, perimeter,datatype,
                                     missingData,dataThresholds,saveFile=FALSE){
  # Preconditions:trait_list = list of trait geotiff file names as strings, 
  #       assumed to be in the same folder, path is a string with the path
  #       to that folder; trait = the string of the trait name as spelt in filename,
  #       missingData is the data value used for missing data and the
  #       dataThresholds is a list with the max value for "reasonable" data and
  #       the min value for "reasonable data. Savefile = FALSE
  #       will remove the warpped, clipped geotiff; TRUE will keep it. datatype 
  #       is either "tiff" for geotiff or "envi" for envi binary .dat file with 
  #       associated .hdr header file
  
  # Postcondition: list of clipped and cleaned rasters of each flightline
  
  # get list of all flightlines in folder
  trait_data = Filter(function(x) grepl(trait,x), trait_list)
  
  if (datatype == "envi"){
    trait_data = Filter(function(x) grepl(".hdr",x), trait_list)
    trait_data = stringr::str_remove_all(trait_data,".hdr")
  }
  
  flightline_data = list()
  for (i in 1:length(trait_data)){
    print(paste("flightline ",as.character(i)," of ",as.character(length(trait_data))))
    
    # create datafile path and name
    datafile <- paste(path,trait_data[i],sep="")
    
    if (file.exists(datafile)){
      if (datatype == "tiff"){
        data1 <- raster(datafile,band=1)
        crs_reference <- crs(data1)
        rm(data1)
      } else if (datatype =="envi"){
        data1 <- readLines(paste0(datafile,".hdr"))
        crs_index <- which(grepl("coordinate system string",data1))
        clean_crs_str <- stringr::str_remove_all(data1[13],"\"")
        if (str_detect(clean_crs_str,"UTM_Zone_13N")){proj_str <- "+init=EPSG:32613"}
        crs_reference <- crs(proj_str)
      } else {print("unsupported file format of trait data")}
    } else {print(paste0(datafile," does not exist."))}

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

mosaic_flightlines <- function(traits, path, bounding_vector, missingData,datatype="tiff",
                               dataThresholds_upper, dataThresholds_lower,
                               save_intermediate_output = FALSE,outputfile_descriptor){
  # Preconditions: traits are a list of strings specifying the traits to process,
  #       path is a string with the path to the folder with all the trait geotiffs,
  #       bounding_vector is a vector file (e.g., kml or esri shapefile) for the area
  #       to which we clip the trait data, missingData is the assumed value of
  #       the missing data, dataThresholds_upper is a vector of the upper thresholds
  #       for a reasonable range on the traits (see Wang et al. 2020, New Phytologist),
  #       dataThresholds_lower is the lower threshold for a reasonable range on
  #       the traits (see Wang et al 2020, New Phytologist). save_intermediate_output
  #       determines whether we save the clipped flightline data pre-mosaic.
  #       outputfile_descriptor is a string for naming the output file. Recommend
  #       "FlightboxName_date" or something equivalent. datatype is either "tiff" for
  #       geotiff or "envi" for envi binary .dat file with associated .hdr header file
  
  # Postcondition: A saved geotiff under path of the mosaicked traits
  
  ##### Begin Mosaics of Traits ####
  for (i in 1:length(traits)){
    print (paste("Working on trait: ",traits[i],sep=""))
    
    # get list of all flightlines
    AVIRISdata <- list.files(path,pattern = traits[i])
    
    # Clean Data, but first check if you have run this function before 
    # and if you have clipped flightlines already, skip this step
    trait_data_clipped <- Filter(function(x) grepl("clip",x), AVIRISdata)
    if (length(trait_data_clipped) == 0){
      flightline_data <- clean_AVIRISFlightTraits(trait_list = AVIRISdata, datatype = datatype,
                                                  path = path,
                                                  trait = traits[i], 
                                                  missingData = missingData,
                                                  perimeter = bounding_vector, 
                                                  dataThresholds = c(dataThresholds_upper[i],
                                                                     dataThresholds_lower[i]),
                                                  saveFile = save_intermediate_output)
      
      # get new clipped geotiff list
      new_AVIRIStifs <- list.files(path,pattern="\\.tif$") #list all Tifs
      new_AVIRIStraits <- Filter(function(x) grepl("PLSR",x), new_AVIRIStifs)
      trait_data <- Filter(function(x) grepl(traits[i],x), new_AVIRIStraits)
      trait_data_clipped <- Filter(function(x) grepl("clip_mask",x), trait_data)
    }
    
    # creates a new layer list based on flightline # in flightline_data list
    clip_layer_list <- vector()
    for (j in 1:length(trait_data_clipped)){
      layer_name <- paste("layer",j,sep="")
      clip_layer_list[j] <- assign(layer_name, 
                                   paste(path,trait_data_clipped[j],sep=""))
    }
    
    # mosaic flightlines
    raster_mosaic <- mosaic_rasters(gdalfile=clip_layer_list,
                           dst_dataset=paste(path,outputfile_descriptor,"_mosaic_",traits[i],".tif",sep=""),
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

normalize_traits <- function(RasterLayer, max_value = -9999){
  # Precondition: R is a raster object of trait data to be normalized; max_value
  #           is the upper bound of trait value where -9999 sets the max value to the 
  #           value within a single raster scene, otherwise it sets the max to whatever
  #           value you select across sites (e.g., max value across sites).
  
  # Postconditions: normalized trait raster to provide values between 0 and 1. 
  #           See Fabian D Schneider et al. (2017) Nature Communications
  #           DOI: 10.1038/s41467-017-01530-3.
  
  raster_matrix <- raster2matrix(RasterLayer)
  if (max_value == -9999){raster_max <- max(raster_matrix[!is.na(raster_matrix)])}
  else {raster_max <- max_value}
  raster_matrix_normalized <- 
    apply(raster_matrix, MARGIN = c(1,2), FUN = function(x) x/(raster_max))
  
  return (raster_matrix_normalized)
}

trait_matrix_prep <- function(mosaic_size_1D, traits, traits_4_diversity, 
                              path, AVIRISmosaics){
  # Preconditions: mosaic_size_1D is the total number of pixels in the mosaiced
  #               trait scene. traits is a list of strings state the traits available.
  #               traits_4_diversity are a list of strings with only the traits
  #               from the list available to consider for calculating diversity.
  #               Recommended 3 traits for diversity metric calculation. 
  #               path is the path to the geotiff raster mosaics. AVIRIS mosaics
  #               is a list of strings containing the names of each raster mosaic
  
  # Postcondition: a matrix set up with all pixels as rows and each trait needed
  #               to calculate the diversity metrics as a column
  
  trait_indices <- match(traits_4_diversity,traits)
  physiological_traits <- traits[trait_indices[!is.na(trait_indices)]]
  
  # Read in Files for trait mosaics for physiological diversity metrics and prepare
  # a trait matrix as input to diversity functions
  print("Initializing empty Trait Matrix...")
  Trait_Matrix <- matrix(,nrow= mosaic_size_1D, ncol=length(trait_indices))
  for (i in 1:length(trait_indices)){
    # Get Mosaiced Trait file names
    file_list <- Filter(function(x) grepl(physiological_traits[i],x), AVIRISmosaics)
    temp_file <- paste0(AVIRISpath,file_list[1]) # get ".tif" not .tif.aux.xml
    
    # Read in mosaic trait from Geotiff as Raster
    print(paste0("Reading in raster from Geotiff of: ",physiological_traits[i],"..."))
    trait_mosaic <- raster(temp_file,band=1)
    
    # Flatten trait matrix to 1D vector
    print("Flattening raster to 1-D vector...")
    trait_flat <- as.vector(trait_mosaic)
    
    # Prepare input matrix for Diversity metrics
    print("Adding to Trait Matrix...")
    Trait_Matrix[,i] <- trait_flat
    
    # clean up variables
    rm(file_list, temp_file,trait_mosaic,trait_flat)
  }
  return(Trait_Matrix)
}
