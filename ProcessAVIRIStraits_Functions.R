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

plot_flightlines <-function(flightline_data){
  # plot(flightline_data[[1]]$f180612t01p00r06_rfl_v1k2_img_PLSR_500_raw_coef_Carbon_400_2400_clip.1)
  # par(new=TRUE)
  # plot(flightline_data[[2]]$f180612t01p00r07_rfl_v1k2_img_PLSR_500_raw_coef_Carbon_400_2400_clip.1)
  # par(new=TRUE)
  # plot(flightline_data[[3]]$f180612t01p00r08_rfl_v1k2_img_PLSR_500_raw_coef_Carbon_400_2400_clip.1)
  # par(new=TRUE)
  # plot(flightline_data[[4]]$f180612t01p00r09_rfl_v1k2_img_PLSR_500_raw_coef_Carbon_400_2400_clip.1)
  # par(new=TRUE)
  # plot(flightline_data[[5]]$f180831t01p00r08_rfl_v1k2_img_PLSR_500_raw_coef_Carbon_400_2400_clip.1)
  # par(new=TRUE)
  # plot(flightline_data[[6]]$f180831t01p00r09_rfl_v1k2_img_PLSR_500_raw_coef_Carbon_400_2400_clip.1)
  # par(new=TRUE)
  # plot(flightline_data[[7]]$f180831t01p00r10_rfl_v1k2_img_PLSR_500_raw_coef_Carbon_400_2400_clip.1)
  # par(new=TRUE)
  # plot(flightline_data[[8]]$f180831t01p00r11_rfl_v1k2_img_PLSR_500_raw_coef_Carbon_400_2400_clip.1)
  # par(new=TRUE)
  # plot(flightline_data[[9]]$f180904t01p00r07_rfl_v1k2_img_PLSR_500_raw_coef_Carbon_400_2400_clip.1)
  # par(new=TRUE)
  # plot(flightline_data[[10]]$f180904t01p00r08_rfl_v1k2_img_PLSR_500_raw_coef_Carbon_400_2400_clip.1)
  # 
  # res_plot <- ggplot(as.data.frame(flightline_data[[i]],
  #                                  xy=TRUE, na.rm = TRUE), aes(x = x, y = y)) +
  #   geom_raster(aes(fill = alpha)) +
  #   scale_fill_viridis_c() +
  #   ggtitle("Resistance Data") +
  #   coord_equal() + theme_bw()
  # print(res_plot)
  # par(new=TRUE)
  
}

extract_pixel_trait_value <- function(AVIRISdata, point_locations_longitude, 
                                      point_locations_latitude){
  # Precondition: AVIRISdata is a raster object, point_locations is a vector of
  #      field point locations
  # Postcondition: a dataframe of AVIRIS data as columns and point values as rows
  
  # Set Given variables
  deg2rad = pi / 180.0
  rad2deg = 180.0 / pi
  EquatorialRadius = 2
  eccentricitySquared = 3
  
  
  ###### Example code from Python for pixel extraction
  # import sys
  # import spectral.io.envi as envi
  # import os.path
  # from math import pi, sin, cos, tan, sqrt
  # from scipy import datetime64
  # import scipy.misc as misc 
  # import scipy as s 
  # import argparse
  # import matplotlib.pyplot as plt
  # import scipy.ndimage as ndimage
  # from scipy.optimize import fmin
  # import time
  # import matplotlib.path as path
  # 
  # _deg2rad = pi / 180.0
  # _rad2deg = 180.0 / pi
  # _EquatorialRadius = 2
  # _eccentricitySquared = 3
  # 
  # 
  # def getLLPix(I, point):
  # 
  # # Get header details
  # mapinfo = I.metadata['map info']
  # zone = str(I.metadata['map info'][7])+'S'
  # ref = str(I.metadata['map info'][9])
  # 
  # # Pixel offset and size, in UTM
  # offs = [float(mapinfo[3]), float(mapinfo[4])]           
  # size = [float(mapinfo[5]), float(mapinfo[6])]           
  # 
  # # Parse geolocalization information out of the header
  # R = s.array([[1,0],[0,1]])
  # for m in I.metadata['map info']:
  #   if 'rotation' in m:
  #   rot = float(m.split('=')[-1])*(2*pi)/360.0
  # R = s.array([[cos(rot),-sin(rot)],[sin(rot),cos(rot)]])
  # if ref == 'WGS-84':
  #   ref = 23
  # else:
  #   raise ValueError('unsupported ellipsoid '+ref)  
  # 
  # # Find the un-rotated Lat / Lon position 
  # # Center of mass
  # def pix2ll(r,c):
  #   rot = s.dot(R,s.array(([[c],[-r]])))
  # utm = offs + (rot[:,0]+0.5)*size
  # ll = UTMtoLL(ref, utm[1], utm[0], zone)
  # return ll[0],ll[1]   
  # 
  # def err(state):
  #   return sum(pow(s.array(point) - s.array(pix2ll(state[0],state[1])),2))
  # 
  # guess = s.array([I.nrows/2, I.ncols/2])
  # best =  fmin(err, guess, disp=False)
  # 
  # if (best[0] >= 0 and best[0] < I.nrows and
  #     best[1] >= 0 and best[1] < I.ncols):
  #   return best
  # else:
  #   raise IndexError  
}