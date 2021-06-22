extract_pixel_trait_value <- function(AVIRISdata, point_locations_longitude, 
                                      point_locations_latitude){
  # Precondition: AVIRISdata is a raster object, point_locations is a vector of
  #      field point locations
  # Postcondition: a dataframe of AVIRIS data as columns and point values as rows
  
  # Set static variables
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