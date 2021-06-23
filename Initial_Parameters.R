# path to all geotifs for all traits and all flightlines to mosaic
AVIRISpath <- "/Users/natasha/Desktop/RussianRiver_Analysis/2018-AVIRIS/"

# kml of perimeter
boundingKML <- "/Users/natasha/Desktop/RussianRiver_Analysis/RR_HU8_projected.kml"

# Value of missing data in raw trait data from flightlines
missingData <- -9999

# From Wang et al. (2020) New Phytologist, Table 1 based on 
# minimum (rounded down) - standard deviation (rounded up) for the lower limit and 
# maximum (rounded up) + standard deviation (rounded up) for upper limit
# NOTE: "additions" are adjustments based on the local area
traits <- c("Carbon","carotenoid","Cellulose","chl","d13C","d15N","Fiber","Lignin",
           "LMA","Nitrogen","NSC","Phenolics","Phosphorus","Sugar","rgbim")
dataThresholds_max <- c(606,3,413,13,-13,4,847,557,508,59,472,272,4,395,1000)
dataThresholds_min <- c(413,0,27,0,-41,-5,12, 5 + 185,33,7,21,9,0,14,0)
dataThresholds_std <- c(38,2,67,6,4,2,132,87,63,8,78,37,1,68,0)
dataThresholds_upper <- dataThresholds_max + dataThresholds_std
dataThresholds_lower <- dataThresholds_min - dataThresholds_std
dataThresholds_df <- data.frame(traits,dataThresholds_min,dataThresholds_max, 
                               dataThresholds_std, dataThresholds_lower,
                               dataThresholds_upper)

# Descriptor; suggested flightbox or region and date of acquisition
outputfile_descriptor <- "RussianRiver_2018"

# Physiological traits to include in diversity metrics; pick 3
traits_4_diversity <- c("Carbon","Cellulose","Phosphorus")
