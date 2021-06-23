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
  
  trait_indices <- match(traits,traits_4_diversity)
  physiological_traits <- traits[trait_indices[!is.na(trait_indices)]]
  
  # Read in Files for trait mosaics for physiological diversity metrics and prepare
  # a trait matrix as input to diversity functions
  
  Trait_Diversity_Matrix <- matrix(,nrow= mosaic_size_1D, ncol=length(trait_indices))
  for (i in 1:length(trait_indices)){
    # Get Mosaiced Trait file names
    file_list <- Filter(function(x) grepl(physiological_traits[1],x), AVIRISmosaics)
    temp_file <- paste0(AVIRISpath,file_list[1]) # get ".tif" not .tif.aux.xml
    
    # Read in mosaic trait from Geotiff as Raster
    trait_mosaic <- raster(temp_file,band=1)

    # Normalize trait data by max value in scene
    trait_normalized <- normalize_traits(trait_mosaic) 
    
    # Flatten trait matrix to 1D vector
    trait_norm_flat <- as.vector(trait_normalized)
    
    # Prepare input matrix for Diversity metrics
    Trait_Diversity_Matrix <-cbind(Trait_Diversity_Matrix,trait_norm_flat)
    
    # clean up variables
    rm(file_list, temp_file,trait_mosaic,trait_normalized,trait_norm_flat)
  }
  return(Trait_Diversity_Matrix)
}

functional_divergence <- function(X){
  # Preconditions:   X is a matrix with functional traits as columns and 
  #           samples/pixels as rows, values/traits in X should be normalized;
  #           apply normalize_traits function first. Function courtesy of 
  #           Fabian D. Schneider, code adapted from Matlab by Natasha Stavros
  
  # Postcondition: matrix of functional divergence based on 
  #           Villeger et al (2008), DOI: 10.1890/07-1206.1, 
  
  # sum by row and find indices where sum is >0
  summed_rows <- rowSums(X, na.rm = TRUE)
  indices <- match(summed_rows[summed_rows > 0],summed_rows)
  
  # Find unique values of all pixels for rows where sum > 0
  uniquePts <- uniquecombs(X[indices,],ordered=TRUE)
  
  # Find # of unique rows
  number_UniquePts <- dim(uniquePts)[1]
  
  # 4 or more unique points needed
  if (number_UniquePts > 3){
    # set up all points by flattening matrix
    g <- rowMeans(uniquePts,na.rm = TRUE)
    
    # find the euclidean distance between unique points
    # NOTE: Fabian's code included g?
    # dG = pdist( cat( 1, g, uniquePts ), 'euclidean' ); ### MATLAB CODE
    dG <- pdist(uniquePts[!is.na(uniquePts)], metric = 'euclidean' ,p=2)
    #### NOTE THROWS AN ERROR BECAUSE IT'S TOO BIG AND COMPUTE IS EXPENSIVE
    
    dG <- dG(1:number_UniquePts )
    dG_mean <- mean(dG)
    
    deltaD <- apply(dG, MARGIN = c(1,2), FUN = function(x) 1/number_UniquePts * (x - dG_mean))
    #deltaD = sum( 1./nbUniquePts .* ( dG - dG_mean ) ) ### MATLAB CODE
    deltaAbsD <- apply(dG, MARGIN = c(1,2), FUN = function(x) 1/number_UniquePts * abs(x - dG_mean))
    # deltaAbsD = sum( 1./nbUniquePts .* abs( dG - dG_mean ) ) ### MATLAB CODE
    
    out <- ( deltaD + dG_mean ) / ( deltaAbsD + dG_mean )
  }
  
  return(out)
}