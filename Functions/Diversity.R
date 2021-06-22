functional_divergence <- function(X){
  # Preconditions:   X is a matrix with functional traits as columns and 
  #           samples/pixels as rows, values/traits in X should be normalized;
  #           apply normalize_traits function first. Function courtesy of 
  #           Fabian D. Schneider.
  
  # Postcondition: raster of functional divergence based on Villeger et al (2008), DOI: 10.1890/07-1206.1, 
  
  out = 0
  
  ind = sum( X, 2 ) > 0
  uniquePts = unique( X(ind,:), 'rows' )
  nbUniquePts = size( uniquePts, 1 )
  
  # 4 or more unique points needed
  if (nbUniquePts > 3){
    g = mean( uniquePts )
    
    dG = pdist( cat( 1, g, uniquePts ), 'euclidean' )
    dG = dG( 1:nbUniquePts )
    dG_mean = mean( dG )
    
    deltaD = sum( 1./nbUniquePts .* ( dG - dG_mean ) )
    deltaAbsD = sum( 1./nbUniquePts .* abs( dG - dG_mean ) )
    
    out = ( deltaD + dG_mean ) / ( deltaAbsD + dG_mean )
  }
  
  return(out)
}
