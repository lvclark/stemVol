stemVol <- function(len, diam){
  # This function estimates the volume of one or more stems.
  # len is a vector indicating the length of each stem.
  # diam is a vector indicating the diameter of each stem.
  #   diam and len should be in the same units.
  # A numeric vector is returned indicating the volume of each
  # stem, assuming a perfect cylinder.
  if(length(len) != length(diam)){
    stop("len and diam must be vectors of equal length.")
  }
  notPos <- len <= 0 | diam <= 0
  if(any(notPos)){
    cat("Problematic indices: ")
    cat(which(notPos))
    stop("All values of len and diam must be greater than zero.")
  }
  ratio <- len/diam
  ratioNotOk <- ratio > 500 | ratio < 10
  if(any(ratioNotOk)){
    warning("Some observations have a ratio of stem length to stem diameter outside of the expected range.")
    cat("", sep = "\n")
    cat("Problematic indices: ")
    cat(which(ratioNotOk))
    cat("", sep = "\n")
  }
  
  vol <- len * (diam/2) ^ 2 * pi
  return(vol)
}
