Noisify <- function(data,magnitude) {
  
  if (is.vector(data)) {
    noise <- runif(length(data), -1*magnitude, magnitude)
    noisified <- data + noise
  } else {
    length <- dim(data)[1] * dim(data)[2]
    noise <- matrix(runif(length, -1*magnitude, magnitude), dim(data)[1])
    noisified <- data + noise
  }
  return(noisified)
}