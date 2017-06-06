se <- function(est,n,pop){
  return(sqrt(((1-(n/pop))/n)*(pop/(pop-1))*(est*(1-est))))
}