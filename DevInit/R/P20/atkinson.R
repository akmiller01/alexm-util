gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
atkinson = function(x,aversion=1,na.rm=TRUE){
  if(aversion ==1){
    return(1-(gm_mean(x,na.rm=na.rm)/mean(x,na.rm=na.rm)))
  }else{
    return(
      1 - ((1/mean(x,na.rm=na.rm))*((1/length(x))*sum(x^(1-aversion),na.rm=na.rm))^(1/(1-aversion)))
    )
  }
}

#install.packages("DescTools")
# library(DescTools)
# Atkinson(c(1,2,3),parameter=0.5)
atkinson(c(1,2,3),0.5)
