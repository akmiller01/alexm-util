c_to_f = function(c){
  return((c*(9/5))+32)
}

str_rev = function(x){
  return(paste0(rev(x),collapse=""))
}

reverse_digit = function(x){
  digit_split = strsplit(as.character(abs(x)), "")
  return(as.numeric(sapply(digit_split,str_rev))*sign(x))
}

dat = data.frame(
  c = c(0:100)
)

dat$f = round(c_to_f(dat$c))

dat$c_reverse = reverse_digit(dat$c)

dat_sub = subset(dat,f==c_reverse)

View(dat_sub)