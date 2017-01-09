library(foreign)

setwd("D:/Documents/Data/DHSauto/ugbr60dt")
data <- read.dta("UGBR60FL.dta")
data$age.months <- (data$v008-data$b3)-1
data$age.months[which(data$age.months<0)] <- 0
data$weights <- data$v005/1000000
first.recent <- subset(data,age.months<60)
second.recent <- subset(data,age.months>=60 & age.months<120 & (b5=="yes" | b7<60))
third.recent <- subset(data,age.months>=120 & age.months<180 & (b5=="yes" | b7<60))

weighted.mean(first.recent$b5=="no",first.recent$v005)
weighted.mean(second.recent$b5=="no",second.recent$v005)
weighted.mean(third.recent$b5=="no",third.recent$v005)

probs <- c()
segments <- list(
  list("min"=0,"max"=0)
  ,list("min"=1,"max"=2)
  ,list("min"=3,"max"=5)
  ,list("min"=6,"max"=11)
)
for(i in 1:length(segments)){
  segment = segments[[i]]
  seg.min = segment[["min"]]
  seg.max = segment[["max"]]
  mortalities.data <- subset(data,age.months>=seg.min & age.months<=seg.max & b7>=seg.min & b7<=seg.max)
  mortalities.weights <- mean(mortalities.data$weights)
  mortalities <- nrow(mortalities.data)*mortalities.weights
  survivals.data <- subset(data,age.months>=seg.min & age.months<=seg.max & b5=="yes")
  survivals.weights <- mean(survivals.data$weights)
  survivals <- nrow(survivals.data)*survivals.weights
  prob <- 1-((mortalities/(survivals+mortalities)))
  probs <- c(probs,prob)
}
(1-prod(probs))*1000
