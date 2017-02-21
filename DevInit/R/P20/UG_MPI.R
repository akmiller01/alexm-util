# 
# ** Uganda 2011 **
#   
#   ** HH DEPRIVATIONS **
#   
#   /* REFORMULATED MPI - MAY 7 2013 */
#   
#   /*	A household is deprived in LIVING STANDARDS if it
# 
# a.	Does not have electricity or 
# Has electricity but neither has a television nor a refrigerator
# b.	Does not use improved drinking water sources (MDG indicator 7.8)
# c.	Does not use improved sanitation (MDG indicator 7.9)
# d.	Uses solid fuel for cooking and heating (non-MDG indicator related to target 7)
# e.	Does not have the finished floor (non-MDG indicator related to Target 7).
# Weighting: each indicator is weighted by 1/5
# A household is poor in Living standards if the sum of weighted deprivations is 1/3 or more. */
#   
#   clear
# set more off
# clear matrix
# set maxvar 7000
# cd "C:\Users\cecilia.calderon\HDRO_MCC\MPI\MPI_new calculations\Uganda 2011_DHS\"\
library(foreign)
library(data.table)
library(Hmisc)
# use "UGHR60FL.DTA", clear
dat <- read.dta("D:/Documents/Data/DHSauto/ughr60dt/UGHR60FL.dta",convert.factors=FALSE)
# 
# gen wealthi = hv270
dat$wealthi = dat$hv270
# gen wealths = hv271/100000
dat$wealths = dat$hv271/100000
# gen weight = hv005/1000000
dat$weight = dat$hv005/1000000
# 
# gen hhmembers_hh = hv009
dat$hhmembers_hh = dat$hv009
# 
# gen urban = 1 if hv025==1
dat$urban <- NA
dat$urban[which(dat$hv025==1)] <- 1
# replace urban = 0 if hv025==2
dat$urban[which(dat$hv025==2)] <- 0
# 
# gen electricity = hv206 if hv206==0 | hv206==1
dat$electricity <- 1
dat$electricity[which(dat$hv206==0)] <- 0
# 
# gen electricity_depriv = 1 if electricity==0
dat$electricity_depriv <- NA
dat$electricity_depriv[which(dat$electricity==0)] <- 1
# replace electricity_depriv = 0 if electricity==1
dat$electricity_depriv[which(dat$electricity==1)] <- 0

# 
# gen sanitation = 0 if hv205==22 | hv205==24 | hv205==31 | hv205==96
dat$sanitation <- NA
dat$sanitation[which(dat$hv205 %in% c(22,24,31,96))] <- 0
# replace sanitation = 1 if hv205==11 | hv205==21 | hv205==23 | hv205==25 | hv205==41 | hv205==44
dat$sanitation[which(dat$hv205 %in% c(11,21,23,25,41,44))] <- 1

# replace sanitation = 0 if hv225==1 /*shared*/
dat$sanitation[which(dat$hv225==1)] <- 0
# 
# /*         flush or pour flush toilet |        302        3.34        3.34 11 y
# ventilated improved pit latrine (vip) |        618        6.84       10.18 21 y
# covered pit latrine no slab |      3,442       38.10       48.29 22 n
# covered pit latrine with slab |      2,083       23.06       71.35 23 y
# uncovered pit latrine no slab |      1,115       12.34       83.69 24 n
# uncovered pit latrine with slab |        260        2.88       86.57 25 y
# no facility/bush/field |      1,147       12.70       99.27 31 n
# composting toilet |         24        0.27       99.54 41 y
# ecosan |          9        0.10       99.63 44 y
# other |         30        0.33       99.97 96 n
# 99 |          3        0.03      100.00  */
# 
# 
# gen water = 1 if hv201==11 | hv201==12 | hv201==13 | hv201==22 | hv201==23 | hv201==33 | hv201==34 | hv201==51
dat$water <- NA
dat$water[which(dat$hv201 %in% c(11,12,13,22,23,33,34,51))] <- 1
# replace water = 0 if hv201==35 | hv201==36 | hv201==44 | hv201==45 | hv201==46 | hv201==61 | hv201==62 | hv201==71 | hv201==96
dat$water[which(dat$hv201 %in% c(35,36,44,45,46,61,62,71,96))] <- 0
# replace water=0 if (hv204>=30 & hv204<=900) | ((hv204==998 | hv204==999) & (hv201==13 | hv201==22 | hv201==23 | hv201==33 | hv201==34 | hv201==51))
dat$water[which( (dat$hv201 %in% c(13,22,23,33,34,51) | (dat$hv204>=30 & dat$hv204<=900)) )] <- 0
# 
# /*                  piped into dwelling |        267        2.96        2.96 11 y
# piped to yard/plot |        511        5.66        8.61 12 y
# public tap/standpipe |      1,441       15.95       24.57 13 y
# borehole in yard/plot |         73        0.81       25.37 22 y
# public borehole |      3,417       37.83       63.20 23 y
# protected well/spring in yard/plot |         46        0.51       63.71 33 y
# protected public well/spring |        729        8.07       71.78 34 y
# unprotected well/spring in yard/plot |         60        0.66       72.45 35 n
# unprotected public well/spring |      1,099       12.17       84.61 36 n
# river/stream |        579        6.41       91.02 44 n
# pond/lake |        333        3.69       94.71 45 n
# dam |        107        1.18       95.89 46 n
# rainwater |         99        1.10       96.99 51 y
# tanker truck |          7        0.08       97.07 61 n
# vendor: cart with small tank |         93        1.03       98.10 62 n
# bottled water |        111        1.23       99.32 71 n
# other |         61        0.68      100.00 96 n */
# 
# 
# gen floor = 1 if hv213==31 | hv213==32 | hv213==33 | hv213==34 | hv213==35 | hv213==36
dat$floor <- NA
dat$floor[which(dat$hv213 %in% c(31,32,33,34,35,36))] <- 1
# replace floor = 0 if hv213==11 | hv213==12 | hv213==96
dat$floor[which(dat$hv213 %in% c(11,12,96))] <- 0

# 
# gen cookingfuel = 0 if (hv226>=6 &  hv226<=11) | hv226==95 | hv226==96
dat$cookingfuel <- NA
dat$cookingfuel[which(( (dat$hv226>=6 &  dat$hv226<=11) | dat$hv226==95 | dat$hv226==96) )] <- 0
# replace cookingfuel = 1 if hv226>=1 & hv226<=5
dat$cookingfuel[which(dat$hv226>=1 & dat$hv226<=5)] <- 1
# 
# 
# * ASSETS *
# /* Household is not deprived in assets if it owns 
# 
# at least one of the assets for access to information (phone (mobile or fixed), radio, TV) AND
# 
# either one asset for easy mobility (bicycle, motorbike, motorboat, car, truck or animal wheel cart)
# 
# OR one asset for livelihood (refrigerator, agricultural land or livestock (at least one cattle or at least one horse or at least two goats or at least two sheep, or at least 10 chicken) */
# 
# 
# foreach var in hv207 hv208 hv221 hv210 hv211 hv209 hv212 hv243a hv243d hv243c hv244 {
# replace `var'=. if `var'==9
# }
for(var in c("hv207","hv208","hv221","hv210","hv211","hv209","hv212","hv243a","hv243d","hv243c","hv244")){
  dat[,var][which(dat[,var]==9)] <- NA
}
# 
# 
# foreach var in hv246a hv246b hv246c hv246d hv246e hv246f {
# replace `var'=. if `var'>=98
# }
for(var in c("hv246a","hv246b","hv246c","hv246d","hv246e","hv246f")){
  dat[,var][which(dat[,var]>=98)] <- NA
}
# 
# 
# *****************************************
# ** BOAT AND CATTLE ARE N/A FOR Uganda **
# *****************************************
# 
# * Information *
# gen phone = 0 if hv221==0 & hv243a==0 
# replace phone = 1 if hv221==1 | hv243a==1 /* cell phone or landline */
dat$phone <- NA
dat$phone[which(dat$hv221==0 & dat$hv243a==0)] <- 0
dat$phone[which(dat$hv221==1 | dat$hv243a==1)] <- 1
# 
# gen radio = 0 if hv207==0
# replace radio = 1 if hv207==1
dat$radio = NA
dat$radio[which(dat$hv207==0)] <- 0
dat$radio[which(dat$hv207==1)] <- 1

# 
# gen tv = 0 if hv208==0
# replace tv = 1 if hv208==1
dat$tv = NA
dat$tv[which(dat$hv208==0)] <- 0
dat$tv[which(dat$hv208==1)] <- 1
# 
# * Mobility *
# gen bicycle = 0 if hv210==0
# replace bicycle = 1 if hv210==1
dat$bicycle = NA
dat$bicycle[which(dat$hv210==0)] <- 0
dat$bicycle[which(dat$hv210==1)] <- 1
# 
# gen motorcycle = 0 if hv211==0
# replace motorcycle = 1 if hv211==1
dat$motorcycle = NA
dat$motorcycle[which(dat$hv211==0)] <- 0
dat$motorcycle[which(dat$hv211==1)] <- 1

# 
# gen motorboat = 0 if hv243d==0
# replace motorboat = 1 if hv243d==1
dat$motorboat = NA
dat$motorboat[which(dat$hv243d==0)] <- 0
dat$motorboat[which(dat$hv243d==1)] <- 1
# 
# gen car_truck = 0 if hv212==0
# replace car_truck = 1 if hv212==1
dat$car_truck = NA
dat$car_truck[which(dat$hv212==0)] <- 0
dat$car_truck[which(dat$hv212==1)] <- 1
# 
# gen animal_cart = 0 if hv243c==0
# replace animal_cart = 1 if hv243c==1
dat$animal_cart = NA
dat$animal_cart[which(dat$hv243c==0)] <- 0
dat$animal_cart[which(dat$hv243c==1)] <- 1
# 
# * Livelihood *
# gen refrigerator = 0 if hv209==0
# replace refrigerator = 1 if hv209==1
dat$refrigerator = NA
dat$refrigerator[which(dat$hv209==0)] <- 0
dat$refrigerator[which(dat$hv209==1)] <- 1
# 
# gen land = 0 if hv244==0
# replace land = 1 if hv244==1
dat$land = NA
dat$land[which(dat$hv244==0)] <- 0
dat$land[which(dat$hv244==1)] <- 1
# 
# 
# *gen cattle = 0 if hv246a==0 & hv246b==0
# *replace cattle = 1 if (hv246a>0 & hv246a<=95) | (hv246b>0 & hv246b<=95)
# dat$cattle = NA
# dat$cattle[which(dat$hv246a==0 & dat$hv246b==0)] <- 0
# dat$cattle[which((dat$hv246a>0 & dat$hv246a<=95) | (dat$hv246b>0 & dat$hv246b<=95))] <- 1
# 
# gen cattle = 0 if hv246a==0
# replace cattle = 1 if hv246a>0 & hv246a<=95
dat$cattle = NA
dat$cattle[which(dat$hv246a==0)] <- 0
dat$cattle[which(dat$hv246a>0 & dat$hv246a<=95)] <- 1
# 
# gen horses = 0 if hv246c==0
# replace horses = 1 if (hv246c>0 & hv246c<=95)
dat$horses = NA
dat$horses[which(dat$hv246c==0)] <- 0
dat$horses[which(dat$hv246c>0 & dat$hv246c<=95)] <- 1
# 
# gen goats = 0 if hv246d<2
# replace goats = 1 if (hv246d>=2 & hv246d<=95)
dat$goats = NA
dat$goats[which(dat$hv246d<2)] <- 0
dat$goats[which(dat$hv246d>=2 & dat$hv246d<=95)] <- 1
# 
# gen sheeps = 0 if hv246e<2
# replace sheeps = 1 if (hv246e>=2 & hv246e<=95)
dat$sheeps = NA
dat$sheeps[which(dat$hv246e<2)] <- 0
dat$sheeps[which(dat$hv246e>=2 & dat$hv246e<=95)] <- 1
# 
# gen chicken = 0 if hv246f<10
# replace chicken = 1 if (hv246f>=10 & hv246f<=95)
dat$chicken = NA
dat$chicken[which(dat$hv246f<10)] <- 0
dat$chicken[which(dat$hv246f>=10 & dat$hv246f<=95)] <- 1
# 
# 
# egen information_miss = rowmiss(phone radio tv)
psum <- function(...,na.rm=FALSE) { 
  rowSums(do.call(cbind,list(...)),na.rm=na.rm) } 
dat$information_miss = psum(is.na(dat$phone),is.na(dat$radio),is.na(dat$tv))
# egen mobility_miss = rowmiss(bicycle motorcycle motorboat car_truck animal_cart)
dat$mobility_miss = psum(is.na(dat$bicycle),is.na(dat$motorcycle),is.na(dat$motorboat),is.na(dat$car_truck),is.na(dat$animal_cart))
# egen livelihood_miss = rowmiss(refrigerator land cattle horses goats sheeps chicken)
dat$livelihood_miss = psum(is.na(dat$refrigerator),is.na(dat$land),is.na(dat$cattle),is.na(dat$horses),is.na(dat$goats),is.na(dat$sheeps),is.na(dat$chicken))

# 
# 
# gen information_depriv = . if information_miss==2 | information_miss==3
dat$information_depriv = NA
# replace information_depriv = 0 if phone==1 | radio==1 | tv==1
dat$information_depriv[which((dat$phone==1 | dat$radio==1 | dat$tv==1) & dat$information_miss<2)] <- 0
# replace information_depriv = 1 if phone==0 & radio==0 & tv==0
dat$information_depriv[which(psum(dat$phone,dat$radio,dat$tv,na.rm=TRUE)==0 & dat$information_miss<2)] <- 1
# 
# 
# gen mobility_depriv = . if mobility_miss==3 | mobility_miss==4 | mobility_miss==5
dat$mobility_depriv = NA
# replace mobility_depriv = 0 if bicycle==1 | motorcycle==1 | car_truck==1 | animal_cart==1 | motorboat==1
dat$mobility_depriv[which((dat$bicycle==1 | dat$motorcycle==1 | dat$car_truck==1 | dat$animal_cart==1 | dat$motorboat==1) & dat$mobility_miss<3)] <- 0
# replace mobility_depriv = 1 if bicycle==0 & motorcycle==0 & car_truck==0 & animal_cart==0 & motorboat==0
dat$mobility_depriv[which(psum(dat$bicycle,dat$motorcycle,dat$car_truck,dat$animal_car,dat$motorboat,na.rm=TRUE)==0 & dat$mobility_miss<3)] <- 1
# 
# 
# gen livelihood_depriv = . if livelihood_miss==4 | livelihood_miss==5 | livelihood_miss==6 | livelihood_miss==7
dat$livelihood_depriv = NA
# replace livelihood_depriv = 0 if refrigerator== 1 | land==1 | cattle==1 | horses==1 | goats==1 | sheeps==1 | chicken==1
dat$livelihood_depriv[which((dat$refrigerator== 1 | dat$land==1 | dat$cattle==1 | dat$horses==1 | dat$goats==1 | dat$sheeps==1 | dat$chicken==1) & dat$livelihood_miss<4)] <- 0
# replace livelihood_depriv = 1 if refrigerator==0 & land==0 & cattle==0 & horses==0 & goats==0 & sheeps==0 & chicken==0
dat$livelihood_depriv[which(psum(dat$refrigerator,dat$land,dat$cattle,dat$horses,dat$goats,dat$sheeps,dat$chicken,na.rm=TRUE)==0 & dat$livelihood_miss<4)] <- 1
# 
# 
# gen asset_depriv = . if information_depriv==. | (information_depriv==1 & mobility_depriv==0 & livelihood_depriv==.) | (information_depriv==1 & mobility_depriv==. & livelihood_depriv==0)
dat$asset_depriv = NA
# replace asset_depriv = 0 if information_depriv==0 & (mobility_depriv==0 | livelihood_depriv==0)
dat$asset_depriv[which(dat$information_depriv==0 & (dat$mobility_depriv==0 | dat$livelihood_depriv==0))] <- 0
# replace asset_depriv = 1 if information_depriv==1 | (information_depriv==0 & mobility_depriv==1 & livelihood_depriv==1)
dat$asset_depriv[which(dat$information_depriv==1 | (dat$information_depriv==0 & dat$mobility_depriv==1 & dat$livelihood_depriv==1))] <- 1
dat$asset_depriv[which(
  is.na(dat$information_depriv) |
  (dat$information_depriv==1 & dat$mobility_depriv==0 & is.na(dat$livelihood_depriv)) |
  (dat$information_depriv==1 & is.na(dat$mobility_depriv) & dat$livelihood_depriv==0)
  )] <- NA
# 
# 
# 
# foreach var in sanitation water floor cookingfuel {
for(var in c("sanitation","water","floor","cookingfuel")){
# 
# gen `var'_depriv=1 if `var'==0
  varname <- paste0(var,"_depriv")
  dat[,varname] <- NA
  dat[,varname][which(dat[,var]==0)] <- 1
# replace `var'_depriv=0 if `var'==1
  dat[,varname][which(dat[,var]==1)] <- 0
# }
}
# 
# egen depriv = rsum(electricity_depriv sanitation_depriv water_depriv floor_depriv cookingfuel_depriv asset_depriv)
dat$depriv = psum(dat$electricity_depriv,dat$sanitation_depriv,dat$water_depriv,dat$floor_depriv,dat$cookingfuel_depriv,dat$asset_depriv,na.rm=TRUE)
# replace depriv = . if electricity_depriv==. & sanitation_depriv==. & water_depriv==. & floor_depriv==. & cookingfuel_depriv==. & asset_depriv==.
dat$depriv[which(
  is.na(dat$electricity_depriv) &
  is.na(dat$sanitation_depriv) &
  is.na(dat$water_depriv) &
  is.na(dat$floor_depriv) &
  is.na(dat$cookingfuel_depriv) &
  is.na(dat$asset_depriv)
)] <- NA
# 
# gen ls_sample=1 if electricity_depriv<. & sanitation_depriv<. & water_depriv<. & floor_depriv<. & cookingfuel_depriv<. & asset_depriv<.
dat$ls_sample = NA
dat$ls_sample[which(
  !is.na(dat$electricity_depriv) &
  !is.na(dat$sanitation_depriv) &
  !is.na(dat$water_depriv) &
  !is.na(dat$floor_depriv) &
  !is.na(dat$cookingfuel_depriv) &
  !is.na(dat$asset_depriv)
)] <- 1
# egen livings_miss = rowmiss(electricity_depriv sanitation_depriv water_depriv floor_depriv cookingfuel_depriv asset_depriv)
dat$livings_miss <- NA
dat$livings_miss = psum(
  is.na(dat$electricity_depriv),
  is.na(dat$sanitation_depriv),
  is.na(dat$water_depriv),
  is.na(dat$floor_depriv),
  is.na(dat$cookingfuel_depriv),
  is.na(dat$asset_depriv)
)
# 
# keep if hv015==1 /* completed HH interview */
dat <- dat[which(dat$hv015==1),]
# sort hv001 hv002
dat <- dat[order(dat$hv001,dat$hv002),]
# 
# 
# keep wealthi wealths weight hhmembers_hh urban electricity electricity_depriv water sanitation cookingfuel floor sanitation_depriv water_depriv floor_depriv cookingfuel_depriv depriv ls_sample livings_miss hv001 hv002 phone radio tv bicycle motorcycle motorboat car_truck animal_cart refrigerator land cattle horses goats sheeps chicken information_miss mobility_miss livelihood_miss information_depriv mobility_depriv livelihood_depriv asset_depriv
keep <- c("wealthi", "wealths", "weight", "hhmembers_hh", "urban", "electricity", "electricity_depriv", "water", "sanitation", "cookingfuel", "floor", "sanitation_depriv", "water_depriv", "floor_depriv", "cookingfuel_depriv", "depriv", "ls_sample", "livings_miss", "hv001", "hv002", "phone", "radio", "tv", "bicycle", "motorcycle", "motorboat", "car_truck", "animal_cart", "refrigerator", "land", "cattle", "horses", "goats", "sheeps", "chicken", "information_miss", "mobility_miss", "livelihood_miss", "information_depriv", "mobility_depriv", "livelihood_depriv", "asset_depriv")
dat <- dat[keep]
hh <- dat
# 
# 
# save "HH_MPI2_MCC.dta", replace
remove(dat)
# 
# 
# * BIRTHs' QUESTIONNAIRE TO CALCULATE WHEN THE DEATH OF A CHILD HAPPENED *
# 
# use "UGBR60FL.DTA", clear
dat <- read.dta("D:/Documents/Data/DHSauto/ugbr60dt/UGBR60FL.dta",convert.factors=FALSE)

# 
# sort b3
dat <- dat[order(dat$b3),]
# 
# 
# gen usual_res=(v135==1)
dat$usual_res <- psum(dat$v135==1)
# drop if usual_res==0
dat <- dat[which(!dat$usual_res==0),]
# 
# 
# gen date_death = b3+b7
dat$date_death = dat$b3+dat$b7
# 
# gen mdead_from_survey = v008-date_death
dat$mdead_from_survey = dat$v008-dat$date_death
# 
# gen ydead_from_survey = mdead_from_survey/12
dat$ydead_from_survey = dat$mdead_from_survey/12
# 
# 
# gen y = ydead_from_survey if b7<=60
dat$y <- NA
dat$y[which(dat$b7<=60)] <- dat$ydead_from_survey[which(dat$b7<=60)]
# 
# label var ydead_from_survey "# years from survey that a child died"
# label var y "# years from survey that a child<=5y died"
# 
# gen b5r=0 if b5==1
dat$b5r <- NA
dat$b5r[which(dat$b5==1)] <- 0
# replace b5r=1 if b5==0
dat$b5r[which(dat$b5==0)] <- 1

# 
# egen child_died=sum(b5r), by(v001 v002 v003)
child_died <- data.table(dat)[,.(child_died=sum(b5r,na.rm=TRUE)),by=.(v001,v002,v003)]
dat <- merge(dat,child_died,by=c("v001","v002","v003"),all.x=TRUE)
# egen child_died2=rsum(v206 v207)
dat$child_died2 <- psum(dat$v206,dat$v207,na.rm=TRUE)
# 
# compare child_died child_died2
# * they are identical *
#   
#   
#   egen child_died5=sum(b5r) if ydead_from_survey<=5, by(v001 v002 v003)
child_died5 <- data.table(subset(dat,ydead_from_survey<=5))[,.(child_died5=sum(b5r,na.rm=TRUE)),by=.(v001,v002,v003)]
dat <- merge(dat,child_died5,by=c("v001","v002","v003"),all.x=TRUE)
# * only deaths in the past 5 years *
#   replace child_died5=0 if child_died5==. & child_died>=0 & child_died<.
dat$child_died5[which(is.na(dat$child_died5) & dat$child_died>=0 & !is.na(dat$child_died))] <- 0
# replace child_died5=. if b5r==1 & ydead_from_survey==.
dat$child_died5[which(dat$b5r==1 & is.na(dat$ydead_from_survey))] <- NA
# 
# sort v001 v002 v003 ydead_from_survey
dat <- dat[order(dat$v001,dat$v002,dat$v003,dat$ydead_from_survey),]
# bys v001 v002 v003: gen uno=1 if _n==1
dat$uno <- !duplicated( dat[,c("v001","v002","v003")])
# keep if uno==1
dat <- subset(dat,uno==TRUE)
# drop uno
dat$uno <- NULL
# 
# tab child_died child_died5,m
table(dat$child_died,dat$child_died5)
# 
# egen child_diedhh=sum(child_died), by(v001 v002)
child_diedhh <- data.table(dat)[,.(child_diedhh=sum(child_died,na.rm=TRUE)),by=.(v001,v002)]
dat <- merge(dat,child_diedhh,by=c("v001","v002"),all.x=TRUE)
# *egen child_died2hh=sum(child_died2), by(v001 v002)
# egen child_died5hh=sum(child_died5), by(v001 v002)
child_died5hh <- data.table(dat)[,.(child_died5hh=sum(child_died5,na.rm=TRUE)),by=.(v001,v002)]
dat <- merge(dat,child_died5hh,by=c("v001","v002"),all.x=TRUE)
# 
# 
# gen uno=1
dat$uno <- 1
# egen nwomen_birth_15_49=sum(uno), by(v001 v002)
nwomen_birth_15_49 <- data.table(dat)[,.(nwomen_birth_15_49=sum(uno,na.rm=TRUE)),by=.(v001,v002)]
dat <- merge(dat,nwomen_birth_15_49,by=c("v001","v002"),all.x=TRUE)
# 
# drop uno
dat$uno <- NULL
# bys v001 v002: gen uno=1 if _n==1
dat$uno <- !duplicated( dat[,c("v001","v002")])
# keep if uno==1
dat <- subset(dat,uno==TRUE)
# drop uno
dat$uno <- NULL
# 
# 
# keep child_died5 v001 v002 nwomen_birth_15_49 child_died5hh child_diedhh
keep <- c("child_died5", "v001", "v002", "nwomen_birth_15_49", "child_died5hh", "child_diedhh")
dat <- dat[keep]
# 
# ren v001 hv001
names(dat)[which(names(dat)=="v001")] <- "hv001"
# ren v002 hv002
names(dat)[which(names(dat)=="v002")] <- "hv002"
# 
# sort hv001 hv002
dat <- dat[order(dat$hv001,dat$hv002),]
# 
# save "child_death_MPI2_MCC.dta", replace
br <- dat
# 
remove(child_died,child_died5,child_died5hh,child_diedhh,nwomen_birth_15_49,dat)
# 
# 
# * WOMEN'S QUESTIONNAIRE (15-49 y) *
# 
# use "UGIR60FL.DTA", clear
dat <- read.dta("D:/Documents/Data/DHSauto/ugir60dt/UGIR60FL.dta",convert.factors=FALSE)

# 
# gen usual_res=(v135==1) /* keep only usual residents, drop visitors */
dat$usual_res <- dat$v135==1
# drop if usual_res==0
dat <- subset(dat,usual_res==TRUE)
# 
# egen children_died=rsum(v206 v207)
dat$children_died <- psum(dat$v206,dat$v207,na.rm=TRUE)
# egen children_diedhh=sum(children_died), by(v001 v002)
children_diedhh <- data.table(dat)[,.(children_diedhh=sum(children_died,na.rm=TRUE)),by=.(v001,v002)]
dat <- merge(dat,children_diedhh,by=c("v001","v002"),all.x=TRUE)
remove(children_diedhh)
# 
# 
# gen uno=1
dat$uno <- 1
# egen nwomen15_49=sum(uno), by(v001 v002)
nwomen15_49 <- data.table(dat)[,.(nwomen15_49=sum(uno)),by=.(v001,v002)]
dat <- merge(dat,nwomen15_49,by=c("v001","v002"),all.x=TRUE)

# 
# drop uno
dat$uno <- NULL
remove(nwomen15_49)
# 
# 
# sort v001 v002 v003
dat <- dat[order(dat$v001,dat$v002,dat$v003),]
# bys v001 v002: gen uno=1 if _n==1
dat$uno <- !duplicated( dat[,c("v001","v002")])
# keep if uno==1
dat <- subset(dat,uno==TRUE)
# drop uno
dat$uno <- NULL
# 
# ren v001 hv001
names(dat)[which(names(dat)=="v001")] <- "hv001"
# ren v002 hv002
names(dat)[which(names(dat)=="v002")] <- "hv002"
# ren v003 hv003
names(dat)[which(names(dat)=="v003")] <- "hv003"
# 
# keep hv001 hv002 children_diedhh nwomen15_49
keep <- c("hv001","hv002","children_diedhh","nwomen15_49")
dat <- dat[keep]
# 
# sort hv001 hv002
dat <- dat[order(dat$hv001,dat$hv002),]
# save "UGIR60FL_MCC.dta", replace
ir <- dat
remove(dat)
# 
# * MEN'S QUESTIONNAIRE (15-59 y) *
#   
#   /*use "UGMR60FL.DTA", clear
if(!file_test(op="-f", "D:/Documents/Data/DHSauto/ugmr60dt/UGMR60FL.dta")){message("No MR, skipping...");}else{
  dat <- read.dta("D:/Documents/Data/DHSauto/ugmr60dt/UGMR60FL.dta",convert.factors=FALSE)

# 
# * keep only usual residents, drop visitors
# gen usual_res=(mv135==1) * keep only usual residents, drop visitors
  dat$usual_res <- dat$mv135==1
# drop if usual_res==0
  dat <- subset(dat,usual_res==TRUE)
# 
# egen mchildren_died=rsum(mv206 mv207)
  dat$mchildren_died=psum(dat$mv206,dat$mv207,na.rm=TRUE)
# egen mchildren_diedhh=sum(mchildren_died), by(mv001 mv002)
  mchildren_diedhh <- data.table(dat)[,.(mchildren_diedhh=sum(mchildren_died,na.rm=TRUE)),by=.(mv001,mv002)]
  dat <- merge(dat,mchildren_diedhh,by=c("mv001","mv002"),all.x=TRUE)
  remove(mchildren_diedhh)
# 
# sort mv001 mv002 mv003
  dat <- dat[order(dat$mv001,dat$mv002,dat$mv003),]
# bys mv001 mv002: gen uno=1 if _n==1
  dat$uno <- !duplicated( dat[,c("mv001","mv002")])
# keep if uno==1
  dat <- subset(dat,uno==TRUE)
# drop uno
  dat$uno <- NULL
# 
# ren mv001 hv001  
  names(dat)[which(names(dat)=="mv001")] <- "hv001"
# ren mv002 hv002
  names(dat)[which(names(dat)=="mv002")] <- "hv002"
# ren mv003 hv003
  names(dat)[which(names(dat)=="mv003")] <- "hv003"
# 
# keep hv001 hv002 mchildren_diedhh
  keep <- c("hv001","hv002","mchildren_diedhh")
  dat <- dat[keep]
# 
# sort hv001 hv002
  dat <- dat[order(dat$hv001,dat$hv002),]
# save "UGMR60FL_MCC.dta", replace
  mr <- dat
# */
}
#   
#   
#   * INDIVIDUALS' QUESTIONNAIRE *
# 
# use "UGPR60FL.DTA", clear
dat <- read.dta("D:/Documents/Data/DHSauto/ugpr60dt/UGPR60FL.dta",convert.factors=FALSE)

# 
# sort hv001 hv002 hv003
# merge hv001 hv002 using "UGIR60FL_MCC.dta"
dat$master <- 1
dat$women <- 1
dat <- merge(dat,ir,by=c("hv001","hv002"),all.x=TRUE)
dat$merge_women <- NA
dat$merge_women[which(!is.na(dat$master) & is.na(dat$women))] <- 1
dat$merge_women[which(is.na(dat$master) & !is.na(dat$women))] <- 2
dat$merge_women[which(!is.na(dat$master) & !is.na(dat$women))] <- 3
dat$master <- NULL
dat$women <- NULL
# tab _merge
# ren _merge merge_women
# 
# /*sort hv001 hv002 hv003
# merge hv001 hv002 using "UGMR60FL_MCC.dta"
if(exists("mr")){
  dat <- merge(dat,mr,by=c("hv001","hv002"),all.x=TRUE)
}
# tab _merge
# ren _merge merge_men*/
# 
# sort hv001 hv002
# merge hv001 hv002 using "HH_MPI2_MCC.dta"
dat <- merge(dat,hh,by=c("hv001","hv002"),all.x=TRUE)
# tab _merge
# drop _merge
# 
# 
# sort hv001 hv002
# merge hv001 hv002 using "child_death_MPI2_MCC.dta"
dat <- merge(dat,br,by=c("hv001","hv002"),all.x=TRUE)
# tab _merge
# ren _merge merge_births
# 
# replace child_died5hh=0 if children_diedhh==0 & child_died5hh==.
dat$child_died5hh[which(dat$children_diedhh==0 & is.na(dat$child_died5hh))] <- 0
# replace child_diedhh=0 if children_diedhh==0 & child_diedhh==.
dat$child_diedhh[which(dat$children_diedhh==0 & is.na(dat$child_diedhh))] <- 0

# 
# 
# 
# gen age = hv105 if hv105<98
dat$age <- dat$hv105
dat$age[which(dat$hv105>=98)] <- NA
# gen sex = 1 if hv104==1
dat$sex <- NA
dat$sex[which(dat$hv104==1)] <- 1
# replace sex = 0 if hv104==2
dat$sex[which(dat$hv104==2)] <- 0
# egen hh=group(hv001 hv002)
dat$hh <- paste(dat$hv001,dat$hv002)
# 
# gen uno=1
# egen hhmembers=sum(uno), by(hh)
# compare hhmembers hv009
# drop hhmembers uno
# 
# gen usual_res=(hv102==1)
dat$usual_res <- dat$hv102==1
# drop if usual_res==0
dat <- subset(dat,usual_res==TRUE)
# 
# gen uno=1
dat$uno <- 1
# egen hhmembers=sum(uno), by(hh)
hhmembers <- data.table(dat)[,.(hhmembers=sum(uno)),by=.(hh)]
dat <- merge(dat,hhmembers,by="hh",all.x=TRUE)
remove(hhmembers)
# 
# label var hhmembers "Number of HH members (only usual residents, excludes visitors)"
# label var hhmembers_hh "Number of HH members (usual residents + visitors)"
# 
# drop uno
dat$uno <- NULL
# 
# compress
# sort hv001 hv002 hvidx
dat <- dat[order(dat$hv001,dat$hv002,dat$hvidx),]
# save "Uganda_MPI2_2011.dta", replace
# 
# * MPI CALCULATION *
# 
# *** A- EDUCATION DIMENSION ***
# 
# /* For the indicator on years of education, if we observe at least one member with five or more years of  education then, regardless of the number of other members with missing data, we classify the household as non-deprived. If more than 1/3 of the household members have missing information on years of education, and the people for which we observe the years of education have less than five years, the household is given a missing value in this indicator. If we have information of 2/3 (or more) of household members, and these report less than five years of education, the household will be classified as deprived. 
# 
# For the school attendance indicator, if all school-aged children in  a household have missing information in enrolment, that value is considered missing. As long as we have information for one of the children in the household, the household will be classified as non-deprived or deprived depending on whether that child is reported to be attending school or not. */
# 
# * http://stats.uis.unesco.org/unesco/TableViewer/tableView.aspx?ReportId=163
# * Entrance age of primary: 6y
# * Duration of primary: 7y
# * Entrance age of lower secondary: 13y
# * Durantion lower secondary: 4y
# * Entrance age high secondary: 17y
# * Duration high secondary: 2y
# 
# ** 1- DEPRIVED IN EDUCATION **
# 
# gen yschooling = 0 if hv106==0 | (hv106==1 & hv107==0)
dat$yschooling <- NA
dat$yschooling[which(dat$hv106==0 | (dat$hv106==1 & dat$hv107==0))] <- 0
# replace yschooling =1 if hv106==1 & hv107==1
dat$yschooling[which(dat$hv106==1 & dat$hv107==1)] <- 1
# replace yschooling =2 if hv106==1 & hv107==2
dat$yschooling[which(dat$hv106==1 & dat$hv107==2)] <- 2
# replace yschooling =3 if hv106==1 & hv107==3
dat$yschooling[which(dat$hv106==1 & dat$hv107==3)] <- 3
# replace yschooling =4 if hv106==1 & hv107==4
dat$yschooling[which(dat$hv106==1 & dat$hv107==4)] <- 4
# replace yschooling =5 if hv106==1 & hv107==5
dat$yschooling[which(dat$hv106==1 & dat$hv107==5)] <- 5
# replace yschooling =6 if hv106==1 & hv107==6
dat$yschooling[which(dat$hv106==1 & dat$hv107==6)] <- 6
# replace yschooling =7 if hv106==1 & hv107==7
dat$yschooling[which(dat$hv106==1 & dat$hv107==7)] <- 7
# 
# replace yschooling =8 if hv106==2 & hv107==1
dat$yschooling[which(dat$hv106==2 & dat$hv107==1)] <- 8
# replace yschooling =9 if hv106==2 & hv107==2
dat$yschooling[which(dat$hv106==2 & dat$hv107==2)] <- 9
# replace yschooling =10 if hv106==2 & hv107==3
dat$yschooling[which(dat$hv106==2 & dat$hv107==3)] <- 10
# replace yschooling =11 if hv106==2 & hv107==4
dat$yschooling[which(dat$hv106==2 & dat$hv107==4)] <- 11
# replace yschooling =12 if hv106==2 & hv107==5
dat$yschooling[which(dat$hv106==2 & dat$hv107==5)] <- 12
# replace yschooling =13 if hv106==2 & hv107==6
dat$yschooling[which(dat$hv106==2 & dat$hv107==6)] <- 13
# 
# replace yschooling =14 if hv106==3 & hv107==1
dat$yschooling[which(dat$hv106==3 & dat$hv107==1)] <- 14
# replace yschooling =15 if hv106==3 & hv107==2
dat$yschooling[which(dat$hv106==3 & dat$hv107==2)] <- 15
# replace yschooling =16 if hv106==3 & hv107==3
dat$yschooling[which(dat$hv106==3 & dat$hv107==3)] <- 16
# replace yschooling =17 if hv106==3 & hv107==4
dat$yschooling[which(dat$hv106==3 & dat$hv107==4)] <- 17
# replace yschooling =18 if hv106==3 & hv107==5
dat$yschooling[which(dat$hv106==3 & dat$hv107==5)] <- 18
# 
# replace yschooling =7 if hv106==2 & hv107==0
dat$yschooling[which(dat$hv106==2 & dat$hv107==0)] <- 7
# replace yschooling =13 if hv106==3 & hv107==0
dat$yschooling[which(dat$hv106==3 & dat$hv107==0)] <- 13
# 
# replace yschooling=. if age<5
dat$yschooling[which(dat$age<5)] <- NA
# 
# gen uno14=1 if age>=12 & age<. /* Number of HH members 12y or older */
dat$uno14 <- NA
dat$uno14[which(dat$age>=12 & !is.na(dat$age))] <- 1
# bys hhid: egen hhmember14 = sum(uno14)
hhmember14 <- data.table(dat)[,.(hhmember14=sum(uno14)),by=.(hhid)]
dat <- merge(dat,hhmember14,by="hhid",all.x=TRUE)
remove(hhmember14)
# 
# ** 1- DEPRIVED IN EDUCATION **
# 
# gen yschoolingi=0 if yschooling<6 & age>=12 & age<.
dat$yschoolingi <- NA
dat$yschoolingi[which(dat$yschooling<6 & dat$age>=10 & !is.na(dat$age))] <- 0
# replace yschoolingi=1 if yschooling>=6 & yschooling<.
dat$yschoolingi[which(dat$yschooling>=6 & !is.na(dat$yschooling))] <- 1
# replace yschoolingi=1 if yschooling==. & ((hv106==2 | hv106==3) & (hv107==98 | hv107==99))
dat$yschoolingi[which(is.na(dat$yschooling) & ((dat$hv106==2 | dat$hv106==3) & (dat$hv107==98 | dat$hv107==99)))] <- 1
# 
# 
# gen schooling_missing = 1 if ((hv106==1 & hv107==98) | hv106==8 | hv106==9) & age>=12 & age<.
dat$schooling_missing <- NA
dat$schooling_missing[which(((dat$hv106==1 & dat$hv107==98) | dat$hv106==8 | dat$hv106==9) & dat$age>=12 & !is.na(dat$age))] <- 1
# egen schooling_missinghh=sum(schooling_missing), by(hhid)
schooling_missinghh <- data.table(dat)[,.(schooling_missinghh=sum(schooling_missing,na.rm=TRUE)),by=.(hhid)]
dat <- merge(dat,schooling_missinghh,by="hhid",all.x=TRUE)
remove(schooling_missinghh)
# 
# egen yeduchh=sum(yschoolingi), by(hhid) /* assumes that a member has <6 y schooling when there is missing info on that member's schooling  */
yeduchh <- data.table(dat)[,.(yeduchh=sum(yschoolingi,na.rm=TRUE)),by=.(hhid)]
dat <- merge(dat,yeduchh,by="hhid",all.x=TRUE)
remove(yeduchh)
#   replace yeduchh=. if schooling_missinghh==hhmember14
dat$yeduchh[which(dat$schooling_missinghh==dat$hhmember14)] <- NA
# 
# * DEPRIVED IN EDUCATION INDICATOR *
#   gen educ_depriv=1 if yeduchh==0 
dat$educ_depriv <- NA
dat$educ_depriv[which(dat$yeduchh==0)] <- 1
# replace educ_depriv=0 if yeduchh>=1 & yeduchh<.
dat$educ_depriv[which(dat$yeduchh>=1 & !is.na(dat$yeduchh))] <- 0
# replace educ_depriv=. if yeduchh ==0 & schooling_missinghh>0 & schooling_missinghh>(1/3*hhmember14)
dat$educ_depriv[which(dat$yeduchh ==0 & dat$schooling_missinghh>0 & dat$schooling_missinghh>(1/3*dat$hhmember14))] <- NA
# 
# 
# 
# 
# ** 2- DEPRIVED IN SCHOOL ATTENDANCE 7-14Y (entrance age is 6 but we allow for 1 year of late enrollment) **
#   
#   gen attendance_missing=1 if hv121==9 & age>=7  & age<=14
dat$attendance_missing <- NA
dat$attendance_missing[which(dat$hv121==9 & dat$age>=7  & dat$age<=14)] <- 1
# gen child_7_14=1 if age>=7 & age<=14
dat$child_7_14 <- NA
dat$child_7_14[which(dat$age>=7 & dat$age<=14)] <- 1
# 
# egen attendance_missinghh=sum(attendance_missing), by(hhid)
attendance_missinghh <- data.table(dat)[,.(attendance_missinghh=sum(attendance_missing,na.rm=TRUE)),by=.(hhid)]
dat <- merge(dat,attendance_missinghh,by="hhid",all.x=TRUE)
remove(attendance_missinghh)
# egen child_7_14hh=sum(child_7_14), by(hhid)
child_7_14hh <- data.table(dat)[,.(child_7_14hh=sum(child_7_14,na.rm=TRUE)),by=.(hhid)]
dat <- merge(dat,child_7_14hh,by="hhid",all.x=TRUE)
remove(child_7_14hh)
# 
# gen child_6_14=1 if age>=6 & age<=14
dat$child_6_14 <- NA
dat$child_6_14[which(dat$age>=6 & dat$age<=14)] <- 1
# egen child_6_14hh=sum(child_6_14), by(hhid)
child_6_14hh <- data.table(dat)[,.(child_6_14hh=sum(child_6_14,na.rm=TRUE)),by=.(hhid)]
dat <- merge(dat,child_6_14hh,by="hhid",all.x=TRUE)
remove(child_6_14hh)
# 
# 
# 
# gen attendance_missing_final = 1 if attendance_missinghh==child_7_14hh & child_7_14hh>0 & attendance_missing==1 & child_7_14==1
dat$attendance_missing_final <- NA
dat$attendance_missing_final[which(dat$attendance_missinghh==dat$child_7_14hh & dat$child_7_14hh>0 & dat$attendance_missing==1 & dat$child_7_14==1)] <- 1
# egen attendance_missing_finalhh = sum(attendance_missing_final), by(hhid)
attendance_missing_finalhh <- data.table(dat)[,.(attendance_missing_finalhh=sum(attendance_missing_final,na.rm=TRUE)),by=.(hhid)]
dat <- merge(dat,attendance_missing_finalhh,by="hhid",all.x=TRUE)
remove(attendance_missing_finalhh)
# 
# gen child_noattend=1 if hv121==0 & age>=7 & age<=14 
dat$child_noattend <- NA
dat$child_noattend[which(dat$hv121==0 & dat$age>=7 & dat$age<=14)] <- 1
# replace child_noattend=0 if hv121==2 & age>=7 & age<=14
dat$child_noattend[which(dat$hv121==2 & dat$age>=7 & dat$age<=14)] <- 0
# 
# * DEPRIVED IN SCHOOL ATTENDANCE 7-14Y *
#   egen child_noattendhh=sum(child_noattend), by(hhid)
child_noattendhh <- data.table(dat)[,.(child_noattendhh=sum(child_noattend,na.rm=TRUE)),by=.(hhid)]
dat <- merge(dat,child_noattendhh,by="hhid",all.x=TRUE)
remove(child_noattendhh)
# replace child_noattendhh=1 if child_noattendhh>1 & child_noattendhh<.
dat$child_noattendhh[which(dat$child_noattendhh>1 & !is.na(dat$child_noattendhh))] <- 1
# *replace child_noattendhh=. if attendance_missing_finalhh>=1 & attendance_missing_finalhh<.
# *replace child_noattendhh=. if child_noattendhh==0 & attendance_missing_finalhh>=1 & attendance_missing_finalhh>(1/3*child_7_14hh)
# replace child_noattendhh=. if child_noattendhh==0 & attendance_missinghh>=1 & attendance_missinghh>(1/3*child_7_14hh)
dat$child_noattendhh[which(dat$child_noattendhh==0 & dat$attendance_missinghh>=1 & dat$attendance_missinghh>(1/3*dat$child_7_14hh))] <- NA
# replace child_noattendhh=0 if child_6_14hh>0 & child_6_14hh<. & child_7_14hh==0
dat$child_noattendhh[which(dat$child_6_14hh>0 & !is.na(dat$child_6_14hh) & dat$child_7_14hh==0)] <- 0

# 
# 
# 
# 
# *** B- HEALTH DIMENSION ***
#   
#   /* For the nutritional indicator, in DHS countries, if nutritional information for women and children in the household was missing and these were households with  applicable  members (that is with children and/or women), we consider the household as missing this indicator. Otherwise, we used the available information. Similarly, for child  mortality, households that had applicable members who did not respond to the mortality question are considered to be missing this information; otherwise the household is considered non-deprived. */
#   
#   ** 1- DEPRIVED IN NUTRITION **
#   
#   /* Adults are considered malnourished if their BMI is below 18.5. Children are considered malnourished if their z-score of weight-for-age is below minus two standard deviations from the median of the reference population. 
# 
# DHS contains nutritional information on women between 15 and 49 years (Body Mass Index, BMI hereafter) and on the under-5-year-old children of the household (weight and height). This allows constructing a composite indicator which considers a household to be deprived (and therefore all its members) if there is either a woman or a child undernourished in the household.  */
#   
#   
#   * WOMEN'S UNDERNUTRITION (BMI<18.5) *
# 
# gen w=1 if sex==0 & age>=15 & age<=49
dat$w <- NA
dat$w[which(dat$sex==0 & dat$age>=15 & dat$age<=49)] <- 1
# egen whh=sum(w), by(hhid)
whh <- data.table(dat)[,.(whh=sum(w,na.rm=TRUE)),by=.(hhid)]
dat <- merge(dat,whh,by="hhid",all.x=TRUE)
remove(whh)
# 
# gen bmi=(ha2/(ha3^2))*100000
dat$bmi <- (dat$ha2/(dat$ha3^2))*100000
# replace bmi=. if (ha2>=9994 & ha2<=9999) | (ha3>=9994 & ha3<=9999) | ha40==9998 | ha40==9999
dat$bmi[which((dat$ha2>=9994 & dat$ha2<=9999) | (dat$ha3>=9994 & dat$ha3<=9999) | dat$ha40==9998 | dat$ha40==9999)] <- NA
# 
# gen malnourishedw = 1 if bmi<18.5
dat$malnourishedw <- NA
dat$malnourishedw[which(dat$bmi<18.5)] <- 1
# replace malnourishedw=0 if bmi>=18.5 & bmi<.
dat$malnourishedw[which(dat$bmi>=18 & !is.na(dat$bmi))] <- 0
# egen malnourishedwhh=sum(malnourishedw), by (hhid) missing
malnourishedwhh <- data.table(dat)[,.(malnourishedwhh=sum(malnourishedw,na.rm=TRUE),not.missing=sum(!is.na(malnourishedw))),by=.(hhid)]
malnourishedwhh$malnourishedwhh[which(malnourishedwhh$not.missing==0)] <- NA
malnourishedwhh <- data.frame(malnourishedwhh)
malnourishedwhh$not.missing <- NULL
dat <- merge(dat,malnourishedwhh,by="hhid",all.x=TRUE)
remove(malnourishedwhh)
# 
# gen missing_bmi=1 if ha40>=9996 & ha40<=9999
dat$missing_bmi <- NA
dat$missing_bmi[which(dat$ha40>=9996 & dat$ha40<=9999)] <- 1
# egen missing_bmihh=sum(missing_bmi), by(hhid) missing
missing_bmihh <- data.table(dat)[,.(missing_bmihh=sum(missing_bmi,na.rm=TRUE),not.missing=sum(!is.na(missing_bmi))),by=.(hhid)]
missing_bmihh$missing_bmihh[which(missing_bmihh$not.missing==0)] <- NA
missing_bmihh <- data.frame(missing_bmihh)
missing_bmihh$not.missing <- NULL
dat <- merge(dat,missing_bmihh,by="hhid",all.x=TRUE)
remove(missing_bmihh)
# 
# gen nomissing_bmi=1 if ha40<9996
dat$nomissing_bmi <- NA
dat$nomissing_bmi[which(dat$ha40<9996)] <- 1
# egen nomissing_bmihh=sum(nomissing_bmi), by(hhid) missing
nomissing_bmihh <- data.table(dat)[,.(nomissing_bmihh=sum(nomissing_bmi,na.rm=TRUE),not.missing=sum(!is.na(nomissing_bmi))),by=.(hhid)]
nomissing_bmihh$nomissing_bmihh[which(nomissing_bmihh$not.missing==0)] <- NA
nomissing_bmihh <- data.frame(nomissing_bmihh)
nomissing_bmihh$not.missing <- NULL
dat <- merge(dat,nomissing_bmihh,by="hhid",all.x=TRUE)
remove(nomissing_bmihh)
# 
# gen noaplic_bmi=1 if ha40==.
dat$noaplic_bmi <- NA
dat$noaplic_bmi[which(is.na(dat$ha40))] <- 1
# egen noaplic_bmihh=sum(noaplic_bmi), by(hhid) missing
noaplic_bmihh <- data.table(dat)[,.(noaplic_bmihh=sum(noaplic_bmi,na.rm=TRUE),not.missing=sum(!is.na(noaplic_bmi))),by=.(hhid)]
noaplic_bmihh$noaplic_bmihh[which(noaplic_bmihh$not.missing==0)] <- NA
noaplic_bmihh <- data.frame(noaplic_bmihh)
noaplic_bmihh$not.missing <- NULL
dat <- merge(dat,noaplic_bmihh,by="hhid",all.x=TRUE)
remove(noaplic_bmihh)
# 
# gen womanbmi=1 if ha40<.
dat$womanbmi <- NA
dat$womanbmi[which(!is.na(dat$ha40))] <- 1
# egen womanbmihh=sum(womanbmi), by(hhid) missing
womanbmihh <- data.table(dat)[,.(womanbmihh=sum(womanbmi,na.rm=TRUE),not.missing=sum(!is.na(womanbmi))),by=.(hhid)]
womanbmihh$womanbmihh[which(womanbmihh$not.missing==0)] <- NA
womanbmihh <- data.frame(womanbmihh)
womanbmihh$not.missing <- NULL
dat <- merge(dat,womanbmihh,by="hhid",all.x=TRUE)
remove(womanbmihh)
# 
# 
# gen missing_bmi_finalhh=1 if missing_bmihh>=1 & missing_bmihh<. & nomissing_bmihh==.
dat$missing_bmi_finalhh <- NA
dat$missing_bmi_finalhh[which(dat$missing_bmihh>=1 & !is.na(dat$missing_bmihh) & is.na(dat$nomissing_bmihh))] <- 1
# 
# gen uno_bmi=1 if bmi<.
dat$uno_bmi <- NA
dat$uno_bmi[which(!is.na(dat$bmi))] <- 1
# egen nw_bmi=sum(uno_bmi), by (hhid)
nw_bmi <- data.table(dat)[,.(nw_bmi=sum(uno_bmi,na.rm=TRUE)),by=.(hhid)]
dat <- merge(dat,nw_bmi,by="hhid",all.x=TRUE)
remove(nw_bmi)
# 
# ren malnourishedwhh nmalnourishedwhh
names(dat)[which(names(dat)=="malnourishedwhh")] <- "nmalnourishedwhh"
# gen malnourishedwhh=1 if nmalnourishedwhh>=1 & nmalnourishedwhh<.
dat$malnourishedwhh <- NA
dat$malnourishedwhh[which(dat$nmalnourishedwhh>=1 & !is.na(dat$nmalnourishedwhh))] <- 1
# replace malnourishedwhh=0 if nmalnourishedwhh==0
dat$malnourishedwhh[which(dat$nmalnourishedwhh==0)] <- 0
# 
# replace missing_bmi_finalhh=1 if malnourishedwhh==0 & whh>(2*nw_bmi) & nw_bmi<.
dat$missing_bmi_finalhh[which(dat$malnourishedwhh==0 & dat$whh>(2*dat$nw_bmi) & !is.na(dat$nw_bmi))] <- 1
# replace missing_bmihh=1 if malnourishedwhh==0 & whh>(2*nw_bmi) & nw_bmi<.
dat$missing_bmihh[which(dat$malnourishedwhh==0 & dat$whh>(2*dat$nw_bmi) & !is.na(dat$nw_bmi))] <- 1
# replace malnourishedwhh=. if malnourishedwhh==0 & whh>(2*nw_bmi) & nw_bmi<.
dat$malnourishedwhh[which(dat$malnourishedwhh==0 & dat$whh>(2*dat$nw_bmi) & !is.na(dat$nw_bmi))] <- NA
# 
# 
# * MEN'S UNDERNUTRITION (BMI<18.5) *
#   capture drop m mhh
# 
# gen m=1 if sex==1 & age>=15 & age<=54
dat$m <- NA
dat$m[which(dat$sex==1 & dat$age>=15 & dat$age<=54)] <- 1
# egen mhh=sum(m), by(hhid)
mhh <- data.table(dat)[,.(mhh=sum(m,na.rm=TRUE)),by=.(hhid)]
dat <- merge(dat,mhh,by="hhid",all.x=TRUE)
remove(mhh)
# 
# gen mbmi=(hb2/(hb3^2))*100000
dat$mbmi <- (dat$hb2/(dat$hb3^2))*100000
# replace mbmi=. if (hb2>=9994 & hb2<=9999) | (hb3>=9994 & hb3<=9999) | hb40==9998 | hb40==9999
dat$mbmi[which((dat$hb2>=9994 & dat$hb2<=9999) | (dat$hb3>=9994 & dat$hb3<=9999) | dat$hb40==9998 | dat$hb40==9999)] <- NA
# 
# gen malnourishedm = 1 if mbmi<18.5
dat$malnourishedm <- NA
dat$malnourishedm[which(dat$mbmi<18.5)] <- 1
# replace malnourishedm=0 if mbmi>=18.5 & mbmi<.
dat$malnourishedm[which(dat$mbmi>=18.5 & !is.na(dat$mbmi))] <- 0
# egen malnourishedmhh=sum(malnourishedm), by (hhid) missing
malnourishedmhh <- data.table(dat)[,.(malnourishedmhh=sum(malnourishedm,na.rm=TRUE),not.missing=sum(!is.na(malnourishedm))),by=.(hhid)]
malnourishedmhh$malnourishedmhh[which(malnourishedmhh$not.missing==0)] <- NA
malnourishedmhh <- data.frame(malnourishedmhh)
malnourishedmhh$not.missing <- NULL
dat <- merge(dat,malnourishedmhh,by="hhid",all.x=TRUE)
remove(malnourishedmhh)
# 
# gen missing_mbmi=1 if hb40>=9996 & hb40<=9999
dat$missing_mbmi <- NA
dat$missing_mbmi[which(dat$hb40>=9996 & dat$hb40<=9999)] <- 1
# egen missing_mbmihh=sum(missing_mbmi), by(hhid) missing
missing_mbmihh <- data.table(dat)[,.(missing_mbmihh=sum(missing_mbmi,na.rm=TRUE),not.missing=sum(!is.na(missing_mbmi))),by=.(hhid)]
missing_mbmihh$missing_mbmihh[which(missing_mbmihh$not.missing==0)] <- NA
missing_mbmihh <- data.frame(missing_mbmihh)
missing_mbmihh$not.missing <- NULL
dat <- merge(dat,missing_mbmihh,by="hhid",all.x=TRUE)
remove(missing_mbmihh)
# 
# gen nomissing_mbmi=1 if hb40<9996
dat$nomissing_mbmi <- NA
dat$nomissing_mbmi[which(dat$hb40<9996)] <- 1
# egen nomissing_mbmihh=sum(nomissing_mbmi), by(hhid) missing
nomissing_mbmihh <- data.table(dat)[,.(nomissing_mbmihh=sum(nomissing_mbmi,na.rm=TRUE),not.missing=sum(!is.na(nomissing_mbmi))),by=.(hhid)]
nomissing_mbmihh$nomissing_mbmihh[which(nomissing_mbmihh$not.missing==0)] <- NA
nomissing_mbmihh <- data.frame(nomissing_mbmihh)
nomissing_mbmihh$not.missing <- NULL
dat <- merge(dat,nomissing_mbmihh,by="hhid",all.x=TRUE)
remove(nomissing_mbmihh)
# 
# gen noaplic_mbmi=1 if hb40==.
dat$noaplic_mbmi <- NA
dat$noaplic_mbmi[which(is.na(dat$hb40))] <- 1
# egen noaplic_mbmihh=sum(noaplic_mbmi), by(hhid) missing
noaplic_mbmihh <- data.table(dat)[,.(noaplic_mbmihh=sum(noaplic_mbmi,na.rm=TRUE),not.missing=sum(!is.na(noaplic_mbmi))),by=.(hhid)]
noaplic_mbmihh$noaplic_mbmihh[which(noaplic_mbmihh$not.missing==0)] <- NA
noaplic_mbmihh <- data.frame(noaplic_mbmihh)
noaplic_mbmihh$not.missing <- NULL
dat <- merge(dat,noaplic_mbmihh,by="hhid",all.x=TRUE)
remove(noaplic_mbmihh)
# 
# gen manbmi=1 if hb40<.
dat$manbmi <- NA
dat$manbmi[which(!is.na(dat$hb40))] <- 1
# egen manbmihh=sum(manbmi), by(hhid) missing
manbmihh <- data.table(dat)[,.(manbmihh=sum(manbmi,na.rm=TRUE),not.missing=sum(!is.na(manbmi))),by=.(hhid)]
manbmihh$manbmihh[which(manbmihh$not.missing==0)] <- NA
manbmihh <- data.frame(manbmihh)
manbmihh$not.missing <- NULL
dat <- merge(dat,manbmihh,by="hhid",all.x=TRUE)
remove(manbmihh)
# 
# 
# gen missing_mbmi_finalhh=1 if missing_mbmihh>=1 & missing_mbmihh<. & nomissing_mbmihh==.
dat$missing_mbmi_finalhh <- NA
dat$missing_mbmi_finalhh[which(dat$missing_mbmihh>=1 & !is.na(dat$missing_mbmihh) & is.na(dat$nomissing_mbmihh))] <- 1
# 
# gen uno_mbmi=1 if mbmi<.
dat$uno_mbmi <- NA
dat$uno_mbmi[which(!is.na(dat$mbmi))] <- 1
# egen nm_bmi=sum(uno_mbmi), by (hhid)
nm_bmi <- data.table(dat)[,.(nm_bmi=sum(uno_mbmi,na.rm=TRUE)),by=.(hhid)]
dat <- merge(dat,nm_bmi,by="hhid",all.x=TRUE)
remove(nm_bmi)
# 
# ren malnourishedmhh nmalnourishedmhh
names(dat)[which(names(dat)=="malnourishedmhh")] <- "nmalnourishedmhh"
# gen malnourishedmhh=1 if nmalnourishedmhh>=1 & nmalnourishedmhh<.
dat$malnourishedmhh <- NA
dat$malnourishedmhh[which(dat$nmalnourishedmhh>=1 & !is.na(dat$nmalnourishedmhh))] <- 1
# replace malnourishedmhh=0 if nmalnourishedmhh==0
dat$malnourishedmhh[which(dat$nmalnourishedmhh==0)] <- 0
# 
# replace missing_mbmi_finalhh=1 if malnourishedmhh==0 & mhh>(2*nm_bmi) & nm_bmi<.
dat$missing_mbmi_finalhh[which(dat$malnourishedmhh==0 & dat$mhh>(2*dat$nm_bmi) & !is.na(dat$nm_bmi))] <- 1
# replace missing_mbmihh=1 if malnourishedmhh==0 & mhh>(2*nm_bmi) & nm_bmi<.
dat$missing_mbmihh[which(dat$malnourishedmhh==0 & dat$mhh>(2*dat$nm_bmi) & !is.na(dat$nm_bmi))] <- 1
# replace malnourishedmhh=. if malnourishedmhh==0 & mhh>(2*nm_bmi) & nm_bmi<.
dat$malnourishedmhh[which(dat$malnourishedmhh==0 & dat$mhh>(2*dat$nm_bmi) & !is.na(dat$nm_bmi))] <- NA

# 
# 
# sort hv001 hv002 hvidx
dat <- dat[order(dat$hv001,dat$hv002,dat$hvidx),]
# save "Uganda_MPI2_2011.dta", replace
total <- dat
# 
# /* clear
# set maxvar 7000
# 
# use "C:\Users\cecilia.calderon\HDRO_MCC\MPI\MPI_new calculations\Uganda 2011_DHS\Uganda_MPI2_2011.dta", clear
# 
# 
# keep hc27 hc1 hc2 hc3 hc13 hc15 weight hv001 hv002 hvidx
keep <- c("hc27","hc1","hc2","hc3","hc13","hc15","weight","hv001","hv002","hvidx")
dat <- dat[keep]
# 
# 
# * Variable "Sex" (1=male; 2=female)
# tab hc27, miss 
# gen gender = hc27
dat$gender <- dat$hc27
# desc gender
# tab gender
# 
# * Variable "Age"can be expresses it in months or days
# tab hc1, miss
# codebook hc1 
# gen age_months = hc1
dat$age_months <- dat$hc1
# desc age_months
# summ age_months 
# gen str6 ageunit = "months" 
dat$ageunit <- "months"
# label var ageunit "Months"
# 
# *Variable "Sampling weight"
# ren weight sw
names(dat)[which(names(dat)=="weight")] <- "sw"
# desc sw
# summ sw
# 
# * Variable "body weight" - it must be in kilograms
# ta hc2, miss
# codebook hc2
# gen weight = hc2/10 if hc2<9000
dat$weight <- NA
dat$weight[which(dat$hc2<9000)] <- dat$hc2[which(dat$hc2<9000)]/10
# desc weight 
# replace weight=. if hc13>0
dat$weight[which(dat$hc13>0)] <- NA
# tab hc2 hc13 if hc13>0, miss
# summ weight
# 
# * Variable "height" - it must be in centimetres
# ta hc3, miss
# codebook hc3
# gen height = hc3/10 if hc3<9000
dat$height <- NA
dat$height[which(dat$hc3<9000)] <- dat$hc3[which(dat$hc3<9000)]/10
# desc height 
# replace height=. if hc13>0
dat$height[which(dat$hc13>0)] <- NA
# tab hc3 hc13 if hc13>0, miss
# summ height
# 
# codebook hc15
# gen measure = "l" if hc15==1 
dat$measure <- NA
dat$measure[which(dat$hc15==1)] <- "l"
# replace measure = "h" if hc15==2 
dat$measure[which(dat$hc15==2)] <- "h"
# replace measure = " " if hc15==0 | hc15==9 | hc15==.
# desc measure
# tab measure
# 
# *Variable "Oedema" 
# gen oedema=.
dat$oedema <- NA
# desc oedema
# 
# 
# 
# keep hv001 hv002 hvidx gender age_months ageunit weight height oedema measure sw
keep <- c("hv001", "hv002", "hvidx", "gender", "age_months", "ageunit", "weight", "height", "oedema", "measure", "sw")
dat <- dat[keep]
# save "C:\Users\cecilia.calderon\HDRO_MCC\MPI\WHO igrowup STATA\WHO igrowup workdata\UG_survey.dta", replace
# 
# 
# * THIS RUNS THE ANTHRO ADO FILE FROM WHO *
#   
#   
#   /*	Example: survey_standard.do using survey.dta */
#   
#   clear
# 
# set more 1
# 
# /*	Higher memory might be necessary for larger datasets */
#   set memory 20m
# set maxvar 10000
# 
# 
# /* Indicate to the Stata compiler where the igrowup_standard.ado file is stored*/
#   adopath + "C:\Users\cecilia.calderon\HDRO_MCC\MPI\WHO igrowup STATA\"
# 
# 
# /* Load the data file */
# use "C:\Users\cecilia.calderon\HDRO_MCC\MPI\WHO igrowup STATA\WHO igrowup workdata\UG_survey.dta", clear
# 
# 
# /* generate the first three parameters reflib, datalib & datalab	*/
# gen str60 reflib="C:\igrowup_stata"
# lab var reflib "Directory of reference tables"
# 
# gen str60 datalib="C:\Users\cecilia.calderon\HDRO_MCC\MPI\WHO igrowup STATA\WHO igrowup workdata"
# lab var datalib "Directory for datafiles"
# 
# gen str30 datalab="UG_survey"
# lab var datalab "Working file"
# 
# /*	check the variable for "sex"	1 = male, 2=female */
# desc gender
# tab gender
# 
# 
# /*	check the variable for "age"	*/
# ren age_months agemons
names(dat)[which(names(dat)=="age_months")] <- "agemons"
# desc agemons
# summ agemons
# 
# 
# /*	define your ageunit	*/
# *gen str6 ageunit="months"				/* or gen ageunit="days" */
# *lab var ageunit "=days or =months"
# 
# 
# /*	check the variable for body "weight" which must be in kilograms*/
# /* 	NOTE: if not available, please create as [gen weight=.]*/
# desc weight 
# summ weight
# 
# /* 	check the variable for "height" which must be in centimeters*/ 
# /* 	NOTE: if not available, please create as [gen height=.]*/
# desc height 
# summ height
# 
# 
# /*	check the variable for "measure"*/
# /* 	NOTE: if not available, please create as [gen str1 measure=" "]*/
# desc measure
# tab measure
# 
# /* 	check the variable for "headc" which must be in centimeters*/ 
# /* 	NOTE: if not available, please create as [gen headc=.]*/
# gen headc=.
# desc head 
# summ head
# 
# /* 	check the variable for "armc" which must be in in centimeters*/ 
# /* 	NOTE: if not available, please create as [gen armc=.]*/
# gen muac=.
# desc muac 
# summ muac
# 
# /* 	check the variable for "triskin" which must be in millimeters*/ 
# /* 	NOTE: if not available, please create as [gen triskin=.]*/
# gen triskin=.
# desc tri
# summ tri
# 
# /* 	check the variable for "subskin" which must be in millimeters*/ 
# /* 	NOTE: if not available, please create as [gen subskin=.]*/
# gen subskin=.
# desc sub 
# summ sub
# 
# /* 	check the variable for "oedema"*/
# /* 	NOTE: if not available, please create as [gen str1 oedema="n"]*/
# desc oedema
# tab oedema
# 
# 
# /*	check the variable for "sw" for the sampling weight*/
# /* 	NOTE: if not available, please create as [gen sw=1]*/
# desc sw
# summ sw
# 
# 
# /* 	Fill in the macro parameters to run the command */

igu.dir <- "D:/Documents/igrowup_R/"
weianthro<-read.table(paste0(igu.dir,"/weianthro.txt"),header=T,sep="",skip=0)
lenanthro<-read.table(paste0(igu.dir,"/lenanthro.txt"),header=T,sep="",skip=0)
bmianthro<-read.table(paste0(igu.dir,"/bmianthro.txt"),header=T,sep="",skip=0)
hcanthro<-read.table(paste0(igu.dir,"/hcanthro.txt"),header=T,sep="",skip=0)
acanthro<-read.table(paste0(igu.dir,"/acanthro.txt"),header=T,sep="",skip=0)
ssanthro<-read.table(paste0(igu.dir,"/ssanthro.txt"),header=T,sep="",skip=0)
tsanthro<-read.table(paste0(igu.dir,"/tsanthro.txt"),header=T,sep="",skip=0)
wflanthro<-read.table(paste0(igu.dir,"/wflanthro.txt"),header=T,sep="",skip=0)
wfhanthro<-read.table(paste0(igu.dir,"/wfhanthro.txt"),header=T,sep="",skip=0)
source(paste0(igu.dir,"igrowup_standard.r"))
source(paste0(igu.dir,"igrowup_restricted.r"))
igrowup.restricted(FileLab="dat",FilePath=igu.dir,
                   mydf=dat, sex=gender
                   , age=agemons, age.month=TRUE
                   , weight=weight
                   , lenhei=height
                   , measure=measure
                   , sw=sw)

dat <- read.csv(paste0(igu.dir,"dat_z_rc.csv"))

# igrowup_standard reflib datalib datalab gender agemons ageunit weight height measure head muac tri sub oedema sw 
# 
# keep hv001 hv002 hvidx agemons height _zwei _zlen _zbmi _zwfl _fwfl _flen _fwei _fbmi
keep <- c("hv001","hv002","hvidx","agemons","height","zwei","zlen","zbmi","zwfl","fwfl","flen","fwei","fbmi")
dat <- dat[keep]
names(dat)[c(6:13)] <- paste0("_",names(dat)[c(6:13)])
# sort hv001 hv002 hvidx
dat <- dat[order(dat$hv001,dat$hv002,dat$hvidx),]
# save "C:\Users\cecilia.calderon\HDRO_MCC\MPI\MPI_new calculations\Uganda 2011_DHS\UG_anthro.dta", replace */
anthro <- dat
remove(acanthro,bmianthro,hcanthro,lenanthro,matprev,matz,ssanthro,tsanthro,weianthro,wfhanthro,wflanthro)
# 
# 
# 
# use "Uganda_MPI2_2011.dta", clear
dat <- total
# merge hv001 hv002 hvidx using "UG_anthro.dta"
dat <- merge(dat,anthro,by=c("hv001","hv002","hvidx"),all.x=TRUE)
# tab _merge
# drop _merge
# 
# * UNDERNUTRITION USING Z SCORES CALCULATED WITH WHO CODE *
# 
# * Weight-for-age *
# gen zwa = _zwei if _fwei==0
dat$zwa <- NA
dat$zwa[which(dat[,"_fwei"]==0)] <- dat[,"_zwei"][which(dat[,"_fwei"]==0)]
# 
# gen malnourishedw5 = 1 if zwa<=-2
dat$malnourishedw5 <- NA
dat$malnourishedw5[which(dat$zwa<=-2)] <- 1
# replace malnourishedw5=0 if zwa>-2 & zwa<.
dat$malnourishedw5[which(dat$zwa>-2 & !is.na(dat$zwa))] <- 0
# egen malnourishedw5hh=sum(malnourishedw5), by (hhid) missing
malnourishedw5hh <- data.table(dat)[,.(malnourishedw5hh=sum(malnourishedw5,na.rm=TRUE),not.missing=sum(!is.na(malnourishedw5))),by=.(hhid)]
malnourishedw5hh$malnourishedw5hh[which(malnourishedw5hh$not.missing==0)] <- NA
malnourishedw5hh <- data.frame(malnourishedw5hh)
malnourishedw5hh$not.missing <- NULL
dat <- merge(dat,malnourishedw5hh,by="hhid",all.x=TRUE)
remove(malnourishedw5hh)
# 
# gen missing_zwa=1 if _fwei==1 | (hc2>9000 & hc2<=9999) | (hc13>0 & hc13<=9)
dat$missing_zwa <- NA
dat$missing_zwa[which(dat[,"_fwei"]==1 | (dat$hc2>9000 & dat$hc2<=9999) | (dat$hc13>0 & dat$hc13<=9))] <- 1
# egen missing_zwahh=sum(missing_zwa), by(hhid) missing
missing_zwahh <- data.table(dat)[,.(missing_zwahh=sum(missing_zwa,na.rm=TRUE),not.missing=sum(!is.na(missing_zwa))),by=.(hhid)]
missing_zwahh$missing_zwahh[which(missing_zwahh$not.missing==0)] <- NA
missing_zwahh <- data.frame(missing_zwahh)
missing_zwahh$not.missing <- NULL
dat <- merge(dat,missing_zwahh,by="hhid",all.x=TRUE)
remove(missing_zwahh)
# 
# gen nomissing_zwa=1 if _fwei==0
dat$nomissing_zwa <- NA
dat$nomissing_zwa[which(dat[,"_fwei"]==0)] <- 1
# egen nomissing_zwahh=sum(nomissing_zwa), by(hhid) missing
nomissing_zwahh <- data.table(dat)[,.(nomissing_zwahh=sum(nomissing_zwa,na.rm=TRUE),not.missing=sum(!is.na(nomissing_zwa))),by=.(hhid)]
nomissing_zwahh$nomissing_zwahh[which(nomissing_zwahh$not.missing==0)] <- NA
nomissing_zwahh <- data.frame(nomissing_zwahh)
nomissing_zwahh$not.missing <- NULL
dat <- merge(dat,nomissing_zwahh,by="hhid",all.x=TRUE)
remove(nomissing_zwahh)
# 
# gen missing_zwa_finalhh=1 if missing_zwahh>=1 & missing_zwahh<. & nomissing_zwahh==.
dat$missing_zwa_finalhh <- NA
dat$missing_zwa_finalhh[which(dat$missing_zwahh>=1 & !is.na(dat$missing_zwahh) & is.na(dat$nomissing_zwahh))] <- 1
# replace missing_zwa_finalhh=0 if missing_zwa_finalhh==.
dat$missing_zwa_finalhh[which(is.na(dat$missing_zwa_finalhh))] <- 0
# 
# gen uno_zwa=1 if zwa<.
dat$uno_zwa <- NA
dat$uno_zwa[which(!is.na(dat$zwa))] <- 1
# egen nw_zwa=sum(uno_zwa), by (hhid)
nw_zwa <- data.table(dat)[,.(nw_zwa=sum(uno_zwa,na.rm=TRUE)),by=.(hhid)]
dat <- merge(dat,nw_zwa,by="hhid",all.x=TRUE)
remove(nw_zwa)
# 
# 
# * Height-for-age *
# gen zha = _zlen if _flen==0
dat$zha <- NA
dat$zha[which(dat[,"_flen"]==0)] <- dat[,"_zlen"][which(dat[,"_flen"]==0)]
# 
# gen malnourished5 = 1 if zha<=-2
dat$malnourished5 <- NA
dat$malnourished5[which(dat$zha<=-2)] <- 1
# replace malnourished5=0 if zha>-2 & zha<.
dat$malnourished5[which(dat$zha>-2 & !is.na(dat$zha))] <- 0
# egen malnourished5hh=sum(malnourished5), by (hhid) missing
malnourished5hh <- data.table(dat)[,.(malnourished5hh=sum(malnourished5,na.rm=TRUE),not.missing=sum(!is.na(malnourished5))),by=.(hhid)]
malnourished5hh$malnourished5hh[which(malnourished5hh$not.missing==0)] <- NA
malnourished5hh <- data.frame(malnourished5hh)
malnourished5hh$not.missing <- NULL
dat <- merge(dat,malnourished5hh,by="hhid",all.x=TRUE)
remove(malnourished5hh)
# 
# gen missing_zha=1 if _flen==1 | (hc3>9000 & hc3<=9999) | (hc13>0 & hc13<=9)
dat$missing_zha <- NA
dat$missing_zha[which(dat[,"_flen"]==1 | (dat$hc3>9000 & dat$hc3<=9999) | (dat$hc13>0 & dat$hc13<=9))] <- 1
# egen missing_zhahh=sum(missing_zha), by(hhid) missing
missing_zhahh <- data.table(dat)[,.(missing_zhahh=sum(missing_zha,na.rm=TRUE),not.missing=sum(!is.na(missing_zha))),by=.(hhid)]
missing_zhahh$missing_zhahh[which(missing_zhahh$not.missing==0)] <- NA
missing_zhahh <- data.frame(missing_zhahh)
missing_zhahh$not.missing <- NULL
dat <- merge(dat,missing_zhahh,by="hhid",all.x=TRUE)
remove(missing_zhahh)
# 
# gen nomissing_zha=1 if _flen==0
dat$nomissing_zha <- NA
dat$nomissing_zha[which(dat[,"_flen"]==0)] <- 1
# egen nomissing_zhahh=sum(nomissing_zha), by(hhid) missing
nomissing_zhahh <- data.table(dat)[,.(nomissing_zhahh=sum(nomissing_zha,na.rm=TRUE),not.missing=sum(!is.na(nomissing_zha))),by=.(hhid)]
nomissing_zhahh$nomissing_zhahh[which(nomissing_zhahh$not.missing==0)] <- NA
nomissing_zhahh <- data.frame(nomissing_zhahh)
nomissing_zhahh$not.missing <- NULL
dat <- merge(dat,nomissing_zhahh,by="hhid",all.x=TRUE)
remove(nomissing_zhahh)
# 
# gen missing_zha_finalhh=1 if missing_zhahh>=1 & missing_zhahh<. & nomissing_zhahh==.
dat$missing_zha_finalhh <- NA
dat$missing_zha_finalhh[which(dat$missing_zhahh>=1 & !is.na(dat$missing_zhahh) & is.na(dat$nomissing_zhahh))] <- 1
# replace missing_zha_finalhh=0 if missing_zha_finalhh==.
dat$missing_zha_finalhh[which(is.na(dat$missing_zha_finalhh))] <- 0
# 
# gen uno_zha=1 if zha<.
dat$uno_zha <- NA
dat$uno_zha[which(!is.na(dat$zha))] <- 1
# egen nw_zha=sum(uno_zha), by (hhid)
nw_zha <- data.table(dat)[,.(nw_zha=sum(uno_zha,na.rm=TRUE)),by=.(hhid)]
dat <- merge(dat,nw_zha,by="hhid",all.x=TRUE)
remove(nw_zha)
# 
# 
# ren malnourished5hh nmalnourished5hh
names(dat)[which(names(dat)=="malnourished5hh")] <- "nmalnourished5hh"
# gen malnourished5hh=1 if nmalnourished5hh>=1 & nmalnourished5hh<.
dat$malnourished5hh <- NA
dat$malnourished5hh[which(dat$nmalnourished5hh>=1 & !is.na(dat$nmalnourished5hh))] <- 1
# replace malnourished5hh=0 if nmalnourished5hh==0
dat$malnourished5hh[which(dat$nmalnourished5hh==0)] <- 0
# 
# gen uno0_5=1 if age<=5
dat$uno0_5 <- NA
dat$uno0_5[which(dat$age<=5)] <- 1
# bys hhid: egen hhmember0_5 = sum(uno0_5)
hhmember0_5 <- data.table(dat)[,.(hhmember0_5=sum(uno0_5,na.rm=TRUE)),by=.(hhid)]
dat <- merge(dat,hhmember0_5,by="hhid",all.x=TRUE)
remove(hhmember0_5)
# 
# replace missing_zha_finalhh=1 if malnourished5hh==0 & hhmember0_5>(2*nw_zha) & nw_zha<.
dat$missing_zha_finalhh[which(dat$malnourished5hh==0 & dat$hhmember0_5>(2*dat$nw_zha) & !is.na(dat$nw_zha))] <- 1
# replace missing_zhahh=1 if malnourished5hh==0 & hhmember0_5>(2*nw_zha) & nw_zha<.
dat$missing_zhahh[which(dat$malnourished5hh==0 & dat$hhmember0_5>(2*dat$nw_zha) & !is.na(dat$nw_zha))] <- 1
# replace malnourished5hh=. if malnourished5hh==0 & hhmember0_5>(2*nw_zha) & nw_zha<.
dat$malnourished5hh[which(dat$malnourished5hh==0 & dat$hhmember0_5>(2*dat$nw_zha) & !is.na(dat$nw_zha))] <- NA
# 
# 
# * Number of children 5y but not necessarily <60 months
# capture drop uno5
# capture drop nc5
# gen uno5=1 if age==5
dat$uno5 <- NA
dat$uno5[which(dat$age==5)] <- 1
# egen nc5=sum(uno5), by(hhid)
nc5 <- data.table(dat)[,.(nc5=sum(uno5,na.rm=TRUE)),by=.(hhid)]
dat <- merge(dat,nc5,by="hhid",all.x=TRUE)
remove(nc5)
# 
# 
# gen undernutritionhh=0 if malnourishedwhh==0 & malnourished5hh==0
dat$undernutritionhh <- NA
dat$undernutritionhh[which(dat$malnourishedwhh==0 & dat$malnourished5hh==0)] <- 0
# replace undernutritionhh=0 if malnourishedwhh==0 & malnourished5hh==. & hhmember0_5==0
dat$undernutritionhh[which(dat$malnourishedwhh==0 & is.na(dat$malnourished5hh) & dat$hhmember0_5==0)] <- 0
# replace undernutritionhh=0 if malnourished5hh==0 & malnourishedwhh==. & whh==0
dat$undernutritionhh[which(dat$malnourishedwhh==0 & is.na(dat$malnourished5hh) & dat$whh==0)] <- 0
# replace undernutritionhh=1 if (malnourishedwhh>0 & malnourishedwhh<.) | (malnourished5hh>0 & malnourished5hh<.)
dat$undernutritionhh[which((dat$malnourishedwhh>0 & !is.na(dat$malnourishedwhh)) | (dat$malnourished5hh>0 & !is.na(dat$malnourished5hh)))] <- 1
# 
# replace undernutritionhh=0 if undernutritionhh==. & malnourishedwhh==0 & malnourished5hh==. & (whh>0 | hhmember0_5>0) & nc5>0 & nc5<. & nc5==hhmember0_5
if(length(which(is.na(dat$undernutritionhh) & dat$malnourishedwhh==0 & is.na(dat$malnourished5hh) & (dat$whh>0 | dat$hhmember0_5>0) & dat$nc5>0 & !is.na(dat$nc5) & dat$nc5==dat$hhmember0_5))>0){
  dat$undernutritionhh[which(is.na(dat$undernutritionhh) & dat$malnourishedwhh==0 & is.na(dat$malnourished5hh) & (dat$whh>0 | dat$hhmember0_5>0) & dat$nc5>0 & !is.na(dat$nc5) & dat$nc5==dat$hhmember0_5)] <- 0
}
# replace undernutritionhh=0 if undernutritionhh==. & malnourishedwhh==0 & malnourished5hh==. & (whh>0 | hhmember0_5>0) & nc5>0 & nc5<. & hhmember0_5>nc5 & (nw_zha/(hhmember0_5-nc5)>=(1/2))
if(length(which(is.na(dat$undernutritionhh) & dat$malnourishedwhh==0 & is.na(dat$malnourished5hh) & (dat$whh>0 | dat$hhmember0_5>0) & dat$nc5>0 & !is.na(dat$nc5) & dat$hhmember0_5>dat$nc5 & (dat$nw_zha/(dat$hhmember0_5-dat$nc5)>=(1/2))))>0){
  dat$undernutritionhh[which(is.na(dat$undernutritionhh) & dat$malnourishedwhh==0 & is.na(dat$malnourished5hh) & (dat$whh>0 | dat$hhmember0_5>0) & dat$nc5>0 & !is.na(dat$nc5) & dat$hhmember0_5>dat$nc5 & (dat$nw_zha/(dat$hhmember0_5-dat$nc5)>=(1/2)))] <- 0
}
# replace undernutritionhh=0 if undernutritionhh==. & malnourishedwhh==. & malnourished5hh==. & (whh>0 | hhmember0_5>0) & nc5>0 & nc5<. & hhmember0_5>nc5 & (nw_zha/(hhmember0_5-nc5)>=(1/2)) & (nw_bmi>=(1/2)*whh)
if(length(which(is.na(dat$undernutritionhh) & is.na(dat$malnourishedwhh) & is.na(dat$malnourished5hh) & (dat$whh>0 | dat$hhmember0_5>0) & dat$nc5>0 & !is.na(dat$nc5) & dat$hhmember0_5>dat$nc5 & (dat$nw_zha/(dat$hhmember0_5-dat$nc5)>=(1/2)) & (dat$nw_bmi>=(1/2)*dat$whh)))>0){
  dat$undernutritionhh[which(is.na(dat$undernutritionhh) & is.na(dat$malnourishedwhh) & is.na(dat$malnourished5hh) & (dat$whh>0 | dat$hhmember0_5>0) & dat$nc5>0 & !is.na(dat$nc5) & dat$hhmember0_5>dat$nc5 & (dat$nw_zha/(dat$hhmember0_5-dat$nc5)>=(1/2)) & (dat$nw_bmi>=(1/2)*dat$whh))] <- 0
}
# 
# 
# 
# ** 2- DEPRIVED IN MORTALITY **
# 
# replace children_diedhh=0 if children_diedhh==. & whh==0
dat$children_diedhh[which(is.na(dat$children_diedhh) & dat$whh==0)] <- 0
# replace child_died5hh=0 if whh==0 & child_died5hh==.
dat$child_died5hh[which(is.na(dat$child_died5hh) & dat$whh==0)] <- 0
# replace child_diedhh=0 if whh==0 & child_diedhh==.
dat$child_diedhh[which(is.na(dat$child_diedhh) & dat$whh==0)] <- 0
# 
# gen mortality5hh=0 if child_died5hh==0
dat$mortality5hh <- NA
dat$mortality5hh[which(dat$child_died5hh==0)] <- 0
# replace mortality5hh=1 if (child_died5hh>=1 & child_died5hh<.)
dat$mortality5hh[which((dat$child_died5hh>=1 & !is.na(dat$child_died5hh)))] <- 1

# 
# 
# /* the mortality indicator uses the available information. If there is a HH where one women lost a kid an another has missing info, the HH is considered deprived in this indicator. */
# 
# 
# 
# gen region = hv024
dat$region <- dat$hv024
# 
# compress
# sort hv001 hv002 hvidx
dat <- dat[order(dat$hv001,dat$hv002,dat$hvidx),]
# save "Uganda_MPI2_2011.dta", replace
# 
# 
# 
# * NON-ELIGIBLE POPULATION *
# 
# gen uno7_14=1 if age>=7 & age<=14
dat$uno7_14 <- NA
dat$uno7_14[which(dat$age>=7 & dat$age<=14)] <- 1
# bys hhid: egen hhmember7_14 = sum(uno7_14)
hhmember7_14 <- data.table(dat)[,.(hhmember7_14=sum(uno7_14,na.rm=TRUE)),by=.(hhid)]
dat <- merge(dat,hhmember7_14,by="hhid",all.x=TRUE)
remove(hhmember7_14)
# 
# 
# gen uno15_49=1 if age>=15 & age<=49 & sex==0
dat$uno15_49 <- NA
dat$uno15_49[which(dat$age>=15 & dat$age<=49 & dat$sex==0)] <- 1
# bys hhid: egen hhmember15_49 = sum(uno15_49)
hhmember15_49 <- data.table(dat)[,.(hhmember15_49=sum(uno15_49,na.rm=TRUE)),by=.(hhid)]
dat <- merge(dat,hhmember15_49,by="hhid",all.x=TRUE)
remove(hhmember15_49)
# 
# 
# capture drop uno0_5 hhmember0_5
dat$uno0_5 <- NULL
dat$hhmember0_5 <- NULL
# gen uno0_5=1 if age<=5
dat$uno0_5 <- NA
dat$uno0_5[which(dat$age<=5)] <- 1
# bys hhid: egen hhmember0_5 = sum(uno0_5)
hhmember0_5 <- data.table(dat)[,.(hhmember0_5=sum(uno0_5,na.rm=TRUE)),by=.(hhid)]
dat <- merge(dat,hhmember0_5,by="hhid",all.x=TRUE)
remove(hhmember0_5)
# 
# 
# gen uno7_14hh=0 if hhmember7_14==0
dat$uno7_14hh <- NA
dat$uno7_14hh[which(dat$hhmember7_14==0)] <- 0
# replace uno7_14hh=1 if hhmember7_14>=1 & hhmember7_14<.
dat$uno7_14hh[which(dat$hhmember7_14>=1 & !is.na(dat$hhmember7_14))] <- 1
# 
# gen uno15_49hh=0 if hhmember15_49==0
dat$uno15_49hh <- NA
dat$uno15_49hh[which(dat$hhmember15_49==0)] <- 0
# replace uno15_49hh=1 if hhmember15_49>=1 & hhmember15_49<.
dat$uno15_49hh[which(dat$hhmember15_49>=1 & !is.na(dat$hhmember15_49))] <- 1
# 
# 
# gen uno0_5hh=0 if hhmember0_5==0
dat$uno0_5hh <- NA
dat$uno0_5hh[which(dat$hhmember0_5==0)] <- 0
# replace uno0_5hh=1 if hhmember0_5>=1 & hhmember0_5<.
dat$uno0_5hh[which(dat$hhmember0_5>=1 & !is.na(dat$hhmember0_5))] <- 1

# 
# * Number of eligible children for anthropometrics
# egen nch05=sum(hv120), by(hhid)
nch05 <- data.table(dat)[,.(nch05=sum(hv120,na.rm=TRUE)),by=.(hhid)]
dat <- merge(dat,nch05,by="hhid",all.x=TRUE)
remove(nch05)
# 
# gen nutrihh=1 if undernutritionhh<. & (malnourished5hh<. | malnourishedwhh<.)
dat$nutrihh <- NA
dat$nutrihh[which(!is.na(dat$undernutritionhh) & (!is.na(dat$malnourished5hh) | !is.na(dat$malnourishedwhh)))] <- 1
# replace nutrihh=3 if undernutritionhh==. & malnourished5hh==. &  malnourishedwhh==. & uno15_49hh==0 & uno0_5hh==0
dat$nutrihh[which(is.na(dat$undernutritionhh) & is.na(dat$malnourished5hh) &  is.na(dat$malnourishedwhh) & dat$uno15_49hh==0 & dat$uno0_5hh==0)] <- 3
# replace nutrihh=4 if undernutritionhh==. & malnourished5hh==. &  malnourishedwhh==. & (uno15_49hh==1 | uno0_5hh==1)
dat$nutrihh[which(is.na(dat$undernutritionhh) & is.na(dat$malnourished5hh) &  is.na(dat$malnourishedwhh) & (dat$uno15_49hh==1 | dat$uno0_5hh==1))] <- 4
# replace nutrihh=2 if undernutritionhh==. & nutrihh==.
dat$nutrihh[which(is.na(dat$undernutritionhh) & is.na(dat$nutrihh))] <- 2
# replace nutrihh=1 if undernutritionhh==0 & (malnourishedwhh==0 & malnourished5hh==. & (whh>0 | hhmember0_5>0) & nc5>0 & nc5<. & nc5==hhmember0_5)
dat$nutrihh[which(dat$undernutritionhh==0 & (dat$malnourishedwhh==0 & is.na(dat$malnourished5hh) & (dat$whh>0 | dat$hhmember0_5>0) & dat$nc5>0 & !is.na(dat$nc5) & dat$nc5==dat$hhmember0_5))] <- 1
# replace nutrihh=1 if undernutritionhh==0 & (malnourishedwhh==0 & malnourished5hh==. & (whh>0 | hhmember0_5>0) & nc5>0 & nc5<. & hhmember0_5>nc5 & (nw_zha/(hhmember0_5-nc5)>=(1/2)))
dat$nutrihh[which(dat$undernutritionhh==0 & (dat$malnourishedwhh==0 & is.na(dat$malnourished5hh) & (dat$whh>0 | dat$hhmember0_5>0) & dat$nc5>0 & !is.na(dat$nc5) & dat$hhmember0_5>dat$nc5 & (dat$nw_zha/(dat$hhmember0_5-dat$nc5)>=(1/2))))] <- 1
# replace nutrihh=3 if nutrihh==4 & hhmember0_5>0 & nc5==hhmember0_5 & whh==0
dat$nutrihh[which(dat$nutrihh==4 & dat$hhmember0_5>0 & dat$nc5==dat$hhmember0_5 & dat$whh==0)] <- 3
# replace nutrihh=2 if nutrihh==4 & missing_bmihh>0 & missing_bmihh<.
dat$nutrihh[which(dat$nutrihh==4 & dat$missing_bmihh>0 & !is.na(dat$missing_bmihh))] <- 2
# replace nutrihh=2 if nutrihh==4 & missing_zhahh>0 & missing_zhahh<.
dat$nutrihh[which(dat$nutrihh==4 & dat$missing_zhahh>0 & !is.na(dat$missing_zhahh))] <- 2
# 
# label define nutrihh 1 "HH with nutrition information" 2 "HH with missing information on nutrition" 3 "HH with no eligible population for nutrition (no children 0-5y & no women 15-49y)" 4 "HH with eligible population for nutrition but measures not taken"
# label values nutrihh nutrihh
# 
# ** the following will take into account men Anthropometrics **
# 
# ren undernutritionhh undernutritionhh_old
names(dat)[which(names(dat)=="undernutritionhh")] <- "undernutritionhh_old"
# gen undernutritionhh = undernutritionhh_old
dat$undernutritionhh <- dat$undernutritionhh_old
# replace undernutritionhh=1 if malnourishedmhh==1
dat$undernutritionhh[which(dat$malnourishedmhh==1)] <- 1
# replace undernutritionhh=0 if malnourishedmhh==0 & nutrihh==3
dat$undernutritionhh[which(dat$malnourishedmhh==0 & dat$nutrihh==3)] <- 0
# 
# replace nutrihh=1 if undernutritionhh<. & undernutritionhh_old==.
dat$nutrihh[which(!is.na(dat$undernutritionhh) & is.na(dat$undernutrition_old))] <- 1
# 
# replace undernutritionhh=. if undernutritionhh_old==0 & missing_mbmi_finalhh==1 & undernutritionhh==0
dat$undernutritionhh[which(dat$undernutritionhh_old==0 & dat$missing_mbmi_finalhh==1 & dat$undernutritionhh==0)] <- NA
# replace nutrihh=2 if undernutritionhh_old==0 & missing_mbmi_finalhh==1 & undernutritionhh==.
dat$nutrihh[which(dat$undernutritionhh_old==0 & dat$missing_mbmi_finalhh==1 & is.na(dat$undernutritionhh))] <- 2
# 
# * special case
# replace nutrihh=1 if nutrihh==. & hv001==622 & hv002==15
dat$nutrihh[which(is.na(dat$nutrihh) & dat$hv001==622 & dat$hv002==15)] <- 1
# 
# gen attehh=1 if child_noattendhh<. & uno7_14hh==1
dat$attehh <- NA
dat$attehh[which(!is.na(dat$child_noattendhh) & dat$uno7_14==1)] <- 1
# replace attehh=2 if child_noattendhh==.
dat$attehh[which(is.na(dat$child_noattendhh))] <- 2
# replace attehh=3 if child_noattendhh==0 & uno7_14hh==0
dat$attehh[which(dat$child_noattendhh==0 & dat$uno7_14hh==0)] <- 3
# replace attehh=1 if attehh==3 & child_6_14hh>0 & child_6_14hh<. & child_7_14hh==0
dat$attehh[which(dat$attehh==3 & dat$child_6_14hh>0 & !is.na(dat$child_6_14hh) & dat$child_7_14hh==0)] <- 1
# 
# label define attehh 1 "HH with attendance information" 2 "HH with missing information on attendance" 3 "HH with no eligible population for attendace (no children 7-14y)"
# label values attehh attehh
# 
# 
# gen morthh=1 if mortality5hh<. & uno15_49hh==1
dat$morthh <- NA
dat$morthh[which(!is.na(dat$mortality5hh) & dat$uno15_49hh==1)] <- 1
# replace morthh=3 if mortality5hh==0 & uno15_49hh==0
dat$morthh[which(dat$mortality5hh==0 & dat$uno15_49hh==0)] <- 3
# replace morthh=4 if mortality5hh==.
dat$morthh[which(is.na(dat$mortality5hh))] <- 4
# 
# label define morthh 1 "HH with mortality information" 2 "HH with missing information on mortality" 3 "HH with no eligible population for mortality (no women 15-49y)" 4 "HH with eligible population for mortality but do not appear in fertility section"
# label values morthh morthh
# 
# replace mortality5hh=0 if mortality5hh==. & merge_women==1
dat$mortality5hh[which(is.na(dat$mortality5hh) & dat$merge_women==1)] <- 0
# replace mortality5hh=. if mortality5hh==0 &  whh>(2*nwomen15_49) & nwomen15_49<.
dat$mortality5hh[which(dat$mortality5hh==0 &  dat$whh>(2*dat$nwomen15_49) & !is.na(dat$nwomen15_49))] <- NA
# 
# replace morthh=2 if morthh==1 & mortality5hh==. &  whh>(2*nwomen15_49) & nwomen15_49<.
dat$morthh[which(dat$morthh==1 & is.na(dat$mortality5hh) &  dat$whh>(2*dat$nwomen15_49) & !is.na(dat$nwomen15_49))] <- 2
# compress
# sort hv001 hv002 hvidx
dat <- dat[order(dat$hv001,dat$hv002,dat$hvidx),]
# save "Uganda_MPI2_2011.dta", replace
save(dat,file="UG_MPI.RData")
# install.packages("haven")
library(haven)
write_dta(dat,"D:/Documents/Data/MPI/Uganda_MPI2_2011.dta",version=12)

load("C:/Users/alexm/Documents/Rwork/UG_MPI.RData")
library(Hmisc)
dat <- subset(dat,hv042==1)
psum <- function(...,na.rm=FALSE) { 
  rowSums(do.call(cbind,list(...)),na.rm=na.rm) } 

# egen ls=rowmiss(electricity_depriv sanitation_depriv water_depriv floor_depriv cookingfuel_depriv asset_depriv)
dat$ls = psum(
    is.na(dat$electricity_depriv)
    ,is.na(dat$sanitation_depriv)
    ,is.na(dat$water_depriv)
    ,is.na(dat$floor_depriv)
    ,is.na(dat$cookingfuel_depriv)
    ,is.na(dat$asset_depriv)
)
varlist_pov <- c(
  "electricity_depriv"
  ,"sanitation_depriv"
  ,"water_depriv"
  ,"floor_depriv"
  ,"cookingfuel_depriv"
  ,"asset_depriv"
  ,"educ_depriv"
  ,"child_noattendhh"
  ,"undernutritionhh"
  ,"mortality5hh"
)

# 
# 
# foreach var in electricity_depriv sanitation_depriv water_depriv floor_depriv cookingfuel_depriv asset_depriv {
#   gen w_`var'=1/18 if ls==0
# }
for(var in varlist_pov){
  varname <- paste0("w_",var)
  dat[,varname] <- NA
  dat[,varname][which(dat$ls==0)] <- (1/18)
}

# gen w_mortality5hh = 1/6 if morthh==1 & nutrihh==1
dat$w_mortality5hh <- NA
dat$w_mortality5hh[which(dat$morthh==1 & dat$nutrihh==1)] <- (1/6)
# replace w_mortality5hh = 1/3 if morthh==1 & nutrihh==3
dat$w_mortality5hh[which(dat$morthh==1 & dat$nutrihh==3)] <- (1/3)
# replace w_mortality5hh = 0 if morthh==3 & nutrihh==1
dat$w_mortality5hh[which(dat$morthh==3 & dat$nutrihh==1)] <- 0
# 
# gen w_undernutritionhh = 1/6 if morthh==1 & nutrihh==1
dat$w_undernutritionhh <- NA
dat$w_undernutritionhh[which(dat$morthh==1 & dat$nutrihh==1)] <- (1/6)
# replace w_undernutritionhh = 1/3 if morthh==3 & nutrihh==1
dat$w_undernutritionhh[which(dat$morthh==3 & dat$nutrihh==1)] <- (1/3)
# replace w_undernutritionhh = 0 if morthh==1 & nutrihh==3
dat$w_undernutritionhh[which(dat$morthh==1 & dat$nutrihh==3)] <- 0
# 
# 
# 
# gen w_child_noattendhh = 1/6 if attehh==1 | attehh==3
dat$w_child_noattendhh <- NA
dat$w_child_noattendhh[which(dat$attehh==1 | dat$attehh==3)] <- (1/6)
# 
# gen w_educ_depriv = 1/6 if educ_depriv<.
dat$w_educ_depriv = NA
dat$w_educ_depriv[which(!is.na(dat$educ_depriv))] <- (1/6)

# foreach var in `varlist_pov' {
# gen wg0_`var' = `var'*w_`var'
# }
for(var in varlist_pov){
  varname <- paste0("wg0_",var)
  weightname <- paste0("w_",var)
  dat[,varname] <- dat[,var]*dat[,weightname]
}

wg0_names <- names(dat)[which(substr(names(dat),1,3)=="wg0")]
dat$ci <- psum(dat[wg0_names],na.rm=TRUE)
dat$mpi.poor <- dat$ci>=(1/3)


### and now P20 ###
wd <- "D:/Documents/Data/P20_2013/meta"
setwd(wd)

povcalcuts <- read.csv("headcounts.csv",as.is=TRUE)
weighted.percentile <- function(x,w,prob,na.rm=TRUE){
  df <- data.frame(x,w)
  if(na.rm){
    df <- df[which(complete.cases(df)),]
  }
  #Sort
  df <- df[order(df$x),]
  sumw <- sum(df$w)
  df$cumsumw <- cumsum(df$w)
  #For each percentile
  cutList <- c()
  cutNames <-c()
  for(i in 1:length(prob)){
    p <- prob[i]
    pStr <- paste0(round(p*100,digits=2),"%")
    sumwp <- sumw*p
    df$above.prob <- df$cumsumw>=sumwp
    thisCut <- df$x[which(df$above.prob==TRUE)[1]]
    cutList <- c(cutList,thisCut)
    cutNames <- c(cutNames,pStr)
  }
  names(cutList) <- cutNames
  return(cutList)
}
# povcalcut <- subset(povcalcuts,filename=="ughr72dt")$hc
povcalcut <- 0.4891
povperc <- weighted.percentile(dat$wealths,dat$weight,prob=povcalcut)

dat$p20 <- (dat$wealths < povperc)

library(descr)
crosstab(dat$p20,dat$mpi.poor,prop.t=TRUE,weight=dat$weight)

p20.not.mpi <- subset(dat,p20==TRUE & mpi.poor==FALSE)
mpi.not.p20 <- subset(dat,p20==FALSE & mpi.poor==TRUE)

# describe(p20.not.mpi[varlist_pov])
# describe(mpi.not.p20[varlist_pov])

library(data.table)
setwd("D:/Documents/Data/MICSmeta/")
load("child.maternal.RData")
setnames(child.health,"skilled.attendant","ch.skilled.attendant")
valid.vacc <- c(TRUE,"TRUE","Yes","Oui","Sí")
no.vacc <- c(FALSE,"FALSE","No","Non","No sabe")
missing.vacc <- c("DK",NA,"Missing","NSP","Manquant")
recode.vacc <- function(x){
  if(is.na(x)){return(NA)}
  else if(x %in% valid.vacc){return(1)}
  else if(x %in% no.vacc){return(0)}
  else if(x %in% missing.vacc){return(NA)}
  return(NA)
}
child.health$vacc <- sapply(child.health$any.vacc,recode.vacc)
child.health.tab <- child.health[,.(
  ch.skilled.attendant=sum(ch.skilled.attendant==TRUE,na.rm=TRUE)>=1
  ,any.vacc=sum(vacc,na.rm=TRUE)>=1
),by=.(filename,cluster,household)]
maternal.health.tab <- maternal.health[,.(
  ceb=sum(ceb,na.rm=TRUE)
  ,cdead=sum(cdead,na.rm=TRUE)
  ,skilled.attendant=sum(skilled.attendant,na.rm=TRUE)>=1
  ,maternal.deaths=sum(maternal.deaths,na.rm=TRUE)
),by=.(filename,cluster,household)]

maternal.health.tab <- subset(maternal.health.tab,filename=="ughr60dt")
child.health.tab <- subset(child.health.tab,filename=="ughr60dt")
setnames(maternal.health.tab,"cluster","hv001")
setnames(maternal.health.tab,"household","hv002")
setnames(child.health.tab,"cluster","hv001")
setnames(child.health.tab,"household","hv002")
dat <- merge(dat,maternal.health.tab,by=c("hv001","hv002"),all.x=TRUE)
dat <- merge(dat,child.health.tab,by=c("hv001","hv002"),all.x=TRUE)

crosstab(dat$p20,dat$skilled.attendant,prop.r=TRUE,weight=dat$weight)
crosstab(dat$mpi.poor,dat$skilled.attendant,prop.r=TRUE,weight=dat$weight)

crosstab(dat$p20,dat$maternal.deaths>=1,prop.r=TRUE,weight=dat$weight)
crosstab(dat$mpi.poor,dat$maternal.deaths>=1,prop.r=TRUE,weight=dat$weight)

