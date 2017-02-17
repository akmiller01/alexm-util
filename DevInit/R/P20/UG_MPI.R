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
dat$information_depriv[which(dat$phone==1 | dat$radio==1 | dat$tv==1)] <- 0
# replace information_depriv = 1 if phone==0 & radio==0 & tv==0
dat$information_depriv[which(dat$phone==0 & dat$radio==0 & dat$tv==0)] <- 1
# 
# 
# gen mobility_depriv = . if mobility_miss==3 | mobility_miss==4 | mobility_miss==5
dat$mobility_depriv = NA
# replace mobility_depriv = 0 if bicycle==1 | motorcycle==1 | car_truck==1 | animal_cart==1 | motorboat==1
dat$mobility_depriv[which(dat$bicycle==1 | dat$motorcycle==1 | dat$car_truck==1 | dat$animal_cart==1 | dat$motorboat==1)] <- 0
# replace mobility_depriv = 1 if bicycle==0 & motorcycle==0 & car_truck==0 & animal_cart==0 & motorboat==0
dat$mobility_depriv[which(dat$bicycle==0 & dat$motorcycle==0 & dat$car_truck==0 & dat$animal_cart==0 & dat$motorboat==0)] <- 1
# 
# 
# gen livelihood_depriv = . if livelihood_miss==4 | livelihood_miss==5 | livelihood_miss==6 | livelihood_miss==7
# replace livelihood_depriv = 0 if refrigerator== 1 | land==1 | cattle==1 | horses==1 | goats==1 | sheeps==1 | chicken==1
# replace livelihood_depriv = 1 if refrigerator==0 & land==0 & cattle==0 & horses==0 & goats==0 & sheeps==0 & chicken==0
# 
# 
# gen asset_depriv = . if information_depriv==. | (information_depriv==1 & mobility_depriv==0 & livelihood_depriv==.) | (information_depriv==1 & mobility_depriv==. & livelihood_depriv==0)
# replace asset_depriv = 0 if information_depriv==0 & (mobility_depriv==0 | livelihood_depriv==0)
# replace asset_depriv = 1 if information_depriv==1 | (information_depriv==0 & mobility_depriv==1 & livelihood_depriv==1)
# 
# 
# 
# foreach var in sanitation water floor cookingfuel {
# 
# gen `var'_depriv=1 if `var'==0
# replace `var'_depriv=0 if `var'==1
# }
# 
# egen depriv = rsum(electricity_depriv sanitation_depriv water_depriv floor_depriv cookingfuel_depriv asset_depriv)
# replace depriv = . if electricity_depriv==. & sanitation_depriv==. & water_depriv==. & floor_depriv==. & cookingfuel_depriv==. & asset_depriv==.
# 
# gen ls_sample=1 if electricity_depriv<. & sanitation_depriv<. & water_depriv<. & floor_depriv<. & cookingfuel_depriv<. & asset_depriv<.
# egen livings_miss = rowmiss(electricity_depriv sanitation_depriv water_depriv floor_depriv cookingfuel_depriv asset_depriv)
# 
# keep if hv015==1 /* completed HH interview */
# sort hv001 hv002
# 
# 
# keep wealthi wealths weight hhmembers_hh urban electricity electricity_depriv water sanitation cookingfuel floor sanitation_depriv water_depriv floor_depriv cookingfuel_depriv depriv ls_sample livings_miss hv001 hv002 phone radio tv bicycle motorcycle motorboat car_truck animal_cart refrigerator land cattle horses goats sheeps chicken information_miss mobility_miss livelihood_miss information_depriv mobility_depriv livelihood_depriv asset_depriv
# 
# 
# save "HH_MPI2_MCC.dta", replace
# 
# 
# * BIRTHs' QUESTIONNAIRE TO CALCULATE WHEN THE DEATH OF A CHILD HAPPENED *
# 
# use "UGBR60FL.DTA", clear
# 
# sort b3
# 
# 
# gen usual_res=(v135==1)
# drop if usual_res==0
# 
# 
# gen date_death = b3+b7
# 
# gen mdead_from_survey = v008-date_death
# 
# gen ydead_from_survey = mdead_from_survey/12
# 
# 
# gen y = ydead_from_survey if b7<=60
# 
# label var ydead_from_survey "# years from survey that a child died"
# label var y "# years from survey that a child<=5y died"
# 
# gen b5r=0 if b5==1
# replace b5r=1 if b5==0
# 
# egen child_died=sum(b5r), by(v001 v002 v003)
# egen child_died2=rsum(v206 v207)
# 
# compare child_died child_died2
# * they are identical *
#   
#   
#   egen child_died5=sum(b5r) if ydead_from_survey<=5, by(v001 v002 v003)
# * only deaths in the past 5 years *
#   replace child_died5=0 if child_died5==. & child_died>=0 & child_died<.
# replace child_died5=. if b5r==1 & ydead_from_survey==.
# 
# sort v001 v002 v003 ydead_from_survey
# bys v001 v002 v003: gen uno=1 if _n==1
# keep if uno==1
# drop uno
# 
# tab child_died child_died5,m
# 
# egen child_diedhh=sum(child_died), by(v001 v002)
# *egen child_died2hh=sum(child_died2), by(v001 v002)
# egen child_died5hh=sum(child_died5), by(v001 v002)
# 
# 
# gen uno=1
# egen nwomen_birth_15_49=sum(uno), by(v001 v002)
# 
# drop uno
# bys v001 v002: gen uno=1 if _n==1
# 
# keep if uno==1
# drop uno
# 
# 
# keep child_died5 v001 v002 nwomen_birth_15_49 child_died5hh child_diedhh
# 
# ren v001 hv001
# ren v002 hv002
# 
# sort hv001 hv002
# 
# save "child_death_MPI2_MCC.dta", replace
# 
# 
# 
# * WOMEN'S QUESTIONNAIRE (15-49 y) *
# 
# use "UGIR60FL.DTA", clear
# 
# gen usual_res=(v135==1) /* keep only usual residents, drop visitors */
# drop if usual_res==0
# 
# egen children_died=rsum(v206 v207)
# egen children_diedhh=sum(children_died), by(v001 v002)
# 
# 
# gen uno=1
# egen nwomen15_49=sum(uno), by(v001 v002)
# 
# drop uno
# 
# 
# sort v001 v002 v003
# bys v001 v002: gen uno=1 if _n==1
# keep if uno==1
# drop uno
# 
# ren v001 hv001      
# ren v002 hv002
# ren v003 hv003
# 
# keep hv001 hv002 children_diedhh nwomen15_49
# 
# sort hv001 hv002
# save "UGIR60FL_MCC.dta", replace
# 
# * MEN'S QUESTIONNAIRE (15-59 y) *
#   
#   /*use "UGMR60FL.DTA", clear
# 
# * keep only usual residents, drop visitors
# gen usual_res=(mv135==1) * keep only usual residents, drop visitors
# drop if usual_res==0
# 
# egen mchildren_died=rsum(mv206 mv207)
# egen mchildren_diedhh=sum(mchildren_died), by(mv001 mv002)
# 
# sort mv001 mv002 mv003
# bys mv001 mv002: gen uno=1 if _n==1
# keep if uno==1
# drop uno
# 
# ren mv001 hv001      
# ren mv002 hv002
# ren mv003 hv003
# 
# keep hv001 hv002 mchildren_diedhh
# 
# sort hv001 hv002
# save "UGMR60FL_MCC.dta", replace
# */
#   
#   
#   * INDIVIDUALS' QUESTIONNAIRE *
# 
# use "UGPR60FL.DTA", clear
# 
# sort hv001 hv002 hv003
# merge hv001 hv002 using "UGIR60FL_MCC.dta"
# tab _merge
# ren _merge merge_women
# 
# /*sort hv001 hv002 hv003
# merge hv001 hv002 using "UGMR60FL_MCC.dta"
# tab _merge
# ren _merge merge_men*/
# 
# sort hv001 hv002
# merge hv001 hv002 using "HH_MPI2_MCC.dta"
# tab _merge
# drop _merge
# 
# 
# sort hv001 hv002
# merge hv001 hv002 using "child_death_MPI2_MCC.dta"
# tab _merge
# ren _merge merge_births
# 
# replace child_died5hh=0 if children_diedhh==0 & child_died5hh==.
# replace child_diedhh=0 if children_diedhh==0 & child_diedhh==.
# 
# 
# 
# gen age = hv105 if hv105<98
# gen sex = 1 if hv104==1
# replace sex = 0 if hv104==2
# egen hh=group(hv001 hv002)
# 
# gen uno=1
# egen hhmembers=sum(uno), by(hh)
# compare hhmembers hv009
# drop hhmembers uno
# 
# gen usual_res=(hv102==1)
# drop if usual_res==0
# 
# gen uno=1
# egen hhmembers=sum(uno), by(hh)
# 
# label var hhmembers "Number of HH members (only usual residents, excludes visitors)"
# label var hhmembers_hh "Number of HH members (usual residents + visitors)"
# 
# drop uno
# 
# compress
# sort hv001 hv002 hvidx
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
# replace yschooling =1 if hv106==1 & hv107==1
# replace yschooling =2 if hv106==1 & hv107==2
# replace yschooling =3 if hv106==1 & hv107==3
# replace yschooling =4 if hv106==1 & hv107==4
# replace yschooling =5 if hv106==1 & hv107==5
# replace yschooling =6 if hv106==1 & hv107==6
# replace yschooling =7 if hv106==1 & hv107==7
# 
# replace yschooling =8 if hv106==2 & hv107==1
# replace yschooling =9 if hv106==2 & hv107==2
# replace yschooling =10 if hv106==2 & hv107==3
# replace yschooling =11 if hv106==2 & hv107==4
# replace yschooling =12 if hv106==2 & hv107==5
# replace yschooling =13 if hv106==2 & hv107==6
# 
# replace yschooling =14 if hv106==3 & hv107==1
# replace yschooling =15 if hv106==3 & hv107==2
# replace yschooling =16 if hv106==3 & hv107==3
# replace yschooling =17 if hv106==3 & hv107==4
# replace yschooling =18 if hv106==3 & hv107==5
# 
# replace yschooling =7 if hv106==2 & hv107==0
# replace yschooling =13 if hv106==3 & hv107==0
# 
# replace yschooling=. if age<5
# 
# gen uno14=1 if age>=12 & age<. /* Number of HH members 12y or older */
# bys hhid: egen hhmember14 = sum(uno14)
# 
# ** 1- DEPRIVED IN EDUCATION **
# 
# gen yschoolingi=0 if yschooling<6 & age>=12 & age<.
# replace yschoolingi=1 if yschooling>=6 & yschooling<.
# replace yschoolingi=1 if yschooling==. & ((hv106==2 | hv106==3) & (hv107==98 | hv107==99))
# 
# 
# gen schooling_missing = 1 if ((hv106==1 & hv107==98) | hv106==8 | hv106==9) & age>=12 & age<.
# egen schooling_missinghh=sum(schooling_missing), by(hhid)
# 
# egen yeduchh=sum(yschoolingi), by(hhid) /* assumes that a member has <6 y schooling when there is missing info on that member's schooling  */
#   replace yeduchh=. if schooling_missinghh==hhmember14
# 
# * DEPRIVED IN EDUCATION INDICATOR *
#   gen educ_depriv=1 if yeduchh==0 
# replace educ_depriv=0 if yeduchh>=1 & yeduchh<.
# replace educ_depriv=. if yeduchh ==0 & schooling_missinghh>0 & schooling_missinghh>(1/3*hhmember14)
# 
# 
# 
# 
# ** 2- DEPRIVED IN SCHOOL ATTENDANCE 7-14Y (entrance age is 6 but we allow for 1 year of late enrollment) **
#   
#   gen attendance_missing=1 if hv121==9 & age>=7  & age<=14
# gen child_7_14=1 if age>=7 & age<=14
# 
# egen attendance_missinghh=sum(attendance_missing), by(hhid)
# egen child_7_14hh=sum(child_7_14), by(hhid)
# 
# gen child_6_14=1 if age>=6 & age<=14
# egen child_6_14hh=sum(child_6_14), by(hhid)
# 
# 
# 
# gen attendance_missing_final = 1 if attendance_missinghh==child_7_14hh & child_7_14hh>0 & attendance_missing==1 & child_7_14==1
# egen attendance_missing_finalhh = sum(attendance_missing_final), by(hhid)
# 
# gen child_noattend=1 if hv121==0 & age>=7 & age<=14 
# replace child_noattend=0 if hv121==2 & age>=7 & age<=14
# 
# * DEPRIVED IN SCHOOL ATTENDANCE 7-14Y *
#   egen child_noattendhh=sum(child_noattend), by(hhid)
# replace child_noattendhh=1 if child_noattendhh>1 & child_noattendhh<.
# *replace child_noattendhh=. if attendance_missing_finalhh>=1 & attendance_missing_finalhh<.
# *replace child_noattendhh=. if child_noattendhh==0 & attendance_missing_finalhh>=1 & attendance_missing_finalhh>(1/3*child_7_14hh)
# replace child_noattendhh=. if child_noattendhh==0 & attendance_missinghh>=1 & attendance_missinghh>(1/3*child_7_14hh)
# replace child_noattendhh=0 if child_6_14hh>0 & child_6_14hh<. & child_7_14hh==0
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
# egen whh=sum(w), by(hhid)
# 
# gen bmi=(ha2/(ha3^2))*100000
# replace bmi=. if (ha2>=9994 & ha2<=9999) | (ha3>=9994 & ha3<=9999) | ha40==9998 | ha40==9999
# 
# gen malnourishedw = 1 if bmi<18.5
# replace malnourishedw=0 if bmi>=18.5 & bmi<.
# egen malnourishedwhh=sum(malnourishedw), by (hhid) missing
# 
# gen missing_bmi=1 if ha40>=9996 & ha40<=9999
# egen missing_bmihh=sum(missing_bmi), by(hhid) missing
# 
# gen nomissing_bmi=1 if ha40<9996
# egen nomissing_bmihh=sum(nomissing_bmi), by(hhid) missing
# 
# gen noaplic_bmi=1 if ha40==.
# egen noaplic_bmihh=sum(noaplic_bmi), by(hhid) missing
# 
# gen womanbmi=1 if ha40<.
# egen womanbmihh=sum(womanbmi), by(hhid) missing
# 
# 
# gen missing_bmi_finalhh=1 if missing_bmihh>=1 & missing_bmihh<. & nomissing_bmihh==.
# 
# gen uno_bmi=1 if bmi<.
# egen nw_bmi=sum(uno_bmi), by (hhid)
# 
# ren malnourishedwhh nmalnourishedwhh
# gen malnourishedwhh=1 if nmalnourishedwhh>=1 & nmalnourishedwhh<.
# replace malnourishedwhh=0 if nmalnourishedwhh==0
# 
# replace missing_bmi_finalhh=1 if malnourishedwhh==0 & whh>(2*nw_bmi) & nw_bmi<.
# replace missing_bmihh=1 if malnourishedwhh==0 & whh>(2*nw_bmi) & nw_bmi<.
# replace malnourishedwhh=. if malnourishedwhh==0 & whh>(2*nw_bmi) & nw_bmi<.
# 
# 
# * MEN'S UNDERNUTRITION (BMI<18.5) *
#   capture drop m mhh
# 
# gen m=1 if sex==1 & age>=15 & age<=54
# egen mhh=sum(m), by(hhid)
# 
# gen mbmi=(hb2/(hb3^2))*100000
# replace mbmi=. if (hb2>=9994 & hb2<=9999) | (hb3>=9994 & hb3<=9999) | hb40==9998 | hb40==9999
# 
# gen malnourishedm = 1 if mbmi<18.5
# replace malnourishedm=0 if mbmi>=18.5 & mbmi<.
# egen malnourishedmhh=sum(malnourishedm), by (hhid) missing
# 
# gen missing_mbmi=1 if hb40>=9996 & hb40<=9999
# egen missing_mbmihh=sum(missing_mbmi), by(hhid) missing
# 
# gen nomissing_mbmi=1 if hb40<9996
# egen nomissing_mbmihh=sum(nomissing_mbmi), by(hhid) missing
# 
# gen noaplic_mbmi=1 if hb40==.
# egen noaplic_mbmihh=sum(noaplic_mbmi), by(hhid) missing
# 
# gen manbmi=1 if hb40<.
# egen manbmihh=sum(manbmi), by(hhid) missing
# 
# 
# gen missing_mbmi_finalhh=1 if missing_mbmihh>=1 & missing_mbmihh<. & nomissing_mbmihh==.
# 
# gen uno_mbmi=1 if mbmi<.
# egen nm_bmi=sum(uno_mbmi), by (hhid)
# 
# ren malnourishedmhh nmalnourishedmhh
# gen malnourishedmhh=1 if nmalnourishedmhh>=1 & nmalnourishedmhh<.
# replace malnourishedmhh=0 if nmalnourishedmhh==0
# 
# replace missing_mbmi_finalhh=1 if malnourishedmhh==0 & mhh>(2*nm_bmi) & nm_bmi<.
# replace missing_mbmihh=1 if malnourishedmhh==0 & mhh>(2*nm_bmi) & nm_bmi<.
# replace malnourishedmhh=. if malnourishedmhh==0 & mhh>(2*nm_bmi) & nm_bmi<.
# 
# 
# sort hv001 hv002 hvidx
# save "Uganda_MPI2_2011.dta", replace
# 
# /* clear
# set maxvar 7000
# 
# use "C:\Users\cecilia.calderon\HDRO_MCC\MPI\MPI_new calculations\Uganda 2011_DHS\Uganda_MPI2_2011.dta", clear
# 
# 
# keep hc27 hc1 hc2 hc3 hc13 hc15 weight hv001 hv002 hvidx
# 
# 
# * Variable "Sex" (1=male; 2=female)
# tab hc27, miss 
# gen gender = hc27
# desc gender
# tab gender
# 
# * Variable "Age"can be expresses it in months or days
# tab hc1, miss
# codebook hc1 
# gen age_months = hc1 
# desc age_months
# summ age_months 
# gen str6 ageunit = "months" 
# label var ageunit "Months"
# 
# *Variable "Sampling weight"
# ren weight sw
# desc sw
# summ sw
# 
# * Variable "body weight" - it must be in kilograms
# ta hc2, miss
# codebook hc2
# gen weight = hc2/10 if hc2<9000
# desc weight 
# replace weight=. if hc13>0
# tab hc2 hc13 if hc13>0, miss
# summ weight
# 
# * Variable "height" - it must be in centimetres
# ta hc3, miss
# codebook hc3
# gen height = hc3/10 if hc3<9000
# desc height 
# replace height=. if hc13>0
# tab hc3 hc13 if hc13>0, miss
# summ height
# 
# codebook hc15
# gen measure = "l" if hc15==1 
# replace measure = "h" if hc15==2 
# replace measure = " " if hc15==0 | hc15==9 | hc15==.
# desc measure
# tab measure
# 
# *Variable "Oedema" 
# gen oedema=.
# desc oedema
# 
# 
# 
# keep hv001 hv002 hvidx gender age_months ageunit weight height oedema measure sw
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
# igrowup_standard reflib datalib datalab gender agemons ageunit weight height measure head muac tri sub oedema sw 
# 
# keep hv001 hv002 hvidx agemons height _zwei _zlen _zbmi _zwfl _fwfl _flen _fwei _fbmi
# sort hv001 hv002 hvidx
# save "C:\Users\cecilia.calderon\HDRO_MCC\MPI\MPI_new calculations\Uganda 2011_DHS\UG_anthro.dta", replace */
# 
# 
# 
# use "Uganda_MPI2_2011.dta", clear
# merge hv001 hv002 hvidx using "UG_anthro.dta"
# tab _merge
# drop _merge
# 
# * UNDERNUTRITION USING Z SCORES CALCULATED WITH WHO CODE *
# 
# * Weight-for-age *
# gen zwa = _zwei if _fwei==0
# 
# gen malnourishedw5 = 1 if zwa<=-2
# replace malnourishedw5=0 if zwa>-2 & zwa<.
# egen malnourishedw5hh=sum(malnourishedw5), by (hhid) missing
# 
# gen missing_zwa=1 if _fwei==1 | (hc2>9000 & hc2<=9999) | (hc13>0 & hc13<=9)
# egen missing_zwahh=sum(missing_zwa), by(hhid) missing
# 
# gen nomissing_zwa=1 if _fwei==0
# egen nomissing_zwahh=sum(nomissing_zwa), by(hhid) missing
# 
# gen missing_zwa_finalhh=1 if missing_zwahh>=1 & missing_zwahh<. & nomissing_zwahh==.
# replace missing_zwa_finalhh=0 if missing_zwa_finalhh==.
# 
# gen uno_zwa=1 if zwa<.
# egen nw_zwa=sum(uno_zwa), by (hhid)
# 
# 
# * Height-for-age *
# gen zha = _zlen if _flen==0
# 
# gen malnourished5 = 1 if zha<=-2
# replace malnourished5=0 if zha>-2 & zha<.
# egen malnourished5hh=sum(malnourished5), by (hhid) missing
# 
# gen missing_zha=1 if _flen==1 | (hc3>9000 & hc3<=9999) | (hc13>0 & hc13<=9)
# egen missing_zhahh=sum(missing_zha), by(hhid) missing
# 
# gen nomissing_zha=1 if _flen==0
# egen nomissing_zhahh=sum(nomissing_zha), by(hhid) missing
# 
# gen missing_zha_finalhh=1 if missing_zhahh>=1 & missing_zhahh<. & nomissing_zhahh==.
# replace missing_zha_finalhh=0 if missing_zha_finalhh==.
# 
# gen uno_zha=1 if zha<.
# egen nw_zha=sum(uno_zha), by (hhid)
# 
# 
# ren malnourished5hh nmalnourished5hh
# gen malnourished5hh=1 if nmalnourished5hh>=1 & nmalnourished5hh<.
# replace malnourished5hh=0 if nmalnourished5hh==0
# 
# gen uno0_5=1 if age<=5
# bys hhid: egen hhmember0_5 = sum(uno0_5)
# 
# replace missing_zha_finalhh=1 if malnourished5hh==0 & hhmember0_5>(2*nw_zha) & nw_zha<.
# replace missing_zhahh=1 if malnourished5hh==0 & hhmember0_5>(2*nw_zha) & nw_zha<.
# replace malnourished5hh=. if malnourished5hh==0 & hhmember0_5>(2*nw_zha) & nw_zha<.
# 
# 
# * Number of children 5y but not necessarily <60 months
# capture drop uno5 
# capture drop nc5
# gen uno5=1 if age==5
# egen nc5=sum(uno5), by(hhid)
# 
# 
# gen undernutritionhh=0 if malnourishedwhh==0 & malnourished5hh==0
# replace undernutritionhh=0 if malnourishedwhh==0 & malnourished5hh==. & hhmember0_5==0
# replace undernutritionhh=0 if malnourished5hh==0 & malnourishedwhh==. & whh==0
# replace undernutritionhh=1 if (malnourishedwhh>0 & malnourishedwhh<.) | (malnourished5hh>0 & malnourished5hh<.)
# 
# replace undernutritionhh=0 if undernutritionhh==. & malnourishedwhh==0 & malnourished5hh==. & (whh>0 | hhmember0_5>0) & nc5>0 & nc5<. & nc5==hhmember0_5
# replace undernutritionhh=0 if undernutritionhh==. & malnourishedwhh==0 & malnourished5hh==. & (whh>0 | hhmember0_5>0) & nc5>0 & nc5<. & hhmember0_5>nc5 & (nw_zha/(hhmember0_5-nc5)>=(1/2))
# replace undernutritionhh=0 if undernutritionhh==. & malnourishedwhh==. & malnourished5hh==. & (whh>0 | hhmember0_5>0) & nc5>0 & nc5<. & hhmember0_5>nc5 & (nw_zha/(hhmember0_5-nc5)>=(1/2)) & (nw_bmi>=(1/2)*whh)
# 
# 
# 
# ** 2- DEPRIVED IN MORTALITY **
# 
# replace children_diedhh=0 if children_diedhh==. & whh==0
# replace child_died5hh=0 if whh==0 & child_died5hh==.
# replace child_diedhh=0 if whh==0 & child_diedhh==.
# 
# gen mortality5hh=0 if child_died5hh==0
# replace mortality5hh=1 if (child_died5hh>=1 & child_died5hh<.)
# 
# 
# /* the mortality indicator uses the available information. If there is a HH where one women lost a kid an another has missing info, the HH is considered deprived in this indicator. */
# 
# 
# 
# gen region = hv024
# 
# compress
# sort hv001 hv002 hvidx
# save "Uganda_MPI2_2011.dta", replace
# 
# 
# 
# * NON-ELIGIBLE POPULATION *
# 
# gen uno7_14=1 if age>=7 & age<=14
# bys hhid: egen hhmember7_14 = sum(uno7_14)
# 
# 
# gen uno15_49=1 if age>=15 & age<=49 & sex==0
# bys hhid: egen hhmember15_49 = sum(uno15_49)
# 
# 
# capture drop uno0_5 hhmember0_5
# gen uno0_5=1 if age<=5
# bys hhid: egen hhmember0_5 = sum(uno0_5)
# 
# 
# gen uno7_14hh=0 if hhmember7_14==0
# replace uno7_14hh=1 if hhmember7_14>=1 & hhmember7_14<.
# 
# gen uno15_49hh=0 if hhmember15_49==0
# replace uno15_49hh=1 if hhmember15_49>=1 & hhmember15_49<.
# 
# 
# gen uno0_5hh=0 if hhmember0_5==0
# replace uno0_5hh=1 if hhmember0_5>=1 & hhmember0_5<.
# 
# * Number of eligible children for anthropometrics
# egen nch05=sum(hv120), by(hhid)
# 
# gen nutrihh=1 if undernutritionhh<. & (malnourished5hh<. | malnourishedwhh<.)
# replace nutrihh=3 if undernutritionhh==. & malnourished5hh==. &  malnourishedwhh==. & uno15_49hh==0 & uno0_5hh==0
# replace nutrihh=4 if undernutritionhh==. & malnourished5hh==. &  malnourishedwhh==. & (uno15_49hh==1 | uno0_5hh==1)
# replace nutrihh=2 if undernutritionhh==. & nutrihh==.
# replace nutrihh=1 if undernutritionhh==0 & (malnourishedwhh==0 & malnourished5hh==. & (whh>0 | hhmember0_5>0) & nc5>0 & nc5<. & nc5==hhmember0_5)
# replace nutrihh=1 if undernutritionhh==0 & (malnourishedwhh==0 & malnourished5hh==. & (whh>0 | hhmember0_5>0) & nc5>0 & nc5<. & hhmember0_5>nc5 & (nw_zha/(hhmember0_5-nc5)>=(1/2)))
# replace nutrihh=3 if nutrihh==4 & hhmember0_5>0 & nc5==hhmember0_5 & whh==0
# replace nutrihh=2 if nutrihh==4 & missing_bmihh>0 & missing_bmihh<.
# replace nutrihh=2 if nutrihh==4 & missing_zhahh>0 & missing_zhahh<.
# 
# label define nutrihh 1 "HH with nutrition information" 2 "HH with missing information on nutrition" 3 "HH with no eligible population for nutrition (no children 0-5y & no women 15-49y)" 4 "HH with eligible population for nutrition but measures not taken"
# label values nutrihh nutrihh
# 
# ** the following will take into account men Anthropometrics **
# 
# ren undernutritionhh undernutritionhh_old
# gen undernutritionhh = undernutritionhh_old
# replace undernutritionhh=1 if malnourishedmhh==1
# replace undernutritionhh=0 if malnourishedmhh==0 & nutrihh==3
# 
# replace nutrihh=1 if undernutritionhh<. & undernutritionhh_old==.
# 
# replace undernutritionhh=. if undernutritionhh_old==0 & missing_mbmi_finalhh==1 & undernutritionhh==0
# replace nutrihh=2 if undernutritionhh_old==0 & missing_mbmi_finalhh==1 & undernutritionhh==.
# 
# * special case
# replace nutrihh=1 if nutrihh==. & hv001==622 & hv002==15
# 
# gen attehh=1 if child_noattendhh<. & uno7_14hh==1
# replace attehh=2 if child_noattendhh==.
# replace attehh=3 if child_noattendhh==0 & uno7_14hh==0
# replace attehh=1 if attehh==3 & child_6_14hh>0 & child_6_14hh<. & child_7_14hh==0
# 
# label define attehh 1 "HH with attendance information" 2 "HH with missing information on attendance" 3 "HH with no eligible population for attendace (no children 7-14y)"
# label values attehh attehh
# 
# 
# gen morthh=1 if mortality5hh<. & uno15_49hh==1
# replace morthh=3 if mortality5hh==0 & uno15_49hh==0
# replace morthh=4 if mortality5hh==.
# 
# label define morthh 1 "HH with mortality information" 2 "HH with missing information on mortality" 3 "HH with no eligible population for mortality (no women 15-49y)" 4 "HH with eligible population for mortality but do not appear in fertility section"
# label values morthh morthh
# 
# replace mortality5hh=0 if mortality5hh==. & merge_women==1
# replace mortality5hh=. if mortality5hh==0 &  whh>(2*nwomen15_49) & nwomen15_49<.
# 
# replace morthh=2 if morthh==1 & mortality5hh==. &  whh>(2*nwomen15_49) & nwomen15_49<.
# compress
# sort hv001 hv002 hvidx
# save "Uganda_MPI2_2011.dta", replace
# 
# 
# 
# 
# 
# ** DO FILE FROM OPHI TO COMPUTE MPI **
# 
# ***************************************************************************
# **** Multidimensional Poverty Measure *************************************
# **** OPHI-HDCA Summer School 2011 *****************************************
# **** 24 August to 3 September 2011 - Delft, the Netherlands ***************
# ***************************************************************************
# 
# ** ADAPTED BY CECILIA ON MAY 2013 **
# ** Uganda 2011 **
# 
# 
# 
# clear
# set more off
# set maxvar 10000
# set mem 500m
# cap log close
# 
# *** Replace here your path to the dataset***
# cd "C:\Users\cecilia.calderon\HDRO_MCC\MPI\MPI_new calculations\Uganda 2011_DHS\"
# 
# *** Replace here the name of your dataset***
# capture log close
# log using "M0_dofile_Uganda2011_MPI2.log", text replace
# use "Uganda_MPI2_2011.dta", clear
# keep if hv042==1
# 
# 
# *********************************************************************************
# ******* Define the deprivation  matrix   ****************************************
# *********************************************************************************
# 
# *** The dataset can be interpreted as the matrix of achivemnts 
# *** We will now crate the deprivation matrix based on the indicator definition; and deprivation cut-off defined by you
# *** during the last working session on the normative Issues in Multidimensional Poverty Measure
# *** In order to do this, we will generate a new variable for each indicator contained in our measure
# *** This will identify with 0 individuals or households who are not deprived in the specific indicator
# *** and identify with 1 individual deprived in the specific indicator.
# *** Note that you will need to replace with dot "." if the information is missing or inconsistent. 
# *** We will assess later the frequency of missing values.
# 
# 
# ** DEPRIVED IN DRINKING WATER
# ** (the individual -HH- is considered deprived in drinking water is: unprotected well, spring, river/lake/pond or other)
# 
# /*lookfor water
# codebook b2q12, tab(20)
# gen d_dwater=(b2q12==5 | b2q12==6 | b2q12==7 | b2q12==8)
# replace d_dwater=. if b2q12==.
# tab b2q12 d_dwater [aw=weight], miss
# label variable d_dwater "Deprived in drinking water"ic*/
# 
# ** Please follow the same logic for all indicators. 
# ** Note that for advance indicator you may have to use more complex algorithms
# 
# *****************************************************************************
# ********  Create a local variable with all your indicators          *********
# *****************************************************************************
# 
# local varlist_pov electricity_depriv sanitation_depriv water_depriv floor_depriv cookingfuel_depriv asset_depriv educ_depriv child_noattendhh undernutritionhh mortality5hh
# 
# 
# 
# *****************************************************************************
# ****            Define the weights              *****************************
# *****************************************************************************
# ** Create a loop for the variables with the same weight *********************
# *****************************************************************************
# 
# 
# egen ls=rowmiss(electricity_depriv sanitation_depriv water_depriv floor_depriv cookingfuel_depriv asset_depriv)
# 
# 
# gen hh_missing=0
# replace hh_missing=1 if educ_depriv==. | nutrihh==2 | attehh==2 | morthh==2 | (ls>0 & ls<.)
# 
# foreach var in electricity_depriv sanitation_depriv water_depriv floor_depriv cookingfuel_depriv asset_depriv {
# gen w_`var'=1/18 if ls==0
# *replace w_`var'=1/12 if ls==1
# }
# 
# /*foreach var in electricity_depriv sanitation_depriv water_depriv floor_depriv cookingfuel_depriv asset_depriv {
# replace `var'=0 if `var'==. & ls==1
# }*/
# /*foreach var in mortality5hh undernutritionhh child_noattendhh educ_depriv {
# local w_`var'=1/6
# }*/
# 
# 
# gen w_mortality5hh = 1/6 if morthh==1 & nutrihh==1
# replace w_mortality5hh = 1/3 if morthh==1 & nutrihh==3
# replace w_mortality5hh = 0 if morthh==3 & nutrihh==1
# 
# gen w_undernutritionhh = 1/6 if morthh==1 & nutrihh==1
# replace w_undernutritionhh = 1/3 if morthh==3 & nutrihh==1
# replace w_undernutritionhh = 0 if morthh==1 & nutrihh==3
# 
# 
# 
# gen w_child_noattendhh = 1/6 if attehh==1 | attehh==3
# 
# gen w_educ_depriv = 1/6 if educ_depriv<.
# 
# 
# 
# *******************************************************************
# *********     Define the weigthed deprivation g0* matrix       ****
# *******************************************************************
# 
# foreach var in `varlist_pov' {
# gen wg0_`var' = `var'*w_`var'
# }
# 
# ******************************************************************************
# *********** Compute the frequency of missing values for indicator ************
# ******************************************************************************
# 
# foreach var in `varlist_pov' {
# gen `var'_miss=1 if `var'==.
# replace `var'_miss=0 if `var'!=.
# }
# 
# sum *_miss
# 
# 
# ********************************************************************************
# *************   Define the (weighted) deprivation count vector "ci" ************
# ********************************************************************************
# 
# egen ci=rsum(wg0_*)
# label variable ci "Deprivation Count"
# 
# egen n_missing=rowmiss(wg0_*)
# label variable n_missing "Number of missing variables by individual"
# gen missing=(n_missing>0)
# label variable missing "Individual with missing variables"
# 
# *** Check sample drop due to missing values
# tab missing
# 
# *******************************************************************************
# ***** Create de identification vector (poor/non poor) *************************
# ***** and compute individual average of deprivation ***************************
# *******************************************************************************
# 
# forvalues x=1(1)10 {
# gen h_`x'0p=(ci>=`x'/10) 
# replace h_`x'0p=. if missing==1
# gen a_`x'0p=(ci) if h_`x'0p==1
# replace a_`x'0p=. if missing==1
# label var h_`x'0p "Condition of Multidimensional Poverty  k=`x'"
# label var a_`x'0p "Individual Average deprivation  k=`x'"
# }
# 
# sum h_10p-a_100p [aw=weight]
# 
# 
# 
# gen h_33p=(ci>=3.33/10) 
# replace h_33p=. if missing==1
# gen a_33p=(ci) if h_33p==1
# replace a_33p=. if missing==1
# label var h_33p "Condition of Multidimensional Poverty  k=33.3"
# label var a_33p "Individual Average deprivation  k=33.3"
# 
# sum h_33p a_33p [aw=weight]
# 
# 
# 
# 
# ********************************************************************************
#   ******* Compute raw headcounts        ******************************************
#   ********************************************************************************
#   
#   foreach var in `varlist_pov' {
# gen `var'_raw=(`var')
#                 replace `var'_raw=. if missing==1
# }
# 
# su *_raw  [iw=weight]
# 
# ******************************************************************************
#   *********** Compute Censored headcount and censored headocunt ****************
#   ******************************************************************************
#   ***** Please define in the first line your poverty cutoff, the example shows k=33.3 is 33.3%
# 
# local k=1/3
# foreach var in `varlist_pov' {
# *gen `var'_CH_`k'=(`var'==1 & h_`k'==1)
# *replace `var'_CH_`k'=. if missing==1
# 
# gen `var'_CH_33=(`var'==1 & h_33==1)
#                   replace `var'_CH_33=. if missing==1
#                   
# }
# 
# sum electricity_depriv_CH_33-educ_depriv_CH_33 [iw=weight]
# sum h_33 a_33 [iw=weight]
# 
# *h = HC
# *a = intensity
# 
# 
# 
# 
# capture drop cedu chealth cls
# 
# scalar drop _all
# 
# 
# 
# gen cedu = (child_noattendhh_CH_33 *  w_child_noattendhh) + (educ_depriv_CH_33 * w_educ_depriv)
# sum cedu [iw=weight]
# scalar mpi_edu=r(mean)
# 
# 
# gen chealth = (mortality5hh_CH_33 * w_mortality5hh) + (undernutritionhh_CH_33 * w_undernutritionhh)
# sum chealth [iw=weight]
# scalar mpi_health=r(mean)
# 
# 
# gen cls = (electricity_depriv_CH_33 * w_electricity_depriv) + (sanitation_depriv_CH_33 * w_sanitation_depriv) + (water_depriv_CH_33 * w_water_depriv) + (floor_depriv_CH_33 * w_floor_depriv) + (cookingfuel_depriv_CH_33 * w_cookingfuel_depriv) + (asset_depriv_CH_33 * w_asset_depriv)
# sum cls [iw=weight]
# scalar mpi_ls=r(mean)
# 
# 
# sum a_33p [iw=weight]
# scalar intensity = r(mean)
# sum h_33p [iw=weight]
# scalar headcount = r(mean)
# 
# 
# scalar mpi=headcount*intensity
# 
# scalar headcount100=headcount*100
# scalar intensity100=intensity*100
# 
# 
# scalar edu_contrib = (mpi_edu/mpi)*100
# scalar health_contrib = (mpi_health/mpi)*100
# scalar ls_contrib = (mpi_ls/mpi)*100
# 
# 
# capture drop uno
# gen uno=1 if h_33p<.
# 
# sum uno [aw=weight] if h_33p<.
# scalar pop=r(sum_w)
# 
# sum uno [aw=weight] if h_33p<. & ci>0 & ci<(1/3)
# scalar pop_vuln0=r(sum_w)
# 
# sum uno [aw=weight] if h_33p<. & ci>=0.2 & ci<(1/3)
# scalar pop_vuln20=r(sum_w)
# 
# sum uno [aw=weight] if h_33p<. & ci>=0.5
# scalar pop_severe=r(sum_w)
# 
# scalar vulnerable0=(pop_vuln0/pop)*100
# scalar vulnerable20=(pop_vuln20/pop)*100
# scalar severity=(pop_severe/pop)*100
# 
# 
# * Sample size
# 
# * HHs
# capture drop hh
# egen hh=group(hv001 hv002)
# sort age
# bys hh: gen n=_n
# gen nhh=1 if n==1
# 
# * Individuals
# 
# capture drop uno
# gen uno=1
# 
# sum uno 
# scalar ind_sample_tot=r(N)
# sum uno if h_33p<.
# scalar ind_sample_used=r(N)
# 
# sum uno [w=weight] if h_33p<.
# scalar ind_sample_used_w=r(sum)
# 
# 
# sum nhh
# scalar hh_sample_tot=r(N)
# sum nhh if h_33p<.
# scalar hh_sample_used=r(N)
# 
# sum nhh [w=weight] if h_33p<.
# scalar hh_sample_used_w=r(sum)
# 
# * Inequality
# sum a_33p [aw=weight]
# scalar cv_ineq_poor = r(sd)/r(mean)
# 
# sum ci [aw=weight] if h_33p<.
# scalar cv_ineq_hc = r(sd)/r(mean)
# 
# sum ci [aw=weight] if h_33p<. & ci>0
# scalar cv_ineq_positive = r(sd)/r(mean)
# 
# * CONTRIBUTION OF EACH DIMENSION FOR THE VULNERABLE POEPLE
# 
# foreach var in `varlist_pov' {
# gen `var'_CH_33vu=0 if h_33p<. & ci>=0.2 & ci<(1/3)
# replace `var'_CH_33vu=1 if `var'==1 & h_33p<. & ci>=0.2 & ci<(1/3)
# replace `var'_CH_33vu=. if missing==1
# }
# 
# 
# 
# gen ceduvu = (child_noattendhh_CH_33vu * w_child_noattendhh) + (educ_depriv_CH_33vu * w_educ_depriv)
# sum ceduvu [iw=weight] if h_33p<. & ci>=0.2 & ci<(1/3)
# scalar mpi_eduvu=r(mean)
# 
# 
# gen chealthvu = (mortality5hh_CH_33vu * w_mortality5hh) + (undernutritionhh_CH_33vu * w_undernutritionhh)
# sum chealthvu [iw=weight] if h_33p<. & ci>=0.2 & ci<(1/3)
# scalar mpi_healthvu=r(mean)
# 
# 
# gen clsvu = (asset_depriv_CH_33vu * w_asset_depriv) + (electricity_depriv_CH_33vu * w_electricity_depriv) + (sanitation_depriv_CH_33vu * w_sanitation_depriv) + (water_depriv_CH_33vu * w_water_depriv) + (floor_depriv_CH_33vu * w_floor_depriv) + (cookingfuel_depriv_CH_33vu * w_cookingfuel_depriv)
# sum clsvu [iw=weight] if h_33p<. & ci>=0.2 & ci<(1/3)
# scalar mpi_lsvu=r(mean)
# 
# sum ci [iw=weight] if h_33p<. & ci>=0.2 & ci<(1/3)
# scalar headcountvu = r(mean)
# 
# 
# *scalar mpivu=headcountvu*intensityvu
# 
# scalar headcountvu100=headcountvu*100
# *scalar intensityvu100=intensityvu*100
# 
# 
# scalar edu_contribvu = (mpi_eduvu/headcountvu100)*10000
# scalar health_contribvu = (mpi_healthvu/headcountvu100)*10000
# scalar ls_contribvu = (mpi_lsvu/headcountvu100)*10000
# 
# 
# scalar list mpi headcount100 intensity100 vulnerable0 vulnerable20 severity edu_contrib health_contrib ls_contrib edu_contribvu health_contribvu ls_contribvu cv_ineq_poor cv_ineq_positive cv_ineq_hc hh_sample_tot hh_sample_used hh_sample_used_w ind_sample_tot ind_sample_used ind_sample_used_w
# 
# 
# gen sample6y=1 if h_33p<.
# 
# gen ineligible=0
# replace ineligible=1 if (whh==0 & hhmember0_5==0) | (whh==0 & hhmember0_5>0 & hhmember0_5==nc5 & nutrihh==3)
# 
# 
# *gen sample_all=1 if (sample6y==. & (nutrihh==1 | nutrihh==2 | nutrihh==3)) | sample6y==1
# *replace sample_all=. if sample_all==1 & morthh==4 & hv010==0 & sample_original==.
# gen sample_all=1
# 
# sort hv001 hv002 hvidx
# save "Uganda_MPI2_2011_final.dta", replace
# 
# capture log close
# 
# 
# 
# ********************
#   *** RE-WEIGHTING ***
#   ********************
#   clear
# 
# use "Uganda_MPI2_2011_final.dta", clear
# keep if hv042==1
# 
# 
# gen age_group=1 if age>=0 & age<=5
# replace age_group=2 if age>=6 & age<=14
# replace age_group=3 if age>=15 & age<=49
# replace age_group=4 if age>=50 & age<.
# 
# set more off
# scalar drop _all
# 
# forvalues a=1(1)4 {
#   forvalues s=0(1)1 {
#     forvalues u=0(1)1 {
#       
#       sum sample_all [w=weight] if age_group==`a' & sex==`s' & urban==`u'
#       scalar sample_`a'_`s'_`u' = r(sum_w)
#       
#     }
#   }
# }
# 
# 
# ***************************************************************************
#   **** MISSING IN THE 3 VARIABLES (we don't observe sex, age and urban) *****
#                                    ***************************************************************************
#                                    
#                                    sum sample_all [w=weight] if (sex==. & urb==. & age_g==.) | (sex<. & urb<. & age_g<.)
#                                    scalar sample_mnm = r(sum_w)
#                                    sum sample_all [w=weight] if (sex<. & urb<. & age_g<.)
#                                    scalar sample_nm = r(sum_w)
#                                    
#                                    
#                                    scalar ratio_all = sample_mnm / sample_nm
#                                    
#                                    forvalues a=1(1)4 {
#                                    forvalues s=0(1)1 {
#                                    forvalues u=0(1)1 {
#                                    
#                                    scalar sample_nm_`a'_`s'_`u' = ratio_all * sample_`a'_`s'_`u'
#                                    
#                                    }
#                                    }
#                                    }
#                                    
#                                    
#                                    **************************************************************************************
#                                    **** MPI SAMPLE MISSING IN THE 3 VARIABLES (we don't observe sex, age and urban) *****
#   **************************************************************************************
#   
#   * NEW MPI SAMPLE
# 
# forvalues a=1(1)4 {
#   forvalues s=0(1)1 {
#     forvalues u=0(1)1 {
#       
#       sum sample_all [w=weight] if age_group==`a' & sex==`s' & urban==`u' & sample6y==1
#       scalar sample_`a'_`s'_`u'_6y = r(sum_w)
#       
#     }
#   }
# }
# 
# sum sample_all [w=weight] if sample6y==1 & ((sex==. & urb==. & age_g==.) | (sex<. & urb<. & age_g<.))
# scalar sample_mnm_6y = r(sum_w)
# sum sample_all [w=weight] if (sex<. & urb<. & age_g<.) & sample6y==1
# scalar sample_nm_6y = r(sum_w)
# 
# 
# scalar ratio_all_6y = sample_mnm_6y / sample_nm_6y
# 
# forvalues a=1(1)4 {
#   forvalues s=0(1)1 {
#     forvalues u=0(1)1 {
#       
#       scalar sample_nm_`a'_`s'_`u'_6y = ratio_all_6y * sample_`a'_`s'_`u'_6y
#       
#     }
#   }
# }
# 
# 
# **************************************************************************************
#   **** MISSING IN TWO VARIABLES AT A TIME (we only observe one: sex, age or urban) *****
#   **************************************************************************************
#   
#   * ONLY OBSERVE SEX
# forvalues s = 0(1)1 {
#   sum sample_all [w=weight] if sex==`s' & (sex<. & urb==. & age_g==.)
#   scalar sample_m_`s's_onlysex = r(sum_w)
#   
#   sum sample6y [w=weight] if sex==`s' & (sex<. & urb==. & age_g==.)
#   scalar sample_m_`s's_onlysex_6y = r(sum_w)
# }
# 
# 
# forvalues s = 0(1)1 {
#   scalar samplenew_`s's = sample_nm_4_`s'_1 + sample_nm_4_`s'_0 + sample_nm_3_`s'_1 + sample_nm_3_`s'_0 + sample_nm_2_`s'_1 + sample_nm_2_`s'_0 + sample_nm_1_`s'_1 + sample_nm_1_`s'_0
#   
#   scalar samplenew_`s's_onlysex = samplenew_`s's + sample_m_`s's_onlysex
#   
#   
#   scalar samplenew_`s's_6y = sample_nm_4_`s'_1_6y + sample_nm_4_`s'_0_6y + sample_nm_3_`s'_1_6y + sample_nm_3_`s'_0_6y + sample_nm_2_`s'_1_6y + sample_nm_2_`s'_0_6y + sample_nm_1_`s'_1_6y + sample_nm_1_`s'_0_6y
#   
#   scalar samplenew_`s's_onlysex_6y = samplenew_`s's_6y + sample_m_`s's_onlysex_6y
# }
# 
# 
# forvalues s = 0(1)1 {
#   scalar ratio_`s's_onlysex = samplenew_`s's_onlysex / samplenew_`s's
#   scalar ratio_`s's_onlysex_6y = samplenew_`s's_onlysex_6y / samplenew_`s's_6y
# }
# 
# forvalues a=1(1)4 {
#   forvalues s=0(1)1 {
#     forvalues u=0(1)1 {
#       
#       scalar sample_onlysex_`a'a_`s's_`u'u_all = sample_nm_`a'_`s'_`u' * ratio_`s's_onlysex
#       scalar sample_onlysex_`a'a_`s's_`u'u_6y = sample_nm_`a'_`s'_`u'_6y * ratio_`s's_onlysex_6y
#       
#     }
#   }
#   }
# 
# * ONLY OBSERVE AGE
# forvalues a = 1(1)4 {
#   sum sample_all [w=weight] if age_g==`a' & (sex==. & urb==. & age_g<.)
#   scalar sample_m_`a'a_onlyage = r(sum_w)
#   
#   sum sample6y [w=weight] if age_g==`a' & (sex==. & urb==. & age_g<.)
#   scalar sample_m_`a'a_onlyage_6y = r(sum_w)
# }
# 
# 
# forvalues a = 1(1)4 {
#   scalar samplenew_`a'a = sample_onlysex_`a'a_1s_1u_all + sample_onlysex_`a'a_1s_0u_all + sample_onlysex_`a'a_0s_1u_all + sample_onlysex_`a'a_0s_0u_all
#   
#   scalar samplenew_`a'a_onlyage = samplenew_`a'a + sample_m_`a'a_onlyage
#   
#   
#   scalar samplenew_`a'a_6y = sample_onlysex_`a'a_1s_1u_6y + sample_onlysex_`a'a_1s_0u_6y + sample_onlysex_`a'a_0s_1u_6y + sample_onlysex_`a'a_0s_0u_6y
#   
#   scalar samplenew_`a'a_onlyage_6y = samplenew_`a'a_6y + sample_m_`a'a_onlyage_6y
# }
# 
# 
# forvalues a = 1(1)4 {
# scalar ratio_`a'a_onlyage = samplenew_`a'a_onlyage / samplenew_`a'a
# scalar ratio_`a'a_onlyage_6y = samplenew_`a'a_onlyage_6y / samplenew_`a'a_6y
# }
# 
# forvalues a=1(1)4 {
# forvalues s=0(1)1 {
# forvalues u=0(1)1 {
# 
# scalar sample_onlyage_`a'a_`s's_`u'u_all = sample_onlysex_`a'a_`s's_`u'u_all * ratio_`a'a_onlyage
# scalar sample_onlyage_`a'a_`s's_`u'u_6y = sample_onlysex_`a'a_`s's_`u'u_6y * ratio_`a'a_onlyage_6y
# 
# }
# }
# }
# 
# * ONLY OBSERVE URBAN
# forvalues u = 0(1)1 {
# sum sample_all [w=weight] if urban==`u' & (sex==. & urb<. & age_g==.)
# scalar sample_m_`u'u_onlyurb = r(sum_w)
# 
# sum sample6y [w=weight] if urban==`u' & (sex==. & urb<. & age_g==.)
# scalar sample_m_`u'u_onlyurb_6y = r(sum_w)
# }
# 
# 
# forvalues u = 0(1)1 {
# scalar samplenew_`u'u = sample_onlyage_4a_1s_`u'u_all + sample_onlyage_4a_0s_`u'u_all + sample_onlyage_3a_1s_`u'u_all + sample_onlyage_3a_0s_`u'u_all + sample_onlyage_2a_1s_`u'u_all + sample_onlyage_2a_0s_`u'u_all + sample_onlyage_1a_1s_`u'u_all + sample_onlyage_1a_0s_`u'u_all
# 
# scalar samplenew_`u'u_onlyurb = samplenew_`u'u + sample_m_`u'u_onlyurb
# 
# 
# scalar samplenew_`u'u_6y = sample_onlyage_4a_1s_`u'u_6y + sample_onlyage_4a_0s_`u'u_6y + sample_onlyage_3a_1s_`u'u_6y + sample_onlyage_3a_0s_`u'u_6y + sample_onlyage_2a_1s_`u'u_6y + sample_onlyage_2a_0s_`u'u_6y + sample_onlyage_1a_1s_`u'u_6y + sample_onlyage_1a_0s_`u'u_6y
# 
# scalar samplenew_`u'u_onlyurb_6y = samplenew_`u'u_6y + sample_m_`u'u_onlyurb_6y
# }
# 
# 
# forvalues u = 0(1)1 {
# scalar ratio_`u'u_onlyurb = samplenew_`u'u_onlyurb / samplenew_`u'u
# scalar ratio_`u'u_onlyurb_6y = samplenew_`u'u_onlyurb_6y / samplenew_`u'u_6y
# }
# 
# forvalues a=1(1)4 {
# forvalues s=0(1)1 {
# forvalues u=0(1)1 {
# 
# scalar sample_onlyurb_`a'a_`s's_`u'u_all = sample_onlyage_`a'a_`s's_`u'u_all * ratio_`u'u_onlyurb
# scalar sample_onlyurb_`a'a_`s's_`u'u_6y = sample_onlyage_`a'a_`s's_`u'u_6y * ratio_`u'u_onlyurb_6y
# 
# }
# }
# }
# 
# 
# ***********************************************************************
# **** MISSING IN ONE VARIABLE AT A TIME (either sex, age or urban) *****
# ***********************************************************************
# * missing sex
# forvalues a=1(1)4 {
# forvalues u=0(1)1 {
# 
# sum sample_all [w=weight] if age_group==`a' & urban==`u' & (sex==. & age_g<. & urban<.)
# scalar sample_m_`a'a_`u'u = r(sum_w)
# 
# sum sample6y [w=weight] if age_group==`a' & urban==`u' & (sex==. & age_g<. & urban<.)
# scalar sample_m_`a'a_`u'u_6y = r(sum_w)
# 
# }
# }
# 
# * SEX MISSING: COLLAPSE THE 16 GROUPS INTO 8 GROUPS (AGE-URBAN)
# forvalues a=1(1)4 {
# forvalues u=0(1)1 {
# 
# scalar samplenew_`a'a_`u'u = sample_onlyurb_`a'a_1s_`u'u_all + sample_onlyurb_`a'a_0s_`u'u_all
# scalar samplenew_`a'a_`u'u_sexm = samplenew_`a'a_`u'u + sample_m_`a'a_`u'u
# 
# scalar samplenew_`a'a_`u'u_6y = sample_onlyurb_`a'a_1s_`u'u_6y + sample_onlyurb_`a'a_0s_`u'u_6y
# scalar samplenew_`a'a_`u'u_sexm_6y = samplenew_`a'a_`u'u_6y + sample_m_`a'a_`u'u_6y
# 
# }
# }
# 
# 
# forvalues a=1(1)4 {
# forvalues u = 0(1)1 {
# scalar ratio_`a'a_`u'u = samplenew_`a'a_`u'u_sexm / samplenew_`a'a_`u'u
# scalar ratio_`a'a_`u'u_6y = samplenew_`a'a_`u'u_sexm_6y / samplenew_`a'a_`u'u_6y
# }
# }
# 
# 
# forvalues a=1(1)4 {
# forvalues s=0(1)1 {
# forvalues u=0(1)1 {
# 
# scalar sample_sexmis_`a'a_`s's_`u'u_all = sample_onlyurb_`a'a_`s's_`u'u_all * ratio_`a'a_`u'u
# scalar sample_sexmis_`a'a_`s's_`u'u_6y = sample_onlyurb_`a'a_`s's_`u'u_6y * ratio_`a'a_`u'u_6y
# 
# }
# }
# }
# 
# 
# * missing age
# forvalues s=0(1)1 {
# forvalues u=0(1)1 {
# 
# sum sample_all [w=weight] if sex==`s' & urban==`u' & (sex<. & age_g==. & urban<.)
# scalar sample_m_`s's_`u'u = r(sum_w)
# 
# sum sample6y [w=weight] if sex==`s' & urban==`u' & (sex<. & age_g==. & urban<.)
# scalar sample_m_`s's_`u'u_6y = r(sum_w)
# 
# }
# }
# 
# 
# * AGE MISSING: COLLAPSE THE 16 GROUPS INTO 4 GROUPS (SEX-URBAN)
# forvalues s=0(1)1 {
# forvalues u=0(1)1 {
# 
# scalar samplenew_`s's_`u'u = sample_sexmis_4a_`s's_`u'u_all + sample_sexmis_3a_`s's_`u'u_all + sample_sexmis_2a_`s's_`u'u_all + sample_sexmis_1a_`s's_`u'u_all
# scalar samplenew_`s's_`u'u_agem = samplenew_`s's_`u'u + sample_m_`s's_`u'u
# 
# scalar samplenew_`s's_`u'u_6y = sample_sexmis_4a_`s's_`u'u_6y + sample_sexmis_3a_`s's_`u'u_6y + sample_sexmis_2a_`s's_`u'u_6y + sample_sexmis_1a_`s's_`u'u_6y
# scalar samplenew_`s's_`u'u_agem_6y = samplenew_`s's_`u'u_6y + sample_m_`s's_`u'u_6y
# 
# }
# }
# 
# 
# forvalues s=0(1)1 {
# forvalues u = 0(1)1 {
# scalar ratio_`s's_`u'u = samplenew_`s's_`u'u_agem / samplenew_`s's_`u'u
# scalar ratio_`s's_`u'u_6y = samplenew_`s's_`u'u_agem_6y / samplenew_`s's_`u'u_6y
# }
# }
# 
# 
# forvalues a=1(1)4 {
# forvalues s=0(1)1 {
# forvalues u=0(1)1 {
# 
# scalar sample_agemis_`a'a_`s's_`u'u_all = sample_sexmis_`a'a_`s's_`u'u_all * ratio_`s's_`u'u
# scalar sample_agemis_`a'a_`s's_`u'u_6y = sample_sexmis_`a'a_`s's_`u'u_6y * ratio_`s's_`u'u_6y
# 
# }
# }
# }
# 
# 
# 
# * missing urban
# forvalues a=1(1)4 {
# forvalues s=0(1)1 {
# 
# sum sample_all [w=weight] if age_g==`a' & sex==`s' & (sex<. & age_g<. & urban==.)
# scalar sample_m_`a'a_`s's = r(sum_w)
# 
# sum sample6y [w=weight] if age_g==`a' & sex==`s' & (sex<. & age_g<. & urban==.)
# scalar sample_m_`a'a_`s's_6y = r(sum_w)
# 
# }
# }
# 
# 
# * URBAN MISSING: COLLAPSE THE 16 GROUPS INTO 8 GROUPS (AGE-SEX)
# forvalues a=1(1)4 {
# forvalues s=0(1)1 {
# 
# scalar samplenew_`a'a_`s's = sample_agemis_`a'a_`s's_1u_all + sample_agemis_`a'a_`s's_0u_all
# scalar samplenew_`a'a_`s's_urbm = samplenew_`a'a_`s's + sample_m_`a'a_`s's
# 
# scalar samplenew_`a'a_`s's_6y = sample_agemis_`a'a_`s's_1u_6y + sample_agemis_`a'a_`s's_0u_6y
# scalar samplenew_`a'a_`s's_urbm_6y = samplenew_`a'a_`s's_6y + sample_m_`a'a_`s's_6y
# 
# }
# }
# 
# 
# forvalues a=1(1)4 {
# forvalues s = 0(1)1 {
# scalar ratio_`a'a_`s's = samplenew_`a'a_`s's_urbm / samplenew_`a'a_`s's
# scalar ratio_`a'a_`s's_6y = samplenew_`a'a_`s's_urbm_6y / samplenew_`a'a_`s's_6y
# }
# }
# 
# 
# forvalues a=1(1)4 {
# forvalues s=0(1)1 {
# forvalues u=0(1)1 {
# 
# scalar sample_urbmis_`a'a_`s's_`u'u_all = sample_agemis_`a'a_`s's_`u'u_all * ratio_`a'a_`s's
# scalar sample_urbmis_`a'a_`s's_`u'u_6y = sample_agemis_`a'a_`s's_`u'u_6y * ratio_`a'a_`s's_6y
# 
# }
# }
# }
# 
# **********************************************************************
# *** CORRECTING THE ORIGINAL WEIGHTS FOR EVERYONE IN THE MPI SAMPLE ***
# **********************************************************************
# 
# * COLLAPSE THE CORRECTED CELLS INTO DIFFERENT GROUPS ACCORDING TO THEIR MISSINGS TO CORRECT THE WEIGHTS OF THE OBSERVATIONS IN THE MPI SAMPLE WITH MISSINGS IN AGE, SEX AND OR URBAN
# 
# 
# * SEX, AGE AND URBAN MISSING
# scalar total_all = sample_urbmis_4a_1s_1u_all + sample_urbmis_4a_1s_0u_all + sample_urbmis_4a_0s_1u_all + sample_urbmis_4a_0s_0u_all + sample_urbmis_3a_1s_1u_all + sample_urbmis_3a_1s_0u_all + sample_urbmis_3a_0s_1u_all + sample_urbmis_3a_0s_0u_all + sample_urbmis_2a_1s_1u_all + sample_urbmis_2a_1s_0u_all + sample_urbmis_2a_0s_1u_all + sample_urbmis_2a_0s_0u_all + sample_urbmis_1a_1s_1u_all + sample_urbmis_1a_1s_0u_all + sample_urbmis_1a_0s_1u_all + sample_urbmis_1a_0s_0u_all
# 
# scalar total_6y = sample_urbmis_4a_1s_1u_6y + sample_urbmis_4a_1s_0u_6y + sample_urbmis_4a_0s_1u_6y + sample_urbmis_4a_0s_0u_6y + sample_urbmis_3a_1s_1u_6y + sample_urbmis_3a_1s_0u_6y + sample_urbmis_3a_0s_1u_6y + sample_urbmis_3a_0s_0u_6y + sample_urbmis_2a_1s_1u_6y + sample_urbmis_2a_1s_0u_6y + sample_urbmis_2a_0s_1u_6y + sample_urbmis_2a_0s_0u_6y + sample_urbmis_1a_1s_1u_6y + sample_urbmis_1a_1s_0u_6y + sample_urbmis_1a_0s_1u_6y + sample_urbmis_1a_0s_0u_6y
# 
# scalar ratio_total = total_all / total_6y
# 
# 
# * ONLY OBSERVE SEX
# forvalues s = 0(1)1 {
# scalar total_sex`s'_all = sample_urbmis_4a_`s's_1u_all + sample_urbmis_4a_`s's_0u_all + sample_urbmis_3a_`s's_1u_all + sample_urbmis_3a_`s's_0u_all + sample_urbmis_2a_`s's_1u_all + sample_urbmis_2a_`s's_0u_all + sample_urbmis_1a_`s's_1u_all + sample_urbmis_1a_`s's_0u_all
# 
# scalar total_sex`s'_6y = sample_urbmis_4a_`s's_1u_6y + sample_urbmis_4a_`s's_0u_6y + sample_urbmis_3a_`s's_1u_6y + sample_urbmis_3a_`s's_0u_6y + sample_urbmis_2a_`s's_1u_6y + sample_urbmis_2a_`s's_0u_6y + sample_urbmis_1a_`s's_1u_6y + sample_urbmis_1a_`s's_0u_6y
# 
# scalar ratio_total_sex`s' = total_sex`s'_all / total_sex`s'_6y
# }
# 
# * ONLY OBSERVE AGE
# forvalues a = 1(1)4 {
# scalar total_age`a'_all = sample_urbmis_`a'a_1s_1u_all + sample_urbmis_`a'a_1s_0u_all + sample_urbmis_`a'a_0s_1u_all + sample_urbmis_`a'a_0s_0u_all
# 
# scalar total_age`a'_6y = sample_urbmis_`a'a_1s_1u_6y + sample_urbmis_`a'a_1s_0u_6y + sample_urbmis_`a'a_0s_1u_6y + sample_urbmis_`a'a_0s_0u_6y
# 
# scalar ratio_total_age`a' = total_age`a'_all / total_age`a'_6y
# }
# 
# * ONLY OBSERVE URBAN
# forvalues u = 0(1)1 {
# scalar total_urb`u'_all = sample_urbmis_4a_1s_`u'u_all + sample_urbmis_4a_0s_`u'u_all + sample_urbmis_3a_1s_`u'u_all + sample_urbmis_3a_0s_`u'u_all + sample_urbmis_2a_1s_`u'u_all + sample_urbmis_2a_0s_`u'u_all + sample_urbmis_1a_1s_`u'u_all + sample_urbmis_1a_0s_`u'u_all
# 
# scalar total_urb`u'_6y = sample_urbmis_4a_1s_`u'u_6y + sample_urbmis_4a_0s_`u'u_6y + sample_urbmis_3a_1s_`u'u_6y + sample_urbmis_3a_0s_`u'u_6y + sample_urbmis_2a_1s_`u'u_6y + sample_urbmis_2a_0s_`u'u_6y + sample_urbmis_1a_1s_`u'u_6y + sample_urbmis_1a_0s_`u'u_6y
# 
# scalar ratio_total_urb`u' = total_urb`u'_all / total_urb`u'_6y
# }
# 
# * SEX MISSING ONLY
# forvalues a = 1(1)4 {
# forvalues u = 0(1)1 {
# 
# scalar total_sexmis_`a'a_`u'u_all = sample_urbmis_`a'a_1s_`u'u_all + sample_urbmis_`a'a_0s_`u'u_all
# 
# scalar total_sexmis_`a'a_`u'u_6y = sample_urbmis_`a'a_1s_`u'u_6y + sample_urbmis_`a'a_0s_`u'u_6y
# 
# scalar ratio_total_sexmis_`a'a_`u'u = total_sexmis_`a'a_`u'u_all / total_sexmis_`a'a_`u'u_6y
# 
# }
# }
# 
# * AGE MISSING ONLY
# forvalues s = 0(1)1 {
# forvalues u = 0(1)1 {
# 
# scalar total_agemis_`s's_`u'u_all = sample_urbmis_4a_`s's_`u'u_all + sample_urbmis_3a_`s's_`u'u_all + sample_urbmis_2a_`s's_`u'u_all + sample_urbmis_1a_`s's_`u'u_all
# 
# scalar total_agemis_`s's_`u'u_6y = sample_urbmis_4a_`s's_`u'u_6y + sample_urbmis_3a_`s's_`u'u_6y + sample_urbmis_2a_`s's_`u'u_6y + sample_urbmis_1a_`s's_`u'u_6y
# 
# scalar ratio_total_agemis_`s's_`u'u = total_agemis_`s's_`u'u_all / total_agemis_`s's_`u'u_6y
# 
# }
# }
# 
# * URBAN MISSING ONLY
# forvalues a = 1(1)4 {
# forvalues s = 0(1)1 {
# 
# scalar total_urbmis_`a'a_`s's_all = sample_urbmis_`a'a_`s's_1u_all + sample_urbmis_`a'a_`s's_0u_all
# 
# scalar total_urbmis_`a'a_`s's_6y = sample_urbmis_`a'a_`s's_1u_6y + sample_urbmis_`a'a_`s's_0u_6y
# 
# scalar ratio_total_urbmis_`a'a_`s's = total_urbmis_`a'a_`s's_all / total_urbmis_`a'a_`s's_6y
# 
# }
# }
# 
# * DEFINE THE RATIOS FOR NON-MISSING VALUES IN SEX, AGE AND URBAN
# forvalues a=1(1)4 {
# forvalues s=0(1)1 {
# forvalues u=0(1)1 {
# scalar sample_all_`a'_`s'_`u'_w = sample_urbmis_`a'a_`s's_`u'u_all / sample_urbmis_`a'a_`s's_`u'u_6y
# }
# }
# }
# 
# capture drop weight_w
# gen weight_w = weight if sample6y==1 
# *& age_group<. & sex<. & urban<.
# 
# forvalues a=1(1)4 {
# forvalues s=0(1)1 {
# forvalues u=0(1)1 {
# replace weight_w = weight * sample_all_`a'_`s'_`u'_w if age_group==`a' & sex==`s' & urban==`u' & sample6y==1
# }
# }
# }
# 
# replace weight_w = weight * ratio_total if sample6y==1 & age_group==. & sex==. & urban==.
# 
# forvalues s=0(1)1 {
# replace weight_w = weight * ratio_total_sex`s' if sample6y==1 & age_group==. & sex==`s' & urban==.
# }
# 
# forvalues a=1(1)4 {
# replace weight_w = weight * ratio_total_age`a' if sample6y==1 & age_group==`a' & sex==. & urban==.
# }
# 
# forvalues u=0(1)1 {
# replace weight_w = weight * ratio_total_urb`u' if sample6y==1 & age_group==. & sex==. & urban==`u'
# }
# 
# forvalues a = 1(1)4 {
# forvalues u = 0(1)1 {
# replace weight_w = weight * ratio_total_sexmis_`a'a_`u'u if sample6y==1 & age_group==`a' & sex==. & urban==`u'
# }
# }
# 
# forvalues s = 0(1)1 {
#   forvalues u = 0(1)1 {
#     replace weight_w = weight * ratio_total_agemis_`s's_`u'u if sample6y==1 & age_group==. & sex==`s' & urban==`u'
#   }
#   }
#     
#     forvalues a = 1(1)4 {
#     forvalues s = 0(1)1 {
#     replace weight_w = weight * ratio_total_urbmis_`a'a_`s's if sample6y==1 & age_group==`a' & sex==`s' & urban==.
#     }
#     }
# 
# sort hv001 hv002 hhid
# 
# save "Uganda_MPI2_2011_final_w.dta", replace
# 
# 
# * OPEN DATA WITH NEW WEIGHT FOR HH USED IN NEW MPI (CORRECTS FOR THE EXCLUSION OF HHs WITHOUT ELIGIBLE POP)
# 
# ** ADAPTED BY CECILIA ON JULY 8 2013 **
#   ** Uganda 2011 **
#   
#   
#   
#   clear
# set more off
# set maxvar 10000
# set mem 500m
# cap log close
# 
# *** Replace here your path to the dataset***
#   cd "C:\Users\cecilia.calderon\HDRO_MCC\MPI\MPI_new calculations\Uganda 2011_DHS\"
# 
# *** Replace here the name of your dataset***
# capture log close
# log using "M0_dofile_Uganda2011_MPI2_w.log", text replace
# use "Uganda_MPI2_2011_final_w.dta", clear
# keep if hv042==1
# 
# capture drop ls-uno
# 
# *********************************************************************************
# ******* Define the deprivation  matrix   ****************************************
# *********************************************************************************
# 
# *** The dataset can be interpreted as the matrix of achivemnts 
# *** We will now crate the deprivation matrix based on the indicator definition; and deprivation cut-off defined by you
# *** during the last working session on the normative Issues in Multidimensional Poverty Measure
# *** In order to do this, we will generate a new variable for each indicator contained in our measure
# *** This will identify with 0 individuals or households who are not deprived in the specific indicator
# *** and identify with 1 individual deprived in the specific indicator.
# *** Note that you will need to replace with dot "." if the information is missing or inconsistent. 
# *** We will assess later the frequency of missing values.
# 
# 
# ** DEPRIVED IN DRINKING WATER
# ** (the individual -HH- is considered deprived in drinking water is: unprotected well, spring, river/lake/pond or other)
# 
# /*lookfor water
# codebook b2q12, tab(20)
# gen d_dwater=(b2q12==5 | b2q12==6 | b2q12==7 | b2q12==8)
# replace d_dwater=. if b2q12==.
# tab b2q12 d_dwater [aw=weight], miss
# label variable d_dwater "Deprived in drinking water"ic*/
# 
# ** Please follow the same logic for all indicators. 
# ** Note that for advance indicator you may have to use more complex algorithms
# 
# *****************************************************************************
# ********  Create a local variable with all your indicators          *********
# *****************************************************************************
# 
# local varlist_pov electricity_depriv sanitation_depriv water_depriv floor_depriv cookingfuel_depriv asset_depriv educ_depriv child_noattendhh undernutritionhh mortality5hh
# 
# 
# 
# *****************************************************************************
# ****            Define the weights              *****************************
# *****************************************************************************
# ** Create a loop for the variables with the same weight *********************
# *****************************************************************************
# 
# 
# egen ls=rowmiss(electricity_depriv sanitation_depriv water_depriv floor_depriv cookingfuel_depriv asset_depriv)
# 
# 
# gen hh_missing=0
# replace hh_missing=1 if educ_depriv==. | nutrihh==2 | attehh==2 | morthh==2 | (ls>0 & ls<.)
# 
# foreach var in electricity_depriv sanitation_depriv water_depriv floor_depriv cookingfuel_depriv asset_depriv {
# gen w_`var'=1/18 if ls==0
# }
# 
# 
# 
# gen w_mortality5hh = 1/6 if morthh==1 & nutrihh==1
# replace w_mortality5hh = 1/3 if morthh==1 & nutrihh==3
# replace w_mortality5hh = 0 if morthh==3 & nutrihh==1
# 
# gen w_undernutritionhh = 1/6 if morthh==1 & nutrihh==1
# replace w_undernutritionhh = 1/3 if morthh==3 & nutrihh==1
# replace w_undernutritionhh = 0 if morthh==1 & nutrihh==3
# 
# 
# 
# gen w_child_noattendhh = 1/6 if attehh==1 | attehh==3
# 
# gen w_educ_depriv = 1/6 if educ_depriv<.
# 
# 
# 
# 
# *******************************************************************
# *********     Define the weigthed deprivation g0* matrix       ****
# *******************************************************************
# 
# foreach var in `varlist_pov' {
# gen wg0_`var' = `var'*w_`var'
# }
# 
# ******************************************************************************
# *********** Compute the frequency of missing values for indicator ************
# ******************************************************************************
# 
# foreach var in `varlist_pov' {
# gen `var'_miss=1 if `var'==.
# replace `var'_miss=0 if `var'!=.
#     }
# 
# sum *_miss
# 
# 
# ********************************************************************************
# *************   Define the (weighted) deprivation count vector "ci" ************
# ********************************************************************************
# 
# egen ci=rsum(wg0_*)
# label variable ci "Deprivation Count"
# 
# egen n_missing=rowmiss(wg0_*)
# label variable n_missing "Number of missing variables by individual"
# gen missing=(n_missing>0)
# label variable missing "Individual with missing variables"
# 
# *** Check sample drop due to missing values
# tab missing
# 
# *******************************************************************************
# ***** Create de identification vector (poor/non poor) *************************
# ***** and compute individual average of deprivation ***************************
# *******************************************************************************
# 
# forvalues x=1(1)10 {
# gen h_`x'0p=(ci>=`x'/10) 
# replace h_`x'0p=. if missing==1
# gen a_`x'0p=(ci) if h_`x'0p==1
# replace a_`x'0p=. if missing==1
# label var h_`x'0p "Condition of Multidimensional Poverty  k=`x'"
# label var a_`x'0p "Individual Average deprivation  k=`x'"
# }
# 
# sum h_10p-a_100p [aw=weight_w]
# 
# 
# 
# gen h_33p=(ci>=3.33/10) 
# replace h_33p=. if missing==1
# gen a_33p=(ci) if h_33p==1
# replace a_33p=. if missing==1
# label var h_33p "Condition of Multidimensional Poverty  k=33.3"
# label var a_33p "Individual Average deprivation  k=33.3"
# 
# sum h_33p a_33p [aw=weight_w]
# 
# 
# 
# 
# ********************************************************************************
#   ******* Compute raw headcounts        ******************************************
#   ********************************************************************************
#   
#   foreach var in `varlist_pov' {
# gen `var'_raw=(`var')
#                 replace `var'_raw=. if missing==1
# }
# 
# su *_raw  [iw=weight_w]
# 
# ******************************************************************************
#   *********** Compute Censored headcount and censored headocunt ****************
#   ******************************************************************************
#   ***** Please define in the first line your poverty cutoff, the example shows k=33.3 is 33.3%
# 
# local k=1/3
# foreach var in `varlist_pov' {
# *gen `var'_CH_`k'=(`var'==1 & h_`k'==1)
# *replace `var'_CH_`k'=. if missing==1
# 
# gen `var'_CH_33=(`var'==1 & h_33==1)
#                   replace `var'_CH_33=. if missing==1
#                   
# }
# 
# sum electricity_depriv_CH_33-educ_depriv_CH_33 [iw=weight_w]
# sum h_33 a_33 [iw=weight_w]
# 
# *h = HC
# *a = intensity
# 
# 
# 
# 
# capture drop cedu chealth cls
# 
# scalar drop _all
# 
# 
# 
# gen cedu = (child_noattendhh_CH_33 *  w_child_noattendhh) + (educ_depriv_CH_33 * w_educ_depriv)
# sum cedu [iw=weight_w]
# scalar mpi_edu=r(mean)
# 
# 
# gen chealth = (mortality5hh_CH_33 * w_mortality5hh) + (undernutritionhh_CH_33 * w_undernutritionhh)
# sum chealth [iw=weight_w]
# scalar mpi_health=r(mean)
# 
# 
# gen cls = (electricity_depriv_CH_33 * w_electricity_depriv) + (sanitation_depriv_CH_33 * w_sanitation_depriv) + (water_depriv_CH_33 * w_water_depriv) + (floor_depriv_CH_33 * w_floor_depriv) + (cookingfuel_depriv_CH_33 * w_cookingfuel_depriv) + (asset_depriv_CH_33 * w_asset_depriv)
# sum cls [iw=weight_w]
# scalar mpi_ls=r(mean)
# 
# 
# sum a_33p [iw=weight_w]
# scalar intensity = r(mean)
# sum h_33p [iw=weight_w]
# scalar headcount = r(mean)
# 
# 
# scalar mpi=headcount*intensity
# 
# scalar headcount100=headcount*100
# scalar intensity100=intensity*100
# 
# 
# scalar edu_contrib = (mpi_edu/mpi)*100
# scalar health_contrib = (mpi_health/mpi)*100
# scalar ls_contrib = (mpi_ls/mpi)*100
# 
# 
# capture drop uno
# gen uno=1 if h_33p<.
# 
# sum uno [aw=weight_w] if h_33p<.
# scalar pop=r(sum_w)
# 
# sum uno [aw=weight_w] if h_33p<. & ci>0 & ci<(1/3)
# scalar pop_vuln0=r(sum_w)
# 
# sum uno [aw=weight_w] if h_33p<. & ci>=0.2 & ci<(1/3)
# scalar pop_vuln20=r(sum_w)
# 
# sum uno [aw=weight_w] if h_33p<. & ci>=0.5
# scalar pop_severe=r(sum_w)
# 
# scalar vulnerable0=(pop_vuln0/pop)*100
# scalar vulnerable20=(pop_vuln20/pop)*100
# scalar severity=(pop_severe/pop)*100
# 
# 
# * Sample size
# 
# * HHs
# capture drop hh
# egen hh=group(hv001 hv002)
# sort age
# bys hh: gen n=_n
# gen nhh=1 if n==1
# 
# * Individuals
# 
# capture drop uno
# gen uno=1
# 
# sum uno 
# scalar ind_sample_tot=r(N)
# sum uno if h_33p<.
# scalar ind_sample_used=r(N)
# 
# sum uno [w=weight_w] if h_33p<.
# scalar ind_sample_used_w=r(sum)
# 
# 
# sum nhh
# scalar hh_sample_tot=r(N)
# 
# sum nhh if h_33p<.
# scalar hh_sample_used=r(N)
# 
# sort hv001 hv002 hvidx
# sum nhh [w=weight_w] if h_33p<.
# scalar hh_sample_used_w=r(sum)
# 
# * Inequality
# sum a_33p [aw=weight_w]
# scalar cv_ineq_poor = r(sd)/r(mean)
# 
# sum ci [aw=weight_w] if h_33p<.
# scalar cv_ineq_hc = r(sd)/r(mean)
# 
# sum ci [aw=weight_w] if h_33p<. & ci>0
# scalar cv_ineq_positive = r(sd)/r(mean)
# 
# * CONTRIBUTION OF EACH DIMENSION FOR THE VULNERABLE POEPLE
# capture drop *vu
# 
# foreach var in `varlist_pov' {
# gen `var'_CH_33vu=0 if h_33p<. & ci>=0.2 & ci<(1/3)
# replace `var'_CH_33vu=1 if `var'==1 & h_33p<. & ci>=0.2 & ci<(1/3)
# replace `var'_CH_33vu=. if missing==1
#     }
# 
# 
# 
# gen ceduvu = (child_noattendhh_CH_33vu * w_child_noattendhh) + (educ_depriv_CH_33vu * w_educ_depriv)
# sum ceduvu [iw=weight_w] if h_33p<. & ci>=0.2 & ci<(1/3)
# scalar mpi_eduvu=r(mean)
# 
# 
# gen chealthvu = (mortality5hh_CH_33vu * w_mortality5hh) + (undernutritionhh_CH_33vu * w_undernutritionhh)
# sum chealthvu [iw=weight_w] if h_33p<. & ci>=0.2 & ci<(1/3)
# scalar mpi_healthvu=r(mean)
# 
# 
# gen clsvu = (asset_depriv_CH_33vu * w_asset_depriv) + (electricity_depriv_CH_33vu * w_electricity_depriv) + (sanitation_depriv_CH_33vu * w_sanitation_depriv) + (water_depriv_CH_33vu * w_water_depriv) + (floor_depriv_CH_33vu * w_floor_depriv) + (cookingfuel_depriv_CH_33vu * w_cookingfuel_depriv)
# sum clsvu [iw=weight_w] if h_33p<. & ci>=0.2 & ci<(1/3)
# scalar mpi_lsvu=r(mean)
# 
# sum ci [iw=weight_w] if h_33p<. & ci>=0.2 & ci<(1/3)
# scalar headcountvu = r(mean)
# 
# 
# scalar headcountvu100=headcountvu*100
# 
# 
# scalar edu_contribvu = (mpi_eduvu/headcountvu100)*10000
# scalar health_contribvu = (mpi_healthvu/headcountvu100)*10000
# scalar ls_contribvu = (mpi_lsvu/headcountvu100)*10000
# 
# 
# * CONTRIBUTION OF EACH DIMENSION FOR THE VULNERABLE + POOR POEPLE (ci >= 0.2)
# capture drop *vupo
# 
# foreach var in `varlist_pov' {
# gen `var'_CH_33vupo=0 if h_33p<. & ci>=0.2 & ci<.
# replace `var'_CH_33vupo=1 if `var'==1 & h_33p<. & ci>=0.2 & ci<.
# replace `var'_CH_33vupo=. if missing==1
#     }
# 
# 
# 
# gen ceduvupo = (child_noattendhh_CH_33vupo * w_child_noattendhh) + (educ_depriv_CH_33vupo * w_educ_depriv)
# sum ceduvupo [iw=weight_w] if h_33p<. & ci>=0.2 & ci<.
# scalar mpi_eduvupo=r(mean)
# 
# 
# gen chealthvupo = (mortality5hh_CH_33vupo * w_mortality5hh) + (undernutritionhh_CH_33vupo * w_undernutritionhh)
# sum chealthvupo [iw=weight_w] if h_33p<. & ci>=0.2 & ci<.
# scalar mpi_healthvupo=r(mean)
# 
# 
# gen clsvupo = (asset_depriv_CH_33vupo * w_asset_depriv) + (electricity_depriv_CH_33vupo * w_electricity_depriv) + (sanitation_depriv_CH_33vupo * w_sanitation_depriv) + (water_depriv_CH_33vupo * w_water_depriv) + (floor_depriv_CH_33vupo * w_floor_depriv) + (cookingfuel_depriv_CH_33vupo * w_cookingfuel_depriv)
# sum clsvupo [iw=weight_w] if h_33p<. & ci>=0.2 & ci<.
# scalar mpi_lsvupo=r(mean)
# 
# sum ci [iw=weight_w] if h_33p<. & ci>=0.2 & ci<.
# scalar headcountvupo = r(mean)
# 
# 
# 
# scalar headcountvupo100=headcountvupo*100
# 
# 
# scalar edu_contribvupo = (mpi_eduvupo/headcountvupo100)*10000
# scalar health_contribvupo = (mpi_healthvupo/headcountvupo100)*10000
# scalar ls_contribvupo = (mpi_lsvupo/headcountvupo100)*10000
# 
# 
# scalar list mpi headcount100 intensity100 vulnerable0 vulnerable20 severity edu_contrib health_contrib ls_contrib edu_contribvu health_contribvu ls_contribvu edu_contribvupo health_contribvupo ls_contribvupo cv_ineq_poor cv_ineq_positive cv_ineq_hc hh_sample_tot hh_sample_used hh_sample_used_w ind_sample_tot ind_sample_used ind_sample_used_w
# 
# 
# 
# sum sample_all [w=weight]
# scalar sall = r(sum)
# sum sample6y [w=weight_w]
# scalar smpi =  r(sum)
# 
# scalar list sall smpi
# ************************************
#   **  ALWAYS CHECK THAT sall = smpi **
#   ************************************
#   