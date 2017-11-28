library(readr)
library(treemap)
library(data.table)
library(WDI)

setwd("C:/Users/Alex/Documents/Data/IDC")

pop <- WDI(country="all","SP.POP.TOTL",extra=TRUE,start=2000,end=2015)
keep <- c("iso3c","region","income")
meta <- unique(pop[keep])
setnames(meta,"iso3c","wb.iso3c")

oda <- read_csv("TABLE2A_27112017150311239.csv")
all_recip <- unique(oda$Recipient)

totals <- subset(oda,Recipient=="All Recipients, Total")

multilats <- c(
  "Af. D B"
  ,"African Dev. Fund"
  ,"AsDB Special Funds"
  ,"Asian Dev. Bank"
  ,"CABEI"
  ,"Caribbean Dev. Bank"
  ,"East African Community"
  ,"EC"
  ,"EDF"
  ,"EIB"
  ,"IBRD"
  ,"IDA"
  ,"IDA-MDRI"
  ,"IDB"
  ,"IDB Special Oper. Fund"
  ,"IFAD"
  ,"IFC"
  ,"IMF"
  ,"Indus Basin"
  ,"MIGA"
  ,"States Ex-Yugoslavia"
  ,"UNDP"
  ,"UNFPA"
  ,"UNHCR"
  ,"UNICEF"
  ,"UNRWA"
  ,"WFP"
)

oda <- oda[which(!grepl("Total",oda$Recipient)),]
oda <- oda[which(!grepl("regional",oda$Recipient)),]
oda <- oda[which(!grepl("unspecified",oda$Recipient)),]
# oda <- oda[which(!grepl("income",oda$Recipient)),]
oda <- oda[which(!grepl("Other",oda$Recipient)),]
oda <- subset(oda,!(Recipient %in% multilats))
subset_recip <- unique(oda$Recipient)
setdiff(all_recip,subset_recip)[order(setdiff(all_recip,subset_recip))]

oda$Recipient[which(oda$RECIPIENT==247)] <- "Cote d'Ivoire"
# recips <- unique(oda$Recipient)
# recips <- data.frame(Recipient=recips)
# recips <- merge(recips,isokey,all.x=TRUE,by="Recipient")
# write.csv(recips,"isokey.csv",row.names=FALSE,na="")
isokey <- read_csv("isokey.csv")
oda <- merge(oda,isokey,by="Recipient",all.x=TRUE)

uk.oda <- subset(oda,Donor=="United Kingdom" & Year==2015)

keep <- c("Recipient","iso3c","Year","Value")
uk.oda <- uk.oda[keep]
uk.oda <- subset(uk.oda, !is.na(iso3c) & iso3c!=0)

pov <- read_csv("PovertySummary2013.csv")

# setdiff(uk.oda$iso3c,pov$iso3c)
# setdiff(pov$iso3c,uk.oda$iso3c)

dat <- merge(uk.oda,pov,by="iso3c",all=TRUE)
dat$Recipient[which(is.na(dat$Recipient))] <- dat$CountryName[which(is.na(dat$Recipient))]

dat$gt10 <- dat$poorpop>10
gt10.tab <- data.table(dat)[,.(total=sum(Value,na.rm=TRUE)),by=.(gt10)]

write.csv(dat,"merged_oda.csv",row.names=FALSE,na="")

dat <- merge(dat,meta,by="wb.iso3c",all.x=TRUE)

regionDict = list(
  "Sub-Saharan Africa (all income levels)" =  "Sub-Saharan Africa"       
  ,"Europe & Central Asia (all income levels)" = "Europe & Central Asia"     
  ,"East Asia & Pacific (all income levels)" = "East Asia & Pacific"       
  ,"South Asia" = "South Asia"                                    
  ,"Latin America & Caribbean (all income levels)" = "Latin America & Caribbean" 
  ,"North America" = "North America"                                 
  ,"Middle East & North Africa (all income levels)" = "Middle East & North Africa"
)
dictTranslate <- function(xVec,dict){
  results <- c()
  for(x in xVec){
    if(is.na(x)){
      results <- c(results,NA)
    }else{
      results <- c(results,dict[x][[1]])
    }
  }
  return(results)
}
dat$region <- dictTranslate(dat$region,regionDict)
dat$region[which(dat$Recipient=="Kosovo")] <- "Europe & Central Asia"
dat$region[which(dat$Recipient=="Cabo Verde")] <- "Sub-Saharan Africa"
dat <- subset(dat,!is.na(region))

dat$PovGap <- dat$PovGap*100
dat$poorpop <- dat$poorpop*1000000

library(ggplot2)
percent <- ggplot(dat,aes(x=HeadCount*100,y=PovGap,size=Value,colour=region,label=Recipient)) + geom_point(alpha=0.5) +
  labs(x="Percent of population in extreme poverty"
       ,y="Depth of extreme poverty"
       ,size="UK bilateral ODA (millions)"
       ,colour="Region") +
  theme_bw() + scale_size(range=c(1,10))
# +
# geom_text(aes(label=ifelse(UK.Bilateral.ODA>334745686,as.character(name),'')),hjust=0,vjust=1)
require(scales)
labeled <- ggplot(dat,aes(y=poorpop,x=PovGap,size=Value,colour=region)) + geom_point(alpha=0.5) +
  labs(y="Population in extreme poverty"
       ,x="Depth of extreme poverty"
       ,size="UK bilateral ODA (millions)"
       ,colour="Region"
  ) +
  theme_bw() + scale_size(range=c(1,15)) + scale_y_log10(labels = comma)  +
  geom_text(aes(label=ifelse(Value>300,as.character(Recipient),'')),hjust=0,vjust=1)

unlabeled <- ggplot(dat,aes(y=poorpop,x=PovGap,size=Value,colour=region)) + geom_point(alpha=0.5) +
  labs(y="Population in extreme poverty"
       ,x="Depth of extreme poverty"
       ,size="UK bilateral ODA (millions)"
       ,colour="Region"
  ) +
  theme_bw() + scale_size(range=c(1,15)) + scale_y_log10(labels = comma)

ggsave("Percent_Bubble.png",percent,width=10,height=6)
ggsave("Unlabeled_Bubble.png",unlabeled,width=10,height=6)
ggsave("Labeled_Bubble.png",labeled,width=10,height=6)

#Figure 7
pov <- pov[order(-pov$poorpop),]
poorest <- pov$iso3c[c(1:10)]
poorest.oda <- subset(oda,(iso3c %in% poorest) & Year==2015)
poorest.oda.tab <- data.table(poorest.oda)[,.(poorest.oda=sum(Value,na.rm=TRUE)),by=.(Donor)]

total.oda.tab <- data.table(subset(oda,Year==2015))[,.(total.oda=sum(Value,na.rm=TRUE)),by=.(Donor)]

oda.tab <- merge(total.oda.tab,poorest.oda.tab,by="Donor")
oda.tab$percent.poorest <- oda.tab$poorest.oda/oda.tab$total.oda
write.csv(oda.tab,"Percent_poorest.csv",row.names=FALSE,na="")

#Figure 9
dac <- c(
  "AUS"
  ,"AUT"
  ,"BEL"
  ,"CAN"
  ,"CZE"
  ,"DNK"
  ,"FIN"
  ,"FRA"
  ,"DEU"
  ,"GRC"
  ,"ISL"
  ,"IRL"
  ,"ITA"
  ,"JPN"
  ,"KOR"
  ,"LUX"
  ,"NLD"
  ,"NZL"
  ,"NOR"
  ,"POL"
  ,"PRT"
  ,"SVK"
  ,"SVN"
  ,"ESP"
  ,"SWE"
  ,"CHE"
  ,"GBR"
  ,"USA"
)
rev <- read_csv("Total_Revenue_PPP_per_capita.csv")
rev$iso3c <- rev$wb.iso3c
rev$iso3c[which(rev$wb.iso3c=="COD")]="ZAR"
rev$iso3c[which(rev$wb.iso3c=="TLS")]="TMP"
rev$iso3c[which(rev$wb.iso3c=="XKX")]="KSV"
rev$iso3c[which(rev$wb.iso3c=="PSE")]="WBG"
rev$dac <- rev$iso3c %in% dac
dac.rev <- subset(rev,Year==2015 & dac)
mean(dac.rev$rev)
rev <- subset(rev,Year==2015 & rev<1500)
less1500 <- unique(rev$iso3c)
over1mil <- subset(pov,poorpop>1)$iso3c

less_1500_rev_and_over_1_mil_pov = intersect(less1500,over1mil)
# pov$lrop <- pov$iso3c %in% less_1500_rev_and_over_1_mil_pov
# lrop.pov <- subset(pov,lrop)
# sum(lrop.pov$poorpop,na.rm=TRUE)/sum(pov$poorpop,na.rm=TRUE)
lrop.oda <- subset(oda,(iso3c %in% less_1500_rev_and_over_1_mil_pov) & Year==2015) 

lrop.oda.tab <- data.table(lrop.oda)[,.(lrop.oda=sum(Value,na.rm=TRUE)),by=.(Donor)]

oda.tab2 <- merge(total.oda.tab,lrop.oda.tab,by="Donor")
oda.tab2$percent.lrop <- oda.tab2$lrop.oda/oda.tab2$total.oda
write.csv(oda.tab2,"Percent_lrop.csv",row.names=FALSE,na="")

frag <- read_csv("fragile_states.csv")
frag$iso3c <- frag$wb.iso3c
frag$iso3c[which(frag$wb.iso3c=="COD")]="ZAR"
frag$iso3c[which(frag$wb.iso3c=="TLS")]="TMP"
frag$iso3c[which(frag$wb.iso3c=="XKX")]="KSV"
frag$iso3c[which(frag$wb.iso3c=="PSE")]="WBG"
frag <- subset(frag,value>0)
fragile_states <- frag$iso3c

frag.oda <- subset(oda,(iso3c %in% fragile_states) & Year==2015)
frag.oda.tab <- data.table(frag.oda)[,.(frag.oda=sum(Value,na.rm=TRUE)),by=.(Donor)]

oda.tab3 <- merge(total.oda.tab,frag.oda.tab,by="Donor")
oda.tab3$percent.frag <- oda.tab3$frag.oda/oda.tab3$total.oda
write.csv(oda.tab3,"Percent_frag.csv",row.names=FALSE,na="")

library(varhandle)
meta$iso3c <- unfactor(meta$wb.iso3c)
meta$iso3c[which(meta$wb.iso3c=="COD")]="ZAR"
meta$iso3c[which(meta$wb.iso3c=="TLS")]="TMP"
meta$iso3c[which(meta$wb.iso3c=="XKX")]="KSV"
meta$iso3c[which(meta$wb.iso3c=="PSE")]="WBG"
oda.income <- merge(oda,meta,by="iso3c")

income.tab <- data.table(oda.income)[,.(total=sum(Value,na.rm=TRUE)),by=.(Donor,income,Year)]
income.tab <- subset(income.tab,Year==2015)
write.csv(income.tab,"oda_by_income.csv",row.names=FALSE,na="")

entity <- read_csv("C:/git/digital-platform/reference/entity.csv")
keep <- c("iso-alpha-3","income-group")
entity <- entity[keep]
names(entity) <- c("wb.iso3c","income")
entity$iso3c <- entity$wb.iso3c
entity$iso3c[which(entity$wb.iso3c=="COD")]="ZAR"
entity$iso3c[which(entity$wb.iso3c=="TLS")]="TMP"
entity$iso3c[which(entity$wb.iso3c=="XKX")]="KSV"
entity$iso3c[which(entity$wb.iso3c=="PSE")]="WBG"
entity <- subset(entity,!is.na(income))

ldc.oda <- merge(oda,entity,by="iso3c",all.x=TRUE)
ldc.oda$income[which(is.na(ldc.oda$income))] <- "Unclassified"
ldc.oda.tab <- data.table(ldc.oda)[,.(total=sum(Value,na.rm=TRUE)),by=.(Donor,income,Year)]
ldc.oda.tab <- subset(ldc.oda.tab,Year==2015)
# ldc.oda.tab$income <- toupper(ldc.oda.tab$income)

write.csv(ldc.oda.tab,"Percent_ldc.csv",row.names=FALSE,na="")

dat <- read_csv("dfid.pov.csv")
dat <- merge(dat,meta,by="iso3c",all.x=TRUE)
dat$region <- dictTranslate(dat$region,regionDict)
dat$region[which(dat$Recipient=="Kosovo")] <- "Europe & Central Asia"
dat$region[which(dat$Recipient=="Cabo Verde")] <- "Sub-Saharan Africa"
dat <- subset(dat,!is.na(region))

dat$PovGap <- dat$PovGap*100
dat$poorpop <- dat$poorpop*1000000

dfid_bubble <- ggplot(dat,aes(y=poorpop,x=PovGap,size=Value,colour=region)) + geom_point(alpha=0.5) +
  labs(y="Population in extreme poverty"
       ,x="Depth of extreme poverty"
       ,size="DFID bilateral ODA (millions)"
       ,colour="Region"
  ) +
  theme_bw() + scale_size(range=c(1,15)) + scale_y_log10(labels = comma)

ggsave("DFID_Bubble.png",dfid_bubble,width=10,height=6)

# dat <- subset(dat,Value>0)
# 
# diRamp <- function(colorText1,colorText2=NA,colorText3=NA){
#   colorRef <- list("red"="#e84439")
#   colorRef <- c(colorRef,"orange"="#eb642b")
#   colorRef <- c(colorRef,"yellow"="#f49b21")
#   colorRef <- c(colorRef,"pink"="#c2135b")
#   colorRef <- c(colorRef,"purple"="#893f90")
#   colorRef <- c(colorRef,"blue"="#0089cc")
#   colorRef <- c(colorRef,"green"="#109e68")
#   colorRef <- c(colorRef,"grey"="#6a6569")
#   colorRef <- c(colorRef,"white"="#FFFFFF")
#   colorRef <- c(colorRef,"black"="#000000")
#   
#   if(!is.na(colorText2)){
#     if(!is.na(colorText3)){
#       color1 <- colorRef[[colorText1]]
#       if(is.null(color1)){color1 <- colorText1}
#       color2 <- colorRef[[colorText2]]
#       if(is.null(color2)){color2 <- colorText2}
#       color3 <- colorRef[[colorText3]]
#       if(is.null(color3)){color3 <- colorText3}
#       colorRamp(c(color1,color2,color3), interpolate="linear")
#     }else{
#       color1 <- colorRef[[colorText1]]
#       if(is.null(color1)){color1 <- colorText1}
#       color2 <- colorRef[[colorText2]]
#       if(is.null(color2)){color2 <- colorText2}
#       colorRamp(c(color1,color2), interpolate="linear")
#     }
#   }else{
#     color1 <- colorRef[["white"]]
#     color2 <- colorRef[[colorText1]]
#     if(is.null(color2)){color2 <- colorText2}
#     colorRamp(c(color1,color2), interpolate="linear")
#   }
# }
# 
# treeMapRamp <- function(vector){
#   vectorMax = max(vector,na.rm=TRUE)
#   message(vectorMax)
#   colors <- c()
#   for(i in 1:length(vector)){
#     newVal <- vector[i]/vectorMax
#     if(!is.na(vector[i])){
#       rgb <- diRamp("red")(newVal)
#       color <- substr(rgb(rgb[1,1],rgb[1,2],rgb[1,3],1,maxColorValue=255),1,7)
#       colors <- c(colors,color)
#     }else{
#       color <- "#cccccc"
#       colors <- c(colors,color)
#     }
#   }
#   return(colors)
# }
# 
# dat <- transform(dat,hc.color=treeMapRamp(HeadCount))
# dat <- transform(dat,pp.color=treeMapRamp(poorpop))
# dat <- transform(dat,oda.pc=Value/poorpop)
# 
# dat$Recipient[which(dat$Recipient=="West Bank and Gaza Strip")] <- "Palestine"
# 
# treemap(dat
#         ,index="Recipient"
#         ,vSize="Value"
#         ,vColor="hc.color"
#         ,type="color"
#         ,title="2015 UK ODA (size) vs. 2013 extreme poverty % (colour)"
#         # ,lowerbound.cex.labels=0
#         ,fontsize.labels=12
#         ,inflate.labels=TRUE
# )
# treemap(dat
#         ,index="Recipient"
#         ,vSize="Value"
#         ,vColor="pp.color"
#         ,type="color"
#         ,title="2015 UK ODA (size) vs. 2013 extreme poverty population (colour)"
#         # ,lowerbound.cex.labels=1
#         ,fontsize.labels=12
#         ,inflate.labels=TRUE
# )
# pc.dat <- subset(dat,!is.na(oda.pc) & is.finite(oda.pc))
# treemap(pc.dat
#         ,index="Recipient"
#         ,vSize="oda.pc"
#         ,title="2015 UK ODA per poor person"
#         # ,lowerbound.cex.labels=1
#         ,fontsize.labels=12
#         ,inflate.labels=TRUE
# )
