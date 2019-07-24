list.of.packages <- c("data.table","ggplot2","scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

download_url = "https://docs.google.com/spreadsheets/d/1hGUj_cw1L6Xv54QfoEMG1iHlI52lcbt5prnAhVI-7pI/export?format=csv&id=1hGUj_cw1L6Xv54QfoEMG1iHlI52lcbt5prnAhVI-7pI&gid=0"

svy = data.table(read.csv(download_url,as.is=T,skip=2,na.strings=""))
svy$config.date = as.Date(svy$Configuration.date.., format="%d %b %Y")
svy$config.month = svy$config.date - (as.numeric(format(svy$config.date,"%d"))-1)
svy$software = factor(svy$Autopilot, levels=c("None", "Enhanced Autopilot", "Autopilot", "Full Self-Driving"))
svy = svy[complete.cases(svy$software),]
svy[,month.count:=nrow(.SD),by=.(config.month)]
svy.tab = svy[,.(percent=nrow(.SD)/max(month.count)),by=.(config.month,software)]
svy.grid = expand.grid(config.month=unique(svy$config.month),software=unique(svy$software))
svy.tab = merge(svy.tab,svy.grid,by=c("config.month","software"),all=T)
svy.tab$percent[which(is.na(svy.tab$percent))] = 0
ggplot(svy.tab,aes(x=config.month,y=percent,fill=software)) +
  geom_area() +
  scale_y_continuous(labels=percent) +
  scale_x_date(breaks=pretty_breaks(n=17)) +
  labs(y="Percent of configurations", x="Configuration month",fill="Software") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1))
