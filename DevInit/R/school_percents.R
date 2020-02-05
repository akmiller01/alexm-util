list.of.packages <- c("ggplot2","data.table","scales","XML","httr","jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

key = c(
  "1"="White",
  "2"="Black",
  "3"="Hispanic",
  "4"="Asian",
  "5"="American Indian",
  "6"="Pacific Islander",
  "7"="Two or more races",
  "99"="Total"
)

FIPS = "51" #VA
FIPS = "24" #MD
FIPS = "11" #DC
# FIPS = "49" #UT

school_list = list()
school_page = 2
school_total = 0

schools_url1 = paste0(
  "https://educationdata.urban.org/api/v1/schools/ccd/directory/2016/?fips=",
  FIPS,
  "&fields=school_name,ncessch,city_location,state_location,enrollment"
)
schools_source1 = fromJSON(schools_url1)
school_count1 = schools_source1$count

schools1 = schools_source1$results
school_list[[1]] = schools1
school_total = school_total + nrow(schools1)

while(school_total < school_count1){
  schools_url2 = paste0(schools_url1,"&page=", school_page)
  schools_source2 = fromJSON(schools_url2)
  schools2 = schools_source2$results
  school_total = school_total + nrow(schools2)
  school_list[[school_page]] = schools2
  school_page = school_page + 1
}

schools = rbindlist(school_list)

school_index = 71

SCHOOL_ID = schools$ncessch[[school_index]]
SCHOOL_NAME = schools$school_name[[school_index]]

years = as.character(c(1989:2016))

data_list = list()

for(year in years){
  message(year)
  dat_url = paste0(
    "https://educationdata.urban.org/api/v1/schools/ccd/enrollment/",
    year,
    "/grade-99/race/?ncessch=",
    SCHOOL_ID,
    "&fields=year,ncessch,race,enrollment"
  )
  result = fromJSON(dat_url)$result
  data_list[[year]] = result
}

dat = rbindlist(data_list)
dat$race_str = key[as.character(dat$race)]

grid = expand.grid(race_str=key, year=unique(dat$year))
dat.grid = merge(dat,grid,by=c("race_str","year"),all=T)
dat.grid$enrollment[which(is.na(dat.grid$enrollment))] = 0
dat.grid$enrollment[which(dat.grid$enrollment<0)] = 0

dat.grid = subset(dat.grid,race_str!="Total")

dat.grid[,percent:=.SD$enrollment/sum(.SD$enrollment),by=.(year)]
ggplot(dat.grid,aes(x=year,y=percent,group=race_str,fill=race_str)) +
  geom_area() +
  theme_classic() +
  theme(legend.title=element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(y="Percent enrollment", x="Year", title=paste("Enrollment at",SCHOOL_NAME))
ggplot(dat.grid,aes(x=year,y=enrollment,group=race_str,color=race_str)) +
  geom_line() +
  theme_classic() +
  theme(legend.title=element_blank()) +
  labs(y="Enrollment", x="Year", title=paste("Enrollment at",SCHOOL_NAME))
