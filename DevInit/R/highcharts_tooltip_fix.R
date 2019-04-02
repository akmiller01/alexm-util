library(rCharts)

clara <- data.frame(HumanHazard=c(1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10),
                NaturalHazard=c(0.7,0.8,0.7,0.3,0.5,0.2,0.4,0.4,0.5,0.5,
                    0.3,0.2,0.3,0.7,0.5,0.8,0.6,0.6,0.5,0.7),
                GrossOda=c(0.7,0.8,0.7,0.3,0.5,0.2,0.4,0.4,0.5,0.5,
                                0.3,0.2,0.3,0.7,0.5,0.8,0.6,0.6,0.5,0.7),
                region=factor(c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B","B","B","B")),
                CountryName=c("Group 1","Group 1","Group 1","Group 1","Group 1","Group 1","Group 2","Group 2","Group 2","Group 2",
                      "Group 1","Group 1","Group 1","Group 1","Group 1","Group 1","Group 2","Group 2","Group 2","Group 2"))

clara$x = clara$NaturalHazard
clara$y = clara$HumanHazard
clara$z = clara$GrossOda

p1 = rCharts:::Highcharts$new()
rlev = levels(clara$region)
for(i in 1:length(rlev))
{
  p1$series(data = toJSONArray2(clara[clara$region==rlev[i],,drop=F], json = F,names=T), name = rlev[i],type = c("bubble"), marker = list(radius = 3))
}
p1$plotOptions(bubble = list(pointPadding = 0, groupPadding = 0, borderWidth = 0))
p1$tooltip(useHTML = T, formatter="#!function () {
  return '<b>Country: </b>' +
this.point.CountryName +
'</br><b>Human Hazard: </b>' +
this.point.y +
'</br><b>Natural Hazard: </b>' +
this.point.x +
'</br><b>ODA: </b>$' +
this.point.z;
}!#")
p1$colors("#443e42", "#f8c1b2", "#f0826d", "#8f1b13", "#8f1b13")
p1$legend(enabled = T)
p1
