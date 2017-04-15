##work file
library(dplyr)
library(ggplot2)
c1<-read.csv("repdata_data_StormData.csv.bz2",stringsAsFactors = FALSE)
d1<-tbl_df(c1)


d2<-group_by(d1,EVTYPE)
d3<-select(d2,EVTYPE,FATALITIES,INJURIES)
d4<-filter(d3,FATALITIES,!is.na(FATALITIES))
d5<-summarise(d4,FATALITIES=sum(FATALITIES))
d6<-arrange(d5,desc(FATALITIES))
d7<-head(d6,10)
d7
g<-ggplot(d7,aes(EVTYPE,FATALITIES))
plotfatal<-g+geom_col(fill="red")
plotfatal