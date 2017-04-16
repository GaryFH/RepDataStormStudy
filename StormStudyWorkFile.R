##work file
library(dplyr)
library(ggplot2)
c1<-read.csv("repdata_data_StormData.csv.bz2",stringsAsFactors = FALSE)
d1<-tbl_df(c1)


d2<-group_by(d1,EVTYPE)
d3<-select(d2,EVTYPE,FATALITIES,INJURIES)
d4<-d3##d4<-filter(d3,FATALITIES,!is.na(FATALITIES))
d5<-summarise(d4,FATALITIES=sum(FATALITIES),INJURIES=sum(INJURIES))
d6<-arrange(d5,desc(FATALITIES))
d7<-head(d6,8)
d7
g<-ggplot(d7,aes(x=reorder(EVTYPE,-FATALITIES),y=FATALITIES))
plotfatal<-g+geom_col(fill="yellow")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_text(aes(label=FATALITIES), vjust=1.5, color="black")+labs(x="Weather Event",title="Eight most Deadly Weather Events US 1950-2011")
plotfatal

g2<-ggplot(d7,aes(x=reorder(EVTYPE,-INJURIES),y=INJURIES))
plotinjury<-g2+geom_col(fill="magenta")+labs(x="Weather Event",title="Eight most Injuries due to Weather Events US 1950-2011")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ geom_text(aes(label=INJURIES), vjust=1.5, color="black")
plotinjury





##TotalDamage=PROPDMG+CROPDMG
dd2<-group_by(d1,EVTYPE)
dd25<-mutate(dd2, pdmultplyr = 
        ifelse(PROPDMGEXP == "h" | PROPDMGEXP == "H", 100,
        ifelse(PROPDMGEXP == "k" | PROPDMGEXP == "K", 1000,
        ifelse(PROPDMGEXP == "m" | PROPDMGEXP == "M", 1000000,
        ifelse(PROPDMGEXP == "b" | PROPDMGEXP == "B", 1000000000, 0 )))))
dd26<-mutate(dd25, cdmultplyr=
        ifelse(CROPDMGEXP == "h" | CROPDMGEXP == "H", 100,
        ifelse(CROPDMGEXP == "k" | CROPDMGEXP == "K", 1000,
        ifelse(CROPDMGEXP == "m" | CROPDMGEXP == "M", 1000000,
        ifelse(CROPDMGEXP == "b" | CROPDMGEXP == "B", 1000000000, 0 )))))
dd3<-select(dd26,EVTYPE,PROPDMG:CROPDMGEXP,pdmultplyr,cdmultplyr)

dd35<-mutate(dd3,TotalDamage=(PROPDMG*pdmultplyr)+(CROPDMG*cdmultplyr))

dd4<-filter(dd35,TotalDamage,!is.na(TotalDamage))

dd5<-summarise(dd4,TotalDamage=sum(TotalDamage)/1000000000)
dd6<-arrange(dd5,desc(TotalDamage))
dd7<-head(dd6,8)
dd7
g3<-ggplot(dd7,aes(x=reorder(EVTYPE,-TotalDamage),y=TotalDamage))
plotdamage<-g3+geom_col(fill="orange")+labs(x="Weather Event",title="Eight highest damages due to Weather Events US 1950-2011")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_text(aes(label=TotalDamage), vjust=1.5)+labs(x="Weather event type",y="Total Property damage in billions",title="Top eight most expensive weather events")
plotdamage

####print(plotfatal,fig.height=4)












##The stuff below did not work

d433<-for (i in length(dd4$PROPDMGEXP)) {            
        if (dd4$PROPDMGEXP[i]=="K"){
                dd4$PROPDMG[i]=dd4$PROPDMG[i]*1000
        } else 
                if(dd4$PROPDMGEXP[i]=="M"){dd4$pdam[i]=dd4$PROPDMG[i]*1000000
                } else 
                        if(dd4$PROPDMGEXP[i]=="B"){dd4$pdam[i]=dd4$PROPDMG[i]*1000000000      
                        } else 
                                if(dd4$PROPDMGEXP[i]=="H"){dd4$pdam[i]=dd4$PROPDMG[i]*100      
                                } else 
                                        if(dd4$PROPDMGEXP[i]=="m"){dd4$pdam[i]=dd4$PROPDMG[i]*1000000
                                        } else 
                                                if(dd4$PROPDMGEXP[i]=="b"){dd4$pdam[i]=dd4$PROPDMG[i]*1000000000     
                                                } else 
                                                        if(dd4$PROPDMGEXP[i]=="h"){dd4$pdam[i]=dd4$PROPDMG[i]*100
                                                        } else 
                                                                if(dd4$PROPDMGEXP[i]=="k"){dd4$pdam[i]=dd4$PROPDMG[i]*1000
                                                                }}