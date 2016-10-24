library(data.table)
library(lubridate)
library(ggplot2)

data.obs<-fread("Rdata/sat_over_obs.csv")
names(data.obs)<-c("IT_DATETIME","DS_CODE","Q","DS_LON","DS_LAT","SAT","DIFF")
data.obs$IT_DATETIME<-as.Date(data.obs$IT_DATETIME)
data.obs$day<-yday(data.obs$IT_DATETIME)
data.obs$month<-month(data.obs$IT_DATETIME)

cols<-c("Q","SAT","DIFF")
#look at .SDcols<-cols
#http://stackoverflow.com/questions/16513827/r-summarizing-multiple-columns-with-data-table

#examples: https://www.r-bloggers.com/two-of-my-favorite-data-table-features/
summary_by_station<-data.obs[,list(observed=mean(Q,na.rm=TRUE),satellite=mean(SAT,na.rm=TRUE),difference=mean(DIFF,na.rm=TRUE)),by=DS_CODE]
summary_by_yday<-data.obs[,list(observed=mean(Q,na.rm=TRUE),satellite=mean(SAT,na.rm=TRUE),difference=mean(DIFF,na.rm=TRUE)),by=day]
summary_by_month<-data.obs[,list(observed=mean(Q,na.rm=TRUE),satellite=mean(SAT,na.rm=TRUE),difference=mean(DIFF,na.rm=TRUE)),by=month]
summary_by_stationmonth<-data.obs[,list(observed=mean(Q,na.rm=TRUE),
                                        satellite=mean(SAT,na.rm=TRUE),
                                        difference=mean(DIFF,na.rm=TRUE)),
                                  by=list(month,DS_CODE)]


hist(summary_by_station$difference)
plot(summary_by_yday$day,summary_by_yday$difference)
plot(summary_by_month$month,summary_by_month$difference)

#http://www.statmethods.net/advgraphs/ggplot2.html
qplot(day,difference,data=summary_by_yday,geom=c("point","smooth")) #beginnen bij -2 einde bij 0
qplot(month,difference,data=summary_by_stationmonth,geom=c("boxplot","jitter"),group=month,color=DS_CODE)
