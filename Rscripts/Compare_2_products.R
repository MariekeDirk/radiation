library(data.table)
library(lubridate)
library(ggplot2)
library(mgcv)

data.obs<-fread("Rdata/sat_over_obs.csv")
names(data.obs)<-c("IT_DATETIME","DS_CODE","Q","DS_LON","DS_LAT","SAT","DIFF")
data.obs$IT_DATETIME<-as.Date(data.obs$IT_DATETIME)
data.obs$day<-yday(data.obs$IT_DATETIME)
data.obs$month<-month(data.obs$IT_DATETIME)

setkey(data.obs, day)
setkey(summary_by_yday, day)

# summary_by_stationday <-data.obs[, mean(DIFF), by = .(year = year(IT_DATETIME),day=day,DS_CODE)]
summary_by_year_day <- data.obs[, mean(DIFF), by = .(year = year(IT_DATETIME), day)]
summary_by_year_month <- data.obs[, mean(DIFF), by = .(year = year(IT_DATETIME), month)]


#fitting routines
# fit1<-gam(V1 ~ s(year) + s(day, bs = "cc"), data=summary_by_stationday)
# plot(fit1)
fit1 <- gam(V1 ~ s(year) + s(day, bs = "cc"), data = summary_by_year_day)
plot(fit1)

fit2 <- gam(V1 ~ s(year) + s(month, bs = "cc"), data = summary_by_year_month)
plot(fit2)

fit <- gam(DIFF ~ s(day, bs="cc"), data = data.obs)
#95% interval
p<-predict(fit,data.obs,se.fit=TRUE)
upr<-p$fit+(2*p$se.fit)
lwr<-p$fit-(2*p$se.fit)

plot(data.obs$IT_DATETIME,data.obs$DIFF,pch=".",xlab="time [years]",ylab="difference solar irradiance [W/m2]")
points(data.obs$IT_DATETIME,fit$fitted.values,col="yellow",lwd=0.5,pch='.')

#points(data.obs$IT_DATETIME,upr,col="yellow",lwd=0.3,pch='-')
#points(data.obs$IT_DATETIME,lwr,col="yellow",lwd=0.3,pch='-')

# cols<-c("Q","SAT","DIFF")
# #look at .SDcols<-cols
# #http://stackoverflow.com/questions/16513827/r-summarizing-multiple-columns-with-data-table
# 
# #examples: https://www.r-bloggers.com/two-of-my-favorite-data-table-features/
# summary_by_station<-data.obs[,list(observed=mean(Q,na.rm=TRUE),satellite=mean(SAT,na.rm=TRUE),difference=mean(DIFF,na.rm=TRUE)),by=DS_CODE]
# summary_by_yday<-data.obs[,list(observed=mean(Q,na.rm=TRUE),satellite=mean(SAT,na.rm=TRUE),difference=mean(DIFF,na.rm=TRUE)),by=day]
# summary_by_month<-data.obs[,list(observed=mean(Q,na.rm=TRUE),satellite=mean(SAT,na.rm=TRUE),difference=mean(DIFF,na.rm=TRUE)),by=month]
# summary_by_stationmonth<-data.obs[,list(observed=mean(Q,na.rm=TRUE),
#                                         satellite=mean(SAT,na.rm=TRUE),
#                                         difference=mean(DIFF,na.rm=TRUE)),
#                                   by=list(month,DS_CODE)]
# 
# 
# hist(summary_by_station$difference)
# plot(summary_by_yday$day,summary_by_yday$difference)
# plot(summary_by_month$month,summary_by_month$difference)
# 
# #http://www.statmethods.net/advgraphs/ggplot2.html
# qplot(day,difference,data=summary_by_yday,geom=c("point","smooth")) #beginnen bij -2 einde bij 0
# qplot(month,difference,data=summary_by_stationmonth,geom=c("boxplot","jitter"),group=month,color=DS_CODE)


