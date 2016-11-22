library(data.table)

#solar.data<-fread("http://solarscience.msfc.nasa.gov/greenwch/g2004.txt",sep="\t",sep2=" ",colClasses = "character")

#solar.meta<-fread("http://services.swpc.noaa.gov/text/srs.txt",header=TRUE)

sunspot.data<-fread("http://solarscience.msfc.nasa.gov/greenwch/daily_area.txt",header=T)
