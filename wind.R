
# Wind modelling

# Read ASOS data from ftp

year<-as.character(seq(2000,2014,1))
month<-c("01","02","03","04","05","06","07","08","09","10","11","12")

for (i in 1:length(year)){
    for (j in 5:length(months)){
        download.file(paste0("ftp://ftp.ncdc.noaa.gov/pub/data/asos-onemin/6405-",
            year[i],"/64050KNYC",year[i],month[j],
            ".dat"),paste0("file",year[i],month[j],".dat"))
        
    }
}


# read files
date<-c()
dir<-c()
speed<-c()
input<-c()
# year<-as.character(seq(2000,2009,1))
year<-2009
month<-c("01","02","03","04","05","06","07","08","09","10","11","12")
library(assertthat)

for (i in 1:length(year)){
    for (j in 1:12){
        cat("\n year ",i," month ",j)
        data<- read.table(paste0("file",year[i],month[j],".dat"),
                stringsAsFactors=F,col.names=paste0("V",seq_len(8)),
                fill=T,na.strings="M")
        data<-subset(data,select=c(V2,V5,V6))
        assert_that(length(data$V2)==length(data$V5))
        ok<-complete.cases(data)
        input<-c(input,substr(data$V2[ok],start=4,stop=15))
        dir<-c(dir,data$V5[ok])
        speed<-c(speed,data$V6[ok]) 
        
    }
}

# january 2010
data<- read.table(paste0("file201001.dat"),
                  stringsAsFactors=F,col.names=paste0("V",seq_len(8)),
                  fill=T,na.strings="M")
data<-subset(data,select=c(V2,V5,V6))
assert_that(length(data$V2)==length(data$V5))
ok<-complete.cases(data)
input<-c(input,substr(data$V2[ok],start=4,stop=15))
dir<-c(dir,data$V5[ok])
speed<-c(speed,data$V6[ok])


# Filter uncorrect values of dir and 
okdir<-dir %in% as.character(seq(0,360,1))
dir<-dir[okdir]
speed<-speed[okdir]
input<-input[okdir]

# Filter uncorrect values of speed
okspeed<-!is.na(as.numeric(speed))
dir<-dir[okspeed]
speed<-speed[okspeed]
input<-input[okspeed]


speed<-0.5144444*as.numeric(speed) #converted from knots to m/s
dir<-as.numeric(dir)

#generate dates
date<-strptime(input, format="%Y%m%d%H%M",tz="America/New_York")
#transform dir and speed to speed in xy axis
wind_x<-speed*cos((90-dir)*pi/180)
wind_y<-speed*sin((90-dir)*pi/180)

ourdata<-data.frame(date=date,wx=wind_x,wy=wind_y)

write.table(ourdata,"CentralPark_NY_wind_2009_2010.dat")

# If starting here read table
# ourdata<-read.table("CentralPark_NY_wind_2000_2010.dat",
#        header=T,colClasses=c("POSIXlt", "POSIXt","numeric","numeric"))

# check number of gaps 2009+jan2010
ideal_num<-60*24*365+60*24*31
real_num<-dim(ourdata2)[1]
ideal_num-real_num

plot(ourdata2$date,ourdata2$wx,type="l")

shift_data<-c(ourdata$date[1],ourdata$date)
shift_data2<-c(ourdata$date,ourdata$date[1])
shift<-shift_data2-shift_data
shift<-shift[2:(length(shift)-1)]
max(shift)

# generate complete set of data to have gaps
time.min<-ourdata$date[1]
time.max<-ourdata$date[length(ourdata$date)]

all.dates <- seq(time.min, time.max, by="min")
#convert to data frame using same name as in ourdata
all.dates.frame <- data.frame(list(date=all.dates))

# Merge the two datasets: the full dates and original data
merged <- merge(all.dates.frame, ourdata, all=T) 
# now gaps are 'voids' in the graph
plot(merged$date,merged$wx,type="l")

 # use gapfillSSA to fill gaps
library(spectral.methods)
filled_wx<-gapfillSSA(series = merged$wx, plot.results = TRUE, open.plot = FALSE)
f_wx<-filled_wx$filled.series

filled_wy<-gapfillSSA(series = merged$wy, plot.results = TRUE, open.plot = FALSE)
f_wy<-filled_wy$filled.series

filled<-data.frame(date=merged$date,wx=f_wx,wy=f_wy)
write.table(filled,"CPNY_wind_2009_filled.dat",sep=",")
write.csv(filled,"CPNY_wind_2009_filled.csv")

# plot
plot(merged$date,f_wx,type="l")
lines(merged$date,merged$wx,type="l",col="blue")

plot(merged$date,f_wy,type="l")
lines(merged$date,merged$wy,type="l",col="blue")

# package mAr for multivariate AR model
# acf()
library(mAr)
prova<-filled[,2:3]
prova[,2]<-as.numeric(prova[,2])
prova[,1]<-as.numeric(prova[,1])
mod<-mAr.est(prova,5)
mod$wHat
mod$AHat
head(mod$resid)
plot(mod$resid[,1],type="l")
plot