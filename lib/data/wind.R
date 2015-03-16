# Wind modelling

# ----------------------------------------------------------------------
# Download wind data Central Park NY
# ----------------------------------------------------------------------
#'
#' ftp.wind function
#' 
#' Download Central Park New York wind ASOS data from ftp
#' 
#' @param year (integer) Year of interest.
#' @return Downloads locally a set of monthly files, including 1-min lag wind direction and speed at Central Park New York, for a particular year
#' @export
#' @examples
#' # start download
#' ftp.wind(2009)
#'
ftp.wind<-function(year){
    month<-c("01","02","03","04","05","06","07","08","09","10","11","12")
    
    for (i in 1:length(year)){
        for (j in 5:length(months)){
            download.file(paste0("ftp://ftp.ncdc.noaa.gov/pub/data/asos-onemin/6405-",
                                 year[i],"/64050KNYC",year[i],month[j],
                                 ".dat"),paste0("file",year[i],month[j],".dat"))
            
        }
    }
}

# ----------------------------------------------------------------------
# Read wind data Central Park NY
# ----------------------------------------------------------------------
#'
#' read.wind function
#' 
#' Read Central Park New York wind ASOS data from local files. Filename has to be
#' stored in the working directory, in the format name 'file'+year+month+'.dat'.
#' Example: january 2009 would be file200901.dat.
#' Function download.wind can be use to download this data from Asos NOAA ftp
#' Input wind direction and speed data is converted to speed in m/s in x & y axis.
#' 
#' @param year (integer) Year of interest.
#' @param months (character vector) Months of interest (january would be "01", etc) 
#' @return Data frame with three components: date (Date format), wx (numeric, wind in x axis, m/s) and wy (numeric, wind in y axis, m/s)
#' @export
#' @examples
#' # Read 2009 data
#' year<-2009
#' months<-c("01","02","03","04","05","06","07","08","09","10","11","12")
#' data<-read.wind(year,months)
#'

read.wind<-function(year,months){
    # read files
    date<-c()
    dir<-c()
    speed<-c()
    input<-c()
    
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
    
    return(ourdata)
}



# ----------------------------------------------------------------------
# Fill gaps at wind data Central Park NY
# ----------------------------------------------------------------------
#'
#' fill.wind function
#' 
#' Fill gaps in wind data, using gapfillSSA from package spectral.methods.
#' Input wind direction and speed data is converted to speed in m/s in x & y axis.
#' 
#' @param data Data frame outcome of read.wind funtion date (POSIXct format), wx (numeric, wind in x axis, m/s) and wy (numeric, wind in y axis, m/s)
#' @export
#' @import spectral.methods
#' @examples
#' # Read 2009 data using read.wind function
#' year<-2009
#' months<-c("01","02","03","04","05","06","07","08","09","10","11","12")
#' data<-read.wind(year,months)
#' # Fill gaps
#' filled<-fill.wind(data)
#'

    # generate complete set of data to have gaps
fill.wind<-function(data){
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
    
    # plot
    plot(merged$date,f_wx,type="l")
    lines(merged$date,merged$wx,type="l",col="blue")
    
    plot(merged$date,f_wy,type="l")
    lines(merged$date,merged$wy,type="l",col="blue")
    
    filled$date<-as.POSIXct(strptime(filled$date, "%Y-%m-%d %H:%M:%S",tz="America/New_York"))
    
    filled<-filled[complete.cases(filled),]
    
    return(filled)
}



#--------------------------------------------------------------------
### Wind algorithm #######
# -------------------------------------------------------------------

# Goal: build an autorregressive model for 2009 data, to have separately 
# a deterministic and a stochastic part

# Here is all the code, if want to skip some parts plese just load the data 
# at certain points

# download data
ftp.wind(2009)
ftp.wind(2010) # here will stop at some month but in fact we only use january

# Read data
year<-2009
months<-c("01","02","03","04","05","06","07","08","09","10","11","12")
ourdata<-read.wind(year,months)
datajan2010<-read.wind(2010,"01")
ourdata<-rbind(ourdata,datajan2010)
# write.table(ourdata,"CentralPark_NY_wind_2009_2010.dat")

# NOTE: If starting here read table
# ourdata<-read.table("CentralPark_NY_wind_2009_2010.dat",
#        header=T,colClasses=c("POSIXlt", "POSIXt","numeric","numeric"))

# Fill gaps, this is time consuming!!!
filled<-fill.wind(ourdata)
# write.csv(filled,"CPNY_wind_2009_filled.csv")

# NOTE: If starting here read table
filled<-read.csv("CPNY_wind_2009_filled.csv",stringsAsFactors =F)


# Adjust autorregressive model


# Subset up to 30 min before new year 2009-2010
library(lubridate)
library(dplyr)
filled<-tbl_df(filled)
train<-filled %>% filter(date<as.POSIXct("2009-12-31 23:31:00 EST"))
valid<-filled %>% filter(date>=as.POSIXct("2009-12-31 23:31:00 EST"))
which(filled$date=="2009-12-31 23:31:00 EST")

# package mAr for multivariate AR model
# acf()
library(mAr)
library(moments)
library(normwhn.test)


mod<-list()
test<-c()
# Fit bivariate Ar models AR(i) and test normality of residuals
for (i in 1:5){
    mod[[i]]<-mAr.est(as.matrix(train[,2:3]),i)
    test[i]<-as.numeric(normality.test1(mod[[i]]$resid))
}
# Check normality of residuals: all are normal
test


# Validation with test data, always 1 step ahead
N_val<-60
sqe<-data.frame(sqe1=c(0),sqe2=c(0),sqe3=c(0),sqe4=c(0),sqe5=c(0))
for (j in 1:N_val){
    cat("\n N equals ",j)
    modv<-list()
    if (j>1){
        data<-as.matrix(rbind(train,valid[j-1,])[,2:3])
    } else {data<-as.matrix(train[,2:3])}
    
    for (i in 1:5){
        modv[[i]]<-mAr.est(data,i)
        sim<-mAr.sim(modv[[i]]$wHat,modv[[i]]$AHat,modv[[i]]$CHat,2)[1,]
        sqe[[paste0('sqe',i)]]<-sqe[[paste0('sqe',i)]]+
            sqrt(sum((valid[j,2:3]-sim)^2))
    }
}
sqe
# best validation error N=60 at p=5 (183.9) and p=1 (209.8), others>230  
# pick p=1 for simplicity
rm(modv)
rm(data)

# check autoregressive function hand made
train_t1<-as.matrix(train[-1,2:3])
train_t0<-as.matrix(train[-(dim(train)[1]),2:3])
w_t1<-lm(train_t1~train_t0)

coefs<-w_t1$coefficients
resid<-w_t1$residuals
p<-as.numeric(normality.test1(resid)) # p<0.01 residuals are normal
cov_resid<-cov(resid)
mean_resid<-mean(resid) # ok mean is 0!

# check hand made bivariateAR1 equals mAr function 
# coefs
mod[[1]]$wHat
mod[[1]]$AHat
coefs
# OK!

# Cov matrix
mod[[1]]$CHat
cov_resid
# OK!

# Write results to csv
write.csv(cov_resid,"cov_wind_residuals.csv") # To build the gaussian shocks
write.csv(coefs,"coefs_AR1_wind.csv") # To predict the deterministic part of AR1
write.csv(valid[1:500,],"CPNY_wind_NYeve.csv")
