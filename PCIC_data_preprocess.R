library(ncdf);library(stringr);library(lubridate);library(dplyr)
getwd()
##DOWNLOAD DATA TO RESCPECTIVE RCP SCENARIO FOLDERS E.G. ALL 8.5 TO 8.5 FOLDER, 4.5 TO 4.5 AND SO ON
#EXAMPLE DATA
#KAMLOOPS SOUTH THOMPSON RIVER INTAKE
#50.676093,120.309370
#DOWNLOAD DATA FROM PCIC/ENVT CANADA - DAILY PRECIP AND MEAN TEMP AND MAKE AN R DATA FILE
#FIGURE OUT WHICH CELL IN THE DOWNLOADED DATA CORRESPONDS TO YOUR STUDY AREA
nc<- open.ncdf(file.choose())# read ONE NCDF file
str(nc)
print(nc)
summary(nc)

precip<-get.var.ncdf(nc, "pr") ##Reads data value from the precipitation variable 
precip[precip=="32768"] <- NA ##code missing value as NA
x<-get.var.ncdf(nc,"lon") ### Reads data value from the LONGITUDE variable 
y<-get.var.ncdf(nc,"lat") ### Reads data value from the LATITUDE variable 
time<-get.var.ncdf(nc,"time")### Reads data value from the LATITUDE variable 
#summary(precip[2,2,])#summary for precipitation [pixel2,pixel 2, all time(1950-2100 in days)]

#16 (y) X 30 (x) pixels
nc$dim$lon$vals <-x # get longitude
nc$dim$lat$vals <-y ## get lat
nc$dim$time$vals <-time # # get time in days since 1950-01-01
time = as.Date(time,origin = '1950-01-01') # format time 1990-2100
#lat <- rev(x)#reverse for display
#precip<- precip[, ncol(precip):1,  ] #lat being our dimension number 2
str(precip)#note cells
head(precip) # columns are lat(y), rows(x) are lon
precip19500106 <- precip[,,1 ] #precipitation for day days since 1950-01-01(Jan 6, 1950)
library(fields)
image.plot(x,y,precip19500106,xlab="Lon",ylab="Lat")
grid(nx = dim(precip)[1], ny = dim(precip)[2], col = "black", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
title (main="Precipitation (mm/day) in Study area on Jan 6,1950 ")
points(-120.309370,50.676093)#Enter your lat long and note the cell in the image
points(x[4],y[3])# Do BOTH CORRESPOND?
precip[4,3,1]

#NOW WE KNOW WHICH PIXEL AND READY TO PUT THE DATA TOGETHER, Remove all files from workspace before proceeding
XU<-4 #ENTER CELL ADDRESS HERE FOR X
YU<-3 #ENTER CELL ADDRESS HERE FOR Y

#Change directory to appropriate rcp scenario before proceeding(use Session->Set Working Directory from R Studio Menu)

files <- list.files(pattern=".nc$") #READ THE DOWNLOADED netcdf FILES
files
nc<- open.ncdf(files[1]) #read first netcdf file
#str(nc);print(nc);summary(nc)
time<-get.var.ncdf(nc,"time")### Reads data value from the TIME variable 
time = as.Date(time,origin = '1950-01-01') # format time 1990-2100

#dataframe dummy with time
t<-data.frame(tm=time)
t$week=rep(1:round((nrow(t)/7)), each=7,length.out=nrow(t))
t$month<-month(t$tm)
t$year<-year(t$tm)

for(i in 1:length(files)) 
{
  nc<- open.ncdf(files[i])
  precip<-get.var.ncdf(nc, "pr") ##Reads data value from the precipitation variable 
  precip[precip=="32768"] <- NA ##code missing value as NA
   
  #Precip for stn698
  precip<-precip[XU,YU,] # rows is Y, coulmn is x extract by [x,y,time] or [long lat time] 
  t<-data.frame(t,precip)
  colnames(t)[i+4]<-str_sub(files[i],start=39, end=-49)
  }

rm(files,i,nc,precip,time,x,y)
print(object.size(t),units = "auto")
getwd()


##NOW AGGREGATE TO WEEKLY DATA###
  tw<- t%>%select(-tm,-month,-year)%>%group_by(week)%>%summarize_each(funs(sum))%>%ungroup()%>%arrange(week)

  #write.csv(t,"RCP4.5.csv")#your RCP
