setwd("C://Users//solasoy//Dropbox//R//Coursera//Exploratory Data Analysis")

# Load Data ----------------------------------------------
print("Reading data from file, please wait (this will take just a little while)...")
x <- read.table("household_power_consumption.txt")
print("Done!")

# Clean and subset data ----------------------------------------
print("Now cleaning data....")
writeflag=0

temp_label <- as.character(x[1,])
label <- list()
while (grepl(";",temp_label) == TRUE) {
        indx <- regexpr("[;]",temp_label)
        label <- c(label,substr(temp_label,1,indx-1))
        temp_label <- substr(temp_label,indx+1,nchar(temp_label)) 
}
label <- c(label,temp_label)

d <- as.character(x[1:dim(x)[1],])

xout <- list()
for (j in 2:dim(x)[1]){
  f <- d[j]  
  
  indx <- regexpr("[;]",f)
  if (substr(f,1,indx-1) == '1/2/2007' | substr(f,1,indx-1) == '2/2/2007'){
    h <- list()
    
    while (grepl(";",f) == TRUE) {
      indx <- regexpr("[;]",f)
      
      if (grepl("[?]",substr(f,1,indx-1))==FALSE){
        h <- c(h,substr(f,1,indx-1))
      }
      else {
        h <- c(h,"NA")
      }
      
      
      f <- substr(f,indx+1,nchar(f)) 
    }
    
    h <- c(h,f)
    xout <- rbind(xout,h)     
  }
}

data <- data.frame(xout)
colnames(data) <- label
data$Date <- as.Date(as.character(data$Date),format="%d/%m/%Y")

print("Done!")

# Plot Data --------------------------------------

# Subplot1-------------
par(mfrow=c(2,2))
tm <- paste(as.character(data$Date),as.character(data$Time),sep=" ")
tm <- strptime(tm,format="%Y-%m-%d %H:%M:%S")
plot(tm,as.numeric(data$Global_active_power),
     type="l",xlab="",
     ylab="Global Active Power (kilowatts)")
axis.Date(1,at=seq(tm[1],tm[length(tm)],by=60),format="%d")

# Subplot2--------------
plot(tm,as.numeric(data$Voltage),xlab="datetime",ylab="Voltage",type="l")
axis.Date(1,at=seq(tm[1],tm[length(tm)],by=60),format="%d")


# Subplot3----------
plot(c(tm[1],tm[length(tm)]),c(0,50),type="n",
     xlab="",
     ylab="Energy Sub Metering")
lines(tm,as.numeric(data$Sub_metering_1),col="black",type="l")
lines(tm,as.numeric(data$Sub_metering_2),col="red",type="l")
lines(tm,as.numeric(data$Sub_metering_3),col="blue",type="l")
legend("topright", 
       c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       lty=c(1,1),
       lwd=c(1,1),
       col=c("black","red","blue"),
       cex=0.6,
       bty="n")
axis.Date(1,at=seq(tm[1],tm[length(tm)],by=60),format="%d")

# Subplot4---------
plot(tm,as.numeric(data$Global_reactive_power),xlab="datetime",
     ylab="Global_reactive_power",type="l")
axis.Date(1,at=seq(tm[1],tm[length(tm)],by=60),format="%d")


#dev.copy(png, file = "Plot4.png") 
png(filename="Plot4.png",width=480,height=480,units="px")
dev.off()