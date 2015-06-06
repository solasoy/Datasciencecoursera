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
y <- as.numeric(data$Global_active_power)
y <- y[!is.na(y)]
hist(y,
     #breaks=seq(0, 6, by=0.5),
     main="Global Active Power",
     col="red",
     xlim=c(0,6),
     ylim=c(0,1200),
     xlab = "Global Active Power (kilowatts)")

dev.copy(png, file = "Plot1.png") 
png(filename="Plot1.png",width=480,height=480,units="px")
dev.off()
