setwd("C://Users//solasoy//Dropbox//R//Coursera//Exploratory Data Analysis")

print("Reading data from file, please wait...")
x <- read.table("household_power_consumption.txt")
print("done!")


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
        if (substr(f,1,indx-1) == '2/1/2007' | substr(f,1,indx-1) == '2/2/2007'){
                h <- list()
                flag=0
                while (grepl(";",f) == TRUE) {
                        indx <- regexpr("[;]",f)
                        h <- c(h,substr(f,1,indx-1))
                        if (h=="?"){
                                flag=1
                                break
                        }
                        f <- substr(f,indx+1,nchar(f)) 
                }
                if (flag==1) {next}
                
                h <- c(h,f)
                xout <- rbind(xout,h)     
        }
}

data <- data.frame(xout)
colnames(data) <- label
data$Date <- as.Date(as.character(data$Date),format="%m/%d/%Y")

print("Done!")

#Plot2-------------------------------------------------
tm <- paste(as.character(data$Date),as.character(data$Time),sep=" ")
tm <- strptime(tm,format="%Y-%m-%d %H:%M:%S")
plot(tm,as.numeric(data$Global_active_power),
     type="l",xlab="",ylab="Global Active Power (kilowatts)")
axis.Date(1,at=seq(tm[1],tm[length(tm)],by=60),format="%d")

dev.copy(png, file = "Plot2.png") 
png(filename="Plot2.png",width=480,height=480,units="px")
dev.off()
