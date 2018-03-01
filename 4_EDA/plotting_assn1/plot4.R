# load libraries
library(downloader)
library(lubridate)
library(dplyr)

# download and unzip the file
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download(url, dest = "power.zip", mode = "wb") 
unzip("power.zip", exdir = "./")

# create classes vector
classes <- c("character", "character", "numeric", "numeric",
             "numeric", "numeric", "numeric", "numeric",
             "numeric")

# read in the data
hpc <- read.table(file = "household_power_consumption.txt",
                  na.strings = "?",
                  colClasses = classes, 
                  header = TRUE, sep = ";")

# parse date and time together
hpc$datetime <- dmy_hms(paste(hpc$Date, hpc$Time))

# convert Date to Date and filter for given dates
hpc$Date <- dmy(hpc$Date)
hpc <- filter(hpc, Date >= "2007-02-01" 
              & Date <= "2007-02-02")

# send to device
png("plot4.png")

# create frame
par(mfrow = c(2,2))
# plot2 in top left
with(hpc, plot(x = datetime, y = Global_active_power, 
               type = "l", 
               ylab = "Global Active Power", 
               xlab = ""))

# top left plot
with(hpc, plot(x = datetime, y = Voltage, 
               type = "l", 
               ylab = "Voltage", 
               xlab = "datetime"))

# plot3 in bottom left with legend border removed
with(hpc, plot(x = datetime, y = Sub_metering_1, 
               type = "l", 
               ylab = "Energy sub metering", 
               xlab = ""))
with(hpc, lines(x = datetime, y = Sub_metering_2, 
                type = "l", 
                col = "red"))
with(hpc, lines(x = datetime, y = Sub_metering_3, 
                type = "l", 
                col = "blue"))
legend("topright", 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
       lty = c(1,1,1), col = c("black", "red", "blue"),
       bty = "n")

# bottom right plot
with(hpc, plot(x = datetime, y = Global_reactive_power, 
               type = "l", 
               ylab = "Global_reactive_power",
               xlab = "datetime"))

# close device
dev.off()