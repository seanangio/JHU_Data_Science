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

# create the plot
png("plot1.png")
hist(power$Global_active_power, 
     col = "red", 
     main = "Global Active Power", 
     xlab = "Global Active Power (kilowatts)")

# close the device
dev.off()
