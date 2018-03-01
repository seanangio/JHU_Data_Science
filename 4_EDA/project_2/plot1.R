# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 emission 
# from all sources for each of the years 1999, 2002, 2005, and 2008.

# load libraries
library(dplyr)

# download the file
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download(url, dest = "nei_data.zip", mode = "wb") 
unzip("nei_data.zip", exdir = "./")

# read files into dataframes
if (!exists("NEI")) {
  NEI <- readRDS("summarySCC_PM25.rds")
}
if (!exists("SCC")) {
  SCC <- readRDS("Source_Classification_Code.rds")
}

# create a dataframe of summed Emissions, grouped by year
total_US <- NEI %>%
  group_by(year) %>%
  summarise(sumE = sum(Emissions))

# plot 1
png("plot1.png")
par(mar = c(5.1, 5.1, 4.1, 2.1))
barplot(total_US$sumE/10^6, 
        xlab = "Year",
        names.arg = total_US$year,
        ylab = expression(PM[2.5]*" Emissions (10^6 Tons)"),
        main = expression("Total U.S. "*PM[2.5]*" Emissions"))
dev.off()