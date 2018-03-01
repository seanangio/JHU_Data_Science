# Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999
# to 2008? Use the base plotting system to make a plot answering this question.

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

# filter dataset for only Baltimore City, MD
bcm <- NEI %>%
  filter(fips == "24510")

# create a dataframe of summed Emissions for bcm, grouped by year
total_bcm <- bcm %>%
  group_by(year) %>%
  summarise(sumE = sum(Emissions))

# plot 2
png("plot2.png")
par(mar = c(5.1, 5.1, 4.1, 2.1))
barplot(total_bcm$sumE,
        xlab = "Year",
        names.arg = total_bcm$year,
        ylab = expression(PM[2.5]*" Emissions (Tons)"),
        main = expression("Total Baltimore City, MD "*PM[2.5]*" Emissions"))
dev.off()