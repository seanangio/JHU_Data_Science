# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

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

# filter SCC for onroad
onroad <- SCC %>%
  filter(Data.Category == "Onroad")

# filter dataset for only Baltimore City, MD
bcm <- NEI %>%
  filter(fips == "24510")

# filter bcm for onroad SCC
bcm_road <- bcm %>%
  filter(SCC %in% onroad$SCC)

# create a dataframe of summed Emissions, grouped by year
bcm_road_sum <- bcm_road %>%
  group_by(year) %>%
  summarise(sumE = sum(Emissions))

# plot 5
png("plot5.png")
par(mar = c(5.1, 5.1, 4.1, 2.1))
barplot(bcm_road_sum$sumE, bcm_road_sum$year,
        xlab = "Year",
        names.arg = bcm_road_sum$year,
        main = "Emissions from Motor Vehicle Sources in Baltimore City, MD",
        ylab = expression(PM[2.5]*" Emissions (Tons)"))
dev.off()

