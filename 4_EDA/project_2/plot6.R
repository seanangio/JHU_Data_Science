# Compare emissions from motor vehicle sources in Baltimore City with emissions 
# from motor vehicle sources in Los Angeles County, California (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽").
# Which city has seen greater changes over time in motor vehicle emissions?

# load libraries
library(dplyr)
library(ggplot2)

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

# filter NEI for BCM and LAC
bcm_lac <- NEI %>%
  filter(fips == "06037" | fips == "24510")

# filter bmc_lac for onroad SCC
bcm_lac_road <- bcm_lac %>%
  filter(SCC %in% onroad$SCC)

# create a dataframe of summed Emissions, grouped by year and county
bcm_lac_road_sum <- bcm_lac_road %>%
  group_by(year, fips) %>%
  summarise(sumE = sum(Emissions))

# plot 6
png("plot6.png")
qplot(x = year, y = sumE, 
      data = bcm_lac_road_sum, 
      color = factor(fips, labels = c("LAC", "BCM"))) + 
      labs(color = "County") +
      geom_line() +
      xlab("Year") +
      ylab(expression(PM[2.5]*" Emissions (Tons)")) +
      ggtitle('Baltimore City, MD and Los Angeles County, CA Emissions by Type') +
      theme(plot.title = element_text(hjust = 0.5))
dev.off()