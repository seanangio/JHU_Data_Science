# Of the four types of sources indicated by the 𝚝𝚢𝚙𝚎 (point, nonpoint, onroad, nonroad) variable,
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

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

# filter dataset for only Baltimore City, MD
bcm <- NEI %>%
  filter(fips == "24510")

# convert type variable from character to factor
bcm$type <- as.factor(bcm$type)

# convert year variable to factor (for plotting)
bcm$year <- as.factor(as.character(bcm$year))

# create a dataframe of summed Emissions for bcm, grouped by year and type
bcm_type <- bcm %>%
  group_by(type, year) %>%
  summarise(sumE = sum(Emissions))

# plot 3
png("plot3.png")
ggplot(data = bcm_type, 
       aes(x = year, y = sumE)) +
  geom_bar(stat = "identity") + 
  facet_grid(. ~ type) +
  xlab("Year") +
  ylab(expression(PM[2.5]*" Emissions (Tons)")) +
  ggtitle('Baltimore City, MD Emissions by Type') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()