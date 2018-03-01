# Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999–2008?

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

# filter SCC for any mentions of coal
SCC_coal <- SCC %>%
  filter(grepl('[Cc]oal', SCC$Short.Name) |
           grepl('[Cc]oal', SCC$EI.Sector) |
           grepl('[Cc]oal', SCC$SCC.Level.Three) |
           grepl('[Cc]oal', SCC$SCC.Level.Four))

# filter NEI for any sources found in SCC_coal
NEI_coal <- NEI %>%
  filter(SCC %in% SCC_coal$SCC)

# create a dataframe of summed Emissions, grouped by year
NEI_cs <- NEI_coal %>%
  group_by(year) %>%
  summarise(sumE = sum(Emissions))

# plot 4
png("plot4.png")
par(mar = c(5.1, 5.1, 4.1, 2.1))
barplot(NEI_cs$sumE/10^3, NEI_cs$year,
        xlab = "Year",
        names.arg = NEI_cs$year,
        main = "U.S. Emissions from Coal Combustion Sources",
        ylab = expression(PM[2.5]*" Emissions (Thousands Tons)"))
dev.off()