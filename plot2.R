# Coursera - Exploratory Data Analysis
# Course project 2

# Plot 2
# Question: Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system 
# to make a plot answering this question.

library(sqldf)
library(dplyr)

# Load  data for NEI (SCC not needed for this analysis)
dataNEI <- readRDS("./data/summarySCC_PM25.rds")

# Subset only measurements of Baltimore City, Maryland (fips == "24510")
dataBaltimore <- sqldf("select * from dataNEI where fips = '24510'")

# Free memory by removing the big dataset from the environment
rm(dataNEI)

# Compute the sum of emissions by year for Baltimore City
# Columns "fips", "SCC", "Pollutant" and "type" are irrelevant for this summary,
# so we use the special summarise_each_() function to exclude those
groupByYear <- group_by(dataBaltimore, year) 
emissionsByYear <- 
    summarise_each_(groupByYear, 
                    funs(sum), 
                    list(quote(-fips), 
                         quote(-SCC), 
                         quote(-Pollutant), 
                         quote(-type)))

# Plot saved as PNG file of 480x480 pixels
par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))

png(filename = "./plot2.png", 
    width = 480, 
    height = 480, 
    units = "px")

# Let alone the limits of x and y (ranges), there is no need to change them in 
# order to answer the question
plot(emissionsByYear$year,
     emissionsByYear$Emissions, 
     type = "p",
     pch = 19,
     cex = 2,
     xaxt = "n",
     ylab = expression(paste(PM[2.5], " emissions (tons)")),
     xlab = "Years")

# Add a regression line
model <- lm(Emissions ~ year, emissionsByYear)
abline(model, col = "blue")

title(expression(paste("Total ", PM[2.5], 
                       " emissions in Baltimore City, Maryland: 1999 to 2008")))

# Add customized x-ticks for each year measured
axis(1, at = emissionsByYear$year,
     labels = emissionsByYear$year)

dev.off()