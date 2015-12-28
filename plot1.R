# Coursera - Exploratory Data Analysis
# Course project 2

# Plot 1
# Question: Have total emissions from PM2.5 decreased in the United States 
# from 1999 to 2008? Using the base plotting system, make a plot showing the 
# total PM2.5 emission from all sources for each of the years 1999, 2002, 2005,
# and 2008.

library(dplyr)

# Load  data for NEI (SCC not needed for this analysis)
dataNEI <- readRDS("./data/summarySCC_PM25.rds")

# Compute the sum of emissions by year
# Columns "fips", "SCC", "Pollutant" and "type" are irrelevant for this summary,
# so we use the special summarise_each_() function to exclude those
groupByYear <- group_by(dataNEI, year) 
emissionsByYear <- 
    summarise_each_(groupByYear, 
                    funs(sum), 
                    list(quote(-fips), 
                         quote(-SCC), 
                         quote(-Pollutant), 
                         quote(-type)))

# Add a column with the emissions in thousands of tons for plotting purposes
emissionsByYear$ThousandsOfTons <- emissionsByYear$Emissions / 1000

# Plot saved as PNG file of 480x480 pixels
par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))

png(filename = "./plot1.png", 
    width = 480, 
    height = 480, 
    units = "px")

# Let alone the limits of x and y (ranges), there is no need to change them in 
# order to answer the question
plot(emissionsByYear$year,
     emissionsByYear$ThousandsOfTons, 
     type = "p",
     pch = 19,
     cex = 2,
     xaxt = "n",
     ylab = expression(paste(PM[2.5], " emissions (thousands of tons)")),
     xlab = "Years")

# Add a regression line
model <- lm(ThousandsOfTons ~ year, emissionsByYear)
abline(model, col = "blue")

title(expression(paste("Total ", PM[2.5], 
                       " emissions in the United States: 1999 to 2008")))

# Add customized x-ticks for each year measured
axis(1, at = emissionsByYear$year,
     labels = emissionsByYear$year)

dev.off()