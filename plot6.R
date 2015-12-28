# Coursera - Exploratory Data Analysis
# Course project 2

# Plot 6
# Question: Compare emissions from motor vehicle sources in Baltimore City 
# with emissions from motor vehicle sources in Los Angeles County, California
# (fips == "06037"). Which city has seen greater changes over time in motor
# vehicle emissions?

library(dplyr)
library(ggplot2)

# Load  data for NEI and SCC
dataNEI <- readRDS("./data/summarySCC_PM25.rds")
dataSCC <- readRDS("./data/Source_Classification_Code.rds")

# Subset only measurements of motor vehicle sources for both Baltimore City, 
# Maryland (fips=="24510") and Los Angeles County, California (fips=="06037")
dataSourceMotorVehicle <- 
    dataSCC[grep("vehicles", dataSCC$EI.Sector, ignore.case = TRUE), ]
selectedRows <- dataNEI$SCC %in% dataSourceMotorVehicle$SCC
dataMotorVehicle <- 
    dataNEI[(selectedRows & 
            (dataNEI$fips == "24510" | dataNEI$fips == "06037")), ]

# Free memory by removing the big dataset from the environment
rm(dataNEI)

# Convert column "year" to factor so the x-ticks will be on a discrete scale
dataMotorVehicle$year <- as.factor(dataMotorVehicle$year)

# Convert column "fips" to factor so it can be used in the plot facets
dataMotorVehicle$fips <- as.factor(dataMotorVehicle$fips)

# Add name of the county: fips level 1 = LA, level 2 = Baltimore
dataMotorVehicle$county <- 
    factor(dataMotorVehicle$fips, 
           labels = c("Los Angeles County, California",
                      "Baltimore City, Maryland"))

# Compute the sum of emissions by county and year for motor vehicle sources
# in Baltimore City, Maryland and Los Angeles County, California.
# Columns "fips", SCC", "Pollutant" and "type" are irrelevant for this summary,
# so we use the special summarise_each_() function to exclude those
groupByCountyAndYear <- group_by(dataMotorVehicle, county, year) 
emissionsByCountyAndYear <- 
    summarise_each_(groupByCountyAndYear, 
                    funs(sum), 
                    list(quote(-fips),
                         quote(-SCC), 
                         quote(-Pollutant),
                         quote(-type)))

# Make a plot
## On title, use atop() to fake a line break while using expression(). See:
## http://stackoverflow.com/questions/13223846/ggplot2-two-line-label-with-expression
plot6 <- ggplot(emissionsByCountyAndYear, aes(year, Emissions))
plot6 <- plot6 + 
    geom_point(size = 4) + 
    geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
    theme_bw() +
    labs(title = expression(
        atop(paste(PM[2.5], " emissions from motor vehicle sources"), 
             paste("in Los Angeles County and Baltimore City: 1999 to 2008")))) +
    labs(x = "Year") +
    labs(y = expression(paste(PM[2.5], " emissions (tons)"))) +
    facet_grid(. ~ county)

# Plot saved as PNG (resolution of 72 dpi like the default in base package)
ggsave(filename = "./plot6.png", plot = plot6, dpi = 72)