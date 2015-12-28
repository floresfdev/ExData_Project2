# Coursera - Exploratory Data Analysis
# Course project 2

# Plot 3
# Question: Of the four types of sources indicated by the type (point,
# nonpoint, onroad, nonroad) variable, which of these four sources have seen
# decreases in emissions from 1999-2008 for Baltimore City? Which have seen
# increases in emissions from 1999-2008? Use the ggplot2 plotting system to
# make a plot answer this question.

library(sqldf)
library(dplyr)
library(ggplot2)

# Load  data for NEI (SCC not needed for this analysis)
dataNEI <- readRDS("./data/summarySCC_PM25.rds")

# Subset only measurements of Baltimore City, Maryland (fips == "24510")
dataBaltimore <- sqldf("select * from dataNEI where fips = '24510'")

# Free memory by removing the big dataset from the environment
rm(dataNEI)

# Convert column "type" to factor to use it in facets
dataBaltimore$type <- as.factor(dataBaltimore$type)

# Convert column "year" to factor so the x-ticks will be on a discrete scale
dataBaltimore$year <- as.factor(dataBaltimore$year)

# Compute the sum of emissions by type and year for Baltimore City
# Columns "fips", "SCC" and "Pollutant" are irrelevant for this summary, 
# so we use the special summarise_each_() function to exclude those
groupByTypeAndYear <- group_by(dataBaltimore, type, year) 
emissionsByTypeAndYear <- 
    summarise_each_(groupByTypeAndYear, 
                    funs(sum), 
                    list(quote(-fips), 
                         quote(-SCC), 
                         quote(-Pollutant)))

# Make a plot with facets
plot3 <- ggplot(emissionsByTypeAndYear, aes(year, Emissions))
plot3 <- plot3 + 
    geom_point(size = 4) + 
    geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
    theme_bw() +
    labs(title = expression(paste("Total ", PM[2.5], 
                " emissions in Baltimore City, Maryland: 1999 to 2008"))) +
    labs(x = "Year") +
    labs(y = expression(paste(PM[2.5], " emissions (tons)"))) +
    facet_grid(. ~ type)

# Plot saved as PNG (resolution of 72 dpi like the default in base package)
ggsave(filename = "./plot3.png", plot = plot3, dpi = 72)