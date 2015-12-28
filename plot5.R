# Coursera - Exploratory Data Analysis
# Course project 2

# Plot 5
# Question: How have emissions from motor vehicle sources changed from
# 1999-2008 in Baltimore City?

library(dplyr)
library(ggplot2)

# Load  data for NEI and SCC
dataNEI <- readRDS("./data/summarySCC_PM25.rds")
dataSCC <- readRDS("./data/Source_Classification_Code.rds")

# Subset only measurements of motor vehicle sources from Baltimore City, 
# Maryland (fips == "24510")
dataSourceMotorVehicle <- 
    dataSCC[grep("vehicles", dataSCC$EI.Sector, ignore.case = TRUE), ]
selectedRows <- dataNEI$SCC %in% dataSourceMotorVehicle$SCC
dataMotorVehicle <- dataNEI[(selectedRows & dataNEI$fips == "24510"), ]

# Free memory by removing the big dataset from the environment
rm(dataNEI)

# Convert column "year" to factor so the x-ticks will be on a discrete scale
dataMotorVehicle$year <- as.factor(dataMotorVehicle$year)

# Compute the sum of emissions by year for motor vehicle sources in Baltimore
# Columns "fips", "SCC", "Pollutant" and "type" are irrelevant for this summary, 
# so we use the special summarise_each_() function to exclude those
groupByYear <- group_by(dataMotorVehicle, year) 
emissionsByYear <- 
    summarise_each_(groupByYear, 
                    funs(sum), 
                    list(quote(-fips), 
                         quote(-SCC), 
                         quote(-Pollutant),
                         quote(-type)))

# Make a plot
## On title, use atop() to fake a line break while using expression(). See:
## http://stackoverflow.com/questions/13223846/ggplot2-two-line-label-with-expression
plot5 <- ggplot(emissionsByYear, aes(year, Emissions))
plot5 <- plot5 + 
    geom_point(size = 4) + 
    geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
    theme_bw() +
    labs(title = expression(
        atop(paste(PM[2.5], " emissions from motor vehicle sources"), 
             paste("in Baltimore City, Maryland: 1999 to 2008")))) +
    labs(x = "Year") +
    labs(y = expression(paste(PM[2.5], " emissions (tons)")))

# Plot saved as PNG (resolution of 72 dpi like the default in base package)
ggsave(filename = "./plot5.png", plot = plot5, dpi = 72)