# Coursera - Exploratory Data Analysis
# Course project 2

# Plot 4
# Question: Across the United States, how have emissions from coal 
# combustion-related sources changed from 1999-2008?

library(dplyr)
library(ggplot2)

# Load  data for NEI and SCC
dataNEI <- readRDS("./data/summarySCC_PM25.rds")
dataSCC <- readRDS("./data/Source_Classification_Code.rds")

# Subset only measurements of coal combustion related sources
dataSourceCoal <- dataSCC[grep("coal", dataSCC$EI.Sector, ignore.case = TRUE), ]
selectedRows <- dataNEI$SCC %in% dataSourceCoal$SCC
dataCoal <- dataNEI[selectedRows, ]

# Free memory by removing the big dataset from the environment
rm(dataNEI)

# Convert column "year" to factor so the x-ticks will be on a discrete scale
dataCoal$year <- as.factor(dataCoal$year)

# Compute the sum of emissions by year for coal combustion-related sources
# Columns "fips", "SCC", "Pollutant" and "type" are irrelevant for this summary, 
# so we use the special summarise_each_() function to exclude those
groupByYear <- group_by(dataCoal, year) 
emissionsByYear <- 
    summarise_each_(groupByYear, 
                    funs(sum), 
                    list(quote(-fips), 
                         quote(-SCC), 
                         quote(-Pollutant),
                         quote(-type)))

# Add a column with the emissions in thousands of tons for plotting purposes
emissionsByYear$ThousandsOfTons <- emissionsByYear$Emissions / 1000

# Make a plot
## On title, use atop() to fake a line break while using expression(). See:
## http://stackoverflow.com/questions/13223846/ggplot2-two-line-label-with-expression
plot4 <- ggplot(emissionsByYear, aes(year, ThousandsOfTons))
plot4 <- plot4 + 
    geom_point(size = 4) + 
    geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
    theme_bw() +
    labs(title = expression(
        atop(paste(PM[2.5], " emissions from coal combustion-related"), 
             paste("sources in the United States: 1999 to 2008")))) +
    labs(x = "Year") +
    labs(y = expression(paste(PM[2.5], " emissions (thousands of tons)")))

# Plot saved as PNG (resolution of 72 dpi like the default in base package)
ggsave(filename = "./plot4.png", plot = plot4, dpi = 72)