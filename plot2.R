##  Load the appropriate libraries

library(dplyr)
library(ggplot2)
library(scales)
library(data.table)

##  Read in the records from the data files

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

##  View the data
head(NEI)

str(NEI)

head(SCC)

str(SCC)

## Question 2:  Have total emissions from PM2.5 decreased in Baltimore City, 
##              Maryland (fips == “24510”) from 1999 to 2008? Use the base 
##              plotting system to make a plot answering this question.

baltimore <- NEI %>% 
        filter(fips == "24510") %>% 
        group_by(year) %>% 
        summarize(Annual.Total = sum(Emissions));
baltimore.pts <- pretty(baltimore$Annual.Total/1000);
plot(baltimore$year, baltimore$Annual.Total/1000, type = "l", lwd = 2, axes = FALSE,
     xlab = "Year", 
     ylab = expression("Total Tons of PM"[2.5]*" Emissions"), 
     main = expression("Total Tons of PM"[2.5]*" Emissions in Baltimore"));
axis(1, at = c(1999,2002,2005,2008))

axis(2, at = baltimore.pts, labels = paste(baltimore.pts, "K", sep = ""));
box()

##  Save plot 2
dev.copy(png, file="plot2.png", width=480, height=480)
dev.off()

##  Answer for Question 2:  Overall the PM 2.5 emissions have decreased from 1999
##                              to 2008 in Baltimore, however there was a spike
##                              in 2005 prior to the overall drop for the full
##                              time period
