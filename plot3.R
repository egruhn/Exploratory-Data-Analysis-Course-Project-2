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

## Question 3:  Of the four types of sources indicated by the type (point, nonpoint,
##              onroad, nonroad) variable, which of these four sources have seen
##              decreases in emissions from 1999-2008 for Baltimore City?  Which
##              have seen increases in emissions form 1999-2008?  Use the ggplot2
##              plotting system to make a plot to answer this question.

nei.baltimore <- NEI %>% filter(fips == "24510") %>% group_by(type, year) %>% summarize(Annual.Total = sum(Emissions));
nei.baltimore$type <- factor(nei.baltimore$type, levels = c("ON-ROAD", "NON-ROAD", "POINT", "NONPOINT")) # Re-order factor levels so they plot in the order we wish
ggplot(nei.baltimore, aes(x = factor(year), y = Annual.Total, fill = type)) + 
        geom_bar(stat = "identity") + 
        facet_grid(. ~ type) + 
        xlab("Year") + 
        ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
        ggtitle(expression("Total Tons of PM"[2.5]*" Emissions in Baltimore by Source Type")) +
        theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        scale_y_continuous(labels = comma) +
        guides(fill = FALSE)

##  Save plot 3
dev.copy(png, file="plot3.png", width=480, height=480)
dev.off()

##  Answer for Question 3:  ON-ROAD, NON-ROAD, and NON-POINT sources have all seen
##                              decreases in PM 2.5 emissions from 1999-2008.  POINT
##                              sources have seen an increase over the 1999-2008
##                              time period.
