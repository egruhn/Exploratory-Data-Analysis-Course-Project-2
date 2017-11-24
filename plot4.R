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

## Question 4:  Across the United States, how have emissions from coal 
##              combustion-related sources changed from 1999–2008?

scc.coal <- SCC[grep("Fuel Comb.*Coal", SCC$EI.Sector),  ];
scc.coal.list <- unique(scc.coal$SCC);
nei.coal <- subset(NEI, SCC %in% scc.coal.list);
nei.coal <- nei.coal %>% group_by(type, year) %>% summarize(Annual.Total = sum(Emissions))
nei.coal.total <- nei.coal %>% group_by(year) %>% summarize(Annual.Total = sum(Annual.Total)) %>% mutate(type = "TOTAL");
nei.coal <- nei.coal %>% select(Annual.Total, type, year);
nei.coal <- bind_rows(nei.coal, nei.coal.total);
nei.coal$type <- factor(nei.coal$type, levels = c("TOTAL", "ON-ROAD", "NON-ROAD", "POINT", "NONPOINT")); # Re-order factor levels to they plot in the order we wish
ggplot(nei.coal, aes(x = factor(year), y = Annual.Total, fill = type)) +
        geom_bar(stat = "identity") +
        facet_grid(. ~ type) +
        xlab("Year") +
        ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
        ggtitle(expression(atop("Total Tons of PM"[2.5]*" Emissions in the United States", 
                                paste("from Coal Combustion-Related Sources")))) +
        theme(plot.margin = unit(c(1,1,1,1), "cm")) +
        scale_y_continuous(labels = comma) +
        scale_fill_brewer(palette = "Dark2") +
        guides(fill = FALSE)

##  Save plot 4
dev.copy(png, file="plot4.png", width=480, height=480)
dev.off()

##  Answer for Question 4:  Total emissions from coal combustion-relates sources 
##                              have decreased from 1999-2008, with a significant
##                              drop from 2005 to 2008.
