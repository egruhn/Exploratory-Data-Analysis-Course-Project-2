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

## Question 6:  Compare emissions from motor vehicle sources in Baltimore City 
##              with emissions from motor vehicle sources in Los Angeles County, 
##              California (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽"). Which city has seen greater
##              changes over time in motor vehicle emissions?

scc.vehicles <- SCC[grep("Mobile.*Vehicles", SCC$EI.Sector),  ]; # Pattern match mobile vehicles in SCC description
scc.vehicles.list <- unique(scc.vehicles$SCC); # Create motor vehicle lookup list by SCC
nei.vehicles <- subset(NEI, SCC %in% scc.vehicles.list); # Filter for motor vehicle sources
nei.vehicles <- nei.vehicles %>% filter(fips == "24510"| fips == "06037"); # Filter for Baltimore City or Los Angeles County
nei.vehicles$fips[nei.vehicles$fips == "24510"] <- "Baltimore";
nei.vehicles$fips[nei.vehicles$fips == "06037"] <- "Los Angeles";
nei.vehicles <- merge(x = nei.vehicles, 
                      y = scc.vehicles[, c("SCC", "SCC.Level.Two")], by = "SCC"); # Join in descriptive data on SCC codes
nei.vehicles <- nei.vehicles %>% group_by(fips, year, SCC.Level.Two) %>% 
        summarize(Annual.Total = sum(Emissions));
nei.vehicles.total <- nei.vehicles %>% group_by(fips, year) %>% 
        summarize(Annual.Total = sum(Annual.Total)) %>% mutate(SCC.Level.Two = "Total");
nei.vehicles <- bind_rows(nei.vehicles, nei.vehicles.total);
nei.vehicles$SCC.Level.Two <- factor(nei.vehicles$SCC.Level.Two, 
                                     levels = c("Total", "Highway Vehicles - Diesel",
                                                "Highway Vehicles - Gasoline"));
ggplot(nei.vehicles, aes(x = factor(year), y = Annual.Total, fill = SCC.Level.Two)) +
        geom_bar(stat = "identity") +
        facet_grid(fips ~ SCC.Level.Two) + 
        xlab("Year") +
        ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
        ggtitle(expression(atop("Total Tons of PM"[2.5]*" Emissions from Motor Vehicle Sources", 
                                paste("in Baltimore City, MD and Los Angeles County, CA")))) +
        theme(plot.title = element_text(hjust = 0.5)) + # Center the plot title
        theme(plot.margin = unit(c(1,1,1,1), "cm")) + # Adjust plot margins
        scale_fill_brewer(palette = "Set1") +
        guides(fill = FALSE)


##  Save plot 6a
dev.copy(png, file="plot6a.png", width=480, height=480)
dev.off()

scc.vehicles <- SCC[grep("Mobile.*Vehicles", SCC$EI.Sector),  ]; # Pattern match mobile vehicles in SCC description
scc.vehicles.list <- unique(scc.vehicles$SCC); # Create motor vehicle lookup list by SCC
nei.vehicles <- subset(NEI, SCC %in% scc.vehicles.list); # Filter for motor vehicle sources
nei.vehicles <- nei.vehicles %>% filter(fips == "24510"| fips == "06037"); # Filter for Baltimore City or Los Angeles County
nei.vehicles$fips[nei.vehicles$fips == "24510"] <- "Baltimore";
nei.vehicles$fips[nei.vehicles$fips == "06037"] <- "Los Angeles";
nei.vehicles <- merge(x = nei.vehicles, y = scc.vehicles[, c("SCC", "SCC.Level.Two")], by = "SCC"); # Join in descriptive data on SCC codes
nei.vehicles <- nei.vehicles %>% 
        group_by(fips, year, SCC.Level.Two) %>% summarize(Annual.Total = sum(Emissions));
nei.vehicles.total <- nei.vehicles %>% 
        group_by(fips, year) %>% summarize(Annual.Total = sum(Annual.Total)) %>% mutate(SCC.Level.Two = "Total");
nei.vehicles <- bind_rows(nei.vehicles, nei.vehicles.total);
nei.vehicles$SCC.Level.Two <- factor(nei.vehicles$SCC.Level.Two, levels = c("Total", "Highway Vehicles - Diesel", 
                                                                            "Highway Vehicles - Gasoline"));
ggplot(nei.vehicles, aes(x = factor(year), y = Annual.Total, fill = SCC.Level.Two)) +
        geom_bar(stat = "identity") +
        facet_grid(fips ~ SCC.Level.Two, scales = "free") + # Setup facets and allow scales to adjust to data in each location
        xlab("Year") +
        ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
        ggtitle(expression(atop("Total Tons of PM"[2.5]*" Emissions from Motor Vehicle Sources", 
                                paste("in Baltimore City, MD and Los Angeles County, CA")))) +
        theme(plot.title = element_text(hjust = 0.5)) + # Center the plot title
        theme(plot.margin = unit(c(1,1,1,1), "cm")) + # Adjust plot margins
        scale_fill_brewer(palette = "Set1") +
        guides(fill = FALSE)

##  Save plot 6b
dev.copy(png, file="plot6b.png", width=480, height=480)
dev.off()


## It’s still not very clear which has a greater change in percentage terms. 
## We can use data.table function called shift to help us calculate percent 
## change between measurement years. 

nei.vehicles.DT <- data.table(nei.vehicles)
yoyFunc <- function(x) {x/shift(x)}
yoy.cols <- c("Annual.Total")
nei.vehicles.DT <- nei.vehicles.DT[, paste0("Percent.Change.", yoy.cols) := lapply(.SD, yoyFunc), by = "fips,SCC.Level.Two", .SDcols = yoy.cols]
nei.vehicles.DT <- mutate(nei.vehicles.DT, Percent.Change.Annual.Total = Percent.Change.Annual.Total - 1)
ggplot(nei.vehicles.DT, aes(x = factor(year), y = Percent.Change.Annual.Total, fill = SCC.Level.Two)) +
        geom_bar(stat = "identity") +
        facet_grid(fips ~ SCC.Level.Two) +
        xlab("Year") +
        ylab(expression("% Change From Prior Measurement")) + 
        ggtitle(expression(atop("Percentage Change in Total Tons of PM"[2.5]*" Emissions from Motor Vehicle", 
                                paste("Sources in Baltimore City, MD and Los Angeles County, CA")))) +
        theme(plot.title = element_text(hjust = 0.5)) + # Center the plot title
        theme(plot.margin = unit(c(1,1,1,1), "cm")) + # Adjust plot margins
        scale_fill_brewer(palette = "Set1") +
        guides(fill = FALSE)

##  Save plot 6c
dev.copy(png, file="plot6c.png", width=480, height=480)
dev.off()

## As we can see from this bar chart, Baltimore City appears to have a more meaningful decline in PM2.5 emissions from motor vehicle sources than Los Angeles County. It is tempting to use use summary to look at the Percent.Change.Annual.Total in each location to find the average Percent.Change.Annual.Total; however, this is not correct. 
## The correct way to do this is to calculate the growth rate, defined as: 
## [[Ending Quantity - Beginning QuantityNumber of Years]÷Beginning Quantity]×100
## 

## We can do this easily using dplyr.

CAGR.df <- nei.vehicles.DT %>% 
        group_by(fips, SCC.Level.Two) %>% 
        summarize(N.Years = max(year) - min(year), 
                  Beginning.Qty = Annual.Total[which(year==min(year))],
                  Ending.Qty = Annual.Total[which(year==max(year))],
                  CAGR = ((Ending.Qty-Beginning.Qty)/N.Years)/Beginning.Qty);
CAGR.df 

## As we can see from using summary below, the annual growth rate is overstated by taking a simple 
## mean of the Percent.Change.Annual.Total. First for Baltimore:

summary(nei.vehicles.DT$Percent.Change.Annual.Total[nei.vehicles.DT$fips=="Baltimore"])

## Now for Los Angeles:

summary(nei.vehicles.DT$Percent.Change.Annual.Total[nei.vehicles.DT$fips=="Los Angeles"])

##  Answer for question 6:  Baltimore City has seen a negative 8% compound annual 
##                              growth rate (CAGR) in emissions from motor vehicles 
##                              over the 9 years of data whereas Los Angeles has 
##                              experienced a positive 0.5% CAGR over the same period.
##                              The data indicate that motor vehicle emissions have 
##                              declined in Baltimore City and increased in Los Angeles.