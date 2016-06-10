setwd("/Users/mengdiyue/Downloads/NEI-SCC")

#read in the data
SCC <- readRDS("Source_Classification_Code.rds")
NEI <- readRDS("summarySCC_PM25.rds")
rownames(NEI) <- NULL

#1. Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
#for each of the years 1999, 2002, 2005, and 2008.
library(dplyr)
aggre <- aggregate(Emissions ~ year,NEI, sum)
png("plot1.png", width=480,height=480,units="px")
barplot(aggre$Emissions, names.arg = aggre$year, xlab = "Year", ylab = "PM2.5 Emissions", main = "Total PM2.5 Emission from All Sources")
dev.off()

#2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland from 1999 to 2008?
baltimore <- NEI[NEI$fips == "24510",]
library(dplyr)
aggreBaltimore <- aggregate(Emissions ~ year,baltimore, sum)
png("plot2.png", width=480,height=480,units="px")
barplot(aggreBaltimore$Emissions, names.arg = aggreBaltimore$year, xlab = "Year", ylab = "PM2.5 Emissions", main = "Total PM2.5 Emission from All Sources in Baltimore, Maryland")
dev.off()

#3. Of the four types of sources indicated by the type variable, which decreases/increases from 1999-2008 in Baltimore city?
library(ggplot2)
png("plot3.png", width=480,height=480,units="px")
ggplot(baltimore,aes(factor(year),Emissions)) + geom_bar(stat = "identity") + facet_grid(.~type) + labs(x="year", y="PM2.5 Emissions", title="Total PM2.5 Emissions in Baltimore by Source")
dev.off()

#4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
coal_combustion <- grepl("Combustion", SCC$SCC.Level.One) & grepl("Coal", SCC$SCC.Level.Three)
coal_combustionSCC <- SCC[coal_combustion,]$SCC
coal_combustionNEI <- NEI[NEI$SCC %in% coal_combustionSCC,]

library(ggplot2)
png("plot4.png", width=480,height=480,units="px")
ggplot(coal_combustionNEI, aes(factor(year), Emissions)) + geom_bar(stat = "identity") + labs(x="Year", y="PM2.5 Emissions", title="Emissions from Coal Combustion-related Sources from 1999–2008")
dev.off()

#5.How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
baltimore <- NEI[NEI$fips == "24510",]
Vehicle <- grepl("Vehicles", SCC$SCC.Level.Two)
VehicleSCC <- SCC[Vehicle,]$SCC
VehicleBaltimore <- baltimore[baltimore$SCC %in% VehicleSCC,]

library(ggplot2)
png("plot5.png", width=480,height=480,units="px")
ggplot(VehicleBaltimore, aes(factor(year), Emissions)) + geom_bar(stat = "identity") + labs(x="Year", y="PM2.5 Emissions", title="Emissions from Motor Vehicle Sources from 1999–2008 in Baltimore")
dev.off()

#6.Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California.
#Which city has seen greater changes over time in motor vehicle emissions?

library(dplyr)
library(ggplot2)
baltimore <- NEI[NEI$fips == "24510",]
LA <- NEI[NEI$fips == "06037",]

#find vehicle-related SSCs
Vehicle <- grepl("Vehicles", SCC$SCC.Level.Two)
VehicleSCC <- SCC[Vehicle,]$SCC

#subset vehicle-related data for Baltimore and LA
VehicleBaltimore <- baltimore[baltimore$SCC %in% VehicleSCC,]
VehicleLA <- LA[LA$SCC %in% VehicleSCC,]

#organize emissions data by year
aggreBaltimore <- aggregate(Emissions ~ year,VehicleBaltimore, sum)
aggreLA <- aggregate(Emissions ~ year,VehicleLA, sum)
aggreBaltimore$city <- "Baltimore"
aggreLA$city <- "LA"
total <- rbind(aggreBaltimore, aggreLA)

#plot the line graph
png("plot6.png", width=480,height=480,units="px")
ggplot(total, aes(x = factor(year), y = Emissions, color = city, group = city)) + geom_line() + labs(x="Year", y="PM2.5 Emissions", title="Emissions from Motor Vehicle Sources in Baltimore and LA")
dev.off()