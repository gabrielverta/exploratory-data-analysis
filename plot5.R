# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
library(dplyr)
library(ggplot2)

pm25Data <- function(){
  readRDS("summarySCC_PM25.rds")
}

classifications <- function(){
  readRDS("Source_Classification_Code.rds")
}

pm25Classification <- function(data, classification){
  inner_join(data, classification, by=c("SCC" = "SCC"))
}

parseNumber <- function(num){
  paste(sprintf("%.1f", num / 1000000), "M", sep = "")
}

plot5 <- function(data, classification){
  baltimore <- filter(data, fips == "24510")
  data_classification <- pm25Classification(baltimore, classification)
  sectors <- unique(classification$EI.Sector)
  coal_sectors <- sectors[grep("^Mobile", sectors)]
  coal_data <- filter(data_classification, EI.Sector %in% coal_sectors)
  
  group <- group_by(coal_data, year, EI.Sector)
  data <- summarise(group, total=sum(Emissions))
  
  qplot(year, total, data=data, color=EI.Sector, geom=c("point", "smooth"), main="Motor Vehicle PM2.5 Emissions in Baltimore City")
}