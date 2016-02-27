# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
library(dplyr)
library(ggplot2)

pm25Data <- function(){
  readRDS("summarySCC_PM25.rds")
}

parseNumber <- function(num){
  paste(sprintf("%.1f", num / 1000000), "M", sep = "")
}

plot3 <- function(data){
  baltimore <- filter(data, fips == "24510")
  group <- group_by(baltimore, year, type)
  data <- summarise(group, total=sum(Emissions))
  qplot(year, total, data=h, color=type, geom=c("point", "smooth"), main="PM2.5 Emissions in Baltimore City")
}