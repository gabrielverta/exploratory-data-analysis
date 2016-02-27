# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
library(dplyr)

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

plot4 <- function(data, classification){
  data_classification <- pm25Classification(data, classification)
  sectors <- unique(classification$EI.Sector)
  coal_sectors <- sectors[grep("Coal$", sectors)]
  coal_data <- filter(data_classification, EI.Sector %in% coal_sectors)
  
  group <- group_by(coal_data, year)
  data <- summarise(group, total=sum(Emissions))
  yrng <- range(data$total)
  ylabels <- c(parseNumber(yrng[1]), parseNumber(yrng[2]))
  plot(data, pch=19, xlab="Year", ylab="PM2.5 Emission", xlim=c(1999, 2008), yaxt="n", main="PM2.5 Emission in Baltimore City")
  axis(2, at=yrng, labels=ylabels)
  lines(data, lwd = 2, col="red")
}