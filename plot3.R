# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

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
  qplot(year, total, data=data, color=type, geom=c("point", "smooth"), main="PM2.5 Emissions in Baltimore City")
}

png("plots/plot3.png")
d <- pm25Data()
print(plot3(d))
dev.off()