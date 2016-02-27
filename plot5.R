# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

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

png("plots/plot5.png")
d <- pm25Data()
cl <- classifications()
print(plot5(d, cl))
dev.off()