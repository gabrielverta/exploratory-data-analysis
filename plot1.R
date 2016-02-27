# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

library(dplyr)

pm25Data <- function(){
  readRDS("summarySCC_PM25.rds")
}

parseNumber <- function(num){
  paste(sprintf("%.1f", num / 1000000), "M", sep = "")
}

plot1 <- function(data){
    group <- group_by(data, year)
    data <- summarise(group, total=sum(Emissions))
    yrng <- range(data$total)
    ylabels <- c(parseNumber(yrng[1]), parseNumber(yrng[2]))
    plot(data, pch=19, xlab="Year", ylab="PM2.5 Emission", xlim=c(1999, 2008), yaxt="n", main="PM2.5 Emissions in United States")
    axis(2, at=yrng, labels=ylabels)
    lines(data, lwd = 2, col="red")
}

png("plots/plot1.png")
d <- pm25Data()
plot1(d)
dev.off()