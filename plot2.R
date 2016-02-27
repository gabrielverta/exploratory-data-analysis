# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.

library(dplyr)

pm25Data <- function(){
  readRDS("summarySCC_PM25.rds")
}

parseNumber <- function(num){
  paste(sprintf("%.1f", num / 1000000), "M", sep = "")
}

plot2 <- function(data){
  baltimore <- filter(data, fips == "24510")
  group <- group_by(baltimore, year)
  data <- summarise(group, total=sum(Emissions))
  yrng <- range(data$total)
  ylabels <- c(parseNumber(yrng[1]), parseNumber(yrng[2]))
  plot(data, pch=19, xlab="Year", ylab="PM2.5 Emission", xlim=c(1999, 2008), yaxt="n", main="PM2.5 Emission in Baltimore City")
  axis(2, at=yrng, labels=ylabels)
  lines(data, lwd = 2, col="red")
}

png("plots/plot2.png")
d <- pm25Data()
plot2(d)
dev.off()