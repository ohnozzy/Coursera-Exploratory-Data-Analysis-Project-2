plot1<-function(file="summarySCC_PM25.rds"){
  NEI <- readRDS(file)
  sum_by_year<-tapply(NEI$Emissions, NEI$year, sum)
  year<-as.numeric(names(sum_by_year))
  data<-data.frame(cbind(sum_by_year, year))
  png("plot1.png")
  with(data, plot(year, sum_by_year,  ylab="Total PM2.5 Emission",pch = 20))
  model <- lm(sum_by_year ~ year, data)
  abline(model, lwd = 2)
  dev.off()
}