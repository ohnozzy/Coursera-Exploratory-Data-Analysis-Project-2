plot4<-function(summarySCC="summarySCC_PM25.rds", SourceClassificationCode="Source_Classification_Code.rds"){
  NEI <- readRDS(summarySCC)
  SCC <- readRDS(SourceClassificationCode)
  subSCC<-SCC[sapply(SCC$EI.Sector, grepl, pattern="Coal"),]
  subSCC$SCC<-as.character(subSCC$SCC)
  m_data<-merge(NEI, subSCC[,c("SCC","EI.Sector")])
  sum_by_year<-tapply(m_data$Emissions, m_data$year, sum)
  year<-as.numeric(names(sum_by_year))
  data<-data.frame(cbind(sum_by_year, year))
  png("plot4.png")
  with(data, plot(year, sum_by_year,  ylab="Total PM2.5 Emission",pch = 20, main="Coal Combustion-Related Emission"))
  model <- lm(sum_by_year ~ year, data)
  abline(model, lwd = 2)
  dev.off()
}