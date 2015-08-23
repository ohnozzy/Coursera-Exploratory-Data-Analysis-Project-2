plot6<-function(summarySCC="summarySCC_PM25.rds", SourceClassificationCode="Source_Classification_Code.rds"){
  NEI <- readRDS(summarySCC)
  SCC <- readRDS(SourceClassificationCode)
  NEI<-NEI[NEI$fips == "24510" | NEI$fips == "06037",]
  subSCC<-SCC[sapply(SCC$EI.Sector, grepl, pattern="Vehicles"),]
  subSCC$SCC<-as.character(subSCC$SCC)
  city_name<-rbind(c(fips="24510",city="Baltimore City"),c(fips="06037",city="Los Angeles County"))
  m_data<-merge(NEI, subSCC[,c("SCC","EI.Sector")])
  m_data<-merge(m_data,city_name)
  sum_by_year<-tapply(m_data$Emissions, m_data[,c("year","city")], sum)
  #Because the emission of Los Angeles is 10x more than Baltimore
  #The emission numbers for both city are normalized, using the emission of each city on 1999 as the reference point. 
  sum_by_year[,1]<-sum_by_year[,1]/sum_by_year[1,1]
  sum_by_year[,2]<-sum_by_year[,2]/sum_by_year[1,2]
  ggdata<-rbind(cbind(sum_by_year[,1], rownames(sum_by_year),colnames(sum_by_year)[1]), 
                cbind(sum_by_year[,2], rownames(sum_by_year),colnames(sum_by_year)[2]))
  rownames(ggdata)<-NULL
  ggdata<-as.data.frame(ggdata)
  
  ggdata[,1]<-as.numeric(as.character(ggdata[,1]))
  ggdata[,2]<-as.numeric(as.character(ggdata[,2]))
  colnames(ggdata)<-c("Emissions","year","city")
  
  qplot(year,	Emissions,	data	=	ggdata,	facets	=	.	~	city, geom	=	c("point",	"smooth"),	method	=	"lm")
  ggsave("plot6.png")
}