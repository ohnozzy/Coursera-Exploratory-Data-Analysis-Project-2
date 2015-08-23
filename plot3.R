plot3<-function(file="summarySCC_PM25.rds"){
  NEI <- readRDS(file)
  NEI<-NEI[NEI$fips == "24510",]
  sum_by_year<-tapply(NEI$Emissions, NEI[,c("year","type")], sum)
  ggdata<-rbind(cbind(sum_by_year[,1], rownames(sum_by_year),colnames(sum_by_year)[1]), 
        cbind(sum_by_year[,2], rownames(sum_by_year),colnames(sum_by_year)[2]),
        cbind(sum_by_year[,3], rownames(sum_by_year),colnames(sum_by_year)[3]),
        cbind(sum_by_year[,4], rownames(sum_by_year),colnames(sum_by_year)[4]))
  rownames(ggdata)<-NULL
  ggdata<-as.data.frame(ggdata)
  
  ggdata[,1]<-as.numeric(as.character(ggdata[,1]))
  ggdata[,2]<-as.numeric(as.character(ggdata[,2]))
  colnames(ggdata)<-c("Emissions","year","type")

  qplot(year,	Emissions,	data	=	ggdata,	facets	=	.	~	type, geom	=	c("point",	"smooth"),	method	=	"lm")
  ggsave("plot3.png")

}