NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# QUESTION 6: Compare emissions from motor vehicle sources
# in Baltimore City (fips=="24510") with emissions from motor vehicle sources
# in Los Angeles County, California (fips == "06037"). Which 
# city has seen greater changes over time in motor vehicle emissions? 

# identify motor vehicle emissions
sub_motor_df <- SCC[grepl("motor",SCC$Short.Name,ignore.case=TRUE)==TRUE,]

SCC_ids <- as.character(sub_motor_df$SCC)
NEI$SCC <- as.character(NEI$SCC)
NEI_motor <- NEI[NEI$SCC %in% SCC_ids,]

NEI_motor_balt <- NEI_motor[which(NEI_motor$fips=="24510"),]
NEI_motor_los <- NEI_motor[which(NEI_motor$fips=="06037"),]

agg_NEI_motor_balt <- with(NEI_motor_balt,aggregate(Emissions, by=list(year),sum))
agg_NEI_motor_los <- with(NEI_motor_los,aggregate(Emissions, by=list(year),sum))

colnames(agg_NEI_motor_balt) <- c("Year","Emissions")
colnames(agg_NEI_motor_los) <- c("Year","Emissions")

agg_NEI_motor_balt$Location <- "Baltimore City"
agg_NEI_motor_los$Location <- "Los Angeles County"

NEI_motor_balt_los <- rbind(agg_NEI_motor_balt, agg_NEI_motor_los)

qplot(Year,Emissions, data=NEI_motor_balt_los, group=Location, color=Location, geom=c("point","line"), xlab="Year",ylab=expression("Total Emissions, PM"[2.5]),main="Comparing Emissions, Baltimore City & Los Angeles County")

dev.copy(png,file="plot_question6.png",width=480,height=480)
dev.off()
