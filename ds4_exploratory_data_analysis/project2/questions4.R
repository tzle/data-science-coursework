NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# QUESTION 3: Across the United States, how have emissions from coal 
# combustion-related sources changed from 1999–2008? 


library(plyr)
# STEP 1: extract records by FIPS# & aggregate emissions 
type_of_pollutant <- with(NEI[which(NEI$fips=="24510"),], aggregate(Emissions, by=list(year,type),sum))

# STEP 2: name the columns in order to plot
colnames(type_of_pollutant) <- c("Year","Type","Emissions")


# STEP 4: plot the data
qplot(Year, EmissionsPPM, data=type_of_pollutant, group=Type, color=Type, geom=c("point","line"),xlab="Year", ylab=expression("Total Emissions PPM"[2.5]),main="Baltimore City Emissions by Type & Year")

# STEP 5: output results to graphics device
dev.copy(png,file="plot_question_3.png", width=480, height=480)
dev.off()
