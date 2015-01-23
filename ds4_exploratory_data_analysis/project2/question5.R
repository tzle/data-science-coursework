NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# QUESTION 5: How have emissions from motor vehicle sources
# changed from 1999–2008 in Baltimore City?

# STEP 1: extract SCC records with emissions from motor
sub_motor_df <- SCC[grepl("motor",SCC$Short.Name,ignore.case=TRUE)==TRUE,]
# STEP 2: build list of ids to match NEI & SCC data
SCC_ids <- as.character(sub_motor_df$SCC)
NEI$SCC <- as.character(NEI$SCC)

NEI_motor <- NEI[NEI$SCC %in% SCC_ids,]
NEI_motor_fips <- NEI_motor[which(NEI_motor$fips=="24510"),]


# STEP 3: 
agg_NEI_motor_fips <- with(NEI_motor_fips, aggregate(Emissions,by=list(year),sum))
plot(agg_NEI_motor_fips, type="o",xlab="Year",ylab=expression("Total Emissions,PM"[2.5]),main="Baltimore Motor Emissions",xlim=c(1999,2008))

# STEP 4: Edit the plot for readability
the_label_vec <- c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)
axis(side=1,at=the_label_vec,labels=the_label_vec)

# STEP 5: output results to graphics device
dev.copy(png,file="plot_question_5.png", width=480, height=480)
dev.off()
