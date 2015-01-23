NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# QUESTION 3: Across the United States, how have emissions from coal 
# combustion-related sources changed from 1999–2008? 

# STEP 1: extract "coal combustion-related sources", multiple grep()
# subset dataframe based on " coal" in Short.Name field
subcoal_df <- SCC[grepl(" coal",SCC$Short.Name,ignore.case=TRUE)==TRUE,]
# subset the coal subset dataframe based on "combustion" in Short.Name field
subcoalcomb_df <- subcoal_df[grepl("combustion",subcoal_df$Short.Name,ignore.case=TRUE)==TRUE,]

# STEP 2: collect list of SCC identifiers from the "coal combustion-related" records
SCC_ids <- as.character(subcoalcomb_df$SCC)

# STEP 3: identify coal comb emissions in NEI based on SCC queries
NEI$SCC <- as.character(NEI$SCC)
NEI_coalcomb <- NEI[NEI$SCC %in% SCC_ids,]

# STEP 4: aggregate variables for plotting, THEN Plot
agg_coalcomb <- with(NEI_coalcomb, aggregate(Emissions, by=list(year),sum))
colnames(agg_coalcomb) <- c("Year","Emissions")
the_label_vec <- c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)
plot(agg_coalcomb, type="o", xlab="Year", ylab=expression("Total Emissions, PM"[2.5]),main="US Coal Combustion Emissions", xlim=c(1999,2008))
axis(side=1,at=the_label_vec,labels=the_label_vec)

# STEP 5: output results to graphics device
dev.copy(png,file="plot_question_4.png", width=480, height=480)
dev.off()
