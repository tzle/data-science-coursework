NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# QUESTION 5: How have emissions from motor vehicle sources
# changed from 1999–2008 in Baltimore City?


# STEP 1: extract records by FIPS# & aggregate emissions
FIPS24510_annual_emissions <- with(NEI[which(NEI$fips == "24510"), ], aggregate(Emissions, by=list(year), sum))





# STEP 2: name the columns in order to plot
colnames(FIPS24510_annual_emissions) <- c("Year", "Emissions")


# STEP 3: plot the data
plot(FIPS24510_annual_emissions, type = "o", xlab = "Year", ylab = expression("Total in PM"[2.5]), main = "Baltimore City Emissions", xlim = c(1999,2008))


# STEP 3: Edit the plot for readability
the_label_vec <- c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

axis(side=1,at=the_label_vec,labels=the_label_vec)


# STEP 4: output results to graphics device
dev.copy(png,file="plot_question_2.png", width=480, height=480)
dev.off()
