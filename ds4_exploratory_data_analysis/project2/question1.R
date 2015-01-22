NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# QUESTION 1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?

# REQUIREMENTS: Using the base plotting system, make a plot showing the total PM2.5 emission from
# all sources for each of the years 1999, 2002, 2005, and 2008

# STEP 1: Aggregate the records by year in order to plot Emissions change over time (1999-2008)
the_aggregated_data <- with(NEI, aggregate(Emissions,by=list(year),sum))

# STEP 2: Plot the data
plot(the_aggregated_data, type="o", xlab="Year",ylab=expression("Total in PPM"[2.5]),main="US Emissions per Year")

# STEP 3: Edit the plot
the_label_vec <- c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)
axis(side=1,at=the_label_vec,labels=the_label_vec)

# STEP 4: output results to graphics device
dev.copy(png,file="question1.png",width=480,height=480)
dev.off()
