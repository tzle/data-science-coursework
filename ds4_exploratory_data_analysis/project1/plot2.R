# Student: S.Metzler
# Course: Exploratory Data Analysis
# Course Project: 1
# Task: Plot 2
# Objective: Line Graph by Day of Week for Household Power Data

setwd("F:/_rdev/data")
# load the source file
power_table <- read.table("household_power_consumption.txt",sep=";",header=TRUE)

# format the date field
power_table$Date <- as.Date(power_table$Date, format="%d/%m/%Y")

# extract subset using date criteria, collect all columns
february_power_data <- power_table[(power_table$Date=="2007-02-01") | (power_table$Date=="2007-02-02"),]

# format fields for graphing
february_power_data$Global_active_power <- as.numeric(as.character(february_power_data$Global_active_power))
february_power_data$Global_reactive_power <- as.numeric(as.character(february_power_data$Global_reactive_power))
february_power_data$Voltage <- as.numeric(as.character(february_power_data$Voltage))
february_power_data$Sub_metering_1 <- as.numeric(as.character(february_power_data$Sub_metering_1))
february_power_data$Sub_metering_2 <- as.numeric(as.character(february_power_data$Sub_metering_2))
february_power_data$Sub_metering_3 <- as.numeric(as.character(february_power_data$Sub_metering_3))

# create field combining date & time for graphing requirements
february_power_data <- transform(february_power_data, date_time_factor=as.POSIXct(paste(Date,Time)),"%d/%m/%Y %H:%M:%S")

plot(february_power_data$date_time_factor,
	february_power_data$Global_active_power,
	type="l",
	ylab="Global Active Power (kilowatts",
	xlab="")

dev.copy(png,file="plot2.png",width=480,height=480)
dev.off()