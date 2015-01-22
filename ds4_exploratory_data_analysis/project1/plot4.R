# Student: S.Metzler
# Course: Exploratory Data Analysis
# Course Project: 1
# Task: Plot 4
# Objective: Single with Page 4 Graphics

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

# orient display space into 2 rows, 2 columns
par(mfrow=c(2,2))

# plot 1
the_plot <- hist(february_power_data$Global_active_power, 
	main="Global Active Power",
	xlab="Global Active Power (kilowatts)",
	col="red")

# plot 2
plot(february_power_data$date_time_factor,
	february_power_data$Global_active_power,
	type="l",
	ylab="Global Active Power (kilowatts",
	xlab="")

# plot 3
plot(february_power_data$date_time_factor,
	february_power_data$Sub_metering_1,
	type="l",
	ylab="Energy sSub Mmetering",
	xlab="")

lines(february_power_data$date_time_factor,february_power_data$Sub_metering_2,col="red")
lines(february_power_data$date_time_factor,february_power_data$Sub_metering_3,col="blue")
legend("topright",col=c("black","red","blue"),c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c(1,1),lwd=c(1,1))


# plot 4
plot(february_power_data$date_time_factor,
	february_power_data$Global_reactive_power,
	type="l",
	xlab="date_time_factor",
	ylab="Global_reactive_power")


dev.copy(png,file="plot4.png",width=480,height=480)
dev.off()