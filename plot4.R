plot4 <- function() {
# This function generates the 4-panel plot line graphs to plot4.png
# using the base graphics package.
#    
# Source data for this function is taken from the "Individual household electric
# power consumption" dataset from UC Irvine Machine Learning Repository, which
# is assumed to be downloaded into the file 'household_power_consumption.txt',
# and made available in the current working directory.
#    
# Arguments : None
# Inputs    : 'household_power_consumption.txt' in current working directory.
# Outputs   : 'plot4.png' in current working directory
#    
    # Check that the data file household power consumption.txt exists in the 
    # current working directory
    datafile <- "household_power_consumption.txt"
    if (!file.exists(datafile)) {
        stop("datafile 'household_power_consumption.txt' does not 
             exist in current working directory")
    }

    # Load the full household power consumption dataset
    # As this is a big dataset, data loading will take some time to complete.
    cat("Loading 'houshold_power_consumption.txt' file.  Please wait...\n")
    rawdata <- read.csv(datafile,sep=";",
                 na.string="?",
                 colClasses=c("character","character","numeric",
                              "numeric","numeric","numeric","numeric",
                              "numeric", "numeric"))

    # Extract data only for 1/2/2007 and 2/2/2007 from the full dataset
    cat("'houshold_power_consumption.txt' data file loaded.\n")
    cat("Extracting the data between 1/2/2007 and 2/2/2007.\n")
    data <- rawdata[rawdata$Date == "1/2/2007" | rawdata$Date == "2/2/2007",]
    
    # Remove rawdata to free up resources
    rm(rawdata)

    # Combine and convert Date and Time variables to a new DateTime variable
    data$DateTime <- strptime(paste(data$Date,data$Time), 
                          format="%d/%m/%Y %H:%M:%S")


    cat("Generating the multi-panel plot to 'plot4.png' file.\n")
    
    # Set the graphics device to png and destination output to plot4.png
    png(file="plot4.png",width=480,height=480)

    # Set the mfcol parameter to create 2x2 four-panel plot.
    par(mfcol=c(2,2))

    # Generate the first single variable line graph panel with plot()    
    plot(data$DateTime,data$Global_active_power,xlab="",type='l',
         ylab="Global Active Power")

    # Generate the second multivariate line graph panel with plot(), lines()
    # and legend()
    plot(data$DateTime,data$Sub_metering_1,xlab="",type='l',
         ylab="Energy sub metering")
    lines(data$DateTime,data$Sub_metering_2,col="red",type='l')
    lines(data$DateTime,data$Sub_metering_3,col="blue",type='l')
    legend("topright",
           legend=c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"),
           col=c("black","red","blue"),lty = 1, bty="n")

    # Generate the third single variable line graph panel with plot()    
    plot(data$DateTime,data$Voltage,xlab="datetime",type='l',
         ylab="Voltage")
    
    # Generate the fourth single variable line graph panel with plot()    
    plot(data$DateTime,data$Global_reactive_power,xlab="datetime",type='l',
         ylab="Global_reactive_power")

    # Shuts down the png device
    dev.off()

    cat("'plot4.png' file generated.\n")
}