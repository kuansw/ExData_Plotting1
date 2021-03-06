plot1 <- function() {
# This function generates the frequency histogram for Global Active Power 
# variable to plot1.png using the base graphics package.
#
# Source data for this function is taken from the "Individual household electric
# power consumption" dataset from UC Irvine Machine Learning Repository, which
# is assumed to be downloaded into the file 'household_power_consumption.txt',
# and made available in the current working directory.
#    
# Arguments : None
# Inputs    : 'household_power_consumption.txt' in current working directory.
# Outputs   : 'plot1.png' in current working directory
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

    cat("Generating the frequency histogram plot to 'plot1.png' file.\n")

    # Set the graphics device to png and destination output to plot2.png
    png(file="plot1.png",width=480,height=480)
    
    # Generate the histogram plot using hist()     
    hist(data$Global_active_power, col="red",
         main="Global Active Power", 
        xlab="Global Active Power (kilowatts)")

    # Shuts down the png device
    dev.off()

    cat("'plot1.png' file generated.\n")

}