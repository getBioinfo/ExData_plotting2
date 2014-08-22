## plot5.R that does the following. 
##   1. Get data files from web site if they are not in local file system
##   2. Read PM2.5 emission data from files.
##   3. Read source classification code (SCC) data
##   4. Join emission data with source classification code data by SCC ID
##   5. Extract motor vehicle-related PM2.5 emission data in Baltimore City, Maryland
##   6. Summarize motor vehicle-related PM2.5 emission data in Baltimore City by year
##   7. Plot data to show the emission trend from  

## download and unzip data
##   Function getData(url, dirname) takes url and directory name as input.
##     1. check whether directory exists, return its name if it exists
##     2. if directory doesn't exist, check whether the zipped data file exists
##     3. if zipped file exists, unzip it and return dirname
##     4. if zipped doesn't exist, download it from url and unzip the file and
##        return dirname

getData <- function(url = "https://d396qusza40orc.cloudfront.net/exdata/data/NEI_data.zip", 
                    dirname = "NEI_data") {
    # check whether directory exists
    if (file.exists(dirname)) {
        message("Data directory - '", dirname, "' exists")
        return(dirname)
    }
    
    # if directory doesn't exist, check whether the zip file exists
    zipFile = paste(dirname, '.zip', sep = "")
    if (file.exists(zipFile)) {
        message("Unzip file - '", zipFile, "'")
        unzip(zipFile)
        return(dirname)
    }
    
    # else download file and unzip it
    message("Download data file ...")
    download.file(url, destfile = zipFile, method = "curl", quiet = TRUE)
    message("Unzip file - '", zipFile, "'")
    unzip(zipFile)
    message("Data directory ready")
    return(dirname)
}

## prepare data
##   Function prepareData(dirname) transform data to tidy data set.
##     1. read summarySCC_PM25.rds for PM2.5 emission data
##     2. read Source_Classification_code.rds for source code
##     3. return summary and source data as list of data.frame

prepareData <- function(dirname = "NEI_data") {
    # read PM2.5 emission summary file
    pm25File <- paste(dirname, '/summarySCC_PM25.rds', sep = "")
    NEI <- readRDS(pm25File)
    NEI$fips <- as.factor(NEI$fips)
    NEI$SCC <- as.factor(NEI$SCC)
    NEI$Pollutant <- as.factor(NEI$Pollutant)
    NEI$type <- as.factor(NEI$type)
    
    # read source classification code file
    sccFile <- paste(dirname, '/Source_Classification_Code.rds', sep = "")
    SCC <- readRDS(sccFile)
    
    # return list of NEI, SCC
    list(NEI = NEI, SCC = SCC)
}

## main program part
##   1. call function getData() to get data in directory
##   2. call function prepareData() to prepare emission and source data sets
##   3. subset motor vehicle-related PM2.5 emission data in Baltimore City, Maryland
##   3. summarize motor vehicle-related PM2.5 emission data in Baltimore City, Maryland by year
##   4. plot 

# get data first
directory <- getData()

# prepare data set list
dataList <- prepareData(dirname = directory)

# get PM2.5 emission data
NEI <- dataList$NEI

# get source classification code data
SCC <- dataList$SCC

# Join emission data with source classification code data by SCC ID
allData <- merge(NEI, SCC, by = "SCC", sort = FALSE)

# subset motor vehicle-related PM2.5 emission data in Baltimore City, Maryland
subData1 <- subset(allData, fips == "24510")
subData2 <- subData1[grep("Mobile .+ Vehicles", subData1$EI.Sector, perl=TRUE),]

# summarize motor vehicle-related PM2.5 emission data in Baltimore City, Maryland
aggrData <- aggregate(Emissions ~ year, FUN=sum, data=subData2)

## Plot PM2.5 emission trend
library(ggplot2)
# plot coal combustion-related PM2.5 emission data of US
plotObj <- ggplot(aggrData, aes(year, Emissions)) + 
    geom_point() + geom_line(linetype=3) +
    labs(title = "Motor vehicle-related PM2.5 emission data\n in Baltimore City, Maryland",
         x = "Year", y = "PM2.5 Emission (tons)")

# http://stackoverflow.com/questions/7034647/save-ggplot-within-a-function
# IMPORTANT: rescale PNG image size
#            width pixes = width (in) x dpi
#            height pixes = height (in) x dpi
######## SO: changing dpi from default 300 to 100 shrinks the PNG size 
ggsave(filename="plot5.png", plot=plotObj, dpi=100)
