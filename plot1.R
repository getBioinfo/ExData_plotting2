## plot1.R that does the following. 
##   1. Get data files from web site if they are not in local file system
##   2. Read PM2.5 emission data from files.
##   3. Summarize the total PM2.5 emission data of US by year
##   4. Plot data to show the emission trend from  

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
##   3. summarize total PM2.5 emission data by year 
##   4. plot 

# get data first
directory <- getData()

# prepare data set list
dataList <- prepareData(dirname = directory)

# get PM2.5 emission data
NEI <- dataList$NEI
# summarize total PM2.5 emission in US by year
aggrData <- aggregate(Emissions ~ year, FUN=sum, data=NEI)

## Plot PM2.5 emission trend
png("plot1.png", bg="lightgrey") # PNG graph device - default 480x480 pxs
# plot total PM2.5 emission in US
plot(aggrData$year, aggrData$Emissions, type = "b", xaxp=c(1999, 2008,3), 
     main="Annual Total PM2.5 Emission in US", xlab="Year", ylab="PM2.5 Emission (tons)",
     lty=3, pch=20)

# close graphing device
dev.off()
