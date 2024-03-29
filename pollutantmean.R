##################################################################
##        Programming assignment 1
##      First part: pollutantmean function
pollutantmean <- function(directory, pollutant, id = 1:332) {
        ##      final directory for the specdata files
        dir <- paste(getwd(),directory,sep="/")
        
        ##      vector containing the id's to load according
        ##      to the file name convention
        names <- paste("000",as.character(id), sep="")
        for(i in seq_along(names)){
                names[i] <- substr(names[i],nchar(names[i])-2,nchar(names[i]))
        }
        fileNames <-paste(paste(dir,names,sep="/"),".csv",sep="")

        ##      creates a vector to hold the means
        pollution <- vector("numeric")

        ##      gets the .csv data into a temp structure and evaluates the means vector
        for(i in seq_along(fileNames)){
                monitor <- read.csv(fileNames[i], header = TRUE)
                #       appends all the counts for every monitor
                pollution <- append(pollution,monitor[,pollutant])
        }
        ##      Gets the final mean
        pollMean <- mean(pollution,na.rm = TRUE)
}
