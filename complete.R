##      Second part: complete function
complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        ##      final directory for the specdata files
        dir <- paste(getwd(),directory,sep="/")
        
        ##      vector containing the id's to load according
        ##      to the file name convention
        names <- paste("000",as.character(id), sep="")
        for(i in seq_along(names)){
                names[i] <- substr(names[i],nchar(names[i])-2,nchar(names[i]))
        }
        fileNames <-paste(paste(dir,names,sep="/"),".csv",sep="")
        
        ##      creates a data frame to hold the id/nobs
        completeFiles <- data.frame(id=numeric(),nobs=numeric())
        ##      initializes the data frame by knowing the number of rows
        names(completeFiles) = c("id","nobs")
        completeFiles <- data.frame(id=id, nobs=0)
        ##      gets the .csv files in the reference vector
        for(i in seq_along(fileNames)){
                #       loads the filename in a free variable
                File <- read.csv(fileNames[i], header = TRUE)
                #       subsets the free variable having only complete cases
                File <- File[complete.cases(File),]
                #       at the row whose id equals the source id, we set the value of nobs
                #       to the length of the complete cases vector
                completeFiles[completeFiles$id == id[i],]$nobs = nrow(File)
        }
        ##      Returns the data frame containing the id / nobs
        completeFiles
}