##########################
##        Programming assignment 1

##      Setting the working folder
setwd("C:/Users/xgarcia/Documents/GitHub/WorkPlaceRepositori")
getwd()

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
#pm <- pollutantmean("specdata", "nitrate")

pm <- pollutantmean("specdata", "sulfate", 1:10)
pm <- pollutantmean("specdata", "nitrate", 70:72)
pm <- pollutantmean("specdata", "nitrate", 23)
pm <- pollutantmean("specdata", "nitrate", 888)


##      Second part: complete function
complete <- function(directory, id = 1:332) {
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
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)
complete("specdata", 993)


###     Third part: correlation function
corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        dir <- paste(getwd(),directory,sep="/")
        # initializes a loop control variable and a vector of valid names
        id <- 1
        names <- data.frame(id=numeric(),name=character())
        # repeats while there are stil files to explore in the folder
        repeat{
                # prepares the name of the file according to the name convention
                name <- paste("000",as.character(id), sep="")
                name <- substr(name,nchar(name)-2,nchar(name))
                name <- paste(paste(dir,name,sep="/"),".csv",sep="")
                
                # verifies the existence of the file, if not, scapes the loop
                if(!file.exists(name)) break
                names <- rbind(names,data.frame(id=id,name=name))
                
                # security break
                if(id >= 9999) break  
                id <- id + 1
        }
        # Gets the list of files and complete cases from the directory
        # using the previous function and passing the vector of valid files
        potentialFiles <- complete(directory, names$id)
        
        # Subsets the structure to only those files over the threshold
        # of complete cases
        realFiles <- potentialFiles[potentialFiles$nobs > threshold,]
        # Prepares a vector of correlations
        corSN <- vector("numeric")
        # Loops over the realFiles to calculate the correlations
         for(i in realFiles$id){
                # loads the filename + completes cases
                fileName <- as.vector(names$name[names$id == i])
                fileRed <- read.csv(fileName, header = TRUE)
                fileRed <- fileRed[complete.cases(fileRed),]
                # performs the correlation between sulfate and nitrate and adds it to the 
                # vector to return
                corSN <- append(corSN,cor(fileRed$sulfate, fileRed$nitrate))
        }
        ## Return a numeric vector of correlations
        corSN
}
cr <- corr("specdata", 150)
head(cr)
summary(cr)
cr <- corr("specdata", 400)
head(cr)
summary(cr)
cr <- corr("specdata", 5000)
summary(cr)
length(cr)
cr <- corr("specdata")
summary(cr)


