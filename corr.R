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