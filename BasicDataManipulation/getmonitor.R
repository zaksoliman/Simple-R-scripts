getmonitor <- function(id, directory, summarize = FALSE) {
		## 'id' is a vector of length 1 indicating the monitor ID
        ## number. The user can specify 'id' as either an integer, a
        ## character, or a numeric.
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files, can either be a relative path or an absolute path.

        ## 'summarize' is a logical indicating whether a summary of
        ## the data should be printed to the console; the default is
        ## FALSE
		
	if(id<10)
 	{
		id<-paste("00",id,sep="");
	}
	else if (id<100)
	{
		id<-paste("0",id,sep="");
	}
	directory <- paste(directory, id, sep = "\\");
	directory <- paste(directory, ".csv", sep="");
	data <- read.csv(directory);

	if (summarize)
	{
		print(summary(data));
	}

	return(data);
}