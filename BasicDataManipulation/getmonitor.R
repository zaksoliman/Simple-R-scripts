getmonitor <- function(id, directory, summarize = FALSE) {

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