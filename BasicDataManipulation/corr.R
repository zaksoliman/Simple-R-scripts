corr <- function(directory, threshold = 0) 
{
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
		
		compl <- complete(directory, id = 1:332);		
		monitor <- 1;
		goodMonitors <- numeric();
		
		while(monitor<=332)
		{
			if (compl[[monitor,2]] > threshold)
			{
				goodMonitors<-c(goodMonitors,monitor);
			}
			
			monitor<-monitor+1;
		}
		
		result <-numeric()
		
		if (length(goodMonitors > 0))
		{
			for (id in goodMonitors)
			{
				dat <- getmonitor(id, directory);
				dat<-na.omit(dat);
			
				#We want to calculate the correlation between sulfate and nitrate
				#dat has a column named sulfate and the othe named nitrate
			
				result <- c(result,cor(dat$sulfate, dat$nitrate));
			}
		}
		
		return(result);		
}