complete <- function(directory, id = 1:332) 
{
		## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
		
		count<-1;
		nob <- rep(NA,length(id));
		
		for (elem in id)
		{
			data<-getmonitor(elem, directory, FALSE);
			number <- nrow(na.omit(data));
			nob[count] <- number;
			count<-count+1;
		}
		
		id<-as.integer(id);
		return(data.frame(id=id, nobs=nob));
}