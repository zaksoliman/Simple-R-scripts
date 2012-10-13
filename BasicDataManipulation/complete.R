complete <- function(directory, id = 1:332) 
{
		## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
		
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