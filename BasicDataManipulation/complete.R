complete <- function(directory, id = 1:332) 
{
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