rankhospital<-function(state, outcome, ranking="best"){

	## Read outcome data
	dat<-read.csv("outcome-of-care-measures.csv", colClasses="character");
	states <- dat$State;
	## Check that state and outcome are valid and return hospital name in that state with the given ranking
	if(!(state %in% states))
	{
		stop("invalid state");
	}
	
	#If the chosen ranking is higher then the rankingber of states we return NA
	if (is.numeric(ranking) & ranking > length(states))
	{
		return(NA);
	}
		
	if(outcome == "heart attack") 
	{
		#Cast the 30-Day Death (Mortality) Rates from heart attacks to numeric
		dat[,11] <- as.numeric(dat[,11]);
		#Sort the data frame rows in ascending order of the mortality rates
		dat<-dat[order(dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ];
		#Remove all data points from the other states and keep only those form the one given as input
		dat<-dat[dat$State == state, ];
		#and remove rows contaning na values
		dat<-na.omit(dat);
		
		#Return the name of the hospital that corresponds to the specified ranking
		if(ranking == "best")
		{
			return(as.vector(dat$Hospital.Name[1]));
		}
		else if (ranking == "worst")
		{			
			return(as.vector(dat$Hospital.Name[nrow(dat)]));
		}
		else
		{
			return(as.vector(dat$Hospital.Name[ranking]));
		}
	} 
	else if(outcome == "heart failure")
	{
		#Cast the 30-Day Death (Mortality) Rates from heart Failures to numeric
		dat[,17] <- as.numeric(dat[,17]);
		#Sort the data frame rows in ascending order of the mortality rates
		dat<-dat[order(dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), ];
		#Remove all data points from the other states and keep only those form the one given as input
		dat<-dat[dat$State == state, ];
		#and remove rows contaning na values
		dat<-na.omit(dat);
		
		#Return the name of the hospital that corresponds to the specified ranking
		if(ranking == "best")
		{
			return(as.vector(dat$Hospital.Name[1]));
		}
		else if (ranking == "worst")
		{
			return(as.vector(dat$Hospital.Name[nrow(dat)]));
		}
		else
		{
			return(as.vector(dat$Hospital.Name[ranking]));
		}
	}	
	else if(outcome == "pneumonia")
	{
		#Cast the 30-Day Death (Mortality) Rates from pneumonia to numeric
		dat[,23] <- as.numeric(dat[,23]);
		#Sort the data frame rows in ascending order of the mortality rates
		dat<-dat[order(dat$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), ];
		#Remove all data points from the other states and keep only those form the one given as input
		dat<-dat[dat$State == state, ];
		#and remove rows contaning na values
		dat<-na.omit(dat);
		
		#Return the name of the hospital that corresponds to the specified ranking
		if(ranking == "best")
		{
			return(as.vector(dat$Hospital.Name[1]));
		}
		else if (ranking == "worst")
		{
			return(as.vector(dat$Hospital.Name[nrow(dat)]));
		}
		else
		{
			return(as.vector(dat$Hospital.Name[ranking]));
		}
	}
	else
	{
		stop("invalid outcome");
	}
}