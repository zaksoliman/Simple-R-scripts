best <- function(state, outcome){	
	
	## Read outcome data
	dat<-read.csv("outcome-of-care-measures.csv", colClasses="character");
	states <- dat$State;
	## Check that state and outcome are valid and return hospital name in that state with lowest 30-day death rate
	if(!(state %in% states))
	{
		stop("invalid state");
	}
	
	#Produce a logical vector that will help me only keep only the data points that are
	#related to the chosen state
	inState <- states %in% state;
	#Get the names of the hospitals
	hospitalNames <- dat$Hospital.Name
		
	if(outcome == "heart attack") 
	{
		#Read in the 30-Day Death (Mortality) Rates from Heart Attack
		heartAttack <- as.numeric(dat[,11]);
		#Get the death rates of the corespoding state chosen by the caller of the function
		heartAttack <- heartAttack[inState];
		#Get the best (lowest) rate and it's position in the vector
		minValue <- min(heartAttack, na.rm = TRUE);
		minValPos <- which(heartAttack %in% minValue);
		
		#To break a tie I sort them aphabeticaly and return the first hospital name in the lsit
		if (length(minValPos) > 1)
		{
			hospitalNames <- hospitalNames[inState];
			for (i in minValPos)
			{
				bestHospitals <- hospitalNames[i];
			}
			
			sort(bestHospitals);
			return(bestHospitals[1]);
		}
		
		hospitalNames <- hospitalNames[inState];
		return(hospitalNames[minValPos]);
	} 
	else if(outcome == "heart failure")
	{
		#Read in the 30-Day Death (Mortality) Rates from Heart Failure
		heartFailure <- as.numeric(dat[,17]);
		#Get the death rates of the corespoding state chosen by the caller of the function
		heartFailure <- heartFailure[inState];
		#Get the best (lowest) rate and it's position in the vector
		minValue <- min(heartFailure, na.rm = TRUE);
		minValPos <- which(heartFailure %in% minValue);
		
		#To break a tie I sort them aphabeticaly and return the first hospital name in the lsit
		if (length(minValPos) > 1)
		{
			hospitalNames <- hospitalNames[inState];
			for (i in minValPos)
			{
				bestHospitals <- hospitalNames[i];
			}
			
			sort(bestHospitals);
			return(bestHospitals[1]);
		}
		
		hospitalNames <- hospitalNames[inState];
		return(hospitalNames[minValPos]);
	}	
	else if(outcome == "pneumonia")
	{
		#Read in the 30-Day Death (Mortality) Rates from Heart Pneumonia
		pneumonia <- as.numeric(dat[,23]);
		#Get the death rates of the corespoding state chosen by the caller of the function
		pneumonia<-pneumonia[inState];
		#Get the best (lowest) rate and it's position in the vector
		minValue <- min(pneumonia, na.rm = TRUE);
		minValPos <- which(pneumonia %in% minValue);
		
		#To break a tie I sort them aphabeticaly and return the first hospital name in the lsit
		if (length(minValPos) > 1)
		{
			hospitalNames <- hospitalNames[inState];
			for (i in minValPos)
			{
				bestHospitals <- hospitalNames[i];
			}
			
			sort(bestHospitals);
			return(bestHospitals[1]);
		}
		
		hospitalNames <- hospitalNames[inState];
		return(hospitalNames[minValPos]);
	}
	else
	{
		stop("invalid outcome");
	}

}