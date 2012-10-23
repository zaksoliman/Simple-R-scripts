rankall<-function(outcome, num="best"){
	
	## Read outcome data
	dat<-read.csv("outcome-of-care-measures.csv", colClasses="character");
	
	#list of character strings of all the states that appear in the data frame
	states <- names(table(dat$State));
	## Check that the outcome is valid and return dataframe with best hospitals and their respective state.
	
	HospitalState<-character();
	HospitalName<-character();

	
	isTooBig <-FALSE;
	
	if(outcome == "heart attack") 
	{
		#Cast the 30-Day Death (Mortality) Rates from heart attacks to numeric
		dat[,11] <- as.numeric(dat[,11]);
		#Sort data frame for the Heart attack rates
		sorted<-dat[order(dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),];
		#removing na values
		sorted<-sorted[complete.cases(sorted[,12]),];
		
		for(i in 1:length(states))
		{
			isTooBig <-FALSE;
			state<-states[i];
			#Append the state			
			HospitalState<-c(HospitalState, state);
			
			#Get index
			if(num =="best")
			{
				index<-match(state,sorted$State);
			}
			else if (num == "worst")
			{
				
				#getting indices of all occurrences of the given state
				occurrences<-which(state == sorted$State)
				#Getting the index of the worst state
				index<-occurrences[length(occurrences)];
			}
			else
			{
				#getting indices of all occurrences of the given state
				occurrences<-which(state == sorted$State)
				#Getting the index of state with the given rate
				if(num>length(occurrences))
				{
					isTooBig<-TRUE;
				}
				else
				{
					index<-occurrences[num];
				}
			}
			#Append the name of the hospital of the current state with the wanted rank.
			
			if(isTooBig)
			{
				HospitalName<-c(HospitalName,NA);
			}
			else
			{
				HospitalName<-c(HospitalName,sorted$Hospital.Name[index]);
			}					
			
		}
		output<-data.frame(hospital=HospitalName, state=HospitalState);
		output<-output[order(output$hospital),];
		
		return(output);
	} 
	else if(outcome == "heart failure")
	{
		#Cast the 30-Day Death (Mortality) Rates from heart failure to numeric
		dat[,17] <- as.numeric(dat[,17]);
		#Sort data frame for the heart failure rates
		sorted<-dat[order(dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, dat$Hospital.Name),];
		#removing na values
		sorted<-sorted[complete.cases(sorted[,18]), ];
		
		for(i in 1:length(states))
		{
			isTooBig <-FALSE;
			state<-states[i];
			#Append the state
			HospitalState<-c(HospitalState, state);
			if(num =="best")
			{
				index<-match(state,sorted$State);
			}
			else if (num == "worst")
			{
				
				#getting indices of all occurrences of the given state
				occurrences<-which(state == sorted$State)
				#Getting the index of the worst state
				index<-occurrences[length(occurrences)];
			}
			else
			{
				#getting indices of all occurrences of the given state
				occurrences<-which(state == sorted$State)
				#Getting the index of state with the given rate
				if(num>length(occurrences))
				{
					isTooBig<-TRUE;
				}
				else
				{
					index<-occurrences[num];
				}
			}
			#Append the name of the hospital of the current state with the wanted rank.
			
			if(isTooBig)
			{
				HospitalName<-c(HospitalName,NA);
			}
			else
			{				
				HospitalName<-c(HospitalName,sorted$Hospital.Name[index]);
				
			}
				
		}		
		
		
		output<-data.frame(hospital=HospitalName, state=HospitalState);
		
		return(output);
	}	
	else if(outcome == "pneumonia")
	{
		#Cast the 30-Day Death (Mortality) Rates from pneumonia to numeric
		dat[,23] <- as.numeric(dat[,23]);
		#Sort data frame for the Heart attack rates
		sorted<-dat[order(dat$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),];
		#removing na values
		sorted<-sorted[complete.cases(sorted[,23]),];
		
		for(i in 1:length(states))
		{
			isTooBig <-FALSE;
			state<-states[i];
			#Append the state
			HospitalState<-c(HospitalState, state);
			if(num =="best")
			{
				index<-match(state,sorted$State);
			}
			else if (num == "worst")
			{
				
				#getting indices of all occurrences of the given state
				occurrences<-which(state == sorted$State)
				#Getting the index of the worst state
				index<-occurrences[length(occurrences)];
			}
			else
			{
				#getting indices of all occurrences of the given state
				occurrences<-which(state == sorted$State)
				#Getting the index of state with the given rate
				if(num>length(occurrences))
				{
					isTooBig<-TRUE;
				}
				else
				{
					index<-occurrences[num];
				}
			}
			#Append the name of the hospital of the current state with the wanted rank.
			
			
			
			if(isTooBig)
			{
				HospitalName<-c(HospitalName,NA);
			}
			else
			{
				HospitalName<-c(HospitalName,sorted$Hospital.Name[index]);
			}	
		}
		
		output<-data.frame(hospital=HospitalName, state=HospitalState);
		output<-output[order(output$hospital),];
		
		
		return(output);
	}
	else
	{
		stop("invalid outcome");
	}
}