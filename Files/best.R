best <- function(state, outcome) {
  # Reading the data file
  outcome_dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Throwing an error if an invalid state argument is supplied
  if (!(state %in% outcome_dat$State)){
    stop("invalid state")
  }
  # Filtering to only include data for the user supplid state
  state_data <- outcome_dat[outcome_dat$State == state, ]
  
  
  # Based on outcomes, the lowest rate of mortality rate is calculated. The corresponding hospital
  # is returned for the calculated rate. If multiple hospitals share the lowest rate, the hospitals 
  # are filtered based on names with A being at the top and Z at the bottom. 
  if (outcome == "heart attack"){
    index <- which(as.numeric(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) == 
                     min(as.numeric(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), na.rm = TRUE))
    hos_names <- state_data$Hospital.Name[index]
    return(sort(hos_names)[1])
  
  
  }else if (outcome == "heart failure"){
    index <- which(as.numeric(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) == 
                     min(as.numeric(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), na.rm = TRUE))
    hos_names <- state_data$Hospital.Name[index]
    return(sort(hos_names)[1])
    
    
  }else if (outcome == "pneumonia") {
    index <- which(as.numeric(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) == 
                     min(as.numeric(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), na.rm = TRUE))
    hos_names <- state_data$Hospital.Name[index]
    return(sort(hos_names)[1])
    
    
  } else {
    stop("invalid outcome")
  }
}