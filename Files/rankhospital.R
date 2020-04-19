rankhospital <- function(state, outcome, num = "best") { 
  
  
  ## Loading data and changing relavant column types from string to numeric
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome_data[, c(11, 17, 23)] <- suppressWarnings(as.data.frame(lapply(outcome_data[, c(11, 17, 23)], as.numeric)))
  
  
  
  ## Throwing an error if an invalid state argument is supplied
  if (!(state %in% outcome_data$State)){
    stop("invalid state")
  }
  
  
  ## Filtering to only include data for the user supplid state
  state_data <- outcome_data[outcome_data$State == state, ]
  
  
  
  if (outcome == "heart attack"){
    ordered_data <- state_data[order(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, 
                                     state_data$Hospital.Name, decreasing = FALSE),]
    
    ordered_data <- ordered_data[!is.na(ordered_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),
                                 ]$Hospital.Name
    
    
    
  }else if (outcome == "heart failure"){
    ordered_data <- state_data[order(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, 
                                     state_data$Hospital.Name, decreasing = FALSE),]
    
    ordered_data <- ordered_data[!is.na(ordered_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
                                 ]$Hospital.Name
    
    
    
    
  }else if (outcome == "pneumonia") {
    ordered_data <- state_data[order(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, 
                                     state_data$Hospital.Name, decreasing = FALSE),]
    
    ordered_data <- ordered_data[!is.na(ordered_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),
                                 ]$Hospital.Name
    

  } else {
    stop("invalid outcome")
  }
  
  
  
  
  if (num == "best"| num == 1){
    return(ordered_data[1])
  }else if (num == "worst"){
    return(ordered_data[length(ordered_data)])
  }else if (num > length(ordered_data)){
    return(NA)
  }else{
    return(ordered_data[num])
  }
  
}