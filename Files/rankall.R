rankall <- function(outcome, num = "best") { 
  
  ## Read outcome data
  ## Loading data and changing relavant column types from string to numeric
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome_data[, c(11, 17, 23)] <- suppressWarnings(as.data.frame(lapply(outcome_data[, c(11, 17, 23)], as.numeric)))
  data <- outcome_data[, c(2,7,11,17,23)]
  names(data) <- c("hospital name", "states", "heart attack", "heart failure", "pneumonia")
  
  data <- data[!is.na(data[[outcome]]),]
  
  
  ## Check that outcome are valid
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
    stop("invalid outcome")
  }
  
  
  data_list <- list()

  ## For each state, find the hospital of the given rank
  for (state in unique(data$states)){
    state_data <- data[data$states == state,]
    order_vector <- order(state_data[[outcome]], state_data[["hospital name"]], decreasing = FALSE) 
    ordered_state_data <- state_data[order_vector, ]
    data_list[[state]] <- ordered_state_data
  }
  
  result_frame <- data.frame()
  
  if (num == "best"){
    for (i in seq_along(data_list)){
      temp_data <- data.frame(hospital = data_list[[i]][1, 1], state = names(data_list[i]))
      result_frame <- rbind(result_frame, temp_data)
    }
    
    
  }else if (num == "worst"){
    for (i in seq_along(data_list)){
      temp_data <- data.frame(hospital = data_list[[i]][length(data_list[[i]][,1]), 1], state = names(data_list[i]))
      result_frame <- rbind(result_frame, temp_data)
    }
    
    
  }else{
    for (i in seq_along(data_list)){
      temp_data <- data.frame(hospital = data_list[[i]][num, 1], state = names(data_list[i]))
      result_frame <- rbind(result_frame, temp_data)
    }
    
  }
    
  result_frame
}
  