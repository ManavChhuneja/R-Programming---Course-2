complete <- function(directory, id = 1:332) {
  data <- data.frame()
  for(i in (id)){
    if(i <= 9){
      dat <- read.csv(file = paste(directory, "/00", i, ".csv", sep = ""), 
                      header = TRUE)
      
      
      num_obs <- sum(!is.na(dat[["sulfate"]]) & !is.na(dat[["nitrate"]]))
      data <- rbind(data, c(i, num_obs))
      
      
    }else if (i > 9 & i <= 99) {
      dat <- read.csv(file = paste(directory, "/0", i, ".csv", sep = ""), 
                      header = TRUE)
      
      num_obs <- sum(!is.na(dat[["sulfate"]]) & !is.na(dat[["nitrate"]]))
      data <- rbind(data, c(i, num_obs))
      
      
    }else{
      dat <- read.csv(file = paste(directory, "/", i, ".csv", sep = ""), 
                      header = TRUE)
      
      num_obs <- sum(!is.na(dat[["sulfate"]]) & !is.na(dat[["nitrate"]]))
      data <- rbind(data, c(i, num_obs))
      
      
    }
  }
  colnames(data) <- c("id", "nobs")
  data
}