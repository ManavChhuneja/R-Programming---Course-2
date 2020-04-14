pollutantmean <- function(directory, pollutant, id = 1:332) {
  data <- data.frame()
  for(i in (id)){
    if(i <= 9){
      dat <- read.csv(file = paste(directory, "/00", i, ".csv", sep = ""), 
                         header = TRUE)
    }else if (i > 9 & i <= 99) {
      dat <- read.csv(file = paste(directory, "/0", i, ".csv", sep = ""), 
                         header = TRUE)
    }else{
      dat <- read.csv(file = paste(directory, "/", i, ".csv", sep = ""), 
                         header = TRUE)
    }
    data <- rbind(data, dat)
  }
  mean(data[[pollutant]], na.rm = TRUE)
}