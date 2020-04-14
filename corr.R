corr <- function(directory, threashold = 0) {
  nobs_data <- complete(directory)
  
  index <- which(nobs_data[["nobs"]] > threashold)
  
  data <- c()
  
  for(i in (index)){
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
    correl <- cor(x = dat[["sulfate"]], y = dat[["nitrate"]], use = "pairwise.complete.obs")
    data <- append(data, correl)
  }
  if(threashold > max(nobs_data[["nobs"]])) {
    return(numeric())
  }else{
    return(data)
  }
}