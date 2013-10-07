complete <- function(directory, id) {
  source("getmonitor.R")
  nobs<-numeric(0)
  for (i in id){
    data<-getmonitor(i,directory)
    nobs<-append(nobs,(nrow(na.omit(data))))}
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  x<-data.frame(id,nobs)
  return(x)
}
