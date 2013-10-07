corr <- function(directory, threshold=0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        ##
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        ##
        ## Return a numeric vector of correlations
  source("getmonitor.R")
  num_sulfate<-numeric(0)
  num_nitrate<-numeric(0)
  end<-length(list.files(directory,pattern="*.csv",all.files=TRUE))
  id<-1:end
  correlat<-numeric(0)
  for (i in id){
    data<-getmonitor(i,directory)
    j<-na.omit(data)
    num_obs<-nrow(j)
    if (num_obs>threshold){
      t<-cor(j$sulfate,j$nitrate)
      correlat<-append(correlat,t)
    }}
  return(correlat)
}
