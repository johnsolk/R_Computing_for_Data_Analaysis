getmonitor <- function(id, directory, summarize = FALSE) {
        ## 'id' is a vector of length 1 indicating the monitor ID
        ## number. The user can specify 'id' as either an integer, a
        ## character, or a numeric.
  new_id=as.numeric(id)
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
  u<-sprintf("%s/%03d.csv",directory,new_id)
  data<-read.csv(file=u,head=TRUE)
  return(data)
}
