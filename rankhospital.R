rankhospital<-function(state,outcome_name,num){
  ##takes 3 arguments:
  ##2-character abbreviation of a state
  ##an outcome
  ##ranking of hospital for that outcome (num)
  ##reads "outcome-of-care-measures.csv" file
  outcome<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  outcomes<-c("heart attack","heart failure","pneumonia")
  if(!state %in% outcome$State) stop("invalid state")
  if(!outcome_name %in% outcomes) stop("invalid outcome")
  outcome2<-subset(outcome,State==state)
  hospital_names<-outcome2$Hospital.Name
  if (outcome_name == "heart attack"){  
    ranked_outcome<-outcome2[order(as.numeric(outcome2[,11])),]
    num_ranked<-as.numeric(ranked_outcome[,11])
    na_removed<-num_ranked[!is.na(num_ranked)]
    data_length<-length(na_removed)
    ranked_outcome2<-ranked_outcome[1:data_length,]
  }
  if (outcome_name == "heart failure"){
    ranked_outcome<-outcome2[order(as.numeric(outcome2[,17])),]
    num_ranked<-as.numeric(ranked_outcome[,17])
    na_removed<-num_ranked[!is.na(num_ranked)]
    data_length<-length(na_removed)
    ranked_outcome2<-ranked_outcome[1:data_length,]
  }
  if (outcome_name == "pneumonia"){
    ranked_outcome<-outcome2[order(as.numeric(outcome2[,23])),]
    num_ranked<-as.numeric(ranked_outcome[,23])
    na_removed<-num_ranked[!is.na(num_ranked)]
    data_length<-length(na_removed)
    ranked_outcome2<-ranked_outcome[1:data_length,]
  }
  if (num == "best"){
    num<-1
  }
  if (num=="worst"){
    num<-data_length
  }
  if (num>data_length) {
    stop(return ("NA"))
  }
  ranked_hospital<-ranked_outcome2$Hospital.Name[num]
  ##returns character vector with name of hospital for num
  #rankhospital("MD", "heart failure", 5)
  ##would return a character vector containing the name of the hospital with
  ##the 5th lowest 30-day death rate for heart failure.
  return(ranked_hospital)
}
