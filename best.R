best<-function(state,outcome_name){
  ## Input 2 character abbreviation of state name
  ## Input name of one outcome of interest: "heart attack", "heart failure", "pneumonia"
  ## Reads outcome data from file "outcome-of-care-measures.cvs"
  outcome<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state is valid
  state_names<-names(table(outcome$State))
  if(!state %in% outcome$State) stop("invalid state") 
  ## Hospitals that do not have data ("NA") on a particular
  ## outcome will be excluded from the set of hospitals
  ## when deciding the rankings.
  ## Finds the hospital name with the lowest 30-day mortality
  outcome2<-subset(outcome,State==state)
  hospital_names<-outcome2$Hospital.Name
  if (outcome_name == "heart attack"){
      heart_attack<-as.numeric(outcome2[,11])
      best<-which.min(heart_attack)
  }
  if (outcome_name == "heart failure"){
      heart_failure<-as.numeric(outcome2[,17])
      best<-which.min(heart_failure)
  }
  if (outcome_name == "pneumonia"){
      pneumonia<-as.numeric(outcome2[,23])
      best<-which.min(pneumonia)
  }
  ## Return hospital name in that state with lowest 30-day death
  best_hospital<-hospital_names[best]
  return(best_hospital)
}
