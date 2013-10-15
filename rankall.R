rankall<-function(outcome_name,num){
  orig_num<-num
  ##takes two arguments: 
  ##outcome name (outcome)
  ##hospital ranking (num)
  ##reads "outcome-of-care-measures.csv"
  outcome<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ##returns a 2-column data frame
  ##containing the hospital in each state 
  ##that has the ranking specied in num
  ##check to make sure outcomes is valid
  outcomes<-c("heart attack","heart failure","pneumonia")
  if(!outcome_name %in% outcomes) stop("invalid outcome")
  ##check to make sure num is valid
  states<-unique(outcome$State)
  sort(states)
  hospital_names<-c()
  state_names<-c()
  for (state in states){
    outcome2<-subset(outcome,State==state)
    if (outcome_name == "heart attack"){  
      ranked_outcome<-outcome2[order(as.numeric(outcome2[,11]),outcome2$Hospital.Name),]
      num_ranked<-as.numeric(ranked_outcome[,11])
      na_removed<-num_ranked[!is.na(num_ranked)]
      data_length<-length(na_removed)
      ranked_outcome2<-ranked_outcome[1:data_length,]
    }
    if (outcome_name == "heart failure"){
      ranked_outcome<-outcome2[order(as.numeric(outcome2[,17]),outcome2$Hospital.Name),]
      num_ranked<-as.numeric(ranked_outcome[,17])
      na_removed<-num_ranked[!is.na(num_ranked)]
      data_length<-length(na_removed)
      ranked_outcome2<-ranked_outcome[1:data_length,]
    }
    if (outcome_name == "pneumonia"){
      ranked_outcome<-outcome2[order(as.numeric(outcome2[,23]),outcome2$Hospital.Name),]
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
      num<-"NA"
    }
    ranked_hospital<-ranked_outcome2$Hospital.Name[num]
    state_names<-rbind(state_names,state)
    hospital_names<-rbind(hospital_names,ranked_hospital)
    num<-orig_num
  }
  state_df<-data.frame(hospital=hospital_names,state=state_names,stringsAsFactors=FALSE)
  return (state_df)
}
