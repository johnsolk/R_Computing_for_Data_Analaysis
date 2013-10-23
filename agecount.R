agecount<-function(age){
  homicides <- readLines("homicides.txt")
  #search for "years old" and "Age:"
  listofagepositions<-regexpr("years old(.*)",homicides)
  count<-1
  finalcount<-0
  ages<-integer()
  age_matches<-integer()
  for (position in listofagepositions){
      anyage<-substr(homicides[count],(position-3),(position-2))
      ages<-c(ages,anyage)
      count<-count+1
    }
  for (testage in ages){
    if (testage==age){
      finalcount<-finalcount+1
    }
  }
  return(finalcount)
  }
