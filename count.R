count<-function(cause){
  causes<-c("asphyxiation","blunt force","other","shooting","stabbing","unknown")
  if(!cause %in% causes) {stop("invalid cause")}
  homicides <- readLines("homicides.txt")
  if (cause =="shooting"){
    count_num<-length(grep("[Cc]ause: [Ss]hooting",homicides))
  }
  if (cause=="blunt force"){
    count_num<-length(grep("[Cc]ause: [Bb]lunt force",homicides))
  }
  if (cause=="asphyxiation"){
    count_num<-length(grep("[Cc]ause: [Aa]sphyxiation",homicides))
  }
  if (cause=="stabbing"){
    count_num<-length(grep("[Cc]ause: [Ss]tabbing",homicides))
  }
  if (cause=="unknown"){
    count_num<-length(grep("[Cc]ause: [Uu]nknown",homicides))
  }
  if (cause=="other"){
    count_num<-length(grep("[Cc]ause: [Oo]ther",homicides))
  }
  return(count_num)
}
