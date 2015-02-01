best<-function(state,outcome){
        outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        statelist<-outcomes[,7] ##only looking for col 7 "state"
        statetrue<-statelist==state ##logical (is state there?)
        possibleoutcomes<-c("heart attack", "heart failure","pneumonia") #given outcomes
        outcometrue<-possibleoutcomes==outcome ##logical (is outcome there?)
        if (sum(statetrue)==0){
                stop("invalid state")
        } ##tells if state is there based on above logical have 1 TRUE
        if (sum(outcometrue)==0){
                stop("invalid outcome")
        }##tells if outcome is there based on above logical have 1 TRUE
        lookinstate<-outcomes[statetrue,]
        if (outcome=="heart attack"){
                lookinstate[,11]<-as.numeric(lookinstate[,11])
                lookinstate<-lookinstate[complete.cases(lookinstate[,11]),]
                lookinstate<-lookinstate[order(lookinstate[,11],lookinstate[,2]),]
                answer<-lookinstate[1,2]
        }
        if (outcome=="heart failure"){
                lookinstate[,17]<-as.numeric(lookinstate[,17])
                lookinstate<-lookinstate[complete.cases(lookinstate[,17]),]
                lookinstate<-lookinstate[order(lookinstate[,17],lookinstate[,2]),]
                answer<-lookinstate[1,2]
        }
        if (outcome=="pneumonia"){
                lookinstate[,23]<-as.numeric(lookinstate[,23])
                lookinstate<-lookinstate[complete.cases(lookinstate[,23]),]
                lookinstate<-lookinstate[order(lookinstate[,23],lookinstate[,2]),]
                answer<-lookinstate[1,2]
        }
       answer
        
}