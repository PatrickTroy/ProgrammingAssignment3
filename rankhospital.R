rankhospital<-function(state,outcome,num = "best"){
        if (num == "best"){
                num<-1
        }
        outcomes<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        statelist<-outcomes[,7] ##only looking for col 7 "state"
        statetrue<-statelist==state ##logical (is state there?)
        possibleoutcomes<-c("heart attack", "heart failure","pneumonia") #given outcomes
        outcometrue<-possibleoutcomes==outcome ##logical (is outcome there?)
        numthere<-num<=length(statelist)|num=="best"|num=="worst"
        if (numthere==FALSE){
                print(NA)
        }
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
                findworst<-nrow(lookinstate)
                if (num=="worst"){
                        num<-findworst
                }
                lookinstate<-lookinstate[order(lookinstate[,11],lookinstate[,2]),]
                answer<-lookinstate[num,2]
        }
        if (outcome=="heart failure"){
                lookinstate[,17]<-as.numeric(lookinstate[,17])
                lookinstate<-lookinstate[complete.cases(lookinstate[,17]),]
                findworst<-nrow(lookinstate)
                if (num=="worst"){
                        num<-findworst
                }
                lookinstate<-lookinstate[order(lookinstate[,17],lookinstate[,2]),]
                answer<-lookinstate[num,2]
        }
        if (outcome=="pneumonia"){
                lookinstate[,23]<-as.numeric(lookinstate[,23])
                lookinstate<-lookinstate[complete.cases(lookinstate[,23]),]
                findworst<-nrow(lookinstate)
                if (num=="worst"){
                        num<-findworst
                }
                lookinstate<-lookinstate[order(lookinstate[,23],lookinstate[,2]),]
                answer<-lookinstate[num,2]
        }
        answer
        
}