rankall<-function(outcome,num = "best"){
        if (num == "best"){
                num<-1
        } ##Had to put this somewhere. "best" is number 1!
        outcomes<- read.csv("outcome-of-care-measures.csv", colClasses = "character",stringsAsFactors=FALSE)
        if (outcome == "heart attack"){outcome<-11}
        else if (outcome == "heart failure"){outcome<-17}
        else if (outcome == "pneumonia"){outcome<-23}
        else {stop("invalid outcome")} ##Likely a better way to do this with a list
        states<-sort(unique(outcomes[,7])) ##List of states. This will be part of output.
        statelist<-as.data.frame(sort(unique(outcomes[,7]))) ##Output is df of state abrvs alphabetized
        colnames(statelist)[1]<-"state"
        outcomes[,outcome]<-suppressWarnings(as.numeric(outcomes[,outcome])) 
        ##Convert values in outcome column to be numeric and suppress warning.
        outcomes<-outcomes[complete.cases(outcomes[,outcome]),] ##Remove NAs for outcome you care about.
        outcomes<-outcomes[,c(2,7,outcome)] ##Pare down data to Hospital, State, Outcome %
        y<-1 #set index to be used for the 54 states/territories
        while (y<=length(states)){
        for (i in states){
                stateoutcomes<-outcomes[outcomes[,2]==i,] ##only looking at one state
                orderedstateoutcomes<-stateoutcomes[order(stateoutcomes[,3],stateoutcomes[,1]),] 
                ##ordering data by outcome % and then alphabetically
                findworst<-nrow(stateoutcomes) ##we need to set a "last" number value
                if (num>findworst&num!="worst"){
                        statelist[y,2]<-NA     ##This covers if the number is too large
                        
                }
                
                else if (num=="worst"){        ##lexical scoping. Each state has a different worst so we need to reset it
                        worstnum<-NA           ##for each iteration
                        worstnum<-findworst
                        statelist[y,2]<-orderedstateoutcomes[worstnum,1] ##taking the outcome with the last number
                        worstnum<-NA           ##lexical scoping! Unnecessary to reset twice, 
                                               ##but this had me caught up for an hour, so not taking chances.
                        
                }
 
                else {
                statelist[y,2]<-orderedstateoutcomes[num,1] ##Easy part. Grabs Hosp name of number you asked for.
                }                                           ##Runs if not "worst"
                y<-y+1                                      ##Iterates through the 54 states/territories
        }}
        colnames(statelist)[2]<-"hospital"
        hospitalfirst<-data.frame(hospital=statelist$hospital,state = statelist$state)
        hospitalfirst                         ##Set up the df backwards originally. Flipped it around here.
}                                             ##Thought that would be less error prone.
