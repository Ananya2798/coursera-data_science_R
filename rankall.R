rankall <- function(outcome, num="best") {
        ## Read outcome data
        outcomes<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ##storing the data we need in new dataframe called data
        data<-as.data.frame(cbind(outcomes[,2], #hospital
                                  outcomes[,7], #state
                                  outcomes[,11],#heart attack
                                  outcomes[,17],#heat failure
                                  outcomes[,23]))#pneumonia
        ##setting up column names
        colnames(data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")

        ##checking validity of outcome entered as arguement
        if(!outcome %in% c("heart attack","heart failure","pneumonia")){
                stop('invalid outcome')
        }
        
        hRank<- data.frame()
        for (state in sort(unique(data[,"state"]))){
                ##getting data in this state
                hRates<- data[data[,"state"]==state,]
                
                ##convert column of the specified outcome as numeric
                hRates[,outcome] <- as.numeric(hRates[ , outcome])
                
                ##since we have have converted the not available values in the desired 
                ##outcome to NAs, we can easily remove them. This is where we understand 
                ##that conversion to numeric of only desired outcome column was needed
                hRates <- hRates[!is.na(hRates[,outcome]), ]
                
                ##converting arguemnt "num" to valid rank
                if (num=="best"){
                        num<- 1
                }
                if (num=="worst"){
                        num<-nrow(hRates)
                }
                
                ##ordering by outcome rate, if there is a tie then by hospital name 
                hRates <- hRates[order(hRates[,outcome],hRates[,"hospital"]), ]
                
                ##storing the name of the hospital
                hName<-hRates[rnum,1]
                
                ##adding to the dataframe at each iteration the dersired values
                hRank<-rbind(hRank,data.frame(hospital=hName,state=state))
        }
        
        ##returning dataframe
        hRank
}