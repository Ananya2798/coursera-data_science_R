best <- function(state, outcome) {
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
        
        ##checking validity of state and outcome entered
        if(!state %in% data[,"state"]){
                stop('invalid state')
        }
        if(!outcome %in% c("heart attack","heart failure","pneumonia")){
                stop('invalid outcome')
        }
        
        ##getting data from chosen state
        hRates<- data[data[,"state"]==state,]
        
        ##convert column of the specified outcome as numeric
        hRates[,outcome] <- as.numeric(hRates[ , outcome])
        
        ##since we have have converted the not available values in the desired 
        ##outcome to NAs, we can easily remove them. This is where we understand 
        ##that conversion to numeric of only desired outcome column was needed
        hRates <- hRates[!is.na(hRates[,outcome]), ]
        
        ##ordering the data as per the outcome rate
        hRates <- hRates[order(hRates[,outcome]), ]
        
        ##getting the name of the hospital with lowest rate
        hName <- hRates[hRates[,outcome]==min(hRates[,outcome]),1]
        
        ##sorting hospital by name if there is a tie
        sort(hName)[1]
}