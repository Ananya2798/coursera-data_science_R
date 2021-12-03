pollutantmean<-function(directory, pollutant, id =1:332){
        loaded_files<- list.files(directory,full.names =TRUE)
        my_dataframe<-data.frame()
        for (i in id){
                my_dataframe <- rbind(my_dataframe, read.csv(loaded_files[i]))
        }
        mean_my_dataframe<- mean(my_dataframe[,pollutant], na.rm=TRUE)
        return(mean_my_dataframe)
}