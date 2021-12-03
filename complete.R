complete <- function(directory, id = 1:332)
{
        files_full <- list.files(directory,full.names = TRUE)
        my_data_ii <-data.frame()
        for ( i in id)
        {
                temp <- read.csv(files_full[i])
                nobs <- sum(complete.cases(temp))
                my_data_ii <- rbind(my_data_ii,data.frame(i,nobs))
        }
        colnames(my_data_ii)<- c("id","nobs")
        return(my_data_ii)
}