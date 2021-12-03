corr <- function(directory,threshold=0){
        
        files_full_ii <- list.files(directory,full.names =TRUE)
        
        my_data <- vector(mode="numeric",length=0) 
        
        for(i in 1:length(files_full_ii)){
                
                temp<- read.csv(files_full_ii[i])
                
                file_sum<- sum(complete.cases(temp))
                
                if (file_sum>threshold){
                        sul <- temp[which(!is.na(temp$sulfate)), ]
                        nit <- sul[which(!is.na(sul$nitrate)), ]
                        my_data <- c(my_data, cor(nit$sulfate, nit$nitrate))
                }
        }
        my_data
}