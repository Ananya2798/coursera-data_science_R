##Caching helps in reducing the computing time. The following function
##caches time consuming computations, so that if the value of a matrix 
##is not changing then it can look up in the cache rather than recomputing.

##Matrix inverse is a costly computation and caching can be a beneficial 
##apprach.

## makeCacheMatrix: This function creates a special "matrix" object 
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set <- function(y){
                x<<- y
                m<<- NULL
        }
        get <- function() x
        setinverse<- function(solve) m<<- solve
        getinverse <- function() m
        list(set=set , get=get,
             setinverse=setinverse,getinverse=getinverse)
}
##This function computes the inverse of a special "matrix" using
##solve function returned by makeCachematrix. If the inverse has 
##already been calculated, then cacheSolve retrieves the inverse 
##from the cache.

## cacheSolve: This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. If the inverse has 
##already been calculated (and the matrix has not changed), then 
##the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m<- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return (m)
        }
        data<-x$get()
        m<- solve(data,...)
        x$setinverse(m)
        m              
        ## Return a matrix that is the inverse of 'x'
        
        
}
##We have assumed that the matrix supplied is always invertible.