## Functions for Caching an Inverse of a Matrix
## 
## The following functions calculates the inverse of a matrix, by first checking to see
## if the inverse has already been calculated. If so, it gets the inverse matrix from the cache
## and skips the computation. Otherwise, it calculates the inverse of the data and sets the 
## value of the mean in the cache via the setmean function.

## Function to initialize the special vector and functions

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}



## Funtion to calculate the inverse, using solve with B=1

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
