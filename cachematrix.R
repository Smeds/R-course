## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Creates a matrix with special features. It has
#the possibility of storing a cached inverse
#version of it self. 
makeCacheMatrix <- function(x = matrix()) {
    iM <- NULL
    set <- function(y) {
        x <<- y
        iM <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) iM <<- inverse
    getInverse <- function() iM
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

## Write a short comment describing this function
#This function will calculate the inverse of a matrix
#if it hasn't been calculated before. If it has been
#calculated before it will be retrieved from a cache
cacheSolve <- function(x, ...) {
    iM <- x$getInverse()
    if(!is.null(iM)) {
	message("Getting chach version")
	return(iM)
    }
    data <- x$get()
    iM <- solve(data)
    x$setInverse(iM)
    iM
}
