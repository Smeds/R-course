## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Creates a matrix with special features. It has
#the possibility of storing a cached inverse
#version of it self. 
makeCacheMatrix <- function(x = matrix()) {
    iM <- NULL
    #Function used to change matrix content
    #will reset the inverse
    set <- function(y) {
        x <<- y
        iM <<- NULL
    }
    
    #Retrieve the matrix
    get <- function() x
    #Stores the calculated inverser
    setInverse <- function(inverse) iM <<- inverse
    #Get the stored inverse, if hasn't been 
    #calculated the function will return null
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
    #Retrieve the stored inverse
    iM <- x$getInverse()
    #If a inverse have been calculated
    #it will be return without recalculating
    #it.
    if(!is.null(iM)) {
	message("Getting chach version")
	return(iM)
    }
    #The inverse haven't been calculated. 
    #First retrieve the matrix
    data <- x$get()
    #Calculate the inverse
    iM <- solve(data)
    #Store the inverse for later retrieval
    x$setInverse(iM)
    #Return the inverse
    iM
}
