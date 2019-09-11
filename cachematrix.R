## Put comments here that give an overall description of what your
## functions do

## Returns a list of helper functions for 
## getting and setting the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Intializing the inverse matrix
    inverse <- NULL
    
    ## Function to set the input matrix
    set <- function(matrix) {
        x <<- matrix
        inverse <<- NULL
    }
    
    ## Function to get the input matrix
    get <- function() x
    
    ## Function to set the inverse matrix
    setInverse <- function(y) inverse <<- y
    
    ## Function to get the inverse matrix
    getInverse <- function() inverse
    
    ## List of functions to be used by cacheSolve function
    list(set = set,
         get = get,
         getInverse = getInverse,
         setInverse = setInverse)
}


## Returns the inverse of the matrix 'x' or its cache if available

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    
    ## Check if the cache is available
    if(!is.null(inverse)) {
        message('getting cached data')
        return(inverse)
    }
    
    data <- x$get()
    
    ## Calculate the inverse since cache is NULL
    inverse <- solve(data, ...)
    
    ## Set the calculated inverse
    x$setInverse(inverse)
    
    ## Return the inverse matrix
    inverse
}
