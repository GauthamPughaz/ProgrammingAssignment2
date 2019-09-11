## Put comments here that give an overall description of what your
## functions do

## Returns a list of helper functions for 
## getting and setting the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(matrix) {
        x <<- matrix
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(y) inverse <<- y
    getInverse <- function() inverse
    
    list(set = set,
         get = get,
         getInverse = getInverse,
         setInverse = setInverse)
}


## Returns the inverse of the matrix 'x' or its cache if available

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) return(inverse)
    
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
