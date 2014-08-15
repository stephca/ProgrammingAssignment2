## Programming Assignment 2 - Caching the Inverse of a Matrix
## Extend the solve() function by adding the ability to cache the result for 
## a performance benefit in case of needing to compute the inverse repeatedly.

## This function creates a custom matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## This function sets the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## This function gets the value of the matrix
    get <- function() x
    ## This function sets the value of the inverse of the matrix
    setinverse <- function(inverse) i <<- inverse
    ## This function gets the value of the inverse of the matrix
    getinverse <- function() i
    ## Create a list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function returns the inverse of a matrix created using the 
## makeCacheMatrix function. Makes use of the cache if already computed.

cacheSolve <- function(x, ...) {

    ## Get the stored value for the inverse
    m <- x$getinverse()
    ## If value is not null, return the cached inverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    ## Get the inverse of the matrix and store it in the cache
    ## Assume the matrix is invertible
    m <- solve(data)
    x$setinverse(m)
    ## Return a matrix that is the inverse of 'x'
    m
}
