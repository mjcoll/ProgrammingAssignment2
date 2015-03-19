## The functions in this code are used to calculate, cache, and retrieve the
## inverse of a square matrix. The caching of the results saves on computation
## time because the inverse only needs to be calculated once for any given matrix.

## This function takes a matrix argument and creates a list of several functions
## that can be used to cache and retrieve the inverse of that matrix.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL        
    }
    get <- function() x
    set_inv <- function(inv) i <<- inv
    get_inv <- function() i
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## This function returns the inverse of the matrix by taking a list argument
## from makeCacheMatrix. First, the function checks to see if the inverse has
## already been cached in another environment and, if so, returns the cached
## value. Otherwise, it calculates the inverse and then caches it for later retrieval.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$get_inv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$set_inv(i)
    i
}