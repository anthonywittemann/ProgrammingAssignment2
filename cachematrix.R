## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set=set, get=get, setinverse=setInverse, getinverse=getInverse)
    
    # example returning the mean
    #m <- NULL
    #set <- function(y) {
    #    x <<- y
    #    m <<- NULL
    #}
    #get <- function() x
    #setmean <- function(mean) m <<- mean
    #getmean <- function() m
    #list(set = set, get = get,
    #     setmean = setmean,
    #     getmean = getmean)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
    
    # example with returning the mean instead of the inverse
    #m <- x$getmean()
    #if(!is.null(m)) {
    #    message("getting cached data")
    #    return(m)
    #}
    #data <- x$get()
    #m <- mean(data, ...)
    #x$setmean(m)
    #m
}
