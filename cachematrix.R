## makeCacheMatrix creates a list containing functions to
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of inverse of the matrix
# 4) get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set=setMatrix, get=getMatrix, setinverse=setInverse, getinverse=getInverse)
}


# cacheSolve returns the inverse of the matrix. First, it checks if
# the inverse has already been computed in the cache. If so, it returns the result. 
# If not, it computes the inverse, sets the value in the cache with the setInverse function.

# NOTE: cacheSolve assumes that the matrix is always invertible.

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
}
