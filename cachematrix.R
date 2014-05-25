##-----------cachematrix.r--------------------
##this file contains a set of functions which
##cache the inverse of a matrix. This way, a user
##can simply access the cached inverse as opposed
##to calculating the inverse each time

## this function creates a list which joins the matrix and its inverse. 
## The matrix can be accessed or changed by using 'get' and 'set', and its
## inverse can be accessed or changed by using 'getInverse' and 'setInverse'
##
## calling makeCacheMatrix on a matrix just initializes this list; the inverse
## is set to NULL.
## cacheSolve with this list as its parameter needs to be called in order to
## fill out the inverse.



makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    
    setInverse <- function(inv) {
        inverse <<- inv
    }
    
    getInverse <- function() {
        inverse
    }
    
    list (set = set, 
        get = get, 
        setInverse = setInverse, 
        getInverse = getInverse)
}


## this function gets the cached inverse from the above list if it is already set,
## or calculates the inverse and sets it into the above list.
## Arguments:
## x - matrix for which we want to get the inverse
## ... - further arguments passed to or from other methods
## Return value:
## inverse of matrix x

cacheSolve <- function(x, ...) {
    
    inverse <- x$getInverse()
    
    if (!is.null(inverse)) {
        message("getting cached data")
        return (inverse)
    }
    
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    
    inverse
}
